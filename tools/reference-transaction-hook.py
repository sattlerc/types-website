#!/usr/bin/env python3
"""
Run with --help for documentation.
See EXAMPLE_HOOK for an example usage as a reference-transaction hook.
"""

import argparse
import contextlib
from dataclasses import dataclass
import os
import shlex
import shutil
import subprocess
import sys
import textwrap

from pathlib import Path, PurePosixPath

REL_CABAL_BUILD = PurePosixPath("dist-newstyle")

CONTAINER_DIR_SRC = PurePosixPath("/src")
CONTAINER_DIR_CABAL = PurePosixPath("/cabal")

EXAMPLE_HOOK = """
#!/bin/bash
<tracking-repo>/tools/collect_output.py '> ' \\
  <tracking-repo>/tools/reference-transaction-hook.py \\
    --branch main \\
    --cabal-executable site \\
    --docker-executable podman \\
    --docker-image haskell:9.6 \\
    --tracking-repo <tracking-repo> \\
    --deploy-script <deploy-script> \\
    "$@"
"""

EPILOG = f"""
This script is meant to be used as reference-transaction hook in your remote repository.
Set the named arguments, but preserve the working directory.

Here is an example hook hooks/reference-transaction:

{textwrap.indent(EXAMPLE_HOOK.strip(), prefix='  ')}

You can test this script by calling it without positional arguments.

To trigger a cabal update, simply delete the package folder in the cabal directory.
"""


def log(msg: str):
    print(msg, file=sys.stderr)
    sys.stderr.flush()


def clear_directory(dir_: Path):
    for path in dir_.iterdir():
        if path.is_file():
            path.unlink()
        else:
            shutil.rmtree(path)


@dataclass
class Runner:
    verbose: bool

    def __call__(self, args, return_output: bool = False, **kwargs):
        if self.verbose:
            log(shlex.join(map(str, args)))
        p = subprocess.run(
            args,
            text=True,
            check=True,
            stdout=subprocess.PIPE if return_output else sys.stderr,
            **kwargs,
        )
        return p.stdout if return_output else None


@dataclass
class TrackingRepo:
    path: Path
    runner: Runner

    def __post_init__(self):
        if not self.path.exists():
            self.clone()

    def clone(self):
        log(f"Creating tracking repository {self.path}...")
        self.runner(["git", "clone", "--quiet", "--shared", Path(), self.path])

    def pull(self, rev: str):
        env = dict(os.environ)
        git_local_env_keys = self.runner(
            ["git", "rev-parse", "--local-env-vars"],
            return_output=True,
        ).splitlines()
        for key in git_local_env_keys:
            env.pop(key, None)

        log(f"Pulling commit {rev}...")
        for cmd in [
            ["git", "fetch", "--quiet", "origin", rev],
            ["git", "checkout", "--quiet", f"{rev}"],
        ]:
            self.runner(cmd, cwd=str(self.path.resolve()), env=env)

    def find_cabal_project_executable(self, name: str):
        path = self.path / REL_CABAL_BUILD / "build"
        while True:
            file_ = path / name
            if file_.is_file():
                return file_

            for subdir in [name, "build"]:
                dir_ = path / subdir
                if dir_.is_dir():
                    path = dir_
                    continue

            try:
                [dir_] = [path_ for path_ in path.iterdir() if path_.is_dir()]
                path = dir_
                continue
            except ValueError:
                pass

            raise RuntimeError(f"cannot locate Cabal project executable '{name}'.")


class PodmanContainer(contextlib.AbstractContextManager):
    @classmethod
    def volume_source_path(cls, path: Path):
        """Format a volume source path as desired by podman."""
        if path.is_absolute():
            return str(path)
        path = str(path)
        if not path.startswith("."):
            path = f"./{path}"
        return path

    @classmethod
    def volume(cls, path_host, path_container, options):
        parts = [
            PodmanContainer.volume_source_path(path_host),
            str(path_container),
            ",".join(options),
        ]
        yield from ["--volume", ":".join(parts)]

    @classmethod
    def env(cls, key: str, value: str):
        yield from ["--env", "=".join([key, value])]

    def __init__(self, args, runner: Runner, executable: Path, **kwargs):
        self.runner = runner
        self.executable = executable
        self.id = self.runner(
            [self.executable, "create", *args],
            return_output=True,
            **kwargs,
        ).strip()

    def __exit__(self, exc_type, exc_value, traceback):
        try:
            _id = self.id
        except AttributeError:
            pass
        else:
            self.runner([self.executable, "rm", "--force", _id], return_output=True)

    def start(self):
        self.runner([self.executable, "start", self.id], return_output=True)

    def exec(self, args, **kwargs):
        self.runner([self.executable, "exec", self.id, *args], **kwargs)


def run_hakyll(
    tracking_repo: TrackingRepo,
    container: PodmanContainer,
    cabal_subdir: Path,
    cabal_executable: str,
    site_subdir: Path,
    cache_subdir: Path,
):
    # Update cabal if necessary.
    if not (tracking_repo.path / cabal_subdir / "packages").exists():
        container.exec(["cabal", "update"])

    log("Building and running Hakyll...")

    # Watch output of cabal build for rebuild.
    need_rebuild = True
    with subprocess.Popen(
        [
            container.executable,
            "exec",
            container.id,
            "cabal",
            "build",
        ],
        text=True,
        stdout=subprocess.PIPE,
    ) as p:
        first_line = True
        for line in p.stdout:
            line = line.removesuffix("\n")
            if first_line and line == "Up to date":
                need_rebuild = False
            else:
                log(line)
            first_line = False
        p.wait()
        if not p.returncode == 0:
            raise subprocess.CalledProcessError(p.returncode, p.args)

    # Cabal rebuild requires site rebuilding.
    # We cannot use site rebuild as this deletes the mount points.
    if need_rebuild:
        log("Rebuilding site.")
        for subdir in [site_subdir, cache_subdir]:
            clear_directory(tracking_repo.path / subdir)

    # This costs ~0.25s (on a non-virtual machine) more than running the executable directly.
    # But there does not seem to be a way to tell Cabal that the build already happened.
    # container.exec(["cabal", "run", "site", "build"])

    # Hack that is faster.
    site_executable = tracking_repo.find_cabal_project_executable(cabal_executable)
    container.exec([site_executable.relative_to(tracking_repo.path), "build"])


def parse_reference_from_input(branch: str):
    target_rev_name = str(PurePosixPath() / "refs" / "heads" / branch)

    def revs():
        for line in sys.stdin:
            line = line.removesuffix("\n")
            _rev_old, rev_new, rev_name = line.split(" ")
            if rev_name == target_rev_name:
                yield rev_new

    it = revs()
    try:
        rev = next(it)
    except StopIteration:
        return None
    [] = it
    return rev


def main_inner(
    args: list[str],
    branch: str,
    tracking_repo: Path,
    cabal_subdir: Path,
    cabal_executable: str,
    site_subdir: Path,
    cache_subdir: Path,
    docker_executable: Path,
    docker_image: str,
    docker_memory: str,
    deploy_script: Path,
    verbose: bool,
):
    runner = Runner(verbose=verbose)
    tracking_repo = TrackingRepo(path=tracking_repo, runner=runner)

    # Pull if needed.
    try:
        state = args[0]
    except IndexError:
        run = True
    else:
        run = False
        if state == "prepared":
            rev = parse_reference_from_input(branch)
            if rev is not None:
                tracking_repo.pull(rev)
                run = True

    if not run:
        return 0

    def create_args():
        def mount(host: Path, container: PurePosixPath, writable: bool):
            if writable:
                host.mkdir(exist_ok=True)
            yield from PodmanContainer.volume(
                host.relative_to(tracking_repo.path),
                container,
                ["rw" if writable else "ro"],
            )

        def src_mount(subdir: PurePosixPath, writable: bool):
            yield from mount(
                tracking_repo.path / subdir,
                CONTAINER_DIR_SRC / subdir,
                writable=writable,
            )

        def mounts():
            yield from mount(
                tracking_repo.path / cabal_subdir,
                CONTAINER_DIR_CABAL,
                writable=True,
            )
            yield from src_mount(Path(), writable=False)
            for subdir in [REL_CABAL_BUILD, site_subdir, cache_subdir]:
                yield from src_mount(subdir, writable=True)

        yield from mounts()
        yield from PodmanContainer.env("CABAL_DIR", str(CONTAINER_DIR_CABAL))
        yield from ["--workdir", str(CONTAINER_DIR_SRC)]
        yield from ["--memory", docker_memory]
        yield from ["--stop-signal", "SIGKILL"]
        yield docker_image
        yield from ["sleep", "infinity"]

    try:
        with PodmanContainer(
            create_args(),
            runner=runner,
            cwd=tracking_repo.path,
            executable=docker_executable,
        ) as container:
            log(f"Container id: {container.id}")
            container.start()
            run_hakyll(
                tracking_repo=tracking_repo,
                container=container,
                cabal_subdir=cabal_subdir,
                cabal_executable=cabal_executable,
                site_subdir=site_subdir,
                cache_subdir=cache_subdir,
            )
    except subprocess.CalledProcessError as e:
        # Podman error codes
        if not e.returncode in [125, 126, 127]:
            return 1

    log("Deploying...")
    runner([deploy_script.resolve(), (tracking_repo.path / site_subdir).resolve()])
    return 0


def main():
    parser = argparse.ArgumentParser(
        prog="reference-transaction-hook.py",
        description="Deploy website on update of a branch.",
        epilog=EPILOG,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--branch",
        type=str,
        default="master",
        help="""
        Branch to deploy the website.
        Default: master.
        """,
    )
    parser.add_argument(
        "--tracking-repo",
        type=Path,
        required=True,
        help="""
        Directory to use for tracking the deployment branch.
        Will be created if missing (using git clone --shared).
        Used to as a cache (for both Cabal and Hakyll) to avoid rebuilding the project on every update.
        """,
    )
    parser.add_argument(
        "--docker-executable",
        type=Path,
        default="docker",
        help="""
        Docker executable.
        Default: docker.
        Some distributions prefer podman.
        """,
    )
    parser.add_argument(
        "--docker-image",
        type=str,
        required=True,
        help="""
        Container image to use.
        Example: haskell:9.6.
        """,
    )
    parser.add_argument(
        "--cabal-subdir",
        type=Path,
        default=Path(".git/cabal"),
        help="""
        Cabal directory, relative to the tracking directory.
        Created if missing.
        Persists between container runs.
        Delete this for a clean build.
        Default: .git/cabal
        """,
    )
    parser.add_argument(
        "--cabal-executable",
        type=Path,
        required=True,
        help="Cabal executable responsible for building the site.",
    )
    parser.add_argument(
        "--site-subdir",
        type=Path,
        default=Path("_site"),
        help="""
        Subdirectory where the configured Cabal executable will generate the website.
        Default: _site.
        """,
    )
    parser.add_argument(
        "--cache-subdir",
        type=Path,
        default=Path("_cache"),
        help="""
        Subdirectory where the configured Cabal executable will cache build products for the website.
        Default: _cache.
        """,
    )
    parser.add_argument(
        "--docker-memory",
        type=str,
        default="1g",
        help="""
        How much memory to use for running containers.
        Default: 1g.
        """,
    )
    parser.add_argument(
        "--deploy-script",
        type=Path,
        required=True,
        help="Script that deploys a given directory to the webserver.",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Print program invocations.",
    )
    parser.add_argument(
        "args",
        type=str,
        nargs="*",
        help="""
        Arguments for the reference-transaction hook.
        If missing, perform a deploy without updating the tracking directory.
        """,
    )
    sys.exit(main_inner(**parser.parse_args().__dict__))


if __name__ == "__main__":
    main()
