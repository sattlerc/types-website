#!/usr/bin/env python3
import contextlib
import os
import shlex
import shutil
import subprocess
import sys

from pathlib import Path, PurePosixPath

PATH_DEPLOY_CLIENT = Path("/home/types-2026/deploy-client.py")
IMAGE = "haskell:9.6"

PATH_CLONE = Path("deploy_clone")

# Will be mounted in the image.
REL_SITE = PurePosixPath("_site")
REL_CACHE = PurePosixPath("_cache")
REL_CABAL = PurePosixPath("_cabal")
REL_CABAL_BUILD = PurePosixPath("dist-newstyle")

# Location of the cabal configuration file in the image.
CABAL_CONFIG = PurePosixPath("/root/.config/cabal/config")


def clear_directory(dir_):
    for path in dir_.iterdir():
        if path.is_file():
            path.unlink()
        else:
            shutil.rmtree(path)


def find_cabal_project_executable(path, name):
    path = path / REL_CABAL_BUILD / "build"
    while True:
        file_ = path / name
        if file_.is_file():
            return file_

        subdir = path / "build"
        if subdir.is_dir():
            path = subdir
            continue

        try:
            [subdir] = [subpath for subpath in path.iterdir() if subpath.is_dir()]
            path = subdir
            continue
        except ValueError:
            pass

        raise RuntimeError(f"cannot locate Cabal project executable '{name}'.")


def log(msg):
    print(msg, file=sys.stderr)


def run(args, return_output=False, **kwargs):
    # log(shlex.join(map(str, args)))
    p = subprocess.run(
        args,
        text=True,
        check=True,
        stdout=subprocess.PIPE if return_output else sys.stderr,
        **kwargs,
    )
    return p.stdout if return_output else None


def get_reference():
    def revs():
        for line in sys.stdin:
            line = line.removesuffix("\n")
            _rev_old, rev_new, rev_name = line.split(" ")
            if rev_name == "refs/heads/main":
                yield rev_new

    it = revs()
    try:
        rev = next(it)
    except StopIteration:
        return None
    [] = it
    return rev


def pull(path, rev):
    env = dict(os.environ)
    git_local_env_keys = run(
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
        run(cmd, cwd=str(path.resolve()), env=env)


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

    def __init__(self, args, **kwargs):
        self.id = run(
            ["podman", "create", *args],
            return_output=True,
            **kwargs,
        ).strip()

    def __exit__(self, exc_type, exc_value, traceback):
        try:
            _id = self.id
        except AttributeError:
            pass
        else:
            run(["podman", "rm", "--force", _id], return_output=True)

    def start(self):
        run(["podman", "start", self.id], return_output=True)

    def exec(self, args, **kwargs):
        return run(["podman", "exec", self.id, *args], **kwargs)


def run_hakyll(path, memory=2 * 1024 * 1024 * 1024):
    log("Running Hakyll...")

    def create_args():
        src = Path("/src")
        yield from PodmanContainer.volume(Path(), src, ["ro"])
        for subdir in [REL_CABAL, REL_CABAL_BUILD, REL_SITE, REL_CACHE]:
            (path / subdir).mkdir(exist_ok=True)
            yield from PodmanContainer.volume(subdir, src / subdir, ["rw"])
        yield from PodmanContainer.env("CABAL_DIR", str(src / REL_CABAL))
        yield from ["--workdir", str(src)]
        yield from ["--memory", str(memory)]
        yield from ["--stop-signal", "SIGKILL"]
        yield IMAGE
        yield from ["sleep", "infinity"]

    with PodmanContainer(create_args(), cwd=path) as container:
        log(f"Container id: {container.id}")
        container.start()

        # Update cabal if necessary.
        if not (path / REL_CABAL / "packages").exists():
            container.exec(["cabal", "update"])

        # Watch output of cabal build for rebuild.
        need_rebuild = True
        with subprocess.Popen(
            ["podman", "exec", container.id, "cabal", "build", "site"],
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
            for subdir in [REL_SITE, REL_CACHE]:
                clear_directory(path / subdir)

        # This costs ~0.25s (on a non-virtual machine) more than running the executable directly.
        # But there does not seem to be a way to tell Cabal that the build already happened.
        # container.exec(["cabal", "run", "site", "build"])

        # Hack that is faster.
        site_executable = find_cabal_project_executable(path, "site")
        container.exec([site_executable.relative_to(path), "build"])


def deploy(path):
    log("Deploying...")
    run([PATH_DEPLOY_CLIENT.resolve(), path.resolve()])


def main_inner():
    class Fail(Exception):
        pass

    try:
        rev = get_reference()
        if rev is not None:
            pull(PATH_CLONE, rev)
            try:
                run_hakyll(PATH_CLONE)
            except subprocess.CalledProcessError as e:
                # Podman error codes
                if not e.returncode in [125, 126, 127]:
                    raise Fail from None
            deploy(PATH_CLONE / REL_SITE)
    except Fail:
        sys.exit(1)


def main():
    try:
        state = sys.argv[1]
    except IndexError:
        run_hakyll(PATH_CLONE)
    else:
        if state == "prepared":
            main_inner()


if __name__ == "__main__":
    main()
