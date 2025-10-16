#!/usr/bin/env python3
import contextlib
import shlex
import socket
import subprocess
import sys

from pathlib import Path


SERVER_ADDRESS = "\0types-2026-deploy"
KEYTAB = Path("sattler.keytab")

DEPLOY_USER = "sattler"
DEPLOY_HOST = "remote11.chalmers.se"
DEPLOY_DIR = "/chalmers/groups/w3types/www/types2026.cse.chalmers.se/"
DEPLOY_TARGET = f"{DEPLOY_USER}@{DEPLOY_HOST}:{DEPLOY_DIR}"
DEPLOY_REMOTE_SHELL = [
    "ssh",
    "-o",
    "GSSAPIAuthentication yes",
    "-o",
    "GSSAPIDelegateCredentials yes",
]


class ServerError(Exception):
    pass


class ServerSide(contextlib.AbstractContextManager):
    def __init__(self, s):
        self.socket = s
        self.file = self.socket.makefile("rw")

    def __exit__(self, exc_type, exc_value, traceback):
        self.file.close()
        self.socket.close()

    def success(self):
        self.file.write("OK\n")

    def log_line(self, line):
        print(line, file=sys.stderr)
        self.file.write(f"> {line}\n")
        self.file.flush()

    def run(self, args):
        with subprocess.Popen(
            args,
            text=True,
            stdin=None,
            stderr=subprocess.STDOUT,
            stdout=subprocess.PIPE,
        ) as p:
            for line in p.stdout:
                line = line.removesuffix("\n")
                self.log_line(line)
            p.wait()
            if not p.returncode == 0:
                self.log_line(f"Process failed with exit code {p.returncode}")
                raise ServerError()

    def handle(self):
        try:
            path = self.file.read()
            print(f"Source directory: {path}")

            self.log_line("Requesting Kerberos ticket...")
            self.run(["kinit", "-f", "-k", "-t", KEYTAB, "sattler@CHALMERS.SE"])
            self.log_line("Deploying files to web server...")
            self.run(
                [
                    "rsync",
                    "-e",
                    shlex.join(DEPLOY_REMOTE_SHELL),
                    "--verbose",
                    "--recursive",
                    "--delete",
                    "--delete-delay",
                    "--links",
                    "--times",
                    path + "/",
                    "--exclude=/.git",
                    DEPLOY_TARGET,
                ]
            )
            self.success()
        except ServerError:
            pass


def main():
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as server:
        server.bind(SERVER_ADDRESS)
        server.listen()

        while True:
            (s, _) = server.accept()
            with ServerSide(s) as server_side:
                server_side.handle()


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        pass
