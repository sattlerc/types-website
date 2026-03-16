#!/usr/bin/env python3
import contextlib
import shlex
import socket
import subprocess
import sys

from pathlib import Path

SERVER_ADDRESS = "\0types-2026-deploy"


# From util.general of data structure lab tools
def remove_prefix(x: str, prefix: str, strict: bool = False) -> str | None:
    """
    Version of x.removeprefix(prefix) that returns None if unsuccessful.
    If strict is set, raises a ValueError instead.
    """
    if x[: len(prefix)] == prefix:
        return x[len(prefix) :]

    if strict:
        raise ValueError(f"does not have prefix {shlex.quote(prefix)}: {x}")

    return None


def deploy(path):
    print(f"Source directory: {path}")
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as client:
        client.connect(SERVER_ADDRESS)
        with client.makefile("rw") as file:
            file.write(path)
            file.flush()
            client.shutdown(socket.SHUT_WR)
            for line in file:
                line = line.removesuffix("\n")
                if (msg := remove_prefix(line, "> ")) is not None:
                    print(msg, file=sys.stderr)
                elif line == "OK":
                    return 0
                else:
                    print(
                        f"unexpected deployment server response: {shlex.quote(msg)}",
                        file=sys.stderr,
                    )
                    return 1
            return 1


def main():
    try:
        [_, path] = sys.argv
    except ValueError:
        print("usage: <script> <directory to deploy>", file=sys.stderr)
        sys.exit(1)

    sys.exit(deploy(path))


if __name__ == "__main__":
    main()
