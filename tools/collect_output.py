#!/usr/bin/env python3
import subprocess
import sys


def out(line: str):
    print(line)
    sys.stdout.flush()


def main():
    try:
        [_, prefix, *args] = sys.argv
    except ValueError:
        print(
            "usage: <script> <output-line-prefix> <program invocation>*",
            file=sys.stderr,
        )
        sys.exit(1)

    with subprocess.Popen(
        args,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    ) as p:
        for line in p.stdout:
            out(prefix + line.removesuffix("\n"))
        p.wait()
        sys.exit(p.returncode)


if __name__ == "__main__":
    main()
