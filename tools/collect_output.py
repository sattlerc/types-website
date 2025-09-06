#!/usr/bin/env python3
import subprocess
import sys


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
            line = line.removesuffix("\n")
            print(prefix + line)
            sys.stdout.flush()
        p.wait()
        sys.exit(p.returncode)


if __name__ == "__main__":
    main()
