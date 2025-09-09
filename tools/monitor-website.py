#!/usr/bin/env python3
import argparse
import enum
import hashlib
import random
import time

from pathlib import Path

import requests


class Action(enum.StrEnum):
    GENERATE = "generate"
    MONITOR = "monitor"


def test_filename(salt: str, index: int):
    return hashlib.sha256((salt + str(index)).encode()).hexdigest()


def generate(salt, count):
    print(count)
    for k in range(count):
        Path(test_filename(salt, k)).write_text(str(k), encoding="utf-8")


def monitor(salt, count, url_base, interval):
    session = requests.Session()
    while True:
        if count == 0:
            url = url_base
        else:
            k = random.randrange(count)
            url = url_base + "/" + test_filename(salt, k)
        start = time.monotonic()
        session.get(url)
        stop = time.monotonic()
        duration = stop - start
        print(f"accessing {url} took {duration}s")
        time.sleep(interval)


def main():
    p = argparse.ArgumentParser(
        prog="website-monitor.py",
        description="Simple tool for monitoring performance of a website. Generate a bunch of test files to upload and then randomly monitor for their availability.",
    )
    p.add_argument(
        "action",
        choices=Action,
        help="generate: generate test files in current directory; monitor: monitor test file availability under given URL.",
    )
    p.add_argument(
        "-s",
        "--salt",
        type=str,
        default="unimportant",
        help="Salt to use for generating the test filenames. Must be consistent over invocations of 'generate' and 'monitor'.",
    )
    p.add_argument(
        "-c",
        "--count",
        type=int,
        default=0,
        help="Number of test files to generate and use for testing. Must be consistent over invocations of 'generate' and 'monitor'.",
    )
    p.add_argument(
        "-i",
        "--interval",
        type=float,
        default=30,
        help="Time interval in seconds for requesting files when monitoring.",
    )
    p.add_argument(
        "-u",
        "--url",
        type=str,
        help="Base URL of the directory containing the test files when monitoring. If --count is not given, this is the URL monitored.",
    )
    a = p.parse_args()
    match a.action:
        case Action.GENERATE:
            if a.count == 0:
                raise RuntimeError("Specify number of test files to generate.")

            generate(a.salt, a.count)
        case Action.MONITOR:
            monitor(a.salt, a.count, a.url, a.interval)


if __name__ == "__main__":
    main()
