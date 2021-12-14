"""Advent of Code - Golden test.

Runs all of our python solutions and compares to known-good solutions. If the
solution does not exist, then we create the file
"""

import argparse
from filecmp import cmp
from os.path import exists
from subprocess import run
from tempfile import NamedTemporaryFile
from time import time
from typing import Iterable, Set, Tuple

PYTHON_EXECUTABLE: str = "python"
SOURCE_FILE_FORMAT: str = "python/aoc_{year}_{day:02}.py"
INPUT_FILE_FORMAT: str = "../aoc-data/input/{year}_{day:02}.txt"
GOOD_FILE_FORMAT: str = "../aoc-data/good/{year}_{day:02}.txt"
ELAPSED_MS_THRESHOLD: int = 500  # Show time taken if reaches this

completed: list[Tuple[int, Iterable[int]]] = [
    (2019, list(range(1, 12)) + [13, 14, 15]),
    (2020, range(1, 26)),
    (2021, range(1, 15)),
]

slow: Set[Tuple[int, int]] = {
    (2019, 10),
    (2019, 13),
    (2020, 11),
    (2020, 15),
    (2020, 17),
    (2020, 22),
    (2020, 23),
    (2020, 24),
}


def test(year: int, day: int) -> None:
    """Run test for given year and day. Compare output to known-good (or create a missing known-good file)."""
    source_fn = SOURCE_FILE_FORMAT.format(year=year, day=day)
    input_file = open(INPUT_FILE_FORMAT.format(year=year, day=day), mode="r")
    good_filename = GOOD_FILE_FORMAT.format(year=year, day=day)

    print(f"{year} {day:2}: ", end="")

    if exists(good_filename):
        output_file = NamedTemporaryFile()
        start = time()
        run([PYTHON_EXECUTABLE, source_fn], stdin=input_file, stdout=output_file)
        elapsed = round(1000 * (time() - start))
        print("ok" if cmp(output_file.name, good_filename) else "FAIL", end="")
        print(f" ({elapsed} ms) " if elapsed > ELAPSED_MS_THRESHOLD else "")
        output_file.close()
    else:
        good_file = open(good_filename, mode="w")
        run([PYTHON_EXECUTABLE, source_fn], stdin=input_file, stdout=good_file)
        print("Created", good_filename)
        good_file.close()


def all_tests(fast_only: bool) -> None:
    """Run tests for all completed problems."""
    for year, days in completed:
        for day in days:
            if not fast_only or (year, day) not in slow:
                test(year, day)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--fast", action="store_true", help="only run solutions which are not slow"
    )
    args = parser.parse_args()
    all_tests(args.fast)
