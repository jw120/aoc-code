"""Advent of Code - Golden test.

Runs all of our python solutions and compares to known-good solutions. If the
solution does not exist, then we create the file
"""

import argparse
from filecmp import cmp
from pathlib import Path
from subprocess import run
from tempfile import NamedTemporaryFile
from time import time
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from collections.abc import Iterable

PYTHON_EXECUTABLE: str = "python"
SOURCE_FILE_FORMAT: str = "src/aoc_{year}_{day:02}.py"
INPUT_FILE_FORMAT: str = "../../aoc-data/input/{year}_{day:02}.txt"
GOOD_FILE_FORMAT: str = "../../aoc-data/good/{year}_{day:02}.txt"
ELAPSED_MS_THRESHOLD: int = 500  # Show time taken if reaches this

completed: list[tuple[int, Iterable[int]]] = [
    (2019, [*list(range(1, 12)), 13, 14, 15]),
    (2020, range(1, 26)),
    (2021, list(set(range(1, 26)) - {16, 18, 24})),
    (2022, range(1, 26)),
    (2023, range(1, 4)),
    (2024, list(set(range(1, 26)) - {21, 24})),
]

# With >3s execution time
slow: set[tuple[int, int]] = {
    (2020, 11),  # 7s
    (2020, 15),  # 16s
    (2020, 17),  # 7s
    (2020, 22),  # 11s
    (2020, 23),  # 10s
    (2020, 24),  # 15s
    (2021, 15),  # 14s
    (2021, 17),  # 17s
    (2021, 19),  # 217s (~=4 mins)
    (2021, 20),  # 28s
    (2021, 22),  # 2517s  (~=42 mins) FAILS
    (2021, 23),  # 284s (~=5 mins)
    (2021, 25),  # 53s
    (2022, 14),  # 11s
    (2022, 15),
    (2022, 16),  # minutes
    (2022, 19),  # minutes
    (2022, 20),  # 20s
    (2022, 23),  # 31s
    (2022, 24),  # very slow and fails
    (2024, 17),
}


def test(year: int, day: int, *, times: bool) -> None:
    """Run test for given year and day.

    Compare output to known-good (or create a missing known-good file).
    """
    source_fn = SOURCE_FILE_FORMAT.format(year=year, day=day)
    with Path(INPUT_FILE_FORMAT.format(year=year, day=day)).open(encoding="utf8") as input_file:
        good_filename = GOOD_FILE_FORMAT.format(year=year, day=day)

        print(f"{year} {day:2}: ", end="")

        if Path(good_filename).exists():
            with NamedTemporaryFile() as output_file:
                start = time()
                run(
                    [PYTHON_EXECUTABLE, source_fn],
                    stdin=input_file,
                    stdout=output_file,
                    check=True,
                )
                elapsed = round(1000 * (time() - start))
                print("ok" if cmp(output_file.name, good_filename) else "FAIL", end="")
                print(f" ({elapsed} ms) " if times or elapsed > ELAPSED_MS_THRESHOLD else "")
        else:
            with Path(good_filename).open(mode="w", encoding="utf8") as good_file:
                run(
                    [PYTHON_EXECUTABLE, source_fn],
                    stdin=input_file,
                    stdout=good_file,
                    check=True,
                )
                print("Created", good_filename)


def all_tests(*, fast_only: bool, times: bool) -> None:
    """Run tests for all completed problems."""
    for year, days in completed:
        for day in days:
            if not fast_only or (year, day) not in slow:
                test(year, day, times=times)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--fast", action="store_true", help="only run solutions which are not slow")
    parser.add_argument(
        "--times",
        action="store_true",
        help="show execution times for all tests (not just those which are slow)",
    )
    args = parser.parse_args()
    all_tests(fast_only=args.fast, times=args.times)
