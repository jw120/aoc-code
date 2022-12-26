"""Advent of Code 2022 - Day 25."""


from doctest import testmod
from typing import Final

# import fileinput

SNAFU_VALUES: Final[dict[str, int]] = {"2": 2, "1": 1, "0": 0, "-": -1, "=": -2}
SNAFU_DIGITS: Final[dict[int, str]] = {v: k for k, v in SNAFU_VALUES.items()}


def snafu_to_decimal(s: str) -> int:
    """Convert SNAFU number to decimal.

    >>> sum(snafu_to_decimal(line) for line in TEST_DATA)
    4890
    """
    acc = 0
    base = 1
    for digit in reversed(s):
        acc += SNAFU_VALUES[digit] * base
        base *= 5
    return acc


def decimal_to_snafu(x: int) -> str:
    """Convert decimal number to SNAFU."""
    # Find highest base we will need
    highest_base = 1
    while x // highest_base >= 3:
        highest_base *= 5
    # Work out required digits
    acc = 0
    base = highest_base
    while base > 1:



TEST_DATA: Final[
    list[str]
] = """1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122""".splitlines()


if __name__ == "__main__":
    testmod()
