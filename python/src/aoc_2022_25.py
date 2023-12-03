"""Advent of Code 2022 - Day 25."""


from doctest import testmod
from sys import stdin
from typing import Final

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
    """Convert decimal number to SNAFU.

    >>> decimal_to_snafu(4890)
    '2=-1=0'
    """
    acc = ""
    while x != 0:
        digit = (x + 2) % 5 - 2
        acc = SNAFU_DIGITS[digit] + acc
        x = x - digit
        assert x % 5 == 0
        x //= 5
    return acc


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
    print(
        decimal_to_snafu(
            sum(snafu_to_decimal(line.strip()) for line in stdin.readlines())
        )
    )
