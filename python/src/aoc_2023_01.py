"""Advent of Code 2023 - Day 1."""

from doctest import testmod
from sys import stdin
from typing import Final


def calibration(s: str) -> int:
    """Return first and last digit on the line as an integer

    >>> calibration("pqr3stu8vwx")
    38
    >>> calibration("tree7the")
    77
    """
    digits: list[int] = [int(c) for c in s if c.isdigit()]
    return digits[0] * 10 + digits[-1]


WORD_DIGITS: Final[list[str]] = [
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
]


def starts_num_word(s: str) -> int | None:
    """If string starts with a digit or its word version, return it.

    >>> starts_num_word("2qw")
    2
    >>> starts_num_word("eight23")
    8
    >>> starts_num_word("a2") is None
    True
    """
    if s[0].isdigit():
        return int(s[0])
    for i, w in zip(range(1, 10), WORD_DIGITS, strict=True):
        if s.startswith(w):
            return i
    return None


def calibration_word(s: str) -> int:
    """Return first and last digit on the line as an integer allowing word digits.

    >>> calibration_word("two1nine")
    29
    >>> calibration_word("7sixteen")
    76
    """
    digits: list[int] = [
        x for x in [starts_num_word(s[i:]) for i in range(0, len(s))] if x is not None
    ]
    return digits[0] * 10 + digits[-1]


if __name__ == "__main__":
    testmod()
    lines: list[str] = list(stdin.readlines())
    print(sum(calibration(s) for s in lines))
    print(sum(calibration_word(s) for s in lines))
