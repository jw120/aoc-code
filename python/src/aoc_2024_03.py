"""Advent of Code 2024 - Day 3."""

from re import findall
from sys import stdin

MUL_REGEX = r"mul\((\d+),(\d+)\)"
ALL_REGEX = r"(do\(\))|(don't\(\))|" + MUL_REGEX


def sum_uncorrupted(data: str) -> int:
    """Return sum of uncorrupted multiplications."""
    return sum(int(x) * int(y) for x, y in findall(MUL_REGEX, data))


def sum_enabled(data: str) -> int:
    """Return sum of enabled multiplications."""
    enabled = True
    total = 0
    for m in findall(ALL_REGEX, data):
        match m:
            case ("do()", "", "", ""):
                enabled = True
            case ("", "don't()", "", ""):
                enabled = False
            case ("", "", x, y):
                if enabled:
                    total += int(x) * int(y)
            case _:
                raise ValueError(f"Unexpected match {m}")
    return total


if __name__ == "__main__":
    data: str = stdin.read()
    print(sum_uncorrupted(data))
    print(sum_enabled(data))
