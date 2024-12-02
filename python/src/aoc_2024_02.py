"""Advent of Code 2024 - Day 2."""

from itertools import pairwise
from sys import stdin


def is_safe(xs: list[int]) -> bool:
    """Test is the levels are safe.

    Safe if:
     * all increasing or all decreasing and all ga
     * all differences are between 1 and 3
    """
    increasing: bool = xs[1] > xs[0]
    for x, y in pairwise(xs):
        diff = abs(y - x)
        if (y > x) != increasing or diff < 1 or diff > 3:
            return False
    return True


def is_safe_one(xs: list[int]) -> bool:
    """Test is the levels are safe allowing for one level to be removed."""
    if is_safe(xs):
        return True
    for i in range(len(xs)):
        ys = xs[:i] + xs[i + 1 :]
        if is_safe(ys):
            return True
    return False


if __name__ == "__main__":
    lines: list[list[int]] = [[int(x) for x in s.split()] for s in stdin.readlines()]
    print(sum(is_safe(line) for line in lines))
    print(sum(is_safe_one(line) for line in lines))
