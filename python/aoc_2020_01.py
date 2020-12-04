"""Advent of Code 2020 - Day 1."""

from doctest import testmod
from sys import stdin
from typing import List


def solve_part_one(target: int, xs: List[int]) -> int:
    """Solve part one.

    >>> solve_part_one(2020, [1721, 979, 366, 299, 675, 1456])
    514579
    """
    for i in range(len(xs)):
        for j in range(i + 1, len(xs)):
            if xs[i] + xs[j] == target:
                return xs[i] * xs[j]
    raise RuntimeError("Failed to find a solution")


def solve_part_two(target: int, xs: List[int]) -> int:
    """Solve part two.

    >>> solve_part_two(2020, [1721, 979, 366, 299, 675, 1456])
    241861950
    """
    for i in range(len(xs)):
        for j in range(i + 1, len(xs)):
            for k in range(j + 1, len(xs)):
                if xs[i] + xs[j] + xs[k] == target:
                    return xs[i] * xs[j] * xs[k]
    raise RuntimeError("Failed to find a solution")


if __name__ == "__main__":
    testmod()
    nums: List[int] = [int(line) for line in stdin]
    print(solve_part_one(2020, nums))
    print(solve_part_two(2020, nums))
