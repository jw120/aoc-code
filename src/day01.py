# Advent of Code 2020 - Day 1

from sys import stdin
from typing import List


def solve_part_one(target: int, xs: List[int]) -> int:
    for i in range(len(xs)):
        for j in range(i + 1, len(xs)):
            if xs[i] + xs[j] == target:
                return xs[i] * xs[j]
    raise RuntimeError("Failed to find a solution")


def solve_part_two(target: int, xs: List[int]) -> int:
    for i in range(len(xs)):
        for j in range(i + 1, len(xs)):
            for k in range(j + 1, len(xs)):
                if xs[i] + xs[j] + xs[k] == target:
                    return xs[i] * xs[j] * xs[k]
    raise RuntimeError("Failed to find a solution")


def read_ints() -> List[int]:
    """Read a list of ints from stdin, assuming formatted one int per line, from stdin"""
    return [int(line) for line in stdin]


if __name__ == "__main__":
    nums: List[int] = read_ints()
    print(solve_part_one(2020, nums))
    print(solve_part_two(2020, nums))
