"""Advent of Code 2021 - Day 7."""

from doctest import testmod
from sys import stdin
from typing import Callable, Tuple

test_data: list[int] = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]


def fuel(positions: list[int], target: int) -> int:
    """Return fuel need to move crabs from their initial positions to the target position.

    >>> [fuel(test_data, i) for i in [2, 1, 3, 10]]
    [37, 41, 39, 71]
    """
    return sum(abs(x - target) for x in positions)


def find_minimum(f: Callable[[int], int], bounds: Tuple[int, int]) -> int:
    """Find the minimum value of f(x) within the given bounds x_min <= x <= x_max.

    >>> find_minimum(lambda x: fuel(test_data, x), (0, 16))
    37
    """
    x_min, x_max = bounds
    while True:
        if x_min == x_max:
            return f(x_min)
        x: int = (x_min + x_max) // 2
        z: int = f(x)
        if x > x_min and f(x - 1) < z:
            x_max = x - 1
        elif x < x_max and f(x + 1) < z:
            x_min = x + 1
        elif f(x - 1) > z and f(x + 1) > z:
            return z
        else:
            raise ValueError("confused in find_minimum at", x)


if __name__ == "__main__":
    testmod()
    crabs: list[int] = [int(x) for x in stdin.read().split(",")]
    print(find_minimum(lambda x: fuel(crabs, x), (min(crabs), max(crabs))))
