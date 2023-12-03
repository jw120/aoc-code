"""Advent of Code 2021 - Day 7."""

from collections.abc import Callable
from doctest import testmod
from sys import stdin

test_data: list[int] = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]


def fuel1(positions: list[int], target: int) -> int:
    """Return part 1 fuel need to move crabs from their initial positions to the target position.

    >>> [fuel1(test_data, i) for i in [2, 1, 3, 10]]
    [37, 41, 39, 71]
    """
    return sum(abs(x - target) for x in positions)


def fuel2(positions: list[int], target: int) -> int:
    """Return part 2 fuel need to move crabs from their initial positions to the target position.

    >>> [fuel2(test_data, i) for i in [5, 2]]
    [168, 206]
    """

    def sum_to(n: int) -> int:
        """Sum of 1, 2, 3... n."""
        return n * (n + 1) // 2

    return sum(sum_to(abs(x - target)) for x in positions)


def find_minimum(f: Callable[[int], int], bounds: tuple[int, int]) -> int:
    """Find the minimum value of f(x) within the given bounds x_min <= x <= x_max.

    >>> find_minimum(lambda x: fuel1(test_data, x), (0, 16))
    37
    >>> find_minimum(lambda x: fuel2(test_data, x), (0, 16))
    168
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
    input_crabs: list[int] = [int(x) for x in stdin.read().split(",")]
    input_bounds = (min(input_crabs), max(input_crabs))
    print(find_minimum(lambda x: fuel1(input_crabs, x), input_bounds))
    print(find_minimum(lambda x: fuel2(input_crabs, x), input_bounds))
