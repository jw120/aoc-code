"""Advent of Code 2021 - Day 1."""

from doctest import testmod
from sys import stdin


def number_of_increases(xs: list[int]) -> int:
    """Return number of values in the list that are higher than the preceding value.

    >>> number_of_increases([199, 200, 208, 210, 200, 207, 240, 269, 260, 263])
    7
    >>> number_of_increases([])
    0
    >>> number_of_increases([7])
    0
    """
    return sum(x < y for x, y in zip(xs, xs[1:]))


def sliding(xs: list[int]) -> list[int]:
    """Apply a three-value sliding window sum to the list.

    >>> sliding([1,2,3,4,5])
    [6, 9, 12]
    >>> sliding([])
    []
    >>> sliding([1, 2])
    []
    """
    return [x + y + z for x, y, z in zip(xs, xs[1:], xs[2:])]


if __name__ == "__main__":
    testmod()
    depths: list[int] = [int(line) for line in stdin]
    print(number_of_increases(depths))
    print(number_of_increases(sliding(depths)))
