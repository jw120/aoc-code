"""Advent of Code 2019 - Day 1."""

from doctest import testmod
from sys import stdin
from typing import List


def fuel(mass: int) -> int:
    """Fuel required for a module of given mass.

    >>> fuel(12)
    2
    >>> fuel(1969)
    654
    """
    return mass // 3 - 2


def fuel2(mass: int) -> int:
    """Fuel, with part two definition, required for a module of given mass.

    >>> fuel2(14)
    2
    >>> fuel2(1969)
    966
    """
    total: int = 0
    m: int = mass
    while m > 0:
        m = max(0, fuel(m))
        total += m
    return total


if __name__ == "__main__":
    testmod()
    masses: List[int] = [int(line) for line in stdin]
    print(sum(fuel(mass) for mass in masses))
    print(sum(fuel2(mass) for mass in masses))
