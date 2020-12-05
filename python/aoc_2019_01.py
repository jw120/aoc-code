"""Advent of Code 2019 - Day 1."""

from doctest import testmod
from sys import stdin
from typing import List


def fuel_one(mass: int) -> int:
    """Fuel required for a module of given mass.

    >>> fuel_one(12)
    2
    >>> fuel_one(1969)
    654
    """
    return mass // 3 - 2


def fuel_two(mass: int) -> int:
    """Fuel, with part two definition, required for a module of given mass.

    >>> fuel_two(14)
    2
    >>> fuel_two(1969)
    966
    """
    total: int = 0
    m: int = mass
    while m > 0:
        m = max(0, fuel_one(m))
        total += m
    return total


if __name__ == "__main__":
    testmod()
    masses: List[int] = [int(line) for line in stdin]
    print(sum(fuel_one(mass) for mass in masses))
    print(sum(fuel_two(mass) for mass in masses))
