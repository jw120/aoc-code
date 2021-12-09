"""Advent of Code 2021 - Day 9."""

from doctest import testmod
from sys import stdin

test1: list[str] = [
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678",
]


def risk(h: list[list[int]]) -> int:
    """Return the sum of the risk levels of all the minima of the height maps.

    Risk levels are 1 plus the height of each minima. Minima have strictly lower
    heights than all adjacent (up, down, left, right) positions.
    """
    return 0


if __name__ == "__main__":
    testmod()
    heights: list[list[int]] = [
        [int(c) for c in line] for line in stdin.read().splitlines()
    ]
    for row in heights[1:]:
        assert len(row) == len(heights[0])
    assert len(heights) == len(heights[0])
    print(risk(heights))
