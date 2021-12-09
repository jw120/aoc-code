"""Advent of Code 2021 - Day 9."""

from doctest import testmod
from sys import stdin

test1: list[list[int]] = [
    [int(c) for c in line]
    for line in [
        "2199943210",
        "3987894921",
        "9856789892",
        "8767896789",
        "9899965678",
    ]
]


def risk(heights: list[list[int]]) -> int:
    """Return the sum of the risk levels of all the minima of the height maps.

    Risk levels are 1 plus the height of each minima. Minima have strictly lower
    heights than all adjacent (up, down, left, right) positions.

    >>> risk(test1)
    15
    """
    i_size = len(heights)
    j_size = len(heights[0])
    total = 0
    for i in range(i_size):
        for j in range(j_size):
            z = heights[i][j]
            if i + 1 < i_size and heights[i + 1][j] <= z:
                continue
            if i - 1 >= 0 and heights[i - 1][j] <= z:
                continue
            if j + 1 < j_size and heights[i][j + 1] <= z:
                continue
            if j - 1 >= 0 and heights[i][j - 1] <= z:
                continue
            total += z + 1
    return total


if __name__ == "__main__":
    testmod()
    heights: list[list[int]] = [
        [int(c) for c in line] for line in stdin.read().splitlines()
    ]
    for row in heights[1:]:
        assert len(row) == len(heights[0])
    assert len(heights) == len(heights[0])
    print(risk(heights))
