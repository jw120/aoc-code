"""Advent of Code 2021 - Day 9."""

from collections import Counter
from doctest import testmod
from math import prod
from sys import stdin
from typing import Set, Tuple

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

Coord = Tuple[int, int]


def find_minima(heights: list[list[int]]) -> list[Coord]:
    """Return the coordinates of the minima of the height map.

    >>> set(find_minima(test1)) ==  set([(0, 1), (0, 9), (2, 2), (4, 6)])
    True
    """
    i_size = len(heights)
    j_size = len(heights[0])
    minima: list[Tuple[int, int]] = []
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
            minima.append((i, j))
    return minima


def risk(heights: list[list[int]]) -> int:
    """Return the sum of the risk levels of all the minima of the height maps.

    Risk levels are 1 plus the height of each minima. Minima have strictly lower
    heights than all adjacent (up, down, left, right) positions.

    >>> risk(test1)
    15
    """
    return sum(heights[i][j] + 1 for i, j in find_minima(heights))


def basins(heights: list[list[int]]) -> int:
    """Return product of sizes of the three largest basins.

    >>> basins(test1)
    1134
    """
    minima: Set[Coord] = set(find_minima(heights))
    minima_counts: Counter[Coord] = Counter()
    i_size = len(heights)
    j_size = len(heights[0])

    def move_down(p: int, q: int) -> Coord:
        """Return an adjacent coordinate with lower height."""
        z = heights[p][q]
        if p - 1 >= 0 and heights[p - 1][q] < z:
            return (p - 1, q)
        if p + 1 < i_size and heights[p + 1][q] < z:
            return (p + 1, q)
        if q - 1 >= 0 and heights[p][q - 1] < z:
            return (p, q - 1)
        if q + 1 < j_size and heights[p][q + 1] < z:
            return (p, q + 1)
        raise ValueError("No way down")

    for i in range(i_size):
        for j in range(j_size):
            if heights[i][j] == 9:
                continue
            x, y = i, j
            while True:
                if (x, y) in minima:
                    minima_counts[(x, y)] += 1
                    break
                x, y = move_down(x, y)
    return prod(count for _minima, count in minima_counts.most_common(3))


if __name__ == "__main__":
    testmod()
    input_heights: list[list[int]] = [
        [int(c) for c in line] for line in stdin.read().splitlines()
    ]
    for row in input_heights[1:]:
        assert len(row) == len(input_heights[0])
    assert len(input_heights) == len(input_heights[0])
    print(risk(input_heights))
    print(basins(input_heights))
