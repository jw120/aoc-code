"""Advent of Code 2021 - Day 5."""

import re
from collections import Counter
from doctest import testmod
from sys import stdin
from typing import Tuple

Coord = Tuple[int, int]
Segment = Tuple[Coord, Coord]


segment_pattern = re.compile(r"(\d+),(\d+) -> (\d+),(\d+)")


def read_segment(s: str) -> Segment:
    """Read a segment. Ensuring ordering by x then y."""
    match = segment_pattern.fullmatch(s)
    if not match:
        raise ValueError("Can't read segment: " + s)
    x1, y1, x2, y2 = (
        int(match.group(1)),
        int(match.group(2)),
        int(match.group(3)),
        int(match.group(4)),
    )
    if x1 < x2:
        return ((x1, y1), (x2, y2))
    if x1 > x2:
        return ((x2, y2), (x1, y1))
    return ((x1, y1), (x2, y2)) if y1 < y2 else ((x2, y2), (x1, y1))


test_data: list[Segment] = [
    read_segment(s)
    for s in [
        "0,9 -> 5,9",
        "8,0 -> 0,8",
        "9,4 -> 3,4",
        "2,2 -> 2,1",
        "7,0 -> 7,4",
        "6,4 -> 2,0",
        "0,9 -> 2,9",
        "3,4 -> 1,4",
        "0,0 -> 8,8",
        "5,5 -> 8,2",
    ]
]


def multiples(segments: list[Segment], use_diagonals: bool) -> int:
    """Fine number of coords with multiple coverage.

    >>> multiples(test_data, False)
    5
    >>> multiples(test_data, True)
    12
    """
    counts: Counter[Coord] = Counter()
    for ((x1, y1), (x2, y2)) in segments:
        if x1 == x2:
            for y in range(y1, y2 + 1):
                counts[(x1, y)] += 1
        elif y1 == y2:
            for x in range(x1, x2 + 1):
                counts[(x, y1)] += 1
        elif use_diagonals:
            assert abs(y2 - y1) == x2 - x1
            direction = 1 if y2 > y1 else -1
            for i in range(0, x2 - x1 + 1):
                counts[(x1 + i, y1 + i * direction)] += 1
    return len(list(filter(lambda n: n > 1, counts.values())))


if __name__ == "__main__":
    testmod()
    segments: list[Segment] = [read_segment(s) for s in stdin.read().splitlines()]
    print(multiples(segments, False))
    print(multiples(segments, True))
