"""Advent of Code 2024 - Day 1."""

from collections import Counter
from sys import stdin


def parse_line(s: str) -> tuple[int, int]:
    """Parse one input line."""
    a, b = s.split()
    return int(a), int(b)


def total_distance(pairs: list[tuple[int, int]]) -> int:
    """Calculate total distance between the pairs."""
    xs, ys = zip(*pairs, strict=True)
    return sum(abs(x - y) for x, y in zip(sorted(xs), sorted(ys), strict=True))


def similarity(pairs: list[tuple[int, int]]) -> int:
    """Calculate total distance between the pairs."""
    y_counts = Counter(y for _x, y in pairs)
    return sum(x * y_counts.get(x, 0) for x, _y in pairs)


if __name__ == "__main__":
    pairs = [parse_line(s) for s in stdin.readlines()]
    print(total_distance(pairs))
    print(similarity(pairs))
