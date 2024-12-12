"""Advent of Code 2024 - Day 11."""

from collections import Counter
from sys import stdin


def blink(x: int) -> list[int]:
    """Blink one stone."""
    if x == 0:
        return [1]
    s = str(x)
    n = len(s)
    if n % 2 == 0:
        return [int(s[: n // 2]), int(s[n // 2 :])]
    return [x * 2024]


def blink_line(counts: Counter[int]) -> Counter[int]:
    """Blink a line of stones."""
    new_counts: Counter[int] = Counter()
    for x, n in counts.items():
        for y in blink(x):
            new_counts[y] += n
    return new_counts


if __name__ == "__main__":
    counts = Counter(int(s) for s in stdin.readline().strip().split())
    for i in range(76):
        if i in {25, 75}:
            print(counts.total())
        counts = blink_line(counts)
