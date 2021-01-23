"""Advent of Code 2019 - Day 4."""

from collections.abc import Callable
from sys import stdin


def valid_one(
    a: int, b: int, c: int, d: int, e: int, f: int, low: int, high: int
) -> bool:
    n: int = 10 * (10 * (10 * (10 * (10 * a + b) + c) + d) + e) + f
    return (low <= n <= high) and (a == b or b == c or c == d or d == e or e == f)


def valid_two(
    a: int, b: int, c: int, d: int, e: int, f: int, low: int, high: int
) -> bool:
    n: int = 10 * (10 * (10 * (10 * (10 * a + b) + c) + d) + e) + f
    return (low <= n <= high) and (
        (a == b and b != c)
        or (b == c and a != b and c != d)
        or (c == d and b != c and d != e)
        or (d == e and c != d and e != f)
        or (e == f and d != e)
    )


def count(
    valid: Callable[[int, int, int, int, int, int, int, int], bool], low: int, high: int
) -> int:
    count: int = 0
    for a in range(0, 10):
        for b in range(a, 10):
            for c in range(b, 10):
                for d in range(c, 10):
                    for e in range(d, 10):
                        for f in range(e, 10):
                            if valid(a, b, c, d, e, f, low, high):
                                count += 1
    return count


if __name__ == "__main__":
    low, high = [int(s) for s in stdin.read().strip().split("-")]
    print(count(valid_one, low, high))
    print(count(valid_two, low, high))
