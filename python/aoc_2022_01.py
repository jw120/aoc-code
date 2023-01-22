"""Advent of Code 2022 - Day 1."""

from doctest import testmod
from sys import stdin


def sum_groups(xs: list[int | None]) -> list[int]:
    """Return sums of groups of lines separated by None.

    >>> sum_groups([1, 2, 3, None, 4, None, 5, 7])
    [6, 4, 12]
    """
    ys: list[int] = []
    group_sum: int = 0
    group_count: int = 0
    for x in xs:
        if x is None:
            ys.append(group_sum)
            group_sum = 0
            group_count = 0
        else:
            group_sum += x
            group_count += 1
    if group_count:
        ys.append(group_sum)
    return ys


if __name__ == "__main__":
    testmod()
    calories: list[int | None] = [
        (int(line) if line.strip() else None) for line in stdin
    ]
    elves = sorted(sum_groups(calories), reverse=True)
    print(elves[0])
    print(sum(elves[:3]))
