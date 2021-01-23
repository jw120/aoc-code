"""Advent of Code 2020 - Day 6."""

from doctest import testmod
from sys import stdin


def part_one(group: list[list[str]]) -> int:
    def union_of_letters(xs: list[str]) -> set[str]:
        xs_as_sets: list[set[str]] = [set(x) for x in xs]
        return set.union(*xs_as_sets)

    return sum(len(union_of_letters(g)) for g in groups)


def part_two(group: list[list[str]]) -> int:
    def intersection_of_letters(xs: list[str]) -> set[str]:
        xs_as_sets: list[set[str]] = [set(x) for x in xs]
        return set.intersection(*xs_as_sets)

    return sum(len(intersection_of_letters(g)) for g in groups)


if __name__ == "__main__":
    testmod()
    groups: list[list[str]] = [g.split() for g in stdin.read().split("\n\n")]
    print(part_one(groups))
    print(part_two(groups))
