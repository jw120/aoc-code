"""Advent of Code 2020 - Day 6."""

from doctest import testmod
from sys import stdin
from typing import List, Set


def part_one(group: List[List[str]]) -> int:
    def union_of_letters(xs: List[str]) -> int:
        xs_as_sets: List[Set[str]] = [set(x) for x in xs]
        return len(set.union(*xs_as_sets))

    return sum(union_of_letters(g) for g in groups)


def part_two(group: List[List[str]]) -> int:
    def intersection_of_letters(xs: List[str]) -> int:
        xs_as_sets: List[Set[str]] = [set(x) for x in xs]
        return len(set.intersection(*xs_as_sets))

    return sum(intersection_of_letters(g) for g in groups)


if __name__ == "__main__":
    testmod()
    groups: List[List[str]] = [g.split() for g in stdin.read().split("\n\n")]
    print(part_one(groups))
    print(part_two(groups))
