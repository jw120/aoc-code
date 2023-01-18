"""Advent of Code 2020 - Day 6."""

from doctest import testmod
from sys import stdin

from utils import set_intersection, set_union


def part_one(groups: list[list[str]]) -> int:
    """Part one solution."""

    def union_of_letters(xs: list[str]) -> set[str]:
        xs_as_sets: list[set[str]] = [set(x) for x in xs]
        return set_union(xs_as_sets)

    return sum(len(union_of_letters(g)) for g in groups)


def part_two(groups: list[list[str]]) -> int:
    """Part two solution."""

    def intersection_of_letters(xs: list[str]) -> set[str]:
        xs_as_sets: list[set[str]] = [set(x) for x in xs]
        return set_intersection(xs_as_sets)

    return sum(len(intersection_of_letters(g)) for g in groups)


if __name__ == "__main__":
    testmod()
    input_groups: list[list[str]] = [g.split() for g in stdin.read().split("\n\n")]
    print(part_one(input_groups))
    print(part_two(input_groups))
