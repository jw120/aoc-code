"""Advent of Code 2019 - Day 6."""

from doctest import testmod
from sys import stdin
from typing import Dict, List, Set

test_one: List[List[str]] = [
    line.split(")")
    for line in [
        "COM)B",
        "B)C",
        "C)D",
        "D)E",
        "E)F",
        "B)G",
        "G)H",
        "D)I",
        "E)J",
        "J)K",
        "K)L",
    ]
]


def count_one(orbits_list: List[List[str]]) -> int:
    """Count number of orbits.

    >>> count_one(test_one)
    42
    """
    # Build tree
    orbits: Dict[str, Set[str]] = {}
    for [orbited, orbiting] in orbits_list:
        if orbited not in orbits:
            orbits[orbited] = set()
        if orbiting not in orbits:
            orbits[orbiting] = set()
        orbits[orbited].add(orbiting)
    orbiting_planets: Set[str] = set.union(*orbits.values())
    [root_planet] = set(orbits.keys()) - orbiting_planets

    # Walk the tree
    count: int = 0
    level: int = 0
    frontier: List[str] = [root_planet]
    while len(frontier) > 0:
        new_frontier: List[str] = []
        for planet in frontier:
            count += level
            new_frontier += orbits[planet]
        frontier = new_frontier
        level += 1
    return count


if __name__ == "__main__":
    testmod()
    orbits_list: List[List[str]] = [line.strip().split(")") for line in stdin]
    print(count_one(orbits_list))
