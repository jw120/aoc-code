"""Advent of Code 2024 - Day 23."""

from itertools import combinations
from sys import stdin


def parse(lines: list[str]) -> dict[str, set[str]]:
    """Read connections."""
    connections: dict[str, set[str]] = {}
    for line in lines:
        x, y = line.strip().split("-")
        if x not in connections:
            connections[x] = {y}
        else:
            connections[x].add(y)
        if y not in connections:
            connections[y] = {x}
        else:
            connections[y].add(x)
    return connections


def triples(connections: dict[str, set[str]]) -> set[tuple[str, ...]]:
    """Return all sets of three interconnected computers."""
    found: set[tuple[str, ...]] = set()
    for s1 in connections:  # noqa: PLC0206
        for s2, s3 in combinations(connections[s1], 2):
            if s3 in connections[s2]:
                found.add(tuple(sorted([s1, s2, s3])))
    return found


if __name__ == "__main__":
    connections = parse(stdin.readlines())
    print(sum(any(s.startswith("t") for s in triple) for triple in triples(connections)))
