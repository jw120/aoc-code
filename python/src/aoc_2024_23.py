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


def largest_group(connections: dict[str, set[str]]) -> set[str]:
    """Return computers in the largest group with all connected."""
    unassigned_computers: set[str] = set(connections.keys())
    largest_set: set[str] = set()
    while unassigned_computers:
        frontier: set[str] = {unassigned_computers.pop()}
        visited: set[str] = set()
        new_set: set[str] = set()
        while frontier:
            x = frontier.pop()
            visited.add(x)
            if all(y in connections[x] for y in new_set):
                new_set.add(x)
            for y in connections[x]:
                if y not in new_set and y not in visited:
                    frontier.add(y)
        if len(new_set) > len(largest_set):
            largest_set = new_set.copy()
    return largest_set


if __name__ == "__main__":
    connections = parse(stdin.readlines())
    print(sum(any(s.startswith("t") for s in triple) for triple in triples(connections)))
    print(",".join(sorted(largest_group(connections))))
