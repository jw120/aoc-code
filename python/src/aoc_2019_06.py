"""Advent of Code 2019 - Day 6."""

from doctest import testmod
from sys import stdin
from typing import TypeVar

from utils import set_union

K = TypeVar("K")


def build_tree(
    links: list[tuple[K, K]]
) -> tuple[dict[K, set[K]], dict[K, K | None], list[K]]:
    """Build a representation of a tree.

    Given a list of key->key links, construct dicts of all forward (one->many)
    and backward (one->one) links in the tree and identify the root nodes.

    >>> build_tree([(1, 2), (1, 3), (2, 4),(5, 6)])[0]
    {1: {2, 3}, 2: {4}, 3: set(), 4: set(), 5: {6}, 6: set()}
    >>> build_tree([(1, 2), (1, 3), (2, 4),(5, 6)])[1]
    {1: None, 2: 1, 3: 1, 4: 2, 5: None, 6: 5}
    >>> build_tree([(1, 2), (1, 3), (2, 4),(5, 6)])[2]
    [1, 5]
    """
    forward: dict[K, set[K]] = {}
    backward: dict[K, K | None] = {}
    k_from: K
    k_to: K
    for (k_from, k_to) in links:
        forward.setdefault(k_from, set())
        forward.setdefault(k_to, set())
        if k_to in backward and backward[k_to] is not None:
            raise RuntimeError("Duplicate back-link", (k_from, k_to))
        backward.setdefault(k_from, None)
        forward[k_from].add(k_to)
        backward[k_to] = k_from
    forward_out_nodes: set[K] = set(forward.keys())
    forward_in_nodes: set[K] = set_union(forward.values())
    roots = forward_out_nodes - forward_in_nodes
    return (forward, backward, list(roots))


def parse_link(s: str) -> tuple[str, str]:
    """Read links from a string."""
    [x, y] = s.split(")")
    return (x, y)


test_links: list[tuple[str, str]] = [
    parse_link(s)
    for s in [
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
        "K)YOU",
        "I)SAN",
    ]
]
(test_forward, test_backward, [test_root]) = build_tree(test_links)


def label_root_distance(root: K, forward: dict[K, set[K]]) -> dict[K, int]:
    """Return labels for each root with their distance from the root.

    >>> label_root_distance(test_root, test_forward)["COM"]
    0
    >>> label_root_distance(test_root, test_forward)["E"]
    4
    """
    labels: dict[K, int] = {}
    dist: int = 0
    frontier: set[K] = {root}
    while len(frontier) > 0:
        new_frontier: set[K] = set()
        for x in frontier:
            labels[x] = dist
            new_frontier |= forward[x]
        frontier = new_frontier
        dist += 1
    return labels


def distance(a: K, b: K, backward: dict[K, K | None], root_dist: dict[K, int]) -> int:
    """Return distance between two nodes along the tree.

    Walk backwards from each node until we find an overlapping node. Distance
    is the sum of the number of steps in each path to reach the overlap

    >>> distance("YOU", "SAN", test_backward, label_root_distance(test_root, test_forward))
    4
    """
    a_visited: set[K] = {a}
    b_visited: set[K] = {b}
    a_walk: K = a
    b_walk: K = b
    while (overlap := a_visited & b_visited) == set():
        a_back: K | None = backward[a_walk]
        b_back: K | None = backward[b_walk]
        if a_back is None or b_back is None:
            raise RuntimeError("No route found")
        a_walk = a_back
        b_walk = b_back
        a_visited.add(a_walk)
        b_visited.add(b_walk)
    [intersection] = overlap
    return (root_dist[a] - root_dist[intersection] - 1) + (
        root_dist[b] - root_dist[intersection] - 1
    )


if __name__ == "__main__":
    testmod()
    orbits: list[tuple[str, str]] = [parse_link(line.strip()) for line in stdin]
    (input_forward, input_backward, [input_root]) = build_tree(orbits)
    input_root_dist: dict[str, int] = label_root_distance(input_root, input_forward)
    print(sum(input_root_dist.values()))
    print(distance("SAN", "YOU", input_backward, input_root_dist))
