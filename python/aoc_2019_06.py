"""Advent of Code 2019 - Day 6."""

from doctest import testmod
from sys import stdin
from typing import Dict, List, Optional, Set, Tuple, TypeVar


K = TypeVar("K")


def build_tree(
    links: List[Tuple[K, K]]
) -> Tuple[Dict[K, Set[K]], Dict[K, Optional[K]], List[K]]:
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
    forward: Dict[K, Set[K]] = {}
    backward: Dict[K, Optional[K]] = {}
    k_from: K
    k_to: K
    for (k_from, k_to) in links:
        if k_from not in forward:
            forward[k_from] = set()
        if k_to not in forward:
            forward[k_to] = set()
        if k_to in backward and backward[k_to] is not None:
            raise RuntimeError("Duplicate back-link", (k_from, k_to))
        if k_from not in backward:
            backward[k_from] = None
        forward[k_from].add(k_to)
        backward[k_to] = k_from
    forward_out_nodes: Set[K] = set(forward.keys())
    forward_in_nodes: Set[K] = set.union(*forward.values())
    roots = forward_out_nodes - forward_in_nodes
    return (forward, backward, list(roots))


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


def label_root_distance(root: K, forward: Dict[K, Set[K]]) -> Dict[K, int]:
    """Return labels for each root with their distance from the root."""
    labels: Dict[K, int] = {}
    distance: int = 0
    frontier: Set[K] = {root}
    while len(frontier) > 0:
        new_frontier: Set[K] = set()
        for x in frontier:
            labels[x] = distance
            new_frontier |= forward[x]
        frontier = new_frontier
        distance += 1
    return labels


def parse_link(s: str) -> Tuple[str, str]:
    [x, y] = s.split(")")
    return (x, y)


test2: List[Tuple[str, str]] = [
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
(forward2, backward2, [root2]) = build_tree(test2)
root_dist2 = label_root_distance(root2, forward2)


def distance(
    a: K, b: K, backward: Dict[K, Optional[K]], root_dist: Dict[K, int]
) -> int:
    """Return distance between two nodes along the tree.

    Walk backwards from each node until we find an overlapping node. Distance
    is the sum of the number of steps in each path to reach the overlap

    >>> distance("I", "K", backward2, root_dist2)
    4
    """
    a_visited: Set[K] = {a}
    b_visited: Set[K] = {b}
    a_walk: K = a
    b_walk: K = b
    while (overlap := a_visited & b_visited) == set():
        print(a_walk, b_walk)
        a_back: Optional[K] = backward[a_walk]
        b_back: Optional[K] = backward[b_walk]
        if a_back is None or b_back is None:
            raise RuntimeError("No route found")
        a_walk = a_back
        b_walk = b_back
        a_visited.add(a_walk)
        b_visited.add(b_walk)
    [intersection] = overlap
    print("Intersection", intersection)
    print("Root dists", root_dist[a], root_dist[b], root_dist[intersection])
    return (root_dist[a] - root_dist[intersection]) + (
        root_dist[b] - root_dist[intersection]
    )


if __name__ == "__main__":
    testmod()
    orbits: List[Tuple[str, str]] = [parse_link(line.strip()) for line in stdin]
    (forward, backward, [root]) = build_tree(orbits)
    root_dist: Dict[str, int] = label_root_distance(root, forward)
    print(sum(root_dist.values()))
    print(distance("SAN", "YOU", backward, root_dist))
