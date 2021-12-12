"""Advent of Code 2021 - Day 12."""

# from __future__ import annotations

from collections import deque
from doctest import testmod
from sys import stdin

Cave = str
Maze = dict[Cave, list[Cave]]
Path = deque[str]

test1: list[str] = ["start-A", "start-b", "A-c", "A-b", "b-d", "A-end", "b-end"]

test2: list[str] = [
    "dc-end",
    "HN-start",
    "start-kj",
    "dc-start",
    "dc-HN",
    "LN-dc",
    "HN-end",
    "kj-sa",
    "kj-HN",
    "kj-dc",
]

test3: list[str] = [
    "fs-end",
    "he-DX",
    "fs-he",
    "start-DX",
    "pj-DX",
    "end-zg",
    "zg-sl",
    "zg-pj",
    "pj-he",
    "RW-he",
    "fs-DX",
    "pj-RW",
    "zg-RW",
    "start-pj",
    "he-WI",
    "zg-he",
    "pj-fs",
    "start-RW",
]


def read_maze(links: list[str]) -> Maze:
    """Read a maze from a list of links.

    >>> sorted(read_maze(test1)['b'])
    ['A', 'd', 'end']
    """
    maze: Maze = {}
    for from_cave, to_cave in [s.split("-") for s in links]:
        if from_cave != "end" and to_cave != "start":
            maze[from_cave] = maze.get(from_cave, []) + [to_cave]
        if from_cave != "start" and to_cave != "end":
            maze[to_cave] = maze.get(to_cave, []) + [from_cave]
    return maze


def all_lower_unique(p: Path) -> bool:
    """Test if all lower case caves in the path unique.

    >>> all_lower_unique(deque(["start", "a", "B", "c", "B", "d"]))
    True
    >>> all_lower_unique(deque(["start", "a", "B", "c", "B", "a"]))
    False
    """
    visited: set[str] = set()
    for x in p:
        if x.islower():
            if x in visited:
                return False
            visited.add(x)
    return True


def paths(maze: Maze, allow_one_revisit: bool) -> int:
    """Return number of paths from the start to the end.

    Paths can only visit small caves (with lower case names) except that one small caves
    can be visited twice if allow_one_revisit is True.

    >>> paths(read_maze(test1), False)
    10
    >>> paths(read_maze(test2), False)
    19
    >>> paths(read_maze(test3), False)
    226
    >>> paths(read_maze(test1), True)
    36
    >>> paths(read_maze(test2), True)
    103
    >>> paths(read_maze(test3), True)
    3509
    """
    complete_paths: deque[Path] = deque()
    working_paths: deque[Path] = deque([deque(["start"])])
    while working_paths:
        current_path: Path = working_paths.pop()
        current_path_final_cave: Cave = current_path[-1]
        if current_path_final_cave == "end":
            complete_paths.append(current_path)
            continue
        for exit_cave in maze[current_path_final_cave]:
            if (
                exit_cave.isupper()
                or exit_cave not in current_path
                or (allow_one_revisit and all_lower_unique(current_path))
            ):
                new_path = current_path.copy()
                new_path.append(exit_cave)
                working_paths.append(new_path)
    # for p in complete_paths:
    #     print(list(p))
    return len(complete_paths)


if __name__ == "__main__":
    testmod()
    maze = read_maze(stdin.read().splitlines())
    print(paths(maze, False))
    print(paths(maze, True))
