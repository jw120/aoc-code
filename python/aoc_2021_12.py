"""Advent of Code 2021 - Day 12."""

# We (ab)use private members - don't complain
# pylint: disable=protected-access

from __future__ import annotations

from collections import deque
from doctest import testmod
from sys import stdin
from typing import Optional

Cave = str
Maze = dict[Cave, list[Cave]]

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


class Path:
    """Main class for day 12."""

    def __init__(self) -> None:
        self._final_cave: Optional[Cave] = None
        self._visited: frozenset[Cave] = frozenset()
        self._all_small_unique: bool = True

    def final_cave(self) -> Cave:
        """Return final cave."""
        if self._final_cave is None:
            raise ValueError("Empty path!")
        return self._final_cave

    def includes(self, cave: Cave) -> bool:
        """Test if the cave has been visited."""
        return cave in self._visited

    def all_small_unique(self) -> bool:
        """Return all small unique."""
        return self._all_small_unique

    def extend(self, cave: Cave) -> Path:
        """Extend the path to a new cave."""
        p = Path()
        p._final_cave = cave
        p._visited = self._visited | frozenset([cave])
        p._all_small_unique = self._all_small_unique and not (
            cave.islower() and self.includes(cave)
        )
        return p


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
    complete_paths: int = 0
    working_paths: deque[Path] = deque([Path().extend("start")])
    while working_paths:
        current_path: Path = working_paths.pop()
        if current_path.final_cave() == "end":
            complete_paths += 1
            continue
        for exit_cave in maze[current_path.final_cave()]:
            if (
                exit_cave.isupper()
                or not current_path.includes(exit_cave)
                or (allow_one_revisit and current_path.all_small_unique())
            ):
                new_path = current_path.extend(exit_cave)
                working_paths.append(new_path)
    return complete_paths


if __name__ == "__main__":
    testmod()
    input_maze = read_maze(stdin.read().splitlines())
    print(paths(input_maze, False))
    print(paths(input_maze, True))
