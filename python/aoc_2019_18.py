"""Advent of Code 2019 - Day 18."""

from __future__ import annotations

from collections import deque
from dataclasses import dataclass
from enum import Enum
from sys import stdin
from typing import Callable, Dict, List, NoReturn, Optional, Tuple, TypeVar


def assert_never(value: NoReturn) -> NoReturn:
    assert False, f"Unhandled value: {value} ({type(value).__name__})"


@dataclass(eq=True, frozen=True)
class Coord:
    row: int
    col: int

    @staticmethod
    def origin() -> Coord:
        return Coord(0, 0)

    def move(self, d: Direction) -> Coord:
        if d is Direction.UP:
            return Coord(self.row - 1, self.col)
        elif d is Direction.RIGHT:
            return Coord(self.row, self.col + 1)
        if d is Direction.DOWN:
            return Coord(self.row + 1, self.col)
        elif d is Direction.LEFT:
            return Coord(self.row, self.col - 1)
        else:
            assert_never(d)


class Direction(Enum):
    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT = 3

    @staticmethod
    def from_char(c: str) -> Optional[Direction]:
        if i := "^>v<".find(c) >= 0:
            return Direction(i)
        return None

    def to_char(self) -> str:
        return "^>v<"[self.value]

    def opposite(self) -> Direction:
        return Direction((self.value + 2) % 4)


class Maze:
    def __init__(self, lines: List[str]) -> None:

        self.rows: int = len(lines)
        self.cols: int = len(lines)
        self.doors: Dict[Coord, str] = {}
        self.keys: Dict[Coord, str] = {}
        self.start: Coord
        self.walls: List[List[bool]] = [
            [self._add_char(x, row, col) for col, x in enumerate(line)]
            for row, line in enumerate(lines)
        ]

        assert self.start is not None
        assert len(self.walls) == self.rows
        assert all(len(row) == self.cols for row in self.walls)
        assert sorted(self.doors.values()) == list(
            map(str.upper, sorted(self.keys.values()))
        )

    def _add_char(self, x: str, row: int, col: int) -> bool:
        """Add a single maze location from the read input (helper function for __init__)."""
        if x == "@":
            self.start = Coord(row, col)
        elif x.isupper():
            self.doors[Coord(row, col)] = x
        elif x.islower():
            self.keys[Coord(row, col)] = x
        elif x == "#":
            return True
        else:
            assert x == "."
        return False

    def wall(self, p: Coord) -> bool:
        return self.walls[p.row][p.col]

    def _show_location(self, row: int, col: int) -> str:
        p = Coord(row, col)
        if p == self.start:
            return "@"
        if p in self.doors:
            return self.doors[p]
        if p in self.keys:
            return self.keys[p]
        return "#" if self.wall(p) else "."

    def _show_row(self, row: int) -> str:
        return "".join(self._show_location(row, col) for col in range(self.cols))

    def show(self) -> str:
        return "\n".join(self._show_row(row) for row in range(self.rows))


S = TypeVar("S")


def bfs(start: S, goal: S, adjacent: Callable[[S], List[S]]) -> Optional[int]:
    # Queue of states to visit
    q: deque[Tuple[S, int]] = deque([(start, 0)])
    # Distances from start for states visited or in queue
    distance: Dict[S, int] = {start: 0}
    while q:
        s, dist = q.popleft()
        if s == goal:
            return dist
        new_states = [(a, dist + 1) for a in adjacent(s) if a not in distance]
        q.extend(new_states)
        distance |= dict(new_states)
    return None


if __name__ == "__main__":
    maze = Maze(stdin.read().splitlines())
    print(maze.show())
