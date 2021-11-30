"""Advent of Code 2019 - Day 18."""

from __future__ import annotations

from collections import deque
from dataclasses import dataclass
from sys import stdin
from typing import Callable, Dict, List, Optional, Set, Tuple, TypeVar


@dataclass(eq=True, frozen=True)
class Coord:
    """Coordinates of a position in our grid."""

    row: int
    col: int

    def out_of_bounds(self, num_rows: int, num_cols: int) -> bool:
        """Test if this position is outside the given bounds."""
        return (
            self.row < 0 or self.row >= num_rows or self.col < 0 or self.col >= num_cols
        )


@dataclass(eq=True, frozen=True)
class Walker:
    """State of our walker - our position and the keys we hold."""

    position: Coord
    keys: frozenset[str]

    def add_key(self, k: Optional[str]) -> Walker:
        """Return a new walker if the key is new."""
        if k is None or k in self.keys:
            return self
        return Walker(self.position, self.keys | {k})

    def adjacents(self) -> List[Walker]:
        """Return the four adjacent positions."""
        return [
            Walker(Coord(self.position.row - 1, self.position.col), self.keys),
            Walker(Coord(self.position.row, self.position.col + 1), self.keys),
            Walker(Coord(self.position.row + 1, self.position.col), self.keys),
            Walker(Coord(self.position.row, self.position.col - 1), self.keys),
        ]


class Maze:
    def __init__(self, lines: List[str]) -> None:

        self._rows: int = len(lines)
        self._cols: int = len(lines[0])
        self._doors: Dict[Coord, str] = {}
        self._keys: Dict[Coord, str] = {}
        self.start: Coord
        self._walls: List[List[bool]] = [
            [self._add_char(x, row, col) for col, x in enumerate(line)]
            for row, line in enumerate(lines)
        ]
        self._all_keys: Set[str] = set(self._keys.values())

        assert self.start is not None
        assert len(self._walls) == self._rows
        assert all(len(row) == self._cols for row in self._walls)
        assert set(self._doors.values()) <= set(map(str.upper, self._keys.values()))

    def _add_char(self, x: str, row: int, col: int) -> bool:
        """Add a single maze location from the read input (helper function for __init__)."""
        if x == "@":
            self.start = Coord(row, col)
        elif x.isupper():
            self._doors[Coord(row, col)] = x
        elif x.islower():
            self._keys[Coord(row, col)] = x
        elif x == "#":
            return True
        else:
            assert x == "."
        return False

    def available(self, w: Walker) -> List[Walker]:
        """Return the states which can be moved to."""
        return [
            x.add_key(self._keys.get(x.position))
            for x in w.adjacents()
            if self._open(x)
        ]

    def has_all_keys(self, w: Walker) -> bool:
        """Have all keys been collected."""
        return w.keys == self._all_keys

    def _open(self, w: Walker) -> bool:
        """Can we move to the given walker state."""
        #        print("Is open", w)
        if w.position.out_of_bounds(self._rows, self._cols):
            #            print("OOB")
            return False
        if self.wall(w.position):
            #            print("Wall")
            return False
        if w.position in self._doors:
            #            print("Door")
            return self._doors[w.position].lower() in w.keys
        #        print("Clear")
        return True

    def wall(self, p: Coord) -> bool:
        """Is the coordinate a wall."""
        return self._walls[p.row][p.col]

    def _show_location(self, row: int, col: int) -> str:
        p = Coord(row, col)
        if p == self.start:
            return "@"
        if p in self._doors:
            return self._doors[p]
        if p in self._keys:
            return self._keys[p]
        return "#" if self.wall(p) else "."

    def _show_row(self, row: int) -> str:
        return "".join(self._show_location(row, col) for col in range(self._cols))

    def show(self) -> str:
        return "\n".join(self._show_row(row) for row in range(self._rows))


S = TypeVar("S")


def bfs(
    start: S, at_goal: Callable[[S], bool], available: Callable[[S], List[S]]
) -> Optional[int]:
    """Conduct basic BFS search and return length of minimum path."""
    # Queue of states to visit
    q: deque[Tuple[S, int]] = deque([(start, 0)])
    # Distances from start for states visited or in queue
    distance: Dict[S, int] = {start: 0}
    while q:
        s, dist = q.popleft()
        #        print("Dequeue", s, dist)
        if at_goal(s):
            return dist
        new_states = [(a, dist + 1) for a in available(s) if a not in distance]
        q.extend(new_states)
        #       print("Enqueue", new_states)
        distance |= dict(new_states)
    return None


test1 = """
#########
#b.A.@.a#
#########"""[
    1:
]

test2 = """
########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################"""[
    1:
]

test3 = """
########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################"""[
    1:
]

test4 = """
#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################"""[
    1:
]

test5 = """
########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################"""[
    1:
]

if __name__ == "__main__":
    maze = Maze(stdin.read().splitlines())
    # maze = Maze(test5.splitlines())
    print(maze.show())
    start = Walker(maze.start, frozenset())
    solution = bfs(start, lambda s: maze.has_all_keys(s), lambda s: maze.available(s))
    print(solution)
