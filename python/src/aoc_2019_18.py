"""Advent of Code 2019 - Day 18."""

from __future__ import annotations

from dataclasses import dataclass
from sys import stdin

from coord import Coord, Extent
from search import bfs


@dataclass(eq=True, frozen=True)
class Walker:
    """State of our walker - our position and the keys we hold."""

    position: Coord
    keys: frozenset[str]

    def add_key(self, k: str | None) -> Walker:
        """Return a new walker if the key is new."""
        if k is None or k in self.keys:
            return self
        return Walker(self.position, self.keys | {k})

    def adjacents(self) -> list[Walker]:
        """Return the four adjacent positions."""
        return [
            Walker(Coord(self.position.x - 1, self.position.y), self.keys),
            Walker(Coord(self.position.x, self.position.y + 1), self.keys),
            Walker(Coord(self.position.x + 1, self.position.y), self.keys),
            Walker(Coord(self.position.x, self.position.y - 1), self.keys),
        ]


class Maze:
    """Main class for day 18."""

    def __init__(self, lines: list[str]) -> None:

        self._extent: Extent = Extent(len(lines), len(lines[0]))
        self._doors: dict[Coord, str] = {}
        self._keys: dict[Coord, str] = {}
        self.start: Coord
        self._walls: list[list[bool]] = [
            [self._add_char(x, row, col) for col, x in enumerate(line)]
            for row, line in enumerate(lines)
        ]
        self._all_keys: frozenset[str] = frozenset(self._keys.values())

        assert self.start is not None
        assert len(self._walls) == self._extent.x
        assert all(len(row) == self._extent.y for row in self._walls)
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

    def available(self, w: Walker) -> list[Walker]:
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
        if not w.position.in_bounds(self._extent):
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
        return self._walls[p.x][p.y]

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
        return "".join(self._show_location(row, col) for col in range(self._extent.y))

    def show(self) -> str:
        """Return debugging information."""
        return "\n".join(self._show_row(row) for row in range(self._extent.x))


TEST1 = """
#########
#b.A.@.a#
#########"""[
    1:
]

TEST2 = """
########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################"""[
    1:
]

TEST3 = """
########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################"""[
    1:
]

TEST4 = """
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

TEST5 = """
########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################"""[
    1:
]

if __name__ == "__main__":
    input_maze = Maze(stdin.read().splitlines())
    print(input_maze.show())
    input_start = Walker(input_maze.start, frozenset())
    solution = bfs(input_start, input_maze.has_all_keys, input_maze.available)
    print(solution)
