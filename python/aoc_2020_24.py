"""Advent of Code 2020 - Day 24."""

from __future__ import annotations

import re
from dataclasses import dataclass
from doctest import testmod
from sys import stdin


@dataclass(eq=True, frozen=True)
class HexCoord:
    """Hexagonal grid coordinates."""

    x: int
    y: int

    # "Odd-r" layout from https://www.redblobgames.com/grids/hexagons/

    def move(self, s: str) -> HexCoord:
        """Generate new coordinate after a move."""
        if s == "e":
            return HexCoord(self.x + 1, self.y)
        if s == "w":
            return HexCoord(self.x - 1, self.y)
        if s not in ["ne", "se", "nw", "sw"]:
            raise RuntimeError("Bad move direction", s)
        next_y = self.y + 1 if s[0] == "s" else self.y - 1
        if self.y % 2 == 0:
            next_x = self.x - (s[1] == "w")
        else:
            next_x = self.x + (s[1] == "e")
        return HexCoord(next_x, next_y)

    def adjoining(self) -> set[HexCoord]:
        """Return set of adjoining coordinates."""
        return {self.move(d) for d in ["e", "w", "ne", "se", "nw", "sw"]}


def split_moves(s: str) -> list[str]:
    """Split input string into a list of moves."""
    return [m[0] for m in re.finditer("[ns]?[ew]", s)]


test1: list[list[str]] = [
    split_moves(s)
    for s in [
        "sesenwnenenewseeswwswswwnenewsewsw",
        "neeenesenwnwwswnenewnwwsewnenwseswesw",
        "seswneswswsenwwnwse",
        "nwnwneseeswswnenewneswwnewseswneseene",
        "swweswneswnenwsewnwneneseenw",
        "eesenwseswswnenwswnwnwsewwnwsene",
        "sewnenenenesenwsewnenwwwse",
        "wenwwweseeeweswwwnwwe",
        "wsweesenenewnwwnwsenewsenwwsesesenwne",
        "neeswseenwwswnwswswnw",
        "nenwswwsewswnenenewsenwsenwnesesenew",
        "enewnwewneswsewnwswenweswnenwsenwsw",
        "sweneswneswneneenwnewenewwneswswnese",
        "swwesenesewenwneswnwwneseswwne",
        "enesenwswwswneneswsenwnewswseenwsese",
        "wnwnesenesenenwwnenwsewesewsesesew",
        "nenewswnwewswnenesenwnesewesw",
        "eneswnwswnwsenenwnwnwwseeswneewsenese",
        "neswnwewnwnwseenwseesewsenwsweewe",
        "wseweeenwnesenwwwswnew",
    ]
]


def walk(path_lists: list[list[str]]) -> set[HexCoord]:
    """Walk along given paths and return number of black tiles.

    >>> len(walk(test1))
    10
    """
    black_tiles: set[HexCoord] = set()
    for path in path_lists:
        pos = HexCoord(0, 0)
        for m in path:
            pos = pos.move(m)
        if pos in black_tiles:
            black_tiles.remove(pos)
        else:
            black_tiles.add(pos)
    return black_tiles


class Floor:
    """Floor class."""

    def __init__(self, starting_black_tiles: set[HexCoord]) -> None:
        self.days: int = 0
        self.g: list[set[HexCoord]] = [starting_black_tiles, set()]

    def active(self) -> int:
        """Return the active index."""
        return self.days % 2

    def inactive(self) -> int:
        """Return the inactive index."""
        return (self.days + 1) % 2

    def count_black_neighbours(self, t: HexCoord) -> int:
        """Return number of black neighbours."""
        return len(t.adjoining() & self.g[self.active()])

    def count_black_cells(self) -> int:
        """Return number of black cells."""
        return len(self.g[self.active()])

    def update(self) -> Floor:
        """Update the floor."""
        self.g[self.inactive()].clear()
        # Update black cells
        for black_cell in self.g[self.active()]:
            n = self.count_black_neighbours(black_cell)
            if n in (1, 2):
                self.g[self.inactive()].add(black_cell)
        # Find all white cells that neighbour a black cell
        white_cells: set[HexCoord] = set()
        for black_cell in self.g[self.active()]:
            neighbours = black_cell.adjoining() - self.g[self.active()]
            white_cells = white_cells | neighbours
        # Activate any eligible white cells
        for white_cell in white_cells:
            n = self.count_black_neighbours(white_cell)
            if n == 2:
                self.g[self.inactive()].add(white_cell)
        self.days += 1
        return self

    def run(self, n: int) -> Floor:
        """Update the floor for n days.

        >>> Floor(walk(test1)).run(20).count_black_cells()
        132
        """
        for _ in range(n):
            self.update()
        return self


if __name__ == "__main__":
    testmod()
    paths = [split_moves(line) for line in stdin]
    starting_set = walk(paths)
    print(len(starting_set))
    print(Floor(starting_set).run(100).count_black_cells())
