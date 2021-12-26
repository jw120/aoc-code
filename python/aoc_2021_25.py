"""Advent of Code 2021 - Day 25."""

from __future__ import annotations

from doctest import testmod
from enum import Enum, auto
from sys import stdin
from typing import Iterator

from Coord import Coord, Extent

import numpy as np


class Item(Enum):
    EMPTY = auto()
    EAST = auto()
    SOUTH = auto()

    def show(self) -> str:
        if self == Item.EAST:
            return ">"
        if self == Item.SOUTH:
            return "v"
        return "."

    @staticmethod
    def parse(s: str) -> Item:
        if s == ">":
            return Item.EAST
        if s == "v":
            return Item.SOUTH
        if s == ".":
            return Item.EMPTY
        raise ValueError("Cannot parse item '" + s + "'")


class SeaFloor:
    def __init__(self, row_data: list[str]) -> None:
        assert len(row_data) > 0, "No row data to make SeaFloor"
        self._extent = Extent(len(row_data[0]), len(row_data))
        assert all(
            len(row) == self._extent.x for row in row_data
        ), "Rows not equal length when making Seafloor"
        self._a = np.empty((self._extent.x, self._extent.y), dtype=np.int)
        self._moved: bool = False
        for y, row in enumerate(row_data):
            for x, ch in enumerate(row):
                self._a[x, y] = Item.parse(ch).value

    def show(self) -> None:
        """Print a visual representation of the sea floor for debugging."""
        for y in range(self._extent.y):
            for x in range(self._extent.x):
                print(Item(self._a[x, y]).show(), end="")
            print()

    def set_floor(self, c: Coord, val: Item) -> None:
        """Set the item on the floor at the given coordinates."""
        self._a[c.x, c.y] = val.value

    def get_floor(self, c: Coord) -> Item:
        """Get the item on the floor at the given coordinates."""
        # print("get", c.x, c.y, Item(self._a[c.x, c.y]).name)
        return Item(self._a[c.x, c.y])

    def over_floor(self) -> Iterator[Coord]:
        """Provide an iterator over all sea floor coordinates."""
        return self._extent.upto()

    def east(self, c: Coord) -> Coord:
        """Return the coordinate to the East of the given coordinate (wrapping if needed)."""
        return Coord((c.x + 1) % self._extent.x, c.y)

    def south(self, c: Coord) -> Coord:
        """Return the coordinate to the South of the given coordinate (wrapping if needed)."""
        return Coord(c.x, (c.y + 1) % self._extent.y)

    def can_east(self, c: Coord) -> bool:
        """Test if there is a slug at the coordinate able to move East."""
        return (
            self.get_floor(c) == Item.EAST
            and self.get_floor(self.east(c)) == Item.EMPTY
        )

    def can_south(self, c: Coord) -> bool:
        """Test if there is a slug at the coordinate able to move East."""
        return (
            self.get_floor(c) == Item.SOUTH
            and self.get_floor(self.south(c)) == Item.EMPTY
        )

    def step(self) -> bool:
        """Step the sea floor once, returning true if any slug moved."""
        moved = False
        can_move: list[Coord] = [c for c in self.over_floor() if self.can_east(c)]
        for m in can_move:
            # print(">", m)
            self.set_floor(m, Item.EMPTY)
            moved = True
        for m in can_move:
            self.set_floor(self.east(m), Item.EAST)
        can_move = [c for c in self.over_floor() if self.can_south(c)]
        for m in can_move:
            # print("v", m)
            self.set_floor(m, Item.EMPTY)
            moved = True
        for m in can_move:
            self.set_floor(self.south(m), Item.SOUTH)
            moved = True
        return moved

    def first_stationary(self) -> int:
        """Return the first step on which no slugs move.

        >>> SeaFloor(test2).first_stationary()
        58
        """
        steps: int = 0
        while True:
            moved: bool = self.step()
            steps += 1
            if not moved:
                return steps


test0 = ["...>>>>>..."]

test1 = ["..........", ".>v....v..", ".......>..", ".........."]

test2 = [
    "v...>>.vv>",
    ".vv>>.vv..",
    ">>.>v>...v",
    ">>v>>.>.v.",
    "v>v.vv.v..",
    ">.>>..v...",
    ".vv..>.>v.",
    "v.v..>>v.v",
    "....v..v.>",
]

if __name__ == "__main__":
    testmod()
    sea_floor = SeaFloor(stdin.read().splitlines())
    print(sea_floor.first_stationary())
