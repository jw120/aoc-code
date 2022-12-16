"""Advent of Code 2022 - Day 14."""

from __future__ import annotations

from doctest import testmod
from re import match
from sys import stdin
from typing import Iterable

from Coord import Coord


def read_coord(s: str) -> Coord:
    """Read a coordinate from a common-separated string.

    >>> read_coord("2,34")
    Coord(x=2, y=34)
    """
    parts = s.split(",")
    assert len(parts) == 2, f"Bad split {s} {parts}"
    x_str, y_str = parts
    return Coord(int(x_str), int(y_str))


def line_coords(c1: Coord, c2: Coord) -> list[Coord]:
    """Return all the points between the two coordinates.

    Both end-points are included and the two points must be horizontally of vertically aligned.

    >>> line_coords(Coord(2, 2), Coord(4, 2))
    [Coord(x=2, y=2), Coord(x=3, y=2), Coord(x=4, y=2)]
    >>> line_coords(Coord(0, 2), Coord(0, 1))
    [Coord(x=0, y=2), Coord(x=0, y=1)]
    """
    if c1.x == c2.x and c2.y > c1.y:
        return [Coord(c1.x, y) for y in range(c1.y, c2.y + 1)]
    if c1.x == c2.x and c2.y < c1.y:
        return [Coord(c1.x, y) for y in range(c1.y, c2.y - 1, -1)]
    if c1.y == c2.y and c2.x > c1.x:
        return [Coord(x, c1.y) for x in range(c1.x, c2.x + 1)]
    if c1.y == c2.y and c2.x < c1.x:
        return [Coord(x, c1.y) for x in range(c1.x, c2.x - 1, -1)]
    raise ValueError(f"Not a line {c1} {c2}")


class Waterfall:
    def __init__(self, paths: Iterable[str]):
        """Initialize the waterfall from a list of rock paths."""
        self.rock: set[Coord] = set()
        self.sand: set[Coord] = set()
        self.overflowing = False
        self.start = Coord(500, 0)
        self.bottom_left: Coord = self.start
        self.top_right: Coord = self.start
        for path in paths:
            print(path)
            points: list[Coord] = [read_coord(s) for s in path.split(" -> ")]
            assert points, f"No points found: '{path}'"
            cursor: Coord = points[0]
            for next_point in points[1:]:
                for c in line_coords(cursor, next_point):
                    self.rock.add(c)
                    self.bottom_left = Coord(
                        min(self.bottom_left.x, c.x), max(self.bottom_left.y, c.y)
                    )
                    self.top_right = Coord(
                        max(self.top_right.x, c.x), min(self.top_right.y, c.y)
                    )
                cursor = next_point

    @property
    def sand_amount(self) -> int:
        """The amount of sand in the waterfall."""
        return len(self.sand)

    def show(self) -> None:
        """Print a image of the waterfall."""
        for y in range(self.top_right.y, self.bottom_left.y + 1):
            for x in range(self.bottom_left.x, self.top_right.x + 1):
                if Coord(x, y) == self.start:
                    c = "+"
                elif Coord(x, y) in self.rock:
                    c = "#"
                elif Coord(x, y) in self.sand:
                    c = "o"
                else:
                    c = "."
                print(c, end="")
            print()

    def empty(self, c: Coord) -> bool:
        """Is the coordinate empty?"""
        return c not in self.rock and c not in self.sand

    def add(self) -> None:
        """Add one unit of sand."""
        sand_coord = self.start
        while True:
            if sand_coord.y >= self.bottom_left.y:
                self.overflowing = True
                return
            trial_coord = sand_coord + Coord(0, 1)
            if self.empty(trial_coord):
                sand_coord = trial_coord
                continue
            trial_coord = sand_coord + Coord(-1, 1)
            if self.empty(trial_coord):
                sand_coord = trial_coord
                continue
            trial_coord = sand_coord + Coord(1, 1)
            if self.empty(trial_coord):
                sand_coord = trial_coord
                continue
            self.sand.add(sand_coord)
            return

    def add_until_overflowing(self) -> Waterfall:
        """Add sand until overflowing.

        >>> Waterfall(test_data).add_until_overflowing().sand_amount
        24
        """
        while not self.overflowing:
            self.add()
        return self


test_data = ["498,4 -> 498,6 -> 496,6", "503,4 -> 502,4 -> 502,9 -> 494,9"]

if __name__ == "__main__":
    #    testmod()
    w = Waterfall(stdin.readlines()).add_until_overflowing()
    w.show()
    print(w.sand_amount)
