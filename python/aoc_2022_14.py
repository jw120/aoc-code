"""Advent of Code 2022 - Day 14."""

from __future__ import annotations

from doctest import testmod
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
    """Base object for day 14."""

    def __init__(self, paths: Iterable[str]):
        """Initialize the waterfall from a list of rock paths."""
        self.rock: set[Coord] = set()
        self.sand: set[Coord] = set()
        self.finished = False
        self.floor = False
        self.start = Coord(500, 0)
        self.bottom_left: Coord = self.start
        self.top_right: Coord = self.start
        for path in paths:
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

    def reset(self) -> None:
        """Reset waterfall configuration for second run."""
        self.sand = set()
        self.finished = False

    @property
    def sand_amount(self) -> int:
        """The amount of sand in the waterfall."""
        return len(self.sand)

    def show(self) -> None:
        """Print a image of the waterfall."""
        if self.floor:
            x_min = min(c.x for c in self.sand) - 2
            x_max = max(c.x for c in self.sand) + 2
            y_min = self.start.y
            y_max = self.bottom_left.y + 2
        else:
            x_min = self.bottom_left.x
            x_max = self.top_right.x
            y_min = self.start.y
            y_max = self.bottom_left.y
        for y in range(y_min, y_max + 1):
            for x in range(x_min, x_max + 1):
                if Coord(x, y) in self.sand:
                    c = "o"
                elif Coord(x, y) == self.start:
                    c = "+"
                elif Coord(x, y) in self.rock or y == self.bottom_left.y + 2:
                    c = "#"
                else:
                    c = "."
                print(c, end="")
            print()

    def empty(self, c: Coord) -> bool:
        """Is the coordinate empty?"""
        if c in self.rock or c in self.sand:
            return False
        if self.floor and c.y == self.bottom_left.y + 2:
            return False
        return True

    def add(self) -> None:
        """Add one unit of sand."""
        sand_coord = self.start
        while True:
            if not self.floor:
                if sand_coord.y >= self.bottom_left.y:
                    self.finished = True
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
            if self.floor:
                if sand_coord == self.start:
                    self.finished = True
                    return
            return

    def add_until_overflowing(self) -> Waterfall:
        """Add sand until overflowing.

        >>> Waterfall(TEST_DATA).add_until_overflowing().sand_amount
        24
        >>> w = Waterfall(TEST_DATA)
        >>> w.floor = True
        >>> print(w.add_until_overflowing().sand_amount)
        93
        """
        while not self.finished:
            self.add()
        return self


TEST_DATA = ["498,4 -> 498,6 -> 496,6", "503,4 -> 502,4 -> 502,9 -> 494,9"]

if __name__ == "__main__":
    testmod()
    w = Waterfall(stdin.readlines())
    print(w.add_until_overflowing().sand_amount)
    w.reset()
    w.floor = True
    print(w.add_until_overflowing().sand_amount)
