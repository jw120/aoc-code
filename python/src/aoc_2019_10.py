"""Advent of Code 2019 - Day 10."""

from __future__ import annotations

from dataclasses import dataclass
from doctest import testmod
from math import atan2, gcd, pi
from sys import stdin
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from collections.abc import Iterable


@dataclass(frozen=True)
class Vec:
    """Two-d vector class."""

    x: int
    y: int

    def __add__(self, other: Vec) -> Vec:
        return Vec(self.x + other.x, self.y + other.y)

    def __sub__(self, other: Vec) -> Vec:
        return Vec(self.x - other.x, self.y - other.y)

    def primitive(self) -> Vec:
        """Return the shortest vector in the given vector's direction.

        >>> Vec(6, 4).primitive()
        Vec(x=3, y=2)
        >>> Vec(3, 0).primitive()
        Vec(x=1, y=0)
        """
        if self.x == 0 and self.y == 0:
            raise RuntimeError("Zero vector in primitive")
        if self.x == 0:
            return Vec(0, 1 if self.y > 0 else -1)
        if self.y == 0:
            return Vec(1 if self.x > 0 else -1, 0)
        return Vec(self.x // gcd(self.x, self.y), self.y // gcd(self.x, self.y))


class Grid:
    """Main class for day 10."""

    def __init__(self, s: str) -> None:
        self._m: list[list[bool]] = [[s == "#" for s in row.strip()] for row in s.split()]
        self.x_num: int = len(self._m[0])
        self.y_num: int = len(self._m)

    def all_positions(self) -> Iterable[Vec]:
        """Provide iteration over all positions, row-wise."""
        for y in range(self.y_num):
            for x in range(self.x_num):
                yield Vec(x, y)

    def m(self, v: Vec) -> bool:
        """Test if given location contains an asteroid."""
        return self._m[v.y][v.x]

    def visible(self, ast: Vec, source: Vec) -> bool:
        """Test if an asteroid can be seen from the source."""
        if not self.m(ast):
            return False
        diff = ast - source
        if diff == Vec(0, 0):
            return True
        step = diff.primitive()
        v = source + step
        while v != ast:
            if self.m(v):
                return False
            v += step
        return True

    def count_visible(self, source: Vec) -> int:
        """Return number of other asteroids visible from (p, q)."""
        count = 0
        for v in self.all_positions():
            count += self.visible(v, source)
        return count - self.m(source)

    def best(self) -> tuple[Vec, int]:
        """Find the asteroid from which the most others are visible.

        >>> test_one.best()
        (Vec(x=3, y=4), 8)
        >>> test_two.best()
        (Vec(x=5, y=8), 33)
        >>> test_three.best()
        (Vec(x=1, y=2), 35)
        >>> test_four.best()
        (Vec(x=6, y=3), 41)
        >>> test_five.best()
        (Vec(x=11, y=13), 210)
        """
        best_count = -1
        best_v = Vec(0, 0)
        for v in self.all_positions():
            if self.m(v) and self.count_visible(v) > best_count:
                best_count = self.count_visible(v)
                best_v = v
        return (best_v, best_count)

    def laser(self, laser: Vec) -> Iterable[Vec]:
        """Fire lasers from asteroid at (lx, ly)."""

        def sort_by_mag(vs: list[Vec]) -> list[Vec]:
            return sorted(vs, key=lambda v: abs(v.x) + abs(v.y))

        def comparison_angle(x: tuple[Vec, list[Vec]]) -> float:
            base_angle = atan2(x[0].x, -x[0].y)
            if base_angle < 0:
                return base_angle + 2 * pi
            return base_angle

        # Group all the asteroids by their primitive vector from the laser
        ast_groups: dict[Vec, list[Vec]] = {}
        for v in self.all_positions():
            if self.m(v) and (v != laser):
                delta = v - laser
                prim_delta = delta.primitive()
                ast_groups.setdefault(prim_delta, []).append(delta)

        # Convert to a list sorted by the angle, with elements as sorted lists
        ast_list: list[tuple[Vec, list[Vec]]] = [
            (k, sort_by_mag(v)) for (k, v) in ast_groups.items()
        ]
        ast_list.sort(key=comparison_angle)

        # Spin the laser
        count = 0
        while ast_list:
            for _, asts in ast_list:
                count += 1
                yield laser + asts[0]
                asts.pop(0)
            # Prune empty entries
            ast_list = [(v, asts) for (v, asts) in ast_list if asts]


def part_two(grid: Grid) -> int:
    """Solve part two.

    >>> part_two(test_five)
    802
    """
    (laser_position, _) = grid.best()
    target = list(grid.laser(laser_position))[199]
    return target.x * 100 + target.y


test_one: Grid = Grid(
    """.#..#
    .....
    #####
    ....#
    ...##
    """
)

test_two: Grid = Grid(
    """......#.#.
    #..#.#....
    ..#######.
    .#.#.###..
    .#..#.....
    ..#....#.#
    #..#....#.
    .##.#..###
    ##...#..#.
    .#....####
    """
)

test_three: Grid = Grid(
    """
    #.#...#.#.
    .###....#.
    .#....#...
    ##.#.#.#.#
    ....#.#.#.
    .##..###.#
    ..#...##..
    ..##....##
    ......#...
    .####.###.
    """
)

test_four: Grid = Grid(
    """
    .#..#..###
    ####.###.#
    ....###.#.
    ..###.##.#
    ##.##.#.#.
    ....###..#
    ..#.#..#.#
    #..#.#.###
    .##...##.#
    .....#.#..
    """
)

test_five: Grid = Grid(
    """.#..##.###...#######
    ##.############..##.
    .#.######.########.#
    .###.#######.####.#.
    #####.##.#.##.###.##
    ..#####..#.#########
    ####################
    #.####....###.#.#.##
    ##.#################
    #####.##.###..####..
    ..######..##.#######
    ####.##.####...##..#
    .#####..#.######.###
    ##...#.##########...
    #.##########.#######
    .####.#.###.###.#.##
    ....##.##.###..#####
    .#.#.###########.###
    #.#.#.#####.####.###
    ###.##.####.##.#..##
    """
)

test_six: Grid = Grid(
    """.#....#####...#..
    ##...##.#####..##
    ##...#...#.#####.
    ..#.....X...###..
    ..#.#.....#....##
    """
)

if __name__ == "__main__":
    testmod()
    input_grid: Grid = Grid(stdin.read())
    print(input_grid.best()[1])
    print(part_two(input_grid))
