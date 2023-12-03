"""Advent of Code - 2d coordinate class."""

from __future__ import annotations

from collections.abc import Iterable, Iterator
from dataclasses import dataclass
from doctest import testmod
from typing import Any


@dataclass(eq=True, frozen=True)
class Coord:
    """Coordinates of a position in a 2-d grid."""

    x: int
    y: int

    def in_bounds(self, extent: Extent) -> bool:
        """Test if this position is within the given bounds.

        Extent is a coordinate that is just outside the grid's bounds

        >>> test_coords = [(0, 0), (-1, 0), (5, 0), (0, 6), (2, 3), (4, 4)]
        >>> [Coord(x, y).in_bounds(Coord(5, 5)) for x, y in test_coords]
        [True, False, False, False, True, True]

        """
        return self.x >= 0 and self.x < extent.x and self.y >= 0 and self.y < extent.y

    def adjacents(self, extent: Extent | None = None) -> Iterable[Coord]:
        """Return all the cells adjacent to this one given one (excluding diagonals).

        Optionally checks bounds."""
        x, y = self.x, self.y
        possible_cells = (
            Coord(p, q)
            for p, q in [
                (x - 1, y),
                (x, y - 1),
                (x, y + 1),
                (x + 1, y),
            ]
        )
        if extent is None:
            return possible_cells
        return (c for c in possible_cells if c.in_bounds(extent))

    def adjacents_with_diagonals(self, extent: Extent | None = None) -> Iterable[Coord]:
        """Return all the cells adjacent to this one given one (including diagonals).

        Optionally checks bounds."""
        x, y = self.x, self.y
        possible_cells = (
            Coord(p, q)
            for p, q in [
                (x - 1, y - 1),
                (x - 1, y),
                (x - 1, y + 1),
                (x, y - 1),
                (x, y + 1),
                (x + 1, y - 1),
                (x + 1, y),
                (x + 1, y + 1),
            ]
        )
        if extent is None:
            return possible_cells
        return (c for c in possible_cells if c.in_bounds(extent))

    def mag2(self) -> int:
        """Return magnitude-squared of the coordinate."""
        return self.x * self.x + self.y * self.y

    def dist(self, other: Coord) -> int:
        """Manhattan distance to another point."""
        return abs(self.x - other.x) + abs(self.y - other.y)

    @staticmethod
    def origin() -> Coord:
        """Return the (0, 0) coordinate."""
        return Coord(0, 0)

    def __add__(self, other: Any) -> Coord:
        if isinstance(other, Coord):
            return Coord(self.x + other.x, self.y + other.y)
        raise TypeError

    def __sub__(self, other: Any) -> Coord:
        if isinstance(other, Coord):
            return Coord(self.x - other.x, self.y - other.y)
        raise TypeError

    def __floordiv__(self, other: Any) -> Coord:
        if isinstance(other, int):
            return Coord(self.x // other, self.y // other)
        raise TypeError


def manhattan(c1: Coord, c2: Coord) -> int:
    """Return Manhattan distance between two coordinates.

    >>> manhattan(Coord(1, 2), Coord(3, 0))
    4
    """
    return abs(c1.x - c2.x) + abs(c1.y - c2.y)


class Extent(Coord):
    """Class to track a range of coordinates from origin to give point.

    Given point is not included."""

    @property
    def size(self) -> int:
        """Return the number of coordinates covered by the extent."""
        return self.x * self.y

    def upto(self) -> Iterator[Coord]:
        """Return an iterator over all coordinates in bound.

        Order is (x=0, y=0), (x=0, y=1),...
        """
        return (Coord(x, y) for x in range(self.x) for y in range(self.y))

    def upto_by_y(self) -> Iterator[Coord]:
        """Return an iterator over all coordinates in bound.

        Order is (x=0, y=0), (x=1, y=0),...
        """
        return (Coord(x, y) for y in range(self.y) for x in range(self.x))


@dataclass(eq=True, frozen=True)
class Coord3:
    """Coordinates of a position in a 3-d grid."""

    x: int
    y: int
    z: int

    def manhattan(self, other: Coord3) -> int:
        """Return Manhattan distance to the other coordinate."""
        return abs(self.x - other.x) + abs(self.y - other.y) + abs(self.z - other.z)

    def as_tuple(self) -> tuple[int, int, int]:
        """Return a tuple-version of the coordinate."""
        return (self.x, self.y, self.z)

    def adjacents(self) -> Iterable[Coord3]:
        """Return all the cells adjacent to this one (excluding diagonals)."""
        x, y, z = self.x, self.y, self.z
        return (
            Coord3(p, q, r)
            for p, q, r in [
                (x - 1, y, z),
                (x, y - 1, z),
                (x, y + 1, z),
                (x + 1, y, z),
                (x, y, z + 1),
                (x, y, z - 1),
            ]
        )

    def __add__(self, other: Any) -> Coord3:
        if isinstance(other, Coord3):
            return Coord3(self.x + other.x, self.y + other.y, self.z + other.z)
        raise TypeError

    def __sub__(self, other: Any) -> Coord3:
        if isinstance(other, Coord3):
            return Coord3(self.x - other.x, self.y - other.y, self.z - other.z)
        raise TypeError

    def __neg__(self) -> Coord3:
        return Coord3(-self.x, -self.y, -self.z)


if __name__ == "__main__":
    testmod()
