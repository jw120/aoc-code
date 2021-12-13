"""Advent of Code - 2d coordinate class."""

from __future__ import annotations

from dataclasses import dataclass
from doctest import testmod
from typing import Iterable, Iterator, Optional


@dataclass(eq=True, frozen=True)
class Coord:
    """Coordinates of a position in a 2-d grid."""

    x: int
    y: int

    def in_bounds(self, extent: Extent) -> bool:
        """Test if this position is within the given bounds.

        Extent is a coordinate that is just outside the grid's bounds

        >>> [Coord(x, y).in_bounds(Coord(5, 5)) for x, y in [(0, 0), (-1, 0), (5, 0), (0, 6), (2, 3), (4, 4)]]
        [True, False, False, False, True, True]

        """
        return self.x >= 0 and self.x < extent.x and self.y >= 0 and self.y < extent.y

    def adjacents(self, extent: Optional[Extent] = None) -> Iterable[Coord]:
        """Return all the cells adjacent to this one given one (excluding diagonals), optionally checking bounds."""
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

    def adjacents_with_diagonals(
        self, extent: Optional[Extent] = None
    ) -> Iterable[Coord]:
        """Return all the cells adjacent to this one given one (including diagonals), optionally checking bounds."""
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


class Extent(Coord):
    @property
    def size(self) -> int:
        return self.x * self.y

    def upto(self) -> Iterator[Coord]:
        """Return an iterator over all."""
        return (Coord(x, y) for x in range(self.x) for y in range(self.y))


if __name__ == "__main__":
    testmod()
