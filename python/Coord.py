"""Advent of Code - 2d coordinate class."""

from __future__ import annotations

from dataclasses import dataclass
from doctest import testmod
from typing import Iterable, Iterator, Optional


@dataclass(eq=True, frozen=True)
class Coord:
    """Coordinates of a position in a 2-d grid."""

    row: int
    col: int

    def in_bounds(self, extent: Extent) -> bool:
        """Test if this position is within the given bounds.

        Extent is a coordinate that is just outside the grid's bounds

        >>> [Coord(x, y).in_bounds(Coord(5, 5)) for x, y in [(0, 0), (-1, 0), (5, 0), (0, 6), (2, 3), (4, 4)]]
        [True, False, False, False, True, True]

        """
        return (
            self.row >= 0
            and self.row < extent.row
            and self.col >= 0
            and self.col < extent.col
        )

    def adjacents(self, extent: Optional[Extent] = None) -> Iterable[Coord]:
        """Return all the cells adjacent to this one given one (excluding diagonals), optionally checking bounds."""
        r, c = self.row, self.col
        possible_cells = (
            Coord(x, y)
            for x, y in [
                (r - 1, c),
                (r, c - 1),
                (r, c + 1),
                (r + 1, c),
            ]
        )
        if extent is None:
            return possible_cells
        return (c for c in possible_cells if c.in_bounds(extent))

    def adjacents_with_diagonals(
        self, extent: Optional[Extent] = None
    ) -> Iterable[Coord]:
        """Return all the cells adjacent to this one given one (including diagonals), optionally checking bounds."""
        r, c = self.row, self.col
        possible_cells = (
            Coord(x, y)
            for x, y in [
                (r - 1, c - 1),
                (r - 1, c),
                (r - 1, c + 1),
                (r, c - 1),
                (r, c + 1),
                (r + 1, c - 1),
                (r + 1, c),
                (r + 1, c + 1),
            ]
        )
        if extent is None:
            return possible_cells
        return (c for c in possible_cells if c.in_bounds(extent))


class Extent(Coord):
    @property
    def size(self) -> int:
        return self.row * self.col

    def upto(self) -> Iterator[Coord]:
        """Return an iterator over all."""
        return (Coord(r, c) for r in range(self.row) for c in range(self.col))


if __name__ == "__main__":
    testmod()
