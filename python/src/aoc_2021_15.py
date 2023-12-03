"""Advent of Code 2021 - Day 15."""

# This would better done using Dijkstra's algorithm or A*

from __future__ import annotations

from doctest import testmod
from sys import stdin

from coord import Coord, Extent

test_data: list[str] = [
    "1163751742",
    "1381373672",
    "2136511328",
    "3694931569",
    "7463417111",
    "1319128137",
    "1359912421",
    "3125421639",
    "1293138521",
    "2311944581",
]


class Grid:
    """Main class for day 15."""

    def __init__(self, rows: list[str]) -> None:
        self._extent = Extent(len(rows), len(rows[0]))
        self._risk: list[list[int]] = [[int(d) for d in row] for row in rows]
        self._total_risk: list[list[int]] = [
            [0] * self._extent.x for _ in range(self._extent.y)
        ]
        self._expanded: bool = False

    def expand(self) -> Grid:
        """Expand the grid."""
        self._expanded = True
        self._total_risk = [[0] * self._extent.x * 5 for _ in range(self._extent.y * 5)]
        return self

    @property
    def extent(self) -> Extent:
        """Provide extent of the grid."""
        if self._expanded:
            return Extent(self._extent.x * 5, self._extent.y * 5)
        return self._extent

    def risk(self, c: Coord) -> int:
        """Return risk of the grid."""
        if self._expanded:
            offset = c.x // self._extent.x + c.y // self._extent.y
            return (
                (self._risk[c.x % self._extent.x][c.y % self._extent.y] + offset - 1)
                % 9
            ) + 1
        return self._risk[c.x][c.y]

    def walk(self) -> int:
        """Return risk of lowest risk path to destination.

        >>> Grid(test_data).walk()
        40
        >>> Grid(test_data).expand().walk()
        315
        """
        risk: int = 0
        frontier: dict[Coord, int] = {Coord(0, 0): 0}
        visited: set[Coord] = set()
        extent: Extent = self.extent
        destination: Coord = extent + Coord(-1, -1)
        while destination not in frontier:
            risk += 1
            new_frontier: dict[Coord, int] = {}
            not_needed: set[Coord] = set()
            for c, c_risk in frontier.items():
                if c_risk + 9 < risk:
                    not_needed.add(c)
                else:
                    for n in c.adjacents(extent):
                        if (
                            n not in frontier
                            and n not in visited
                            and c_risk + self.risk(n) == risk
                        ):
                            new_frontier[n] = risk
            frontier = frontier | new_frontier
            for c in not_needed:
                del frontier[c]
                visited.add(c)
        return risk


if __name__ == "__main__":
    testmod()
    input_rows = stdin.read().splitlines()
    g = Grid(input_rows)
    print(g.walk())
    print(g.expand().walk())
