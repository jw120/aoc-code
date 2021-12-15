"""Advent of Code 2021 - Day 15."""

# This assumes we never need to go up or left

from doctest import testmod
from sys import stdin

from Coord import Coord, Extent

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
    def __init__(self, rows: list[str]) -> None:
        self._extent = Extent(len(rows), len(rows[0]))
        self._risk: list[list[int]] = [[int(d) for d in row] for row in rows]
        self._total_risk: list[list[int]] = [
            [0] * self._extent.x for _ in range(self._extent.y)
        ]

    def risk(self, c: Coord) -> int:
        return self._risk[c.x][c.y]

    def set_risk(self, c: Coord, val: int) -> None:
        self._risk[c.x][c.y] = val

    def total_risk(self, c: Coord) -> int:
        return self._total_risk[c.x][c.y]

    def set_total_risk(self, c: Coord, val: int) -> None:
        self._total_risk[c.x][c.y] = val

    def assign_total_risks(self) -> None:
        self.set_total_risk(Coord(0, 0), 0)
        for c in self._extent.upto():
            if c.x == 0 and c.y == 0:
                incoming: int = -self.risk(c)
            elif c.x == 0 and c.y > 0:
                incoming = self.total_risk(c + Coord(0, -1))
            elif c.y == 0 and c.x > 0:
                incoming = self.total_risk(c + Coord(-1, 0))
            else:
                incoming = min(
                    self.total_risk(c + Coord(0, -1)), self.total_risk(c + Coord(-1, 0))
                )
            self.set_total_risk(c, self.risk(c) + incoming)

    def exit_total_risk(self) -> int:
        """Total risk for the whole grid.

        >>> Grid(test_data).exit_total_risk()
        40
        """
        self.assign_total_risks()
        return self.total_risk(self._extent + Coord(-1, -1))


if __name__ == "__main__":
    testmod()
    rows = stdin.read().splitlines()
    g = Grid(rows)
    print(g.exit_total_risk())
