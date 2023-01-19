"""Advent of Code 2022 - Day 12."""

from doctest import testmod
from sys import stdin
from typing import Optional

from coord import Coord, Extent
from search import bfs


class HeightMap:
    """Main class for day 12."""

    def __init__(self, s: str) -> None:
        s_lines = s.split("\n")

        self.extent = Extent(len(s_lines), len(s_lines[0]))
        self.start = Coord(-1, -1)
        self.goal = Coord(-1, -1)
        self.h: dict[Coord, int] = {}

        for c in self.extent.upto():
            x = s_lines[c.x][c.y]
            if x == "S":
                self.start = c
                x = "a"
            elif x == "E":
                self.goal = c
                x = "z"
            self.h[c] = ord(x) - ord("a") + 1

    def shortest_path_length(self) -> Optional[int]:
        """Return the length of the shortest path from start to goal.

        >>> HeightMap(TEST_DATA).shortest_path_length()
        31
        """

        def at_goal(c: Coord) -> bool:
            return c == self.goal

        def available(c: Coord) -> list[Coord]:
            h_max = self.h[c] + 1
            return [a for a in c.adjacents(self.extent) if self.h[a] <= h_max]

        return bfs(self.start, at_goal, available)

    def shortest_path_from_any_length(self) -> Optional[int]:
        """Return the length of shortest path from any a-height start to goal.

        Computed as path from goal to any a-height point.

        >>> HeightMap(TEST_DATA).shortest_path_from_any_length()
        29
        """

        def at_goal(c: Coord) -> bool:
            return self.h[c] == 1

        def available(c: Coord) -> list[Coord]:
            h_min = self.h[c] - 1
            return [a for a in c.adjacents(self.extent) if self.h[a] >= h_min]

        return bfs(self.goal, at_goal, available)


TEST_DATA = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""

if __name__ == "__main__":
    testmod()
    height_map = HeightMap(stdin.read())
    print(height_map.shortest_path_length())
    print(height_map.shortest_path_from_any_length())
