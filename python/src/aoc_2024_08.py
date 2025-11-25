"""Advent of Code 2024 - Day 8."""

from itertools import combinations
from math import gcd
from sys import stdin

from coord import Coord, Extent


class Map:
    """Puzzle map."""

    def __init__(self, lines: list[str]) -> None:
        height = 0
        width = 0
        self.antenna: dict[str, list[Coord]] = {}
        for y, line in enumerate(lines):
            for x, ch in enumerate(line.strip()):
                height = max(height, y + 1)
                width = max(width, x + 1)
                if ch != ".":
                    if ch in self.antenna:
                        self.antenna[ch].append(Coord(x, y))
                    else:
                        self.antenna[ch] = [Coord(x, y)]
        self.extent = Extent(width, height)

    def print(self, antinodes: set[Coord]) -> None:
        """Print map for debugging."""
        for crd in self.extent.upto_by_y():
            for frequency, locations in self.antenna.items():
                if crd in locations:
                    print(frequency, end="")
                    break
            else:
                if crd in antinodes:
                    print("#", end="")
                else:
                    print(".", end="")
                if crd.x == self.extent.x - 1:
                    print()

    def antinodes(self) -> set[Coord]:
        """Return antinode locations."""
        locations: set[Coord] = set()
        for nodes in self.antenna.values():
            for n1, n2 in combinations(nodes, 2):
                for a in (n1 - (n2 - n1), n2 + (n2 - n1)):
                    if a.in_bounds(self.extent):
                        locations.add(a)
        return locations

    def antinodes_b(self) -> set[Coord]:
        """Return antinode locations for part b."""
        locations: set[Coord] = set()
        for nodes in self.antenna.values():
            for n1, n2 in combinations(nodes, 2):
                d = n2 - n1
                d //= gcd(abs(d.x), abs(d.y))
                c = n1
                while c.in_bounds(self.extent):
                    locations.add(c)
                    c += d
                c = n1 - d
                while c.in_bounds(self.extent):
                    locations.add(c)
                    c -= d
        return locations


if __name__ == "__main__":
    m = Map(stdin.readlines())
    print(len(m.antinodes()))
    print(len(m.antinodes_b()))
