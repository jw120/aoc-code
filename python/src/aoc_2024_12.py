"""Advent of Code 2024 - Day 12."""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum, auto
from itertools import combinations
from sys import stdin

from coord import Coord, Extent


class Garden:
    """Garden of plots."""

    def __init__(self, lines: list[str]) -> None:
        self.extent = Extent(len(lines[0].strip()), len(lines))
        self.grid: list[str] = list(reversed(lines))  # So y-axis is up

    def g(self, crd: Coord) -> str:
        """Get plant type at given coordinate."""
        return self.grid[crd.y][crd.x]

    def region(self, crd: Coord) -> set[Coord]:
        """Return coordinate and all adjacent coordinates with same type."""
        crd_type: str = self.g(crd)
        outside: set[Coord] = set()  # Known outside region
        inside: set[Coord] = set()  # Known inside region
        frontier: set[Coord] = {crd}  # Unknown
        while frontier:
            c = frontier.pop()
            if c in outside or c in inside:
                continue
            if self.g(c) == crd_type:
                inside.add(c)
                for adj in c.adjacents(self.extent):
                    if adj not in outside and adj not in inside:
                        frontier.add(adj)
            else:
                outside.add(c)
        return inside

    def assign_regions(self) -> list[tuple[str, set[Coord]]]:
        """Find all the regions of adjacent plants of the same type."""
        regions: list[tuple[str, set[Coord]]] = []
        assigned: list[list[bool]] = [
            [False for _ in range(self.extent.x)] for _ in range(self.extent.y)
        ]
        for crd in self.extent.upto():
            if not assigned[crd.y][crd.x]:
                crd_region = self.region(crd)
                for c in crd_region:
                    assigned[c.y][c.x] = True
                regions.append((self.g(crd), crd_region))
        assert sum(len(cells) for _type, cells in regions) == self.extent.x * self.extent.y
        return regions


def perimeter(s: set[Coord]) -> int:
    """Calculate the perimeter of a region."""
    interior_boundaries = sum((c1.dist(c2) == 1) for (c1, c2) in combinations(s, 2))
    return 4 * len(s) - 2 * interior_boundaries


class Dir(Enum):
    """Label for an edge of a cell."""

    Top = auto()
    Right = auto()
    Bottom = auto()
    Left = auto()

    def letter(self) -> str:
        """Return single-letter version for printing."""
        match self:
            case Dir.Top:
                return "T"
            case Dir.Right:
                return "R"
            case Dir.Bottom:
                return "B"
            case Dir.Left:
                return "L"


@dataclass(frozen=True)
class Edge:
    """Edge is attached to each cell."""

    cell: Coord
    label: Dir

    def opposite(self) -> Edge:
        """Return the parallel edge of the adjacent cell."""
        match self.label:
            case Dir.Top:
                return Edge(self.cell + Coord(0, 1), Dir.Bottom)
            case Dir.Right:
                return Edge(self.cell + Coord(1, 0), Dir.Left)
            case Dir.Bottom:
                return Edge(self.cell + Coord(0, -1), Dir.Top)
            case Dir.Left:
                return Edge(self.cell + Coord(-1, 0), Dir.Right)

    def __str__(self) -> str:
        """Return string form for debugging."""
        return f"{self.label.letter()}({self.cell.x},{self.cell.y})"


def next_exterior_edge(edge: Edge, cells: set[Coord]) -> Edge:
    """Return next exterior edge if moving clockwise."""
    match edge.label:
        case Dir.Top:
            if edge.cell + Coord(1, 0) in cells:
                if edge.cell + Coord(1, 1) in cells:
                    return Edge(edge.cell + Coord(1, 1), Dir.Left)
                return Edge(edge.cell + Coord(1, 0), Dir.Top)
            return Edge(edge.cell, Dir.Right)
        case Dir.Right:
            if edge.cell + Coord(0, -1) in cells:
                if edge.cell + Coord(1, -1) in cells:
                    return Edge(edge.cell + Coord(1, -1), Dir.Top)
                return Edge(edge.cell + Coord(0, -1), Dir.Right)
            return Edge(edge.cell, Dir.Bottom)
        case Dir.Bottom:
            if edge.cell + Coord(-1, 0) in cells:
                if edge.cell + Coord(-1, -1) in cells:
                    return Edge(edge.cell + Coord(-1, -1), Dir.Right)
                return Edge(edge.cell + Coord(-1, 0), Dir.Bottom)
            return Edge(edge.cell, Dir.Left)
        case Dir.Left:
            if edge.cell + Coord(0, 1) in cells:
                if edge.cell + Coord(-1, 1) in cells:
                    return Edge(edge.cell + Coord(-1, 1), Dir.Bottom)
                return Edge(edge.cell + Coord(0, 1), Dir.Left)
            return Edge(edge.cell, Dir.Top)


def number_of_sides(s: set[Coord]) -> int:
    """Calculate the number of sides of a region."""
    # Find all exterior edges (all non-duplicated cell edges)
    exterior_edges: set[Edge] = set()
    for cell in s:
        for direction in Dir:
            edge = Edge(cell, direction)
            opposite = edge.opposite()
            if opposite in exterior_edges:
                exterior_edges.remove(opposite)
            else:
                exterior_edges.add(edge)
    assert len(exterior_edges) == perimeter(s)

    # Walk around edges, counting turning points
    count: int = 0
    while exterior_edges:
        first_edge: Edge = exterior_edges.pop()
        current = first_edge
        while True:
            next_edge = next_exterior_edge(current, s)
            if next_edge.label != current.label:
                count += 1
            if next_edge == first_edge:
                break
            current = next_edge
            exterior_edges.remove(current)
    return count


def price1(s: set[Coord]) -> int:
    """Calculate first version of the price of a region."""
    area = len(s)
    return area * perimeter(s)


def price2(s: set[Coord]) -> int:
    """Calculate second version of the price of a region."""
    area = len(s)
    return area * number_of_sides(s)


if __name__ == "__main__":
    garden = Garden(stdin.readlines())
    regions = garden.assign_regions()
    print(sum(price1(cells) for _type, cells in regions))
    print(sum(price2(cells) for _type, cells in regions))
