"""Advent of Code 2024 - Day 12."""

from itertools import combinations
from sys import stdin

from coord import Coord, Extent


class Garden:
    """Garden of plots."""

    def __init__(self, lines: list[str]) -> None:
        self.extent = Extent(len(lines[0].strip()), len(lines))
        self.grid: list[str] = lines

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


# Edge c is to left, below cell c. True if edge is horizontal
type Edge = tuple[Coord, bool]


def edges(c: Coord) -> list[Edge]:
    """Return edges around a coordinate."""
    return [(c, True), (c + Coord(0, 1), True), (c, False), (c + Coord(1, 0), False)]


def connected(edge: Edge) -> set[Edge]:
    """Return all the edges connected to the given edge."""
    c, is_horizontal = edge
    if is_horizontal:
        return {
            (c + Coord(1, 0), True),
            (c - Coord(1, 0), True),
            (c + Coord(1, 0), False),
            (c + Coord(1, -1), False),
            (c, False),
            (c + Coord(0, -1), False),
        }
    return {
        (c + Coord(0, 1), False),
        (c + Coord(0, -1), False),
        (c, True),
        (c + Coord(-1, 0), True),
        (c + Coord(0, 1), True),
        (c + Coord(-1, 1), True),
    }


def number_of_sides(s: set[Coord]) -> int:
    """Calculate the number of sides of a region."""
    # Find all exterior edges (all non-duplicated cell edges)
    exterior_edges: set[Edge] = set()
    for cell in s:
        for edge in edges(cell):
            if edge in exterior_edges:
                exterior_edges.remove(edge)
            else:
                exterior_edges.add(edge)
    assert len(exterior_edges) == perimeter(s)

    # Walk around edges, counting turning points
    first_edge: Edge = exterior_edges.pop()
    count: int = 0
    current = first_edge
    is_first_edge: bool = True
    while exterior_edges:
        #     print("current", current)
        #     print("exterior edges", exterior_edges)
        #     print("connected", connected(current))
        next_edges = connected(current) & exterior_edges
        # print("next_edges", next_edges)
        assert len(next_edges) == 2 if is_first_edge else 1
        is_first_edge = False
        next_edge = next_edges.pop()
        count += next_edge[1] != current[1]
        current = next_edge
        exterior_edges.remove(current)
    count += current[1] != first_edge[1]
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
    for type_, cells in regions:
        print(type_, ": ", end="")
        print(len(cells), perimeter(cells), number_of_sides(cells))
    print(sum(price1(cells) for _type, cells in regions))
    print(sum(price2(cells) for _type, cells in regions))
