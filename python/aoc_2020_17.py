"""Advent of Code 2020 - Day 17."""

from sys import stdin
from typing import Iterable, List, Set, Tuple

Coord = Tuple[int, int, int]


def min_coord(p: Coord, q: Coord) -> Coord:
    return (min(p[0], q[0]), min(p[1], q[1]), min(p[2], q[2]))


def max_coord(p: Coord, q: Coord) -> Coord:
    return (max(p[0], q[0]), max(p[1], q[1]), max(p[2], q[2]))


class Grid3d:
    """Provide a 3d Grid of cubes.

    Holds two copies of the grid data so one can be updated while
    the other is being read.
    """

    def __init__(self, lines: Iterable[str]) -> None:
        self.time: int = -1
        self.g: List[Set[Coord]] = [set(), set()]
        for y, line in enumerate(lines):
            for x, ch in enumerate(line):
                if ch == "#":
                    self.add_cube((x, y, 0))
        self.time = 0

    def active(self) -> int:
        return self.time % 2

    def inactive(self) -> int:
        return (self.time + 1) % 2

    def add_cube(self, crd: Coord) -> None:
        """Add a cube to the inactive grid."""
        self.g[self.inactive()].add(crd)

    def has_cube(self, crd: Coord) -> bool:
        """Is there a cube at given point in the active grid."""
        return crd in self.g[self.active()]

    @staticmethod
    def neighbouring_locations(crd: Coord) -> Iterable[Coord]:
        """Iterate over the 26 neighbouring locations."""
        xc, yc, zc = crd
        for x in [xc - 1, xc, xc + 1]:
            for y in [yc - 1, yc, yc + 1]:
                for z in [zc - 1, zc, zc + 1]:
                    if (x, y, z) != crd:
                        yield (x, y, z)

    def neighbours(self, crd: Coord) -> int:
        """Return the number of occupied neighbours in the active grid."""
        count = 0
        for xn in self.neighbouring_locations(crd):
            if self.has_cube(xn):
                count += 1
        return count

    def iterate(self) -> None:
        """Iterate the grid."""
        # Clear the inactive grid
        self.g[self.inactive()] = set()
        for cube in self.g[self.active()]:
            # Keep the cube alive if it has 2 or 3 neighbours
            if self.neighbours(cube) in [2, 3]:
                self.add_cube(cube)
            # Consider all nearby empty locations for activation
            for neighbour in self.neighbouring_locations(cube):
                if not self.has_cube(neighbour) and self.neighbours(neighbour) == 3:
                    self.add_cube(neighbour)
        self.time += 1

    def count_cubes(self) -> int:
        """Count the cubes in the active grid."""
        return len(self.g[self.active()])

    def show(self) -> None:
        """Print the active grid (for debugging)."""
        mins: Coord = (0, 0, 0)
        maxs: Coord = (0, 0, 0)
        for c in self.g[self.active()]:
            mins = min_coord(mins, c)
            maxs = max_coord(maxs, c)
        for z in range(mins[2], maxs[2] + 1):
            print(f"z={z}")
            for y in range(mins[1], maxs[1] + 1):
                for x in range(mins[0], maxs[0] + 1):
                    print("#" if self.has_cube((x, y, z)) else ".", end="")
                print()


test1 = Grid3d([".#.", "..#", "###"])


if __name__ == "__main__":
    grid = Grid3d(stdin)
    for _ in range(0, 6):
        grid.iterate()
    print(grid.count_cubes())
