"""Advent of Code 2020 - Day 17."""

from collections.abc import Iterable
from sys import stdin

Coord3 = tuple[int, int, int]


def min_coord3(p: Coord3, q: Coord3) -> Coord3:
    """Minimum of two 3-coordinates."""
    return (min(p[0], q[0]), min(p[1], q[1]), min(p[2], q[2]))


def max_coord3(p: Coord3, q: Coord3) -> Coord3:
    """Maximum of two 3-coordinates."""
    return (max(p[0], q[0]), max(p[1], q[1]), max(p[2], q[2]))


Coord4 = tuple[int, int, int, int]


def min_coord4(p: Coord4, q: Coord4) -> Coord4:
    """Minimum of two 4-coordinates."""
    return (min(p[0], q[0]), min(p[1], q[1]), min(p[2], q[2]), min(p[3], q[3]))


def max_coord4(p: Coord4, q: Coord4) -> Coord4:
    """Maximum of two 4-coordinates."""
    return (max(p[0], q[0]), max(p[1], q[1]), max(p[2], q[2]), max(p[3], q[2]))


class Grid3d:
    """Provide a 3d Grid of cubes.

    Holds two copies of the grid data so one can be updated while
    the other is being read.
    """

    def __init__(self, lines: Iterable[str]) -> None:
        self.time: int = -1
        self.g: list[set[Coord3]] = [set(), set()]
        for y, line in enumerate(lines):
            for x, ch in enumerate(line):
                if ch == "#":
                    self.add_cube((x, y, 0))
        self.time = 0

    def active(self) -> int:
        """Return the index of the active grid data."""
        return self.time % 2

    def inactive(self) -> int:
        """Return the index of the inactive grid data."""
        return (self.time + 1) % 2

    def add_cube(self, crd: Coord3) -> None:
        """Add a cube to the inactive grid."""
        self.g[self.inactive()].add(crd)

    def has_cube(self, crd: Coord3) -> bool:
        """Is there a cube at given point in the active grid."""
        return crd in self.g[self.active()]

    @staticmethod
    def neighbouring_locations(crd: Coord3) -> Iterable[Coord3]:
        """Iterate over the 26 neighbouring locations."""
        xc, yc, zc = crd
        for x in [xc - 1, xc, xc + 1]:
            for y in [yc - 1, yc, yc + 1]:
                for z in [zc - 1, zc, zc + 1]:
                    if (x, y, z) != crd:
                        yield (x, y, z)

    def neighbours(self, crd: Coord3) -> int:
        """Return the number of occupied neighbours in the active grid."""
        count = 0
        for xn in self.neighbouring_locations(crd):
            if self.has_cube(xn):
                count += 1
        return count

    def iterate(self) -> None:
        """Iterate the grid."""
        # Clear the inactive grid
        self.g[self.inactive()].clear()
        for cube in self.g[self.active()]:
            # Keep the cube alive if it has 2 or 3 neighbours
            if self.neighbours(cube) in {2, 3}:
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
        min_coord: Coord3 = (0, 0, 0)
        max_coord: Coord3 = (0, 0, 0)
        for c in self.g[self.active()]:
            min_coord = min_coord3(min_coord, c)
            max_coord = max_coord3(max_coord, c)
        for z in range(min_coord[2], max_coord[2] + 1):
            print(f"z={z}")
            for y in range(min_coord[1], max_coord[1] + 1):
                for x in range(min_coord[0], max_coord[0] + 1):
                    print("#" if self.has_cube((x, y, z)) else ".", end="")
                print()


class Grid4d:
    """Provide a 4d Grid of cubes.

    Holds two copies of the grid data so one can be updated while
    the other is being read.
    """

    def __init__(self, lines: Iterable[str]) -> None:
        self.time: int = -1
        self.g: list[set[Coord4]] = [set(), set()]
        for y, line in enumerate(lines):
            for x, ch in enumerate(line):
                if ch == "#":
                    self.add_cube((x, y, 0, 0))
        self.time = 0

    def active(self) -> int:
        """Return the index of the active grid data."""
        return self.time % 2

    def inactive(self) -> int:
        """Return the index of the inactive grid data."""
        return (self.time + 1) % 2

    def add_cube(self, crd: Coord4) -> None:
        """Add a cube to the inactive grid."""
        self.g[self.inactive()].add(crd)

    def has_cube(self, crd: Coord4) -> bool:
        """Is there a cube at given point in the active grid."""
        return crd in self.g[self.active()]

    @staticmethod
    def neighbouring_locations(crd: Coord4) -> Iterable[Coord4]:
        """Iterate over the 26 neighbouring locations."""
        xc, yc, zc, wc = crd
        for x in [xc - 1, xc, xc + 1]:
            for y in [yc - 1, yc, yc + 1]:
                for z in [zc - 1, zc, zc + 1]:
                    for w in [wc - 1, wc, wc + 1]:
                        if (x, y, z, w) != crd:
                            yield (x, y, z, w)

    def neighbours(self, crd: Coord4) -> int:
        """Return the number of occupied neighbours in the active grid."""
        count = 0
        for xn in self.neighbouring_locations(crd):
            if self.has_cube(xn):
                count += 1
        return count

    def iterate(self) -> None:
        """Iterate the grid."""
        self.g[self.inactive()].clear()
        for cube in self.g[self.active()]:
            # Keep the cube alive if it has 2 or 3 neighbours
            if self.neighbours(cube) in {2, 3}:
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
        min_coord: Coord4 = (0, 0, 0, 0)
        max_coord: Coord4 = (0, 0, 0, 0)
        for c in self.g[self.active()]:
            min_coord = min_coord4(min_coord, c)
            max_coord = max_coord4(max_coord, c)
        for w in range(min_coord[3], max_coord[3] + 1):
            for z in range(min_coord[2], max_coord[2] + 1):
                print(f"z={z}, w={w}")
                for y in range(min_coord[1], max_coord[1] + 1):
                    for x in range(min_coord[0], max_coord[0] + 1):
                        print("#" if self.has_cube((x, y, z, w)) else ".", end="")
                    print()
                print()


test3 = Grid3d([".#.", "..#", "###"])
test4 = Grid4d([".#.", "..#", "###"])


if __name__ == "__main__":
    input_lines = list(stdin)
    grid3 = Grid3d(input_lines)
    for _ in range(6):
        grid3.iterate()
    print(grid3.count_cubes())
    grid4 = Grid4d(input_lines)
    for _ in range(6):
        grid4.iterate()
    print(grid4.count_cubes())
