"""Advent of Code 2022 - Day 18."""

# from __future__ import annotations

from doctest import testmod

# import fileinput

from Coord import Coord3


def read_cube(s: str) -> Coord3:
    """Read a cube.

    >>> read_cube("1,2,3")
    Coord3(x=1, y=2, z=3)
    """
    values = s.strip().split(",")
    assert len(values) == 3, f"Bad read_cube parse '{s}'"
    return Coord3(int(values[0]), int(values[1]), int(values[2]))


def free_surface(cubes: list[Coord3]) -> int:
    """Return the surface area of the cubes not touching other cubes.

    >>> test_cubes = [read_cube(line) for line in TEST_DATA.splitlines()]
    >>> free_surface(test_cubes)
    64
    """
    count: int = 0
    cubes_set: set[Coord3] = set(cubes)
    for c in cubes:
        count += sum(a not in cubes_set for a in c.adjacents())
    return count


TEST_DATA = """2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"""

if __name__ == "__main__":
    testmod()
    # test_cubes = [read_cube(line) for line in TEST_DATA.splitlines()]
    # print(free_surface(test_cubes))
    # # chamber = Chamber(stdin.read().strip())
    # chamber.drop_multiple(2022)
    # print(chamber.height)
