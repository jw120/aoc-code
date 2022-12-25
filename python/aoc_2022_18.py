"""Advent of Code 2022 - Day 18."""

# from __future__ import annotations

from doctest import testmod
import fileinput

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


def cube_content(c_min: Coord3, c_max: Coord3) -> set[Coord3]:
    """Return set of all points within the spanned cube.

    >>> len(cube_content(Coord3(0, 0, 0), Coord3(3, 4, 5))) == 4 * 5 * 6
    True
    """
    return {
        Coord3(x, y, z)
        for x in range(c_min.x, c_max.x + 1)
        for y in range(c_min.y, c_max.y + 1)
        for z in range(c_min.z, c_max.z + 1)
    }


def cube_faces(c_min: Coord3, c_max: Coord3) -> set[Coord3]:
    """Return set of all the points on the face of spanned cube.

    >>> len(cube_faces(Coord3(0, 0, 0), Coord3(3, 3, 3))) == (6 * 2 * 2 + 12 * 2 + 8)
    True
    """
    x_range = range(c_min.x, c_max.x + 1)
    y_range = range(c_min.y, c_max.y + 1)
    z_range = range(c_min.z, c_max.z + 1)
    return (
        {Coord3(c_min.x, y, z) for y in y_range for z in z_range}
        | {Coord3(c_max.x, y, z) for y in y_range for z in z_range}
        | {Coord3(x, c_min.y, z) for x in x_range for z in z_range}
        | {Coord3(x, c_max.y, z) for x in x_range for z in z_range}
        | {Coord3(x, y, c_min.z) for x in x_range for y in y_range}
        | {Coord3(x, y, c_max.z) for x in x_range for y in y_range}
    )


def extent(cubes: list[Coord3]) -> tuple[Coord3, Coord3]:
    """Return min and max corners of cuboid extent of the cubes.

    >>> test_cubes = [read_cube(line) for line in TEST_DATA.splitlines()]
    >>> extent(test_cubes)
    (Coord3(x=1, y=1, z=1), Coord3(x=3, y=3, z=6))
    """
    xs = [c.x for c in cubes]
    ys = [c.y for c in cubes]
    zs = [c.z for c in cubes]
    x_min = min(xs)
    x_max = max(xs)
    y_min = min(ys)
    y_max = max(ys)
    z_min = min(zs)
    z_max = max(zs)
    return Coord3(x_min, y_min, z_min), Coord3(x_max, y_max, z_max)


def exterior_surface(cubes: list[Coord3]) -> int:
    """Return the externally accessible surface of the cubes.

    >>> test_cubes = [read_cube(line) for line in TEST_DATA.splitlines()]
    >>> exterior_surface(test_cubes)
    58
    """
    # Find the interior points by flood-filling from the outside
    occupied: set[Coord3] = set(cubes)
    cube_min, cube_max = extent(cubes)
    universe: set[Coord3] = cube_content(cube_min, cube_max)
    exterior_frontier: set[Coord3] = cube_faces(cube_min, cube_max) - occupied
    exterior: set[Coord3] = exterior_frontier.copy()
    while exterior_frontier:
        new_frontier = set()
        for c in exterior_frontier:
            for p in c.adjacents():
                if (
                    p in universe
                    and p not in occupied
                    and p not in exterior
                    and p not in new_frontier
                ):
                    new_frontier.add(p)
        exterior_frontier = new_frontier
        exterior |= new_frontier
    interior = universe - exterior - occupied

    # Count the exposed area now taking into account the interior space
    count = 0
    blocked = occupied | interior
    for c in cubes:
        count += sum(a not in blocked for a in c.adjacents())
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
    input_cubes = [read_cube(line) for line in fileinput.input()]
    print(free_surface(input_cubes))
    print(exterior_surface(input_cubes))
