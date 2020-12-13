"""Advent of Code 2019 - Day 10."""

from doctest import testmod
from math import atan2, gcd, pi
from sys import stdin
from typing import Dict, Iterable, List, Tuple

Vec = Tuple[int, int]


def primitive(v: Vec) -> Vec:
    """Return the shortest vector in the given vector's direction.

    >>> primitive((6, 4))
    (3, 2)
    >>> primitive((3, 0))
    (1, 0)
    """
    (x, y) = v
    if x == 0 and y == 0:
        raise RuntimeError("Zero vector in primitive")
    if x == 0:
        return (0, 1 if y > 0 else -1)
    if y == 0:
        return (1 if x > 0 else -1, 0)
    return (x // gcd(x, y), y // gcd(x, y))


class Grid:
    def __init__(self, s: str) -> None:
        self.m: List[List[bool]] = [
            [s == "#" for s in row.strip()] for row in s.split()
        ]
        self.x_num: int = len(self.m[0])
        self.y_num: int = len(self.m)

    def visible(self, a: int, b: int, c: int, d: int) -> bool:
        """Test if an asteroid is present at (a,b) and can be seen from (c, d)."""
        if not self.m[b][a]:
            return False
        if a == c and b == d:
            return True
        (x_step, y_step) = primitive((a - c, b - d))
        x = c + x_step
        y = d + y_step
        while x != a or y != b:
            if self.m[y][x]:
                return False
            x += x_step
            y += y_step
        return True

    def count_visible(self, p: int, q: int) -> int:
        """Return number of other asteroids visible from (p, q)."""
        count = 0
        for x in range(0, self.x_num):
            for y in range(0, self.y_num):
                count += self.visible(x, y, p, q)
        return count - self.m[q][p]

    def show(self) -> None:
        for y in range(0, self.y_num):
            for x in range(0, self.x_num):
                print("#" if self.m[y][x] else ".", end="")
            print()

    def show2(self) -> None:
        for y in range(0, self.y_num):
            for x in range(0, self.x_num):
                if self.m[y][x]:
                    print(f"{self.count_visible(x, y):3} ", end="")
                else:
                    print("  . ", end="")
            print()

    def best(self) -> Tuple[int, int, int]:
        """Find the asteroid from which the most others are visible.

        >>> test_one.best()
        (3, 4, 8)
        >>> test_two.best()
        (5, 8, 33)
        >>> test_three.best()
        (1, 2, 35)
        >>> test_four.best()
        (6, 3, 41)
        >>> test_five.best()
        (11, 13, 210)
        """
        best_count = -1
        best_x = -1
        best_y = -1
        for y in range(0, self.y_num):
            for x in range(0, self.x_num):
                if self.m[y][x] and self.count_visible(x, y) > best_count:
                    best_count = self.count_visible(x, y)
                    best_x = x
                    best_y = y
        return (best_x, best_y, best_count)

    def laser(self, lx: int, ly: int) -> Iterable[Vec]:
        """Fire lasers from asteroid at (lx, ly)."""

        def sort_by_mag(vs: List[Vec]) -> List[Vec]:
            return sorted(vs, key=lambda v: abs(v[0]) + abs(v[1]))

        def comparison_angle(x: Tuple[Vec, List[Vec]]) -> float:
            base_angle = atan2(x[0][0], -x[0][1])
            if base_angle < 0:
                return base_angle + 2 * pi
            else:
                return base_angle

        # Group all the asteroids by their primitive vector from the laser
        ast_groups: Dict[Vec, List[Vec]] = {}
        for y in range(0, self.y_num):
            for x in range(0, self.x_num):
                if self.m[y][x] and (x != lx or y != ly):
                    delta = (x - lx, y - ly)
                    prim_delta = primitive(delta)
                    if prim_delta in ast_groups:
                        ast_groups[prim_delta].append(delta)
                    else:
                        ast_groups[prim_delta] = [delta]

        # Convert to a list sorted by the angle, with elements as sorted lists
        ast_list: List[Tuple[Vec, List[Vec]]] = [
            (k, sort_by_mag(v)) for (k, v) in ast_groups.items()
        ]
        ast_list.sort(key=comparison_angle)

        # Spin the laser
        count = 0
        while ast_list:
            for _, asts in ast_list:
                count += 1
                yield ((lx + asts[0][0], ly + asts[0][1]))
                asts.pop(0)
            # Prune empty entries
            ast_list = [(v, asts) for (v, asts) in ast_list if asts]

        return 0


def part_two(grid: Grid) -> int:
    """Solve part two.

    >>> part_two(test_five)
    802
    """
    (grid_lx, grid_ly, _) = grid.best()
    target = list(grid.laser(grid_lx, grid_ly))[199]
    return target[0] * 100 + target[1]


test_one: Grid = Grid(
    """.#..#
    .....
    #####
    ....#
    ...##
    """
)

test_two: Grid = Grid(
    """......#.#.
    #..#.#....
    ..#######.
    .#.#.###..
    .#..#.....
    ..#....#.#
    #..#....#.
    .##.#..###
    ##...#..#.
    .#....####
    """
)

test_three: Grid = Grid(
    """
    #.#...#.#.
    .###....#.
    .#....#...
    ##.#.#.#.#
    ....#.#.#.
    .##..###.#
    ..#...##..
    ..##....##
    ......#...
    .####.###.
    """
)

test_four: Grid = Grid(
    """
    .#..#..###
    ####.###.#
    ....###.#.
    ..###.##.#
    ##.##.#.#.
    ....###..#
    ..#.#..#.#
    #..#.#.###
    .##...##.#
    .....#.#..
    """
)

test_five: Grid = Grid(
    """.#..##.###...#######
    ##.############..##.
    .#.######.########.#
    .###.#######.####.#.
    #####.##.#.##.###.##
    ..#####..#.#########
    ####################
    #.####....###.#.#.##
    ##.#################
    #####.##.###..####..
    ..######..##.#######
    ####.##.####...##..#
    .#####..#.######.###
    ##...#.##########...
    #.##########.#######
    .####.#.###.###.#.##
    ....##.##.###..#####
    .#.#.###########.###
    #.#.#.#####.####.###
    ###.##.####.##.#..##
    """
)

test_six: Grid = Grid(
    """.#....#####...#..
    ##...##.#####..##
    ##...#...#.#####.
    ..#.....X...###..
    ..#.#.....#....##
    """
)

if __name__ == "__main__":
    testmod()
    grid: Grid = Grid(stdin.read())
    print(grid.best()[2])
    print(part_two(grid))
