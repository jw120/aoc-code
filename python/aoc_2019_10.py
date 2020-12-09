"""Advent of Code 2019 - Day 10."""

from doctest import testmod
from math import gcd
from sys import stdin
from typing import List, Tuple


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
        elif a == c:
            x_step: int = 0
            y_step: int = 1 if b > d else -1
        elif b == d:
            x_step = 1 if a > c else -1
            y_step = 0
        else:
            x_step = (a - c) // gcd(a - c, b - d)
            y_step = (b - d) // gcd(a - c, b - d)
        #       print("Visible?", (a, b), "from", (c, d), "step:", (x_step, y_step))
        x = c + x_step
        y = d + y_step
        while x != a or y != b:
            #            print("  checking", (x, y))
            if self.m[y][x]:
                #                print("  blocker found")
                return False
            x += x_step
            y += y_step
        #        print("  visible")
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
            #                print("#" if self.visible(x, y, 3, 4) else ".", end="")
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


if __name__ == "__main__":
    testmod()
    grid: Grid = Grid(stdin.read())
    (_, _, num) = grid.best()
    print(num)
