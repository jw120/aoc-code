"""Advent of Code 2019 - Day 10."""

from math import gcd
from typing import List, Tuple

# from sys import stdin


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

if __name__ == "__main__":
    #    grid: Grid = Grid(stdin.read())
    test_one.show()
    test_one.show2()
    print(test_one.best())
