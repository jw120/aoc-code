"""Advent of Code 2022 - Day 12."""

from doctest import testmod

# from sys import stdin

from Coord import Coord


class HeightMap:
    def __init__(self, s: str) -> None:
        s_lines = s.split("\n")

        self.rows = len(s_lines)
        self.cols = len(s_lines[0])
        self.start = Coord(-1, -1)
        self.goal = Coord(-1, -1)
        self.h: list[list[int]] = [
            [0 for _ in range(self.cols)] for _ in range(self.rows)
        ]

        for i in range(self.rows):
            for j in range(self.cols):
                c = s_lines[i][j]
                if s_lines[i][j] == "S":
                    self.start = Coord(i, j)
                    c = "a"
                elif s_lines[i][j] == "E":
                    self.goal = Coord(i, j)
                    c = "z"
                self.h[i][j] = ord(c) - ord("a") + 1


test_input = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""

if __name__ == "__main__":
    testmod()
    height_map = HeightMap(test_input)
    print(height_map.h)
