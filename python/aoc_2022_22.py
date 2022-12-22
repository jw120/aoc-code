"""Advent of Code 2022 - Day 22."""

from __future__ import annotations

from doctest import testmod
from enum import Enum
from re import findall
from sys import stdin
from typing import Literal


class Direction(Enum):
    """Directions for walking."""

    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT = 3

    def left(self) -> Direction:
        """Return direction after rotating 90 degrees left."""
        return Direction((self.value - 1) % 4)

    def right(self) -> Direction:
        """Return direction after rotating 90 degrees right."""
        return Direction((self.value + 1) % 4)


class MonkeyMap:
    """Main class for Day 22."""

    # Main board is a list of rows lists, each with cols elements
    board: list[list[Literal["#", ".", " "]]]
    rows: int
    cols: int

    # To facilitate wrapping we also hold the minimum and maximum non-space
    # coordinate for each row and column
    row_min: list[int]
    row_max: list[int]
    col_min: list[int]
    col_max: list[int]

    path: list[str]

    def __init__(self, s: str) -> None:
        board_str, path_str = s.split("\n\n")

        def parse_element(t: str) -> Literal["#", ".", " "]:
            if t == "#":
                return "#"
            if t == ".":
                return "."
            if t == " ":
                return " "
            raise ValueError(f"Failed in parse_element '{t}'")

        self.board = [[parse_element(c) for c in row] for row in board_str.split("\n")]
        self.rows = len(self.board)
        self.cols = max(len(row) for row in self.board)

        self.col_min = []
        self.col_max = []
        for row in self.board:
            valid_tile_indices = [j for j, c in enumerate(row) if c != " "]
            self.col_min.append(min(valid_tile_indices))
            self.col_max.append(max(valid_tile_indices))

        self.row_min = []
        self.row_max = []
        for j in range(self.cols):
            valid_tile_indices = []
            for i in range(self.rows):
                if j <= self.col_max[i] and self.board[i][j] != " ":
                    valid_tile_indices.append(i)
            self.row_min.append(min(valid_tile_indices))
            self.row_max.append(max(valid_tile_indices))

        self.path: list[str] = findall(r"\d+|L|R", path_str)

    def walk(self) -> int:
        """Walk along given path, return final password.

        >>> m = MonkeyMap(TEST_DATA)
        >>> m.walk()
        6032
        """
        r = 0
        c = self.col_min[r]
        direction = Direction.RIGHT
        for step in self.path:
            # print("Step", step)
            match step:
                case s if s.isdigit():
                    for _ in range(int(s)):
                        r, c = self.move(r, c, direction)
                case "L":
                    direction = direction.left()
                case "R":
                    direction = direction.right()
        return 1000 * (r + 1) + 4 * (c + 1) + direction.left().value

    def move(self, r: int, c: int, d: Direction) -> tuple[int, int]:
        """Try to move one step in given direction."""
        match d:
            case Direction.UP:
                r_new = r - 1
                c_new = c
                if r_new < self.row_min[c]:
                    r_new = self.row_max[c]
            case Direction.DOWN:
                r_new = r + 1
                c_new = c
                if r_new > self.row_max[c]:
                    r_new = self.row_min[c]
            case Direction.RIGHT:
                r_new = r
                c_new = c + 1
                if c_new > self.col_max[r]:
                    c_new = self.col_min[r]
            case Direction.LEFT:
                r_new = r
                c_new = c - 1
                if c_new < self.col_min[r]:
                    c_new = self.col_max[r]
        match self.board[r_new][c_new]:
            case "#":
                # print(f"Move {d} {r},{c} -> {r_new},{c_new} Blocked")
                return r, c
            case ".":
                # print(f"Move {d} {r},{c} -> {r_new},{c_new} OK")
                return r_new, c_new
            case " ":
                # print(f"Move {d} {r},{c} -> {r_new},{c_new} Error")
                raise ValueError("Moved out of bounds!")
            case _:
                raise ValueError("Bad case")

    def show(self) -> None:
        """Print debugging data."""
        for i, row in enumerate(self.board):
            for j, c in enumerate(row):
                if j <= self.col_max[i]:
                    print(c, end="")
            print()


TEST_DATA = """        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"""


if __name__ == "__main__":
    testmod()
    m = MonkeyMap(stdin.read())
    m.show()
    print(m.walk())
