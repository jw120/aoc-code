"""Advent of Code 2020 - Day 3s."""

from sys import stdin
from typing import List

Line = List[bool]  # Horizontal row of trees (true = a tree)
Grid = List[Line]


def tree_count(grid: Grid, right: int, down: int) -> int:
    """Count the number of trees moving with specified step."""
    height: int = len(grid)
    width: int = len(grid[0])
    col: int = 0
    row: int = 0
    count: int = 0
    while row < height:
        count += grid[row][col]
        col = (col + right) % width
        row += down
    return count


def parse_line(s: str) -> Line:
    """Convert a line of string input into a line of trees."""
    return [ch == "#" for ch in s.strip()]


if __name__ == "__main__":
    grid: Grid = [parse_line(s) for s in stdin]
    print(tree_count(grid, 3, 1))
    print(
        tree_count(grid, 1, 1)
        * tree_count(grid, 3, 1)
        * tree_count(grid, 5, 1)
        * tree_count(grid, 7, 1)
        * tree_count(grid, 1, 2)
    )
