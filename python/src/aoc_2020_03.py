"""Advent of Code 2020 - Day 3."""

from doctest import testmod
from sys import stdin

Line = list[bool]  # Horizontal row of trees (true = a tree)
Grid = list[Line]


def parse_line(s: str) -> Line:
    """Read a Line from the string."""
    return [ch == "#" for ch in s.strip()]


test_grid: Grid = [
    parse_line(s)
    for s in [
        "..##.......",
        "#...#...#..",
        ".#....#..#.",
        "..#.#...#.#",
        ".#...##..#.",
        "..#.##.....",
        "#.#.#....#",
        ".#........#",
        "#.##...#...",
        "#...##....#",
        ".#..#...#.#",
    ]
]


def tree_count(grid: Grid, right: int, down: int) -> int:
    """Count the number of trees moving with specified step.

    >>> tree_count(test_grid, 3, 1)
    7
    """
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


if __name__ == "__main__":
    testmod()
    input_grid: Grid = [parse_line(s) for s in stdin]
    print(tree_count(input_grid, 3, 1))
    print(
        tree_count(input_grid, 1, 1)
        * tree_count(input_grid, 3, 1)
        * tree_count(input_grid, 5, 1)
        * tree_count(input_grid, 7, 1)
        * tree_count(input_grid, 1, 2)
    )
