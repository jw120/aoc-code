"""Advent of Code 2022 - Day 8."""

from doctest import testmod
from sys import stdin

test_input: list[list[int]] = [
    [3, 0, 3, 7, 3],
    [2, 5, 5, 1, 2],
    [6, 5, 3, 3, 2],
    [3, 3, 5, 4, 9],
    [3, 5, 3, 9, 0],
]


def visible(m: list[list[int]]) -> list[list[bool]]:
    """Return matrix showing which tree are visible from outside the grid.

    >>> test_visible = visible(test_input)
    >>> ["".join("*" if b else "." for b in row) for row in test_visible]
    ['*****', '***.*', '**.**', '*.*.*', '*****']
    >>> sum(sum(row) for row in test_visible)
    21
    """

    rows = len(m)
    cols = len(m[0])

    # Start with everything not visible
    v = [[False for _ in row] for row in m]

    # Scan left and right
    for i in range(rows):
        h_max = -1
        for j in range(cols):
            if m[i][j] > h_max:
                v[i][j] = True
                h_max = m[i][j]
        h_max = -1
        for j in reversed(range(cols)):
            if m[i][j] > h_max:
                v[i][j] = True
                h_max = m[i][j]

    # Scan up and down
    for j in range(cols):
        h_max = -1
        for i in range(rows):
            if m[i][j] > h_max:
                v[i][j] = True
                h_max = m[i][j]
        h_max = -1
        for i in reversed(range(rows)):
            if m[i][j] > h_max:
                v[i][j] = True
                h_max = m[i][j]

    return v


def most_scenic(m: list[list[int]]) -> int:
    """Return highest scenic score.

    >>> most_scenic(test_input)
    8
    """
    return max(scenic(m, i, j) for j in range(len(m[0])) for i in range(len(m)))


def scenic(m: list[list[int]], x: int, y: int) -> int:
    """Return scenic score of a tree.

    >>> scenic(test_input, 1 , 2)
    4
    >>> scenic(test_input, 3 , 2)
    8
    """

    # rows = len(m)
    cols = len(m[0])

    h = m[x][y]

    right = 0
    for q in range(y + 1, cols):
        right += 1
        if m[x][q] >= h:
            break
    left = 0
    for q in range(y - 1, -1, -1):
        left += 1
        if m[x][q] >= h:
            break
    up = 0
    for p in range(x - 1, -1, -1):
        up += 1
        if m[p][y] >= h:
            break
    down = 0
    for p in range(x + 1, cols):
        down += 1
        if m[p][y] >= h:
            break

    return left * right * up * down


if __name__ == "__main__":
    testmod()
    input_m = [[int(c) for c in line.strip()] for line in stdin]
    print(sum(sum(b for b in row) for row in visible(input_m)))
    print(most_scenic(input_m))
