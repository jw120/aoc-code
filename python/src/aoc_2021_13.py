"""Advent of Code 2021 - Day 13."""

from __future__ import annotations

from doctest import testmod
from sys import stdin
from typing import Literal

from coord import Coord


def read_dot(s: str) -> Coord:
    """Read a dot.

    >>> read_dot("1,10")
    Coord(x=1, y=10)
    """
    row, col = (int(x) for x in s.split(","))
    return Coord(row, col)


def read_fold(s: str) -> Fold:
    """Read a fold.

    >>> read_fold("fold along y=78")
    ('y', 78)
    """
    axis, value = s.removeprefix("fold along ").split("=")
    if value.isnumeric():
        if axis == "x":
            return ("x", int(value))
        if axis == "y":
            return ("y", int(value))
    raise ValueError("Unrecognized fold: " + s)


test_dots: set[Coord] = {
    read_dot(s)
    for s in [
        "6,10",
        "0,14",
        "9,10",
        "0,3",
        "10,4",
        "4,11",
        "6,0",
        "6,12",
        "4,1",
        "0,13",
        "10,12",
        "3,4",
        "3,0",
        "8,4",
        "1,10",
        "2,14",
        "8,10",
        "9,0",
    ]
}

test_folds: list[Fold] = [read_fold(s) for s in ["fold along y=7", "fold along x=5"]]

Fold = tuple[Literal["x", "y"], int]


def fold_dot(dot: Coord, fold: Fold) -> Coord:
    """Apply a fold to the dot."""
    fold_axis, fold_value = fold
    if fold_axis == "x":
        return (
            dot
            if dot.x < fold_value
            else Coord(fold_value - (dot.x - fold_value), dot.y)
        )
    return (
        dot if dot.y < fold_value else Coord(dot.x, fold_value - (dot.y - fold_value))
    )


def apply_folds(dots: set[Coord], folds: list[Fold]) -> set[Coord]:
    """Apply a series of folds.

    >>> len(apply_folds(test_dots, test_folds[:1]))
    17
    >>> len(apply_folds(test_dots, test_folds))
    16
    """
    for f in folds:
        dots = {fold_dot(d, f) for d in dots}
    return dots


def print_dots(dots: set[Coord]) -> None:
    """Print the dots in output form."""
    x_max = max(d.x for d in dots)
    y_max = max(d.y for d in dots)
    for y in range(y_max + 1):
        for x in range(x_max + 1):
            print("#" if Coord(x, y) in dots else " ", end="")
        print()


if __name__ == "__main__":
    testmod()
    dots_str, folds_str = stdin.read().split("\n\n")
    input_dots = {read_dot(s) for s in dots_str.splitlines()}
    input_folds = [read_fold(s) for s in folds_str.splitlines()]
    print(len(apply_folds(input_dots, input_folds[:1])))
    print_dots(apply_folds(input_dots, input_folds))
