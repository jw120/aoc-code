"""Advent of Code 2022 - Day 9."""

from doctest import testmod
from sys import stdin

from Coord import Coord

test_input: list[str] = [
    "R 4",
    "U 4",
    "L 3",
    "D 1",
    "R 4",
    "D 1",
    "L 5",
    "R 2",
]


def sign(x: int) -> int:
    """Return sign of an integer."""
    if x > 0:
        return 1
    if x < 0:
        return -1
    return 0


def new_tail(h: Coord, t: Coord) -> Coord:
    """Return new tail position."""

    # Position of tail relative to head
    d = t - h

    # If tail touches head, then tail stays where it is
    if d.mag2() <= 2:
        return t

    # If tail two steps left/right/up/down of head, then move one step
    if d.mag2() == 4:
        return t - d // 2

    # If tail is a knights-move away, make a diagonal step
    if d.mag2() == 5:
        return t - Coord(sign(d.x), sign(d.y))

    raise ValueError(f"Unexpected difference {h} {t} {d}")


def walk(steps: list[str]) -> int:
    """Return number of coordinates visited by the tail.

    >>> walk(test_input)
    13
    """
    head = Coord(0, 0)
    tail = Coord(0, 0)
    visited: set[Coord] = {tail}

    for step in steps:
        match step.strip().split():
            case ["U", n] if n.isdigit():
                head_move = Coord(0, 1)
            case ["D", n] if n.isdigit():
                head_move = Coord(0, -1)
            case ["L", n] if n.isdigit():
                head_move = Coord(-1, 0)
            case ["R", n] if n.isdigit():
                head_move = Coord(1, 0)
            case _:
                raise ValueError(f"Bad step: '{step}'")
        for _ in range(int(n)):
            head += head_move
            tail = new_tail(head, tail)
            visited.add(tail)

    return len(visited)


if __name__ == "__main__":
    testmod()
    print(walk(stdin.readlines()))
