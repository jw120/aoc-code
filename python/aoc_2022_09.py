"""Advent of Code 2022 - Day 9."""

from doctest import testmod
from sys import stdin

from coord import Coord
from utils import sign

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

longer_test_input = [
    "R 5",
    "U 8",
    "L 8",
    "D 3",
    "R 17",
    "D 10",
    "L 25",
    "U 20",
]


def update(linked_knot: Coord, knot: Coord) -> Coord:
    """Update knot based on new position of linked knot."""

    # Position relative to linked knot
    relative = knot - linked_knot

    # If linked knot is in touching distance, no move needed
    if relative.mag2() <= 2:
        return knot

    # If linked knot two steps left/right/up/down of head, then move one step
    if relative.mag2() == 4:
        return knot - relative // 2

    # If linked knot is a knights-move away, make a diagonal step
    if relative.mag2() == 5:
        return knot - Coord(sign(relative.x), sign(relative.y))

    # If linked knot is two diagonal steps away, move one step diagonally
    if relative.mag2() == 8:
        return knot - Coord(sign(relative.x), sign(relative.y))

    raise ValueError(f"Unexpected difference {linked_knot} {knot}")


def walk(steps: list[str], knot_count: int) -> int:
    """Return number of coordinates visited by the tail.

    >>> walk(test_input, 2)
    13
    >>> walk(test_input, 10)
    1
    >>> walk(longer_test_input, 10)
    36
    """
    knots: list[Coord] = [Coord(0, 0)] * knot_count
    visited: set[Coord] = {knots[knot_count - 1]}

    for step in steps:
        match step.split():
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
            knots[0] += head_move
            for i in range(1, knot_count):
                knots[i] = update(knots[i - 1], knots[i])
            visited.add(knots[knot_count - 1])

    return len(visited)


if __name__ == "__main__":
    testmod()
    moves = [line.strip() for line in stdin]
    print(walk(moves, 2))
    print(walk(moves, 10))
