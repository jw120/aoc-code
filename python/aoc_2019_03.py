"""Advent of Code 2019 - Day 3."""

import enum
from doctest import testmod
from sys import stdin
from typing import List, Set, Tuple

# TODO
# use https://stackoverflow.com/questions/16258553/best-way-to-define-algebraic-data-types-in-python
# Add position class
# Add tests


class Direction(enum.Enum):
    """Direction of a wire segment."""

    UP = enum.auto()
    RIGHT = enum.auto()
    DOWN = enum.auto()
    LEFT = enum.auto()


Position = Tuple[int, int]


def distance(pos: Position) -> int:
    """Return the Manhattan distance to the origin."""
    return abs(pos[0]) + abs(pos[1])


Wire = List[Tuple[Direction, int]]


def parse_wire(line: str) -> Wire:
    """Convert string into a Wire."""

    def parse_segment(seg: str) -> Tuple[Direction, int]:
        distance = int(seg[1:])
        if seg[0] == "U":
            return (Direction.UP, distance)
        if seg[0] == "R":
            return (Direction.RIGHT, distance)
        if seg[0] == "D":
            return (Direction.DOWN, distance)
        if seg[0] == "L":
            return (Direction.LEFT, distance)
        raise RuntimeError("Bad segment", seg)

    return [parse_segment(seg_str) for seg_str in line.split(",")]


def trace(wire: Wire) -> Set[Position]:
    """Trace a wire from the origin, returning a set of positons visited."""
    pos: Position = (0, 0)
    visited: Set[Position] = set()
    for segment in wire:
        if segment[0] == Direction.UP:
            delta = (0, 1)
        elif segment[0] == Direction.RIGHT:
            delta = (1, 0)
        elif segment[0] == Direction.DOWN:
            delta = (0, -1)
        elif segment[0] == Direction.LEFT:
            delta = (-1, 0)
        else:
            raise RuntimeError("Bad segment", segment)
        for i in range(1, segment[1] + 1):
            pos = (pos[0] + delta[0], pos[1] + delta[1])
            visited.add(pos)

    return visited


test_wires: List[Tuple[Wire, Wire]] = [
    (parse_wire("R8,U5,L5,D3"), parse_wire("U7,R6,D4,L4")),
    (
        parse_wire("R75,D30,R83,U83,L12,D49,R71,U7,L72"),
        parse_wire("U62,R66,U55,R34,D71,R55,D58,R83"),
    ),
    (
        parse_wire("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"),
        parse_wire("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"),
    ),
]


def solve_one(w1: Wire, w2: Wire) -> int:
    """Solve part one - closest intersection to the origin.

    >>> solve_one(test_wires[0][0], test_wires[0][1])
    6
    >>> solve_one(test_wires[1][0], test_wires[1][1])
    159
    >>> solve_one(test_wires[2][0], test_wires[2][1])
    135
    """
    intersections: Set[Position] = trace(w1) & trace(w2)
    distances: List[int] = [distance(pos) for pos in intersections]
    return min(distances)


if __name__ == "__main__":
    testmod()
    wire1, wire2 = [parse_wire(line) for line in stdin]
    print(solve_one(wire1, wire2))
