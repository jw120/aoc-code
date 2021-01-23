"""Advent of Code 2019 - Day 3."""

from __future__ import annotations

from dataclasses import dataclass
from doctest import testmod
from sys import stdin
from typing import ClassVar, Union


@dataclass(eq=True, frozen=True)
class Offset:
    """Represents (x, y) offset from origin or a movement."""

    x: int
    y: int

    def distance(self) -> int:
        """Manhattan distance from origin."""
        return abs(self.x) + abs(self.y)

    def __add__(self, delta: Offset) -> Offset:
        return Offset(self.x + delta.x, self.y + delta.y)


@dataclass
class Up:
    offset: ClassVar[Offset] = Offset(0, 1)
    name: ClassVar[str] = "U"


@dataclass
class Right:
    offset: ClassVar[Offset] = Offset(1, 0)
    name: ClassVar[str] = "R"


@dataclass
class Down:
    offset: ClassVar[Offset] = Offset(0, -1)
    name: ClassVar[str] = "D"


@dataclass
class Left:
    offset: ClassVar[Offset] = Offset(-1, 0)
    name: ClassVar[str] = "L"


Direction = Union[Up, Right, Down, Left]


def parse_direction(s: str) -> Direction:
    if s == Up.name:
        return Up()
    if s == Right.name:
        return Right()
    if s == Down.name:
        return Down()
    if s == Left.name:
        return Left()
    raise RuntimeError("Unknown direction", s)


Wire = list[tuple[Direction, int]]


def parse_wire(line: str) -> Wire:
    def parse_segment(s: str) -> tuple[Direction, int]:
        direction: Direction = parse_direction(s[0])
        distance = int(s[1:])
        return (direction, distance)

    return [parse_segment(seg_str) for seg_str in line.split(",")]


def trace(wire: Wire) -> set[Offset]:
    """Trace along wire from the origin, returning a set of offsets visited."""
    pos: Offset = Offset(0, 0)
    visited: set[Offset] = set()
    for direction, distance in wire:
        for i in range(1, distance + 1):
            pos = pos + direction.offset
            visited.add(pos)
    return visited


def trace_steps(wire: Wire) -> dict[Offset, int]:
    """Trace along wire from the origin.

    Returns a map where the keys are the offsets visited and the values the number of
    steps taken for the first visit to that offset.
    """
    pos: Offset = Offset(0, 0)
    steps: int = 0
    visited: dict[Offset, int] = {}
    for direction, distance in wire:
        for i in range(1, distance + 1):
            steps += 1
            pos = pos + direction.offset
            if pos not in visited:
                visited[pos] = steps
    return visited


test_wires: list[tuple[Wire, Wire]] = [
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
    >>> #solve_one(test_wires[1][0], test_wires[1][1])
    159
    >>> #solve_one(test_wires[2][0], test_wires[2][1])
    135
    """
    intersections: set[Offset] = trace(w1) & trace(w2)
    return min(p.distance() for p in intersections)


def solve_two(w1: Wire, w2: Wire) -> int:
    """Solve part two - least steps intersection.

    >>> solve_two(test_wires[0][0], test_wires[0][1])
    30
    >>> solve_two(test_wires[1][0], test_wires[1][1])
    610
    >>> solve_two(test_wires[2][0], test_wires[2][1])
    410
    """
    steps1: dict[Offset, int] = trace_steps(w1)
    steps2: dict[Offset, int] = trace_steps(w2)
    intersections: set[Offset] = set(steps1.keys()) & set(steps2.keys())
    return min((steps1[p] + steps2[p]) for p in intersections)


if __name__ == "__main__":
    testmod()
    wire1, wire2 = [parse_wire(line) for line in stdin]
    print(solve_one(wire1, wire2))
    print(solve_two(wire1, wire2))
