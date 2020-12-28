"""Advent of Code 2019 - Day 11."""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum

# from doctest import testmod
from sys import stdin

from typing import List, Set

from IntCode import Machine

def assert_never(value: NoReturn) -> NoReturn:
    assert False, f"Unhandled value: {value} ({type(value).__name__})"

@dataclass(eq=True, frozen=True)
class Coord:
    x: int
    y: int

    @staticmethod
    def origin() -> Coord:
        return Coord(0, 0)

    def move(self, d: Direction) -> Coord:
        if d == Direction.UP:
            self.y += 1
        elif d == Direction.RIGHT:
            self.x += 1
        if d == Direction.DOWN:
            self.y -= 1
        elif d == Direction.LEFT:
            self.x += 1
        else:
            assert_never(d)



class Colour(Enum):
    BLACK = 0
    WHITE = 1

class Direction(Enum):
    UP = auto()
    RIGHT = auto()
    DOWN = auto()
    LEFT = auto()


class Robot:
    def __init__(self, prog: List[int]) -> None:
        self.painted: Set[Coord] = set()
        self.white: Set[Coord] = set()
        self.pos: Coord = Coord.origin()
        self.direction:
        self.machine = Machine(prog)

    def paint(self, colour: Colour) -> None:
        self.painted.add(self.pos)
        if colour == Colour.WHITE:
            self.white.add(self.pos)

    def step(self) -> None:
        if self.machine.input_vals:
            raise RuntimeError("Expected empty inputs")
        self.machine.input_vals = [
            Colour.WHITE.value if self.pos in self.white else Colour.BLACK.value
        ]
        if self.machine.output_vals:
            raise RuntimeError("Expected empty outputs")
        self.machine.run()
        if self.machine.halted:
            return
        colour, direction = self.machine.output_vals
        self.machine.output_vals = []
        self.paint(Colour(colour))


if __name__ == "__main__":
    #    testmod()
    program: List[int] = [int(s) for s in stdin.read().split(",")]
    robot = Robot(program)
    print(len(robot.painted))
