"""Advent of Code 2019 - Day 11."""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from sys import stdin

from IntCode import Machine
from utils import assert_never


@dataclass(eq=True, frozen=True)
class Coord:
    x: int
    y: int

    @staticmethod
    def origin() -> Coord:
        return Coord(0, 0)

    def move(self, d: Direction) -> Coord:
        if d is Direction.UP:
            return Coord(self.x, self.y + 1)
        elif d is Direction.RIGHT:
            return Coord(self.x + 1, self.y)
        if d is Direction.DOWN:
            return Coord(self.x, self.y - 1)
        elif d is Direction.LEFT:
            return Coord(self.x - 1, self.y)
        else:
            assert_never(d)


class Colour(Enum):
    BLACK = 0
    WHITE = 1


class Direction(Enum):
    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT = 3

    def rotate(self, rot: int) -> Direction:
        if rot == 1:
            return Direction((self.value + 1) % 4)
        elif rot == 0:
            return Direction((self.value - 1) % 4)
        else:
            raise RuntimeError("Unknown direction to rotate")


class Robot:
    def __init__(self, prog: list[int]) -> None:
        self.painted: set[Coord] = set()
        self.white: set[Coord] = set()
        self.pos: Coord = Coord.origin()
        self.direction: Direction = Direction.UP
        self.machine = Machine(prog)
        self.machine.pause_after_output = True

    def paint(self, colour: Colour) -> Robot:
        self.painted.add(self.pos)
        if colour == Colour.WHITE:
            self.white.add(self.pos)
        else:
            if self.pos in self.white:
                self.white.remove(self.pos)
        return self

    def run(self) -> Robot:
        while True:
            if self.machine.input_vals:
                raise RuntimeError("Expected empty input")
            self.machine.input_vals = [
                Colour.WHITE.value if self.pos in self.white else Colour.BLACK.value
            ]
            if self.machine.output_vals:
                raise RuntimeError("Expected empty outputs")
            while len(self.machine.output_vals) < 2:
                self.machine.run()
                if self.machine.halted:
                    return self
            colour, rot = self.machine.output_vals
            self.machine.output_vals = []
            self.paint(Colour(colour))
            self.direction = self.direction.rotate(rot)
            self.pos = self.pos.move(self.direction)

    def show(self) -> Robot:
        xs = [pt.x for pt in self.white]
        ys = [pt.y for pt in self.white]
        x_min = min(xs)
        x_max = max(xs)
        y_min = min(ys)
        y_max = max(ys)
        for y in range(y_max, y_min - 1, -1):
            for x in range(x_min, x_max + 1):
                print("#" if Coord(x, y) in self.white else " ", end="")
            print()
        return self


if __name__ == "__main__":
    #    testmod()
    program: list[int] = [int(s) for s in stdin.read().split(",")]
    print(len(Robot(program).run().painted))
    Robot(program).paint(Colour.WHITE).run().show()
