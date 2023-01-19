"""Advent of Code 2019 - Day 11."""

from __future__ import annotations

from enum import Enum
from sys import stdin

from coord import Coord
from direction import Direction
from int_code import Machine
from utils import assert_never


def move(c: Coord, d: Direction) -> Coord:
    """Generate a new coordinate after a move in given direction."""
    if d is Direction.UP:
        return c + Coord(0, 1)
    if d is Direction.RIGHT:
        return c + Coord(1, 0)
    if d is Direction.DOWN:
        return c + Coord(0, -1)
    if d is Direction.LEFT:
        return c + Coord(-1, 0)
    assert_never(d)


class Colour(Enum):
    """Colours."""

    BLACK = 0
    WHITE = 1


class Robot:
    """Main class for day 11."""

    def __init__(self, prog: list[int]) -> None:
        self.painted: set[Coord] = set()
        self.white: set[Coord] = set()
        self.pos: Coord = Coord.origin()
        self.direction: Direction = Direction.UP
        self.machine = Machine(prog)
        self.machine.pause_after_output = True

    def paint(self, colour: Colour) -> Robot:
        """Paint in given colour."""
        self.painted.add(self.pos)
        if colour == Colour.WHITE:
            self.white.add(self.pos)
        else:
            if self.pos in self.white:
                self.white.remove(self.pos)
        return self

    def run(self) -> Robot:
        """Run the robot."""
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
            match self.machine.output_vals:
                case colour, rot:
                    self.machine.output_vals = []
                    self.paint(Colour(colour))
                    if rot:
                        self.direction = self.direction.right()
                    else:
                        self.direction = self.direction.left()
                    self.pos = move(self.pos, self.direction)
                case _:
                    raise ValueError("Unexpected output values.")

    def show(self) -> Robot:
        """Print for debugging."""
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
