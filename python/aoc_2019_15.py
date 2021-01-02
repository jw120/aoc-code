"""Advent of Code 2019 - Day 15."""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from random import randrange
from sys import stdin
from typing import List, NoReturn, Optional, Set

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
        if d is Direction.NORTH:
            return Coord(self.x, self.y + 1)
        elif d is Direction.EAST:
            return Coord(self.x + 1, self.y)
        if d is Direction.SOUTH:
            return Coord(self.x, self.y - 1)
        elif d is Direction.WEST:
            return Coord(self.x - 1, self.y)
        else:
            assert_never(d)


class Direction(Enum):
    NORTH = 1
    SOUTH = 2
    WEST = 3
    EAST = 4

    @staticmethod
    def random() -> Direction:
        return Direction(randrange(1, 5))


class Controller:
    def __init__(self, code: List[int]) -> None:
        self.m = Machine(code)
        self.m.pause_after_output = True
        self.m.pause_before_input = True
        self.loc: Coord = Coord.origin()
        self.walls: Set[Coord] = set()
        self.explored: Set[Coord] = {self.loc}
        self.oxygen: Optional[Coord] = None

    def explore(self) -> Controller:
        for _ in range(10_000):
            self.explored.add(self.loc)
            d = Direction.random()
            next_loc = self.loc.move(d)
            self.m.input_vals = [d.value]
            self.m.run()
            response = self.m.output_vals.pop()
            if response == 0:
                self.walls.add(next_loc)
            elif response == 1:
                self.loc = next_loc
            elif response == 2:
                self.oxygen = next_loc
                self.loc = next_loc
            else:
                raise RuntimeError("Bad response from machine", response)

        return self

    def show(self) -> Controller:
        xs = [w.x for w in self.walls]
        ys = [w.y for w in self.walls]
        x_min = min(xs)
        x_max = max(xs)
        y_min = min(ys)
        y_max = max(ys)
        for y in range(y_max, y_min - 1, -1):
            for x in range(x_min, x_max + 1):
                if Coord(x, y) == Coord.origin():
                    print("X", end="")
                elif Coord(x, y) == self.oxygen:
                    print("*", end="")
                elif Coord(x, y) in self.walls:
                    print("#", end="")
                elif Coord(x, y) in self.explored:
                    print(".", end="")
                else:
                    print(" ", end="")
            print()
        return self


if __name__ == "__main__":
    code = [int(x) for x in stdin.read().split(",")]
    Controller(code).explore().show()
