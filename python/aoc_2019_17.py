"""Advent of Code 2019 - Day 17."""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
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
        if d is Direction.UP:
            return Coord(self.x, self.y - 1)
        elif d is Direction.RIGHT:
            return Coord(self.x + 1, self.y)
        if d is Direction.DOWN:
            return Coord(self.x, self.y + 1)
        elif d is Direction.LEFT:
            return Coord(self.x - 1, self.y)
        else:
            assert_never(d)


class Direction(Enum):
    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT = 3

    @staticmethod
    def from_char(c: str) -> Optional[Direction]:
        if i := "^>v<".find(c) >= 0:
            return Direction(i)
        return None

    def to_char(self) -> str:
        return "^>v<"[self.value]


class Scaffolding:
    def __init__(self, code: List[int]) -> None:
        self.m = Machine(code)
        self.scaffold: Set[Coord] = set()
        self.robot_loc: Coord = Coord.origin()
        self.robot_dir: Direction = Direction.UP
        self.max: Coord = Coord.origin()

        self.m.run()
        image: str = "".join(chr(c) for c in self.m.output_vals)
        for row, line in enumerate(image.split("\n")):
            for col, char in enumerate(line):
                if char != ".":
                    self.scaffold.add(Coord(row, col))
                if (d := Direction.from_char(char)) is not None:
                    self.robot_loc = Coord(row, col)
                    self.robot_dir = d
                if col > self.max.y:
                    self.max = Coord(self.max.y, col)
            if row > self.max.x:
                self.max = Coord(row, self.max.y)

    def show(self) -> None:
        for row in range(self.max.x):
            for col in range(self.max.y):
                c: Coord = Coord(row, col)
                if c == self.robot_loc:
                    print(self.robot_dir.to_char(), end="")
                else:
                    print("#" if Coord(row, col) in self.scaffold else ".", end="")
            print()


if __name__ == "__main__":
    code = [int(x) for x in stdin.read().split(",")]
    scaffolding = Scaffolding(code)
    scaffolding.show()
