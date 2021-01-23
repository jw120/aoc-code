"""Advent of Code 2019 - Day 17."""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from sys import stdin
from typing import NoReturn, Optional

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
            return Coord(self.x - 1, self.y)
        elif d is Direction.RIGHT:
            return Coord(self.x, self.y + 1)
        if d is Direction.DOWN:
            return Coord(self.x + 1, self.y)
        elif d is Direction.LEFT:
            return Coord(self.x, self.y - 1)
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

    def opposite(self) -> Direction:
        return Direction((self.value + 2) % 4)


class Scaffolding:
    def __init__(self, code: list[int]) -> None:
        self.m = Machine(code)
        self.scaffold: set[Coord] = set()
        self.intersections: set[Coord] = set()
        self.robot_loc: Coord
        self.robot_dir: Direction
        self.max: Coord = Coord.origin()
        self._scan_image()
        self._walk_scaffold()

    def _scan_image(self) -> None:
        """Find the robot and scaffold locations from IntCode machine output."""
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

    def _walk_scaffold(self) -> int:
        """Find intersections by walking the scaffold from the robot's location."""
        current_loc: Coord = self.robot_loc
        current_dir: Direction = self.robot_dir

        while True:
            available_dirs = [
                d
                for d in Direction
                if current_loc.move(d) in self.scaffold and d != current_dir.opposite()
            ]
            if not available_dirs:  # Dead-end means we have finished
                break
            elif len(available_dirs) == 1:  # Just keep going
                current_dir = available_dirs[0]
            elif len(available_dirs) == 3:  # Cross-roads
                self.intersections.add(current_loc)
            else:
                raise RuntimeError("Unexpected branch in scaffold")
            current_loc = current_loc.move(current_dir)
        return len(self.intersections)

    def tally_intersections(self) -> int:
        return sum(c.x * c.y for c in self.intersections)

    def show(self) -> None:
        for row in range(self.max.x):
            for col in range(self.max.y):
                c: Coord = Coord(row, col)
                if c == self.robot_loc:
                    print(self.robot_dir.to_char(), end="")
                elif c in self.intersections:
                    print("O", end="")
                else:
                    print("#" if Coord(row, col) in self.scaffold else ".", end="")
            print()


if __name__ == "__main__":
    code = [int(x) for x in stdin.read().split(",")]
    scaffolding = Scaffolding(code)
    print(scaffolding.tally_intersections())
    scaffolding.show()
