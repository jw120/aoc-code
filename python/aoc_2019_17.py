"""Advent of Code 2019 - Day 17."""

from __future__ import annotations

from sys import stdin
from typing import Optional

from coord import Coord
from direction import Direction
from int_code import Machine
from utils import assert_never


def move(c: Coord, d: Direction) -> Coord:
    """Return coordinate after moving."""
    if d is Direction.UP:
        return c + Coord(-1, 0)
    if d is Direction.RIGHT:
        return c + Coord(0, 1)
    if d is Direction.DOWN:
        return c + Coord(1, 0)
    if d is Direction.LEFT:
        return c + Coord(0, -1)
    assert_never(d)


def direction_from_char(c: str) -> Optional[Direction]:
    """Read direction from a character string."""
    if i := "^>v<".find(c) >= 0:
        return Direction(i)
    return None


def direction_to_char(d: Direction) -> str:
    """Convert to a character string."""
    return "^>v<"[d.value]


class Scaffolding:
    """Main class for day 17."""

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
                if (d := direction_from_char(char)) is not None:
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
                if move(current_loc, d) in self.scaffold and d != current_dir.opposite()
            ]
            if not available_dirs:  # Dead-end means we have finished
                break
            if len(available_dirs) == 1:  # Just keep going
                current_dir = available_dirs[0]
            elif len(available_dirs) == 3:  # Cross-roads
                self.intersections.add(current_loc)
            else:
                raise RuntimeError("Unexpected branch in scaffold")
            current_loc = move(current_loc, current_dir)
        return len(self.intersections)

    def tally_intersections(self) -> int:
        """Return tally of intersections."""
        return sum(c.x * c.y for c in self.intersections)

    def show(self) -> None:
        """Print debugging information."""
        for row in range(self.max.x):
            for col in range(self.max.y):
                c: Coord = Coord(row, col)
                if c == self.robot_loc:
                    print(direction_to_char(self.robot_dir), end="")
                elif c in self.intersections:
                    print("O", end="")
                else:
                    print("#" if Coord(row, col) in self.scaffold else ".", end="")
            print()


if __name__ == "__main__":
    input_code = [int(x) for x in stdin.read().split(",")]
    scaffolding = Scaffolding(input_code)
    print(scaffolding.tally_intersections())
    scaffolding.show()
