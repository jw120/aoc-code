"""Advent of Code 2020 - Day 12."""

from __future__ import annotations

from doctest import testmod
from enum import Enum, auto
from sys import stdin
from typing import Iterable, List, Tuple


class Command(Enum):
    """Commands given to the ship."""

    NORTH = auto()
    EAST = auto()
    SOUTH = auto()
    WEST = auto()
    LEFT = auto()
    RIGHT = auto()
    FORWARD = auto()

    def left(self) -> Command:
        if self == Command.NORTH:
            return Command.WEST
        if self == Command.EAST:
            return Command.NORTH
        if self == Command.SOUTH:
            return Command.EAST
        if self == Command.WEST:
            return Command.SOUTH
        raise RuntimeError("Bad command to rotate", self)


def parse_command(s: str) -> Tuple[Command, int]:
    param = int(s[1:])
    if s[0] == "N":
        return (Command.NORTH, param)
    if s[0] == "E":
        return (Command.EAST, param)
    if s[0] == "S":
        return (Command.SOUTH, param)
    if s[0] == "W":
        return (Command.WEST, param)
    if s[0] == "F":
        return (Command.FORWARD, param)
    if s[0] == "L":
        return (Command.LEFT, param)
    if s[0] == "R":
        return (Command.RIGHT, param)
    raise RuntimeError("Bad command to parse", s)


test1 = [parse_command(s) for s in ["F10", "N3", "F7", "R90", "F11"]]


class Ship:
    """Basic ship for part one"""

    def __init__(self) -> None:
        self.x = 0
        self.y = 0
        self.facing = Command.EAST

    def action(self, com: Command, param: int) -> Ship:
        """Update the ship based on the action.

        >>> Ship().action(Command.FORWARD, 10).x
        10
        """
        if com == Command.NORTH:
            self.y += param
        elif com == Command.EAST:
            self.x += param
        elif com == Command.SOUTH:
            self.y -= param
        elif com == Command.WEST:
            self.x -= param
        elif com == Command.FORWARD:
            self.action(self.facing, param)
        elif (com == Command.LEFT and param == 90) or (
            com == Command.RIGHT and param == 270
        ):
            self.facing = self.facing.left()
        elif (com == Command.LEFT or com == Command.RIGHT) and param == 180:
            self.facing = self.facing.left().left()
        elif (com == Command.RIGHT and param == 90) or (
            com == Command.LEFT and param == 270
        ):
            self.facing = self.facing.left().left().left()
        else:
            raise RuntimeError("Bad command for action", com, param)
        #        print(com, param, self.x, self.y, self.facing)
        return self

    def actions(self, commands: Iterable[Tuple[Command, int]]) -> Ship:
        """Run the series of actions.

        >>> Ship().actions(test1).manhattan
        25
        >>> ShipWithWaypoint().actions(test1).manhattan
        286
        """
        for c in commands:
            self.action(c[0], c[1])
        return self

    @property
    def manhattan(self) -> int:
        """Return the manhattan distance from the starting point."""
        return abs(self.x) + abs(self.y)


class ShipWithWaypoint(Ship):
    """Part two ship."""

    def __init__(self) -> None:
        self.x = 0
        self.y = 0
        self.wp_x = 10
        self.wp_y = 1

    def action(self, com: Command, param: int) -> Ship:
        """Update the ship based on the action.

        >>> Ship().action(Command.FORWARD, 10).x
        10
        """
        if com == Command.NORTH:
            self.wp_y += param
        elif com == Command.EAST:
            self.wp_x += param
        elif com == Command.SOUTH:
            self.wp_y -= param
        elif com == Command.WEST:
            self.wp_x -= param
        elif com == Command.FORWARD:
            self.x += self.wp_x * param
            self.y += self.wp_y * param
        elif (com == Command.LEFT and param == 90) or (
            com == Command.RIGHT and param == 270
        ):
            self.wp_x, self.wp_y = -self.wp_y, self.wp_x
        elif (com == Command.LEFT or com == Command.RIGHT) and param == 180:
            self.wp_x, self.wp_y = -self.wp_x, -self.wp_y
        elif (com == Command.RIGHT and param == 90) or (
            com == Command.LEFT and param == 270
        ):
            self.wp_x, self.wp_y = self.wp_y, -self.wp_x
        else:
            raise RuntimeError("Bad command for action", com, param)
        return self


if __name__ == "__main__":
    testmod()
    commands: List[Tuple[Command, int]] = [parse_command(line) for line in stdin]
    print(Ship().actions(commands).manhattan)
    print(ShipWithWaypoint().actions(commands).manhattan)
