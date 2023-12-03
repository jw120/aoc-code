"""Advent of Code 2020 - Day 12."""

from __future__ import annotations

from collections.abc import Iterable
from doctest import testmod
from sys import stdin


def rotate(s: str, rotation: int) -> str:
    """Rotate a direction the number of degrees to the right.

    >>> rotate("N", 90)
    'E'
    """
    i = "NESW".find(s)
    if i == -1:
        raise RuntimeError("Bad direction")
    return "NESW"[(i + (rotation // 90)) % 4]


def parse_command(s: str) -> tuple[str, int]:
    """Read a command from string."""
    return (s[0], int(s[1:]))


test1 = [parse_command(s) for s in ["F10", "N3", "F7", "R90", "F11"]]


class Ship:
    """Basic ship for part one."""

    def __init__(self) -> None:
        self.x = 0
        self.y = 0
        self.facing = "E"

    def action(self, com: str, param: int) -> Ship:
        """Update the ship based on the action.

        >>> Ship().action("F", 10).x
        10
        """
        if com == "N":
            self.y += param
        elif com == "E":
            self.x += param
        elif com == "S":
            self.y -= param
        elif com == "W":
            self.x -= param
        elif com == "F":
            self.action(self.facing, param)
        elif com == "L":
            self.facing = rotate(self.facing, -param)
        elif com == "R":
            self.facing = rotate(self.facing, param)
        else:
            raise RuntimeError("Bad command for action", com, param)
        #        print(com, param, self.x, self.y, self.facing)
        return self

    def actions(self, commands: Iterable[tuple[str, int]]) -> Ship:
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
        super().__init__()
        self.wp_x = 10
        self.wp_y = 1

    def action(self, com: str, param: int) -> Ship:
        """Update the ship based on the action.

        >>> ShipWithWaypoint().action("F", 10).x
        100
        """
        if com == "N":
            self.wp_y += param
        elif com == "E":
            self.wp_x += param
        elif com == "S":
            self.wp_y -= param
        elif com == "W":
            self.wp_x -= param
        elif com == "F":
            self.x += self.wp_x * param
            self.y += self.wp_y * param
        elif (com, param) in {("L", 90), ("R", 270)}:
            self.wp_x, self.wp_y = -self.wp_y, self.wp_x
        elif (com, param) in {("L", 180), ("R", 180)}:
            self.wp_x, self.wp_y = -self.wp_x, -self.wp_y
        elif (com, param) in {("L", 270), ("R", 90)}:
            self.wp_x, self.wp_y = self.wp_y, -self.wp_x
        else:
            raise RuntimeError("Bad command for action", com, param)
        return self


if __name__ == "__main__":
    testmod()
    input_commands: list[tuple[str, int]] = [(line[0], int(line[1:])) for line in stdin]
    print(Ship().actions(input_commands).manhattan)
    print(ShipWithWaypoint().actions(input_commands).manhattan)
