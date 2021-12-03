"""Advent of Code 2021 - Day 2."""

from doctest import testmod
from functools import reduce
from sys import stdin
from typing import Tuple


def change(s: str) -> Tuple[int, int]:
    """Return distance and depth changes for a given command."""
    ws = s.strip().split()
    if len(ws) != 2 or not ws[1].isnumeric():
        raise ValueError("Invalid command format: " + s)
    if ws[0] == "forward":
        return (int(ws[1]), 0)
    if ws[0] == "down":
        return (0, int(ws[1]))
    if ws[0] == "up":
        return (0, -int(ws[1]))
    raise ValueError("Unknown command:", s)


def run1(cs: list[str]) -> int:
    """Return product of distance and depth after running given simple commands.

    >>> run1(["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"])
    150
    """
    horiz, depth = reduce(
        lambda acc, x: (acc[0] + x[0], acc[1] + x[1]), [change(c) for c in cs]
    )
    return horiz * depth


def apply2(
    current: Tuple[int, int, int], command: Tuple[int, int]
) -> Tuple[int, int, int]:
    """Apply the distance and depth changes of a complex command."""
    horiz, depth, aim = current
    return (horiz + command[0], depth + command[0] * aim, aim + command[1])


def run2(cs: list[str]) -> int:
    """Return product of distance and depth after running given complex commands.

    >>> run2(["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"])
    900
    """
    horiz, depth, _aim = reduce(apply2, [change(c) for c in cs], (0, 0, 0))
    return horiz * depth


if __name__ == "__main__":
    testmod()
    commands: list[str] = stdin.read().splitlines()
    print(run1(commands))
    print(run2(commands))
