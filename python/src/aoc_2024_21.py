"""Advent of Code 2024 - Day 22."""

from sys import stdin
from typing import Final

from coord import Coord

KEYPAD: Final[dict[str, Coord]] = {
    "7": Coord(0, 0),
    "8": Coord(1, 0),
    "9": Coord(2, 0),
    "4": Coord(0, 1),
    "5": Coord(1, 1),
    "6": Coord(2, 1),
    "1": Coord(0, 2),
    "2": Coord(1, 2),
    "3": Coord(2, 2),
    "0": Coord(1, 3),
    "A": Coord(2, 3),
}

DPAD: Final[dict[str, Coord]] = {
    "^": Coord(1, 0),
    "A": Coord(2, 0),
    "<": Coord(0, 1),
    "v": Coord(1, 1),
    ">": Coord(2, 1),
}


def numeric(s: str) -> str:
    """Return directional keypad moves for robot to type given sequence on the numeric keypad."""
    current: Coord = KEYPAD["A"]
    output = ""
    for target in s:
        moves = numeric_moves(current, KEYPAD[target])
        output += moves
        output += "A"
        current = KEYPAD[target]
    return output


def numeric_moves(start: Coord, end: Coord) -> str:
    """Return directional moves request to move arm for numeric pad. Avoids gap."""
    # If start on bottom row, move up first
    if start.y == 3 and end.y < 3:
        intermediate = Coord(start.x, 2)
        return moves(intermediate - start) + moves(end - intermediate)
    # If moving to bottom row, move down last
    if end.y == 3 and start.y < 3:
        intermediate = Coord(end.x, 2)
        return moves(intermediate - start) + moves(end - intermediate)
    # Otherwise move directly
    return moves(end - start)


def directional(s: str) -> str:
    """Return directional keypad moves for robot to type given sequence on another directional keypad."""
    current: Coord = DPAD["A"]
    output = ""
    for target in s:
        moves = directional_moves(current, DPAD[target])
        output += moves
        output += "A"
        current = DPAD[target]
    return output


def directional_moves(start: Coord, end: Coord) -> str:
    """Return directional moves request to move arm for directional pad. Avoids gap."""
    # If start on bottom-left, and moving to top, go right first
    if start == Coord(0, 1) and end.y == 0:
        intermediate = Coord(end.x, 1)
        return moves(intermediate - start) + moves(end - intermediate)
    # If moving from top to bottom-left, move down first
    if end == Coord(0, 1) and start.y == 0:
        intermediate = Coord(end.x, 1)
        return moves(intermediate - start) + moves(end - intermediate)
    # Otherwise move directly
    return moves(end - start)


def moves(move: Coord) -> str:
    """Return directional moves request to move arm for numeric pad."""
    output = ""
    while move != Coord.origin():
        if move.x > 0:
            output += ">"
            move += Coord(-1, 0)
        if move.x < 0:
            output += "<"
            move += Coord(1, 0)
        if move.y > 0:
            output += "v"
            move += Coord(0, -1)
        if move.y < 0:
            output += "^"
            move += Coord(0, 1)
    return output


if __name__ == "__main__":
    numeric_targets = [line.strip() for line in stdin.readlines()]
    lengths = [len(directional(directional(numeric(n)))) for n in numeric_targets]
    numbers = [int(n[:-1]) for n in numeric_targets]
    print(lengths)
    print(numbers)
    t = numeric_targets[0]
    print(t)
    print("Got:", numeric(t))
    print("   : <A^A>^^AvvvA")
    print("Got:", directional(numeric(t)))
    print("   : v<<A>>^A<A>AvA<^AA>A<vAAA>^A")
    print("Got:", directional(directional(numeric(t))))
    print("   : <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A")
