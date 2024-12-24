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


def numeric(s: str) -> list[list[str]]:
    """Return directional keypad moves for robot to type given sequence on the numeric keypad.

    Output is a list of alternatives.
    """
    current: Coord = KEYPAD["A"]
    output: list[list[str]] = []
    for target in s:
        moves: list[str] = numeric_moves(current, KEYPAD[target])
        output.append([m + "A" for m in moves])
        current = KEYPAD[target]
    return output


def numeric_moves(start: Coord, end: Coord) -> list[str]:
    """Return directional moves request to move arm for numeric pad. Avoids gap."""
    # If start at 0 and move to left-most column, must go up first
    if start == Coord(1, 3) and end.x == 0:
        return ["^" + m for m in numeric_moves(Coord(1, 2), end)]
    # If start at A and going to left-most column, two ways
    if start == Coord(2, 3) and end.x == 0:
        return ["^" + m for m in numeric_moves(Coord(2, 2), end)] + [
            "<^" + m for m in numeric_moves(Coord(1, 2), end)
        ]
    # If moving to 0 from left-most column, move down last
    if end == Coord(1, 3) and start.x == 0:
        return [m + "v" for m in numeric_moves(start, Coord(1, 2))]
    # If moving to A from left-most column, two ways
    if end == Coord(2, 3) and start.x == 0:
        return [m + "v" for m in numeric_moves(start, Coord(2, 2))] + [
            m + "v>" for m in numeric_moves(start, Coord(1, 2))
        ]
    # Simple case
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


def directional_moves(start: Coord, end: Coord) -> list[str]:
    """Return directional moves request to move arm for directional pad. Avoids gap."""
    # If start on bottom-left, and moving to top, go right first
    # if start == Coord(0, 1) and end.y == 0:
    #     intermediate = Coord(end.x, 1)
    #     return moves(intermediate - start) + moves(end - intermediate)
    # # If moving from top to bottom-left, move down first
    # if end == Coord(0, 1) and start.y == 0:
    #     intermediate = Coord(end.x, 1)
    #     return moves(intermediate - start) + moves(end - intermediate)
    # # Otherwise move directly
    # return moves(end - start)
    return [""]


def moves(move: Coord) -> list[str]:
    """Return directional moves request to move arm for numeric pad."""
    if move == Coord.origin():
        return [""]
    output: list[str] = []
    if move.x > 0:
        output.extend(">" + m for m in moves(move + Coord(-1, 0)))
    if move.x < 0:
        output.extend("<" + m for m in moves(move + Coord(1, 0)))
    if move.y > 0:
        output.extend("v" + m for m in moves(move + Coord(0, -1)))
    if move.y < 0:
        output.extend("^" + m for m in moves(move + Coord(0, 1)))
    return output


if __name__ == "__main__":
    numeric_targets = [line.strip() for line in stdin.readlines()]
    # lengths = [len(directional(directional(numeric(n)))) for n in numeric_targets]
    # numbers = [int(n[:-1]) for n in numeric_targets]
    # print(lengths)
    # print(numbers)
    t = numeric_targets[0]
    d1 = numeric(t)
    print(t)
    print("Got:", numeric(t))
    print("   : <A^A>^^AvvvA")

    # print("Got:", directional(numeric(t)))
    # print("   : v<<A>>^A<A>AvA<^AA>A<vAAA>^A")
    # print("Got:", directional(directional(numeric(t))))
    # print("   : <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A")
