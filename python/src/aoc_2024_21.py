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

# We represent moves of the directional keypad by strings
type Moves = str

# When there are more than one alternative moves we return a list of moves
type MoveChoice = list[str]

# We generate move chains which are a series of move sequences (with alternatives)
type MoveChain = list[MoveChoice]


def numeric(s: str) -> MoveChain:
    """Return directional keypad moves for robot to type given sequence on the numeric keypad.

    Output is a list of alternatives.
    """
    current: Coord = KEYPAD["A"]
    output: MoveChain = []
    previous_moves: MoveChoice = []
    for target in s:
        moves: MoveChoice = numeric_moves(current, KEYPAD[target])
        print(target, "->", moves)
        if len(moves) == 1 and len(previous_moves) == 1:
            output[-1] = [output[-1][0] + moves[0] + "A"]
        else:
            output.append([m + "A" for m in moves])
        current = KEYPAD[target]
        previous_moves = moves
    return output


def numeric_moves(start: Coord, end: Coord) -> MoveChoice:
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


def directional(target_moves: Moves) -> MoveChain:
    """Return directional keypad moves for robot to type given sequence on another directional keypad."""
    current: Coord = DPAD["A"]
    output: MoveChain = []
    previous_moves: MoveChoice = []
    for target in target_moves:
        moves: MoveChoice = directional_moves(current, DPAD[target])
        if len(moves) == 1 and len(previous_moves) == 1:
            output[-1] = [output[-1][0] + moves[0] + "A"]
        else:
            output.append([m + "A" for m in moves])
        current = DPAD[target]
        previous_moves = moves
    return output


def directional_moves(start: Coord, end: Coord) -> MoveChoice:
    """Return directional moves request to move arm for directional pad. Avoids gap."""
    # If start on bottom-left, and moving to top, must go right first
    if start == Coord(0, 1) and end.y == 0:
        return [">" + m for m in directional_moves(start + Coord(1, 0), end)]
    # # If moving from top to bottom-left, last step must be left
    if end == Coord(0, 1) and start.y == 0:
        return [m + "<" for m in directional_moves(start, Coord(1, 1))]
    return moves(end - start)


def moves(move: Coord) -> MoveChoice:
    """Return directional moves request to move arm for numeric pad."""
    if move == Coord.origin():
        return [""]
    output: MoveChoice = []
    if move.x > 0:
        output.extend(">" + m for m in moves(move + Coord(-1, 0)))
    if move.x < 0:
        output.extend("<" + m for m in moves(move + Coord(1, 0)))
    if move.y > 0:
        output.extend("v" + m for m in moves(move + Coord(0, -1)))
    if move.y < 0:
        output.extend("^" + m for m in moves(move + Coord(0, 1)))
    return output


def apply_directional(move_chain: MoveChain) -> MoveChain:
    """Apply directional moves with all possible minimum-length move sequences."""
    output: MoveChain = []
    for move_choice in move_chain:
        if len(move_choice) > 1:
            print("Considering", move_choice)
        shortest_length: int | None = None
        shortest_chain: MoveChain = []
        for choice in move_choice:
            choice_chain: MoveChain = directional(choice)
            choice_length: int = chain_length(choice_chain)
            if len(move_choice) > 1:
                print("  Trying", choice, choice_chain, choice_length)
            if shortest_length is None or choice_length < shortest_length:
                shortest_length = choice_length
                shortest_chain = choice_chain
        if len(move_choice) > 1:
            print("  Shortest", shortest_chain)
        output.extend(shortest_chain)
    return output


def chain_length(chain: MoveChain) -> int:
    """Minimum length of a move chain."""
    total = 0
    for choice in chain:
        total += min(len(s) for s in choice)
    return total


if __name__ == "__main__":
    numeric_targets = [line.strip() for line in stdin.readlines()]
    # # Part (a)
    # total = 0
    # for target in numeric_targets:
    #     move_chain = apply_directional(apply_directional(numeric(target)))
    #     complexity = int(target[:-1]) * chain_length(move_chain)
    #     print(target, int(target[:-1]), chain_length(move_chain), complexity)
    #     total += complexity
    # print(total)

    t = numeric_targets[3]
    d1 = numeric(t)
    print(t)
    print(chain_length(d1))
    print("Got:", d1)
    # print("   : <A^A>^^AvvvA", len("<A^A>^^AvvvA"))
    d2 = apply_directional(d1)
    print(chain_length(d2))
    print("Got:", d2)
    # print("   : v<<A>>^A<A>AvA<^AA>A<vAAA>^A", len("v<<A>>^A<A>AvA<^AA>A<vAAA>^A"))
    d3 = apply_directional(d2)
    print(chain_length(d3))
    print("Got:", d3)
    # print(
    #     "   : <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A",
    #     len("<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"),
    # )
