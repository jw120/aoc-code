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


#
# Zero-th level - Unconstrained moves.
#
# Generate shortest move sequences on a directional key pads for a relative move
# without considering the gap constraint
#


def moves_unconstrained(move: Coord) -> MoveChoice:
    """Return unconstrained directional moves request to move arm for numeric pad."""
    if move == Coord.origin():
        return [""]
    output: MoveChoice = []
    if move.x > 0:
        output.extend(">" + m for m in moves_unconstrained(move + Coord(-1, 0)))
    if move.x < 0:
        output.extend("<" + m for m in moves_unconstrained(move + Coord(1, 0)))
    if move.y > 0:
        output.extend("v" + m for m in moves_unconstrained(move + Coord(0, -1)))
    if move.y < 0:
        output.extend("^" + m for m in moves_unconstrained(move + Coord(0, 1)))
    return output


#
# First level - Simple shortest sequences.
#
# Generate all shortest sequences for given move, taking into account
# the gap constrain but only considering the current robot.
#


def numeric_simple(start: Coord, end: Coord) -> MoveChoice:
    """Return directional moves request to move arm for numeric pad. Avoids gap."""
    # If start at 0 and move to left-most column, must go up first
    if start == Coord(1, 3) and end.x == 0:
        return ["^" + m for m in numeric_simple(Coord(1, 2), end)]
    # If start at A and going to left-most column, two ways
    if start == Coord(2, 3) and end.x == 0:
        return ["^" + m for m in numeric_simple(Coord(2, 2), end)] + [
            "<^" + m for m in numeric_simple(Coord(1, 2), end)
        ]
    # If moving to 0 from left-most column, move down last
    if end == Coord(1, 3) and start.x == 0:
        return [m[:-1] + "vA" for m in numeric_simple(start, Coord(1, 2))]
    # If moving to A from left-most column, two ways
    if end == Coord(2, 3) and start.x == 0:
        return [m[:-1] + "vA" for m in numeric_simple(start, Coord(2, 2))] + [
            m[:-1] + "v>A" for m in numeric_simple(start, Coord(1, 2))
        ]
    # Simple case
    return [m + "A" for m in moves_unconstrained(end - start)]


def directional_simple(start: Coord, end: Coord) -> MoveChoice:
    """Return directional moves request to move arm for directional pad. Avoids gap."""
    # If start on bottom-left, and moving to top, must go right first
    if start == Coord(0, 1) and end.y == 0:
        return [">" + m for m in directional_simple(start + Coord(1, 0), end)]
    # # If moving from top to bottom-left, last step must be left
    if end == Coord(0, 1) and start.y == 0:
        return [m[:-1] + "<A" for m in directional_simple(start, Coord(1, 1))]
    return [m + "A" for m in moves_unconstrained(end - start)]


#
# Second level - Final shortest sequences
#
# Find shortest sequences considering n additional robots.
#


def shortest(start: str, end: str, n: int, *, numeric: bool) -> str:
    """Return one shortest directional move sequence considering n additional robots."""
    # Generate all candidate routes
    # print("shortest", start, end, n, numeric)
    candidates = (
        numeric_simple(KEYPAD[start], KEYPAD[end])
        if numeric
        else directional_simple(DPAD[start], DPAD[end])
    )
    # print("  candidates", candidates)
    if n == 0:
        # print("  -->", start, end, n, candidates[0])
        return candidates[0]

    # Select one of the shortest candidates
    shortest_candidate_route: str | None = None
    shortest_candidate: str | None = None
    for candidate in candidates:
        candidate_route = ""
        current = "A"
        for key in candidate:
            step_route = shortest(current, key, n - 1, numeric=False)
            candidate_route += step_route
            current = key
        if shortest_candidate_route is None or len(candidate_route) < len(shortest_candidate_route):
            shortest_candidate_route = candidate_route
            shortest_candidate = candidate
    assert shortest_candidate_route is not None
    # print("  candidate", shortest_candidate)
    # print("  route", shortest_candidate_route)
    return shortest_candidate_route


def shortest_sequence(route: str, n: int, *, numeric: bool) -> str:
    """Return shortest directional moves considering n additional robots."""
    final_route: str = ""
    current = "A"
    for key in route:
        final_route += shortest(current, key, n, numeric=numeric)
        current = key
    return final_route


if __name__ == "__main__":
    numeric_targets = [line.strip() for line in stdin.readlines()]

    # Part a
    total = 0
    for target in numeric_targets:
        sequence = shortest_sequence(target, 2, numeric=True)
        complexity = int(target[:-1]) * len(sequence)
        total += complexity
    print(total)
