"""Advent of Code 2024 - Day 22."""

from collections import Counter
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


#
# Zero-th level - Unconstrained moves.
#
# Generate shortest move sequences on a directional key pads for a relative move
# without considering the gap constraint
#


def moves_unconstrained(move: Coord) -> list[str]:
    """Return unconstrained directional moves request to move arm for numeric pad."""
    if move == Coord.origin():
        return [""]
    output: list[str] = []
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


def numeric_simple(start: Coord, end: Coord) -> list[str]:
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


def directional_simple(start: Coord, end: Coord) -> list[str]:
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

cache: dict[tuple[str, str, int], str] = {}
cache_lanternfish: dict[tuple[str, int], Counter[str]] = {}


def shortest(start: str, end: str, n: int, *, numeric: bool) -> str:
    """Return one shortest directional move sequence considering n additional robots."""
    # if n == 10:
    #     print(start, end, cache.get((start, end, n)))  # , n, numeric)a

    # Check cache
    if (start, end, n) in cache:
        # print("  Cached", cache[start, end, n])
        return cache[start, end, n]

    # Generate all candidate routes
    candidates = (
        numeric_simple(KEYPAD[start], KEYPAD[end])
        if numeric
        else directional_simple(DPAD[start], DPAD[end])
    )
    # print("  candidates", candidates)
    if n == 0:
        # print("  -->", start, end, n, candidates[0])
        cache[start, end, n] = candidates[0]
        return candidates[0]

    # Select one of the shortest candidates
    shortest_candidate_route: str | None = None
    # shortest_candidate: str | None = None
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
    # print("route", shortest_candidate_route)
    cache[start, end, n] = shortest_candidate_route
    return shortest_candidate_route


def shortest_lanternfish(step: str, n: int, *, numeric: bool) -> Counter[str]:
    """Return one shortest directional move sequence considering n additional robots."""

    if (step, n) in cache_lanternfish:
        return cache_lanternfish[step, n]

    # Generate all candidate routes
    candidates = [
        to_lanternfish(s)
        for s in (
            numeric_simple(KEYPAD[step[0]], KEYPAD[step[1]])
            if numeric
            else directional_simple(DPAD[step[0]], DPAD[step[1]])
        )
    ]
    if n == 0:
        cache_lanternfish[step, n] = candidates[0]
        return candidates[0]

    # Select one of the shortest candidates
    shortest_candidate: Counter[str] | None = None
    for candidate in candidates:
        candidate_route: Counter[str] = Counter()
        for candidate_step, candidate_number in candidate.items():
            step_route = shortest_lanternfish(candidate_step, n - 1, numeric=False)
            for step_step, step_number in step_route.items():
                candidate[step_step] += step_number * candidate_number
        if shortest_candidate is None or candidate_route.total() < shortest_candidate.total():
            shortest_candidate = candidate_route
    assert shortest_candidate is not None
    cache_lanternfish[step, n] = shortest_candidate
    return shortest_candidate


def to_lanternfish(route: str) -> Counter[str]:
    """Convert to lanternfish representation."""
    counter: Counter[str] = Counter()
    r = "A" + route
    for i in range(len(r) - 1):
        counter[route[i : i + 2]] += 1
    return counter


def shortest_sequence(route: str, n: int, *, numeric: bool) -> str:
    """Return shortest directional moves considering n additional robots."""
    final_route: str = ""
    current = "A"
    for key in route:
        final_route += shortest(current, key, n, numeric=numeric)
        current = key
    return final_route


def shortest_sequence_lanternfish(route: str, n: int) -> Counter[str]:
    """Return shortest directional moves considering n additional robots."""
    final_route: Counter[str] = Counter()
    current = "A"
    for key in route:
        final_route.update(shortest(current, key, n, numeric=True))
        current = key
    return final_route


if __name__ == "__main__":
    numeric_targets = [line.strip() for line in stdin.readlines()]

    # Part a
    total = 0
    for target in numeric_targets:
        sequence = shortest_sequence(target, 2, numeric=True)
        complexity = int(target[:-1]) * len(sequence)
        print(target, int(target[:-1]), len(sequence), complexity)
        total += complexity
    print(total)

    total = 0
    for target in numeric_targets:
        sequence_lanternfish = shortest_sequence_lanternfish(target, 2)
        complexity = int(target[:-1]) * sequence_lanternfish.total()
        print(target, int(target[:-1]), sequence_lanternfish.total(), complexity)
        total += complexity
    print(total)

    # # Part b
    z = 25
    # total = 0
    # for target in numeric_targets:
    #     sequence = shortest_sequence(target, z, numeric=True)
    #     complexity = int(target[:-1]) * len(sequence)
    #     print(target, int(target[:-1]), len(sequence), complexity)
    #     total += complexity
    # print(total)

    total = 0
    for target in numeric_targets:
        sequence_lanternfish = shortest_sequence_lanternfish(target, z)
        complexity = int(target[:-1]) * sequence_lanternfish.total()
        print(target, int(target[:-1]), sequence_lanternfish.total(), complexity)
        total += complexity
    print(total)
