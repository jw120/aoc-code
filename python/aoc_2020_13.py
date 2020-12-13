"""Advent of Code 2020 - Day 13."""

from doctest import testmod
from sys import stdin
from typing import List, Optional


def part_one(buses: List[int], start: int) -> int:
    """Solve part one - earliest bus.

    >>> part_one([7, 13, 59, 31, 19], 939)
    295
    """

    def waiting_time(bus: int) -> int:
        return bus - (start % bus)

    earliest = min(buses, key=waiting_time)
    return earliest * waiting_time(earliest)


def part_two(buses: List[Optional[int]]) -> int:
    """Solve part two - find time that meets constraints.

    >>> part_two([7,13,None,None,59,None,31,19])
    1068781
    """
    print(buses)
    return 0


if __name__ == "__main__":
    testmod()
    start_time: int = int(stdin.readline())
    buses_str: List[str] = next(stdin).split(",")
    running_buses: List[int] = [int(x) for x in buses_str if x != "x"]
    print(part_one(running_buses, start_time))
    bus_constraints: List[Optional[int]] = [
        None if x == "x" else int(x) for x in buses_str
    ]
    print(part_two(bus_constraints))