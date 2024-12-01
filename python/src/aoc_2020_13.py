"""Advent of Code 2020 - Day 13."""

from doctest import testmod
from operator import itemgetter
from sys import stdin


def part_one(buses: list[int], start: int) -> int:
    """Solve part one - earliest bus.

    >>> part_one([7, 13, 59, 31, 19], 939)
    295
    """

    def waiting_time(bus: int) -> int:
        return bus - (start % bus)

    earliest = min(buses, key=waiting_time)
    return earliest * waiting_time(earliest)


def chinese_remainder_theorem(requirements: list[tuple[int, int]]) -> int:
    """Solve simultaneous modulo equations.

    Chinese Remainder Theorem (Wikipedia)

    If x = a_i mod n_i where the n_i's are all coprime

    Then there is a unique x (in range 0..N where N=product of n_i's) which can be found by sieving:
       * Order i's with largest first (for speed-up)
       * Consider series: a_1, a_1+n_1, a_1+2n_1
       * Find first value in that series that matches a_2 mod n_2, call this x_2
       * Then consider series: x_2, x_2 + n_1 n_2, x_2 + 2 n_1 n_2,...
    >>> chinese_remainder_theorem([(2, 3), (3, 5), (2, 7)])
    23
    """
    sorted_requirements = sorted(requirements, key=itemgetter(1))
    (x, n_prevs) = sorted_requirements[0]
    for a, n in sorted_requirements[1:]:
        while x % n != a:
            x += n_prevs
        n_prevs *= n
    return x


def part_two(buses: list[int | None]) -> int:
    """Solve part two - find time that meets constraints.

    >>> part_two([7,13,None,None,59,None,31,19])
    1068781
    """
    requirements: list[tuple[int, int]] = [
        ((x - i) % x, x) for (i, x) in enumerate(buses) if x is not None
    ]
    return chinese_remainder_theorem(requirements)


if __name__ == "__main__":
    testmod()
    start_time: int = int(stdin.readline())
    input_buses: list[str] = next(stdin).split(",")
    running_buses: list[int] = [int(x) for x in input_buses if x != "x"]
    print(part_one(running_buses, start_time))
    bus_constraints: list[int | None] = [None if x == "x" else int(x) for x in input_buses]
    print(part_two(bus_constraints))
