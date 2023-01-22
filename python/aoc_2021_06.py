"""Advent of Code 2021 - Day 6."""

from doctest import testmod
from sys import stdin

test_data: list[int] = [3, 4, 3, 1, 2]


def fish(initial_timers: list[int], number_of_steps: int) -> int:
    """Return number of fish after given number of steps from initial position.

    >>> [fish(test_data, i) for i in [1, 2, 18, 256]]
    [5, 6, 26, 26984457539]
    """
    f: dict[int, int] = {}  # number of fish after n steps from one timer-0 fish
    for i in range(-8, number_of_steps):
        if i <= 0:
            f[i] = 1
        else:
            f[i] = f[i - 1 - 8] + f[i - 1 - 6]
    return sum(f[number_of_steps - x] for x in initial_timers)


if __name__ == "__main__":
    testmod()
    initial: list[int] = [int(x) for x in stdin.read().split(",")]
    print(fish(initial, 80))
    print(fish(initial, 256))
