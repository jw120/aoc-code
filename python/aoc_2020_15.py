"""Advent of Code 2020 - Day 15."""

from collections.abc import Iterator
from doctest import testmod
from itertools import islice
from sys import stdin


def run1(nums: list[int]) -> Iterator[int]:
    """Generate part one sequence.

    >>> list(islice(run1([0, 3, 6]), 10))
    [0, 3, 6, 0, 3, 3, 1, 0, 4, 0]
    """
    turn = 1
    last_spoken: dict[int, int] = {}
    prev_spoken: dict[int, int] = {}
    for n in nums:
        last_spoken[n] = turn
        yield n
        last_num = n
        turn += 1
    while True:
        if last_num in prev_spoken:
            output = last_spoken[last_num] - prev_spoken[last_num]
        else:
            output = 0
        if output in last_spoken:
            prev_spoken[output] = last_spoken[output]
        last_spoken[output] = turn
        yield output
        last_num = output
        turn += 1


if __name__ == "__main__":
    testmod()
    numbers: list[int] = [int(x) for x in stdin.readline().split(",")]
    print(list(islice(run1(numbers), 2019, 2020))[0])
    print(list(islice(run1(numbers), 30000000 - 1, 30000000))[0])
