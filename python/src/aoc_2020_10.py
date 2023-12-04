"""Advent of Code 2020 - Day 10."""

from doctest import testmod
from sys import stdin

test1: list[int] = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]

test2: list[int] = [
    28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3,
]


def prep_list(xs: list[int]) -> list[int]:
    """Sort list and add first and last elements."""
    ys: list[int] = [0, *sorted(xs)]
    ys.append(ys[-1] + 3)
    return ys


def count_gaps(xs: list[int]) -> tuple[int, int]:
    """Count the numbers of 1-gaps and 3-gaps in a list of adapters.

    >>> count_gaps(prep_list(test1))
    (7, 5)
    >>> count_gaps(prep_list(test2))
    (22, 10)
    """
    x_prev = xs[0]
    count1 = 0
    count3 = 0
    for x in xs[1:]:
        diff = x - x_prev
        if diff == 1:
            count1 += 1
        elif diff == 3:
            count3 += 1
        else:
            raise RuntimeError("Bad diff", diff)
        x_prev = x
    return (count1, count3)


def count_ways(xs: list[int]) -> int:
    """Count the number of ways the adapters can be connected.

    >>> count_ways(prep_list(test1))
    8
    >>> count_ways(prep_list(test2))
    19208
    """
    counts: list[int] = [0 for _ in xs]

    last_i = len(xs) - 1
    counts[last_i] = 1

    for i in range(last_i - 1, -1, -1):
        if i + 1 <= last_i and xs[i + 1] <= xs[i] + 3:
            counts[i] += counts[i + 1]
        if i + 2 <= last_i and xs[i + 2] <= xs[i] + 3:
            counts[i] += counts[i + 2]
        if i + 3 <= last_i and xs[i + 3] <= xs[i] + 3:
            counts[i] += counts[i + 3]
    return counts[0]


if __name__ == "__main__":
    testmod()
    adapters: list[int] = prep_list([int(line) for line in stdin])
    (num1, num3) = count_gaps(adapters)
    print(num1 * num3)
    print(count_ways(adapters))
