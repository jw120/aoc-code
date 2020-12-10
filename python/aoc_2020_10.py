"""Advent of Code 2020 - Day 10."""

from doctest import testmod
from sys import stdin
from typing import Dict, List, Tuple

test1: List[int] = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]

test2: List[int] = [
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


def prep_list(xs: List[int]) -> List[int]:
    ys: List[int] = [0] + sorted(xs)
    ys.append(ys[-1] + 3)
    return ys


def count_gaps(xs: List[int]) -> Tuple[int, int]:
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


def count_ways(xs: List[int], i: int = 0) -> int:
    """Count the number of ways the adapters can be connected.

    >>> count_ways(prep_list(test1))
    8
    >>> count_ways(prep_list(test2))
    19208
    """

    memo: Dict[int, int] = {}

    def go(i: int) -> int:
        if i in memo:
            return memo[i]

        if i == len(xs) - 1:
            return 1
        possible_next = [x for x in xs[i + 1 : i + 4] if x <= xs[i] + 3]
        if len(possible_next) == 0:
            return 0
        if len(possible_next) == 1:  # x0 -> xs[1] -> xs[2:]
            return go(i + 1)
        if len(possible_next) == 2:  # x0 -> xs[1]/xs[2] -> xs[3:]
            return go(i + 1) + go(i + 2)
        if len(possible_next) == 3:  # x0 -> xs[1]/xs[2]/xs[3] -> xs[4:]
            return go(i + 1) + go(i + 2) + go(i + 3)
        raise RuntimeError("Bad number of possible_next", possible_next, xs[:10])


if __name__ == "__main__":
    testmod()
    adapters: List[int] = prep_list([int(line) for line in stdin])
    (num1, num3) = count_gaps(adapters)
    print(num1 * num3)
    print(count_ways(adapters))
