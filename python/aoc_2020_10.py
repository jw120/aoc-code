"""Advent of Code 2020 - Day 10."""

from doctest import testmod
from sys import stdin
from typing import List, Tuple

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


def count_gaps(xs: List[int]) -> Tuple[int, int]:
    """Count the numbers of 1-gaps and 3-gaps in a list of adapters.

    >>> count_gaps(test1)
    (7, 5)
    >>> count_gaps(test2)
    (22, 10)
    """
    xs = sorted(xs)
    xs.append(xs[-1] + 3)
    x_prev = 0
    count1 = 0
    count3 = 0
    for x in xs:
        diff = x - x_prev
        if diff == 1:
            count1 += 1
        elif diff == 3:
            count3 += 1
        else:
            raise RuntimeError("Bad diff", diff)
        x_prev = x
    return (count1, count3)


if __name__ == "__main__":
    testmod()
    adapters: List[int] = [int(line) for line in stdin]
    (num1, num3) = count_gaps(adapters)
    print(num1 * num3)
