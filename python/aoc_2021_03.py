"""Advent of Code 2021 - Day 3."""

from collections import Counter
from doctest import testmod
from sys import stdin


def bit_counts(ss: list[str]) -> Counter[int]:
    """Return the number of 1s at each bit position in a list of binary number strings

    >>> bit_counts(["101"])
    Counter({0: 1, 2: 1})
    >>> bit_counts(["101", "111"])
    Counter({0: 2, 2: 2, 1: 1})
    >>> bit_counts(test_data)
    Counter({0: 5, 1: 7, 2: 8, 3: 5, 4: 7   })
    """
    return Counter(b for s in ss for b in range(0, len(s)) if s[-b - 1] == "1")


test_data: list[str] = [
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010",
]


def most_common_bits(ss: list[str]) -> str:
    """Return a binary number string with the most common bit values.

    >>> most_common_bits(test_data)
    '10110'
    """
    w = len(ss[0])
    assert all(len(s) == w for s in ss)
    n = len(ss)

    counts = bit_counts(ss)
    assert n / 2 not in counts.values()
    return "".join("1" if counts[b] > n // 2 else "0" for b in range(w - 1, -1, -1))


def not_bits(s: str) -> str:
    """Return binary string with all bits negated.

    >>> not_bits("11010")
    '00101'
    """
    return "".join("1" if c == "0" else "0" for c in s)


def part1(numbers: list[str]) -> int:
    """Part one solution.

    >>> part1(test_data)
    198
    """
    mcb = most_common_bits(numbers)
    gamma = int(mcb, 2)
    epsilon = int(not_bits(mcb), 2)
    return gamma * epsilon


if __name__ == "__main__":
    testmod()
    numbers: list[str] = stdin.read().splitlines()
    print(part1(numbers))
