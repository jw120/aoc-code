"""Advent of Code 2021 - Day 3."""

from collections import Counter
from doctest import testmod
from sys import stdin


def bit_counts(ss: list[str]) -> Counter[int]:
    """Return the number of 1s at each bit position in a list of binary number strings.

    Bit positions are 0-indexed from the start of the string.

    >>> sorted(bit_counts(["101"]).items())
    [(0, 1), (2, 1)]
    >>> sorted(bit_counts(["101", "111"]).items())
    [(0, 2), (1, 1), (2, 2)]
    >>> sorted(bit_counts(test_data).items())
    [(0, 7), (1, 5), (2, 8), (3, 7), (4, 5)]
    """
    return Counter(i for s in ss for i, b in enumerate(s) if b == "1")


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
    assert n / 2 not in counts.values()  # Problem does not account for ties
    return "".join("1" if counts[b] > n // 2 else "0" for b in range(w))


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


def filter_string(ss: list[str], *, least: bool) -> str:
    """Return the remaining string after filtering for the most (or least) common bit.

    >>> filter_string(test_data, least=False)
    '10111'
    >>> filter_string(test_data, least=True)
    '01010'
    """
    b = 0
    while len(ss) > 1:
        counts: Counter[str] = Counter(s[b] for s in ss)
        selected_value: str = "1" if (counts["1"] >= len(ss) / 2) ^ least else "0"
        ss = [s for s in ss if s[b] == selected_value]
        b += 1
    assert len(ss) == 1
    return ss[0]


def part2(numbers: list[str]) -> int:
    """Part two solution.

    >>> part2(test_data)
    230
    """
    oxygen = int(filter_string(numbers, least=False), 2)
    co2 = int(filter_string(numbers, least=True), 2)
    return oxygen * co2


if __name__ == "__main__":
    testmod()
    input_numbers: list[str] = stdin.read().splitlines()
    print(part1(input_numbers))
    print(part2(input_numbers))
