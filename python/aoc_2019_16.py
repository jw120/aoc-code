"""Advent of Code 2019 - Day 16."""

from __future__ import annotations

from doctest import testmod
from itertools import islice
from typing import Iterable, List


def combine(xs: Iterable[int], ys: Iterable[int]) -> int:
    """Combine two sequences.

    >>> combine([1, 2, 3], [4, 5]) == (1 * 4 + 2 * 5) % 10
    True
    """
    total: int = 0
    for (x, y) in zip(xs, ys):
        total += x * y
    return abs(total) % 10


def fft(xs: List[int]) -> List[int]:
    """Performance an FFT

    >>> fft([1,2,3,4,5,6,7,8])
    [4, 8, 2, 2, 6, 1, 5, 8]
    """
    output: List[int] = []
    for i in range(1, len(xs) + 1):
        output.append(combine(xs, pattern(i)))
    return output


def pattern(n: int) -> Iterable[int]:
    """Generate a pattern of 0s, 1s and -1s for use in FFT.

    >>> list(islice(pattern(1), 10))
    [1, 0, -1, 0, 1, 0, -1, 0, 1, 0]
    >>> list(islice(pattern(2), 10))
    [0, 1, 1, 0, 0, -1, -1, 0, 0, 1]
    >>> list(islice(pattern(3), 10))
    [0, 0, 1, 1, 1, 0, 0, 0, -1, -1]
    """
    seq_index: int = 0  # Which sequence: 0..3 for 0/1/0/-1
    pos_index: int = 1  # Position in the sequence
    if n == 1:  # Special case for n = 1, start at beginning of 2nd sequence
        seq_index = 1
        pos_index = 0
    while True:
        yield [0, 1, 0, -1][seq_index]
        pos_index += 1
        if pos_index >= n:
            pos_index = 0
            seq_index = (seq_index + 1) % 4


if __name__ == "__main__":
    testmod()
