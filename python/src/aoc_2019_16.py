"""Advent of Code 2019 - Day 16."""

from __future__ import annotations

from doctest import testmod
from itertools import islice
from sys import stdin
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from collections.abc import Iterable


def combine(xs: Iterable[int], ys: Iterable[int]) -> int:
    """Combine two sequences.

    >>> combine([1, 2, 3], [4, 5]) == (1 * 4 + 2 * 5) % 10
    True
    """
    total: int = 0
    for x, y in zip(xs, ys, strict=False):
        total += x * y
    return abs(total) % 10


def fft(xs: list[int], reps: int) -> list[int]:
    """Perform an FFT the given number of times.

    >>> fft([1,2,3,4,5,6,7,8], 1)
    [4, 8, 2, 2, 6, 1, 5, 8]
    >>> fft([1,2,3,4,5,6,7,8], 4)
    [0, 1, 0, 2, 9, 4, 9, 8]
    """
    n: int = len(xs)
    patterns: list[list[int]] = [pattern(i, n) for i in range(1, n + 1)]
    for _ in range(reps):
        xs = [combine(xs, p) for p in patterns]
    return xs


def pattern(i: int, n: int) -> list[int]:
    """Generate a list of length n with a i-pattern of 0s, 1s and -1s for use in FFT.

    >>> pattern(1, 10)
    [1, 0, -1, 0, 1, 0, -1, 0, 1, 0]
    >>> pattern(2, 10)
    [0, 1, 1, 0, 0, -1, -1, 0, 0, 1]
    >>> pattern(3, 10)
    [0, 0, 1, 1, 1, 0, 0, 0, -1, -1]
    """

    def pattern_gen(seq_len: int) -> Iterable[int]:
        seq_index: int = 0  # Which sequence: 0..3 for 0/1/0/-1
        pos_index: int = 1  # Position in the sequence
        if seq_len == 1:  # Special case for n = 1, start at beginning of 2nd sequence
            seq_index = 1
            pos_index = 0
        while True:
            yield [0, 1, 0, -1][seq_index]
            pos_index += 1
            if pos_index >= seq_len:
                pos_index = 0
                seq_index = (seq_index + 1) % 4

    return list(islice(pattern_gen(i), n))


def digits_to_ints(s: str) -> list[int]:
    """Convert a string of digits to a list of ints.

    >>> digits_to_ints("1234")
    [1, 2, 3, 4]
    """
    return [int(x) for x in s]


def ints_to_digits(xs: list[int]) -> str:
    """Convert a list of ints to a string.

    >>> ints_to_digits([1, 2, 3, 4, 5])
    '12345'
    """
    return "".join(str(x) for x in xs)


if __name__ == "__main__":
    testmod()
    signal: list[int] = digits_to_ints(stdin.read().strip())
    print(ints_to_digits(fft(signal, 100))[:8])
