"""Advent of Code 2022 - Day 3."""

from doctest import testmod
from sys import stdin


def duplicate_half(s: str) -> str:
    """Return the character appearing in both first and second half of the string.

    >>> duplicate_half("abcdbf")
    'b'
    """
    n = len(s)
    assert n % 2 == 0, f"Odd input '{s}'"
    overlap = set(s[: n // 2]) & set(s[n // 2 :])
    assert len(overlap) == 1
    [x] = overlap
    return x


def duplicate_three(s: str, t: str, u: str) -> str:
    """Return the character appearing in all three strings.

    >>> duplicate_three("abqc", "pqrZ", "wwbq")
    'q'
    """
    overlap = set(s) & set(t) & set(u)
    assert len(overlap) == 1
    [x] = overlap
    return x


def priority(s: str) -> int:
    """Priority of a letter.

    >>> [priority(c) for c in "abzAY"]
    [1, 2, 26, 27, 51]
    """
    assert len(s) == 1
    return (ord(s.lower()) - ord("a") + 1) + 26 * s.isupper()


if __name__ == "__main__":
    testmod()
    lines = [line.strip() for line in stdin]
    assert len(lines) % 3 == 0
    print(sum(priority(duplicate_half(line)) for line in lines))
    print(
        sum(
            priority(duplicate_three(lines[i], lines[i + 1], lines[i + 2]))
            for i in range(0, len(lines), 3)
        )
    )
