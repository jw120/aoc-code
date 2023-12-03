"""Advent of Code 2022 - Day 6."""

from doctest import testmod
from sys import stdin


def packet_start(s: str) -> int:
    """Return number of characters processed before first start-of-packet marker.

    Marker is four consecutive characters that are unique.

    >>> packet_start("bvwbjplbgvbhsrlpgdmjqwftvncz")
    5
    """
    for n, cs in enumerate(zip(s, s[1:], s[2:], s[3:], strict=False), start=4):
        if len(set(cs)) == 4:
            return n
    raise ValueError("Failed to find start-of-packet marker")


def message_start(s: str) -> int:
    """Return number of characters processed before first start-of-message marker.

    Marker is fourteen consecutive characters that are unique.

    >>> message_start("mjqjpqmgbljsphdztnvjfqwrcgsmlb")
    19
    """
    tuples = zip(
        s,
        s[1:],
        s[2:],
        s[3:],
        s[4:],
        s[5:],
        s[6:],
        s[7:],
        s[8:],
        s[9:],
        s[10:],
        s[11:],
        s[12:],
        s[13:],
        strict=False,
    )
    for n, cs in enumerate(tuples, start=14):
        if len(set(cs)) == 14:
            return n
    raise ValueError("Failed to find start-of-message marker")


if __name__ == "__main__":
    testmod()
    message = stdin.read()
    print(packet_start(message))
    print(message_start(message))
