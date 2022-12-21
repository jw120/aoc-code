"""Advent of Code 2022 - Day 13."""

from doctest import testmod
from functools import cmp_to_key
from re import match
from sys import stdin
from typing import Literal


def leading_int(s: str) -> tuple[int, int]:
    """Read a leading integer returning it and the number of digits.

    >>> leading_int("99,23]")
    (99, 2)
    """
    m = match(r"^\d+", s)
    assert m, f"Digit match failed '{s}'"
    return int(m.group(0)), len(m.group(0))


def promote_to_list(s: str, n: int) -> str:
    """Replace an integer in the string at position x with a list.

    >>> promote_to_list("[1,2,3]", 3)
    '[1,[2],3]'
    """
    num, num_len = leading_int(s[n:])
    return s[:n] + "[" + str(num) + "]" + s[n + num_len :]


def compare(x: str, y: str, open_lists: int = 0) -> Literal[-1, 0, 1]:
    """Compare the two lists returning -1, 0 or 1."""
    i = 0
    j = 0
    while True:
        match x[i], y[i]:
            case "[", "[":
                i += 1
                j += 1
                open_lists += 1
            case "[", q if q.isdigit():
                y = promote_to_list(y, j)
            case p, "[" if p.isdigit():
                x = promote_to_list(x, i)
            case p, q if p.isdigit() and q.isdigit():
                x_int, x_len = leading_int(x[i:])
                y_int, y_len = leading_int(y[i:])
                if x_int == y_int:
                    i += x_len
                    j += y_len
                else:
                    return -1 if x_int < y_int else 1
            case ",", ",":
                i += 1
                j += 1
            case "]", "]":
                open_lists -= 1
                assert open_lists >= 0
                i += 1
                j += 1
            case "]", _:
                return -1
            case _, "]":
                return 1
            case _:
                raise ValueError(f"Unexpected match {x[i]}, {y[i]} for\n{x}\n{y}")


def sum_correct_indices(s: str) -> int:
    """Return sum of indices for pairs which are in correct order.

    >>> sum_correct_indices(TEST_DATA)
    13
    """
    pairs = [pair.split("\n") for pair in s.split("\n\n")]
    return sum(i for i, (x, y) in enumerate(pairs, start=1) if compare(x, y) <= 0)


def divider_positions(s: str) -> int:
    """Return positions of divider packets after sorting.

    >>> divider_positions(TEST_DATA)
    140
    """
    divider_packets = ("[[2]]", "[[6]]")
    packets = [packet for packet in s.split("\n") if packet] + list(divider_packets)
    sorted_packets = sorted(packets, key=cmp_to_key(compare))
    return (1 + sorted_packets.index(divider_packets[0])) * (
        1 + sorted_packets.index(divider_packets[1])
    )


TEST_DATA = """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"""


if __name__ == "__main__":
    testmod()
    input_data = stdin.read()
    print(sum_correct_indices(input_data))
    print(divider_positions(input_data))
