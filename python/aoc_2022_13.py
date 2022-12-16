"""Advent of Code 2022 - Day 13."""

from doctest import testmod

from collections import deque
from re import match
from sys import stdin

from Coord import Coord, Extent


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


def compare(x: str, y: str, open_lists: int = 0) -> bool:
    """Q"""
    i = 0
    j = 0
    while True:
        assert i < len(x) and j < len(
            y
        ), f"Unexpected end of string '{x}' '{y}' {i} {j}"
        # print(f"Comparing '{x[i:]}', '{y[i:]}'")
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
                # print("Comparing ints", x_int, y_int)
                if x_int == y_int:
                    i += x_len
                    j += y_len
                else:
                    return x_int < y_int
            case ",", ",":
                i += 1
                j += 1
            case "]", _:
                return True
            case _, "]":
                return False
            case "]", "]":
                open_lists -= 1
                assert open_lists >= 0
                i += 1
                j += 1
            case _:
                raise ValueError(f"Unexpected match {x[i]}, {y[i]} for\n{x}\n{y}")


def sum_correct_indices(s: str) -> int:
    """Return sum of indices for pairs which are in correct order.

    >>> sum_correct_indices(test_data)
    13
    """
    pairs = [pair.split("\n") for pair in s.split("\n\n")]
    return sum(i for i, (x, y) in enumerate(pairs, start=1) if compare(x, y))


test_data = """[1,1,3,1,1]
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
    print(sum_correct_indices(stdin.read()))
