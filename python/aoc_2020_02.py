"""Advent of Code 2020 - Day 2."""

from doctest import testmod
from sys import stdin
from typing import Tuple

Entry = Tuple[int, int, str, str]


def parse_entry(entry_string: str) -> Entry:
    r"""Convert input string form of an entry.

    >>> parse_entry('1-3 a: abcde\n')
    (1, 3, 'a', 'abcde')
    """
    [range_part, letter_part, password] = entry_string.strip().split(" ")
    [min_string, max_string] = range_part.split("-")
    return (int(min_string), int(max_string), letter_part[0], password)


def check_one(e: Entry) -> bool:
    """Test if the password entry valid under the rules of part one.

    >>> check_one((1, 3, 'a', 'abcde'))
    True
    >>> check_one((1, 3, 'b', 'cdefg'))
    False
    """
    (min_count, max_count, letter, password) = e
    actual_count: int = password.count(letter)
    return min_count <= actual_count <= max_count


def check_two(e: Entry) -> bool:
    """Test if the password entry valid under the rules of part two.

    >>> check_two((1, 3, 'a', 'abcde'))
    True
    >>> check_two((1, 3, 'b', 'cdefg'))
    False
    """
    (pos1, pos2, letter, password) = e
    return (password[pos1 - 1] == letter) != (password[pos2 - 1] == letter)


if __name__ == "__main__":
    testmod()
    entries: list[Entry] = [parse_entry(line) for line in stdin]
    print(sum(check_one(e) for e in entries))
    print(sum(check_two(e) for e in entries))
