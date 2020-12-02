# Advent of Code 2020 - Day 2

from sys import stdin
from typing import Callable, List, Tuple

Entry = Tuple[int, int, str, str]


def parse_entry(entry_string: str) -> Entry:
    [range_part, letter_part, password] = entry_string.strip().split(" ")
    [min_string, max_string] = range_part.split("-")
    return (int(min_string), int(max_string), letter_part[0], password)


def solve(check: Callable[[Entry], bool], entries: List[Entry]) -> int:
    return sum([check(e) for e in entries])


def check_one(e: Entry) -> bool:
    (min, max, letter, password) = e
    letter_count: int = password.count(letter)
    return min <= letter_count <= max


def check_two(e: Entry) -> bool:
    (pos1, pos2, letter, password) = e
    return (password[pos1 - 1] == letter) != (password[pos2 - 1] == letter)


if __name__ == "__main__":
    entries: List[Entry] = [parse_entry(line) for line in stdin]
    print(solve(check_one, entries))
    print(solve(check_two, entries))
