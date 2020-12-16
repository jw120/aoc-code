"""Advent of Code 2020 - Day 16."""


from doctest import testmod
from re import compile
from sys import stdin
from typing import List, Pattern, Set, Tuple

ValidField = Tuple[str, Tuple[int, int], Tuple[int, int]]
valid_pattern: Pattern[str] = compile(r"([a-z ])+: (\d+)-(\d+) or (\d+)-(\d+)")


def parse_input(s: str) -> Tuple[List[ValidField], List[int], List[List[int]]]:
    def parse_valid(line: str) -> ValidField:
        if m := valid_pattern.fullmatch(line):
            return (
                m.group(1),
                (int(m.group(2)), int(m.group(3))),
                (int(m.group(4)), int(m.group(5))),
            )
        raise RuntimeError("Failed to parse valid line", line)

    def parse_nearby(line: str) -> List[int]:
        return [int(x) for x in line.split(",")]

    [v_str, your_str, nearby_str] = s.split("\n\n")
    return (
        [parse_valid(x) for x in v_str.splitlines()],
        [int(x) for x in your_str.splitlines()[1].split(",")],
        [parse_nearby(x) for x in nearby_str.splitlines()[1:]],
    )


test1a: List[ValidField] = [
    ("class", (1, 3), (5, 7)),
    ("row", (6, 11), (33, 44)),
    ("seat", (13, 40), (45, 50)),
]
test1b: List[List[int]] = [[7, 3, 47], [40, 4, 50], [55, 2, 20], [38, 6, 12]]


def run1(valids: List[ValidField], tickets: List[List[int]]) -> int:
    """Return the error rate of nearby tickets.

    >>> run1(test1a, test1b)
    71
    """
    valid_set: Set[int] = set()
    count: int = 0
    for _, (a, b), (c, d) in valids:
        for i in range(a, b + 1):
            valid_set.add(i)
        for i in range(c, d + 1):
            valid_set.add(i)
    for ticket in tickets:
        for num in ticket:
            if num not in valid_set:
                #                print("Not valid", num)
                count += num
    return count


test2a: List[ValidField] = [
    ("class", (0, 1), (4, 19)),
    ("row", (0, 5), (8, 19)),
    ("seat", (0, 13), (16, 19)),
]
test2b: List[int] = [11, 12, 13]
test2c: List[List[int]] = [[3, 9, 18], [15, 1, 5], [5, 14, 9], [24, 15, 16]]


def run2(valids: List[ValidField], your: List[int], tickets: List[List[int]]) -> int:
    """Find the six fields on your ticket.

    >>> run2(test2a, test2b, test2c)
    71
    """
    # Eliminate invalid tickets
    valid_set: Set[int] = set()
    for _, (a, b), (c, d) in valids:
        for i in range(a, b + 1):
            valid_set.add(i)
        for i in range(c, d + 1):
            valid_set.add(i)
    valid_tickets: List[List[int]] = [
        ticket for ticket in tickets if all(num in valid_set for num in ticket)
    ]
    print(valid_tickets)
    return 0


if __name__ == "__main__":
    testmod()
    [valid_fields, your_ticket, nearby_tickets] = parse_input(stdin.read())
    print(run1(valid_fields, nearby_tickets))
