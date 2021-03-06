"""Advent of Code 2020 - Day 16."""


from doctest import testmod
from functools import reduce
from re import Pattern, compile
from sys import stdin

ValidField = tuple[str, tuple[int, int], tuple[int, int]]
valid_pattern: Pattern[str] = compile(r"([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)")


def parse_input(s: str) -> tuple[list[ValidField], list[int], list[list[int]]]:
    def parse_valid(line: str) -> ValidField:
        if m := valid_pattern.fullmatch(line):
            return (
                m.group(1),
                (int(m.group(2)), int(m.group(3))),
                (int(m.group(4)), int(m.group(5))),
            )
        raise RuntimeError("Failed to parse valid line", line)

    def parse_nearby(line: str) -> list[int]:
        return [int(x) for x in line.split(",")]

    [v_str, your_str, nearby_str] = s.split("\n\n")
    return (
        [parse_valid(x) for x in v_str.splitlines()],
        [int(x) for x in your_str.splitlines()[1].split(",")],
        [parse_nearby(x) for x in nearby_str.splitlines()[1:]],
    )


test1a: list[ValidField] = [
    ("class", (1, 3), (5, 7)),
    ("row", (6, 11), (33, 44)),
    ("seat", (13, 40), (45, 50)),
]
test1b: list[list[int]] = [[7, 3, 47], [40, 4, 50], [55, 2, 20], [38, 6, 12]]


def run1(valids: list[ValidField], tickets: list[list[int]]) -> int:
    """Return the error rate of nearby tickets.

    >>> run1(test1a, test1b)
    71
    """
    valid_set: set[int] = set()
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


def run2(valids: list[ValidField], your: list[int], tickets: list[list[int]]) -> int:
    """Find the six fields on your ticket."""

    def valid_value(f: ValidField, x: int) -> bool:
        """Is the value valid for the given field."""
        _, (lo1, hi1), (lo2, hi2) = f
        val = (lo1 <= x <= hi1) or (lo2 <= x <= hi2)
        return val

    # Eliminate invalid tickets
    valid_set: set[int] = set()
    for _, (a, b), (c, d) in valids:
        for i in range(a, b + 1):
            valid_set.add(i)
        for i in range(c, d + 1):
            valid_set.add(i)
    valid_tickets: list[list[int]] = [
        ticket for ticket in tickets if all(num in valid_set for num in ticket)
    ]
    # For each field see which positions are compatible
    field_fit: list[tuple[str, list[int]]] = []
    for field in valids:
        valid_positions: list[int] = []
        for position in range(0, len(tickets[0])):
            if all(valid_value(field, ticket[position]) for ticket in valid_tickets):
                valid_positions.append(position)

        field_fit.append((field[0], valid_positions))
    # Allocate fields with only one possible position until all fields are allocated
    allocated_fields: list[tuple[str, int]] = []
    while field_fit:
        for field_name, field_positions in field_fit:
            if not field_positions:
                raise RuntimeError("No position found for", field_name)
            if len(field_positions) == 1:
                found_position: int = field_positions[0]
                allocated_fields.append((field_name, found_position))
                field_fit.remove((field_name, field_positions))
                for other_field_name, other_field_positions in field_fit:
                    other_field_positions.remove(found_position)
                break
    departure_field_positions = [
        position for name, position in allocated_fields if name.startswith("departure")
    ]
    your_values: list[int] = [your[position] for position in departure_field_positions]
    return reduce(lambda x, y: x * y, your_values, 1)


if __name__ == "__main__":
    testmod()
    [valid_fields, your_ticket, nearby_tickets] = parse_input(stdin.read())
    print(run1(valid_fields, nearby_tickets))
    print(run2(valid_fields, your_ticket, nearby_tickets))
