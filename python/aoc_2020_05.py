"""Advent of Code 2020 - Day 5."""

from doctest import testmod
from sys import stdin


def seat_id(code: str) -> int:
    """Return the seat id for the given boarding pass code.

    >>> seat_id('FBFBBFFRLR')
    357
    >>> seat_id('BFFFBBFRRR')
    567
    >>> seat_id('FFFBBBFRRR')
    119
    >>> seat_id('BBFFBBFRLL')
    820
    """
    swapped: str = (
        code.replace("B", "1").replace("F", "0").replace("R", "1").replace("L", "0")
    )
    return int(swapped, 2)


def empty_seat(seats: list[int]) -> int:
    """Return index of first empty seat."""
    sorted_seats: list[int] = sorted(seats)
    prev_seat: int = sorted_seats[0]
    for seat in sorted_seats[1:]:
        if seat != prev_seat + 1:
            return prev_seat + 1
        prev_seat = seat
    raise RuntimeError("No empty seat found")


if __name__ == "__main__":
    testmod()
    ids: list[int] = [seat_id(code) for code in stdin]
    print(max(ids))
    print(empty_seat(ids))
