"""Advent of Code 2025 - Day 1."""

from sys import stdin
from typing import Final

START_POSITION: Final[int] = 50
DIAL_SIZE: Final[int] = 100


def parse_move(s: str) -> int:
    """Parse one input line."""
    num = int(s[1:])
    match s[0]:
        case "L":
            return -num
        case "R":
            return num
        case _:
            raise ValueError(f"Bad input '{s}'")


def run(_moves: list[int]) -> tuple[int, int]:
    """Run the moves counting zero finishes and clicks."""
    position: int = START_POSITION
    zero_finishes: int = 0
    zero_passes: int = 0
    for move in moves:
        zero_passes += (abs(move)) // DIAL_SIZE
        m: int = (abs(move) % DIAL_SIZE) * (-1 if move < 0 else 1)
        position += m
        if position % DIAL_SIZE == 0:
            zero_finishes += 1
            position = 0
        if position > DIAL_SIZE:
            zero_passes += 1
            position -= DIAL_SIZE
        if position < 0:
            zero_passes += 1 if position != m else 0
            position += DIAL_SIZE
    return zero_finishes, zero_finishes + zero_passes


if __name__ == "__main__":
    moves: list[int] = [parse_move(s) for s in stdin.readlines()]
    part_a, part_b = run(moves)
    print(part_a)
    print(part_b)
