"""Advent of Code 2019 - Day 5."""

from sys import stdin

import int_code


def part_one(code: list[int]) -> None:
    """Solve part one."""
    machine: int_code.Machine = int_code.Machine(code, [1])
    machine.run()
    if any(machine.output_vals[:-1]):
        print(f"Failed tests: {machine.output_vals}")
    else:
        print(machine.output_vals[-1])


def part_two(code: list[int]) -> None:
    """Solve part two."""
    machine: int_code.Machine = int_code.Machine(code, [5])
    machine.run()
    print(machine.output_vals[-1])


if __name__ == "__main__":
    input_code: list[int] = [int(s) for s in stdin.read().split(",")]
    part_one(input_code)
    part_two(input_code)
