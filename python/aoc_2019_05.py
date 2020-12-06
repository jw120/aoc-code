"""Advent of Code 2019 - Day 5."""

from sys import stdin
from typing import List

import IntCode


def part_one(code: List[int]) -> None:
    machine: IntCode.Machine = IntCode.Machine(code, 1)
    machine.run()
    if any(machine.output_vals[:-1]):
        print(f"Failed tests: {machine.output_vals}")
    else:
        print(machine.output_vals[-1])


def part_two(code: List[int]) -> None:
    machine: IntCode.Machine = IntCode.Machine(code, 5)
    machine.run()
    print(machine.output_vals[-1])


if __name__ == "__main__":
    code: List[int] = [int(s) for s in stdin.read().split(",")]
    part_one(code)
    part_two(code)
