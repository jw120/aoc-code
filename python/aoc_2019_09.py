"""Advent of Code 2019 - Day 5."""

from sys import stdin
from typing import List

import IntCode


def part_one(code: List[int]) -> None:
    machine: IntCode.Machine = IntCode.Machine(code, [1])
    machine.run(True)
    print(machine.output_vals)


if __name__ == "__main__":
    code: List[int] = [int(s) for s in stdin.read().split(",")]
    part_one(code)
