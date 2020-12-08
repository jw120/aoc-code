"""Advent of Code 2019 - Day 5."""

from sys import stdin
from typing import List

import IntCode


def run(code: List[int], x: int) -> int:
    machine: IntCode.Machine = IntCode.Machine(code, [x])
    machine.run()
    [ans] = machine.output_vals
    return ans


if __name__ == "__main__":
    code: List[int] = [int(s) for s in stdin.read().split(",")]
    print(run(code, 1))
    print(run(code, 2))
