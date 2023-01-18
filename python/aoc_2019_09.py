"""Advent of Code 2019 - Day 9."""

# pylint: disable=missing-function-docstring

from sys import stdin

import IntCode


def run(code: list[int], x: int) -> int:
    machine: IntCode.Machine = IntCode.Machine(code, [x])
    machine.run()
    assert len(machine.output_vals) == 1
    return machine.output_vals[0]


if __name__ == "__main__":
    input_code: list[int] = [int(s) for s in stdin.read().split(",")]
    print(run(input_code, 1))
    print(run(input_code, 2))
