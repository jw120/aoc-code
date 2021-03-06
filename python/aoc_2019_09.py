"""Advent of Code 2019 - Day 9."""

from sys import stdin

import IntCode


def run(code: list[int], x: int) -> int:
    machine: IntCode.Machine = IntCode.Machine(code, [x])
    machine.run()
    [ans] = machine.output_vals
    return ans


if __name__ == "__main__":
    code: list[int] = [int(s) for s in stdin.read().split(",")]
    print(run(code, 1))
    print(run(code, 2))
