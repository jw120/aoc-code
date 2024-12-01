"""Advent of Code 2019 - Day 2."""

from doctest import testmod
from sys import stdin

import int_code


def run_with_noun_verb(code: list[int], noun: int, verb: int) -> int:
    """Run with noun and verb."""
    machine: int_code.Machine = int_code.Machine(code)
    machine.code[1] = noun
    machine.code[2] = verb
    machine.run()
    return machine.code[0]


def search_noun_verb(code: list[int], target: int) -> int:
    """Search for noun and verb."""
    for noun in range(100):
        for verb in range(100):
            if run_with_noun_verb(code, noun, verb) == target:
                return 100 * noun + verb
    raise RuntimeError("Failed to hit target")


if __name__ == "__main__":
    testmod()
    input_code: list[int] = [int(s) for s in stdin.read().split(",")]
    print(run_with_noun_verb(input_code, 12, 2))
    print(search_noun_verb(input_code, 19690720))
