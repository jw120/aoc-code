"""Advent of Code 2019 - Day 2."""

from doctest import testmod
from sys import stdin
from typing import List

import IntCode


def run_with_noun_verb(code: List[int], noun: int, verb: int) -> int:
    machine: IntCode.Machine = IntCode.Machine(code)
    machine.code[1] = noun
    machine.code[2] = verb
    machine.run()
    return machine.code[0]


def search_noun_verb(code: List[int], target: int) -> int:
    for noun in range(0, 100):
        for verb in range(0, 100):
            if run_with_noun_verb(code, noun, verb) == target:
                return 100 * noun + verb
    raise RuntimeError("Failed to hit target")


if __name__ == "__main__":
    testmod()
    code: List[int] = [int(s) for s in stdin.read().split(",")]
    print(run_with_noun_verb(code, 12, 2))
    print(search_noun_verb(code, 19690720))
