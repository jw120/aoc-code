"""Advent of Code 2019 - Day 7."""

from doctest import testmod
from sys import stdin
from typing import Iterator, List, TypeVar

import IntCode

PhaseSettings = List[int]

test_code_1: List[int] = [
    3,
    15,
    3,
    16,
    1002,
    16,
    10,
    16,
    1,
    16,
    15,
    15,
    4,
    15,
    99,
    0,
    0,
]
test_code_2: List[int] = [
    3,
    23,
    3,
    24,
    1002,
    24,
    10,
    24,
    1002,
    23,
    -1,
    23,
    101,
    5,
    23,
    23,
    1,
    24,
    23,
    23,
    4,
    23,
    99,
    0,
    0,
]

test_code_3: List[int] = [
    3,
    31,
    3,
    32,
    1002,
    32,
    10,
    32,
    1001,
    31,
    -2,
    31,
    1007,
    31,
    0,
    33,
    1002,
    33,
    7,
    33,
    1,
    33,
    31,
    31,
    1,
    32,
    31,
    31,
    4,
    31,
    99,
    0,
    0,
    0,
]


def run_machine(
    code: List[int], phase: int, input_val: int, print_instructions: bool = False
) -> int:
    if print_instructions:
        print(f"Running machine with phase setting {phase} and input {input_val}")
    m: IntCode.Machine = IntCode.Machine(code, [phase, input_val])
    m.run(print_instructions)
    [output_value] = m.output_vals
    if print_instructions:
        print(f"Output {output_value}")
    return output_value


def run_phase_settings(
    code: List[int], phase_settings: PhaseSettings, print_instructions: bool = False
) -> int:
    """Run a series machines from input 0 with given phase settings.

    >>> run_phase_settings(test_code_1, [4, 3, 2, 1, 0])
    43210
    >>> run_phase_settings(test_code_2, [0, 1, 2, 3, 4])
    54321
    >>> run_phase_settings(test_code_3, [1, 0, 4, 3, 2])
    65210
    """
    output_val: int = 0
    while phase_settings:
        output_val = run_machine(
            code, phase_settings[0], output_val, print_instructions
        )
        phase_settings = phase_settings[1:]
    return output_val


X = TypeVar("X")


def permutations(xs: List[X]) -> Iterator[List[X]]:
    """Return all permutations of the list.

    >>> [p for p in permutations([1,2,3])]
    [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
    """
    if len(xs) <= 1:
        yield xs
    else:
        for i in range(0, len(xs)):
            first_val: X = xs[i]
            other_vals: List[X] = xs[:i] + xs[i + 1 :]
            for p in permutations(other_vals):
                yield [first_val] + p


def part_one(code: List[int]) -> int:
    return max(
        run_phase_settings(code, p, False) for p in permutations([0, 1, 2, 3, 4])
    )


if __name__ == "__main__":
    testmod()
    code: List[int] = [int(s) for s in stdin.read().split(",")]
    print(part_one(code))
