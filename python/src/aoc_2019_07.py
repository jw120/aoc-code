"""Advent of Code 2019 - Day 7."""

from doctest import testmod
from sys import stdin
from typing import TYPE_CHECKING

from int_code import Machine

if TYPE_CHECKING:
    from collections.abc import Iterator

PhaseSettings = list[int]

test_code_1: list[int] = [
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
test_code_2: list[int] = [
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

test_code_3: list[int] = [
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

test_code_4: list[int] = [
    3,
    26,
    1001,
    26,
    -4,
    26,
    3,
    27,
    1002,
    27,
    2,
    27,
    1,
    27,
    26,
    27,
    4,
    27,
    1001,
    28,
    -1,
    28,
    1005,
    28,
    6,
    99,
    0,
    0,
    5,
]

test_code_5: list[int] = [
    3,
    52,
    1001,
    52,
    -5,
    52,
    3,
    53,
    1,
    52,
    56,
    54,
    1007,
    54,
    5,
    55,
    1005,
    55,
    26,
    1001,
    54,
    -5,
    54,
    1105,
    1,
    12,
    1,
    53,
    54,
    53,
    1008,
    54,
    0,
    55,
    1001,
    55,
    1,
    55,
    2,
    53,
    55,
    53,
    4,
    53,
    1001,
    56,
    -1,
    56,
    1005,
    56,
    6,
    99,
    0,
    0,
    0,
    0,
    10,
]


def run_machine(
    code: list[int], phase: int, input_val: int, *, print_instructions: bool = False
) -> int:
    """Run the machine."""
    if print_instructions:
        print(f"Running machine with phase setting {phase} and input {input_val}")
    m: Machine = Machine(code, [phase, input_val])
    m.run(print_instructions=print_instructions)
    if len(m.output_vals) != 1:
        raise ValueError("Bad number of output_vals")
    output_value = m.output_vals[0]
    if print_instructions:
        print(f"Output {output_value}")
    return output_value


def run_phase_settings(
    code: list[int], phase_settings: PhaseSettings, *, print_instructions: bool = False
) -> int:
    """Run a series of machines from input 0 with given phase settings.

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
            code, phase_settings[0], output_val, print_instructions=print_instructions
        )
        phase_settings = phase_settings[1:]
    return output_val


def run_phase_settings_with_feedback(code: list[int], phase_settings: PhaseSettings) -> int:
    """Run a series machines with feedback from input 0 with given phase settings.

    >>> run_phase_settings_with_feedback(test_code_4, [9, 8, 7, 6, 5])
    139629729
    >>> run_phase_settings_with_feedback(test_code_5, [9, 7, 8, 5, 6])
    18216
    """
    machines: list[Machine] = [Machine(code, [p]) for p in phase_settings]
    for m in machines:
        m.pause_after_output = True

    input_val: int = 0
    machine_index: int = 0

    while True:
        machines[machine_index].input_vals.append(input_val)
        machines[machine_index].run()
        if machines[machine_index].halted and machine_index == len(machines) - 1:
            return machines[machine_index].output_vals[-1]
        input_val = machines[machine_index].output_vals[-1]
        machine_index += 1
        if machine_index >= len(machines):
            machine_index = 0


def permutations[X](xs: list[X]) -> Iterator[list[X]]:
    """Return all permutations of the list.

    >>> [p for p in permutations([1,2,3])]
    [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
    """
    if len(xs) <= 1:
        yield xs
    else:
        for i, first_val in enumerate(xs):
            other_vals: list[X] = xs[:i] + xs[i + 1 :]
            for p in permutations(other_vals):
                yield [first_val, *p]


def part_one(code: list[int]) -> int:
    """Solve part one."""
    return max(run_phase_settings(code, p) for p in permutations([0, 1, 2, 3, 4]))


def part_two(code: list[int]) -> int:
    """Solve part two."""
    return max(run_phase_settings_with_feedback(code, p) for p in permutations([5, 6, 7, 8, 9]))


if __name__ == "__main__":
    testmod()
    input_code: list[int] = [int(s) for s in stdin.read().split(",")]
    print(part_one(input_code))
    print(part_two(input_code))
