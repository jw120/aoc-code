"""Advent of Code 2020 - Day 8."""

from __future__ import annotations

from doctest import testmod
from enum import Enum, auto
from sys import stdin


class Operation(Enum):
    """Operation class."""

    NOP = auto()
    ACC = auto()
    JMP = auto()

    @staticmethod
    def parse(s: str) -> Operation:
        """Read an operation from a string."""
        if s == "nop":
            return Operation.NOP
        if s == "acc":
            return Operation.ACC
        if s == "jmp":
            return Operation.JMP
        raise RuntimeError("Unknown opcode in parse()", s)


Instruction = tuple[Operation, int]


def parse_instruction(s: str) -> Instruction:
    """Read an instruction from a string."""
    [s1, s2] = s.split()
    return (Operation.parse(s1), int(s2))


class Machine:
    """Main class for day 8."""

    def __init__(self, code: list[Instruction]) -> None:
        self.code: list[Instruction] = code.copy()
        self.acc: int = 0
        self.ip: int = 0

    def step(self) -> None:
        """Step the machine."""
        (operation, argument) = self.code[self.ip]
        if operation == Operation.NOP:
            self.ip += 1
        elif operation == Operation.ACC:
            self.acc += argument
            self.ip += 1
        elif operation == Operation.JMP:
            self.ip += argument
        else:
            raise RuntimeError("Unknown opcode in step", operation)


test_code = [
    parse_instruction(line)
    for line in [
        "nop +0",
        "acc +1",
        "jmp +4",
        "acc +3",
        "jmp -3",
        "acc -99",
        "acc +1",
        "jmp -4",
        "acc +6",
    ]
]
test_code_v1 = test_code.copy()
test_code_v1[0] = (Operation.JMP, 0)
test_code_v2 = test_code.copy()
test_code_v2[7] = (Operation.NOP, -4)


def run_until_loop(code: list[Instruction]) -> int:
    """Run the machine until an instruction is about to repeated.

    >>> run_until_loop(test_code)
    5
    """
    m: Machine = Machine(code)
    visited: set[int] = set()
    while True:
        if m.ip in visited:
            return m.acc
        visited.add(m.ip)
        m.step()


def run_until_loop_or_end(code: list[Instruction]) -> int | None:
    """Run the machine until a loop is found or the program ends.

    Returns the value of the if the programs ends normally.

    >>> run_until_loop_or_end(test_code) is None
    True
    >>> run_until_loop_or_end(test_code_v1) is None
    True
    >>> run_until_loop_or_end(test_code_v2)
    8
    """
    m: Machine = Machine(code)
    visited: set[int] = set()
    while True:
        if m.ip == len(m.code):
            return m.acc
        if m.ip in visited:
            return None
        visited.add(m.ip)
        m.step()


def mutate_until_end(original_code: list[Instruction]) -> int:
    """Mutate each instruction in turn until finding one that makes the code terminate.

    >>> mutate_until_end(test_code)
    8
    """
    for mutate_index in range(1, len(original_code)):
        code: list[Instruction] = original_code.copy()
        (operation, argument) = code[mutate_index]
        if operation == Operation.JMP:
            operation = Operation.NOP
        elif operation == Operation.NOP:
            operation = Operation.JMP
        code[mutate_index] = (operation, argument)
        if acc := run_until_loop_or_end(code):
            return acc
    raise RuntimeError("No mutation found")


if __name__ == "__main__":
    testmod()
    input_code: list[Instruction] = [parse_instruction(line) for line in stdin]
    print(run_until_loop(input_code))
    print(mutate_until_end(input_code))
