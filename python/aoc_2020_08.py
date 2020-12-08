"""Advent of Code 2020 - Day 8."""

from doctest import testmod
from enum import Enum, auto
from sys import stdin
from typing import List, Optional, Set, Tuple


class Operation(Enum):
    NOP = auto()
    ACC = auto()
    JMP = auto()


def parse_opcode(s: str) -> Operation:
    if s == "nop":
        return Operation.NOP
    elif s == "acc":
        return Operation.ACC
    elif s == "jmp":
        return Operation.JMP
    raise RuntimeError("Unknown opcode in parse_opcode", s)


Instruction = Tuple[Operation, int]


def parse_instruction(s: str) -> Instruction:
    [s1, s2] = s.split()
    return (parse_opcode(s1), int(s2))


class Machine:
    def __init__(self, code: List[Instruction]) -> None:
        self.code: List[Instruction] = code.copy()
        self.acc: int = 0
        self.ip: int = 0

    def step(self) -> None:
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


def run_until_loop(code: List[Instruction]) -> int:
    """Run the machine until an instruction is about to repeated.

    >>> run_until_loop(test_code)
    5
    """
    m: Machine = Machine(code)
    visited: Set[int] = set()
    while True:
        if m.ip in visited:
            return m.acc
        visited.add(m.ip)
        m.step()


def run_until_loop_or_end(code: List[Instruction]) -> Optional[int]:
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
    visited: Set[int] = set()
    while True:
        if m.ip == len(m.code):
            return m.acc
        if m.ip in visited:
            return None
        visited.add(m.ip)
        m.step()


def mutate_until_end(original_code: List[Instruction]) -> int:
    """Mutate each instruction in turn until finding one that makes the code terminate.

    >>> mutate_until_end(test_code)
    8
    """
    for mutate_index in range(1, len(original_code)):
        code: List[Instruction] = original_code.copy()
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
    code: List[Instruction] = [parse_instruction(line) for line in stdin]
    print(run_until_loop(code))
    print(mutate_until_end(code))
