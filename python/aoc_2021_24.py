"""Advent of Code 2021 - Day 24."""

from __future__ import annotations

from doctest import testmod
from enum import Enum, auto
from itertools import combinations_with_replacement
from operator import add, mod, mul
from sys import stdin
from typing import Callable, Iterable, Iterator, NoReturn, Optional, Tuple, Union


def assert_never(value: NoReturn) -> NoReturn:
    assert False, f"Unhandled value: {value} ({type(value).__name__})"


class Register(Enum):
    w = auto()
    x = auto()
    y = auto()
    z = auto()


InstructionKind = Callable[[int, int], int]
InstructionSource = Union[Register, int, None]
Instruction = Tuple[InstructionKind, Register, InstructionSource, str]


def replace(reg: int, val: int) -> int:
    return val


def equal(reg: int, val: int) -> int:
    return int(reg == val)


def sign(x: int) -> int:
    if x > 0:
        return 1
    if x < 0:
        return -1
    return 0


def divide(reg: int, val: int) -> int:
    """Floor division rounding towards zero.

    >>> divide(5, 2)
    2
    >>> divide(-7, 2)
    -3
    >>> divide(5, -2)
    -2
    >>> divide(-11, -2)
    5
    """
    return (abs(reg) // abs(val)) * sign(reg) * sign(val)


def parse_instruction(s: str) -> Instruction:
    parts = s.split()
    assert (len(parts) == 3 and parts[0] != "inp") or (
        len(parts) == 2 and parts[0] == "inp"
    )
    assert parts[1] in "wxyz"
    target: Register = Register[parts[1]]

    if parts[0] == "inp":
        assert len(parts) == 2
        return (replace, target, None, s)

    if parts[2] in "wxyz":
        source: InstructionSource = Register[parts[2]]
    else:
        source = int(parts[2])

    if parts[0] == "add":
        kind: InstructionKind = add
    elif parts[0] == "mul":
        kind = mul
    elif parts[0] == "div":
        kind = divide
    elif parts[0] == "mod":
        kind = mod
    elif parts[0] == "eql":
        kind = equal
    else:
        raise ValueError("Unknown instruction " + s)

    return (kind, target, source, s)

    pass


class ALU:
    def __init__(self, program: list[str], inputs: list[int] = []) -> None:
        self.reg: Tuple[int, int, int, int] = (0, 0, 0, 0)
        self.program = [parse_instruction(s) for s in program]
        self.inputs: Iterator[int] = iter(inputs)

    def run(self, inputs: Optional[Iterable[int]] = None) -> ALU:
        """
        Run the program with given inputs.

        >>> ALU(test1).run([5]).reg
        (0, -5, 0, 0)
        >>> ALU(test2).run([15]).reg
        (1, 1, 1, 1)
        >>> ALU(test2).run([6]).reg
        (0, 1, 1, 0)
        """
        self.reg = (0, 0, 0, 0)
        if inputs is not None:
            self.inputs = iter(inputs)
        for instruction in self.program:
            self.step(instruction)
        return self

    def step(self, instruction: Instruction) -> None:
        kind, target, source, text = instruction

        # print(self.reg, text)

        w, x, y, z = self.reg

        if source is None:  # Input instruction
            input_value = next(self.inputs)
        elif source == Register.w:
            input_value = w
        elif source == Register.x:
            input_value = x
        elif source == Register.y:
            input_value = y
        elif source == Register.z:
            input_value = z
        elif isinstance(source, int):
            input_value = source
        else:
            raise ValueError("Internal failure, bad source")

        if target == Register.w:
            self.reg = (kind(w, input_value), x, y, z)
        if target == Register.x:
            self.reg = (w, kind(x, input_value), y, z)
        if target == Register.y:
            self.reg = (w, x, kind(y, input_value), z)
        if target == Register.z:
            self.reg = (w, x, y, kind(z, input_value))


test1 = ["inp x", "mul x -1"]
test2 = [
    "inp w",
    "add z w",
    "mod z 2",
    "div w 2",
    "add y w",
    "mod y 2",
    "div w 2",
    "add x w",
    "mod x 2",
    "div w 2",
    "mod w 2",
]

if __name__ == "__main__":
    testmod()
    monad_source = stdin.read().splitlines()
    alu = ALU(monad_source)
    for model_number in combinations_with_replacement([1, 2, 3, 4, 5, 6, 7, 8, 9], 14):
        alu.run(model_number)
        # print(model_number, alu.reg[3])
        if alu.reg[3] == 0:
            print(model_number)
