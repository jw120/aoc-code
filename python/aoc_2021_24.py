"""Advent of Code 2021 - Day 24."""

# from __future__ import annotations

from doctest import testmod
from typing import Callable, Literal, NoReturn, Tuple, Union

import operator

# from sys import stdin


def assert_never(value: NoReturn) -> NoReturn:
    assert False, f"Unhandled value: {value} ({type(value).__name__})"


InstructionKind = Callable[[int, int], int]
InstructionRegister = Literal["w", "x", "y", "z"]
InstructionSource = Union[InstructionRegister, int, None]
Instruction = Tuple[InstructionKind, InstructionRegister, InstructionSource]


def parse_instruction(s: str) -> Instruction:
    pass


class ALU:
    def __init__(self, program: list[str], inputs: list[int]) -> None:
        self.reg: Tuple[int, int, int, int] = (0, 0, 0, 0)
        self.program = [parse_instruction(s) for s in program]
        self.inputs: list[int] = inputs

    def step(self, instruction: Instruction) -> None:
        kind, target, source = instruction

        w, x, y, z = self.reg

        if source is None:  # Input instruction
            input_value = self.inputs.pop()
        elif source == "w":
            input_value = w
        elif source == "x":
            input_value = x
        elif source == "y":
            input_value = y
        elif source == "z":
            input_value = z
        elif isinstance(source, int):
            input_value = int(source)
        else:
            assert_never(source)

        if target == "w":
            self.reg = (kind(w, input_value), x, y, z)
        if target == "x":
            self.reg = (w, kind(x, input_value), y, z)
        if target == "y":
            self.reg = (w, x, kind(y, input_value), z)
        if target == "z":
            self.reg = (w, x, y, kind(z, input_value))


if __name__ == "__main__":
    testmod()
