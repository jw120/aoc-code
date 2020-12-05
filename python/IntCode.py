"""Machine to run IntCode."""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import ClassVar, Dict, List, Optional


class Mode(Enum):
    POSITION = 0
    IMMEDIATE = 1


def tag(e: Enum) -> str:
    if e == Mode.POSITION:
        return "p"
    if e == Mode.IMMEDIATE:
        return "i"
    raise RuntimeError("Unknown mode", e)


def parameter_decode(opcode: int, n: int) -> List[Mode]:
    """Return the addressing modes associate with the opcode.

    n is the number of inputs and outputs that the instruction has

    >>> parameter_decode(1002, 3)
    [Mode.POSITION, Mode.IMMEDIATE, Mode.Position]
    """
    flags: int = opcode // 100
    modes: List[Mode] = []
    while len(modes) < n:
        if flags % 10 == Mode.POSITION.value:
            modes.append(Mode.POSITION)
        elif flags % 10 == Mode.IMMEDIATE.value:
            modes.append(Mode.IMMEDIATE)
        else:
            raise RuntimeError("Unknown mode", flags)
        flags //= 10
    return modes


@dataclass(frozen=True)
class Instruction:

    name: str
    inputs: int
    has_output: bool


class Machine:
    """Machine that runs IntCode."""

    def __init__(self, code: List[int], input_val: Optional[int] = None) -> None:
        self.code: List[int] = code.copy()
        self.ip: int = 0
        self.input_val: Optional[int] = input_val
        self.output_val: Optional[int] = None
        self.halted = False

    def do_add(self, vals: List[int]) -> int:
        if len(vals) != 2:
            raise RuntimeError("Wrong number of inputs to do_add")
        return vals[0] + vals[1]

    def do_mul(self, vals: List[int]) -> int:
        if len(vals) != 2:
            raise RuntimeError("Wrong number of inputs to do_mul")
        return vals[0] * vals[1]

    def do_input(self, vals: List[int]) -> int:
        if len(vals) != 1:
            raise RuntimeError("Wrong number of inputs to do_input")
        if self.input_val:
            return self.input_val
        raise RuntimeError("No input available")

    def do_output(self, vals: List[int]) -> Optional[int]:
        if len(vals) != 1:
            raise RuntimeError("Wrong number of inputs to do_output")
        self.output_val = vals[0]
        return None

    def do_halt(self, vals: List[int]) -> Optional[int]:
        if len(vals) != 0:
            raise RuntimeError("Wrong number of inputs to halt")
        self.halted = True
        return None

    def do_action(self: Machine, code: int, vals: List[int]) -> Optional[int]:
        if code == 1:
            return self.do_add(vals)
        if code == 2:
            return self.do_mul(vals)
        if code == 3:
            return self.do_input(vals)
        if code == 4:
            return self.do_output(vals)
        if code == 99:
            return self.do_halt(vals)
        raise RuntimeError("Unknown instruction in do_action", code)

    instruction_set: ClassVar[Dict[int, Instruction]] = {
        1: Instruction("add", 2, True),
        2: Instruction("mul", 2, True),
        3: Instruction("input", 1, False),
        4: Instruction("output", 0, True),
        99: Instruction("halt", 0, False),
    }

    def step(self) -> bool:
        """Run one instruction, returning true if execution can continue."""
        opcode: int = self.code[self.ip]
        instruction_code: int = opcode % 100
        if instruction_code not in Machine.instruction_set:
            raise RuntimeError("Unknown opcode in step", instruction_code)
        instruction: Instruction = Machine.instruction_set[instruction_code]
        parameter_modes: List[Mode] = parameter_decode(
            opcode, instruction.inputs + instruction.has_output
        )

        input_codes: List[int] = code[self.ip + 1 : self.ip + 1 + instruction.inputs]
        inputs: List[int] = [
            self.fetch(mode, val) for (mode, val) in zip(parameter_modes, input_codes)
        ]
        output: Optional[int] = self.do_action(instruction_code, inputs)
        if instruction.has_output:
            if output is None:
                raise RuntimeError("None when has_output", opcode)
            if parameter_modes[-1] == Mode.IMMEDIATE:
                raise RuntimeError("Can't have output with immediate mode")
            location: int = code[self.ip + 1 + instruction.inputs]
            code[location] = output
        else:
            if output is not None:
                raise RuntimeError("Unexpected output present")
        self.ip += 1 + instruction.inputs + instruction.has_output
        return not self.halted

    def run(self) -> None:
        """Run until execution stops."""
        while self.step():
            pass

    def fetch(self, mode: Mode, x: int) -> int:
        """Retrieve the data with the given parameter value."""
        if mode == Mode.POSITION:
            return self.code[x]
        if mode == Mode.IMMEDIATE:
            return x
        return 0

    def disassamble(self) -> None:
        """Print a text representation of the code to stdout."""
        i: int = 0
        while i < len(self.code):
            opcode: int = self.code[i]
            instruction_code: int = opcode % 100
            if instruction_code not in Machine.instruction_set:
                raise RuntimeError("Unknown opcode in step", instruction_code)
            instruction: Instruction = Machine.instruction_set[instruction_code]
            params: int = instruction.inputs + instruction.has_output
            modes: List[Mode] = parameter_decode(opcode, params)

            print(f"{i:4} {instruction.name}", end="")
            if params > 0:
                print("_", end="")
            for mode in modes:
                print(tag(mode), end="")
            print(" ", end="")
            param_values: List[int] = self.code[
                i + 1 : i + 1 + instruction.inputs + instruction.has_output
            ]
            print(", ".join(str(p) for p in param_values))

            i += 1 + instruction.inputs + instruction.has_output


if __name__ == "__main__":
    print("Day 2 code")
    with open("input/2019_02.txt", "r") as f:
        code: List[int] = [int(s) for s in f.read().split(",")]
        Machine(code).disassamble()

    print("Day 5 code")
    with open("input/2019_05.txt", "r") as f:
        code = [int(s) for s in f.read().split(",")]
        Machine(code).disassamble()
