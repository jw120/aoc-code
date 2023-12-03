"""IntCode Machine."""

from __future__ import annotations

from collections.abc import Callable
from doctest import testmod
from enum import Enum
from typing import ClassVar


class Mode(Enum):
    """Represents the different address modes of the IntCode machine."""

    POSITION = 0
    IMMEDIATE = 1
    RELATIVE = 2

    def tag(self) -> str:
        """Return a string tag for the address mode for use in disassembly."""
        if self == Mode.POSITION:
            return "p"
        if self == Mode.IMMEDIATE:
            return "i"
        if self == Mode.RELATIVE:
            return "r"
        raise RuntimeError("Unknown mode", self.value)

    @staticmethod
    def modes(instruction_code: int) -> tuple[Mode, Mode, Mode]:
        """Return three addressing modes associated with the instruction code.

        >>> Mode.modes(21002)
        (<Mode.POSITION: 0>, <Mode.IMMEDIATE: 1>, <Mode.RELATIVE: 2>)
        """
        return (
            Mode.mode(instruction_code),
            Mode.mode(instruction_code // 10),
            Mode.mode(instruction_code // 100),
        )

    @staticmethod
    def mode(instruction_code: int) -> Mode:
        """Return one address mode associated with the instruction code."""
        flag: int = instruction_code // 100
        if flag % 10 == Mode.POSITION.value:
            return Mode.POSITION
        if flag % 10 == Mode.IMMEDIATE.value:
            return Mode.IMMEDIATE
        if flag % 10 == Mode.RELATIVE.value:
            return Mode.RELATIVE
        raise RuntimeError("Unknown mode", flag)


class Machine:
    """Machine that runs IntCode."""

    # Special opcode names (used in matching)
    Input_opcode: ClassVar[int] = 3
    Output_opcode: ClassVar[int] = 4
    RelBaseOffset_opcode: ClassVar[int] = 9
    Halt_opcode: ClassVar[int] = 99

    # Opcode names (used for disassembly)
    Opcode_names: ClassVar[dict[int, str]] = {
        1: "add",
        2: "mul",
        Input_opcode: "input",
        Output_opcode: "output",
        5: "jmp",
        6: "jmz",
        7: "les",
        8: "equ",
        9: "rbo",
        Halt_opcode: "halt",
    }

    # Jump opcodes
    Jump_instructions: ClassVar[dict[int, Callable[[int, int], int | None]]] = {
        5: lambda x, y: y if x != 0 else None,
        6: lambda x, y: y if x == 0 else None,
    }

    # Arithmetic opcodes
    Arithmetic_instructions: ClassVar[dict[int, Callable[[int, int], int]]] = {
        1: lambda x, y: x + y,
        2: lambda x, y: x * y,
        7: lambda x, y: x < y,
        8: lambda x, y: x == y,
    }

    def __init__(self, code: list[int], input_vals: list[int] | None = None) -> None:
        self.code: dict[int, int] = dict(enumerate(code))
        self.ip: int = 0
        self.relative_base: int = 0
        self.input_vals: list[int] = [] if input_vals is None else input_vals
        self.output_vals: list[int] = []
        self.pause_after_output = False
        self.pause_before_input = False
        self.paused = False
        self.halted: bool = False

    def _address(self, mode: Mode, x: int) -> int:
        """Return an address in the given mode."""
        if mode == Mode.POSITION:
            return x
        if mode == Mode.RELATIVE:
            return self.relative_base + x
        if mode == Mode.IMMEDIATE:
            raise RuntimeError("Cannot address in immediate mode")
        raise RuntimeError("Unknown mode in _address", mode)

    def _fetch(self, mode: Mode, x: int) -> int:
        """Retrieve a value with the given mode."""
        if mode == Mode.POSITION:
            return self._code(x)
        if mode == Mode.IMMEDIATE:
            return x
        if mode == Mode.RELATIVE:
            return self._code(self.relative_base + x)
        raise RuntimeError("Unknown mode in _fetch", mode)

    def _code(self, x: int) -> int:
        """Retrieve a value from memory, extending memory if needed."""
        if x in self.code:
            return self.code[x]
        if x >= 0:
            self.code[x] = 0
            return 0
        raise RuntimeError("Negative address in _fetch", x)

    def _print(self, comment: str = "") -> None:
        """Print the current instruction and comment."""
        instruction_code: int = self._code(self.ip)
        opcode: int = instruction_code % 100
        if opcode not in self.Opcode_names:
            print("Unknown opcode in _print", opcode)
            return
        instruction_name: str = self.Opcode_names[opcode]
        if opcode == self.Halt_opcode:
            modes: str = ""
            args: str = ""
        elif opcode == self.Input_opcode:
            modes = "_" + Mode.tag(Mode.mode(instruction_code))
            args = str(self._code(self.ip + 1))
        elif opcode in (self.RelBaseOffset_opcode, opcode == self.Output_opcode):
            modes = "_" + Mode.tag(Mode.mode(instruction_code))
            args = str(self._code(self.ip + 1))
        else:
            (mode1, mode2, mode3) = Mode.modes(instruction_code)
            modes = "_" + Mode.tag(mode1) + Mode.tag(mode2) + Mode.tag(mode3)
            args = ", ".join(
                str(x)
                for x in [
                    self._code(self.ip + 1),
                    self._code(self.ip + 2),
                    self._code(self.ip + 3),
                ]
            )
        print(f"{self.ip:4} {instruction_name + modes:8} {args:20} {comment}")

    def step(self, print_instructions: bool = False) -> Machine:
        """Run one instruction."""
        try:
            self._step(print_instructions)
        except:
            self._print("Failed")
            raise
        return self

    def _step(self, print_instructions: bool = False) -> None:
        instruction_code: int = self._code(self.ip)
        opcode: int = instruction_code % 100

        if opcode == self.Input_opcode:
            if self.pause_before_input and not self.input_vals:
                self.paused = True
                return
            mode = Mode.mode(instruction_code)
            if not self.input_vals:
                raise RuntimeError("No input available")
            dest = self._address(mode, self._code(self.ip + 1))
            self.code[dest] = self.input_vals[0]
            if print_instructions:
                self._print(f"input {self.input_vals[0]} -> {dest}")
            del self.input_vals[:1]
            self.ip += 2
            return

        if opcode == self.Output_opcode:
            mode = Mode.mode(instruction_code)
            arg = self._fetch(mode, self._code(self.ip + 1))
            self.output_vals.append(arg)
            if print_instructions:
                self._print(f"output {arg}")
            self.ip += 2
            if self.pause_after_output:
                self.paused = True
            return

        if opcode == self.RelBaseOffset_opcode:
            mode = Mode.mode(instruction_code)
            arg = self._fetch(mode, self._code(self.ip + 1))
            self.relative_base += arg
            if print_instructions:
                self._print(f"offset {arg} -> {self.relative_base}")
            self.ip += 2
            return

        if opcode == self.Halt_opcode:
            self.halted = True
            if print_instructions:
                self._print("halted")
            return

        (mode1, mode2, mode3) = Mode.modes(instruction_code)
        arg1: int = self._fetch(mode1, self._code(self.ip + 1))
        arg2: int = self._fetch(mode2, self._code(self.ip + 2))

        if opcode in self.Jump_instructions:
            jump_value: int | None = self.Jump_instructions[opcode](arg1, arg2)
            if print_instructions:
                self._print(f"Jump to {jump_value}" if jump_value else "Jump skipped")
            self.ip = self.ip + 3 if jump_value is None else jump_value
            return

        if opcode in self.Arithmetic_instructions:
            return_value: int = self.Arithmetic_instructions[opcode](arg1, arg2)
            dest = self._address(mode3, self._code(self.ip + 3))
            if print_instructions:
                self._print(
                    f"{self.Opcode_names[opcode]} {arg1} {arg2} = {return_value} -> {dest}"
                )
            self.code[dest] = return_value
            self.ip += 4
            return

        raise RuntimeError("Unknown opcode in step", opcode)

    def run(self, print_instructions: bool = False) -> Machine:
        """Run until execution stops or a pause from output."""
        self.paused = False
        while not self.halted and not self.paused:
            self.step(print_instructions)
        return self


if __name__ == "__main__":
    testmod()
