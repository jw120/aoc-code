"""IntCode Machine."""

from __future__ import annotations

from doctest import testmod
from enum import Enum
from typing import Callable, ClassVar, Dict, List, Optional, Tuple


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
    def modes(instruction_code: int) -> Tuple[Mode, Mode, Mode]:
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
    Opcode_names: ClassVar[Dict[int, str]] = {
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
    Jump_instructions: ClassVar[Dict[int, Callable[[int, int], Optional[int]]]] = {
        5: lambda x, y: y if x != 0 else None,
        6: lambda x, y: y if x == 0 else None,
    }

    # Arithmetic opcodes
    Arithmetic_instructions: ClassVar[Dict[int, Callable[[int, int], int]]] = {
        1: lambda x, y: x + y,
        2: lambda x, y: x * y,
        7: lambda x, y: x < y,
        8: lambda x, y: x == y,
    }

    def __init__(self, code: List[int], input_vals: List[int] = []) -> None:
        self.code: Dict[int, int] = dict(enumerate(code))
        self.ip: int = 0
        self.relative_base: int = 0
        self.input_vals: List[int] = input_vals
        self.output_vals: List[int] = []
        self.pause_on_output = False
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
        elif opcode == self.RelBaseOffset_opcode or opcode == self.Output_opcode:
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

    def step(self, print_instructions: bool = False) -> None:
        """Run one instruction."""
        try:
            self._step(print_instructions)
        except:
            self._print("Failed")
            raise

    def _step(self, print_instructions: bool = False) -> None:
        instruction_code: int = self._code(self.ip)
        opcode: int = instruction_code % 100

        if opcode == self.Input_opcode:
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
            jump_value: Optional[int] = self.Jump_instructions[opcode](arg1, arg2)
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

    def run(self, print_instructions: bool = False) -> None:
        """Run until execution stops or a pause from output."""
        paused: bool = False
        old_output_vals: List[int] = self.output_vals.copy()
        while not self.halted and not paused:
            self.step(print_instructions)
            paused = self.pause_on_output and self.output_vals != old_output_vals


if __name__ == "__main__":
    testmod()
    print("Day 2 code")
    with open("input/2019_02.txt", "r") as f:
        code: List[int] = [int(s) for s in f.read().split(",")]
        Machine(code).run(True)

    print("Day 5 code")
    with open("input/2019_05.txt", "r") as f:
        code = [int(s) for s in f.read().split(",")]
        Machine(code, [1]).run(True)

    print("Day 9 tests")
    code = [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]
    m = Machine(code)
    m.run(True)
    print("Test 1 OK" if m.output_vals == code else "Test 1 failed")
    code = [1102, 34915192, 34915192, 7, 4, 7, 99, 0]
    m = Machine(code)
    m.run(True)
    [ans] = m.output_vals
    print("Test 2 OK" if len(str(ans)) == 16 else "Test 2 failed")
    code = [104, 1125899906842624, 99]
    m = Machine(code)
    m.run(True)
    [ans] = m.output_vals
    print("Test 3 OK" if ans == 1125899906842624 else "Test 3 failed")
