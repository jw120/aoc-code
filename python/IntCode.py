"""IntCode Machine."""

from __future__ import annotations

from doctest import testmod
from enum import Enum
from typing import ClassVar, Dict, List, Optional, Tuple


class Instruction:
    """Represents an element of the IntCode Machine's instruction set.

    Used via its subclasses which represent specific instructions. Not
    intended to be instantiated (just used as classes)

    Each instruction takes two arguments and returns one (which may be
    None) unless it is a special case.

    is_input    If true, calling machine should use its external input
                as the return value from the instruction, rather than
                calling execute()

    is_output   If true, calling machine should pass the argument to
                its external output, rather than calling execute()

    is_halt     If true, calling machine should halt rather than calling
                excute()

    In addition one flag determines how the calling machine should interpret
    the return value from execute()

    is_jump     If true, the calling machine should interpret a non-None
                return value as the new instruction pointer

    Finally, disassembly_name and dissamebly_params are used when disassembling the
    instruction. disassembly_params is the number of parameters shown
    """

    opcode: ClassVar[int]

    disassembly_name: ClassVar[str]
    disassembly_params: ClassVar[int]

    is_input: ClassVar[bool] = False
    is_output: ClassVar[bool] = False
    is_halt: ClassVar[bool] = False
    is_jump: ClassVar[bool] = False

    @classmethod
    def execute(self, arg1: int, arg2: int) -> Optional[int]:
        raise RuntimeError("Called base instruction execute")


class Add_Ins(Instruction):
    opcode: ClassVar[int] = 1
    disassembly_name: ClassVar[str] = "add"
    disassembly_params: ClassVar[int] = 3

    @classmethod
    def execute(self, x: int, y: int) -> Optional[int]:
        return x + y


class Mul_Ins(Instruction):
    opcode: ClassVar[int] = 2
    disassembly_name: ClassVar[str] = "mul"
    disassembly_params: ClassVar[int] = 3

    @classmethod
    def execute(self, x: int, y: int) -> Optional[int]:
        return x * y


class Inp_Ins(Instruction):
    opcode: ClassVar[int] = 3
    disassembly_name: ClassVar[str] = "inp"
    disassembly_params: ClassVar[int] = 1
    is_input: ClassVar[bool] = True


class Out_Ins(Instruction):
    opcode: ClassVar[int] = 4
    disassembly_name: ClassVar[str] = "out"
    disassembly_params: ClassVar[int] = 1
    is_output: ClassVar[bool] = True


class Jmp_Ins(Instruction):
    opcode: ClassVar[int] = 5
    disassembly_name: ClassVar[str] = "jmp"
    disassembly_params: ClassVar[int] = 3
    is_jump: ClassVar[bool] = True

    @classmethod
    def execute(self, cond: int, target: int) -> Optional[int]:
        if cond != 0:
            return target
        return None


class Jmz_Ins(Instruction):
    opcode: ClassVar[int] = 6
    disassembly_name: ClassVar[str] = "jmz"
    disassembly_params: ClassVar[int] = 3
    is_jump: ClassVar[bool] = True

    @classmethod
    def execute(self, cond: int, target: int) -> Optional[int]:
        if cond == 0:
            return target
        return None


class Les_Ins(Instruction):
    opcode: ClassVar[int] = 7
    disassembly_name: ClassVar[str] = "les"
    disassembly_params: ClassVar[int] = 3

    @classmethod
    def execute(self, x: int, y: int) -> Optional[int]:
        return x < y


class Equ_Ins(Instruction):
    opcode: ClassVar[int] = 8
    disassembly_name: ClassVar[str] = "equ"
    disassembly_params: ClassVar[int] = 3

    @classmethod
    def execute(self, x: int, y: int) -> Optional[int]:
        return x == y


class Halt_Ins(Instruction):
    opcode: ClassVar[int] = 99
    disassembly_name: ClassVar[str] = "halt"
    disassembly_params: ClassVar[int] = 0
    is_halt: ClassVar[bool] = True


class Mode(Enum):
    """Represents the different address modes of the IntCode machine."""

    POSITION = 0
    IMMEDIATE = 1

    def tag(self) -> str:
        """Return a string tag for the address mode for use in disassembly."""
        if self == Mode.POSITION:
            return "p"
        if self == Mode.IMMEDIATE:
            return "i"
        raise RuntimeError("Unknown mode", self.value)

    @staticmethod
    def modes(instruction_code: int) -> Tuple[Mode, Mode]:
        """Return two addressing modes associated with the instruction code.

        >>> Mode.modes(1002)
        (<Mode.POSITION: 0>, <Mode.IMMEDIATE: 1>)
        """
        return (Mode.mode(instruction_code), Mode.mode(instruction_code // 10))

    @staticmethod
    def mode(instruction_code: int) -> Mode:
        """Return one address mode associated with the instruction code."""
        flag: int = instruction_code // 100
        if flag % 10 == Mode.POSITION.value:
            return Mode.POSITION
        if flag % 10 == Mode.IMMEDIATE.value:
            return Mode.IMMEDIATE
        raise RuntimeError("Unknown mode", flag)


class Machine:
    """Machine that runs IntCode."""

    def __init__(self, code: List[int], input_val: Optional[int] = None) -> None:
        self.code: List[int] = code.copy()
        self.ip: int = 0
        self.input_val: Optional[int] = input_val
        self.output_vals: List[int] = []
        self.halted: bool = False
        self.instruction_set: Dict[int, Instruction] = {
            ins.opcode: ins() for ins in Instruction.__subclasses__()
        }

    def _decode(self, opcode: int) -> Instruction:
        """Lookup an opcode ."""
        if opcode in self.instruction_set:
            return self.instruction_set[opcode]
        raise RuntimeError("Unknown opcode", opcode)

    def _fetch(self, mode: Mode, x: int) -> int:
        """Retrieve a value with the given mode."""
        if mode == Mode.POSITION:
            return self.code[x]
        if mode == Mode.IMMEDIATE:
            return x
        return 0

    def _print(self, comment: str = "") -> None:
        """Print the current instruction and comment."""
        instruction_code: int = self.code[self.ip]
        instruction: Instruction = self._decode(instruction_code % 100)
        (mode1, mode2) = Mode.modes(instruction_code)
        modes: str = (
            "_" + Mode.tag(mode1) + Mode.tag(mode2)
            if instruction.disassembly_params > 0
            else ""
        )
        name: str = instruction.disassembly_name + modes
        args = ", ".join(
            str(x)
            for x in self.code[
                self.ip + 1 : self.ip + 1 + instruction.disassembly_params
            ]
        )
        print(f"{self.ip:4} {name:6} {args:20} {comment}")

    def step(self, quiet: bool = True) -> None:
        """Run one instruction."""
        try:
            self._step(quiet)
        except:
            self._print("Failed")
            raise

    def _step(self, quiet: bool = True) -> None:
        instruction_code: int = self.code[self.ip]
        instruction: Instruction = self._decode(instruction_code % 100)

        if instruction.is_input:
            if self.input_val is None:
                raise RuntimeError("No input available")
            self.code[self.code[self.ip + 1]] = self.input_val
            if not quiet:
                self._print(f"input {self.input_val}")
            self.input_val = None
            self.ip += 2
            return

        if instruction.is_output:
            mode = Mode.mode(instruction_code)
            arg = self._fetch(mode, self.code[self.ip + 1])
            self.output_vals.append(arg)
            if not quiet:
                self._print(f"output {arg}")
            self.ip += 2
            return

        if instruction.is_halt:
            self.halted = True
            if not quiet:
                self._print("halted")
            return

        # Now we have either a jump or a normal instruction (add/mul/etc)
        (mode1, mode2) = Mode.modes(instruction_code)
        arg1 = self._fetch(mode1, self.code[self.ip + 1])
        arg2 = self._fetch(mode2, self.code[self.ip + 2])
        return_value: Optional[int] = instruction.execute(arg1, arg2)

        if instruction.is_jump:
            if not quiet:
                self._print()
            self.ip = self.ip + 3 if return_value is None else return_value
        else:
            if return_value is None:
                raise RuntimeError("None returned")
            if not quiet:
                self._print(
                    f"{arg1} {arg2} = {return_value} -> {self.code[self.ip + 3]}"
                )
            self.code[self.code[self.ip + 3]] = return_value
            self.ip += 4

    def run(self, quiet: bool = True) -> None:
        """Run until execution stops."""
        while not self.halted:
            self.step(quiet)


if __name__ == "__main__":
    testmod()
    print("Day 2 code")
    with open("input/2019_02.txt", "r") as f:
        code: List[int] = [int(s) for s in f.read().split(",")]
        Machine(code).run(False)

    print("Day 5 code")
    with open("input/2019_05.txt", "r") as f:
        code = [int(s) for s in f.read().split(",")]
        Machine(code, 1).run(False)
