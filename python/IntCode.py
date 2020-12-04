"""Machine to run IntCode."""

from typing import List


class Machine:
    """Machine that runs IntCode."""

    def __init__(self, code: List[int]) -> None:
        """Create a new IntCode Machine."""
        self.code: List[int] = code.copy()
        self.ip: int = 0

    def step(self) -> bool:
        """Run one instruction, returning true if execution can continue."""
        instruction: int = self.code[self.ip]
        if instruction == 1:  # Add
            input1, input2, output = self.code[self.ip + 1 : self.ip + 4]
            self.code[output] = self.code[input1] + self.code[input2]
            self.ip += 4
            return True
        elif instruction == 2:  # Multiply
            input1, input2, output = self.code[self.ip + 1 : self.ip + 4]
            self.code[output] = self.code[input1] * self.code[input2]
            self.ip += 4
            return True
        elif instruction == 99:  # Halt
            return False
        else:
            raise RuntimeError("Unknown opcode", instruction)

    def run(self) -> None:
        """Run until execution stops."""
        while self.step():
            pass
