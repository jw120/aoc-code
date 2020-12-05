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

    def disassamble(self) -> None:
        """Print a text representation of the code to stdout."""
        i: int = 0
        while i < len(self.code):
            instruction: int = self.code[i]
            if instruction == 1:
                print(
                    f"{i:3} add {self.code[i + 1]}, {self.code[i + 2]}, {self.code[i + 3]}"
                )
                i += 4
            elif instruction == 2:
                print(
                    f"{i:3} mul {self.code[i + 1]}, {self.code[i + 2]}, {self.code[i + 3]}"
                )
                i += 4
            elif instruction == 99:
                print(f"{i:3} halt")
                i += 1
                print(f"{i:3} data", code[i + 1 :])
                break
            else:
                raise RuntimeError("Unknown opcode", instruction)


if __name__ == "__main__":
    print("Day 2 code")
    with open("input/2019_02.txt", "r") as f:
        code: List[int] = [int(s) for s in f.read().split(",")]
        Machine(code).disassamble()
