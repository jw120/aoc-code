"""Advent of Code 2024 - Day 17."""

from sys import stdin


class Computer:
    """Main class."""

    def __init__(self, lines: list[str]) -> None:
        self.a: int = int(lines[0].removeprefix("Register A: "))
        self.b: int = int(lines[1].removeprefix("Register B: "))
        self.c: int = int(lines[2].removeprefix("Register C: "))
        self.program = [int(x) for x in lines[4].removeprefix("Program: ").strip().split(",")]
        self.ip: int = 0
        self.output: list[int] = []

    def reset(self) -> None:
        """Reset registers, ip and output."""
        self.a = 0
        self.b = 0
        self.c = 0
        self.ip = 0
        self.output = []

    def run(self, ip: int | None = None) -> None:
        """Run instructions, optionally setting the ip."""
        if ip is not None:
            self.ip = ip
        while self.ip < len(self.program) - 1:
            self.step()

    def step(self) -> None:
        """Run one instruction."""
        opcode, operand_code = self.program[self.ip : self.ip + 2]

        # Most instructions interpret operand as a combo operand
        if opcode in {1, 3}:
            operand = operand_code
        else:
            assert operand_code >= 0 and operand_code < 7, f"Bad combo operand code: {operand_code}"
            if operand_code <= 3:
                operand = operand_code
            elif operand_code == 4:
                operand = self.a
            elif operand_code == 5:
                operand = self.b
            else:
                operand = self.c

        print(
            f"A={self.a}, B={self.b}, C={self.c}, ip={self.ip} ({opcode}, {operand_code}->{operand})",
            end=" ",
        )

        ip_next = None
        match opcode:
            case 0:  # adv
                print(f"adv {operand}: a //= {2**operand}")
                self.a //= 2**operand
            case 1:  # bxl
                print(f"bxl {operand}: b ^= {operand}")
                self.b ^= operand
            case 2:  # bst
                print(f"bst: b = {operand} % 8")
                self.b = operand % 8
            case 3:  # jnz
                print(f"jnz {operand}", "jumping" if self.a != 0 else "")
                if self.a != 0:
                    ip_next = operand
            case 4:  # bxc: b ^= c
                print("bxc: b ^= c")
                self.b ^= self.c
            case 5:  # out
                print(f"out: {operand} % 8 =", operand % 8)
                self.output.append(operand % 8)
            case 6:  # bdv
                print(f"cdv {operand}: b = a // {2**operand}")
                self.b = self.a // 2**operand
            case 7:  # cdv
                print(f"cdv {operand}: c = a // {2**operand}")
                self.c = self.a // 2**operand
            case _:
                raise ValueError(f"Bad opcode {opcode}")

        self.ip = self.ip + 2 if ip_next is None else ip_next


def tests(x: Computer) -> None:
    """Run example tests."""
    # If register C contains 9, the program 2,6 would set register B to 1.
    x.reset()
    x.c = 9
    x.program = [2, 6]
    x.run()
    assert x.b == 1, f"b is {x.b}"

    # If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2
    x.reset()
    x.a = 10
    x.program = [5, 0, 5, 1, 5, 4]
    x.run()
    assert x.output == [0, 1, 2]

    # If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
    x.reset()
    x.a = 2024
    x.program = [0, 1, 5, 4, 3, 0]
    x.run()
    assert x.output == [4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0]
    assert x.a == 0

    # If register B contains 29, the program 1,7 would set register B to 26.
    x.reset()
    x.b = 29
    x.program = [1, 7]
    x.run()
    assert x.b == 26

    # If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.
    x.reset()
    x.b = 2024
    x.c = 43690
    x.program = [4, 0]
    x.run()
    assert x.b == 44354


if __name__ == "__main__":
    computer = Computer(stdin.readlines())
    computer.run()
    print(",".join(str(i) for i in computer.output))
