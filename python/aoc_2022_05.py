"""Advent of Code 2022 - Day 4."""

from dataclasses import dataclass
from doctest import testmod
from sys import stdin


@dataclass(frozen=True)
class Move:
    number: int
    source: int
    destination: int


def read_move(s: str) -> Move:
    """Read a move from input string.

    >>> read_move("move 1 from 1 to 2")
    Move(number=1, source=1, destination=2)
    """
    assert s.startswith("move "), f"Bad '{s}'"
    number, rest = s.removeprefix("move ").split(" from ")
    source, destination = rest.split(" to ")
    return Move(number=int(number), source=int(source), destination=int(destination))


test_crates = """    [D]____
[N] [C]____
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
"""


class Crates:
    def __init__(self, s: str) -> None:
        """Initialize the crates from the given input.

        >>> Crates(test_crates.replace('_', ' '))
        {1: ZN, 2: MCD, 3: P, [(1, 2, 1), (3, 1, 3)]}
        """
        crate_block, move_block = s.split("\n\n")
        crate_lines = crate_block.split("\n")[:-1]  # Drop row with stack numbers
        # print("crate_lines\n", crate_lines)
        move_lines = move_block.split("\n")
        # print("move_lines\n", move_lines)
        highest_stack = len(crate_lines)
        # print("highest stack", highest_stack)
        self.number_of_stacks: int = (len(crate_lines[0]) + 1) // 4
        # print("num stacks", self.number_of_stacks)
        self.stacks: dict[int, list[str]] = {
            i: [] for i in range(1, self.number_of_stacks + 1)
        }
        for line in crate_lines:
            assert (
                len(line) == self.number_of_stacks * 4 - 1
            ), f"Bad '{line}' {crate_lines}"
        for row in range(0, highest_stack):
            for stack in range(1, self.number_of_stacks + 1):
                crate = crate_lines[-row - 1][stack * 4 - 3]
                # print("row", row, "stack", stack, ":", crate)
                if crate.isalpha():
                    self.stacks[stack].append(crate)
        self.moves: list[Move] = [read_move(line) for line in move_lines if line]

    def __repr__(self) -> str:
        crates = ", ".join(
            f"{i}: {''.join(self.stacks[i])}"
            for i in range(1, self.number_of_stacks + 1)
        )
        moves = str([(m.number, m.source, m.destination) for m in self.moves])
        return "{" + crates + ", " + moves + "}"


if __name__ == "__main__":
    testmod()
    crates = Crates(stdin.read())
    print(repr(crates))
