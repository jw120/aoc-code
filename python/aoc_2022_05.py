"""Advent of Code 2022 - Day 5."""

from dataclasses import dataclass
from doctest import testmod
from sys import stdin


@dataclass(frozen=True)
class Move:
    """Class for a move."""

    number: int
    source: int
    destination: int

    def show(self) -> None:
        """Provide debugging info."""
        print(f"{(self.number, self.source, self.destination)}")


def read_move(s: str) -> Move:
    """Read a move from input string.

    >>> read_move("move 1 from 1 to 2")
    Move(number=1, source=1, destination=2)
    """
    assert s.startswith("move "), f"Bad '{s}'"
    number, rest = s.removeprefix("move ").split(" from ")
    source, destination = rest.split(" to ")
    return Move(number=int(number), source=int(source), destination=int(destination))


Stacks = dict[int, list[str]]


def read_problem(s: str) -> tuple[Stacks, list[Move]]:
    """Read problem and return initial stacks and moves.

    >>> stacks, moves = read_problem(TEST_DATA.replace('_', ' '))
    >>> show_stacks(stacks)
    {1: ZN, 2: MCD, 3: P}
    >>> moves[2]
    Move(number=2, source=2, destination=1)
    """
    stack_block, move_block = s.split("\n\n")
    stack_lines = stack_block.split("\n")[:-1]  # Drop row with stack numbers
    move_lines = move_block.split("\n")
    highest_stack = len(stack_lines)
    number_of_stacks: int = (len(stack_lines[0]) + 1) // 4
    stacks: Stacks = {i: [] for i in range(1, number_of_stacks + 1)}
    for line in stack_lines:
        assert len(line) == number_of_stacks * 4 - 1, f"Bad '{line}' {stack_lines}"
    for row in range(0, highest_stack):
        for stack in range(1, number_of_stacks + 1):
            crate = stack_lines[-row - 1][stack * 4 - 3]
            if crate.isalpha():
                stacks[stack].append(crate)
    moves: list[Move] = [read_move(line) for line in move_lines if line]
    return (stacks, moves)


def apply_moves(stacks: Stacks, moves: list[Move], reverse: bool) -> Stacks:
    """Apply the moves (without mutating).

    >>> stacks, moves = read_problem(TEST_DATA.replace('_', ' '))
    >>> show_stacks(apply_moves(stacks, moves, reverse=True))
    {1: C, 2: M, 3: PDNZ}
    >>> show_stacks(apply_moves(stacks, moves, reverse=False))
    {1: M, 2: C, 3: PZND}
    """
    new_stacks: Stacks = {i: x.copy() for i, x in stacks.items()}
    for move in moves:
        moving = new_stacks[move.source][-move.number :]
        if reverse:
            moving = list(reversed(moving))
        # print("moving", moving)
        new_stacks[move.source][-move.number :] = []
        new_stacks[move.destination] += moving
    return new_stacks


def tops(stacks: Stacks) -> str:
    """Top crate on each stack."""
    return "".join(stacks[i][-1] for i in range(1, len(stacks) + 1))


def show_stacks(stacks: Stacks) -> None:
    """Show stacks for debugging."""
    crates = ", ".join(f"{i}: {''.join(stacks[i])}" for i in range(1, len(stacks) + 1))
    print("{" + crates + "}")


# Test data for doctest (underscores used to preserve trailing spaces)
TEST_DATA = """    [D]____
[N] [C]____
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""


if __name__ == "__main__":
    testmod()
    input_stacks, input_moves = read_problem(stdin.read())
    print(tops(apply_moves(input_stacks, input_moves, reverse=True)))
    print(tops(apply_moves(input_stacks, input_moves, reverse=False)))
