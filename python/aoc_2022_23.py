"""Advent of Code 2022 - Day 23."""

from __future__ import annotations

import fileinput
from doctest import testmod
from enum import Enum
from typing import Final, Iterable, Optional

# We use x as left->right and y as top->bottom
from coord import Coord
from utils import assert_never


class MoveDirection(Enum):
    """MoveDirections for elves to move."""

    N = 0
    S = 1
    W = 2
    E = 3

    def next(self) -> MoveDirection:
        """Return next direction.

        >>> MoveDirection.S.next()
        <MoveDirection.W: 2>
        """
        return MoveDirection((self.value + 1) % 4)

    def coord(self) -> Coord:
        """Return coordinate form.

        >>> MoveDirection.S.coord()
        Coord(x=0, y=1)
        """
        match self:
            case MoveDirection.N:
                return Coord(0, -1)
            case MoveDirection.S:
                return Coord(0, 1)
            case MoveDirection.W:
                return Coord(-1, 0)
            case MoveDirection.E:
                return Coord(1, 0)
            case _:
                assert_never(self)

    def all_directions(self) -> Iterable[MoveDirection]:
        """Return all four directions in order starting from this one.

        >>> list(MoveDirection.E.all_directions())
        [<MoveDirection.E: 3>, <MoveDirection.N: 0>, <MoveDirection.S: 1>, <MoveDirection.W: 2>]
        """
        return (MoveDirection((self.value + i) % 4) for i in range(4))

    def offsets(self) -> Iterable[Coord]:
        """Return the three adjacent coordinates in the given direction.

        >>> list(MoveDirection.E.offsets())
        [Coord(x=1, y=-1), Coord(x=1, y=0), Coord(x=1, y=1)]
        """
        match self:
            case MoveDirection.N:
                return (Coord(x, -1) for x in range(-1, 2))
            case MoveDirection.S:
                return (Coord(x, 1) for x in range(-1, 2))
            case MoveDirection.W:
                return (Coord(-1, y) for y in range(-1, 2))
            case MoveDirection.E:
                return (Coord(1, y) for y in range(-1, 2))
            case _:
                assert_never(self)


class Diffuser:
    """Main class for day 23."""

    def __init__(self, lines: Iterable[str]) -> None:
        self.positions: set[Coord] = set()  # Elf positions
        self.first_direction: MoveDirection = MoveDirection.N

        for row, line in enumerate(lines):
            for col, char in enumerate(line.strip()):
                if char == "#":
                    self.positions.add(Coord(col, row))
                else:
                    assert char == ".", f"Unexpected input: '{char}'"

    def steps(self, n: int) -> None:
        """Execute given number of steps."""
        for _ in range(n):
            self.step()

    def step(self) -> bool:
        """Update positions the given number of steps.

        Return true if any elf moved."""
        proposals: dict[Coord, Optional[Coord]] = {}
        moved = False
        for elf in self.positions:
            # Don't consider moving if all adjacent positions are empty
            if all(x not in self.positions for x in elf.adjacents_with_diagonals()):
                continue
            for d in self.first_direction.all_directions():
                if all((elf + offset) not in self.positions for offset in d.offsets()):
                    elf_new = elf + d.coord()
                    if elf_new in proposals:
                        proposals[elf_new] = None
                    else:
                        proposals[elf_new] = elf
                    break
        for elf_to, elf_from in proposals.items():
            if elf_from is not None:
                self.positions.add(elf_to)
                self.positions.remove(elf_from)
                moved = True
        self.first_direction = self.first_direction.next()
        return moved

    def moves_until_no_move(self) -> int:
        """Return number of moves until a step when no elf moves.

        >>> test_diffuser = Diffuser(TEST_DATA2)
        >>> test_diffuser.moves_until_no_move()
        20
        """
        move_count = 1
        while self.step():
            move_count += 1
        return move_count

    def rectangle(self) -> tuple[Coord, Coord]:
        """Return min and max corners of rectangle including all positions.

        >>> Diffuser(TEST_DATA1).rectangle()
        (Coord(x=2, y=1), Coord(x=3, y=4))
        """
        return (
            Coord(
                x=min(p.x for p in self.positions), y=min(p.y for p in self.positions)
            ),
            Coord(
                x=max(p.x for p in self.positions), y=max(p.y for p in self.positions)
            ),
        )

    def score(self) -> int:
        """Return number of empty tiles in spanning rectangle.

        >>> Diffuser(TEST_DATA1).score()
        3
        >>> test_diffuser = Diffuser(TEST_DATA2)
        >>> test_diffuser.steps(10)
        >>> test_diffuser.score()
        110
        """
        p_min, p_max = self.rectangle()
        count = 0
        for x in range(p_min.x, p_max.x + 1):
            for y in range(p_min.y, p_max.y + 1):
                if Coord(x, y) not in self.positions:
                    count += 1
        return count

    def show(self) -> None:
        """Print debugging information."""
        p_min, p_max = self.rectangle()
        for row in range(p_min.y, p_max.y + 1):
            for col in range(p_min.x, p_max.x + 1):
                print("#" if Coord(col, row) in self.positions else ".", end="")
            print()


TEST_DATA1: Final[
    list[str]
] = """.....
..##.
..#..
.....
..##.
.....""".splitlines()

TEST_DATA2: Final[
    list[str]
] = """..............
..............
.......#......
.....###.#....
...#...#.#....
....#...##....
...#.###......
...##.#.##....
....#..#......
..............
..............
..............""".splitlines()


if __name__ == "__main__":
    testmod()
    diffuser = Diffuser(fileinput.input())
    diffuser.steps(10)
    print(diffuser.score())
    print(diffuser.moves_until_no_move() + 10)
