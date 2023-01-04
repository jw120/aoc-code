"""Advent of Code 2022 - Day 17."""

# from __future__ import annotations

# from doctest import testmod
from math import lcm

# from sys import stdin
from typing import Optional

# Here we take x left-to-right and y down-to-up
from coord import Coord

# Characterize rocks by the offsets from its bottom-left corner
Rock = list[Coord]

ROCKS: list[Rock] = [
    # @@@@
    [Coord(0, 0), Coord(1, 0), Coord(2, 0), Coord(3, 0)],
    #  @
    # @@@
    #  @
    [Coord(1, 0), Coord(0, 1), Coord(1, 1), Coord(2, 1), Coord(1, 2)],
    #   @
    #   @
    # @@@
    [Coord(0, 0), Coord(1, 0), Coord(2, 0), Coord(2, 1), Coord(2, 2)],
    # @
    # @
    # @
    # @
    [Coord(0, 0), Coord(0, 1), Coord(0, 2), Coord(0, 3)],
    # @@
    # @@
    [Coord(0, 0), Coord(1, 0), Coord(0, 1), Coord(1, 1)],
]

WIDTH: int = 7


class Chamber:
    """Main class for day 17."""

    def __init__(self, s: str) -> None:
        self.max_y: int = -1
        self.occupied: set[Coord] = set()
        assert all(c in "<>" for c in s)
        self.jet_pattern: list[bool] = [c == ">" for c in s]
        self.jen_length: int = len(self.jet_pattern)
        self.jet_count: int = 0
        self.jet_period = lcm(len(ROCKS), len(self.jet_pattern))
        self.memory: dict[int, int] = {}

    @property
    def height(self) -> int:
        """Current height of the tower."""
        return self.max_y + 1

    def drop_multiple(self, n: int) -> None:
        """Drop the given number of rocks.

        >>> chamber = Chamber(TEST_DATA)
        >>> chamber.drop_multiple(2022)
        >>> chamber.height
        3068
        """
        for r in range(n):
            self.drop(r % 5)

    def drop(self, rock_index: int) -> None:
        """Drop the given rock."""
        start_position: Coord = Coord(2, self.max_y + 4)
        rock: Rock = [c + start_position for c in ROCKS[rock_index]]
        while True:
            # Move horizontally if possible
            horizontal_move = (
                Coord(1, 0)
                if self.jet_pattern[self.jet_count % self.jen_length]
                else Coord(-1, 0)
            )
            # print(rock, horizontal_move)
            new_rock: Rock = [c + horizontal_move for c in rock]
            if self.all_valid(new_rock):
                # print("Moved horizontally")
                rock = new_rock
            self.jet_count += 1
            # Move down
            new_rock = [c + Coord(0, -1) for c in rock]
            if not self.all_valid(new_rock):
                for c in rock:
                    self.max_y = max(self.max_y, c.y)
                    self.occupied.add(c)
                break
            rock = new_rock
            if self.jet_count % self.jet_period == 0:
                h = self.make_hash()
                print(self.jet_count // self.jet_period, h)
                if h in self.memory:
                    print("Repeat", self.memory[h])
                    return
                self.memory[h] = self.jet_count // self.jet_period

    def valid(self, c: Coord) -> bool:
        """Test if the coordinate a valid unoccupied space."""
        return c.y >= 0 and c.x >= 0 and c.x < WIDTH and c not in self.occupied

    def all_valid(self, coords: list[Coord]) -> bool:
        """Test if all of the coordinates are valid unoccupied spaces."""
        return all(self.valid(c) for c in coords)

    def row_value(self, y: int) -> int:
        """Return the value of row y interpreting booleans as binary."""
        bin_values = [Coord(x, y) in self.occupied for x in range(WIDTH)]
        res = 0
        for bit in bin_values:
            res = (res << 1) | bit
        return res

    def make_hash(self) -> int:
        """Return a hash of top 10 rows of the chamber."""
        return hash(
            tuple(self.row_value(y) for y in range(self.max_y, self.max_y - 20, -1))
        )

    def show(self, max_rows: Optional[int] = None) -> None:
        """Show debugging information."""
        min_y = -1 if max_rows is None else self.max_y - max_rows + 1
        for y in range(self.max_y + 2, min_y - 1, -1):
            for x in range(-1, 8):
                if x in (-1, 7):
                    print_char = "+" if y == -1 else "|"
                elif y == -1:
                    print_char = "-"
                else:
                    print_char = "." if self.valid(Coord(x, y)) else "#"
                print(print_char, end="")
            print()


TEST_DATA = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

if __name__ == "__main__":
    # testmod()
    # chamber = Chamber(stdin.read().strip())
    chamber = Chamber(TEST_DATA)
    chamber.drop_multiple(2022)
    print(chamber.height)
