"""Advent of Code 2021 - Day 11."""

from __future__ import annotations

from doctest import testmod
from sys import stdin
from typing import Set

from coord import Coord, Extent

test: str = (
    "5483143223\n"
    "2745854711\n"
    "5264556173\n"
    "6141336146\n"
    "6357385478\n"
    "4167524645\n"
    "2176841721\n"
    "6882881134\n"
    "4846848554\n"
    "5283751526\n"
)


class Grid:
    def __init__(self, s: str) -> None:
        self.energy: list[list[int]] = [
            [int(c) for c in line] for line in s.splitlines()
        ]
        self.extent = Extent(len(self.energy), len(self.energy[0]))
        assert self.extent.x > 0
        assert all(len(row) == self.extent.y for row in self.energy)
        self.flash_count = 0
        self.step_count = 0

    def step(self, n: int = 1, stop_when_all_flash: bool = False) -> Grid:
        """Advance the grid of octopuses by the given number of steps.

        Stop if all the octopuses flash in the same step if the flag is set.

        >>> Grid(test).step(100).flash_count
        1656
        >>> Grid(test).step(200, True).step_count
        195
        """
        for _ in range(n):
            to_flash: list[Coord] = []
            for c in self.extent.upto():
                self.energy[c.x][c.y] += 1
                if self.energy[c.x][c.y] > 9:
                    to_flash.append(c)
            flashed: Set[Coord] = set()
            while to_flash:
                c = to_flash.pop()
                if c not in flashed:
                    flashed.add(c)
                    self.flash_count += 1
                    self.energy[c.x][c.y] = 0
                    for nc in c.adjacents_with_diagonals(self.extent):
                        self.energy[nc.x][nc.y] += 1
                        if self.energy[nc.x][nc.y] > 9:
                            to_flash.append(nc)
            for c in flashed:
                self.energy[c.x][c.y] = 0
            self.step_count += 1
            if stop_when_all_flash and len(flashed) == self.extent.size:
                return self
        return self

    def show(self) -> None:
        """Print a simple representation fo the grid (for debugging)."""
        print(f"Steps: {self.step_count}, flashes: {self.flash_count}")
        for row in self.energy:
            print("".join(str(c) for c in row))
        print()


if __name__ == "__main__":
    testmod()
    input_data = stdin.read()
    print(Grid(input_data).step(100).flash_count)
    print(Grid(input_data).step(1000, True).step_count)
