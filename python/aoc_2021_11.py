"""Advent of Code 2021 - Day 11."""

from __future__ import annotations

from doctest import testmod
from sys import stdin
from typing import Iterable, Set, Tuple


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
        self.rows = len(self.energy)
        assert self.rows > 0
        self.cols = len(self.energy[0])
        assert all(len(row) == self.cols for row in self.energy)
        self.flash_count = 0
        self.step_count = 0

    def adjacents(self, r: int, c: int) -> Iterable[Tuple[int, int]]:
        """Return all the cells adjacent to the given one (including diagonals)."""
        return (
            (x, y)
            for x in range(r - 1, r + 2)
            for y in range(c - 1, c + 2)
            if (x, y) != (r, c)
            if x >= 0 and y >= 0 and x < self.rows and y < self.cols
        )

    def step(self, n: int = 1, stop_when_all_flash: bool = False) -> Grid:
        """Advance the grid of octopuses by the given number of steps.

        Stop if all the octopuses flash in the same step if the flag is set.

        >>> Grid(test).step(100).flash_count
        1656
        >>> Grid(test).step(200, True).step_count
        195
        """
        for _ in range(n):
            to_flash: list[Tuple[int, int]] = []
            for r in range(self.rows):
                for c in range(self.cols):
                    self.energy[r][c] += 1
                    if self.energy[r][c] > 9:
                        to_flash.append((r, c))
            flashed: Set[Tuple[int, int]] = set()
            while to_flash:
                r, c = to_flash.pop()
                if (r, c) not in flashed:
                    flashed.add((r, c))
                    self.flash_count += 1
                    self.energy[r][c] = 0
                    for nr, nc in self.adjacents(r, c):
                        self.energy[nr][nc] += 1
                        if self.energy[nr][nc] > 9:
                            to_flash.append((nr, nc))
            for r, c in flashed:
                self.energy[r][c] = 0
            self.step_count += 1
            if stop_when_all_flash and len(flashed) == self.rows * self.cols:
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
