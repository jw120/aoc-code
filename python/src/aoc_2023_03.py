"""Advent of Code 2023 - Day 3."""

from dataclasses import dataclass
from doctest import testmod
from sys import stdin
from typing import Iterator

from coord import Coord, Extent


@dataclass
class Number:
    """Represent a number on the grid"""

    value: int
    position: Coord  # position of first digit
    length: int  # number of digits


class Schematic:
    """Main class to hold schematic"""

    def __init__(self, lines: list[str]) -> None:
        # Read grid data
        self.extent: Extent = Extent(x=len(lines[0]), y=len(lines))
        self.data: dict[Coord, str] = {}
        for y, line in enumerate(lines):
            for x, c in enumerate(line):
                self.data[Coord(x, y)] = c
        # Locate symbols
        self.symbols: list[Coord] = []
        for c in self.extent.upto():
            if (not self.data[c].isdigit()) and self.data[c] != ".":
                self.symbols.append(c)
        # Locate numbers
        self.numbers: list[Number] = []
        n: Number | None = None
        for y in range(self.extent.y):
            for x in range(self.extent.x + 1):
                if x == self.extent.x:
                    if n is not None:
                        self.numbers.append(n)
                    n = None
                else:
                    c = Coord(x, y)
                    d = self.data[c]
                    if d.isdigit():
                        if n is None:
                            n = Number(value=int(d), position=c, length=1)
                        else:
                            n.value = n.value * 10 + int(d)
                            n.length += 1
                    else:
                        if n is not None:
                            self.numbers.append(n)
                        n = None

    def gear_ratios(self) -> Iterator[int]:
        """For each star adjacent to two numbers, yield their product"""
        for c in self.extent.upto():
            if self.data[c] == "*":
                adjacent_numbers = [
                    n.value for n in self.numbers if self.is_adjacent(n, c)
                ]
                if len(adjacent_numbers) == 2:
                    yield adjacent_numbers[0] * adjacent_numbers[1]

    def is_adjacent(self, n: Number, c: Coord) -> bool:
        """Is the number adjacent to the given coordinate"""
        return (
            c.x >= n.position.x - 1
            and c.x <= n.position.x + n.length
            and c.y >= n.position.y - 1
            and c.y <= n.position.y + 1
        )


if __name__ == "__main__":
    testmod()
    schematic = Schematic([s.strip() for s in stdin.readlines()])
    print(
        sum(
            n.value
            for n in schematic.numbers
            if any(schematic.is_adjacent(n, s) for s in schematic.symbols)
        )
    )
    print(sum(schematic.gear_ratios()))
