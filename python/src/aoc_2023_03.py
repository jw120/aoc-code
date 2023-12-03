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
        self.extent: Extent = Extent(x=len(lines[0]), y=len(lines))
        self.data: dict[Coord, str] = {}
        for y, line in enumerate(lines):
            for x, c in enumerate(line):
                self.data[Coord(x, y)] = c

    def numbers(self) -> Iterator[Number]:
        """Yield all the numbers."""
        n: Number | None = None
        for y in range(self.extent.y):
            for x in range(self.extent.x + 1):
                if x == self.extent.x:
                    if n is not None:
                        yield n
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
                            yield n
                        n = None

    def is_symbol(self, c: Coord) -> bool:
        """Is the given coordinate a symbol"""
        return not (self.data[c].isdigit() or self.data[c] == ".")

    def is_symbol_adjacent(self, n: Number) -> bool:
        """Is the number adjacent to a symbol on the grid?"""
        if n.position.y > 0:
            for x in range(
                max(0, n.position.x - 1),
                min(self.extent.x, n.position.x + n.length + 1),
            ):
                if self.is_symbol(Coord(x, n.position.y - 1)):
                    return True
        if n.position.y < self.extent.y - 1:
            for x in range(
                max(0, n.position.x - 1),
                min(self.extent.x, n.position.x + n.length + 1),
            ):
                if self.is_symbol(Coord(x, n.position.y + 1)):
                    return True
        if n.position.x > 0 and self.is_symbol(Coord(n.position.x - 1, n.position.y)):
            return True
        if n.position.x + n.length < self.extent.x and self.is_symbol(
            Coord(n.position.x + n.length, n.position.y)
        ):
            return True
        return False


if __name__ == "__main__":
    testmod()
    schematic = Schematic([s.strip() for s in stdin.readlines()])
    print(sum(n.value for n in schematic.numbers() if schematic.is_symbol_adjacent(n)))
