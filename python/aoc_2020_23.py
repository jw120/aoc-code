"""Advent of Code 2020 - Day 23."""

from __future__ import annotations

from copy import copy
from doctest import testmod
from sys import stdin
from typing import List, Tuple


class CrabCups:
    def __init__(self, xs: List[int]) -> None:
        self.cups = copy(xs)
        self.current_value = xs[0]
        self.number_of_cups = len(xs)

    def _print_cups(self) -> None:
        def cup(x: int) -> str:
            if x == self.current_value:
                return "(" + str(x) + ")"
            else:
                return " " + str(x) + " "

        print(f'cups: {"".join(cup(i) for i in self.cups)}')

    def move(self, debug: bool = False) -> CrabCups:
        """Perform one crab move."""

        def take3() -> Tuple[int, int, int]:
            """Remove next 3 values from self.cups and return them."""
            if debug:
                self._print_cups()
            index0: int = self.cups.index(self.current_value)
            index1: int = (index0 + 1) % self.number_of_cups
            index2: int = (index0 + 2) % self.number_of_cups
            index3: int = (index0 + 3) % self.number_of_cups
            value1: int = self.cups[index1]
            value2: int = self.cups[index2]
            value3: int = self.cups[index3]
            if debug:
                print(f"pick up: {value1}, {value2}, {value3}")
            # We can't use index2 or index3 for deletions, as earlier deletes may
            # change the indices
            del self.cups[self.cups.index(value1)]
            del self.cups[self.cups.index(value2)]
            del self.cups[self.cups.index(value3)]
            return (value1, value2, value3)

        def add3(values: Tuple[int, int, int]) -> None:
            """Insert the three values."""
            destination_value = self.current_value - 1
            while destination_value not in self.cups:
                destination_value -= 1
                if destination_value < min(self.cups):
                    destination_value = max(self.cups)
            destination_index: int = self.cups.index(destination_value)
            if debug:
                print(f"destination: {destination_value}")
            self.cups[destination_index + 1 : destination_index + 1] = values

        def update_current() -> None:
            """Update current cup value."""
            current_index: int = self.cups.index(self.current_value)
            next_index: int = (current_index + 1) % self.number_of_cups
            self.current_value = self.cups[next_index]

        add3(take3())
        update_current()
        return self

    def run(self, moves: int, debug: bool = False) -> CrabCups:
        """Perform the given number of moves."""
        for i in range(1, moves + 1):
            if debug:
                print(f"-- move {i} --\n")
            self.move(debug)
        if debug:
            print("-- final --")
            self._print_cups()
        return self

    def order(self) -> str:
        """Return final order of cups.

        >>> CrabCups(test1).run(10).order()
        '92658374'
        >>> CrabCups(test1).run(100).order()
        '67384529'
        """
        index1: int = self.cups.index(1)
        return "".join(
            str(self.cups[(index1 + i) % self.number_of_cups])
            for i in range(1, self.number_of_cups)
        )


test1: List[int] = [int(x) for x in "389125467"]


if __name__ == "__main__":
    testmod()
    starting_cups: List[int] = [int(x) for x in stdin.read().strip()]
    print(CrabCups(starting_cups).run(100).order())
