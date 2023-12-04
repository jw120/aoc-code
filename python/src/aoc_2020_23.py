"""Advent of Code 2020 - Day 23."""

from __future__ import annotations

from doctest import testmod
from sys import stdin


class CrabCups:
    """Main class for day 23."""

    def __init__(self, xs: list[int], extend: int | None = None) -> None:
        self.current_value: int = xs[0]
        self.next: dict[int, int] = {}
        for i in range(len(xs) - 1):
            self.next[xs[i]] = xs[i + 1]
        if extend is None:
            self.num_values: int = len(xs)
            self.next[xs[len(xs) - 1]] = xs[0]
        else:
            self.num_values = extend
            self.next[xs[-1]] = len(xs) + 1
            for j in range(len(xs) + 1, extend):
                self.next[j] = j + 1
            self.next[extend] = xs[0]

    def print_cups(self) -> None:
        """Show debugging information."""

        def cup(x: int) -> str:
            if x == self.current_value:
                return "(" + str(x) + ")"
            return " " + str(x) + " "

        print("cups: ", end="")
        val: int = self.current_value
        while True:
            print(cup(val), end="")
            val = self.next[val]
            if val == self.current_value:
                print()
                break

    def move(self, *, debug: bool = False) -> CrabCups:
        """Perform one crab move."""
        if debug:
            self.print_cups()

        # Remove the next 3 values
        next1: int = self.next[self.current_value]
        next2: int = self.next[next1]
        next3: int = self.next[next2]
        self.next[self.current_value] = self.next[next3]
        if debug:
            print(f"pick up: {next1} {next2} {next3}")

        # Find a destination
        destination: int = self.current_value - 1 if self.current_value > 1 else self.num_values
        while destination in {next1, next2, next3}:
            destination = destination - 1 if destination > 1 else self.num_values
        if debug:
            print(f"destination {destination}")

        # Insert the 3 values after destination
        after_dest: int = self.next[destination]
        self.next[destination] = next1
        self.next[next3] = after_dest

        # Advance current position
        self.current_value = self.next[self.current_value]

        return self

    def run(self, moves: int, *, debug: bool = False) -> CrabCups:
        """Perform the given number of moves."""
        for i in range(1, moves + 1):
            if debug:
                print(f"-- move {i} --\n")
            self.move(debug=debug)
        if debug:
            print("-- final --")
            self.print_cups()
        return self

    def order(self) -> str:
        """Return final order of cups for part one answer.

        >>> CrabCups(test1).run(10).order()
        '92658374'
        >>> CrabCups(test1).run(100).order()
        '67384529'
        """
        output: str = ""
        val = self.next[1]
        while val != 1:
            output += str(val)
            val = self.next[val]
        return output

    def stars(self) -> int:
        """Find stars for part two answer."""
        return self.next[1] * self.next[self.next[1]]


test1: list[int] = [int(x) for x in "389125467"]


if __name__ == "__main__":
    testmod()
    starting_cups: list[int] = [int(x) for x in stdin.read().strip()]
    print(CrabCups(starting_cups).run(100).order())
    print(CrabCups(starting_cups, 1_000_000).run(10_000_000).stars())
