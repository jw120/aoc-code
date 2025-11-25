"""Advent of Code 2024 - Day 18."""

from sys import stdin
from typing import TYPE_CHECKING

from coord import Coord, Extent
from search import bfs

if TYPE_CHECKING:
    from collections.abc import Callable


class Memory:
    """Main class."""

    def __init__(self, lines: list[str], size: int) -> None:
        self.extent = Extent(size, size)
        self.corrupt: list[list[int | None]] = [[None for _ in range(size)] for _ in range(size)]
        for i, line in enumerate(lines):
            x, y = [int(s) for s in line.strip().split(",")]
            self[Coord(x, y)] = i

    def __getitem__(self, coord: Coord) -> int | None:
        return self.corrupt[coord.y][coord.x]

    def __setitem__(self, coord: Coord, value: int | None) -> None:
        self.corrupt[coord.y][coord.x] = value

    def is_corrupt(self, coord: Coord, n: int) -> bool:
        """Test if the location considering first n corruptions only."""
        value = self[coord]
        return value is not None and value <= n

    def shortest_path(self, n: int) -> int | None:
        """Return length of shortest path from origin to bottom-right using first n corruptions only."""
        goal = Coord(self.extent.x - 1, self.extent.y - 1)
        return bfs(
            Coord(0, 0),
            lambda c: c == goal,
            lambda c: [c for c in c.adjacents(self.extent) if not self.is_corrupt(c, n)],
        )

    def print(self) -> None:
        """Print memory for debugging."""
        for line in self.corrupt:
            print("".join("#" if x else "." for x in line))
        print()


def lowest_positive(lo: int, hi: int, f: Callable[[int], bool]) -> int:
    """Return lowest value in range lo..hi for which function is True."""
    while hi - lo > 1:
        n = (lo + hi) // 2
        print(lo, n, hi)
        if f(n):
            hi = n
        else:
            lo = n
    return hi


if __name__ == "__main__":
    corruptions = stdin.readlines()
    memory = Memory(corruptions, 71)
    print(memory.shortest_path(1024))
    print(
        corruptions[
            lowest_positive(1024, len(corruptions), lambda n: memory.shortest_path(n) is None)
        ].strip()
    )
