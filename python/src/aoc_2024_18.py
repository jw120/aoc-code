"""Advent of Code 2024 - Day 18."""

from collections import deque
from collections.abc import Callable
from sys import stdin
from typing import TypeVar

from coord import Coord, Extent
from search import bfs

S = TypeVar("S")


# def bfs(start: S, at_goal: Callable[[S], bool], available: Callable[[S], list[S]]) -> int | None:
#     """Conduct basic BFS search and return length of minimum path."""
#     q: deque[tuple[S, int]] = deque([(start, 0)])
#     distance: dict[S, int] = {start: 0}
#     while q:
#         s, dist = q.popleft()
#         if at_goal(s):
#             return dist
#         new_states = [(a, dist + 1) for a in available(s) if a not in distance]
#         q.extend(new_states)
#         distance |= dict(new_states)
#     return None


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


if __name__ == "__main__":
    corruptions = stdin.readlines()
    memory = Memory(corruptions, 71)
    print(memory.shortest_path(1024))
    for n, c in enumerate(corruptions):
        print(n, c.strip())
        if memory.shortest_path(n) is None:
            break


# 3039 31,22  ANSWER

# 3040 50,24
