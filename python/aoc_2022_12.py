"""Advent of Code 2022 - Day 12."""

from doctest import testmod

from collections import deque
from dataclasses import dataclass
from sys import stdin
from typing import Callable, Dict, List, Optional, Set, Tuple, TypeVar

from Coord import Coord, Extent


from Coord import Coord


class HeightMap:
    def __init__(self, s: str) -> None:
        s_lines = s.split("\n")

        self.extent = Extent(len(s_lines), len(s_lines[0]))
        self.start = Coord(-1, -1)
        self.goal = Coord(-1, -1)
        self.h: dict[Coord, int] = {}

        for c in self.extent.upto():
            x = s_lines[c.x][c.y]
            if x == "S":
                self.start = c
                x = "a"
            elif x == "E":
                self.goal = c
                x = "z"
            self.h[c] = ord(x) - ord("a") + 1

    def shortest_path_length(self) -> Optional[int]:
        """Return the length of the shortest path from start to goal.

        >>> HeightMap(test_input).shortest_path_length()
        31
        """

        def at_goal(c: Coord) -> bool:
            return c == self.goal

        def available(c: Coord) -> list[Coord]:
            h_max = self.h[c] + 1
            return [a for a in c.adjacents(self.extent) if self.h[a] <= h_max]

        return bfs(self.start, at_goal, available)


S = TypeVar("S")


def bfs(
    start: S, at_goal: Callable[[S], bool], available: Callable[[S], List[S]]
) -> Optional[int]:
    """Conduct basic BFS search and return length of minimum path.

    Recycled from 2019 Day 18.
    """
    # Queue of states to visit
    q: deque[Tuple[S, int]] = deque([(start, 0)])
    # Distances from start for states visited or in queue
    distance: Dict[S, int] = {start: 0}
    while q:
        s, dist = q.popleft()
        #        print("Dequeue", s, dist)
        if at_goal(s):
            return dist
        new_states = [(a, dist + 1) for a in available(s) if a not in distance]
        q.extend(new_states)
        #       print("Enqueue", new_states)
        distance |= dict(new_states)
    return None


test_input = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""

if __name__ == "__main__":
    testmod()
    height_map = HeightMap(stdin.read())
    print(height_map.shortest_path_length())
