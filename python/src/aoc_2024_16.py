"""Advent of Code 2024 - Day 16."""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum, auto
from heapq import heapify, heappop, heappush
from sys import stdin

from coord import Coord, Extent


class Dir(Enum):
    """Direction of movement."""

    N = auto()
    E = auto()
    S = auto()
    W = auto()

    def delta(self) -> Coord:
        """Return coordinate form."""
        match self:
            case Dir.N:
                return Coord(0, -1)
            case Dir.E:
                return Coord(1, 0)
            case Dir.S:
                return Coord(0, 1)
            case Dir.W:
                return Coord(-1, 0)

    def adjacents(self) -> list[Dir]:
        """Return adjacent directions."""
        match self:
            case Dir.N:
                return [Dir.E, Dir.W]
            case Dir.E:
                return [Dir.S, Dir.N]
            case Dir.S:
                return [Dir.W, Dir.E]
            case Dir.W:
                return [Dir.N, Dir.S]


@dataclass
class State:
    """State for our breadth-first search."""

    distance: int
    coord: Coord
    direction: Dir

    def __lt__(self, other: State) -> bool:
        return self.distance < other.distance


class Maze:
    """Main class."""

    def __init__(self, lines: list[str]) -> None:
        self.extent = Extent(len(lines[0].strip()), len(lines))
        self.wall: list[list[bool]] = []
        start: Coord | None = None
        finish: Coord | None = None
        for y, line in enumerate(lines):
            self.wall.append([])
            assert len(line.strip()) == self.extent.x
            for x, ch in enumerate(line.strip()):
                if ch == "#":
                    self.wall[-1].append(True)
                else:
                    self.wall[-1].append(False)
                    if ch == "S":
                        start = Coord(x, y)
                    elif ch == "E":
                        finish = Coord(x, y)
                    else:
                        assert ch == "."
        assert start is not None
        assert finish is not None
        self.start = start
        self.finish = finish

    def __getitem__(self, coord: Coord) -> bool:
        """Return contents of given space."""
        return self.wall[coord.y][coord.x]

    def bfs(self) -> int | None:
        """Return shortest-path from start to finish."""
        q: list[State] = [State(distance=0, coord=self.start, direction=Dir.E)]
        heapify(q)
        explored: set[tuple[Coord, Dir]] = {(self.start, Dir.E)}

        while q:
            state = heappop(q)
            print(state)
            if state.coord == self.finish:
                return state.distance
            # Move in current direction
            c_next = state.coord + state.direction.delta()
            if not self[c_next] and (c_next, state.direction) not in explored:
                explored.add((c_next, state.direction))
                heappush(q, State(state.distance + 1, c_next, state.direction))
            for d in state.direction.adjacents():
                if (state.coord, d) not in explored:
                    explored.add((state.coord, d))
                    heappush(q, State(state.distance + 1000, state.coord, d))
        return None

    def print(self) -> None:
        """Print warehouse for debugging."""
        for coord in self.extent.upto_by_y():
            if coord == self.start:
                print("S", end="")
            elif coord == self.finish:
                print("E", end="")
            else:
                print("#" if self[coord] else ".", end="")
            if coord.x == self.extent.x - 1:
                print()
        print()


if __name__ == "__main__":
    maze = Maze(stdin.readlines())
    maze.print()
    print(maze.bfs())
