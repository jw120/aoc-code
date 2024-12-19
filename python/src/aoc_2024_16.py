"""Advent of Code 2024 - Day 16.

Works - but not fully general - e.g., if multiple routes reach goal square.

"""

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


@dataclass(frozen=True)
class State:
    """State for our walker."""

    coord: Coord
    direction: Dir


@dataclass
class PriorityState:
    """State for our priority queue."""

    distance: int
    state: State

    def __lt__(self, other: PriorityState) -> bool:
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

    def bfs(self) -> tuple[int, set[Coord]]:
        """Find shortest-path from start to finish.

        Return path length and number of squares on shortest-length paths.
        """
        # Priority queue of states
        start = State(self.start, Dir.E)
        q: list[PriorityState] = [PriorityState(0, start)]
        heapify(q)

        # Explored set that keeps track of distances and parents
        explored: dict[State, tuple[int, set[State]]] = {start: (0, set())}

        while q:
            priority_state = heappop(q)
            distance = priority_state.distance
            state = priority_state.state
            # Check if at finish
            if state.coord == self.finish:
                break
            # Move in current direction
            state_next = State(state.coord + state.direction.delta(), state.direction)
            distance_next = distance + 1
            if not self[state_next.coord]:
                if state_next not in explored:
                    explored[state_next] = (distance_next, {state})
                    heappush(q, PriorityState(distance_next, state_next))
                else:
                    previous_distance, previous_parent = explored[state_next]
                    if previous_distance == distance_next:
                        previous_parent.add(state)
            for d in state.direction.adjacents():
                state_rotate = State(state.coord, d)
                distance_rotate = distance + 1000
                if state_rotate not in explored:
                    explored[state_rotate] = (distance_rotate, {state})
                    heappush(q, PriorityState(distance_rotate, state_rotate))
        else:
            return -1, set()
        backtrack: set[Coord] = set()
        frontier: set[State] = {state}
        while frontier:
            s = frontier.pop()
            backtrack.add(s.coord)
            _distance, parent = explored[s]
            if not parent:
                break
            frontier |= parent
        return distance, backtrack

    def print(self, highlight: set[Coord] | None = None) -> None:
        """Print warehouse for debugging."""
        for coord in self.extent.upto_by_y():
            if coord == self.start:
                print("S", end="")
            elif coord == self.finish:
                print("E", end="")
            elif highlight is not None and coord in highlight:
                print("O", end="")
            else:
                print("#" if self[coord] else ".", end="")
            if coord.x == self.extent.x - 1:
                print()
        print()


if __name__ == "__main__":
    maze = Maze(stdin.readlines())
    distance, path = maze.bfs()
    print(distance)
    print(len(path))
