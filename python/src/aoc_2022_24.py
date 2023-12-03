"""Advent of Code 2022 - Day 24."""

from __future__ import annotations

from dataclasses import dataclass, field
from doctest import testmod
from heapq import heappop, heappush
from itertools import chain
from math import lcm
from sys import stdin
from typing import TYPE_CHECKING, Final, LiteralString

import numpy as np

# x runs left-to-right, y runs top-to-bottom
from coord import Coord, manhattan

if TYPE_CHECKING:
    from collections.abc import Iterable


@dataclass(frozen=True)
class Blizzard:
    """Data class for each blizzard."""

    start: Coord
    step: Coord


@dataclass(frozen=True)
class State:
    """State for walk of available paths."""

    location: Coord
    time: int

    def wrap_time(self, n: int) -> State:
        """Return state with time modulus n."""
        return State(location=self.location, time=self.time % n)


@dataclass(frozen=True, order=True)
class PrioritizedState:
    """State with priority for A*."""

    state: State = field(compare=False, hash=True)
    priority: int


class Basin:
    """Main class for day 24."""

    def __init__(self, input_lines: Iterable[str], *, debug: bool = False) -> None:
        self.initial_blizzards: list[Blizzard] = []
        for row, raw_line in enumerate(input_lines, start=-1):
            line = raw_line.strip()
            if row == -1:
                assert all(c == "#" for c in (line[0:1] + line[2:])), f"Bad initial line '{line}'"
                assert line[1] == "."
                self.width: int = len(line) - 2
            elif all(c == "#" for c in line[:-2]) and line[-2:] == ".#":
                self.height = row
            else:
                assert line[0] == "#"
                assert line[-1] == "#"
                for col, char in enumerate(line[1:-1]):
                    match char:
                        case "^":
                            self.initial_blizzards.append(
                                Blizzard(start=Coord(x=col, y=row), step=Coord(0, -1))
                            )
                        case ">":
                            self.initial_blizzards.append(
                                Blizzard(start=Coord(x=col, y=row), step=Coord(1, 0))
                            )
                        case "v":
                            self.initial_blizzards.append(
                                Blizzard(start=Coord(x=col, y=row), step=Coord(0, 1))
                            )
                        case "<":
                            self.initial_blizzards.append(
                                Blizzard(start=Coord(x=col, y=row), step=Coord(-1, 0))
                            )
                        case ".":
                            continue
                        case _:
                            raise ValueError(f"Unknown character '{char}' in line '{line}'")
        self.repeat: int = lcm(self.width, self.height)
        self.initial_start = Coord(0, -1)
        self.initial_goal = Coord(self.width - 1, self.height)
        self.start = self.initial_start
        self.goal = self.initial_goal
        self.debug = debug
        self.h_cache = np.full(
            shape=(self.width, self.height, self.width),
            fill_value=False,
            dtype=np.bool_,
        )
        self.v_cache = np.full(
            shape=(self.width, self.height, self.height),
            fill_value=False,
            dtype=np.bool_,
        )
        for b in self.initial_blizzards:
            x = b.start.x
            y = b.start.y
            if b.step.y == 0:
                for t in range(self.width):
                    self.h_cache[(x, y, t)] = True
                    x = (x + b.step.x) % self.width
            else:
                for t in range(self.height):
                    self.v_cache[(x, y, t)] = True
                    y = (y + b.step.y) % self.height
        if self.debug:
            print(f"{self.width}x{self.height}")

    def will_be_empty(self, state: State) -> bool:
        """Test if state will be empty."""
        if state.location in {self.start, self.goal}:
            return True
        if state.location.y <= -1:
            return False  # Top wall
        if state.location.y >= self.height:
            return False  # Bottom wall
        if state.location.x in {-1, self.width}:
            return False  # Side walls
        return not (
            self.h_cache[(state.location.x, state.location.y, state.time % self.width)]
            or self.v_cache[(state.location.x, state.location.y, state.time % self.height)]
        )

    def add_priority(self, state: State) -> PrioritizedState:
        """Add priority to the state.

        Used by the A*-algorithm. Defined as distances from origin
        plus distance to goal (manhattan distances).
        """
        return PrioritizedState(
            state=state,
            priority=state.time + manhattan(state.location, self.goal),
        )

    def path(self, start_time: int) -> int:
        """Return length of shortest path with given start time."""
        initial_state: State = State(
            location=self.start,
            time=start_time,
        )
        heap: list[PrioritizedState] = []
        heappush(heap, self.add_priority(initial_state))
        visited: set[State] = set()
        t_max: int = -1

        while True:
            if not heap:
                raise ValueError("No path found")
            state: State = heappop(heap).state
            # if self.debug:
            #     print(state)
            visited.add(state.wrap_time(self.repeat))
            if state.time > t_max:
                t_max = state.time
                if self.debug and t_max % 10 == 0:
                    print(
                        f"t_max={t_max},",
                    )
                    # if t_max == 100:
                    #     return -1
            if state.location == self.goal:
                return state.time
            for adj in chain(state.location.adjacents(), [state.location]):
                # print(f"Considering move from {state.location} to {adj}")
                trial_state = State(location=adj, time=state.time + 1)
                if trial_state.wrap_time(self.repeat) not in visited and self.will_be_empty(
                    trial_state
                ):
                    heappush(heap, self.add_priority(trial_state))
                    # print("added")

    def forward_path(self, start_time: int) -> int:
        """Return length of shortest path from start to goal with given start time.

        >>> basin = Basin(TEST_DATA2)
        >>> basin.forward_path(0)
        18
        >>> basin.forward_path(41)
        54
        """
        self.start = self.initial_start
        self.goal = self.initial_goal
        return self.path(start_time)

    def backward_path(self, start_time: int) -> int:
        """Return length of path from goal to start with given start time.

        >>> basin = Basin(TEST_DATA2)
        >>> basin.forward_path(0)
        18
        >>> basin.backward_path(18)
        41
        """
        self.start = self.initial_goal
        self.goal = self.initial_start
        return self.path(start_time)

    def show(self, t: int) -> None:
        """Print debugging information for time t."""
        print("#." + "#" * self.width)
        for row in range(self.height):
            print("#", end="")
            for col in range(self.width):
                state = State(location=Coord(col, row), time=t)
                print("." if self.will_be_empty(state) else "*", end="")
            print("#")
        print("#" * self.width + ".#")


TEST_DATA1: Final[list[LiteralString]] = """#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#""".splitlines()


TEST_DATA2: Final[list[LiteralString]] = """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#""".splitlines()


if __name__ == "__main__":
    testmod()
    basin = Basin(stdin.readlines(), debug=True)
    TIME1 = basin.forward_path(0)
    print(TIME1)
    TIME2 = basin.backward_path(TIME1)
    TIME3 = basin.forward_path(TIME2)
    print(TIME3)
