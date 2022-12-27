"""Advent of Code 2022 - Day 24."""


import fileinput
from dataclasses import dataclass, field
from doctest import testmod
from heapq import heappop, heappush
from itertools import chain
from math import lcm
from typing import Final, Iterable

# x runs left-ro-right, y runs top-to-bottom
from coord import Coord, manhattan


@dataclass(frozen=True)
class Blizzard:
    """Data class for each blizzard"""

    start: Coord
    step: Coord


@dataclass(frozen=True)
class State:
    """State for walk of available paths."""

    location: Coord
    time: int


@dataclass(frozen=True, order=True)
class PrioritizedState:
    """State with priority for A*."""

    state: State = field(compare=False, hash=True)
    priority: int


class Basin:
    """Main class for day 24."""

    def __init__(self, input_lines: Iterable[str], debug: bool = False) -> None:
        self.initial_blizzards: list[Blizzard] = []
        self.cached: dict[State, bool] = {}
        for row, line in enumerate(input_lines, start=-1):
            line = line.strip()
            if row == -1:
                assert all(
                    c == "#" for c in (line[0:1] + line[2:])
                ), f"Bad initial line '{line}'"
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
                            step = Coord(0, -1)
                        case ">":
                            step = Coord(1, 0)
                        case "v":
                            step = Coord(0, 1)
                        case "<":
                            step = Coord(-1, 0)
                        case ".":
                            continue
                        case _:
                            raise ValueError(
                                f"Unknown character '{char}' in line '{line}'"
                            )
                    self.initial_blizzards.append(
                        Blizzard(start=Coord(x=col, y=row), step=step)
                    )
        self.start = Coord(0, -1)
        self.goal = Coord(self.width - 1, self.height)
        self.repeat = lcm(self.width, self.height)
        self.debug = debug
        if self.debug:
            print(
                f"{self.width}x{self.height}: repeat: {self.repeat} max_cache {self.repeat*self.width*self.height}"
            )

    def will_be_empty(self, state: State) -> bool:
        """Test if state will be empty."""
        if state.location.y <= -1 and state.location != self.start:
            return False  # Top wall
        if state.location.y >= self.height and state.location != self.goal:
            return False  # Bottom wall
        if state.location.x in (-1, self.width):
            return False  # Side walls
        wrapped_state = State(location=state.location, time=state.time % self.repeat)
        try:
            return self.cached[wrapped_state]
        except KeyError:
            to_cache = True
            for blizzard in self.initial_blizzards:
                if (
                    blizzard.start.x != state.location.x
                    and blizzard.start.y != state.location.y
                ):
                    continue  # Blizzards only move up/down/left/right
                x = (blizzard.start.x + state.time * blizzard.step.x) % self.width
                y = (blizzard.start.y + state.time * blizzard.step.y) % self.height
                if state.location == Coord(x, y):
                    to_cache = False
                    break
            self.cached[wrapped_state] = to_cache
            return to_cache

    def add_priority(self, state: State) -> PrioritizedState:
        """Add priority to the state.

        Used by the A*-algorithm. Defined as distances from origin
        plus distance to goal (manhattan distances)."""
        return PrioritizedState(
            state=state, priority=state.time + manhattan(state.location, self.goal)
        )

    def path(self, start_location: Coord, goal_location: Coord, start_time: int) -> int:
        """Return length of shortest path with given start time."""
        initial_state: State = State(
            location=start_location,
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
            visited.add(state)
            if state.time > t_max:
                t_max = state.time
                if self.debug and t_max % 10 == 0:
                    print(
                        f"t_max={t_max}, cache size={len(self.cached)}",
                        f"{round(100 * len(self.cached)/(self.width * self.height * self.repeat), 1)}%",
                    )
                    if t_max == 100:
                        return -1
            if state.location == goal_location:
                return state.time
            for adj in chain(state.location.adjacents(), [state.location]):
                # print(f"Considering move from {state.location} to {adj}")
                trial_state = State(location=adj, time=state.time + 1)
                if trial_state not in visited and self.will_be_empty(trial_state):
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
        return self.path(self.start, self.goal, start_time)

    def backward_path(self, start_time: int) -> int:
        """Return length of path from goal to start with given start time.

        >>> Basin(TEST_DATA2).backward_path(18)
        41
        """
        return self.path(self.goal, self.start, start_time)

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


TEST_DATA1: Final[
    list[str]
] = """#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#""".splitlines()


TEST_DATA2: Final[
    list[str]
] = """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#""".splitlines()


if __name__ == "__main__":
    testmod()
    basin = Basin(fileinput.input(), debug=True)
    # basin.show(0)
    TIME1 = basin.forward_path(0)
    # print(TIME1)
    # TIME2 = basin.backward_path(TIME1)
    # TIME3 = basin.forward_path(TIME2)
    # print(TIME3)
