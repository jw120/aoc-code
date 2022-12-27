"""Advent of Code 2022 - Day 24."""


from dataclasses import dataclass, field

from doctest import testmod
import fileinput
from heapq import heappush, heappop
from itertools import chain
from typing import Final, Iterable

# x runs left-ro-right, y runs top-to-bottom
from coord import Coord, manhattan


@dataclass(frozen=True)
class Blizzard:
    """Data class for each blizzard"""

    start: Coord
    step: Coord


@dataclass(frozen=True, order=True)
class State:
    """State for walk of available paths.

    Ordering (for A*) is on priority only
    """

    location: Coord = field(compare=False)
    time: int = field(compare=False)
    priority: int


class Basin:
    """Main class for day 24."""

    def __init__(self, input_lines: Iterable[str]) -> None:
        self.initial_blizzards: list[Blizzard] = []
        self.cached_blizzards: dict[int, set[Coord]] = {}
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

    def will_be_empty(self, c: Coord, t: int) -> int:
        """Test if coordinate will be empty after given number of minutes."""
        if c.y <= -1 and c != self.start:  # Top wall
            return False
        if c.y >= self.height and c != self.goal:  # Bottom wall
            return False
        if c.x in (-1, self.width):  # Side walls
            return False
        if t in self.cached_blizzards:
            b = self.cached_blizzards[t]
        else:
            print(t)
            b = set()
            for blizzard in self.initial_blizzards:
                x = (blizzard.start.x + t * blizzard.step.x) % self.width
                y = (blizzard.start.y + t * blizzard.step.y) % self.height
                b.add(Coord(x, y))
            self.cached_blizzards[t] = b
        return c not in b

    def priority(self, c: Coord, t: int) -> int:
        """Return the priority of the given coordinate.

        Used by the A*-algorithm. Defined as distances from origin
        plus distance to goal (manhattan distances)."""
        return t + manhattan(c, self.goal)

    def find_path(self) -> int:
        """Return length of shortest path to goal.

        >>> Basin(TEST_DATA2).find_path()
        18
        """
        initial_state = State(
            location=self.start,
            time=0,
            priority=self.priority(self.start, 0),
        )
        heap: list[State] = []
        heappush(heap, initial_state)
        visited: set[tuple[Coord, int]] = set()

        while True:
            if not heap:
                raise ValueError("No path found")
            state = heappop(heap)
            visited.add((state.location, state.time))
            # print(
            #     f"p={state.priority} t={state.time}: ({state.location.x}, {state.location.y}), {len(visited)} heap: {len(heap)}",
            #     [(c.x, c.y, t) for c, t in visited],
            # )
            # print(
            #     f"({state.location.x}, {state.location.y}), {len(visited)} heap: {len(heap)}"
            # )
            if state.location == self.goal:
                return state.time
            for adj in chain(state.location.adjacents(), [state.location]):
                # print(f"Considering move from {state.location} to {adj}")
                if (adj, state.time + 1) not in visited and self.will_be_empty(
                    adj, state.time + 1
                ):
                    heappush(
                        heap,
                        State(
                            location=adj,
                            time=state.time + 1,
                            # visited=state.visited | frozenset([state.location]),
                            priority=self.priority(adj, state.time + 1),
                        ),
                    )
                    # print("added")

    def show(self, t: int) -> None:
        """Print debugging information for time t."""
        print("#." + "#" * self.width)
        for row in range(self.height):
            print("#", end="")
            for col in range(self.width):
                print("." if self.will_be_empty(Coord(col, row), t) else "*", end="")
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
    basin = Basin(fileinput.input())
    # basin.show(0)
    print(basin.find_path())
