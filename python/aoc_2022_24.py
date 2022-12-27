"""Advent of Code 2022 - Day 24."""


from collections import deque
from dataclasses import dataclass

# from doctest import testmod
from typing import Final, Iterable, Optional

# x runs left-ro-right, y runs top-to-bottom
# (0, 0) is top-left inside the wall, (width -1, height -1) is bottom-right
# (0, -1) is entrance (width -1, height) is exit
from coord import Coord

# import fileinput


@dataclass(frozen=True)
class Blizzard:
    """Data class for each blizzard"""

    start: Coord
    step: Coord


@dataclass(frozen=True)
class State:
    """State for walk of available paths"""

    location: Coord
    time: int
    visited: frozenset[Coord]


class Basin:
    """Main class for day 24."""

    def __init__(self, input_lines: Iterable[str]) -> None:
        self.blizzards: list[Blizzard] = []
        for row, line in enumerate(input_lines, start=-1):
            if row == -1:
                assert all(c == "#" for c in (line[0:1] + line[2:]))
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
                    self.blizzards.append(
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
        for blizzard in self.blizzards:
            x = (blizzard.start.x + t * blizzard.step.x) % self.width
            y = (blizzard.start.y + t * blizzard.step.y) % self.height
            if Coord(x, y) == c:
                return False
        return True

    def find_path(self) -> int:
        """Return length of shortest path to goal."""
        stack: deque[State] = deque(
            [State(location=self.start, time=0, visited=frozenset())]
        )
        shortest_success: Optional[int] = None

        while True:
            if not stack:
                break
            state = stack.pop()
            print(
                f"{state.time}: ({state.location.x}, {state.location.y}), {len(state.visited)}"
            )
            if state.location == self.goal:
                if shortest_success is None or state.time < shortest_success:
                    shortest_success = state.time
                continue
            for adj in state.location.adjacents():
                if adj not in state.visited and self.will_be_empty(adj, state.time + 1):
                    stack.append(
                        State(
                            location=adj,
                            time=state.time + 1,
                            visited=state.visited | frozenset([state.location]),
                        )
                    )
                stack.append(
                    State(
                        location=state.location,
                        time=state.time + 1,
                        visited=state.visited,
                    )
                )
        assert shortest_success is not None, "No path found"
        return shortest_success

    def show(self, t: int) -> None:
        """Print debugging information for time t."""
        print("#." + "#" * self.width)
        for row in range(self.height):
            print("#", end="")
            for col in range(self.width):
                print("." if self.will_be_empty(Coord(col, row), t) else "*", end="")
            print("#")
        print("#" * self.width + ".#")


TEST_DATA: Final[
    list[str]
] = """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#""".splitlines()


if __name__ == "__main__":
    # testmod()
    basin = Basin(TEST_DATA)
    basin.show(0)
    basin.show(1)
    print(basin.find_path())
