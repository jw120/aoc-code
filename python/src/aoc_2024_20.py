"""Advent of Code 2024 - Day 20."""

from sys import stdin

from coord import Coord, Extent
from search import bfs


class RaceTrack:
    """Main class."""

    def __init__(self, lines: list[str]) -> None:
        self.extent = Extent(len(lines[0].strip()), len(lines))
        self.wall: list[list[bool]] = []
        start: Coord | None = None
        end: Coord | None = None
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
                        end = Coord(x, y)
                    else:
                        assert ch == "."
        assert start is not None
        assert end is not None
        self.start: Coord = start
        self.end: Coord = end

    def __getitem__(self, coord: Coord) -> bool:
        return self.wall[coord.y][coord.x]

    def __setitem__(self, coord: Coord, value: bool) -> None:
        self.wall[coord.y][coord.x] = value

    def shortest_path(self) -> int | None:
        """Return length of shortest path from start to end."""
        return bfs(
            self.start,
            lambda c: c == self.end,
            lambda c: [c for c in c.adjacents(self.extent) if not self[c]],
        )

    def print(self) -> None:
        """Print for debugging."""
        for coord in self.extent.upto_by_y():
            if coord == self.start:
                print("S", end="")
            elif coord == self.end:
                print("E", end="")
            else:
                print("#" if self[coord] else ".", end="")
            if coord.x == self.extent.x - 1:
                print()


if __name__ == "__main__":
    race_track = RaceTrack(stdin.readlines())
    race_track.print()
    print(race_track.shortest_path())
