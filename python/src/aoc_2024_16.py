"""Advent of Code 2024 - Day 16."""

from sys import stdin

from coord import Coord, Extent


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
