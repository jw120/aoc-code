"""Advent of Code 2024 - Day 15."""

from enum import Enum, auto
from sys import stdin

from coord import Coord, Extent


class Space(Enum):
    """Represent one space in the warehouse."""

    EMPTY = auto()
    BOX = auto()
    WALL = auto()

    def __str__(self) -> str:
        match self:
            case Space.EMPTY:
                return "."
            case Space.BOX:
                return "O"
            case Space.WALL:
                return "#"


def direction(s: str) -> Coord:
    """Return one step in given direction."""
    match s:
        case ">":
            return Coord(1, 0)
        case "v":
            return Coord(0, 1)
        case "<":
            return Coord(-1, 0)
        case "^":
            return Coord(0, -1)
        case _:
            raise ValueError("Bad direction in direction.")


class Warehouse:
    """Warehouse."""

    def __init__(self, s: str) -> None:
        lines = s.split("\n")
        self.extent = Extent(len(lines[0]), len(lines))
        self.grid: list[list[Space]] = []
        robot: Coord | None = None
        for y, line in enumerate(lines):
            self.grid.append([])
            assert len(line.strip()) == self.extent.x
            for x, ch in enumerate(line.strip()):
                match ch:
                    case ".":
                        self.grid[-1].append(Space.EMPTY)
                    case "O":
                        self.grid[-1].append(Space.BOX)
                    case "#":
                        self.grid[-1].append(Space.WALL)
                    case "@":
                        self.grid[-1].append(Space.EMPTY)
                        assert robot is None
                        robot = Coord(x, y)
                    case _:
                        raise ValueError(f"Unknown square: '{ch}' at ({x}, {y})")
        assert robot is not None
        self.robot = robot

    def g(self, coord: Coord) -> Space:
        """Return contents of given space."""
        return self.grid[coord.y][coord.x]

    def set_g(self, coord: Coord, space: Space) -> None:
        """Set contents of given space."""
        self.grid[coord.y][coord.x] = space

    def push(self, d: str) -> None:
        """Apply push from robot position."""
        delta = direction(d)
        assert self.g(self.robot) == Space.EMPTY  # Robot space always empty
        target = self.robot

        # Scan over boxes until we find a wall or space
        boxes = 0
        while True:
            target += delta
            match self.g(target):
                case Space.WALL:
                    return  # Can't move
                case Space.BOX:
                    boxes += 1
                case Space.EMPTY:
                    break

        # Add box and a space for the robot
        self.set_g(target, Space.BOX)
        self.robot += delta
        self.set_g(self.robot, Space.EMPTY)

    def apply_moves(self, s: str) -> None:
        """Apply the given moves to the robot."""
        for ch in s:
            if ch in "<>^v":
                # print(f"Move {ch}:")
                self.push(ch)
                # self.print()
            elif ch.isspace():
                continue
            else:
                raise ValueError("Bad move.")

    def score(self) -> int:
        """Return sum of GPS coordinates of each box."""
        score = 0
        for coord in self.extent.upto():
            if self.g(coord) == Space.BOX:
                score += coord.x + 100 * coord.y
        return score

    def print(self) -> None:
        """Print warehouse for debugging."""
        for coord in self.extent.upto_by_y():
            if coord == self.robot:
                print("@", end="")
            else:
                print(str(self.g(coord)), end="")
            if coord.x == self.extent.x - 1:
                print()
        print()


if __name__ == "__main__":
    warehouse_str, moves = stdin.read().split("\n\n")
    warehouse = Warehouse(warehouse_str)
    # print("Initial state:")
    # warehouse.print()
    warehouse.apply_moves(moves)
    print(warehouse.score())
