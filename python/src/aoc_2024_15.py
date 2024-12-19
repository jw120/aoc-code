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


class DoubledSpace(Enum):
    """Represent one space in the doubled warehouse."""

    EMPTY = auto()
    LEFT_BOX = auto()
    RIGHT_BOX = auto()
    WALL = auto()

    def __str__(self) -> str:
        match self:
            case DoubledSpace.EMPTY:
                return "."
            case DoubledSpace.LEFT_BOX:
                return "["
            case DoubledSpace.RIGHT_BOX:
                return "]"
            case DoubledSpace.WALL:
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
    """Warehouse for part (a)."""

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

    def __getitem__(self, coord: Coord) -> Space:
        """Return contents of given space."""
        return self.grid[coord.y][coord.x]

    def __setitem__(self, coord: Coord, space: Space) -> None:
        """Set contents of given space."""
        self.grid[coord.y][coord.x] = space

    def push(self, d: str) -> None:
        """Apply push from robot position."""
        delta = direction(d)
        assert self[self.robot] == Space.EMPTY  # Robot space always empty
        target = self.robot

        # Scan over boxes until we find a wall or space
        boxes = 0
        while True:
            target += delta
            match self[target]:
                case Space.WALL:
                    return  # Can't move
                case Space.BOX:
                    boxes += 1
                case Space.EMPTY:
                    break

        # Add box and a space for the robot
        self[target] = Space.BOX
        self.robot += delta
        self[self.robot] = Space.EMPTY

    def apply_moves(self, s: str) -> None:
        """Apply the given moves to the robot."""
        for ch in s:
            if ch in "<>^v":
                self.push(ch)
            elif ch.isspace():
                continue
            else:
                raise ValueError("Bad move.")

    def score(self) -> int:
        """Return sum of GPS coordinates of each box."""
        score = 0
        for coord in self.extent.upto():
            if self[coord] == Space.BOX:
                score += coord.x + 100 * coord.y
        return score

    def print(self) -> None:
        """Print warehouse for debugging."""
        for coord in self.extent.upto_by_y():
            if coord == self.robot:
                print("@", end="")
            else:
                print(str(self[coord]), end="")
            if coord.x == self.extent.x - 1:
                print()
        print()


class DoubledWarehouse:
    """Warehouse with doubled size for part (b)."""

    def __init__(self, s: str) -> None:
        lines = s.split("\n")
        self.extent = Extent(len(lines[0]) * 2, len(lines))
        self.grid: list[list[DoubledSpace]] = []
        robot: Coord | None = None
        for y, line in enumerate(lines):
            self.grid.append([])
            assert len(line.strip()) * 2 == self.extent.x
            x = 0
            for ch in line.strip():
                match ch:
                    case ".":
                        self.grid[-1].append(DoubledSpace.EMPTY)
                        self.grid[-1].append(DoubledSpace.EMPTY)
                    case "O":
                        self.grid[-1].append(DoubledSpace.LEFT_BOX)
                        self.grid[-1].append(DoubledSpace.RIGHT_BOX)
                    case "#":
                        self.grid[-1].append(DoubledSpace.WALL)
                        self.grid[-1].append(DoubledSpace.WALL)
                    case "@":
                        self.grid[-1].append(DoubledSpace.EMPTY)
                        self.grid[-1].append(DoubledSpace.EMPTY)
                        assert robot is None
                        robot = Coord(x, y)
                    case _:
                        raise ValueError(f"Unknown square: '{ch}' at ({x}, {y})")
                x += 2
        assert robot is not None
        self.robot = robot

    def __getitem__(self, coord: Coord) -> DoubledSpace:
        """Return contents of given space."""
        return self.grid[coord.y][coord.x]

    def __setitem__(self, coord: Coord, space: DoubledSpace) -> None:
        """Set contents of given space."""
        self.grid[coord.y][coord.x] = space

    def push_horizontal(self, d: str) -> None:
        """Apply horizontal push from robot position (same as single case)."""
        delta = direction(d)
        assert self[self.robot] == DoubledSpace.EMPTY  # Robot space always empty
        target = self.robot

        # Scan over boxes until we find a wall or space
        boxes = 0
        while True:
            target += delta
            match self[target]:
                case DoubledSpace.WALL:
                    return  # Can't move
                case DoubledSpace.LEFT_BOX:
                    boxes += 1
                case DoubledSpace.RIGHT_BOX:
                    boxes += 1
                case DoubledSpace.EMPTY:
                    break

        # Shift boxes
        self.robot += delta
        self[self.robot] = DoubledSpace.EMPTY
        target = self.robot
        for i in range(boxes):
            target += delta
            left = (i % 2) == (delta == Coord(-1, 0))
            self[target] = DoubledSpace.LEFT_BOX if left else DoubledSpace.RIGHT_BOX

    def push_vertical(self, d: str) -> None:
        """Apply vertical push from robot position (the difficult case).

        Have to cope with difficult configurations like this:
        #############
        #...........#
        #..[]..[]...#
        #...[][]....#
        #....[].....#
        #....@......#
        #############
        """
        delta = direction(d)
        assert self[self.robot] == DoubledSpace.EMPTY  # Robot space always empty

        # See if any boxes to consider moving
        match self[self.robot + delta]:
            case DoubledSpace.EMPTY:
                self.robot += delta
                return
            case DoubledSpace.WALL:
                return
            case DoubledSpace.LEFT_BOX:
                moving_boxes: set[Coord] = {self.robot + delta}
            case DoubledSpace.RIGHT_BOX:
                moving_boxes = {self.robot + delta + Coord(-1, 0)}

        # Look for a set of mobile boxes
        frontier_moving_boxes: set[Coord] = moving_boxes.copy()  # Boxes to check from
        while frontier_moving_boxes:
            new_frontier_moving_boxes: set[Coord] = set()  # New additions for next frontier
            for b in frontier_moving_boxes.copy():
                match self[b + delta]:  # Left side of box
                    case DoubledSpace.WALL:
                        return  # Hit a wall can't move anything
                    case DoubledSpace.EMPTY:
                        pass
                    case DoubledSpace.LEFT_BOX:
                        new_frontier_moving_boxes.add(b + delta)
                    case DoubledSpace.RIGHT_BOX:
                        new_frontier_moving_boxes.add(b + delta + Coord(-1, 0))
                match self[b + delta + Coord(1, 0)]:  # Right side of box
                    case DoubledSpace.WALL:
                        return  # Hit a wall can't move anything
                    case DoubledSpace.EMPTY:
                        pass
                    case DoubledSpace.LEFT_BOX:
                        new_frontier_moving_boxes.add(b + delta + Coord(1, 0))
                    case DoubledSpace.RIGHT_BOX:
                        new_frontier_moving_boxes.add(b + delta)

            moving_boxes |= new_frontier_moving_boxes
            frontier_moving_boxes = new_frontier_moving_boxes

        # Make the move
        for b in moving_boxes:
            self[b] = DoubledSpace.EMPTY
            self[b + Coord(1, 0)] = DoubledSpace.EMPTY
        for b in moving_boxes:
            self[b + delta] = DoubledSpace.LEFT_BOX
            self[b + delta + Coord(1, 0)] = DoubledSpace.RIGHT_BOX
        self.robot += delta

    def apply_moves(self, s: str) -> None:
        """Apply the given moves to the robot."""
        for ch in s:
            if ch in "<>":
                self.push_horizontal(ch)
            elif ch in "^v":
                self.push_vertical(ch)
            elif ch.isspace():
                continue
            else:
                raise ValueError("Bad move.")

    def score(self) -> int:
        """Return sum of GPS coordinates of each box."""
        score = 0
        for coord in self.extent.upto():
            if self[coord] == DoubledSpace.LEFT_BOX:
                score += coord.x + 100 * coord.y
        return score

    def print(self) -> None:
        """Print warehouse for debugging."""
        for coord in self.extent.upto_by_y():
            if coord == self.robot:
                print("@", end="")
            else:
                print(str(self[coord]), end="")
            if coord.x == self.extent.x - 1:
                print()
        print()


if __name__ == "__main__":
    warehouse_str, moves = stdin.read().split("\n\n")
    warehouse = Warehouse(warehouse_str)
    warehouse.apply_moves(moves)
    print(warehouse.score())
    doubled_warehouse = DoubledWarehouse(warehouse_str)
    doubled_warehouse.apply_moves(moves)
    print(doubled_warehouse.score())
