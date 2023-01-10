"""Advent of Code 2022 - Day 22."""

from __future__ import annotations

from doctest import testmod
from enum import Enum
from re import findall
from sys import stdin
from typing import NoReturn, Optional, TypeAlias, Final

from coord import Coord, Extent


def assert_never(value: NoReturn) -> NoReturn:
    """Marker for unreachable code."""
    assert False, f"This code should never be reached, got: {value}"


class Direction(Enum):
    """Directions for walking."""

    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT = 3

    def left(self) -> Direction:
        """Return direction after rotating 90 degrees left."""
        return Direction((self.value - 1) % 4)

    def right(self) -> Direction:
        """Return direction after rotating 90 degrees right."""
        return Direction((self.value + 1) % 4)

    def opposite(self) -> Direction:
        """Return direction after rotating 180 degrees."""
        return Direction((self.value + 2) % 4)


# We reference cube faces by their (x-right, y-down) offsets
FaceOffset: TypeAlias = tuple[int, int]

# We define cube topology by showing for each face, if we leave the face in the given direction
# then we appear on the given face, its edge and heading in the given direction
Topology: TypeAlias = dict[
    FaceOffset, dict[Direction, tuple[FaceOffset, Direction, Direction]]
]


# For the simple wrapping topology for the quesion example, we use a helper function
def wrap(
    u: FaceOffset, r: FaceOffset, d: FaceOffset, l: FaceOffset
) -> dict[Direction, tuple[FaceOffset, Direction, Direction]]:
    return {
        Direction.UP: (u, Direction.DOWN, Direction.UP),
        Direction.RIGHT: (r, Direction.LEFT, Direction.RIGHT),
        Direction.DOWN: (d, Direction.UP, Direction.DOWN),
        Direction.LEFT: (l, Direction.RIGHT, Direction.LEFT),
    }


TEST_TOPOLOGY: Final[Topology] = {
    (2, 0): wrap((2, 2), (2, 0), (2, 1), (2, 0)),
    (0, 1): wrap((0, 1), (1, 1), (0, 1), (2, 1)),
    (1, 1): wrap((1, 1), (2, 1), (1, 1), (0, 1)),
    (2, 1): wrap((2, 0), (0, 1), (2, 2), (1, 1)),
    (2, 2): wrap((2, 1), (3, 2), (2, 0), (3, 2)),
    (3, 2): wrap((3, 2), (2, 2), (3, 2), (2, 2)),
}


class MonkeyMap:
    """Basic class for Day 22, holds (immutable) board."""

    board: dict[Coord, bool]  # True for walls
    extent: Extent
    x_block_size: int
    y_block_size: int
    path: list[str]

    def __init__(self, s: str, topology: Topology) -> None:
        board_str, path_str = s.split("\n\n")

        self.board = {}
        self.extent = Extent(0, 0)
        self.topology = topology
        self.x_start: int = -1
        for y, row in enumerate(board_str.split("\n")):
            for x, c in enumerate(row):
                if self.x_start == -1 and c == ".":
                    self.x_start = x
                match c:
                    case "#" | ".":
                        self.board[Coord(x, y)] = c == "#"
                        if x >= self.extent.x or y >= self.extent.y:
                            self.extent = Extent(
                                max(self.extent.x, x + 1), max(self.extent.y, y + 1)
                            )
                    case " ":
                        pass
                    case _:
                        raise ValueError(f"Bad character in board input '{c}'")

        self.x_block_size = self.extent.x // 4
        self.y_block_size = self.extent.y // 3
        assert (
            self.x_block_size * self.y_block_size * 12 == self.extent.x * self.extent.y
        ), f"Bad size: {self.extent}"
        self.path: list[str] = findall(r"\d+|L|R", path_str)

    def to_coord(self, c: Coord, face: FaceOffset, edge: Direction) -> Coord:
        """Return the coordinate on the given face and edge."""
        c_relative = Coord(c.x % self.x_block_size, c.y % self.y_block_size)
        match edge:
            case Direction.UP:
                c_relative = Coord(c_relative.x, 0)
            case Direction.RIGHT:
                c_relative = Coord(self.x_block_size - 1, c_relative.y)
            case Direction.DOWN:
                c_relative = Coord(c_relative.x, self.y_block_size - 1)
            case Direction.LEFT:
                c_relative = Coord(0, c_relative.y)
        face_x, face_y = face
        return (
            Coord(face_x * self.x_block_size, face_y * self.y_block_size) + c_relative
        )

    def face_if_at_edge(self, c: Coord, d: Direction) -> Optional[FaceOffset]:
        """Return the face the coordinate is on, if move in direction would go off the edge."""
        x = c.x % self.x_block_size
        y = c.y % self.y_block_size
        match d:
            case Direction.UP:
                if y != 0:
                    return None
            case Direction.RIGHT:
                if x != self.x_block_size - 1:
                    return None
            case Direction.DOWN:
                if y != self.y_block_size - 1:
                    return None
            case Direction.LEFT:
                if x != 0:
                    return None
            case _:
                assert_never(d)
        return (c.x // self.x_block_size, c.y // self.y_block_size)

    def move(self, c: Coord, d: Direction) -> tuple[Coord, Direction]:
        """Move one step in given direction.

        If path is blocked, then return the same Coord.
        """
        # print("Move", c, d)
        match d:
            case Direction.UP:
                c_new = c + Coord(0, -1)
            case Direction.DOWN:
                c_new = c + Coord(0, 1)
            case Direction.RIGHT:
                c_new = c + Coord(1, 0)
            case Direction.LEFT:
                c_new = c + Coord(-1, 0)
            case _:
                assert_never(d)
        match self.face_if_at_edge(c, d):
            case None:
                d_new = d
            case (int(face_x), int(face_y)):
                (face_new, edge_new, d_new) = self.topology[(face_x, face_y)][d]
                c_new = self.to_coord(c_new, face_new, edge_new)
        # print(f"Testing move to {c_new} {d_new}, ", end="")
        if self.board[c_new]:
            return c, d
        return c_new, d_new

    def walk(self) -> int:
        """Walk along given path, return final password.

        >>> m = MonkeyMap(TEST_DATA)
        >>> m.walk(WRAP_TOPOLOGY)
        6032
        """
        c = self.to_coord(
            Coord(0, 0), (self.x_start // self.x_block_size, 0), Direction.UP
        )
        d = Direction.RIGHT
        for step in self.path:
            # print("Step", step)
            match step:
                case s if s.isdigit():
                    for _ in range(int(s)):
                        c, d = self.move(c, d)
                case "L":
                    d = d.left()
                case "R":
                    d = d.right()
                case _:
                    raise ValueError("Bad in path")
        return 1000 * (c.y + 1) + 4 * (c.x + 1) + d.left().value

    def show(self) -> None:
        """Print debugging data."""
        for y in range(self.extent.y):
            for x in range(self.extent.x):
                if Coord(x, y) in self.board:
                    print("#" if self.board[Coord(x, y)] else ".", end="")
                else:
                    print(" ", end="")
            print()


TEST_DATA = """        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"""


if __name__ == "__main__":
    # testmod()
    # m = MonkeyMap(stdin.read())
    m = MonkeyMap(TEST_DATA, TEST_TOPOLOGY)
    # m.show()
    print(m.walk())
