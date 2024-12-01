"""Advent of Code 2022 - Day 22."""

from __future__ import annotations

from doctest import testmod
from re import findall
from sys import stdin
from typing import Final

from coord import Coord, Extent
from direction import Direction

# It would be better to derive the connections between faces from the input file
# but that seems hard - we hard-wire them instead.

# We reference cube faces by their (x-right, y-down) offsets
type FaceOffset = tuple[int, int]

# We define cube topology by showing for each face, if we leave the face in the given direction
# then we appear on the given face and edge with coordinate sense preserved or not
type Topology = dict[FaceOffset, dict[Direction, tuple[FaceOffset, Direction, bool]]]


# For the simple wrapping topology for the question example, we use a helper function
def wrap(
    u: FaceOffset, r: FaceOffset, d: FaceOffset, left: FaceOffset
) -> dict[Direction, tuple[FaceOffset, Direction, bool]]:
    """Build topology when wrapping toroid-wise."""
    return {
        Direction.UP: (u, Direction.DOWN, True),
        Direction.RIGHT: (r, Direction.LEFT, True),
        Direction.DOWN: (d, Direction.UP, True),
        Direction.LEFT: (left, Direction.RIGHT, True),
    }


TEST_WRAP_TOPOLOGY: Final[Topology] = {
    (2, 0): wrap((2, 2), (2, 0), (2, 1), (2, 0)),
    (0, 1): wrap((0, 1), (1, 1), (0, 1), (2, 1)),
    (1, 1): wrap((1, 1), (2, 1), (1, 1), (0, 1)),
    (2, 1): wrap((2, 0), (0, 1), (2, 2), (1, 1)),
    (2, 2): wrap((2, 1), (3, 2), (2, 0), (3, 2)),
    (3, 2): wrap((3, 2), (2, 2), (3, 2), (2, 2)),
}

MAIN_WRAP_TOPOLOGY: Final[Topology] = {
    (1, 0): wrap((1, 2), (2, 0), (1, 1), (2, 0)),
    (2, 0): wrap((2, 0), (1, 0), (2, 0), (1, 0)),
    (1, 1): wrap((1, 0), (1, 1), (1, 2), (1, 1)),
    (0, 2): wrap((0, 3), (1, 2), (0, 3), (1, 2)),
    (1, 2): wrap((1, 1), (0, 2), (1, 0), (0, 2)),
    (0, 3): wrap((0, 2), (0, 3), (0, 2), (0, 3)),
}

TEST_FOLD_TOPOLOGY: Final[Topology] = {
    (2, 0): {
        Direction.UP: ((0, 1), Direction.UP, False),
        Direction.RIGHT: ((3, 2), Direction.RIGHT, False),
        Direction.DOWN: ((2, 1), Direction.UP, True),
        Direction.LEFT: ((1, 1), Direction.UP, True),
    },
    (0, 1): {
        Direction.UP: ((2, 0), Direction.UP, False),
        Direction.RIGHT: ((1, 1), Direction.LEFT, True),
        Direction.DOWN: ((2, 2), Direction.DOWN, False),
        Direction.LEFT: ((3, 2), Direction.DOWN, False),
    },
    (1, 1): {
        Direction.UP: ((2, 0), Direction.LEFT, True),
        Direction.RIGHT: ((2, 1), Direction.LEFT, True),
        Direction.DOWN: ((2, 2), Direction.LEFT, False),
        Direction.LEFT: ((0, 1), Direction.RIGHT, True),
    },
    (2, 1): {
        Direction.UP: ((2, 0), Direction.DOWN, True),
        Direction.RIGHT: ((3, 2), Direction.UP, False),
        Direction.DOWN: ((2, 2), Direction.UP, True),
        Direction.LEFT: ((1, 1), Direction.RIGHT, True),
    },
    (2, 2): {
        Direction.UP: ((2, 1), Direction.DOWN, True),
        Direction.RIGHT: ((3, 2), Direction.LEFT, True),
        Direction.DOWN: ((0, 1), Direction.DOWN, False),
        Direction.LEFT: ((1, 1), Direction.DOWN, False),
    },
    (3, 2): {
        Direction.UP: ((2, 1), Direction.RIGHT, False),
        Direction.RIGHT: ((2, 0), Direction.RIGHT, False),
        Direction.DOWN: ((0, 1), Direction.LEFT, False),
        Direction.LEFT: ((2, 2), Direction.RIGHT, True),
    },
}

MAIN_FOLD_TOPOLOGY: Final[Topology] = {
    (1, 0): {
        Direction.UP: ((0, 3), Direction.LEFT, True),
        Direction.RIGHT: ((2, 0), Direction.LEFT, True),
        Direction.DOWN: ((1, 1), Direction.UP, True),
        Direction.LEFT: ((0, 2), Direction.LEFT, False),
    },
    (2, 0): {
        Direction.UP: ((0, 3), Direction.DOWN, True),
        Direction.RIGHT: ((1, 2), Direction.RIGHT, False),
        Direction.DOWN: ((1, 1), Direction.RIGHT, True),
        Direction.LEFT: ((1, 0), Direction.RIGHT, True),
    },
    (1, 1): {
        Direction.UP: ((1, 0), Direction.DOWN, True),
        Direction.RIGHT: ((2, 0), Direction.DOWN, True),
        Direction.DOWN: ((1, 2), Direction.UP, True),
        Direction.LEFT: ((0, 2), Direction.UP, True),
    },
    (0, 2): {
        Direction.UP: ((1, 1), Direction.LEFT, True),
        Direction.RIGHT: ((1, 2), Direction.LEFT, True),
        Direction.DOWN: ((0, 3), Direction.UP, True),
        Direction.LEFT: ((1, 0), Direction.LEFT, False),
    },
    (1, 2): {
        Direction.UP: ((1, 1), Direction.DOWN, True),
        Direction.RIGHT: ((2, 0), Direction.RIGHT, False),
        Direction.DOWN: ((0, 3), Direction.RIGHT, True),
        Direction.LEFT: ((0, 2), Direction.RIGHT, True),
    },
    (0, 3): {
        Direction.UP: ((0, 2), Direction.DOWN, True),
        Direction.RIGHT: ((1, 2), Direction.DOWN, True),
        Direction.DOWN: ((2, 0), Direction.UP, True),
        Direction.LEFT: ((1, 0), Direction.UP, True),
    },
}


def assert_wrap(t: Topology, *, is_wrap: bool) -> None:
    """Check that a wrap-around topology connects properly."""
    for f_from, m in t.items():
        for d in [Direction.UP, Direction.RIGHT, Direction.DOWN, Direction.LEFT]:
            f_to, e_to, preserve = m[d]
            if is_wrap:
                assert e_to == d.opposite()  # Arrive on edge opposite to exit direction
                assert preserve
            # Check reverse connection
            f_back, e_back, preserve_back = t[f_to][e_to]
            assert f_back == f_from  # Arrive back on starting face
            assert e_back == d  # Arrive back on edge we left from
            assert preserve_back == preserve


assert_wrap(TEST_WRAP_TOPOLOGY, is_wrap=True)
assert_wrap(MAIN_WRAP_TOPOLOGY, is_wrap=True)
assert_wrap(TEST_FOLD_TOPOLOGY, is_wrap=False)
assert_wrap(MAIN_FOLD_TOPOLOGY, is_wrap=False)


class MonkeyMap:
    """Basic class for Day 22, holds (immutable) board."""

    board: dict[Coord, bool]  # True for walls
    extent: Extent
    block_size: int
    y_block_size: int
    path: list[str]

    def __init__(self, s: str, topology: Topology) -> None:
        board_str, path_str = s.split("\n\n")

        self.board = {}
        self.extent = Extent(0, 0)
        self.topology = topology
        self.face_extent = Coord(
            x=1 + max(x for x, _ in topology),
            y=1 + max(y for _, y in topology),
        )
        self.x_start: int = -1
        for y, row in enumerate(board_str.split("\n")):
            for x, c in enumerate(row):
                if self.x_start == -1 and c == ".":
                    self.x_start = x
                if c in "#.":
                    self.board[Coord(x, y)] = c == "#"
                    if x >= self.extent.x or y >= self.extent.y:
                        self.extent = Extent(max(self.extent.x, x + 1), max(self.extent.y, y + 1))
                else:
                    assert c == " "

        self.block_size = self.extent.x // self.face_extent.x
        assert self.extent.y // self.face_extent.y == self.block_size
        assert (
            self.block_size * self.block_size * self.face_extent.x * self.face_extent.y
            == self.extent.x * self.extent.y
        ), f"Bad size: {self.extent} {self.face_extent}"
        self.path: list[str] = findall(r"\d+|L|R", path_str)

    def to_coord(
        self, c: Coord, d: Direction, face: FaceOffset, edge: Direction, *, preserve: bool
    ) -> Coord:
        """Return the coordinate on the given face and edge."""
        c_relative = Coord(c.x % self.block_size, c.y % self.block_size)
        offset = c_relative.x if d in {Direction.UP, Direction.DOWN} else c_relative.y
        if not preserve:
            offset = self.block_size - 1 - offset
        match edge:
            case Direction.UP:
                c_relative = Coord(offset, 0)
            case Direction.RIGHT:
                c_relative = Coord(self.block_size - 1, offset)
            case Direction.DOWN:
                c_relative = Coord(offset, self.block_size - 1)
            case Direction.LEFT:
                c_relative = Coord(0, offset)
        face_x, face_y = face
        return Coord(face_x * self.block_size, face_y * self.block_size) + c_relative

    def face_if_at_edge(self, c: Coord, d: Direction) -> FaceOffset | None:
        """Return the face the coordinate is on, if move in direction would go off the edge."""
        x = c.x % self.block_size
        y = c.y % self.block_size
        match d:
            case Direction.UP:
                if y != 0:
                    return None
            case Direction.RIGHT:
                if x != self.block_size - 1:
                    return None
            case Direction.DOWN:
                if y != self.block_size - 1:
                    return None
            case Direction.LEFT:
                if x != 0:
                    return None
        return (c.x // self.block_size, c.y // self.block_size)

    def move(self, c: Coord, d: Direction) -> tuple[Coord, Direction]:
        """Move one step in given direction.

        If path is blocked, then return the same Coord.
        """
        match d:
            case Direction.UP:
                c_new = c + Coord(0, -1)
            case Direction.DOWN:
                c_new = c + Coord(0, 1)
            case Direction.RIGHT:
                c_new = c + Coord(1, 0)
            case Direction.LEFT:
                c_new = c + Coord(-1, 0)
        match self.face_if_at_edge(c, d):
            case None:
                d_new = d
            case (int(face_x), int(face_y)):
                (face_new, edge_new, preserve) = self.topology[face_x, face_y][d]
                d_new = edge_new.opposite()
                c_new = self.to_coord(c_new, d, face_new, edge_new, preserve=preserve)
        if self.board[c_new]:
            return c, d
        return c_new, d_new

    def walk(self) -> int:
        """Walk along given path, return final password.

        >>> MonkeyMap(TEST_DATA, TEST_WRAP_TOPOLOGY).walk()
        6032
        >>> MonkeyMap(TEST_DATA, TEST_FOLD_TOPOLOGY).walk()
        5031
        """
        d = Direction.RIGHT
        c = self.to_coord(
            Coord(0, 0), d, (self.x_start // self.block_size, 0), Direction.UP, preserve=True
        )
        for step in self.path:
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
    testmod()
    input_data = stdin.read()
    print(MonkeyMap(input_data, MAIN_WRAP_TOPOLOGY).walk())
    print(MonkeyMap(input_data, MAIN_FOLD_TOPOLOGY).walk())
