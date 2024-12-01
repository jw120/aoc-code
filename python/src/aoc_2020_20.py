"""Advent of Code 2020 - Day 20."""

from collections.abc import Iterable
from copy import deepcopy
from doctest import testmod
from functools import reduce
from operator import mul
from sys import stdin
from typing import NewType, TypeVar

# Ids for tiles (as assigned in the problem)
TileId = NewType("TileId", int)

""" Interpreting the edge pixels as binary.

We read the pixels in the given directions
 ---->
^     |
|  T  |
| L R |
|  B  |
|     V
 <----
"""
EdgeValue = NewType("EdgeValue", int)

""" Directions are encoded as 0 = Up, 1 = Right, 2 = Down, 3 = Left

For a tile, its direction is which of its edges is up in the grid.

"""
Direction = NewType("Direction", int)

""" Outgoing connections from a tile are labelled by the id of the connected tile,
the edge of the connected tile and whether or not a flip is needed to match the
edge pixels (False means that the edge values match directly)
"""
Connection = tuple[TileId, Direction, bool]

""" When each tile is embedded in the final grid we store its orientation. The direction is
which edge is up and bool whether the tile needs to be flipped (left-to-right) after rotating.
"""
Orientation = tuple[TileId, bool, Direction]


def right(
    rotation: Direction,
    *,
    flipped: bool,
) -> Direction:
    """Return the edge which is to the right for given tile orientation."""
    if flipped:
        return Direction((rotation + 3) % 4)
    return Direction((rotation + 1) % 4)


def down(rotation: Direction) -> Direction:
    """Return the edge which is down for given tile orientation."""
    return Direction((rotation + 2) % 4)


def flip(e: EdgeValue) -> EdgeValue:
    """Reverse the 10-bit binary representation of an integer.

    >>> bin(flip(0b101))
    '0b1010000000'
    """
    return EdgeValue(int(format(e, "010b")[::-1], 2))


T = TypeVar("T")


def len_skip_none(
    xs: list[T | None],  # pyright: ignore [reportInvalidTypeVarUse]
) -> int:
    """Length of a list of optionals excluding the Nones.

    >>> len_skip_none([1, None, 2])
    2
    """
    return len([x for x in xs if x is not None])


def append_cols(xs: list[list[T]], ys: list[list[T]]) -> list[list[T]]:
    """Append columns to a 2-d array.

    >>> append_cols([[1, 2],[11, 12], [21, 22]], [[3, 4], [5, 6], [7, 8]])
    [[1, 2, 3, 4], [11, 12, 5, 6], [21, 22, 7, 8]]
    """
    return [x + y for x, y in zip(xs, ys, strict=True)]


def flip_matrix(xs: list[list[bool]]) -> None:
    """Mutate the 2-d array to be a left/right flipped version."""
    for x in xs:
        x.reverse()


def rotate_matrix(xs: list[list[T]]) -> list[list[T]]:
    """Return an matrix that is 90 degrees rotated anti-clockwise.

    >>> rotate_matrix([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
    [[3, 6, 9], [2, 5, 8], [1, 4, 7]]
    """
    width: int = len(xs[0])
    ret: list[list[T]] = [[] for _ in range(width)]
    for i in range(width):
        for row in xs:
            ret[i].append(row[i])
    ret.reverse()
    return ret


def count_true_values(xs: list[list[bool]]) -> int:
    """Count the number of true values in a matrix of bools.

    >>> count_true_values([[True, False], [False, False]])
    1
    """
    count = 0
    for row in xs:
        for col in row:
            count += col
    return count


monster: list[list[bool]] = [
    [c == "#" for c in line]
    for line in ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]
]


class Tile:
    """Tile class."""

    def __init__(self, lines: list[str]) -> None:
        def str_to_edge(s: str) -> EdgeValue:
            return EdgeValue(int(s.replace(".", "0").replace("#", "1"), 2))

        self.id: TileId = TileId(int(lines[0][5:9]))
        self.edges: list[EdgeValue] = [
            str_to_edge(lines[1]),
            str_to_edge("".join([line[-1] for line in lines[1:]])),
            str_to_edge(lines[-1][::-1]),
            str_to_edge("".join([line[0] for line in lines[-1:0:-1]])),
        ]
        image_strings: list[str] = [line[1:-1] for line in lines[2:-1]]
        self.image: list[list[bool]] = [[c == "#" for c in row] for row in image_strings]

    def __str__(self) -> str:
        return f"{self.id}: {self.edges}"

    def oriented_image(
        self,
        direction: Direction,
        *,
        do_flip: bool,
    ) -> list[list[bool]]:
        """Produce an oriented image."""
        i_m = deepcopy(self.image)
        for _ in range(direction):
            i_m = rotate_matrix(i_m)
        if do_flip:
            flip_matrix(i_m)
        return i_m


class Board:
    """Main class - a collection of tiles which are assembled."""

    def _add_to_edge_to_tile(self, e: EdgeValue, c: Connection) -> None:
        if e in self.edge_to_tile:
            self.edge_to_tile[e].append(c)
        else:
            self.edge_to_tile[e] = [c]

    def __init__(self, ss: Iterable[list[str]]) -> None:
        # All the tiles by id
        tile_list: list[Tile] = [Tile(s) for s in ss]
        self.tile_size = len(tile_list[0].image)
        self.tiles: dict[TileId, Tile] = {t.id: t for t in tile_list}
        # Lookup table from edge values to connections
        self.edge_to_tile: dict[EdgeValue, list[tuple[TileId, Direction, bool]]] = {}
        for tile in self.tiles.values():
            for i, edge in enumerate(tile.edges):
                self._add_to_edge_to_tile(edge, (tile.id, Direction(i), False))
                self._add_to_edge_to_tile(flip(edge), (tile.id, Direction(i), True))
        # For each tile its connections
        self.tile_connections: dict[TileId, list[Connection | None]] = {}
        for tile in self.tiles.values():
            self.tile_connections[tile.id] = []
            for edge in tile.edges:
                connections = [t for t in self.edge_to_tile[edge] if t[0] != tile.id]
                if len(connections) == 1:
                    self.tile_connections[tile.id].append(connections[0])
                elif len(connections) == 0:
                    self.tile_connections[tile.id].append(None)
                else:
                    raise RuntimeError("Too many connections")
        # Space for assembled image
        self.image: list[list[bool]] = []

    def corners(self) -> list[TileId]:
        """Return the corner tile ids."""
        return [
            tile.id
            for tile in self.tiles.values()
            if len_skip_none(self.tile_connections[tile.id]) == 2
        ]

    def starting_corner(self) -> TileId:
        """Return a corner from which we can move right and down."""
        for i in self.tiles:
            connections = self.tile_connections[i]
            if connections[0] is None and connections[3] is None:
                return i
        raise RuntimeError("No suitable starting corner found")

    def show_connections(self) -> None:
        """Print connections for debugging."""
        for tile_id in sorted(self.tiles.keys()):
            print(f"Tile id {tile_id} {self.tile_connections[tile_id]}")

    def show_image(self) -> None:
        """Print image for debugging."""
        for row in self.image:
            for col in row:
                print("#" if col else ".", end="")
            print()

    def next_right(self, cur: Orientation) -> Orientation | None:
        """Return the tile to the right if any."""
        cur_id, cur_flipped, cur_rot = cur
        next_tile: Connection | None = self.tile_connections[cur_id][
            right(cur_rot, flipped=cur_flipped)
        ]
        if next_tile is None:
            return None
        (
            next_id,
            next_incoming,
            connection_flipped,
        ) = next_tile
        next_flipped = cur_flipped if connection_flipped else (not cur_flipped)
        if next_flipped:
            next_dir = Direction((next_incoming + 3) % 4)
        else:
            next_dir = Direction((next_incoming + 1) % 4)
        return (next_id, next_flipped, next_dir)

    def next_down(self, cur: Orientation) -> Orientation | None:
        """Return the tile below if any."""
        cur_id, cur_flipped, cur_rot = cur
        next_tile: Connection | None = self.tile_connections[cur_id][down(cur_rot)]
        if next_tile is None:
            return None
        (
            next_id,
            next_incoming,
            connection_flipped,
        ) = next_tile
        next_flipped = cur_flipped if connection_flipped else (not cur_flipped)
        return (next_id, next_flipped, next_incoming)

    def tile(self) -> list[list[Orientation]]:
        """Assemble images into a grid of orientations."""

        def tile_row(start: Orientation) -> list[Orientation]:
            cur: Orientation | None = start
            row: list[Orientation] = []
            while True:
                if cur is None:
                    return row
                row.append(cur)
                cur = self.next_right(cur)

        def tile_col(start: Orientation) -> list[Orientation]:
            cur: Orientation | None = start
            col: list[Orientation] = []
            while True:
                if cur is None:
                    return col
                col.append(cur)
                cur = self.next_down(cur)

        start: Orientation = (self.starting_corner(), False, Direction(0))
        return [tile_row(t) for t in tile_col(start)]

    def set_image(self) -> None:
        """Build image from tiles."""
        grid: list[list[Orientation]] = self.tile()
        self.image = []
        for row in grid:
            new_rows: list[list[bool]] = [[] for _ in range(self.tile_size)]
            for tile_id, do_flip, direction in row:
                new_rows = append_cols(
                    new_rows, self.tiles[tile_id].oriented_image(direction, do_flip=do_flip)
                )
            self.image += new_rows

    def sea_monsters(self) -> int:
        """Count sea monsters in the image's current orientation."""

        def test_monster(i: int, j: int) -> bool:
            for monster_row in range(monster_height):
                for monster_col in range(monster_width):
                    if (
                        monster[monster_row][monster_col]
                        and not self.image[i + monster_row][j + monster_col]
                    ):
                        return False
            return True

        image_height: int = len(self.image)
        image_width: int = len(self.image[0])
        monster_height: int = len(monster)
        monster_width: int = len(monster[0])
        count = 0
        for row_index in range(image_height - monster_height + 1):
            for col_index in range(image_width - monster_width + 1):
                if test_monster(row_index, col_index):
                    count += 1
        return count

    def max_sea_monsters(self) -> int:
        """Return the number of sea monsters in the orientation with most monsters."""
        counts: list[int] = []
        for _ in range(4):
            counts.append(self.sea_monsters())
            flip_matrix(self.image)
            counts.append(self.sea_monsters())
            flip_matrix(self.image)
            board.image = rotate_matrix(board.image)
        return max(counts)


if __name__ == "__main__":
    testmod()
    board = Board(block.splitlines() for block in stdin.read().split("\n\n"))
    print(reduce(mul, board.corners(), 1))
    board.set_image()
    print(count_true_values(board.image) - board.max_sea_monsters() * count_true_values(monster))
