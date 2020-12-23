"""Advent of Code 2020 - Day 20."""

from copy import deepcopy
from doctest import testmod
from functools import reduce
from sys import stdin
from typing import Dict, Iterable, List, NewType, Optional, Tuple, TypeVar


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
EdgeValue = NewType("EdgeValue", int)  #

""" Directions are encoded as 0 = Up, 1 = Right, 2 = Down, 3 = Left

For a tile, its direction is which of its edges is up in the grid.

"""
Direction = NewType("Direction", int)

""" Outgoing connections from a tile are labelled by the id of the connected tile,
the edge of the connected tile and whether or not a flip is needed to match the
edge pixels (False means that the edge values match directly)
"""
Connection = Tuple[TileId, Direction, bool]

# Label for a tile within the assemebled grid. Flag for whether its contents have been flipped
# and the direction is which edge is up on our grid
""" When each tile is embedded in the final grid we store its orientation. The direction is
which edge is up and bool whether the tile needs to be flipped (left-to-right) after rotating.
"""
Orientation = Tuple[TileId, bool, Direction]


def flip(e: EdgeValue) -> EdgeValue:
    """Reverse the 10-bit binary representation of an integer.

    >>> bin(flip(0b101))
    '0b1010000000'
    """
    return EdgeValue(int(format(e, "010b")[::-1], 2))


def right(flipped: bool, rotation: Direction) -> Direction:
    if flipped:
        return Direction((rotation + 3) % 4)
    else:
        return Direction((rotation + 1) % 4)


def down(flipped: bool, rotation: Direction) -> Direction:
    return Direction((rotation + 2) % 4)


T = TypeVar("T")


def len_skip_none(xs: List[T]) -> int:
    return len([x for x in xs if x is not None])


U = TypeVar("U")


def append_cols(xs: List[List[T]], ys: List[List[T]]) -> List[List[T]]:
    """Append columns to a 2-d array.

    >>> append_cols([[1, 2],[11, 12], [21, 22]], [[3, 4], [5, 6], [7, 8]])
    [[1, 2, 3, 4], [11, 12, 5, 6], [21, 22, 7, 8]]
    """
    return [x + y for x, y in zip(xs, ys)]


def flip_matrix(xs: List[List[T]]) -> None:
    """Mutate the 2-d array to be a left/right flipped version."""
    for x in xs:
        x.reverse()


def rotate_matrix(xs: List[List[T]]) -> List[List[T]]:
    """Return an matrix that is 90 degrees rotated anti-clockwise.

    >>> rotate_matrix([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
    [[3, 6, 9], [2, 5, 8], [1, 4, 7]]
    """
    print("rotating", xs)
    width: int = len(xs[0])
    ret: List[List[T]] = [[] for _ in range(width)]
    for i in range(width):
        for row in xs:
            ret[i].append(row[i])
    ret.reverse()
    return ret


class Tile:
    def __init__(self, lines: List[str]) -> None:
        def str_to_edge(s: str) -> EdgeValue:
            return EdgeValue(int(s.replace(".", "0").replace("#", "1"), 2))

        self.id: TileId = TileId(int(lines[0][5:9]))
        self.edges: List[EdgeValue] = [
            str_to_edge(lines[1]),
            str_to_edge("".join([line[-1] for line in lines[1:]])),
            str_to_edge(lines[-1][::-1]),
            str_to_edge("".join([line[0] for line in lines[-1:0:-1]])),
        ]
        image_strings: List[str] = [line[1:-1] for line in lines[2:-1]]
        self.image: List[List[bool]] = [
            [c == "#" for c in row] for row in image_strings
        ]

    def __str__(self) -> str:
        return f"{self.id}: {self.edges}"

    def oriented_image(self, flip: bool, direction: Direction) -> List[List[bool]]:
        im = deepcopy(self.image)
        for _ in range(direction):
            im = rotate_matrix(im)
        if flip:
            flip_matrix(im)
        print(f'ID: {self.id} {"Flipped" if flip else "no flip"} Dir: {direction}')
        for row in im:
            for col in row:
                print("#" if col else ".", end="")
            print()
        return im


class Board:
    def _add_to_edge_to_tile(self, e: EdgeValue, c: Connection) -> None:
        if e in self.edge_to_tile:
            self.edge_to_tile[e].append(c)
        else:
            self.edge_to_tile[e] = [c]

    def __init__(self, ss: Iterable[List[str]]) -> None:
        # All the tiles by id
        tile_list: List[Tile] = [Tile(s) for s in ss]
        self.tile_size = len(tile_list[0].image)
        print(f"Number of tiles: {len(tile_list)}")
        self.tiles: Dict[TileId, Tile] = {t.id: t for t in tile_list}
        # Lookup table from edge values to connections
        self.edge_to_tile: Dict[EdgeValue, List[Tuple[TileId, Direction, bool]]] = {}
        for tile in self.tiles.values():
            for i, edge in enumerate(tile.edges):
                self._add_to_edge_to_tile(edge, (tile.id, Direction(i), False))
                self._add_to_edge_to_tile(flip(edge), (tile.id, Direction(i), True))
        # For each tile its connections
        self.tile_connections: Dict[TileId, List[Optional[Connection]]] = {}
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

    def corners(self) -> List[TileId]:

        return [
            tile.id
            for tile in self.tiles.values()
            if len_skip_none(self.tile_connections[tile.id]) == 2
        ]

    def starting_corner(self) -> TileId:
        for i in self.tiles.keys():
            connections = self.tile_connections[i]
            if connections[0] is None and connections[3] is None:
                return i
        raise RuntimeError("No suitable starting corner found")

    def show(self) -> None:
        for tile_id in sorted(self.tiles.keys()):
            print(f"Tile id {tile_id} {self.tile_connections[tile_id]}")

    def next_right(self, cur: Orientation) -> Optional[Orientation]:
        """Return the tile to the right if any."""
        cur_id, cur_flipped, cur_rot = cur  # type: TileId, bool, Direction
        next_tile: Optional[Connection] = self.tile_connections[cur_id][
            right(cur_flipped, cur_rot)
        ]
        if next_tile is None:
            return None
        (
            next_id,
            next_incoming,
            connection_flipped,
        ) = next_tile  # type: TileId, Direction, bool
        next_flipped = cur_flipped if connection_flipped else (not cur_flipped)
        if next_flipped:
            next_dir = Direction((next_incoming + 3) % 4)
        else:
            next_dir = Direction((next_incoming + 1) % 4)
        return (next_id, next_flipped, next_dir)

    def next_down(self, cur: Orientation) -> Optional[Orientation]:
        """Return the tile below if any."""
        cur_id, cur_flipped, cur_rot = cur  # type: TileId, bool, Direction
        next_tile: Optional[Connection] = self.tile_connections[cur_id][
            down(cur_flipped, cur_rot)
        ]
        if next_tile is None:
            return None
        (
            next_id,
            next_incoming,
            connection_flipped,
        ) = next_tile  # type: TileId, Direction, bool
        next_flipped = cur_flipped if connection_flipped else (not cur_flipped)
        return (next_id, next_flipped, next_incoming)

    def tile_row(self, start: Orientation) -> List[Orientation]:
        cur: Optional[Orientation] = start
        row: List[Orientation] = []
        while True:
            if cur is None:
                return row
            row.append(cur)
            cur = self.next_right(cur)

    def tile_col(self, start: Orientation) -> List[Orientation]:
        cur: Optional[Orientation] = start
        col: List[Orientation] = []
        while True:
            if cur is None:
                return col
            col.append(cur)
            cur = self.next_down(cur)

    def tile(self) -> List[List[Orientation]]:
        start: Orientation = (self.starting_corner(), False, Direction(0))
        return [self.tile_row(t) for t in self.tile_col(start)]

    def image(self) -> List[List[bool]]:
        grid: List[List[Orientation]] = self.tile()
        image: List[List[bool]] = []
        for row in grid:
            new_rows: List[List[bool]] = [[] for _ in range(self.tile_size)]
            for tile_id, flip, direction in row:
                new_rows = append_cols(
                    new_rows, self.tiles[tile_id].oriented_image(flip, direction)
                )
            image = image + new_rows
        return image


if __name__ == "__main__":
    testmod()
    board = Board(block.splitlines() for block in stdin.read().split("\n\n"))
    board.show()
    print(board.corners())
    print(reduce(lambda x, y: x * y, board.corners(), 1))
    grid: List[List[Orientation]] = board.tile()
    for trow in board.tile():
        print(len(trow), ":", trow)
    final_image: List[List[bool]] = board.image()
    print("image")
    for row in final_image:
        for col in row:
            print("#" if col else ".", end="")
        print()
    print("rot and flip")
    final_image = rotate_matrix(rotate_matrix(final_image))
    flip_matrix(final_image)
    for row in final_image:
        for col in row:
            print("#" if col else ".", end="")
        print()
