"""Advent of Code 2020 - Day 20."""

from doctest import testmod
from functools import reduce
from sys import stdin
from typing import Dict, Iterable, List, NewType, Optional, Tuple, TypeVar


# Ids for tiles (as assigned in the problem)
TileId = NewType("TileId", int)

# Interpreting the edge pixels as binary
EdgeValue = NewType("EdgeValue", int)  #

# 0 = Up, 1 = Right, 2 = Down, 3 = Left
Direction = NewType("Direction", int)

# Label for an outgoing connection - id and ingoing direction of the connected tile, and
# a flag if the connection is a flipped one
Connection = Tuple[TileId, Direction, bool]

# Label for a tile within the assemebled grid. Flag for whether its contents have been flipped
# and the direction is which edge is up on our grid
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


class Tile:
    def __init__(self, lines: List[str]) -> None:
        def str_to_edge(s: str) -> EdgeValue:
            return EdgeValue(int(s.replace(".", "0").replace("#", "1"), 2))

        self.id: TileId = TileId(int(lines[0][5:9]))
        self.edges: List[EdgeValue] = [
            str_to_edge(lines[1]),
            str_to_edge("".join([line[-1] for line in lines[1:]])),
            str_to_edge(lines[-1]),
            str_to_edge("".join([line[0] for line in lines[1:]])),
        ]
        self.image: List[str] = [line[1:-1] for line in lines[1:-1]]

    def __str__(self) -> str:
        return f"{self.id}: {self.edges}"


class Board:
    def _add_to_edge_to_tile(self, e: EdgeValue, c: Connection) -> None:
        if e in self.edge_to_tile:
            self.edge_to_tile[e].append(c)
        else:
            self.edge_to_tile[e] = [c]

    def __init__(self, ss: Iterable[List[str]]) -> None:
        # All the tiles by id
        tile_list: List[Tile] = [Tile(s) for s in ss]
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
        next_flipped: bool = cur_flipped ^ connection_flipped
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
        next_flipped: bool = cur_flipped ^ connection_flipped
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


if __name__ == "__main__":
    testmod()
    board = Board(block.splitlines() for block in stdin.read().split("\n\n"))
    board.show()
    print(board.corners())
    print(reduce(lambda x, y: x * y, board.corners(), 1))
    for row in board.tile():
        print(len(row), ":", row)
