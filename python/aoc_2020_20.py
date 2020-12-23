"""Advent of Code 2020 - Day 20."""

from doctest import testmod
from functools import reduce
from sys import stdin
from typing import Dict, Iterable, List, NewType, Optional, Tuple, TypeVar

EdgeValue = NewType("EdgeValue", int)  # Interpreting the edge pixels as binary
TileId = NewType("TileId", int)
Direction = NewType("Direction", int)  # 0 = Up, 1 = Right, 2 = Down, 3 = Left
Connection = Tuple[TileId, Direction, bool]


def flip(e: EdgeValue) -> EdgeValue:
    """Reverse the 10-bit binary representation of an integer.

    >>> bin(flip(0b101))
    '0b1010000000'
    """
    return EdgeValue(int(format(e, "010b")[::-1], 2))


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

    def show(self) -> None:
        for tile in self.tiles.values():
            print(f"Tile id {tile.id} {self.tile_connections[tile.id]}")


if __name__ == "__main__":
    testmod()
    board = Board(block.splitlines() for block in stdin.read().split("\n\n"))
    #    board.show()
    print(reduce(lambda x, y: x * y, board.corners(), 1))
