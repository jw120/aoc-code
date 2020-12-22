"""Advent of Code 2020 - Day 20."""

from doctest import testmod
from functools import reduce
from sys import stdin
from typing import Dict, List


def horiz(s: str) -> List[int]:
    forwards: str = s.replace(".", "0").replace("#", "1")
    backwards: str = forwards[::-1]
    return [int(forwards, 2), int(backwards, 2)]


def vert(ss: List[str]) -> List[int]:
    forwards: str = "".join(ss).replace(".", "0").replace("#", "1")
    backwards: str = forwards[::-1]
    return [int(forwards, 2), int(backwards, 2)]


def flatten2(xs: List[List[int]]) -> List[int]:
    """Flatten a list of lists.

    >>> flatten2([[1, 2], [3, 4]])
    [1, 2, 3, 4]
    """
    result: List[int] = []
    for x in xs:
        result += x
    return result


class Tile:
    def __init__(self, lines: List[str]) -> None:
        self.id = int(lines[0][5:9])
        left = [line[0] for line in lines[1:]]
        right = [line[-1] for line in lines[1:]]
        self.edges = flatten2(
            [horiz(lines[1]), horiz(lines[-1]), vert(left), vert(right)]
        )

    def __str__(self) -> str:
        return f"{self.id}: {self.edges}"


def part_one(tiles: List[Tile]) -> int:
    edge_to_tile: Dict[int, List[int]] = {}
    for tile in tiles:
        for edge in tile.edges:
            if edge in edge_to_tile:
                edge_to_tile[edge].append(tile.id)
            else:
                edge_to_tile[edge] = [tile.id]
    corner_tiles: List[int] = []
    for tile in tiles:
        count_matches = 0
        for edge in tile.edges:
            count_matches += len(edge_to_tile[edge])
        if count_matches == 12:
            corner_tiles.append(tile.id)
    return reduce(lambda x, y: x * y, corner_tiles, 1)


if __name__ == "__main__":
    testmod()
    tiles = [Tile(block.splitlines()) for block in stdin.read().split("\n\n")]
    print(part_one(tiles))
