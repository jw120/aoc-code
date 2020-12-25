"""Advent of Code 2020 - Day 24."""

from __future__ import annotations

import re
from dataclasses import dataclass
from doctest import testmod
from sys import stdin
from typing import List, Set


@dataclass(eq=True, frozen=True)
class HexCoord:
    """Hexagonal grid coordinates."""

    x: int
    y: int

    # "Odd-r" layout from https://www.redblobgames.com/grids/hexagons/

    def move(self, s: str) -> HexCoord:
        if s == "e":
            return HexCoord(self.x + 1, self.y)
        if s == "w":
            return HexCoord(self.x - 1, self.y)
        if s not in ["ne", "se", "nw", "sw"]:
            raise RuntimeError("Bad move direction", s)
        next_y = self.y + 1 if s[0] == "s" else self.y - 1
        if self.y % 2 == 0:
            next_x = self.x - (s[1] == "w")
        else:
            next_x = self.x + (s[1] == "e")
        return HexCoord(next_x, next_y)


def split_moves(s: str) -> List[str]:
    return [m[0] for m in re.finditer("[ns]?[ew]", s)]


test1: List[List[str]] = [
    split_moves(s)
    for s in [
        "sesenwnenenewseeswwswswwnenewsewsw",
        "neeenesenwnwwswnenewnwwsewnenwseswesw",
        "seswneswswsenwwnwse",
        "nwnwneseeswswnenewneswwnewseswneseene",
        "swweswneswnenwsewnwneneseenw",
        "eesenwseswswnenwswnwnwsewwnwsene",
        "sewnenenenesenwsewnenwwwse",
        "wenwwweseeeweswwwnwwe",
        "wsweesenenewnwwnwsenewsenwwsesesenwne",
        "neeswseenwwswnwswswnw",
        "nenwswwsewswnenenewsenwsenwnesesenew",
        "enewnwewneswsewnwswenweswnenwsenwsw",
        "sweneswneswneneenwnewenewwneswswnese",
        "swwesenesewenwneswnwwneseswwne",
        "enesenwswwswneneswsenwnewswseenwsese",
        "wnwnesenesenenwwnenwsewesewsesesew",
        "nenewswnwewswnenesenwnesewesw",
        "eneswnwswnwsenenwnwnwwseeswneewsenese",
        "neswnwewnwnwseenwseesewsenwsweewe",
        "wseweeenwnesenwwwswnew",
    ]
]


def walk(path_lists: List[List[str]]) -> int:
    """Walk along given paths and return number of black tiles.

    >>> walk(test1)
    10
    """
    black_tiles: Set[HexCoord] = set()
    for path in path_lists:
        pos = HexCoord(0, 0)
        for m in path:
            pos = pos.move(m)
        if pos in black_tiles:
            black_tiles.remove(pos)
        else:
            black_tiles.add(pos)
    return len(black_tiles)


if __name__ == "__main__":
    testmod()
    paths = [split_moves(line) for line in stdin]
    print(walk(paths))
