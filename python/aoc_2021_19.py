"""Advent of Code 2021 - Day 19."""

from __future__ import annotations

from doctest import testmod
from itertools import chain
from typing import Iterable, Optional

# from enum import Enum, auto
# from sys import stdin


from Coord import Coord3


class Rotation:
    """90 degree rotations of axes (which remain right-handed)."""

    def __init__(self, a: Coord3, b: Coord3):
        self.a = a
        self.b = b
        self.c = Coord3(
            a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x
        )

    @staticmethod
    def all_rotations() -> Iterable[Rotation]:

        dirs = [Coord3(1, 0, 0), Coord3(0, 1, 0), Coord3(0, 0, 1)]

        def flips(a: Coord3, b: Coord3) -> list[tuple[Coord3, Coord3]]:
            return [(a, b), (-a, b), (a, -b), (-a, -b)]

        return (
            Rotation(a, b)
            for a, b in chain(
                *[
                    flips(x_dir, y_dir)
                    for x_dir in dirs
                    for y_dir in dirs
                    if y_dir != x_dir
                ]
            )
        )


class Scanner:
    def __init__(self, ss: list[str]):
        assert len(ss) > 0, "Empty list making Scanner"
        assert ss[0].startswith("--- scanner "), (
            "Scanner initial line missing prefix: '" + ss[0] + "'"
        )
        assert ss[0].endswith(" ---"), (
            "Scanner initial line missing suffix: '" + ss[0] + "'"
        )
        self._number: int = int(ss[0].removeprefix("--- scanner ").removesuffix(" ---"))
        self._beacons: list[Coord3] = [
            Coord3(*[int(i) for i in row.split(",")]) for row in ss[1:]
        ]

    def is_match(self, other: Scanner, offset: Coord3, min_match: int) -> bool:
        """Test if other scanner matches with the given offset and at least `min_match` beacons."""
        matches_found: int = 0
        for p in self._beacons:
            for q in other._beacons:
                if q + offset == p:
                    matches_found += 1
                    break
        return matches_found >= min_match

    def find_match(self, other: Scanner, min_match: int) -> Optional[Coord3]:
        """Return relative position of other scanner if a match with at least `min_match` beacons found.

        >>> test1[0].find_match(test1[1], 3)
        Coord3(x=5, y=2, z=0)
        """
        for p in self._beacons:
            for q in other._beacons:
                offset = p - q
                if self.is_match(other, offset, min_match):
                    return offset
        return None


test1 = [
    Scanner(["--- scanner 0 ---", "0,2,0", "4,1,0", "3,3,0"]),
    Scanner(["--- scanner 1 ---", "-1,-1,0", "-5,0,0", "-2,1,0"]),
]


if __name__ == "__main__":
    testmod()
