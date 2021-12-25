"""Advent of Code 2021 - Day 22."""

from __future__ import annotations

from doctest import testmod
from sys import stdin
from typing import Tuple

from Coord import Coord3


def disjoint(a0: int, a1: int, b0: int, b1: int) -> bool:
    """Test whether the ranges a0<=x< a1 and b0<=x < b1 are disjoint.

    >>> [disjoint(0, 2, 1, 3), disjoint(0, 2, -1, 2), disjoint(0, 2, 1, 2)]
    [False, False, False]
    >>> [disjoint(0, 2, 2, 4), disjoint(0, 2, 3, 4), disjoint(0, 2, 3, 4)]
    [True, True, True]
    """
    assert a0 < a1, f"First inputs for disjoint not valid {a0} {a1}"
    assert b0 < b1, f"Second inputs for disjoint not valid {b0} {b1}"
    return b1 <= a0 or b0 >= a1


class Cuboid:
    def __init__(self, x0: int, x1: int, y0: int, y1: int, z0: int, z1: int) -> None:
        #        print("Making cuboid", x0, x1, y0, y1, z0, z1)
        self._min = Coord3(x0, y0, z0)  # Cuboid includes x0 <= x < x1 etc
        self._max = Coord3(x1, y1, z1)
        assert x0 < x1, f"x-range for Cuboid not valid {x0} {x1} {y0} {y1} {z0} {z1}"
        assert y0 < y1, f"y-range for Cuboid not valid {y0} {y1}"
        assert z0 < z1, f"z-range for Cuboid not valid {z0} {z1}"

    def disjoint(self, other: Cuboid) -> bool:
        """Test if the other cuboid is disjoint."""
        return (
            disjoint(self._min.x, self._max.x, other._min.x, other._max.x)
            or disjoint(self._min.y, self._max.y, other._min.y, other._max.y)
            or disjoint(self._min.z, self._max.z, other._min.z, other._max.z)
        )

    def overlap(self, other: Cuboid) -> bool:
        return not self.disjoint(other)

    def divide_by(self, other: Cuboid) -> list[Cuboid]:
        """Return the cuboids formed by intersection with another cuboid.

        >>> actual = sorted([c.as_tuple() for c in Cuboid(0, 5, 1, 6, 2, 7).divide_by(Cuboid(1, 2, 2, 7, 1, 8))])
        >>> expected = []
        >>> expected.extend([(0, 1, 1, 2, 1, 2), (0, 1, 1, 2, 2, 7), (0, 1, 1, 2, 7, 8)])
        >>> expected.extend([(0, 1, 2, 6, 1, 2), (0, 1, 2, 6, 2, 7), (0, 1, 2, 6, 7, 8)])
        >>> expected.extend([(0, 1, 6, 7, 1, 2), (0, 1, 6, 7, 2, 7), (0, 1, 6, 7, 7, 8)])
        >>> expected.extend([(1, 2, 1, 2, 1, 2), (1, 2, 1, 2, 2, 7), (1, 2, 1, 2, 7, 8)])
        >>> expected.extend([(1, 2, 2, 6, 1, 2), (1, 2, 2, 6, 2, 7), (1, 2, 2, 6, 7, 8)])
        >>> expected.extend([(1, 2, 6, 7, 1, 2), (1, 2, 6, 7, 2, 7), (1, 2, 6, 7, 7, 8)])
        >>> expected.extend([(2, 5, 1, 2, 1, 2), (2, 5, 1, 2, 2, 7), (2, 5, 1, 2, 7, 8)])
        >>> expected.extend([(2, 5, 2, 6, 1, 2), (2, 5, 2, 6, 2, 7), (2, 5, 2, 6, 7, 8)])
        >>> expected.extend([(2, 5, 6, 7, 1, 2), (2, 5, 6, 7, 2, 7), (2, 5, 6, 7, 7, 8)])
        >>> actual == expected
        True
        """

        def ranges(xs: list[int]) -> list[Tuple[int, int]]:
            assert len(xs) == 4
            return [
                (a, b)
                for a, b in [(xs[0], xs[1]), (xs[1], xs[2]), (xs[2], xs[3])]
                if b > a
            ]

        x_ranges = ranges(
            sorted([self._min.x, self._max.x, other._min.x, other._max.x])
        )
        y_ranges = ranges(
            sorted([self._min.y, self._max.y, other._min.y, other._max.y])
        )
        z_ranges = ranges(
            sorted([self._min.z, self._max.z, other._min.z, other._max.z])
        )
        ret = [
            Cuboid(xr[0], xr[1], yr[0], yr[1], zr[0], zr[1])
            for xr in x_ranges
            for yr in y_ranges
            for zr in z_ranges
        ]
        for i in range(len(ret)):
            for j in range(i + 1, len(ret)):
                assert ret[i].disjoint(ret[j])
        return ret

    def volume(self) -> int:
        """Volume of the cuboid.

        >>> Cuboid(10, 12, 10, 12, 10, 12).volume()
        8
        """
        return (
            (self._max.x - self._min.x)
            * (self._max.y - self._min.y)
            * (self._max.z - self._min.z)
        )

    def as_tuple(self) -> Tuple[int, int, int, int, int, int]:
        return (
            self._min.x,
            self._max.x,
            self._min.y,
            self._max.y,
            self._min.z,
            self._max.z,
        )


def combine(x_cuboid: Cuboid, y: Tuple[bool, Cuboid]) -> list[Cuboid]:
    """Given an on cuboid combine with an on/off instruction.

    >>> sum(c.volume() for c in combine(test_data[0][1], test_data[1]))
    46
    """
    y_switch, y_cuboid = y

    if y_switch:
        return [
            c
            for c in x_cuboid.divide_by(y_cuboid)
            # Keep the cube if it is in either x or y
            if c.overlap(x_cuboid) or c.overlap(y_cuboid)
        ]
    else:
        return [
            c
            for c in x_cuboid.divide_by(y_cuboid)
            # Keep the cube if it is in x but not in y
            if c.overlap(x_cuboid) and c.disjoint(y_cuboid)
        ]


def combine_list(pairs: list[Tuple[bool, Cuboid]]) -> list[Cuboid]:
    """Combine a list of on/off instructions into a list of disjoint instructions.

    >>> sum(c.volume() for c in combine_list(test_data[:1]))
    27
    >>> sum(c.volume() for c in combine_list(test_data[:2]))
    46
    >>> sum(c.volume() for c in combine_list(test_data[:3]))
    38
    >>> sum(c.volume() for c in combine_list(test_data))
    39
    """
    assert pairs
    assert pairs[0][0]  # First instruction must be to turn on
    acc: list[Cuboid] = [pairs[0][1]]
    for x in pairs[1:]:
        new_acc: list[Cuboid] = []
        for y in acc:
            new_acc.extend(combine(y, x))
        acc = new_acc
    return acc


test_data = [
    (True, Cuboid(10, 13, 10, 13, 10, 13)),
    (True, Cuboid(11, 14, 11, 14, 11, 14)),
    (False, Cuboid(9, 12, 9, 12, 9, 12)),
    (True, Cuboid(10, 11, 10, 11, 10, 11)),
]


if __name__ == "__main__":
    testmod()
