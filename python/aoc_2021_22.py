"""Advent of Code 2021 - Day 22."""

from __future__ import annotations

from doctest import testmod
from functools import reduce
from itertools import chain, combinations
from sys import stdin
from typing import Iterable, Optional, Tuple

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

    def overlap_any(self, others: Iterable[Cuboid]) -> bool:
        return any(self.overlap(c) for c in others)

    def includes(self, other: Cuboid) -> bool:
        return (
            self._min.x <= other._min.x
            and self._max.x >= other._max.x
            and self._min.y <= other._min.y
            and self._max.y >= other._max.y
            and self._min.z <= other._min.z
            and self._max.z >= other._max.z
        )

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


def merge(p: Cuboid, q: Cuboid) -> Optional[Cuboid]:
    """Merge the two cuboids into a single cuboid if possible.

    >>> merge(Cuboid(1, 3, 2, 4, 2, 4), Cuboid(3, 5, 2, 4, 2, 4)).as_tuple()
    (1, 5, 2, 4, 2, 4)
    """
    x_match = p._min.x == q._min.x and p._max.x == q._max.x
    y_match = p._min.y == q._min.y and p._max.y == q._max.y
    z_match = p._min.z == q._min.z and p._max.z == q._max.z
    if y_match and z_match:
        if p._max.x == q._min.x:
            return Cuboid(p._min.x, q._max.x, p._min.y, p._max.y, p._min.z, p._max.z)
        if q._max.x == p._min.x:
            return Cuboid(q._min.x, p._max.x, p._min.y, p._max.y, p._min.z, p._max.z)
    if x_match and z_match:
        if p._max.y == q._min.y:
            return Cuboid(p._min.x, p._max.x, p._min.y, q._max.y, p._min.z, p._max.z)
        if q._max.y == p._min.y:
            return Cuboid(p._min.x, p._max.x, q._min.y, p._max.y, p._min.z, p._max.z)
    if x_match and y_match:
        if p._max.z == q._min.z:
            return Cuboid(p._min.x, p._max.x, p._min.y, p._max.y, p._min.z, q._max.z)
        if q._max.z == p._min.z:
            return Cuboid(p._min.x, p._max.x, q._min.y, p._max.y, q._min.z, p._max.z)
    return None


def merge_adjacents(cs: list[Cuboid]) -> None:
    """Simplify a list of disjoint cuboids by merging adjacenet pairs.

    >>> merge_adjacents(combine(combine([], test1[0]), test1[1]))
    QQ
    """
    for c, d in combinations(cs, 2):
        assert c.disjoint(
            d
        ), f"Overlapping cuboids entering merge_adjacents: {c.as_tuple()} {d.as_tuple()}"
    made_change = True
    while made_change:
        #        print("merged", [c.as_tuple() for c in cs])
        made_change = False
        for i in range(len(cs)):
            for j in range(i + 1, len(cs)):
                ij = merge(cs[i], cs[j])
                if ij is not None:
                    del cs[j]
                    del cs[i]
                    cs.append(ij)
                    made_change = True
                    break
            if made_change:
                break
    for c, d in combinations(cs, 2):
        assert c.disjoint(
            d
        ), f"Overlapping cuboids leaving merge_adjacents: {c.as_tuple()} {d.as_tuple()}"


def combine(cs: list[Cuboid], step: Tuple[bool, Cuboid]) -> list[Cuboid]:
    """Add a new cuboid (which can be on or off) to an existing list of disjoint lit cuboids.

    >>> total_volume(combine([], test1[0]))
    27
    >>> total_volume(combine(combine([], test1[0]), test1[1]))
    46
    >>> total_volume(combine(combine(combine([], test1[0]), test1[1]), test1[2]))
    38
    >>> total_volume(combine(combine(combine(combine([], test1[0]), test1[1]), test1[2]), test1[3]))
    39
    """
    turn_on, x = step
    print("Combining", turn_on, x.as_tuple())
    if not cs:
        return [x] if turn_on else []
    # Start by checking all existing cuboids are disjoint
    for c, d in combinations(cs, 2):
        assert c.disjoint(
            d
        ), f"Overlapping cuboids in combine: {c.as_tuple()} {d.as_tuple()}"
    # Split list into overlapping x and disjoint to x cuboids (ignore any within x)
    overlapping_cuboids = [x] + list(
        filter(lambda c: x.overlap(c) and not x.includes(c), cs)
    )
    disjoint_cuboids = list(filter(lambda c: c.disjoint(x), cs))
    print(
        f"{len(cs)}+1 -> {len(overlapping_cuboids)} overlapping, {len(disjoint_cuboids)} disjoint"
    )
    # Replace the cuboids that overlap with x with all possible cuboids
    x_coords = sorted(
        chain.from_iterable((c._min.x, c._max.x) for c in overlapping_cuboids)
    )
    x_ranges = list(zip(x_coords, x_coords[1:]))
    y_coords = sorted(
        chain.from_iterable((c._min.y, c._max.y) for c in overlapping_cuboids)
    )
    y_ranges = list(zip(y_coords, y_coords[1:]))
    z_coords = sorted(
        chain.from_iterable((c._min.z, c._max.z) for c in overlapping_cuboids)
    )
    z_ranges = list(zip(z_coords, z_coords[1:]))
    new_cuboids = [
        Cuboid(x0, x1, y0, y1, z0, z1)
        for x0, x1 in x_ranges
        for y0, y1 in y_ranges
        for z0, z1 in z_ranges
        if x0 < x1 and y0 < y1 and z0 < z1
    ]
    if turn_on:
        new_on = list(
            filter(
                lambda c: c.overlap_any(overlapping_cuboids) or c.overlap(x),
                new_cuboids,
            )
        )
        merge_adjacents(new_on)
        return disjoint_cuboids + new_on
    else:
        new_off = list(
            filter(
                lambda c: c.overlap_any(overlapping_cuboids) and not c.overlap(x),
                new_cuboids,
            )
        )
        merge_adjacents(new_off)
        return disjoint_cuboids + new_off


def combine_steps(steps: list[Tuple[bool, Cuboid]]) -> list[Cuboid]:
    """Run a series of steps.

    >>> total_volume(combine_steps(test1))
    39
    #    >>> total_volume(combine_steps(test2))
    #   590784
    """
    return reduce(combine, steps, [])


def total_volume(cs: Iterable[Cuboid]) -> int:
    return sum(c.volume() for c in cs)


def read_step(s: str) -> Tuple[bool, Cuboid]:
    on_off, coords = s.split(" ")
    assert on_off in ["on", "off"], f"Bad switch '{on_off}'"
    x_coords, y_coords, z_coords = coords.split(",")
    x0, x1 = x_coords.removeprefix("x=").split("..")
    y0, y1 = y_coords.removeprefix("y=").split("..")
    z0, z1 = z_coords.removeprefix("z=").split("..")
    return (
        on_off == "on",
        Cuboid(int(x0), int(x1) + 1, int(y0), int(y1) + 1, int(z0), int(z1) + 1),
    )


test1 = [
    (True, Cuboid(10, 13, 10, 13, 10, 13)),
    (True, Cuboid(11, 14, 11, 14, 11, 14)),
    (False, Cuboid(9, 12, 9, 12, 9, 12)),
    (True, Cuboid(10, 11, 10, 11, 10, 11)),
]

test2 = [
    read_step(s)
    for s in [
        "on x=-20..26,y=-36..17,z=-47..7",
        "on x=-20..33,y=-21..23,z=-26..28",
        "on x=-22..28,y=-29..23,z=-38..16",
        "on x=-46..7,y=-6..46,z=-50..-1",
        "on x=-49..1,y=-3..46,z=-24..28",
        "on x=2..47,y=-22..22,z=-23..27",
        "on x=-27..23,y=-28..26,z=-21..29",
        "on x=-39..5,y=-6..47,z=-3..44",
        "on x=-30..21,y=-8..43,z=-13..34",
        "on x=-22..26,y=-27..20,z=-29..19",
        "off x=-48..-32,y=26..41,z=-47..-37",
        "on x=-12..35,y=6..50,z=-50..-2",
        "off x=-48..-32,y=-32..-16,z=-15..-5",
        "on x=-18..26,y=-33..15,z=-7..46",
        "off x=-40..-22,y=-38..-28,z=23..41",
        "on x=-16..35,y=-41..10,z=-47..6",
        "off x=-32..-23,y=11..30,z=-14..3",
        "on x=-49..-5,y=-3..45,z=-29..18",
        "off x=18..30,y=-20..-8,z=-3..13",
        "on x=-41..9,y=-7..43,z=-33..15",
        "on x=-54112..-39298,y=-85059..-49293,z=-27449..7877",
        "on x=967..23432,y=45373..81175,z=27513..53682",
    ]
]


if __name__ == "__main__":
    testmod()
    # print(total_volume(combine_steps(test2[:-2])))
    steps = [read_step(line) for line in stdin.read().splitlines()]
    scope = Cuboid(-50, 51, -50, 51, -50, 51)
    small_steps = [s for s in steps if scope.overlap(s[1])]
    print(total_volume(combine_steps(small_steps)))
    print(total_volume(combine_steps(steps)))
