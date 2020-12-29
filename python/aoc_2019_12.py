"""Advent of Code 2019 - Day 12."""

from __future__ import annotations

import re
from dataclasses import dataclass
from doctest import testmod
from sys import stdin
from typing import List, Pattern


def sign(a: int) -> int:
    return (a > 0) - (a < 0)


@dataclass(eq=True)
class Vector:
    x: int
    y: int
    z: int

    def __add__(self, other: Vector) -> Vector:
        return Vector(self.x + other.x, self.y + other.y, self.z + other.z)

    def __sub__(self, other: Vector) -> Vector:
        return Vector(self.x - other.x, self.y - other.y, self.z - other.z)

    def sign(self) -> Vector:
        return Vector(sign(self.x), sign(self.y), sign(self.z))

    def l1_norm(self) -> int:
        return abs(self.x) + abs(self.y) + abs(self.z)


@dataclass(eq=True)
class Moon:
    r: Vector
    v: Vector

    @property
    def energy(self) -> int:
        return self.r.l1_norm() * self.v.l1_norm()


vector_pattern: Pattern[str] = re.compile(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>")


def parse_vector(s: str) -> Vector:
    """Read a vector from a string.

    >>> parse_vector("<x=-14, y=-10, z=9>")
    Vector(x=-14, y=-10, z=9)
    """
    if m := re.fullmatch(vector_pattern, s.strip()):
        return Vector(int(m.group(1)), int(m.group(2)), int(m.group(3)))
    raise RuntimeError("Cannot parse vector:", s)


class Jupiter:
    """Implements a system of four moons.

    >>> Jupiter(test1).run(10).energy
    179
    >>> Jupiter(test2).run(100).energy
    1940
    """

    def __init__(self, moon_positions: List[Vector]) -> None:
        self.moons: List[Moon] = [Moon(r, Vector(0, 0, 0)) for r in moon_positions]

    @property
    def energy(self) -> int:
        return sum(m.energy for m in self.moons)

    def show(self) -> Jupiter:
        for moon in self.moons:
            print(
                f"pos=<x={moon.r.x}, y={moon.r.y}, z={moon.r.z}>,",
                f"vel=<x={moon.v.x}, y={moon.v.y}, z={moon.v.z}>",
            )
        return self

    def step(self) -> Jupiter:
        for i, m1 in enumerate(self.moons):
            for m2 in self.moons[i + 1 :]:
                r_diff: Vector = (m2.r - m1.r).sign()
                m1.v += r_diff
                m2.v -= r_diff
        for m in self.moons:
            m.r += m.v

        return self

    def run(self, n: int) -> Jupiter:
        for _ in range(n):
            self.step()
        return self


test1: List[Vector] = [
    parse_vector(line)
    for line in [
        "<x=-1, y=0, z=2>",
        "<x=2, y=-10, z=-7>",
        "<x=4, y=-8, z=8>",
        "<x=3, y=5, z=-1>",
    ]
]

test2: List[Vector] = [
    parse_vector(line)
    for line in [
        "<x=-8, y=-10, z=0>",
        "<x=5, y=5, z=10>",
        "<x=2, y=-7, z=3>",
        "<x=9, y=-8, z=-3>",
    ]
]

if __name__ == "__main__":
    testmod()
    moon_positions: List[Vector] = [parse_vector(line) for line in stdin.readlines()]
    print(Jupiter(moon_positions).run(1000).energy)
