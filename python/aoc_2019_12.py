"""Advent of Code 2019 - Day 12."""

from __future__ import annotations

import re
from dataclasses import dataclass
from doctest import testmod

# from sys import stdin
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


@dataclass(eq=True)
class Moon:
    r: Vector
    v: Vector


vector_pattern: Pattern[str] = re.compile(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>")


def parse_vector(s: str) -> Vector:
    """Read a vector from a string.

    >>> parse_vector("<x=-14, y=-10, z=9>")
    Vector(x=-14, y=-10, z=9)
    """
    if m := re.fullmatch(vector_pattern, s):
        return Vector(int(m.group(1)), int(m.group(2)), int(m.group(3)))
    raise RuntimeError("Cannot parse vector:", s)


class Jupiter:
    def __init__(self, moon_positions: List[Vector]) -> None:
        self.moons: List[Moon] = [Moon(r, Vector(0, 0, 0)) for r in moon_positions]
        self.steps: int = 0

    def show(self) -> Jupiter:
        print(f"After {self.steps} step{'s' if self.steps != 1 else ''}")
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

        self.steps += 1
        return self


test1: List[str] = [
    "<x=-1, y=0, z=2>",
    "<x=2, y=-10, z=-7>",
    "<x=4, y=-8, z=8>",
    "<x=3, y=5, z=-1>",
]

if __name__ == "__main__":
    testmod()
    moon_positions: List[Vector] = [
        parse_vector(line) for line in test1
    ]  # stdin.readlines()]
    j = Jupiter(moon_positions)
    for _ in range(11):
        j.show()
        print()
        j.step()
