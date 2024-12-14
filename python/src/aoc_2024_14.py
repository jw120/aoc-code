"""Advent of Code 2024 - Day 14."""

from __future__ import annotations

from collections import Counter
from dataclasses import dataclass
from math import prod
from re import match
from sys import stdin

from coord import Coord


@dataclass
class Robot:
    """A robot."""

    position: Coord
    velocity: Coord

    @staticmethod
    def read(s: str) -> Robot:
        """Initialise a robot from a string description."""
        m = match(r"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)", s.strip())
        assert m is not None
        return Robot(
            position=Coord(int(m.group(1)), int(m.group(2))),
            velocity=Coord(int(m.group(3)), int(m.group(4))),
        )


def part_a(robots: list[Robot], width: int, height: int) -> int:
    """Solve part A."""
    positions = [r.position for r in robots]
    velocities = [r.velocity for r in robots]
    for _ in range(100):
        for i in range(len(positions)):
            positions[i] += velocities[i]
            positions[i] = Coord(positions[i].x % width, positions[i].y % height)
    counts: Counter[tuple[bool, bool]] = Counter()
    for position in positions:
        if width % 2 == 1 and position.x == width // 2:
            continue
        if height % 2 == 1 and position.y == height // 2:
            continue
        counts[position.x < width // 2, position.y < height // 2] += 1
    return prod(counts.values())


if __name__ == "__main__":
    robots = [Robot.read(line) for line in stdin.readlines()]
    if len(robots) == 12:
        width, height = 11, 7  # Example problem
    else:
        width, height = 101, 103  # Real problem
    print(part_a(robots, width, height))
