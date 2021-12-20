"""Advent of Code 2021 - Day 17."""

# Ugly brute force solution...

from __future__ import annotations

from doctest import testmod
from re import fullmatch
from sys import stdin
from typing import Optional, Tuple

from Coord import Coord


class Probe:
    def __init__(self, vx: int, vy: int) -> None:
        self.position = Coord(0, 0)
        self.velocity = Coord(vx, vy)

    def step(self) -> None:
        self.position += self.velocity
        if self.velocity.x > 0:
            x_change = -1
        elif self.velocity.x < 0:
            x_change = 1
        else:
            x_change = 0
        self.velocity = self.velocity + Coord(x_change, -1)


class Target:
    def __init__(self, s: str) -> None:
        match = fullmatch(r"target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)", s)
        if match is None:
            raise ValueError("Failed to parse on init")
        self.x_min = int(match.group(1))
        self.x_max = int(match.group(2))
        self.y_min = int(match.group(3))
        self.y_max = int(match.group(4))
        assert self.x_max > self.x_min
        assert self.y_max > self.y_min
        assert self.x_min > 0  # Assume target to right of start
        assert self.y_max < 0  # Assume target below start

    def launch(self, vx: int, vy: int) -> Optional[int]:
        """Launch the probe and return the maximum height if it hits the target.

        >>> Target("target area: x=20..30, y=-10..-5").launch(7, 2)
        3
        >>> Target("target area: x=20..30, y=-10..-5").launch(6, 3)
        6
        >>> Target("target area: x=20..30, y=-10..-5").launch(9, 0)
        0
        >>> Target("target area: x=20..30, y=-10..-5").launch(17, -4)
        >>> Target("target area: x=20..30, y=-10..-5").launch(6, 9)
        45
        """
        probe = Probe(vx, vy)
        max_height = 0
        while True:
            if probe.position.y > max_height:
                max_height = probe.position.y
            if (
                probe.position.x >= self.x_min
                and probe.position.x <= self.x_max
                and probe.position.y >= self.y_min
                and probe.position.y <= self.y_max
            ):
                return max_height
            # Give up if we are below the bottom of the target and moving down
            if probe.velocity.y < 0 and probe.position.y < self.y_min:
                return None
            probe.step()

    def scan(self) -> Tuple[int, int]:
        """Return maximum possible height and number of successful velocities.

        >>> Target("target area: x=20..30, y=-10..-5").scan()
        (45, 112)
        """
        best_height = 0
        count = 0
        for vx in range(0, self.x_max + 1):
            for vy in range(self.y_min, 200):
                result = self.launch(vx, vy)
                if result is not None:
                    count += 1
                    if result > best_height:
                        best_height = result
        return (best_height, count)


if __name__ == "__main__":
    testmod()
    target = Target(stdin.read().strip())
    highest, count = target.scan()
    print(highest)
    print(count)
