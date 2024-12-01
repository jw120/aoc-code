"""Advent of Code 2021 - Day 17."""

# Ugly brute force solution...

from __future__ import annotations

from doctest import testmod
from re import fullmatch
from sys import stdin

from coord import Coord


class Probe:
    """Probe for day 17."""

    def __init__(self, v_x: int, v_y: int) -> None:
        self.position = Coord(0, 0)
        self.velocity = Coord(v_x, v_y)

    def step(self) -> None:
        """Perform one step."""
        self.position += self.velocity
        if self.velocity.x > 0:
            x_change = -1
        elif self.velocity.x < 0:
            x_change = 1
        else:
            x_change = 0
        self.velocity += Coord(x_change, -1)


class Target:
    """Target for day 17."""

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

    def launch(self, v_x: int, v_y: int) -> int | None:
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
        probe = Probe(v_x, v_y)
        max_height = 0
        while True:
            max_height = max(probe.position.y, max_height)
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

    def scan(self) -> tuple[int, int]:
        """Return maximum possible height and number of successful velocities.

        >>> Target("target area: x=20..30, y=-10..-5").scan()
        (45, 112)
        """
        best_height = 0
        success_count = 0
        for v_x in range(self.x_max + 1):
            for v_y in range(self.y_min, 200):
                result = self.launch(v_x, v_y)
                if result is not None:
                    success_count += 1
                    best_height = max(result, best_height)
        return (best_height, success_count)


if __name__ == "__main__":
    testmod()
    input_target = Target(stdin.read().strip())
    highest, count = input_target.scan()
    print(highest)
    print(count)
