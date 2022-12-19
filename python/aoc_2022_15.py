"""Advent of Code 2022 - Day 15."""

from __future__ import annotations

from doctest import testmod
from re import fullmatch
from sys import stdin
from typing import Iterable

from Coord import Coord, manhattan


class Zone:
    def __init__(self, lines: str) -> None:
        self.sensors: list[Coord] = []
        self.closest_beacons: list[Coord] = []
        x_minima: list[int] = []
        x_maxima: list[int] = []
        for line in lines.split("\n"):
            # print(line)
            m = fullmatch(
                r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)",
                line,
            )
            assert m is not None
            sx, sy, cx, cy = m.groups()
            s = Coord(int(sx), int(sy))
            c = Coord(int(cx), int(cy))
            self.sensors.append(s)
            self.closest_beacons.append(c)
            d = manhattan(s, c)
            x_minima.append((s + Coord(-d, 0)).x)
            x_maxima.append((s + Coord(d, 0)).x)
        self.n = len(self.sensors)
        self.x_min = min(x_minima)
        self.x_max = max(x_maxima)

    def must_be_empty(self, x: int, y: int) -> bool:
        """Test if the given cell must be empty."""
        test_beacon = Coord(x, y)
        # print("Testing", test_beacon)
        for sensor, closest_beacon in zip(self.sensors, self.closest_beacons):
            if test_beacon == closest_beacon:
                break
            closest_beacon_distance = manhattan(sensor, closest_beacon)
            test_distance = manhattan(sensor, test_beacon)
            if test_distance <= closest_beacon_distance:
                # print("Forbidden", sensor)
                return True
        return False

    def empty_in_row(self, y: int) -> int:
        """Return number of positions with given y coordinate that must be empty.

        >>> z = Zone(test_data)
        >>> z.empty_in_row(10)
        26
        """
        count = 0
        for x in range(self.x_min, self.x_max + 1):
            count += self.must_be_empty(x, y)
        return count


test_data = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"""


if __name__ == "__main__":
    testmod()
    z = Zone(stdin.read())
    print(z.empty_in_row(2000000))
