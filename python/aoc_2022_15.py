"""Advent of Code 2022 - Day 15."""

from __future__ import annotations

from doctest import testmod
from itertools import combinations
from re import fullmatch
from typing import Optional, Iterable
import fileinput
from coord import Coord, manhattan


class Zone:
    """Main object for day 15."""

    def __init__(self, lines: Iterable[str]) -> None:
        self.sensors: list[Coord] = []
        self.closest_beacons: list[Coord] = []
        x_minima: list[int] = []
        x_maxima: list[int] = []
        for line in lines:
            # print(line)
            m = fullmatch(
                r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)",
                line.strip(),
            )
            assert m is not None
            s = Coord(int(m.group(1)), int(m.group(2)))
            c = Coord(int(m.group(3)), int(m.group(4)))
            self.sensors.append(s)
            self.closest_beacons.append(c)
            d = manhattan(s, c)
            x_minima.append((s + Coord(-d, 0)).x)
            x_maxima.append((s + Coord(d, 0)).x)
        self.n = len(self.sensors)
        self.x_min = min(x_minima)
        self.x_max = max(x_maxima)
        # self.beacons: set[Coord] = set(self.closest_beacons)

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

        >>> z = Zone(TEST_DATA)
        >>> z.empty_in_row(10)
        26
        """
        count = 0
        for x in range(self.x_min, self.x_max + 1):
            count += self.must_be_empty(x, y)
        return count

    # def find_in_row(self, y: int, x_max: int) -> Optional[int]:
    #     """Return coordinate of beacon in row y if possible."""
    #     for x in range(0, x_max + 1):
    #         if Coord(x, y) in self.beacons:
    #             continue
    #         if not self.must_be_empty(x, y):
    #             return x
    #     return None

    # def find_in_any_row(self, x_max: int, y_max: int) -> int:
    #     """Find beacon.

    #     >>> z = Zone(TEST_DATA)
    #     >>> z.find_in_any_row(x_max = 20, y_max=20)
    #     56000011
    #     """
    #     for y in range(0, y_max + 1):
    #         # if y % 100 == 0 and y > 0:
    #         print(y / 1_000_000)
    #         x = self.find_in_row(y, x_max=x_max)
    #         if x is not None:
    #             return x * 4000000 + y
    #     raise ValueError("Failed to find")

    def valid_pairs(self) -> int:
        """TODO."""
        # Distance from each sensor to the closest beacon
        sensor_radius: dict[Coord, int] = {
            s: manhattan(s, b) for s, b in zip(self.sensors, self.closest_beacons)
        }
        for s1, s2 in combinations(self.sensors, 2):
            d = manhattan(s1, s2)
            r1 = sensor_radius[s1]
            r2 = sensor_radius[s2]
            if d == (r1 + 1) + (r2 + 1):
                print(
                    s1,
                    s2,
                    r1,
                    r2,
                    d,
                )
                intersections(s1, s2, r1, r2)
                # # if s2.x >= s1.x and s2.y >= s1.y:
                # #     print("x+y=", s1.x + s1.y + r1 + 1, s2.x + s2.y - r2 - 1)
                # if s1.x + s1.y <= s2.x + s2.y:
                #     print("x+y=", s1.x + s1.y + r1 + 1, s2.x + s2.y - r2 - 1)
                # else:
                #     print("x+y=", s1.x + s1.y - r1 - 1, s2.x + s2.y + r2 + 1)

                # min_xy = min(s1.x + s1.y, s2.x + s2.y)
                # min_xy = min(s1.x + s1.y, s2.x + s2.y)
                # print(r1 + 1 + s1.x + s1.y)
                # print(r2 + 1 + s2.x + s2.y)
        # for s in self.sensors:
        #     print(s, sensor_radius[s], manhattan(s, Coord(14, 11)))
        return 0


def intersections(s1: Coord, s2: Coord, r1: int, r2: int) -> None:
    """Doc."""
    assert s1.x != s2.x and s1.y != s2.y
    if s1.x > s2.x:
        s1, s2 = s2, s1
        r1, r2 = r2, r1
    assert s1.x < s2.x
    if s1.y < s2.y:
        print(f"x+y = {s1.x+s1.y+r1+1} = {s2.x+s2.y-r2-1}")
    else:
        print(f"x-y = {s1.x-s1.y+r1+1} = {s2.x-s2.y-r2-1}")


TEST_DATA: list[
    str
] = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
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
Sensor at x=20, y=1: closest beacon is at x=15, y=3""".split(
    "\n"
)


if __name__ == "__main__":
    # testmod()
    # z = Zone(fileinput.input())
    # print(z.empty_in_row(2_000_000))
    # print(z.find_in_any_row(x_max=4_000_000, y_max=4_000_000))
    z = Zone(fileinput.input())
    z.valid_pairs()
    print()
    z = Zone(TEST_DATA)
    z.valid_pairs()
