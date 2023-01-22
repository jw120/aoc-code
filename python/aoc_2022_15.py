"""Advent of Code 2022 - Day 15."""


from collections.abc import Iterable
from doctest import testmod
from itertools import combinations
from re import fullmatch
from sys import stdin

from coord import Coord, manhattan


class Zone:
    """Main object for day 15."""

    def __init__(self, lines: Iterable[str]) -> None:
        self.sensors: list[Coord] = []
        self.closest_beacons: list[Coord] = []
        x_minima: list[int] = []
        x_maxima: list[int] = []
        for line in lines:
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

    def find_beacon(self) -> Coord:
        """Find the only possible beacon location.

        Uses the fact that this unique location must be just outside the
        forbidden radius of two beacons.

        >>> Zone(TEST_DATA).find_beacon()
        Coord(x=14, y=11)
        """
        sensor_radius: dict[Coord, int] = {
            s: manhattan(s, b) for s, b in zip(self.sensors, self.closest_beacons)
        }
        x_plus_y_constraints: set[int] = set()
        x_minus_y_constraints: set[int] = set()
        for s1, s2 in combinations(self.sensors, 2):
            d = manhattan(s1, s2)
            r1 = sensor_radius[s1]
            r2 = sensor_radius[s2]
            if d == (r1 + 1) + (r2 + 1):
                constraint_sign, constraint_value = get_constraint(s1, s2, r1, r2)
                if constraint_sign:
                    x_plus_y_constraints.add(constraint_value)
                else:
                    x_minus_y_constraints.add(constraint_value)
        assert len(x_plus_y_constraints) > 0 and len(x_minus_y_constraints) > 0
        solutions: set[Coord] = set()
        for p_con in x_plus_y_constraints:
            for m_con in x_minus_y_constraints:
                if p_con + m_con % 2 == 1 or p_con - m_con % 2 == 1:
                    continue
                x = (p_con + m_con) // 2
                y = (p_con - m_con) // 2
                solutions.add(Coord(x, y))
        for solution in solutions:
            if all(
                manhattan(solution, sensor) > sensor_radius[sensor]
                for sensor in self.sensors
            ):
                return solution
        raise ValueError("No valid solution found")


def get_constraint(s1: Coord, s2: Coord, r1: int, r2: int) -> tuple[bool, int]:
    """Return the constraint on x+y or x-y.

    Points must have distance r1+1 from s1 and distance r2+1 from s2.
    Returns (True, z) for x+y=z and (False, z) for x-y=z.
    """
    # Ignore degenerate cases
    assert s1.x != s2.x and s1.y != s2.y
    # Wlg, flip so s1.x is less than s2.x
    if s1.x > s2.x:
        s1, s2 = s2, s1
        r1, r2 = r2, r1
    assert s1.x < s2.x
    if s1.y < s2.y:
        assert s1.x + s1.y + r1 + 1 == s2.x + s2.y - r2 - 1
        return (True, s1.x + s1.y + r1 + 1)
        # print(f"x+y = {s1.x+s1.y+r1+1} = {s2.x+s2.y-r2-1}")
    assert s1.x - s1.y + r1 + 1 == s2.x - s2.y - r2 - 1
    return (False, s1.x - s1.y + r1 + 1)


def encode(c: Coord) -> int:
    """Encode the coordinate."""
    return c.x * 4_000_000 + c.y


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
    testmod()
    z = Zone(stdin.readlines())
    print(z.empty_in_row(2_000_000))
    print(encode(z.find_beacon()))
