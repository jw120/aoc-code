"""Advent of Code 2024 - Day 7."""

from operator import add, mul
from sys import stdin


class Calibration:
    """One puzzle line."""

    def __init__(self, s: str) -> None:
        target, rest = s.strip().split(":")
        self.target: int = int(target)
        self.numbers: list[int] = [int(x) for x in rest.strip().split()]

    def check(self, value: int, index: int) -> bool:
        """Check for solution with given input value and starting index."""
        if index == len(self.numbers):
            return value == self.target
        return any(self.check(op(value, self.numbers[index]), index + 1) for op in (add, mul))

    def test(self) -> int:
        """Return target if valid, zero otherwise."""
        return self.target if self.check(self.numbers[0], 1) else 0

    def check3(self, value: int, index: int) -> bool:
        """Check for solution with given input value and starting index."""
        if index == len(self.numbers):
            return value == self.target
        for op in ["+", "*", "|"]:
            match op:
                case "+":
                    if self.check3(value + self.numbers[index], index + 1):
                        return True
                case "*":
                    if self.check3(value * self.numbers[index], index + 1):
                        return True
                case "|":
                    if self.check3(combine(value, self.numbers[index]), index + 1):
                        return True
                case _:
                    raise ValueError("Bad op")
        return False

    def test3(self) -> int:
        """Return target if valid, zero otherwise."""
        return self.target if self.check3(self.numbers[0], 1) else 0


def combine(a: int, b: int) -> int:
    """Combine digits of two integers."""
    return int(str(a) + str(b))


if __name__ == "__main__":
    calibrations = [Calibration(s) for s in stdin.readlines()]
    print(sum(c.test() for c in calibrations))
    print(sum(c.test3() for c in calibrations))
