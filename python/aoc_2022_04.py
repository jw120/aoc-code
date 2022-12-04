"""Advent of Code 2022 - Day 4."""

from doctest import testmod
from sys import stdin


class Assignment:
    def __init__(self, s: str) -> None:
        """Initialize an assignment from a string.

        >>> Assignment("2-4,6-8")
        2-4,6-8
        """
        s1, s2 = s.split(",")
        self.lo1, self.hi1 = (int(x) for x in s1.split("-"))
        self.lo2, self.hi2 = (int(x) for x in s2.split("-"))
        assert self.lo1 <= self.hi1
        assert self.lo2 <= self.hi2

    def __repr__(self) -> str:
        return f"{self.lo1}-{self.hi1},{self.lo2}-{self.hi2}"

    def contain(self) -> bool:
        """Test if one range fully contains the other.

        >>> Assignment("1-6,2-3").contain()
        True
        >>> Assignment("1-4,2-4").contain()
        True
        >>> Assignment("1-6,2-7").contain()
        False
        """
        return (self.lo1 <= self.lo2 and self.hi1 >= self.hi2) or (
            self.lo2 <= self.lo1 and self.hi2 >= self.hi1
        )

    def overlap(self) -> bool:
        """Test if the two ranges overlap at all.

        >>> Assignment("1-6,2-3").overlap()
        True
        >>> Assignment("4-6,1-2").overlap()
        False
        """
        return not (self.hi1 < self.lo2 or self.hi2 < self.lo1)


if __name__ == "__main__":
    testmod()
    assignments = [Assignment(line.strip()) for line in stdin]
    print(sum(a.contain() for a in assignments))
    print(sum(a.overlap() for a in assignments))
