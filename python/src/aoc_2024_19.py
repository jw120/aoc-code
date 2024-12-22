"""Advent of Code 2024 - Day 19."""

from sys import stdin


def parse(s: str) -> tuple[list[str], list[str]]:
    """Parse available and desired towel designs from input."""
    a, d = s.split("\n\n")
    available = a.split(", ")
    desired = d.split()
    return available, desired


def possible(available: list[str], desired: str) -> bool:
    """Test if the desired pattern be constructed with the available patterns."""
    if not desired:
        return True
    return any(desired.startswith(a) and possible(available, desired[len(a) :]) for a in available)


if __name__ == "__main__":
    available, desired = parse(stdin.read())
    print(sum(possible(available, d) for d in desired))
