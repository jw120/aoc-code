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


def ways(available: list[str], desired: str, cache: dict[str, int] | None) -> int:
    """Count number of ways the desired pattern be constructed with the available patterns."""
    if not desired:
        return 1
    if cache is None:
        cache = {}
    if desired in cache:
        return cache[desired]
    count = sum(
        ways(available, desired[len(a) :], cache) for a in available if desired.startswith(a)
    )
    cache[desired] = count
    return count


if __name__ == "__main__":
    available, desired = parse(stdin.read())
    print(sum(possible(available, d) for d in desired))
    print(sum(ways(available, d, None) for d in desired))
