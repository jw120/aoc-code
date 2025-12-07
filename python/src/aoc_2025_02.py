"""Advent of Code 2025 - Day 1."""

from sys import stdin


def parse_range(s: str) -> tuple[int, int]:
    """Parse one range.

    >>> parse_range("95-115")
    (95, 115)
    """
    a, b = s.split("-")
    return (int(a), int(b))


def parse_ranges(line: str) -> list[tuple[int, int]]:
    """Parse a list of ranges.

    >>> parse_ranges("1-22,95-115")
    [(1, 22), (95, 115)]
    """
    return [parse_range(s) for s in line.strip().split(",")]


def invalid_2(n: int) -> bool:
    """Test if the number is made of twice-repeated digits.

    >>> [invalid_2(x) for x in [11, 12, 22, 3434, 345]]
    [True, False, True, True, False]
    """
    s = str(n)
    n: int = len(s) // 2
    return s[:n] == s[n:]


def invalid_n(n: int) -> bool:
    """Test if the number is made of repeated digits.

    >>> [invalid_n(x) for x in [11, 12, 22, 3434, 345, 111, 232323, 234532]]
    [True, False, True, True, False, True, True, False]
    """
    s = str(n)
    for i in range(1, (len(s) // 2) + 1):
        if len(s) % i != 0:
            continue
        if s[:i] * (len(s) // i) == s:
            return True
    return False


def part_a(ranges: list[tuple[int, int]]) -> int:
    """Solve part a (brute force)."""
    total = 0
    for a, b in ranges:
        total += sum(i for i in range(a, b + 1) if invalid_2(i))
    return total


def part_b(ranges: list[tuple[int, int]]) -> int:
    """Solve part b (brute force)."""
    total = 0
    for a, b in ranges:
        total += sum(i for i in range(a, b + 1) if invalid_n(i))
    return total


if __name__ == "__main__":
    import doctest

    doctest.testmod()
    ranges: list[tuple[int, int]] = parse_ranges(stdin.readline())
    print(part_a(ranges))
    print(part_b(ranges))
