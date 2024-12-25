"""Advent of Code 2024 - Day 25."""

from sys import stdin

type Heights = list[int]  # Always length 5


def parse(s: str) -> tuple[list[Heights], list[Heights]]:
    """Read locks and keys."""
    locks: list[Heights] = []
    keys: list[Heights] = []
    for item in s.split("\n\n"):
        if item[0] == "#":
            locks.append(parse_lock(item.splitlines()))
        else:
            assert item[0] == "."
            keys.append(parse_key(item.splitlines()))
    return locks, keys


def parse_lock(lines: list[str]) -> Heights:
    """Parse a lock."""
    assert lines[0].strip() == "#####"
    assert lines[-1].strip() == "....."
    heights: Heights = [0] * 5
    for line in lines[1:]:
        for i, ch in enumerate(line.strip()):
            if ch == "#":
                heights[i] += 1
            else:
                assert ch == "."
    return heights


def parse_key(lines: list[str]) -> Heights:
    """Parse a key."""
    return parse_lock(list(reversed(lines)))


def fit(lock: Heights, key: Heights) -> bool:
    """Test if lock and key fit."""
    return all(x + y <= 5 for x, y in zip(lock, key, strict=True))


if __name__ == "__main__":
    locks, keys = parse(stdin.read())
    count = 0
    for lock in locks:
        for key in keys:
            count += fit(lock, key)
    print(count)
