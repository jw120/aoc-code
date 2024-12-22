"""Advent of Code 2024 - Day 22."""

from sys import stdin


def evolve(x: int) -> int:
    """Evolve the given secret."""
    x ^= x * 64
    x %= 16777216
    x ^= x // 32
    x %= 16777216
    x ^= x * 2048
    return x % 16777216


if __name__ == "__main__":
    initial_secrets = [int(line) for line in stdin.readlines()]
    secrets = initial_secrets.copy()
    for _ in range(2000):
        secrets = [evolve(x) for x in secrets]
    print(sum(secrets))
