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


def seq_price(initial_secret: int, n: int) -> dict[tuple[int, int, int, int], int]:
    """For given initial secret, return mapping from change sequences to prices."""
    # Generate secrets, prices and differences
    secret = initial_secret
    secrets: list[int] = []
    prices: list[int] = []
    changes: list[int] = []
    for i in range(n):
        secrets.append(secret)
        prices.append(secret % 10)
        changes.append(-999 if i == 0 else prices[i] - prices[i - 1])
        secret = evolve(secret)
    # Look for sequence of four and the resulting prices
    sequences: dict[tuple[int, int, int, int], int] = {}
    for i in range(4, n):
        sequence = (changes[i - 3], changes[i - 2], changes[i - 1], changes[i])
        if sequence not in sequences:
            sequences[sequence] = prices[i]
    return sequences


def best_price(seq_prices: list[dict[tuple[int, int, int, int], int]]) -> int:
    """Find best price from sequence-pricing maps."""
    combined: dict[tuple[int, int, int, int], int] = seq_prices[0].copy()
    for seq_price in seq_prices[1:]:
        new_combined: dict[tuple[int, int, int, int], int] = {}
        for sequence, price in combined.items():
            new_combined[sequence] = price + seq_price.get(sequence, 0)
        for sequence, price in seq_price.items():
            if sequence not in new_combined:
                new_combined[sequence] = price
        combined = new_combined
    return max(combined.values())


if __name__ == "__main__":
    initial_secrets = [int(line) for line in stdin.readlines()]
    secrets = initial_secrets.copy()
    # Part a
    for _ in range(2000):
        secrets = [evolve(x) for x in secrets]
    print(sum(secrets))
    # Part b
    print(best_price([seq_price(s, 2000) for s in initial_secrets]))
