"""Advent of Code 2024 - Day 5."""

from itertools import combinations
from sys import stdin


def read_pages() -> tuple[set[tuple[int, int]], list[list[int]]]:
    """Read page orderings and updates."""
    orderings: set[tuple[int, int]] = set()
    updates: list[list[int]] = []
    in_orderings = True
    for line in (s.strip() for s in stdin.readlines()):
        if not line:
            in_orderings = False
            continue
        if in_orderings:
            a, b = [int(s) for s in line.split("|")]
            orderings.add((a, b))
        else:
            updates.append([int(s) for s in line.split(",")])
    return (orderings, updates)


def valid_middle(update: list[int], orderings: set[tuple[int, int]]) -> int:
    """Return middle page if update is in valid order, zero otherwise."""
    for x, y in combinations(update, 2):
        if (y, x) in orderings:
            return 0
    return update[len(update) // 2]


def fixed_middle(update: list[int], orderings: set[tuple[int, int]]) -> int:
    """Return middle page after fixing if the update was invalid, zero otherwise."""
    any_invalid = False
    for i, j in combinations(range(len(update)), 2):
        if (update[j], update[i]) in orderings:
            any_invalid = True
            update[i], update[j] = update[j], update[i]
    return update[len(update) // 2] if any_invalid else 0


if __name__ == "__main__":
    orderings, updates = read_pages()
    print(sum(valid_middle(u, orderings) for u in updates))
    print(sum(fixed_middle(u, orderings) for u in updates))
