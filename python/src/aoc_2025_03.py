"""Advent of Code 2025 - Day 3."""

from sys import stdin


def parse_bank(s: str) -> list[int]:
    """Parse one bank.

    >>> parse_bank("12345")
    [1, 2, 3, 4, 5]
    """
    return [int(c) for c in s.strip()]


def first_max(xs: list[int]) -> tuple[int, int]:
    """Return index and value of the first occurrence of the maximum value in the list.

    >>> first_max([1, 5, 3])
    (1, 5)
    >>> first_max([])
    Traceback (most recent call last):
      ...
    ValueError: list must not be empty
    """
    if not xs:
        raise ValueError("list must not be empty")
    best_index, best_value = 0, xs[0]
    for i, x in enumerate(xs[1:], start=1):
        if x > best_value:
            best_index, best_value = i, x
    return best_index, best_value


def part_a(banks: list[list[int]]) -> int:
    """Solve part a."""
    result = 0
    for bank in banks:
        m = first_max(bank)
        assert m is not None
        (i, x) = m
        if i == len(bank) - 1:
            _, y = first_max(bank[:i])
            result += y * 10 + x
        else:
            _, y = first_max(bank[i + 1 :])
            result += x * 10 + y
    return result


def part_b(banks: list[list[int]], num_digits: int) -> int:
    """Solve part b."""
    total = 0
    for bank in banks:
        n: int = num_digits  # number of digits left to find
        b: int = 0  # index into bank for remaining searches
        result = 0
        while n > 0:
            # If we need  n digits from N availble, at least one must be in first N-(n-1) digits
            search_space = (len(bank) - b) - (n - 1)
            i, x = first_max(bank[b : b + search_space])
            result = 10 * result + x
            b += i + 1  # move past the digit we found
            n -= 1
        total += result
    return total


if __name__ == "__main__":
    import doctest

    doctest.testmod()

    banks: list[list[int]] = [parse_bank(s) for s in stdin.readlines()]
    print(part_a(banks))
    print(part_b(banks, 12))
