"""Advent of Code 2020 - Day 25."""

from doctest import testmod
from sys import stdin


def step(val: int, subject_number: int) -> int:
    """Apply one round of our transformaion."""
    return (val * subject_number) % 20201227


def transform(subject_number: int, loop_size: int) -> int:
    """Transform the subject number for the given loop size."""
    val: int = 1
    for _ in range(loop_size):
        val = step(val, subject_number)
    return val


def find_loop_size(subject_number: int, output: int) -> int:
    """Given a subject number and output, find the loop size."""
    val: int = 1
    loop_count: int = 0
    while val != output:
        val = step(val, subject_number)
        loop_count += 1
    return loop_count


def part_one(public_key_1: int, public_key_2: int) -> int:
    """Find encryption key given two public keys.

    >>> part_one(5764801, 17807724)
    14897079
    """
    secret_1: int = find_loop_size(7, public_key_1)
    secret_2: int = find_loop_size(7, public_key_2)
    encryption_key_v1: int = transform(public_key_1, secret_2)
    encryption_key_v2: int = transform(public_key_2, secret_1)
    if encryption_key_v1 != encryption_key_v2:
        raise RuntimeError("Inconsistent keys")
    return encryption_key_v1


if __name__ == "__main__":
    testmod()
    public_key_1, public_key_2 = [int(line) for line in stdin.readlines()]
    print(part_one(public_key_1, public_key_2))
