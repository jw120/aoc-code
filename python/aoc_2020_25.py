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


def find2_loop_size(
    subject_number: int, output1: int, output2: int
) -> tuple[int, bool]:
    """Look for two loop sizes in parallel."""
    val: int = 1
    loop_count: int = 0
    while True:
        val = step(val, subject_number)
        loop_count += 1
        if val == output1:
            return (loop_count, True)
        if val == output2:
            return (loop_count, False)


def solve(public_key_1: int, public_key_2: int) -> int:
    """Find encryption key given two public keys.

    >>> solve(5764801, 17807724)
    14897079
    """
    (secret, found_1) = find2_loop_size(7, public_key_1, public_key_2)
    return transform(public_key_2 if found_1 else public_key_1, secret)


if __name__ == "__main__":
    testmod()
    public_key_1, public_key_2 = [int(line) for line in stdin.readlines()]
    print(solve(public_key_1, public_key_2))
