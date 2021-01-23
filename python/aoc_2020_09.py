"""Advent of Code 2020 - Day 9."""

from doctest import testmod
from sys import stdin


test_one: list[int] = [
    35,
    20,
    15,
    25,
    47,
    40,
    62,
    55,
    65,
    95,
    102,
    117,
    150,
    182,
    127,
    219,
    299,
    277,
    309,
    576,
]


def can_be_sum(xs: list[int], y: int) -> bool:
    """Test if y be written as the sum of two distinct values in xs."""
    for i1 in range(0, len(xs)):
        for i2 in range(i1 + 1, len(xs)):
            if xs[i1] + xs[i2] == y and xs[i1] != xs[i2]:
                return True
    return False


def test_sequence(xs: list[int], window: int) -> int:
    """Find the first value which cannot be formed as a sum.

    >>> test_sequence(test_one, 5)
    127
    """
    for i in range(window, len(xs)):
        if not can_be_sum(xs[i - window : i], xs[i]):
            return xs[i]
    raise RuntimeError("No mismatch found")


def find_contig_sum(xs: list[int], target: int) -> int:
    """Find contiguous subsequence which sums to target.

    Returns smallest and largest values in the subsequence.

    >>> find_contig_sum(test_one, 127)
    62
    """
    for sum_start_index in range(0, len(xs)):
        for sum_end_index in range(sum_start_index + 1, len(xs)):
            subseq = xs[sum_start_index:sum_end_index]
            s = sum(subseq)
            if s == target:
                sorted_subseq = sorted(subseq)
                return sorted_subseq[0] + sorted_subseq[-1]
            if s > target:
                break
    raise RuntimeError("No sum found")


if __name__ == "__main__":
    testmod()
    nums: list[int] = [int(line) for line in stdin]
    target = test_sequence(nums, 25)
    print(target)
    print(find_contig_sum(nums, target))
