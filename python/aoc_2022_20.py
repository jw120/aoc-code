"""Advent of Code 2022 - Day 20."""


import fileinput
from collections import Counter
from doctest import testmod
from typing import Final


def mix(initial_numbers: list[int]) -> int:
    """Apply mixing algorithm.

    >>> test_numbers = [int(s) for s in TEST_DATA.splitlines()]
    >>> mix(test_numbers)
    3
    """
    n = len(initial_numbers)
    numbers: list[int] = initial_numbers.copy()
    for target_value in initial_numbers:
        assert len(set(numbers)) == len(numbers)
        old_target_position = numbers.index(target_value)
        # Remove target value
        del numbers[old_target_position : old_target_position + 1]
        new_target_position = (old_target_position + target_value) % (n - 1)
        numbers.insert(new_target_position, target_value)
        # print(target_value, old_target_position, new_target_position, numbers)
    zero_position = numbers.index(0)
    return sum(numbers[(zero_position + i) % n] for i in range(1000, 4000, 1000))

    # def mix(initial_numbers: list[int]) -> list[int]:
    #     """Apply mixing algorithm."""
    #     n = len(initial_numbers)
    #     numbers: list[int] = initial_numbers.copy()
    #     print(numbers)
    #     for x in initial_numbers:
    #         x_from = numbers.index(x)
    #         x_to = (x_from + x) % n
    #         if x_from == x_to:
    #             assert x == 0
    #         elif x_from < x_to:
    #             numbers = (
    #                 numbers[:x_from]
    #                 + numbers[x_from + 1 : x_to + 1]
    #                 + [x]
    #                 + numbers[x_to + 1 :]
    #             )
    #         elif x_from > x_to:
    #             numbers = (
    #                 numbers[:x_to]
    #                 + [x]
    #                 + numbers[x_to + 1 : x_from]
    #                 + numbers[x_from + 1 :]
    #             )
    #         print(x, x_from, x_to, numbers)
    #     return numbers

    # def mix(numbers: list[int]) -> list[int]:
    #     """Apply mixing algorithm."""
    #     n = len(numbers)
    #     left: list[int] = [n - 1] + list(range(n - 1))
    #     right: list[int] = list(range(1, n)) + [0]
    #     for i, x in enumerate(numbers):
    #         if x > 0:  # Move to the right
    #             right[left[i]] = right[i]

    # return []


TEST_DATA: Final[
    str
] = """1
2
-3
3
-2
0
4"""

if __name__ == "__main__":
    testmod()
    input_numbers = [int(line) for line in fileinput.input()]
    c = Counter(input_numbers)
    for value, count in c.items():
        if count != 1:
            print(value, count)
    # print(mix(input_numbers))
