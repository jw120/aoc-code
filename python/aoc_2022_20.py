"""Advent of Code 2022 - Day 20."""


import fileinput
from doctest import testmod
from typing import Final


def mix(initial_numbers: list[int]) -> int:
    """Apply mixing algorithm.

    >>> test_numbers = [int(s) for s in TEST_DATA.splitlines()]
    >>> mix(test_numbers)
    3
    """
    n = len(initial_numbers)
    # We replace numbers with a (number, index) tuple to cope with repeated values
    start_tuples: list[tuple[int, int]] = list(zip(initial_numbers, range(n)))
    number_tuples: list[tuple[int, int]] = start_tuples.copy()
    for target_tuple in start_tuples:
        assert len(set(number_tuples)) == len(number_tuples)
        target_value, _ = target_tuple
        old_target_position = number_tuples.index(target_tuple)
        del number_tuples[old_target_position : old_target_position + 1]
        new_target_position = (old_target_position + target_value) % (n - 1)
        number_tuples.insert(new_target_position, target_tuple)
        # print(target_value, old_target_position, new_target_position, numbers)
    initial_zero_position = initial_numbers.index(0)
    assert initial_numbers.count(0) == 1
    final_zero_position = number_tuples.index((0, initial_zero_position))
    return sum(
        number_tuples[(final_zero_position + i) % n][0] for i in range(1000, 4000, 1000)
    )


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
    print(mix(input_numbers))
