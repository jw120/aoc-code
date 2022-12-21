"""Advent of Code 2022 - Day 10."""

from doctest import testmod
from sys import stdin
from typing import Iterable


def run(instructions: Iterable[str], sample_cycles: set[int]) -> tuple[int, str]:
    """Run the instructions given.

    Returns the sum of the sample strengths for the given cycles and
    the crt pixels.

    >>> n, pixels = run(TEST_LONG, sample_cycles=selected_samples)
    >>> n
    13140
    >>> show_crt(pixels)
    ##..##..##..##..##..##..##..##..##..##..
    ###...###...###...###...###...###...###.
    ####....####....####....####....####....
    #####.....#####.....#####.....#####.....
    ######......######......######......####
    #######.......#######.......#######.....
    """
    cycle = 1
    x = 1
    accumulated_signal_strength = 0
    crt = ""
    for ins in instructions:
        match ins.split():
            case ["addx", d]:
                x_delta = int(d)
                cycles_needed = 2
            case ["noop"]:
                x_delta = 0
                cycles_needed = 1
            case _:
                raise ValueError(f"Bad instruction: '{ins}'")
        for _ in range(cycles_needed):
            # print(cycle, x)
            if cycle in sample_cycles:
                accumulated_signal_strength += cycle * x
            crt_position = (cycle - 1) % 40
            crt_pixel = "#" if abs(crt_position - x) <= 1 else "."
            crt = crt + crt_pixel
            # print(cycle, x, crt_pixel)
            cycle += 1
        x += x_delta

    return (accumulated_signal_strength, crt)


def show_crt(s: str) -> None:
    """Print string in crt format (6x40)."""
    for i in range(6):
        print(s[40 * i : 40 * (i + 1)])


# TEST_SHORT = ["noop", "addx 3", "addx -5", "noop"]

TEST_LONG_ENCODED = (
    "a15a-11a6a-3a5a-1a-8a13a4na-1a5a-1a5a-1a5a-1a5a-1a-35a1a24"
    "a-19a1a16a-11nna21a-15nna-3a9a1a-3a8a1a5nnnnna-36na1a7nnna2"
    "a6nnnnna1nna7a1na-13a13a7na1a-33nnna2nnna8na-1a2a1na17a-9a1"
    "a1a-3a11nna1na1nna-13a-19a1a3a26a-30a12a-1a3a1nnna-9a18a1a2"
    "nna9nnna-1a2a-37a1a3na15a-21a22a-6a1na2a1na-10nna20a1a2a2"
    "a-6a-11nnn"
)

TEST_LONG = (
    TEST_LONG_ENCODED.replace("a", "\naddx ")
    .replace("n", "\nnoop")
    .removeprefix("\n")
    .split("\n")
)

selected_samples = {20, 60, 100, 140, 180, 220}

if __name__ == "__main__":
    testmod()
    n, pixels = run(stdin.readlines(), sample_cycles=selected_samples)
    print(n)
    show_crt(pixels)
