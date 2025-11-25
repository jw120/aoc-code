"""Advent of Code 2021 - Day 8."""

from collections import Counter
from doctest import testmod
from sys import stdin
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from collections.abc import Iterator

# spell-checker:disable

test1: str = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
test2: list[str] = [
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce",
]
# spell-checker:enable


# Mapping from digits to number of segments:
#
# Digit     Number of segments
# 0             6
# 1             2 (only digit with 2 segments)
# 2             5
# 3             5
# 4             4 (only digit with 4 segments)
# 5             5
# 6             6
# 7             3 (only digit with 3 segments)
# 8             7 (only digit with 7 segments)
# 9             6
#
# Mapping from number of segments to digits
# 2 -> 1
# 3 -> 7
# 4 -> 4
# 5 -> 2/3/5
# 6 -> 0/6/9
# 7 -> 8

# Mapping from segments to digits in which the are present
# seg_dig = {'a': set([0, 2, 3, 5, 6, 7, 8, 9]), 'b': set([0, 4])

# spell-checker:disable
segments_to_digit: dict[str, int] = {
    "abcefg": 0,
    "cf": 1,
    "acdeg": 2,
    "acdfg": 3,
    "bcdf": 4,
    "abdfg": 5,
    "abdefg": 6,
    "acf": 7,
    "abcdefg": 8,
    "abcdfg": 9,
}
# spell-checker:enable


def num_output_1478(lines: list[str]) -> int:
    """Count number of times digits 1478 appear in outputs.

    >>> num_output_1478(test2)
    26
    """
    return sum(len(word) in {2, 3, 4, 7} for line in lines for word in line.split(" | ")[1].split())


def solve(line: str) -> int:
    """Return the 4-digit output from the given set of inputs.

    >>> solve(test1)
    5353
    >>> sum(solve(line) for line in test2)
    61229
    """
    signals, outputs = (part.split() for part in line.split(" | "))

    # To start with every wire could be any segment
    wire_to_segment: dict[str, set[str]] = {x: set("abcdefg") for x in "abcdefg"}

    def match_wires(signal: str, segments: str) -> None:
        """Match the wires in the signal to the given segments.

        Any wire in the signal must be one of the segments. Any wire not in the signal
        cannot be one of these segments.
        """
        for wire in "abcdefg":
            if wire in signal:
                wire_to_segment[wire] &= set(segments)
            else:
                wire_to_segment[wire] -= set(segments)

    for signal in signals:
        if len(signal) == 2:  # Must be a '1' which has segments c & f
            match_wires(signal, "cf")
        elif len(signal) == 3:  # Must be a '7' which has segments a, c & f
            match_wires(signal, "acf")
        elif len(signal) == 4:  # Must be a '4' which has segments b, c, d, f
            match_wires(signal, "bcdf")

    wire_counts = Counter("".join(signals))
    for wire in "abcdefg":
        if wire_counts[wire] == 4:  # Must be 'e' segment which only appears in 4 digits
            match_wires(wire, "e")
        elif wire_counts[wire] == 6:
            match_wires(wire, "b")
        elif wire_counts[wire] == 9:
            match_wires(wire, "f")

    return digits_to_int(
        segments_to_digit[
            "".join(sorted(segment for wire in output for segment in wire_to_segment[wire]))
        ]
        for output in outputs
    )


def digits_to_int(xs: Iterator[int]) -> int:
    """Convert digits into an int."""
    a, b, c, d = xs
    return 1000 * a + 100 * b + 10 * c + d


if __name__ == "__main__":
    testmod()
    input_lines: list[str] = stdin.read().splitlines()
    print(num_output_1478(input_lines))
    print(sum(solve(line) for line in input_lines))
