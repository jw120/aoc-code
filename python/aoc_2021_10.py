"""Advent of Code 2021 - Day 10."""

from collections import deque
from doctest import testmod
from sys import stdin
from typing import Dict, Optional


test: list[str] = [
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]",
]


pairs: Dict[str, str] = {"(": ")", "[": "]", "{": "}", "<": ">"}


def first_illegal(s: str) -> Optional[str]:
    """Return the first illegal character, or None if pattern is incomplete.

    >>> [first_illegal(s) for s in test]
    [None, None, '}', None, ')', ']', None, ')', '>', None]
    """
    stack: deque[str] = deque()
    for c in s:
        if c in pairs:
            stack.append(c)
        elif not stack:
            return None
        elif c != pairs[stack.pop()]:
            return c
    return None


scores: Dict[str, int] = {")": 3, "]": 57, "}": 1197, ">": 25137}


def score(c: str) -> int:
    """Return score of the illegal character."""
    try:
        return scores[c]
    except KeyError:
        raise ValueError("Bad illegal character: " + c)


if __name__ == "__main__":
    testmod()
    lines = stdin.read().splitlines()
    print(sum(score(p) for p in (first_illegal(s) for s in lines) if p is not None))
