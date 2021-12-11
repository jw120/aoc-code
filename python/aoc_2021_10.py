"""Advent of Code 2021 - Day 10."""

from collections import deque
from doctest import testmod
from enum import Enum, auto
from sys import stdin
from typing import Dict, Set, Tuple


class Result(Enum):
    Illegal = auto()
    Incomplete = auto()


test: list[Tuple[str, Tuple[Result, str]]] = [
    ("[({(<(())[]>[[{[]{<()<>>", (Result.Incomplete, "}}]])})]")),
    ("[(()[<>])]({[<{<<[]>>(", (Result.Incomplete, ")}>]})")),
    ("{([(<{}[<>[]}>{[]{[(<()>", (Result.Illegal, "}")),
    ("(((({<>}<{<{<>}{[]{[]{}", (Result.Incomplete, "}}>}>))))")),
    ("[[<[([]))<([[{}[[()]]]", (Result.Illegal, ")")),
    ("[{[{({}]{}}([{[{{{}}([]", (Result.Illegal, "]")),
    ("{<[[]]>}<{[{[{[]{()[[[]", (Result.Incomplete, "]]}}]}]}>")),
    ("[<(<(<(<{}))><([]([]()", (Result.Illegal, ")")),
    ("<{([([[(<>()){}]>(<<{{", (Result.Illegal, ">")),
    ("<{([{{}}[<[[[<>{}]]]>[]]", (Result.Incomplete, "])}>")),
]

pairs: Dict[str, str] = {"(": ")", "[": "]", "{": "}", "<": ">"}
closes: Set[str] = set(pairs.values())
illegal_scores: Dict[str, int] = {")": 3, "]": 57, "}": 1197, ">": 25137}
completion_scores: Dict[str, int] = {")": 1, "]": 2, "}": 3, ">": 4}


def scan(s: str) -> Tuple[Result, str]:
    """Return Illegal and the first illegal character or Incomplete and the completion.

    >>> [scan(s) == r for s, r in test]
    [True, True, True, True, True, True, True, True, True, True]
    """
    stack: deque[str] = deque()
    for c in s:
        if c in pairs:
            stack.append(c)
        elif not stack or c not in closes or c != pairs[stack.pop()]:
            return (Result.Illegal, c)
    stack.reverse()
    return (Result.Incomplete, "".join(pairs[c] for c in stack))


def illegal_score(c: str) -> int:
    """Return score of the illegal character."""
    try:
        return illegal_scores[c]
    except KeyError:
        raise ValueError("Bad illegal character: " + c)


def completion_score(s: str) -> int:
    """Return score of the completion string.

    >>> [completion_score(c) for _, (r, c) in test if r == Result.Incomplete]
    [288957, 5566, 1480781, 995444, 294]
    """
    score = 0
    for c in s:
        score = score * 5 + completion_scores[c]
    return score


if __name__ == "__main__":
    testmod()
    lines: list[str] = stdin.read().splitlines()
    scans: list[Tuple[Result, str]] = [scan(s) for s in lines]
    print(sum(illegal_score(c) for r, c in scans if r == Result.Illegal))
    scores: list[int] = [
        completion_score(s) for r, s in scans if r == Result.Incomplete
    ]
    print(sorted(scores)[len(scores) // 2])
