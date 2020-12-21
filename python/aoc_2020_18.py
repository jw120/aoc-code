"""Advent of Code 2020 - Day 18."""

import re
from doctest import testmod
from sys import stdin
from typing import List, Pattern, Union

item: Pattern[str] = re.compile(r"[0-9]+|[-+*/()]|\s+")


def apply(op: str, x: int, y: int) -> int:
    if op == "+":
        return x + y
    elif op == "-":
        return x - y
    elif op == "*":
        return x * y
    elif op == "/":
        return x // y
    raise RuntimeError("Unknown operator", op)


def expression(s: str) -> int:
    """Evaluate an expression.

    >>> expression("1 + (2 * 3) + (4 * (5 + 6))")
    51
    >>> [expression("2 * 3 + (4 * 5)"), expression("5 + (8 * 3 + 9 + 3 * 4 * 3)")]
    [26, 437]
    >>> expression("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
    12240
    >>> expression("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
    13632
    """
    stack: List[Union[str, int]] = []
    for term in re.findall(item, re.sub(r"\s+", "", s)):
        # Add term to our stack
        stack.append(int(term) if term.isdigit() else term)
        # Simplify as much as possible:
        while True:
            # Replace: int op int with result
            if (
                len(stack) >= 3
                and isinstance(stack[-3], int)
                and isinstance(stack[-2], str)
                and stack[-2] in "+-*/"
                and isinstance(stack[-1], int)
            ):
                stack[-3:] = [apply(stack[-2], int(stack[-3]), int(stack[-1]))]
            # Replace: ( val ) with val
            elif len(stack) >= 3 and stack[-3] == "(" and stack[-1] == ")":
                stack[-3:] = [stack[-2]]
            else:
                break
    if len(stack) == 1 and isinstance(stack[0], int):
        return stack[0]
    raise RuntimeError("Expression failed", s, stack)


if __name__ == "__main__":
    testmod()
    print(sum(expression(line) for line in stdin))
