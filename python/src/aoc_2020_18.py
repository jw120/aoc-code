"""Advent of Code 2020 - Day 18."""

import re
from doctest import testmod
from re import Pattern
from sys import stdin

item: Pattern[str] = re.compile(r"[0-9]+|[+*()]|\s+")


def apply(op: str, x: int, y: int) -> int:
    """Apply the operator to two integers."""
    if op == "+":
        return x + y
    if op == "*":
        return x * y
    raise RuntimeError("Unknown operator", op)


def expression1(s: str) -> int:
    """Evaluate an expression using part one rules.

    >>> expression1("1 + (2 * 3) + (4 * (5 + 6))")
    51
    >>> [expression1("2 * 3 + (4 * 5)"), expression1("5 + (8 * 3 + 9 + 3 * 4 * 3)")]
    [26, 437]
    >>> expression1("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
    12240
    >>> expression1("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
    13632
    """
    stack: list[str | int] = []
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
                and stack[-2] in "+*"
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


def expression2(s: str) -> int:
    """Evaluate an expression using part two rules.

    >>> [expression2("1 + 2 * 3 + 4 * 5 + 6"), expression2("1 + (2 * 3) + (4 * (5 + 6))")]
    [231, 51]
    >>> [expression2("2 * 3 + (4 * 5)"), expression2("5 + (8 * 3 + 9 + 3 * 4 * 3)")]
    [46, 1445]
    >>> expression2("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
    669060
    >>> expression2("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
    23340
    """
    term_list: list[str | int] = [
        int(x) if x.isdigit() else x for x in re.findall(item, re.sub(r"\s+", "", s))
    ]

    # Simplify as much as possible:
    while len(term_list) > 1:

        # Replace (X) with X
        for i in range(len(term_list) - 2):
            if term_list[i] == "(" and term_list[i + 2] == ")":
                term_list[i : i + 3] = [term_list[i + 1]]
                break

        # Find and apply the highest-priority available operation
        operations: list[tuple[int, int, int]] = []  # Position, priority, value
        paren_level: int = 0
        for i in range(len(term_list) - 2):
            if term_list[i] == "(":
                paren_level += 1
            elif term_list[i] == ")":
                paren_level -= 1
            else:
                i0, i1, i2 = term_list[i], term_list[i + 1], term_list[i + 2]
                if (
                    isinstance(i0, int)
                    and isinstance(i1, str)
                    and i1 in "+*"
                    and isinstance(i2, int)
                ):
                    priority = paren_level * 2 + (1 if i1 == "+" else 0)
                    operations.append((i, priority, apply(i1, i0, i2)))
        i, _, val = max(operations, key=lambda x: x[1])
        term_list[i : i + 3] = [val]

    if len(term_list) == 1 and isinstance(term_list[0], int):
        return term_list[0]
    raise RuntimeError("Expression failed", s, term_list)


if __name__ == "__main__":
    testmod()
    lines: list[str] = list(stdin)
    print(sum(expression1(line) for line in lines))
    print(sum(expression2(line) for line in lines))
