"""Advent of Code 2022 - Day 21."""

from __future__ import annotations

from dataclasses import dataclass
from doctest import testmod
from re import fullmatch
from sys import stdin
from typing import Callable
import operator

Name = str
NumberMonkey = int


@dataclass
class OpMonkey:
    """Monkey whose job is to do a mathematical operation."""

    left: Name
    op: Callable[[int, int], int]
    right: Name


Monkey = NumberMonkey | OpMonkey

OP_TABLE = {
    "+": operator.add,
    "-": operator.sub,
    "*": operator.mul,
    "/": operator.floordiv,
}


def read_monkey(s: str) -> tuple[Name, Monkey]:
    """Parse a monkey returning its name and value.

    >>> read_monkey("cczh: sllz + lgvd")
    ('cczh', OpMonkey(left='sllz', op=<built-in function add>, right='lgvd'))
    >>> read_monkey("dvpt: 3")
    ('dvpt', 3)
    """
    assert (m := fullmatch(r"([a-z]+)\: (.+)", s.strip())), f"Parse failed: '{s}'"
    name, rest = m.groups()
    if rest.isdigit():
        return (name, int(rest))
    assert (m := fullmatch(r"([a-z]+) (.) ([a-z]+)", rest)), f"Parse rest failed: '{s}'"
    left, op_str, right = m.groups()
    return (name, OpMonkey(left=left, op=OP_TABLE[op_str], right=right))


def eval_monkey(name: Name, monkeys: dict[Name, Monkey]) -> int:
    """Evaluate the value of the given monkey.

    >>> input_monkeys = dict(read_monkey(line) for line in TEST_DATA.splitlines())
    >>> print(eval_monkey("root", input_monkeys))
    152
    """
    match monkeys[name]:
        case int(x):
            return x
        case OpMonkey(left, op, right):
            return op(eval_monkey(left, monkeys), eval_monkey(right, monkeys))
        case _:
            raise ValueError("Bad monkey value")


TEST_DATA = """root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"""


if __name__ == "__main__":
    testmod()
    input_monkeys = dict(read_monkey(line) for line in stdin.readlines())
    print(eval_monkey("root", input_monkeys))
