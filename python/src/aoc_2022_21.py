"""Advent of Code 2022 - Day 21."""

from __future__ import annotations

from dataclasses import dataclass
from doctest import testmod
from enum import Enum
from re import fullmatch
from sys import stdin
from typing import NewType


class Operation(Enum):
    """Operations that monkeys use."""

    ADD = "ADD"
    SUB = "SUB"
    MUL = "MUL"
    DIV = "DIV"


def apply(x: int, op: Operation, y: int) -> int:
    """Apply an operation to two values."""

    match op:
        case Operation.ADD:
            return x + y
        case Operation.SUB:
            return x - y
        case Operation.MUL:
            return x * y
        case Operation.DIV:
            assert x % y == 0
            return x // y


MonkeyName = NewType("MonkeyName", str)


@dataclass
class OpMonkey:
    """Monkey whose job is to do a mathematical operation."""

    left: MonkeyName
    op: Operation
    right: MonkeyName


Monkey = int | OpMonkey

OP_TABLE = {
    "+": Operation.ADD,
    "-": Operation.SUB,
    "*": Operation.MUL,
    "/": Operation.DIV,
}


def read_monkey(s: str) -> tuple[MonkeyName, Monkey]:
    """Parse a monkey returning its name and value.

    >>> read_monkey("cczh: sllz + lgvd")
    ('cczh', OpMonkey(left='sllz', op=<Operation.ADD: 'ADD'>, right='lgvd'))
    >>> read_monkey("dvpt: 3")
    ('dvpt', 3)
    """
    assert (m := fullmatch(r"([a-z]+)\: (.+)", s.strip())), f"Parse failed: '{s}'"
    name, rest = m.groups()
    if rest.isdigit():
        return (MonkeyName(name), int(rest))
    assert (m := fullmatch(r"([a-z]+) (.) ([a-z]+)", rest)), f"Parse rest failed: '{s}'"
    left, op_str, right = m.groups()
    return (
        MonkeyName(name),
        OpMonkey(left=MonkeyName(left), op=OP_TABLE[op_str], right=MonkeyName(right)),
    )


def eval_monkey(
    name: MonkeyName, monkeys: dict[MonkeyName, Monkey], stop_on_humn: bool
) -> int | None:
    """Evaluate the value of the given monkey.

    Flag makes the eval return None if evaluation leads to a "humn" node.

    >>> test = dict(read_monkey(line) for line in TEST_DATA.splitlines())
    >>> eval_monkey("root", test, False)
    152
    >>> eval_monkey("root", test, True) is None
    True
    """
    if stop_on_humn and name == "humn":
        return None
    match monkeys[name]:
        case int(x):
            return x
        case OpMonkey(left, op, right):
            match eval_monkey(left, monkeys, stop_on_humn), eval_monkey(
                right, monkeys, stop_on_humn
            ):
                case int(left_val), int(right_val):
                    return apply(left_val, op, right_val)
                case _:
                    return None
        case _:
            raise ValueError("Bad monkey value")


def solve(monkeys: dict[MonkeyName, Monkey]) -> int:
    """Return value for 'humn' monkey that makes 'root' work as an equality.

    >>> test_monkeys = dict(read_monkey(line) for line in TEST_DATA.splitlines())
    >>> solve(test_monkeys)
    301
    """
    root_monkey = monkeys[MonkeyName("root")]
    assert isinstance(root_monkey, OpMonkey)

    # Root has two branches, one gives the target value the other needs to be set to the same
    # value which we will trace down
    match eval_monkey(root_monkey.left, monkeys, stop_on_humn=True), eval_monkey(
        root_monkey.right, monkeys, stop_on_humn=True
    ):
        case int(left_val), None:
            root_target_value = left_val
            current_monkey_name = root_monkey.right
        case None, int(right_val):
            root_target_value = right_val
            current_monkey_name = root_monkey.left
        case _:
            raise ValueError("Bad root note")

    current_value: int = root_target_value
    while True:
        if current_monkey_name == "humn":
            return current_value
        current_monkey = monkeys[current_monkey_name]
        assert isinstance(current_monkey, OpMonkey)
        match eval_monkey(current_monkey.left, monkeys, stop_on_humn=True), eval_monkey(
            current_monkey.right, monkeys, stop_on_humn=True
        ):
            case int(left_val), None:
                val = left_val
                next_monkey_name = current_monkey.right
                next_left = False
            case None, int(right_val):
                val = right_val
                next_monkey_name = current_monkey.left
                next_left = True
            case _:
                raise ValueError("Unexpected")
        if current_monkey.op == Operation.ADD:
            current_value = current_value - val
        elif current_monkey.op == Operation.SUB and next_left:
            current_value = val + current_value
        elif current_monkey.op == Operation.SUB and not next_left:
            current_value = val - current_value
        elif current_monkey.op == Operation.MUL:
            assert current_value % val == 0
            current_value = current_value // val
        elif current_monkey.op == Operation.DIV and not next_left:
            assert current_value % val == 0
            current_value = val // current_value
        elif current_monkey.op == Operation.DIV and next_left:
            current_value = val * current_value
        else:
            raise ValueError("Bad op")
        current_monkey_name = next_monkey_name


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
    print(eval_monkey(MonkeyName("root"), input_monkeys, False))
    print(solve(input_monkeys))
