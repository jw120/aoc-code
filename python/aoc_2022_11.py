"""Advent of Code 2022 - Day 11."""

from dataclasses import dataclass
from doctest import testmod
from functools import reduce
from re import fullmatch
from sys import stdin
from typing import Callable, Optional


@dataclass
class Monkey:
    items: list[int]
    update: Callable[[int], int]  # function old -> new
    divisor: int
    dest_true: int
    dest_false: int
    count: int


def fullmatch_item(r: str, s: str) -> str:
    """Return first match group from regexp fullmatch.

    Fails is no match or more than one group.
    """
    m = fullmatch(r, s)
    assert m is not None, f"Failed to match '{r}' with '{s}'"
    assert m.lastindex == 1
    return m.group(1)


def read_monkey(expected_index: int, s: str) -> Monkey:
    """Read monkey from string.

    Checks monkey index matches expected index.
    """
    lines = s.splitlines()
    assert len(lines) == 6
    index = int(fullmatch_item(r"Monkey (\d+):", lines[0]))
    assert index == expected_index
    items = [
        int(s) for s in fullmatch_item(r"  Starting items: (.+)", lines[1]).split(", ")
    ]
    match fullmatch_item(r"  Operation: new = old (.+)+", lines[2]).split():
        case ["+", d] if d.isdigit():
            update: Callable[[int], int] = lambda x: x + int(d)
        case ["*", d] if d.isdigit():
            update = lambda x: x * int(d)
        case ["*", "old"]:
            update = lambda x: x * x
        case _:
            raise ValueError(f"Bad line: '{lines[2]}'")
    divisor = int(fullmatch_item(r"  Test: divisible by (\d+)", lines[3]))
    dest_true = int(fullmatch_item(r"    If true: throw to monkey (\d+)", lines[4]))
    assert dest_true != index
    dest_false = int(fullmatch_item(r"    If false: throw to monkey (\d+)", lines[5]))
    assert dest_false != index

    return Monkey(
        items=items,
        update=update,
        divisor=divisor,
        dest_true=dest_true,
        dest_false=dest_false,
        count=0,
    )


def step(monkeys: list[Monkey], steps: int, mode: Optional[int]) -> list[Monkey]:
    """
    Update (mutating) monkeys given number of times.

    If mode is present then we take all items modules its value, if not
    then all items are divided by 3 (for the first part of the problem).

    >>> show_items(step(build_monkeys(test_input), 1, None))
    Monkey 0: 20, 23, 27, 26
    Monkey 1: 2080, 25, 167, 207, 401, 1046
    Monkey 2:
    Monkey 3:
    >>> show_items(step(build_monkeys(test_input), 20, None))
    Monkey 0: 10, 12, 14, 26, 34
    Monkey 1: 245, 93, 53, 199, 115
    Monkey 2:
    Monkey 3:
    """
    for _ in range(steps):
        for monkey in monkeys:
            for item in monkey.items:
                new_item = monkey.update(item)
                if mode is None:
                    new_item = new_item // 3
                else:
                    new_item = new_item % mode
                dest = (
                    monkey.dest_true
                    if new_item % monkey.divisor == 0
                    else monkey.dest_false
                )
                monkeys[dest].items.append(new_item)
                monkey.count += 1
            monkey.items = []
    return monkeys


def monkey_business(monkeys: list[Monkey], steps: int, simple_mode: bool) -> int:
    """Run number of steps and return most active monkeys.

    Simple mode is for first part of the problem where items are divided by 3. For
    non-simple mode we treat all numbers modulus the product of all the divisors.

    >>> monkey_business(build_monkeys(test_input), 20, True)
    10605
    >>> monkey_business(build_monkeys(test_input), 10000, False)
    2713310158
    """
    if simple_mode:
        mode = None
    else:
        mode = reduce(lambda x, y: x * y, (m.divisor for m in monkeys))
    step(monkeys, steps, mode)
    counts = sorted([m.count for m in monkeys])
    return counts[-1] * counts[-2]


def show_items(monkeys: list[Monkey]) -> None:
    for i, m in enumerate(monkeys):
        if m.items:
            print(f"Monkey {i}:", ", ".join(str(x) for x in m.items))
        else:
            print(f"Monkey {i}:")


test_input = """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"""


def build_monkeys(s: str) -> list[Monkey]:
    """Create fresh set of monkeys from the input."""
    return [read_monkey(i, block) for i, block in enumerate(s.split("\n\n"))]


if __name__ == "__main__":
    testmod()
    input = stdin.read()
    print(monkey_business(build_monkeys(input), 20, True))
    print(monkey_business(build_monkeys(input), 10000, False))
