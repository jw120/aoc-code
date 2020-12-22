"""Advent of Code 2020 - Day 19."""

import re
from dataclasses import dataclass
from doctest import testmod
from sys import stdin
from typing import Dict, Iterable, List, NoReturn, Optional, Tuple, Union


def assert_never(value: NoReturn) -> NoReturn:
    assert False, f"Unhandled value: {value} ({type(value).__name__})"


@dataclass(frozen=True)
class Terminal:
    val: str

    def __str__(self) -> str:
        return '"' + self.val + '"'


@dataclass(frozen=True)
class Series:
    rule_indices: List[int]

    def __str__(self) -> str:
        return " ".join(str(r) for r in self.rule_indices)


@dataclass(frozen=True)
class Alternate:
    a: Series
    b: Series

    def __str__(self) -> str:
        return f"{self.a} | {self.b}"


Rule = Union[Terminal, Series, Alternate]


def parse_rule(s: str) -> Tuple[int, Rule]:
    """Convert a string into a rule with index.

    >>> parse_rule("1: 2 3")
    (1, Series(rule_indices=[2, 3]))
    """
    index_str, rule_str = s.split(": ")
    index = int(index_str)
    if m := re.fullmatch(r'"(.)"', rule_str):
        return (index, Terminal(m.group(1)))
    elif m := re.fullmatch(r"([0-9 ]+)\|([0-9 ]+)", rule_str):
        return (
            index,
            Alternate(Series(parse_ints(m.group(1))), Series(parse_ints(m.group(2)))),
        )
    return (index, Series(parse_ints(rule_str)))


def parse_ints(s: str) -> List[int]:
    """Convert a string of space-separated integers into a list.

    >>> parse_ints("1 2 3")
    [1, 2, 3]
    """
    return [int(x) for x in s.strip().split(" ")]


class MessageValidator:
    def __init__(self, rules: Iterable[str]) -> None:
        self.rules: Dict[int, Rule] = {}
        for rule_str in rules:
            i, rule = parse_rule(rule_str)
            self.rules[i] = rule

    def show(self) -> None:
        for i, r in self.rules.items():
            print(f"{i}: {r}")

    def valid(self, s: str) -> bool:
        return self.valid_remains(s, self.rules[0]) == ""

    def valid_remains(self, s: str, r: Rule) -> Optional[str]:
        """Return the remaining string if the rule matches."""
        if isinstance(r, Terminal):
            if s.startswith(r.val):
                return s.removeprefix(r.val)
            return None
        if isinstance(r, Alternate):
            if (remainder := self.valid_remains(s, r.a)) is not None:
                return remainder
            return self.valid_remains(s, r.b)
        if isinstance(r, Series):
            for i in r.rule_indices:
                remainder = self.valid_remains(s, self.rules[i])
                if remainder is None:
                    return None
                s = remainder
            return s
        assert_never(r)


test1 = MessageValidator(["0: 1 2", '1: "a"', "2: 1 3 | 3 1", '3: "b"'])


if __name__ == "__main__":
    testmod()
    rules, messages = stdin.read().split("\n\n")
    mv = MessageValidator(rules.splitlines())
    print(sum(mv.valid(message) for message in messages.splitlines()))
