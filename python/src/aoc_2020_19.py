"""Advent of Code 2020 - Day 19."""

import re
from collections.abc import Iterable
from dataclasses import dataclass
from doctest import testmod
from sys import stdin


@dataclass(frozen=True)
class Terminal:
    """Terminal class."""

    val: str

    def __str__(self) -> str:
        return '"' + self.val + '"'


@dataclass(frozen=True)
class Series:
    """Series class."""

    rule_indices: list[int]

    def __str__(self) -> str:
        return " ".join(str(r) for r in self.rule_indices)


@dataclass(frozen=True)
class Alternate:
    """Alternate class."""

    a: Series
    b: Series

    def __str__(self) -> str:
        return f"{self.a} | {self.b}"


Rule = Terminal | Series | Alternate


def parse_rule(s: str) -> tuple[int, Rule]:
    """Convert a string into a rule with index.

    >>> parse_rule("1: 2 3")
    (1, Series(rule_indices=[2, 3]))
    """
    index_str, rule_str = s.split(": ")
    index = int(index_str)
    if m := re.fullmatch(r'"(.)"', rule_str):
        return (index, Terminal(m.group(1)))
    if m := re.fullmatch(r"([0-9 ]+)\|([0-9 ]+)", rule_str):
        return (
            index,
            Alternate(Series(parse_ints(m.group(1))), Series(parse_ints(m.group(2)))),
        )
    return (index, Series(parse_ints(rule_str)))


def parse_ints(s: str) -> list[int]:
    """Convert a string of space-separated integers into a list.

    >>> parse_ints("1 2 3")
    [1, 2, 3]
    """
    return [int(x) for x in s.strip().split(" ")]


class MessageValidator:
    """Main class for day 19.

    >>> mv = MessageValidator(test_rules.splitlines()) # spell-checker: disable
    >>> inputs = ["abz", "aaww", "baqq", "bby", "", "cz"]
    >>> [mv.valid_remains(s, mv.rules[3]) for s in inputs]
    ['z', None, 'qq', None, None, None]
    """

    # spell-checker: enable

    def __init__(self, rules: Iterable[str]) -> None:
        self.rules: dict[int, Rule] = dict(parse_rule(r) for r in rules)

    def show(self) -> None:
        """Print debugging information."""
        for i, r in self.rules.items():
            print(f"{i}: {r}")

    def valid(self, s: str) -> bool:
        """Test validity."""
        return not self.valid_remains(s, self.rules[0])

    def valid_remains(self, s: str, r: Rule) -> str | None:
        """Return the remaining string if the rule matches."""
        if isinstance(r, Terminal):
            if s.startswith(r.val):
                return s.removeprefix(r.val)
            return None
        if isinstance(r, Alternate):
            if (remainder := self.valid_remains(s, r.a)) is not None:
                return remainder
            return self.valid_remains(s, r.b)
        for i in r.rule_indices:
            remainder = self.valid_remains(s, self.rules[i])
            if remainder is None:
                return None
            s = remainder
        return s

    def valid_part_two(self, s: str) -> bool:
        """Match starting with special part two rules.

        We have the following new rules:

        0: 8 11
        8: 42 | 8
        11: 42 31 | 42 11 31

        So, 8 = 42+ and 11 = 42+31+ (with an equal number of matches), so
        0 matches 42+31+ with more matches of 42 than 31.

        No other rule refers to 0/8/11 so we can treat this as a special top-level rule and try
        matching for n 42s followed by upto (n-1) 31s for increasing n
        """
        count_42 = 0
        s_remainder = s
        while True:
            remainder_42 = self.valid_remains(s_remainder, self.rules[42])
            if remainder_42 is None:
                return False
            count_42 += 1
            count_31 = 0
            s_remainder = remainder_42
            remainder = remainder_42
            while count_31 < count_42 - 1:
                remainder_31 = self.valid_remains(remainder, self.rules[31])
                if remainder_31 is None:
                    break
                if not remainder_31:
                    return True
                remainder = remainder_31
                count_31 += 1


# spell-checker: disable

TEST_DATA = """0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb"""
test_rules, test_messages = TEST_DATA.split("\n\n")

# spell-checker: enable


if __name__ == "__main__":
    testmod()
    input_rules, input_messages = stdin.read().split("\n\n")
    mv = MessageValidator(input_rules.splitlines())
    print(sum(mv.valid(message) for message in input_messages.splitlines()))
    print(sum(mv.valid_part_two(message) for message in input_messages.splitlines()))
