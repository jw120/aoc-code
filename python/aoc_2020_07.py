"""Advent of Code 2020 - Day 7."""

import re
from doctest import testmod
from sys import stdin
from typing import Dict, List, Match, Optional, Pattern, Set, Tuple


rule_divider: Pattern[str] = re.compile(" bags?[,\\.] ?")
bag_match: Pattern[str] = re.compile("([0-9]+) (.+)")

Rule = Tuple[str, List[Tuple[str, int]]]


def parse_rule(s: str) -> Rule:
    """Parse an input rule.

    >>> parse_rule('muted blue bags contain 1 dull violet bag.')
    ('muted blue', [('dull violet', 1)])
    >>> parse_rule('clear blue bags contain 2 plaid gold bags, 5 mirrored white bags.')
    ('clear blue', [('plaid gold', 2), ('mirrored white', 5)])
    >>> parse_rule('dim coral bags contain no other bags.')
    ('dim coral', [])
    """
    [outer_bag, inner_bag_text] = s.split(" bags contain ")
    if inner_bag_text.strip() == "no other bags.":
        return (outer_bag, [])
    inner_bags = re.split(rule_divider, inner_bag_text)
    return (outer_bag, [parse_content(b) for b in inner_bags[:-1]])


def parse_content(s: str) -> Tuple[str, int]:
    """Parse a bag from the contents part of a rule.

    >>> parse_content('2 plaid gold')
    ('plaid gold', 2)
    """
    m: Optional[Match[str]] = re.match(bag_match, s)
    if not m:
        raise RuntimeError("Cannot parse contents", s)
    return (m.group(2), int(m.group(1)))


test_rules: List[Rule] = [
    parse_rule(r)
    for r in [
        "light red bags contain 1 bright white bag, 2 muted yellow bags.",
        "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
        "bright white bags contain 1 shiny gold bag.",
        "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
        "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
        "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
        "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
        "faded blue bags contain no other bags.",
        "dotted black bags contain no other bags.",
    ]
]


def part_one(rules: List[Rule], target: str) -> int:
    """Return how many different bags can include the target bag.

    >>> part_one(test_rules, "shiny gold")
    4
    """
    # Read all the rules into a dict showing what a bag can be insider
    # can_be_inside[x] = {a, b, c}
    # means bag colour can be insider bags of colours a, b or c
    can_be_inside: Dict[str, Set[str]] = {}
    for (outer, contents) in rules:
        for (inner, _) in contents:
            can_be_inside.setdefault(inner, set()).add(outer)
    # Walk backwards from the target bag
    visited: Set[str] = set()
    frontier: Set[str] = can_be_inside[target]
    while frontier:
        new_frontier: Set[str] = set()
        for bag in frontier:
            visited.add(bag)
            if bag in can_be_inside:
                new_frontier |= can_be_inside[bag]
        frontier = new_frontier - visited
    return len(visited)


def part_two(rules: List[Rule], target: str) -> int:
    """Return the number of bags inside.

    >>> part_two(test_rules, "shiny gold")
    32
    """
    count_inside: Dict[str, int] = {}  # Cached results from bags_insider

    def bags_inside(bag: str) -> int:
        if bag not in count_inside:
            count: int = 0
            for (inner, num) in has_inside[bag]:
                count += num * (1 + bags_inside(inner))
            count_inside[bag] = count
        return count_inside[bag]

    # Read the rules into a Dict
    has_inside: Dict[str, List[Tuple[str, int]]] = {}
    for (outer, inners) in rules:
        has_inside[outer] = inners

    return bags_inside(target)


if __name__ == "__main__":
    testmod()
    rules: List[Rule] = [parse_rule(line) for line in stdin]
    print(part_one(rules, "shiny gold"))
    print(part_two(rules, "shiny gold"))
