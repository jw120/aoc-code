"""Advent of Code 2021 - Day 14."""

from collections import Counter
from doctest import testmod
from sys import stdin

# We treat the polymers as counts of pairs and then the rules
# change from AB -> X as AB -> AX, XB

Pair = str
Polymer = tuple[Counter[Pair], str]  # Counts of pairs plus the last monomer
Rules = dict[Pair, tuple[Pair, Pair]]


def apply(polymer: Polymer, rules: Rules, n: int) -> Polymer:
    """Apply the rules to the polymer n times.

    >>> apply(test_template, test_rules, 1)
    (Counter({'NC': 1, 'CN': 1, 'NB': 1, 'BC': 1, 'CH': 1, 'HB': 1}), 'B')
    """
    polymer_counts, polymer_last = polymer
    new_polymer_counts: Counter[Pair] = Counter()

    for _ in range(n):
        new_polymer_counts = Counter()
        for pair in polymer_counts:
            p, q = rules[pair]
            new_polymer_counts[p] += polymer_counts[pair]
            new_polymer_counts[q] += polymer_counts[pair]
        polymer_counts = new_polymer_counts
    return (new_polymer_counts, polymer_last)


def most_least(p: Polymer) -> int:
    """Return count of most common monomer less count of least common monomer.

    >>> most_least(apply(test_template, test_rules, 10))
    1588
    """
    p_counts, p_last = p
    monomer_counts: Counter[str] = Counter()
    for pair, count in p_counts.items():
        monomer_counts[pair[0]] += count
    monomer_counts[p_last] += 1
    monomers_by_count = monomer_counts.most_common()
    most = monomers_by_count[0][1]
    least = monomers_by_count[-1][1]
    return most - least


def read_template(s: str) -> Polymer:
    """Read a polymer template.

    >>> read_template("BALL")
    (Counter({'BA': 1, 'AL': 1, 'LL': 1}), 'L')
    """
    return (Counter(a + b for a, b in zip(s, s[1:])), s[-1])


def read_rules(ss: list[str]) -> Rules:
    """Read the pair insertion rules.

    >>> read_rules(["AB -> C", "DE -> F"])
    {'AB': ('AC', 'CB'), 'DE': ('DF', 'FE')}
    """
    r: Rules = {}
    for s in ss:
        before, new = s.split(" -> ")
        r[before] = (before[0] + new, new + before[1])
    return r


# cspell:disable
test_template: Polymer = read_template("NNCB")
# cspell:enable
test_rules: Rules = read_rules(
    [
        "CH -> B",
        "HH -> N",
        "CB -> H",
        "NH -> C",
        "HB -> C",
        "HC -> B",
        "HN -> C",
        "NN -> C",
        "BH -> H",
        "NC -> B",
        "NB -> B",
        "BN -> B",
        "BB -> N",
        "BC -> B",
        "CC -> N",
        "CN -> C",
    ]
)


if __name__ == "__main__":
    testmod()
    lines = stdin.read().splitlines()
    template = read_template(lines[0])
    input_rules = read_rules(lines[2:])
    print(most_least(apply(template, input_rules, 10)))
    print(most_least(apply(template, input_rules, 40)))
