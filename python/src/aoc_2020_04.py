"""Advent of Code 2020 - Day 4."""

import re
from doctest import testmod
from re import Pattern
from sys import stdin

Passport = dict[str, str]


def parse_passport(block: str) -> Passport:
    r"""Read a passport from a block of text.

    >>> parse_passport("eyr:2023 ecl:amb\niyr:2020")
    {'eyr': '2023', 'ecl': 'amb', 'iyr': '2020'}
    """
    return dict(item.split(":") for item in block.split())


tests_one: list[str] = [
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm",
    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929",
    "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm",
    "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in",
]


def valid_one(p: Passport) -> bool:
    r"""Test if the passport is valid under part one rules.

    >>> [valid_one(parse_passport(p)) for p in tests_one]
    [True, False, True, False]
    """
    return (
        "byr" in p
        and "iyr" in p
        and "eyr" in p
        and "hgt" in p
        and "hcl" in p
        and "ecl" in p
        and "pid" in p
    )  # Note "cid" is not required


tests_two: list[str] = [
    "eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
    "iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946",
    "hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
    "hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007",
    "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f",
    "eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
    "hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022",
    "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719",
]

date_pattern: Pattern[str] = re.compile(r"[0-9]{4}")
hgt_pattern: Pattern[str] = re.compile(r"([0-9]{2,3})(cm|in)")
hcl_pattern: Pattern[str] = re.compile(r"#[0-9a-f]{6}")
ecl_pattern: Pattern[str] = re.compile(r"amb|blu|brn|gry|grn|hzl|oth")
pid_pattern: Pattern[str] = re.compile(r"[0-9]{9}")


def valid_date(s: str, min_year: int, max_year: int) -> bool:
    r"""Is the string a valid (4-digit) year within the given range.

    >>> [valid_date(d, 2010, 2020) for d in ["Q", "10", "2010", "2015", "2020", "2021"]]
    [False, False, True, True, True, False]
    """
    if date_pattern.fullmatch(s):
        return min_year <= int(s) <= max_year
    return False


def valid_hgt(s: str) -> bool:
    r"""Is the string a valid height.

    >>> [valid_hgt(s) for s in ["120", "150cm", "194cm", "58in", "60in"]]
    [False, True, False, False, True]
    """
    if not (m := hgt_pattern.fullmatch(s)):
        return False
    if m.group(2) == "cm":
        return 150 <= int(m.group(1)) <= 193
    return 59 <= int(m.group(1)) <= 76


def valid_two(p: Passport) -> bool:
    r"""Test if the passport is valid under part two rules.

    >>> [valid_two(parse_passport(p)) for p in tests_two]
    [False, False, False, False, True, True, True, True]
    """
    return (
        valid_one(p)
        and valid_date(p["byr"], 1920, 2002)
        and valid_date(p["iyr"], 2010, 2020)
        and valid_date(p["eyr"], 2020, 2030)
        and valid_hgt(p["hgt"])
        and bool(hcl_pattern.fullmatch(p["hcl"]))
        and bool(ecl_pattern.fullmatch(p["ecl"]))
        and bool(pid_pattern.fullmatch(p["pid"]))
    )


if __name__ == "__main__":
    testmod()
    passports: list[Passport] = [parse_passport(block) for block in stdin.read().split("\n\n")]
    print(sum(valid_one(p) for p in passports))
    print(sum(valid_two(p) for p in passports))
