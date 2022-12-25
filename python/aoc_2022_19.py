"""Advent of Code 2022 - Day 19."""

from __future__ import annotations

# import fileinput
from dataclasses import dataclass
from doctest import testmod
import re
from typing import Any


@dataclass
class Amount:
    """Amount of each resource."""

    ore: int
    clay: int
    obsidian: int
    geode: int

    def __add__(self, other: Any) -> Amount:
        if isinstance(other, Amount):
            return Amount(
                ore=self.ore + other.ore,
                clay=self.clay + other.clay,
                obsidian=self.obsidian + other.obsidian,
                geode=self.geode + other.geode,
            )
        raise TypeError


def ore(x: int) -> Amount:
    """Create an amount of ore."""
    return Amount(ore=x, clay=0, obsidian=0, geode=0)


def clay(x: int) -> Amount:
    """Create an amount of clay."""
    return Amount(ore=0, clay=x, obsidian=0, geode=0)


def obsidian(x: int) -> Amount:
    """Create an amount of obsidian."""
    return Amount(ore=0, clay=0, obsidian=x, geode=0)


@dataclass
class BluePrint:
    """Holds resource requirements."""

    number: int
    ore_robot: Amount
    clay_robot: Amount
    obsidian_robot: Amount
    geode_robot: Amount


BLUEPRINT_REGEX = re.compile(
    r"""Blueprint\ (\d+)+\:\ #
        Each\ ore\ robot\ costs\ (\d+)\ ore\.\ #
        Each\ clay\ robot\ costs\ (\d+)\ ore\.\ #
        Each\ obsidian\ robot\ costs\ (\d+)\ ore\ and\ (\d+)\ clay\.\ #
        Each\ geode\ robot\ costs\ (\d+)\ ore\ and\ (\d+)\ obsidian\.""",
    re.VERBOSE,
)


def read_blueprint(s: str) -> BluePrint:
    """Read a cube.

    >>> b = read_blueprint(TEST_DATA.splitlines()[0])
    >>> b.ore_robot.ore
    4
    """
    m = re.fullmatch(BLUEPRINT_REGEX, s)
    assert m, f"Match failed '{s}'"
    return BluePrint(
        number=int(m.group(1)),
        ore_robot=ore(int(m.group(2))),
        clay_robot=ore(int(m.group(3))),
        obsidian_robot=ore(int(m.group(4))) + clay(int(m.group(5))),
        geode_robot=ore(int(m.group(6))) + obsidian(int(m.group(7))),
    )


TEST_DATA = (
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. "
    "Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\n"
    "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. "
    "Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."
)

if __name__ == "__main__":
    testmod()
    # input_cubes = [read_cube(line) for line in fileinput.input()]
    # print(free_surface(input_cubes))
    # print(exterior_surface(input_cubes))
