"""Advent of Code 2022 - Day 19."""

from __future__ import annotations

# import fileinput
from collections import deque
from dataclasses import dataclass
from doctest import testmod
import re
from typing import Any, Optional


@dataclass(frozen=True)
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

    def __sub__(self, other: Any) -> Amount:
        if isinstance(other, Amount):
            return Amount(
                ore=self.ore - other.ore,
                clay=self.clay - other.clay,
                obsidian=self.obsidian - other.obsidian,
                geode=self.geode - other.geode,
            )
        raise TypeError

    def __ge__(self, other: Any) -> bool:
        if isinstance(other, Amount):
            return (
                self.ore >= other.ore
                and self.clay >= other.clay
                and self.obsidian >= other.obsidian
                and self.geode >= other.geode
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


def geode(x: int) -> Amount:
    """Create an amount of geode."""
    return Amount(ore=0, clay=0, obsidian=0, geode=x)


@dataclass(frozen=True)
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


@dataclass(frozen=True)
class State:
    """Track state for path search."""

    resources: Amount
    robots: Amount
    path: list[str]
    time: int

    def buy(self, cost: Amount, robot: Amount, name: str) -> Optional[State]:
        """Return a new state after buying the given robot if possible."""
        if self.resources >= cost:
            return State(
                resources=self.resources + self.robots - cost,
                robots=self.robots + robot,
                path=self.path + [name],
                time=self.time - 1,
            )
        return None

    def wait(self) -> State:
        """Return a new state after one step of waiting."""
        return State(
            resources=self.resources + self.robots,
            robots=self.robots,
            path=self.path + ["."],
            time=self.time - 1,
        )


def best_path(b: BluePrint, time_available: int) -> int:
    """Return maximum number of geodes obtainable."""
    stack: deque[State] = deque(
        [State(resources=ore(0), robots=ore(1), path=[], time=time_available)]
    )

    def push_if_possible(s: Optional[State]) -> None:
        if s is not None:
            stack.append(s)

    most_geodes = 0
    while True:
        if not stack:
            break
        state = stack.pop()
        # print("".join(state.path))
        if state.time == 0:
            if state.resources.geode > most_geodes:
                print(state.resources.geode, "".join(state.path))
                most_geodes = state.resources.geode
            continue
        # If we have the resources to buy a geode robot, just buy it
        if state.resources >= b.geode_robot:
            push_if_possible(state.buy(b.geode_robot, geode(1), "G"))
        else:
            push_if_possible(state.wait())
            push_if_possible(state.buy(b.ore_robot, ore(1), "o"))
            push_if_possible(state.buy(b.clay_robot, clay(1), "c"))
            push_if_possible(state.buy(b.obsidian_robot, obsidian(1), "O"))

    return most_geodes


TEST_DATA = (
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. "
    "Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\n"
    "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. "
    "Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."
)

if __name__ == "__main__":
    testmod()
    test_blueprints = [read_blueprint(line) for line in TEST_DATA.splitlines()]
    print(best_path(test_blueprints[0], 24))
