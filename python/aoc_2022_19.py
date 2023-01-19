"""Advent of Code 2022 - Day 19."""

from __future__ import annotations

import re
from dataclasses import dataclass
from doctest import testmod
from sys import stdin
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

    def __str__(self) -> str:
        return (
            f"{self.number}: "
            f"o: {self.ore_robot.ore}o "
            f"c: {self.clay_robot.ore}o, "
            f"Ob: {self.obsidian_robot.ore}o+{self.obsidian_robot.clay}c, "
            f"G: {self.geode_robot.ore}o+{self.geode_robot.obsidian} Ob."
        )


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
    # path: list[str]
    time: int

    def buy(self, cost: Amount, robot: Amount, _name: str) -> Optional[State]:
        """Return a new state after buying the given robot if possible."""
        if self.resources >= cost:
            return State(
                resources=self.resources + self.robots - cost,
                robots=self.robots + robot,
                # path=self.path + [name],
                time=self.time - 1,
            )
        return None

    def wait(self) -> State:
        """Return a new state after one step of waiting."""
        return State(
            resources=self.resources + self.robots,
            robots=self.robots,
            # path=self.path + ["."],
            time=self.time - 1,
        )


def initial_state(time: int) -> State:
    """Create starting state."""
    return State(resources=ore(0), robots=ore(1), time=time)


class Dynamic:
    """Dynamic programming solution."""

    def __init__(self, blueprint: BluePrint):
        self.cache: dict[State, Optional[int]] = {}
        self.blueprint: BluePrint = blueprint
        self.max_resources = Amount(ore=20, clay=100, obsidian=25, geode=1000)

    def solution(self, state: Optional[State]) -> Optional[int]:
        """Return number of geodes we can produce in time."""
        if state is None:
            return None
        if state.time == 0:
            return 0
        if state in self.cache:
            return self.cache[state]
        if not self.max_resources >= state.resources:
            return None

        # If we can build a geode robot, we always should do only this
        if (
            buy_geode := state.buy(self.blueprint.geode_robot, geode(0), "G")
        ) is not None:
            if (buy_geode_score := self.solution(buy_geode)) is not None:
                self.cache[state] = buy_geode.time + buy_geode_score
                return buy_geode.time + buy_geode_score

        # Otherwise consider all possible moves
        candidate_states: list[Optional[State]] = []
        candidate_states = [
            state.buy(self.blueprint.ore_robot, ore(1), "o"),
            state.buy(self.blueprint.clay_robot, clay(1), "c"),
            state.buy(self.blueprint.obsidian_robot, obsidian(1), "O"),
            state.wait(),
        ]
        best_so_far = None
        for next_state in candidate_states:
            next_solution = self.solution(next_state)
            if next_solution is not None and (
                best_so_far is None or next_solution > best_so_far
            ):
                best_so_far = next_solution
        self.cache[state] = best_so_far
        return best_so_far


def qualities(blueprint_list: list[BluePrint], time: int) -> int:
    """Return sum of qualities for a list of blueprints."""
    acc = 0
    for b in blueprint_list:
        # print(b)
        b_solution = Dynamic(b).solution(initial_state(time))
        assert b_solution is not None
        acc += b.number * b_solution
        # print(b.number, b_solution, acc)
    return acc


def score_product(blueprint_list: list[BluePrint], time: int) -> int:
    """Return product of scores for a list of blueprints."""
    acc = 1
    for b in blueprint_list:
        # print(b)
        b_solution = Dynamic(b).solution(initial_state(time))
        assert b_solution is not None
        acc *= b_solution
        # print(b.number, b_solution, acc)
    return acc


TEST_DATA = (
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. "
    "Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\n"
    "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. "
    "Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."
)

if __name__ == "__main__":
    testmod()
    # test_blueprints = [read_blueprint(line) for line in TEST_DATA.splitlines()]
    # print(qualities(test_blueprints, 24))
    # print(score_product(test_blueprints[:3], 32))
    blueprints = [read_blueprint(line.strip()) for line in stdin.readlines()]
    print(qualities(blueprints, 24))
    print(score_product(blueprints[:3], 32))
