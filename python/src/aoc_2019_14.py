"""Advent of Code 2019 - Day 14."""

from __future__ import annotations

from collections import Counter
from collections.abc import Iterable, Mapping
from dataclasses import dataclass
from doctest import testmod
from sys import stdin
from typing import Any, Final, Union


@dataclass(eq=True, frozen=True)
class BasicRecipe:
    """Recipe to make a basic chemical."""

    ore: int
    produced: int


@dataclass(eq=True, frozen=True)
class CompositeRecipe:
    """Recipe to make a composite chemical."""

    components: Counter[str]
    produced: int


Recipe = Union[BasicRecipe, CompositeRecipe]


def round_up(x: int, y: int) -> int:
    """Integer division rounding up fractions.

    >>> round_up(8, 4)
    2
    >>> round_up(8, 3)
    3
    """
    d, m = divmod(x, y)
    return d + bool(m != 0)


def all_zero(d: Mapping[Any, int]) -> bool:
    """Are all elements zero.

    >>> all_zero(Counter())
    True
    >>> all_zero(Counter({'red': 4, 'blue': 2}))
    False
    >>> all_zero(Counter({'red': 0, 'blue': 0}))
    True
    """
    for v in d.values():
        if v != 0:
            return False
    return True


class NanoFactory:
    """Main class for day 14."""

    def __init__(self, inputs: Iterable[str]) -> None:
        self.composite_recipes: dict[str, CompositeRecipe] = {}
        self.basic_recipes: dict[str, BasicRecipe] = {}
        for chemical, recipe in [parse_recipe(s.strip()) for s in inputs]:
            if isinstance(recipe, BasicRecipe):
                self.basic_recipes[chemical] = recipe
            else:
                self.composite_recipes[chemical] = recipe

    def _basics_required(self, target: str, number: int) -> Counter[str]:
        """Return number of basic chemcials required to make one of the given composite chemical.

        Returns the required basic chemicals and the leftovers
        """
        # What we need to reduce to basics
        needed: Counter[str] = Counter({target: number})
        # Build up the list of basic chemicals
        required: Counter[str] = Counter()
        # Keep track of leftovers
        on_hand: Counter[str] = Counter()
        # Loop until we need nothing else
        while not all_zero(needed):
            new_needed: Counter[str] = Counter()
            for composite, composite_needed in needed.items():
                recipe: CompositeRecipe = self.composite_recipes[composite]
                reactions: int = max(
                    0, round_up(composite_needed - on_hand[composite], recipe.produced)
                )
                for component, quantity in recipe.components.items():
                    if component in self.basic_recipes:
                        required[component] += reactions * quantity
                    else:
                        new_needed[component] += reactions * quantity
                on_hand[composite] += recipe.produced * reactions - composite_needed
            needed = new_needed
        return required

    def _ore_needed(self, targets: Counter[str]) -> int:
        """Return the amount of ore needed to make these basic chemicals."""
        total: int = 0
        for target, needed in targets.items():
            recipe: BasicRecipe = self.basic_recipes[target]
            reactions: int = round_up(needed, recipe.produced)
            total += reactions * recipe.ore
        return total

    def required(self, target: str, number: int = 1) -> int:
        r"""Return the amount of ore needed to make the given composite chemical.

        >>> NanoFactory(TEST1.split('\n')).required("FUEL")
        31
        >>> NanoFactory(TEST2.split('\n')).required("FUEL")
        165
        >>> NanoFactory(TEST3.split("\n")).required("FUEL")
        13312
        >>> NanoFactory(TEST4.split('\n')).required("FUEL")
        180697
        >>> NanoFactory(TEST5.split('\n')).required("FUEL")
        2210736
        """
        return self._ore_needed(self._basics_required(target, number))

    def possible(self, target: str, available_ore: int) -> int:
        r"""Return the amount of the target chemical that can be made from the given amount of ore.

        >>> NanoFactory(TEST3.split('\n')).possible("FUEL", 1_000_000_000_000)
        82892753
        >>> NanoFactory(TEST4.split('\n')).possible("FUEL", 1_000_000_000_000)
        5586022
        >>> NanoFactory(TEST5.split('\n')).possible("FUEL", 1_000_000_000_000)
        460664
        """

        def can_be_made(x: int) -> bool:
            """Can we make the given number of target chemicals with availble ore."""
            return self.required(target, x) <= available_ore

        # Find a range that brackets the maximum amount
        upper_bound: int = 1
        while can_be_made(upper_bound):
            upper_bound *= 2
        lower_bound: int = upper_bound // 2

        # Apply binary search
        while (upper_bound - lower_bound) > 1:
            mid_point = (upper_bound + lower_bound) // 2
            if can_be_made(mid_point):
                lower_bound = mid_point
            else:
                upper_bound = mid_point
        return lower_bound


def parse_recipe(s: str) -> tuple[str, Recipe]:
    """Parse a recipe.

    >>> parse_recipe("5 B, 7 C => 1 BC")
    ('BC', CompositeRecipe(components=Counter({'C': 7, 'B': 5}), produced=1))
    """
    in_str, out_str = s.split(" => ")
    out_quantity, out_name = out_str.split(" ")
    produced = int(out_quantity)
    if in_str.endswith("ORE"):
        in_quantity, _in_name = in_str.split(" ")
        return (
            out_name,
            BasicRecipe(ore=int(in_quantity), produced=produced),
        )
    components: Counter[str] = Counter()
    for component_str in in_str.split(", "):
        component_quantity, component_name = component_str.split(" ")
        components[component_name] += int(component_quantity)
    return (out_name, CompositeRecipe(components=components, produced=produced))


TEST1: Final[
    str
] = """10 ORE => 10 A
    1 ORE => 1 B
    7 A, 1 B => 1 C
    7 A, 1 C => 1 D
    7 A, 1 D => 1 E
    7 A, 1 E => 1 FUEL"""

TEST2: Final[
    str
] = """9 ORE => 2 A
    8 ORE => 3 B
    7 ORE => 5 C
    3 A, 4 B => 1 AB
    5 B, 7 C => 1 BC
    4 C, 1 A => 1 CA
    2 AB, 3 BC, 4 CA => 1 FUEL"""

TEST3: Final[
    str
] = """157 ORE => 5 NZVS
    165 ORE => 6 DCFZ
    44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
    12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
    179 ORE => 7 PSHF
    177 ORE => 5 HKGWZ
    7 DCFZ, 7 PSHF => 2 XJWVT
    165 ORE => 2 GPVTF
    3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"""

TEST4: Final[
    str
] = """2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
    17 NVRVD, 3 JNWZP => 8 VPVL
    53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
    22 VJHF, 37 MNCFX => 5 FWMGM
    139 ORE => 4 NVRVD
    144 ORE => 7 JNWZP
    5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
    5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
    145 ORE => 6 MNCFX
    1 NVRVD => 8 CXFTF
    1 VJHF, 6 MNCFX => 4 RFSQX
    176 ORE => 6 VJHF"""

TEST5: Final[
    str
] = """171 ORE => 8 CNZTR
    7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
    114 ORE => 4 BHXH
    14 VRPVC => 6 BMBT
    6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
    6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
    15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
    13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
    5 BMBT => 4 WPTQ
    189 ORE => 9 KTJDG
    1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
    12 VRPVC, 27 CNZTR => 2 XDBXC
    15 KTJDG, 12 BHXH => 5 XCVML
    3 BHXH, 2 VRPVC => 7 MZWV
    121 ORE => 7 VRPVC
    7 XCVML => 6 RJRHP
    5 BHXH, 4 VRPVC => 5 LTCX"""

if __name__ == "__main__":
    testmod()
    nano = NanoFactory(stdin.readlines())
    print(nano.required("FUEL"))
    print(nano.possible("FUEL", 1_000_000_000_000))
