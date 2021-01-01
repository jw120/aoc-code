"""Advent of Code 2019 - Day 14.

Examples from the problem

>>> NanoFactory(test1).ore_required("FUEL")
31
>>> NanoFactory(test2).ore_required("FUEL")
165
>>> NanoFactory(test3).ore_required("FUEL")
13312
>>> NanoFactory(test4).ore_required("FUEL")
180697
>>> NanoFactory(test5).ore_required("FUEL")
2210736
"""

from __future__ import annotations

from collections import Counter
from dataclasses import dataclass
from doctest import testmod
from sys import stdin
from typing import Counter as Counter_t, Dict, Iterable, List, NoReturn, Tuple, Union

# Counter_t is a workaround as mypy does not support python 3.9


def assert_never(value: NoReturn) -> NoReturn:
    assert False, f"Unhandled value: {value} ({type(value).__name__})"


@dataclass(eq=True, frozen=True)
class BasicRecipe:
    """Recipe to make a basic chemical."""

    ore: int
    produced: int


@dataclass(eq=True, frozen=True)
class CompositeRecipe:
    """Recipe to make a composite chemical."""

    components: Counter_t[str]
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


class NanoFactory:
    def __init__(self, inputs: Iterable[str]) -> None:
        self.composite_recipes: Dict[str, CompositeRecipe] = {}
        self.basic_recipes: Dict[str, BasicRecipe] = {}
        for chemical, recipe in [parse_recipe(s.strip()) for s in inputs]:
            if isinstance(recipe, BasicRecipe):
                self.basic_recipes[chemical] = recipe
            elif isinstance(recipe, CompositeRecipe):
                self.composite_recipes[chemical] = recipe
            else:
                raise RuntimeError("Bad parse type")

    def required(self, target: str) -> Counter_t[str]:
        """Return the number of basic chemcials required to make the given composite chemical.

        Keeps track of extra composites made by reactions - these can be used in reactions but are not required.
        """
        # What we need to reduce to basics
        needed: Counter_t[str] = Counter({target: 1})
        # Excess we make but don't need
        on_hand: Counter_t[str] = Counter()
        # Build up the list of basic chemicals
        required: Counter_t[str] = Counter()
        # Loop until we need nothing else
        while needed:
            new_needed: Counter_t[str] = Counter()
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

    def ore_needed(self, targets: Counter_t[str]) -> int:
        """Return the amount of ore needed to make these basic chemicals."""
        total: int = 0
        for target, needed in targets.items():
            recipe: BasicRecipe = self.basic_recipes[target]
            reactions: int = round_up(needed, recipe.produced)
            total += reactions * recipe.ore
        return total

    def ore_required(self, target: str) -> int:
        """Return the amount of ore needed to make the given composite chemical."""
        return self.ore_needed(self.required(target))


def parse_recipe(s: str) -> Tuple[str, Recipe]:
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
    components: Counter_t[str] = Counter()
    for component_str in in_str.split(", "):
        component_quantity, component_name = component_str.split(" ")
        components[component_name] += int(component_quantity)
    return (out_name, CompositeRecipe(components=components, produced=produced))


test1: List[
    str
] = """10 ORE => 10 A
    1 ORE => 1 B
    7 A, 1 B => 1 C
    7 A, 1 C => 1 D
    7 A, 1 D => 1 E
    7 A, 1 E => 1 FUEL""".split(
    "\n"
)

test2: List[
    str
] = """9 ORE => 2 A
    8 ORE => 3 B
    7 ORE => 5 C
    3 A, 4 B => 1 AB
    5 B, 7 C => 1 BC
    4 C, 1 A => 1 CA
    2 AB, 3 BC, 4 CA => 1 FUEL""".split(
    "\n"
)

test3: List[
    str
] = """157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT""".split(
    "\n"
)


test4: List[
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
176 ORE => 6 VJHF""".split(
    "\n"
)

test5: List[
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
5 BHXH, 4 VRPVC => 5 LTCX""".split(
    "\n"
)

if __name__ == "__main__":
    testmod()
    nano = NanoFactory(stdin.readlines())
    print(nano.ore_required("FUEL"))
