"""Advent of Code 2019 - Day 14."""

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
class Basic:
    """A basic chemical (which is made for ore only)."""

    name: str


@dataclass(eq=True, frozen=True)
class Composite:
    """A composite chemical (which is made from other chemicals, not ore)."""

    name: str


Chemical = Union[Basic, Composite]


@dataclass(eq=True, frozen=True)
class BasicRecipe:
    """Recipe to make a basic chemical."""

    ore: int
    produced: int


@dataclass(eq=True, frozen=True)
class CompositeRecipe:
    """Recipe to make a composite chemical."""

    components: Counter_t[Chemical]
    produced: int


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
        self.composite_recipes: Dict[Composite, CompositeRecipe] = {}
        self.basic_recipes: Dict[Basic, BasicRecipe] = {}
        for chemical, recipe in [parse_chemical(i) for i in inputs]:
            if isinstance(chemical, Basic) and isinstance(recipe, BasicRecipe):
                self.basic_recipes[chemical] = recipe
            elif isinstance(chemical, Composite) and isinstance(
                recipe, CompositeRecipe
            ):
                self.composite_recipes[chemical] = recipe
            else:
                raise RuntimeError("Bad parse type")

    def required(self, target: Composite) -> Counter_t[Basic]:
        """Return the number of basic chemcials required to make one of the given composite chemical.

        Keeps track of extra composites made by reactions - these can be used in reactions but are not required.
        """
        # What we need to reduce to basics
        needed: Counter_t[Composite] = Counter({target: 1})
        # Excess we make but don't need
        on_hand: Counter_t[Composite] = Counter()
        # Build up the list of basic chemicals
        required: Counter_t[Basic] = Counter()
        # Loop until we need nothing else
        while needed:
            new_needed: Counter_t[Composite] = Counter()
            for composite, composite_needed in needed.items():
                recipe: CompositeRecipe = self.composite_recipes[composite]
                reactions: int = max(
                    0, round_up(composite_needed - on_hand[composite], recipe.produced)
                )
                for component, quantity in recipe.components.items():
                    if isinstance(component, Basic):
                        required[component] += reactions * quantity
                    elif isinstance(component, Composite):
                        new_needed[component] += reactions * quantity
                on_hand[composite] += recipe.produced * reactions - composite_needed
            needed = new_needed
        return required

    def ore_needed(self, targets: Counter_t[Basic]) -> int:
        """Return the amount of ore needed to make these basic chemicals."""
        needed: int = 0
        for target, needed in targets.items():
            recipe: BasicRecipe = self.basic_recipes[target]
            reactions: int = round_up(needed, recipe.produced)
            needed += reactions * recipe.ore
        return needed


def parse_chemical(
    s: str,
) -> Union[Tuple[Basic, BasicRecipe], Tuple[Composite, CompositeRecipe]]:
    """Parse a recipe.

    >>>parse_recipe("5 B, 7 C => 1 BC")
    (Comp
    """
    in_str, out_str = s.split(" => ")
    out_quantity, out_name = out_str.split(" ")
    if in_str.endswith("ORE"):
        in_quantity, _in_name = in_str.split(" ")
        return (
            Basic(out_name),
            BasicRecipe(ore=int(in_quantity), produced=int(out_quantity)),
        )
    components: Counter_t[Chemical] = Counter()
    for component_str in in_str.split(", "):
        component_quantity, component_name = component_str.split(" ")
        components[component_name] += int(component_quantity)


if __name__ == "__main__":
    testmod()
    nano = NanoFactory(stdin.readlines())
    print(nano.ore_needed(nano.required(Composite("ORE"))))
