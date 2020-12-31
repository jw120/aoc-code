"""Advent of Code 2019 - Day 14."""

from __future__ import annotations

from doctest import testmod
from sys import stdin
from typing import Dict, List, NewType, Tuple

Ingredient = NewType("Ingredient", str)
Quantity = Tuple[Ingredient, int]
Recipe = Tuple[Quantity, List[Quantity]]


class NanoFactory:
    def __init__(self, recipes: List[Tuple[Ingredient, Recipe]]) -> None:
        self.recipes: Dict[Ingredient, Recipe] = dict(recipes)

    def required(self, i: Ingredient) -> int:
        """Return the amount of ore needed to create one of the given chemical."""
        if i == "ORE":
            return 1
        made, inputs = self.recipes[i]
        return sum(n * self.required(x) for x, n in inputs)


def parse_recipe(s: str) -> Tuple[Ingredient, Recipe]:
    """Parse a recipe.

    >>>parse_recipe("5 B, 7 C => 1 BC")
    ("BC", (1, [("B", 5), ("C", 7)]))
    """
    pass


if __name__ == "__main__":
    testmod()
    recipes = [parse_recipe(line) for line in stdin]
    print(NanoFactory(recipes).required(Ingredient("FUEL")))
