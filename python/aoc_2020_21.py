"""Advent of Code 2020 - Day 21."""

from doctest import testmod
from sys import stdin
from typing import Dict, List, NewType, Set, Tuple


Ingredient = NewType("Ingredient", str)
Allergen = NewType("Allergen", str)


def parse_food(s: str) -> Tuple[List[Ingredient], List[Allergen]]:
    before, after = s.strip()[:-1].split(" (contains ")
    ingredients = [Ingredient(i) for i in before.split()]
    allergens = [Allergen(a) for a in after.split(", ")]
    return (ingredients, allergens)


test1: List[Tuple[List[Ingredient], List[Allergen]]] = [
    parse_food(s)
    for s in [
        "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
        "trh fvjkl sbzzf mxmxvkd (contains dairy)",
        "sqjhc fvjkl (contains soy)",
        "sqjhc mxmxvkd sbzzf (contains fish)",
    ]
]


def part_one(foods: List[Tuple[List[Ingredient], List[Allergen]]]) -> int:
    """Find number of times non-suspect ingredients are mentioned.

    >>> part_one(test1)
    5
    """
    # For each allergen, find what ingredients it could be in
    possible_ingredients: Dict[Allergen, Set[Ingredient]] = {}
    for ingredients, allergens in foods:
        for a in allergens:
            if a in possible_ingredients:
                possible_ingredients[a] = possible_ingredients[a] & set(ingredients)
            else:
                possible_ingredients[a] = set(ingredients)
    # Combine these together to find list of ingredients that could contain allergens
    suspect_ingredients: Set[Ingredient] = set.union(*possible_ingredients.values())
    # Count non-suspect ingredients in all the food lists
    count = 0
    for ingredients, _ in foods:
        for i in ingredients:
            if i not in suspect_ingredients:
                count += 1
    return count


if __name__ == "__main__":
    testmod()
    foods = [parse_food(line) for line in stdin]
    print(part_one(foods))
