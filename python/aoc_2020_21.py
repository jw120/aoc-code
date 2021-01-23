"""Advent of Code 2020 - Day 21."""

from doctest import testmod
from sys import stdin
from typing import NewType


Ingredient = NewType("Ingredient", str)
Allergen = NewType("Allergen", str)


def parse_food(s: str) -> tuple[list[Ingredient], list[Allergen]]:
    before, after = s.strip()[:-1].split(" (contains ")
    ingredients = [Ingredient(i) for i in before.split()]
    allergens = [Allergen(a) for a in after.split(", ")]
    return (ingredients, allergens)


test1: list[tuple[list[Ingredient], list[Allergen]]] = [
    parse_food(s)
    for s in [
        "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
        "trh fvjkl sbzzf mxmxvkd (contains dairy)",
        "sqjhc fvjkl (contains soy)",
        "sqjhc mxmxvkd sbzzf (contains fish)",
    ]
]


def possible_ingredients(
    foods: list[tuple[list[Ingredient], list[Allergen]]]
) -> dict[Allergen, set[Ingredient]]:
    """Find which possible ingredients each allergen might be in."""
    possible: dict[Allergen, set[Ingredient]] = {}
    for ingredients, allergens in foods:
        for a in allergens:
            if a in possible:
                possible[a] = possible[a] & set(ingredients)
            else:
                possible[a] = set(ingredients)
    return possible


def part_one(foods: list[tuple[list[Ingredient], list[Allergen]]]) -> int:
    """Find number of times non-suspect ingredients are mentioned.

    >>> part_one(test1)
    5
    """
    possible: dict[Allergen, set[Ingredient]] = possible_ingredients(foods)
    # Combine these together to find list of ingredients that could contain allergens
    suspect_ingredients: set[Ingredient] = set.union(*possible.values())
    # Count non-suspect ingredients in all the food lists
    count = 0
    for ingredients, _ in foods:
        for i in ingredients:
            if i not in suspect_ingredients:
                count += 1
    return count


def part_two(foods: list[tuple[list[Ingredient], list[Allergen]]]) -> str:
    """Return assigned allergens sorted by corresponding ingredient.

    >>> part_two(test1)
    'mxmxvkd,sqjhc,fvjkl'
    """
    possible: dict[Allergen, set[Ingredient]] = possible_ingredients(foods)

    matched_ingredients: list[tuple[Ingredient, Allergen]] = []

    while possible:
        for allergen, ingredient_set in possible.items():
            # Find an allergen with only one possible ingredient
            if len(ingredient_set) == 1:
                (ingredient,) = ingredient_set
                matched_ingredients.append((ingredient, allergen))
                # Remove the matched allergen and ingredient from the dictionary
                del possible[allergen]
                for a, i_set in possible.items():
                    possible[a] = possible[a] - {ingredient}
                break
    return ",".join(
        [pair[0] for pair in sorted(matched_ingredients, key=lambda x: x[1])]
    )


if __name__ == "__main__":
    testmod()
    foods = [parse_food(line) for line in stdin]
    print(part_one(foods))
    print(part_two(foods))
