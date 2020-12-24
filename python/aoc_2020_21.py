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


def possible_ingredients(
    foods: List[Tuple[List[Ingredient], List[Allergen]]]
) -> Dict[Allergen, Set[Ingredient]]:
    """Find which possible ingredients each allergen might be in."""
    possible: Dict[Allergen, Set[Ingredient]] = {}
    for ingredients, allergens in foods:
        for a in allergens:
            if a in possible:
                possible[a] = possible[a] & set(ingredients)
            else:
                possible[a] = set(ingredients)
    return possible


def part_one(foods: List[Tuple[List[Ingredient], List[Allergen]]]) -> int:
    """Find number of times non-suspect ingredients are mentioned.

    >>> part_one(test1)
    5
    """
    possible: Dict[Allergen, Set[Ingredient]] = possible_ingredients(foods)
    # Combine these together to find list of ingredients that could contain allergens
    suspect_ingredients: Set[Ingredient] = set.union(*possible.values())
    # Count non-suspect ingredients in all the food lists
    count = 0
    for ingredients, _ in foods:
        for i in ingredients:
            if i not in suspect_ingredients:
                count += 1
    return count


def part_two(foods: List[Tuple[List[Ingredient], List[Allergen]]]) -> str:
    """Return assigned allergens sorted by corresponding ingredient.

    >>> part_two(test1)
    'mxmxvkd,sqjhc,fvjkl'
    """
    possible: Dict[Allergen, Set[Ingredient]] = possible_ingredients(foods)

    matched_ingredients: List[Tuple[Ingredient, Allergen]] = []

    while possible:
        new_possible: Dict[Allergen, Set[Ingredient]] = {}
        for allergen, ingredient_set in possible.items():
            # Find an allergen with only one possible ingredient
            if len(ingredient_set) == 1:
                (ingredient,) = ingredient_set
                matched_ingredients.append((ingredient, allergen))
                # Remove the matched allergen and ingredient from the dictionary
                # (Can't edit the dictionary directly while iterating over it)
                for a, i_set in possible.items():
                    if a != allergen:
                        new_possible[a] = possible[a] - {ingredient}
                break
        possible = new_possible
    return ",".join(
        [pair[0] for pair in sorted(matched_ingredients, key=lambda x: x[1])]
    )


if __name__ == "__main__":
    testmod()
    foods = [parse_food(line) for line in stdin]
    print(part_one(foods))
    print(part_two(foods))
