"""Advent of Code 2023 - Day 2."""

from dataclasses import dataclass
from doctest import testmod
from sys import stdin


@dataclass(frozen=True)
class Subset:
    """Class for a subset of cubes."""

    red: int
    green: int
    blue: int


def parse_subset(s: str) -> Subset:
    """Parse a subset from a string.

    >>> parse_subset("3 blue, 4 red")
    Subset(red=4, green=0, blue=3)
    >>> parse_subset("1 red, 2 green, 6 blue")
    Subset(red=1, green=2, blue=6)
    """
    red: int = 0
    green: int = 0
    blue: int = 0
    for piece in s.split(", "):
        match piece.split(" "):
            case [n, "red"] if n.isnumeric():
                assert red == 0
                red = int(n)
            case [n, "green"] if n.isnumeric():
                assert green == 0
                green = int(n)
            case [n, "blue"] if n.isnumeric():
                assert blue == 0
                blue = int(n)
            case _:
                raise ValueError(f"Unrecognized piece: '{piece}'")
    return Subset(red=red, green=green, blue=blue)


@dataclass(frozen=True)
class Game:
    """Class for a game."""

    id_number: int
    subsets: list[Subset]


def parse_game(s: str) -> Game:
    """Parse a game from a string.

    >>> parse_game("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
    Game(id_number=5, subsets=[Subset(red=6, green=3, blue=1), Subset(red=1, green=2, blue=2)])
    """
    game_part, subsets_part = s.split(":")
    assert game_part[:5] == "Game ", f"Failed '{game_part[:5]}'"
    assert game_part[5:].isnumeric()
    assert subsets_part[0] == " "
    return Game(
        id_number=int(game_part[5:]),
        subsets=[parse_subset(s) for s in subsets_part[1:].split("; ")],
    )


def possible(full: Subset, part: Subset) -> bool:
    """Test if part is a subset of the full number of balls."""
    return part.red <= full.red and part.green <= full.green and part.blue <= full.blue


def fewest(game: Game) -> Subset:
    """Return smallest numbers of balls that could have generated the game."""
    return Subset(
        red=max(subset.red for subset in game.subsets),
        green=max(subset.green for subset in game.subsets),
        blue=max(subset.blue for subset in game.subsets),
    )


def power(subset: Subset) -> int:
    """Return power of a set of cubes."""
    return subset.red * subset.green * subset.blue


if __name__ == "__main__":
    testmod()
    games: list[Game] = [parse_game(s.strip()) for s in stdin.readlines()]
    full = Subset(red=12, green=13, blue=14)
    print(
        sum(
            g.id_number
            for g in games
            if all(possible(full, subset) for subset in g.subsets)
        )
    )
    print(sum(power(fewest(game)) for game in games))
