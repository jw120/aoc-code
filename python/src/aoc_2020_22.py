"""Advent of Code 2020 - Day 22."""

from __future__ import annotations

from copy import copy
from dataclasses import dataclass
from doctest import testmod
from enum import Enum, auto
from sys import stdin
from typing import ClassVar


@dataclass(frozen=True)
class History:
    """Wrapper class with a hash so we can put the card state into a set."""

    p1: list[int]
    p2: list[int]

    def __hash__(self) -> int:
        return hash(f"{self.p1} {self.p2}")


class Player(Enum):
    """Class to hold current player."""

    ONE = auto()
    TWO = auto()


test1: str = "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10\n"


class Combat:
    """Main class for day 22."""

    _last_game: ClassVar[int] = 0

    @staticmethod
    def _parse_player(s: str, header: str) -> list[int]:
        """Read one player's part of the input."""
        lines: list[str] = s.strip().split("\n")
        if lines[0] != header:
            raise RuntimeError("Can't find", header, "in", lines[0])
        return [int(x) for x in lines[1:]]

    def __init__(self, s: str | tuple[list[int], list[int]]) -> None:
        self.p1: list[int]
        self.p2: list[int]
        if isinstance(s, str):
            player_1_part, player_2_part = s.split("\n\n")
            self.p1 = self._parse_player(player_1_part, "Player 1:")
            self.p2 = self._parse_player(player_2_part, "Player 2:")
        else:
            self.p1, self.p2 = s

    def winner(self) -> Player:
        """Determine which player is the winner."""
        if self.p1 and not self.p2:
            return Player.ONE
        if not self.p1 and self.p2:
            return Player.TWO
        raise RuntimeError("No winner")

    def score(self) -> int:
        """Return the score based on the winner's hand."""
        winner_deck: list[int] = self.p1 if self.winner() == Player.ONE else self.p2
        return sum(x * y for x, y in enumerate(reversed(winner_deck), start=1))

    @classmethod
    def next_game(cls) -> int:
        """Provide a counter of new games for recursive game debug output."""
        cls._last_game += 1
        return cls._last_game

    def play_round(self) -> Combat:
        """Play one round of the (non-recursive) game.

        >>> Combat(test1).play_round().p1
        [2, 6, 3, 1, 9, 5]
        """
        top_player_1: int = self.p1.pop(0)
        top_player_2: int = self.p2.pop(0)
        if top_player_1 > top_player_2:
            self.p1 += [top_player_1, top_player_2]
        elif top_player_2 > top_player_1:
            self.p2 += [top_player_2, top_player_1]
        else:
            raise RuntimeError("Unexpected tie", top_player_1)
        return self

    def play_game(self) -> Combat:
        """Play (non-recursive) rounds until the game is over.

        >>> Combat(test1).play_game().score()
        306
        """
        while self.p1 and self.p2:
            self.play_round()
        return self

    @staticmethod
    def _show_deck(xs: list[int]) -> str:
        """Return list as a comma-separated string with a leading-space if non-empty."""
        if xs:
            return " " + ", ".join([str(x) for x in xs])
        return ""

    def play_recursive_game(self, debug: bool = False) -> Combat:
        """Play recursive rounds until the game is over.

        >>> Combat(test1).play_recursive_game().score()
        291
        """
        game_num: int = self.next_game()
        history: set[History] = set()

        if debug:
            print(f"=== Game {game_num} ===")
            print()

        round_num = 1

        while True:

            if debug:
                print(f"-- Round {round_num} (Game {game_num}) --")
                print(f"Player 1's deck:{self._show_deck(self.p1)}")
                print(f"Player 2's deck:{self._show_deck(self.p2)}")

            top_player_1 = self.p1.pop(0)
            assert isinstance(top_player_1, int)
            top_player_2 = self.p2.pop(0)
            assert isinstance(top_player_2, int)
            winner: Player

            if debug:
                print(f"Player 1 plays: {top_player_1}")
                print(f"Player 2 plays: {top_player_2}")

            # Player one wins the game if repeating ourselves
            new_history: History = History(self.p1, self.p2)
            if new_history in history:
                self.p2 = []  # To signal player one has won
                return self
            history.add(new_history)

            # Round Recursive game if both players have enough cards
            if top_player_1 <= len(self.p1) and top_player_2 <= len(self.p2):
                if debug:
                    print("Playing a sub-game to determine the winner...")
                    print()

                sub_game = Combat(
                    (copy(self.p1[:top_player_1]), copy(self.p2[:top_player_2]))
                )
                sub_game.play_recursive_game(debug)
                winner = sub_game.winner()
                if debug:
                    print(f"...anyway, back to game {game_num}.")

            # Otherwise regular game
            else:
                if top_player_1 > top_player_2:
                    winner = Player.ONE
                elif top_player_2 > top_player_1:
                    winner = Player.TWO
                else:
                    raise RuntimeError("Unexpected tie", top_player_1)

            # Resolve round winner
            if debug:
                print(f"Player {'1' if winner == Player.ONE else '2'} wins ", end="")
                print(f"round {round_num} of game {game_num}!")
            if winner == Player.ONE:
                self.p1 += [top_player_1, top_player_2]
            else:
                self.p2 += [top_player_2, top_player_1]

            if not self.p1 or not self.p2:
                if debug:
                    print(f"The winner of game {game_num} is ", end="")
                    print(f"player {'1' if self.winner() == Player.ONE else '2'}!")
                    print()
                    if game_num == 1:
                        print()
                        print("== Post-game results ==")
                        print(f"Player 1's deck:{self._show_deck(self.p1)}")
                        print(f"Player 2's deck:{self._show_deck(self.p2)}")

                return self

            if debug:
                print()

            round_num += 1


if __name__ == "__main__":
    testmod()

    input_str: str = stdin.read()

    part_one = Combat(input_str)
    part_one.play_game()
    print(part_one.score())

    part_two = Combat(input_str)
    part_two.play_recursive_game()
    print(part_two.score())
