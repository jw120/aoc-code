"""Advent of Code 2020 - Day 22."""

from __future__ import annotations

from doctest import testmod
from sys import stdin
from typing import List


test1: str = "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10\n"


class Combat:
    @staticmethod
    def _parse_player(s: str, header: str) -> List[int]:
        """Read one player's part of the input."""
        lines: List[str] = s.strip().split("\n")
        if lines[0] != header:
            raise RuntimeError("Can't find", header, "in", lines[0])
        return [int(x) for x in lines[1:]]

    def __init__(self, s: str) -> None:
        player_1_part, player_2_part = s.split("\n\n")
        self.p1: List[int] = self._parse_player(player_1_part, "Player 1:")
        self.p2: List[int] = self._parse_player(player_2_part, "Player 2:")

    def play_round(self) -> Combat:
        """Play one round of the game.

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

    def play_game(self) -> int:
        """Play rounds until the game is over.

        >>> Combat(test1).play_game()
        306
        """
        while self.p1 and self.p2:
            self.play_round()
        winner_deck: List[int] = self.p1 + self.p2
        return sum(x * y for x, y in enumerate(reversed(winner_deck), start=1))


if __name__ == "__main__":
    testmod()
    combat = Combat(stdin.read())
    print(combat.play_game())
