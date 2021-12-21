"""Advent of Code 2021 - Day 21."""


# from __future__ import annotations

from doctest import testmod
from sys import stdin


class Player:
    def __init__(self, square: int):
        self.square = square
        self.score = 0

    def advance(self, dies: int) -> None:
        self.square = (self.square + dies - 1) % 10 + 1
        self.score += self.square


class DeterministicDie:
    def __init__(self, sides: int):
        self.sides = sides
        self.next = 1
        self.count = 0

    def roll(self) -> int:
        next: int = self.next
        self.next = 1 if self.next == self.sides else self.next + 1
        self.count += 1
        return next


def partA(start1: int, start2: int) -> int:
    """Run part A using deterministic dice.

    >>> partA(4, 8)
    739785
    """
    players: list[Player] = [Player(start1), Player(start2)]
    die: DeterministicDie = DeterministicDie(100)
    player: int = 0
    while True:
        p = players[player]
        die_total: int = sum(die.roll() for d in range(3))
        p.advance(die_total)
        # print(
        #     f"Player {player}: rolled {die_total}, moved to {p.square} has score {p.score}"
        # )
        if p.score >= 1000:
            break
        player = (player + 1) % 2
    return players[(player + 1) % 2].score * die.count


if __name__ == "__main__":
    testmod()
    start1, start2 = [int(line.split(": ")[1]) for line in stdin.read().splitlines()]
    print(partA(start1, start2))
