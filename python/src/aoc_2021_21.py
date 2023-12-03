"""Advent of Code 2021 - Day 21."""

from collections import Counter
from doctest import testmod
from sys import stdin
from typing import TypeAlias


class Player:
    """Class for each player."""

    def __init__(self, square: int) -> None:
        self.square = square
        self.score = 0

    def advance(self, dies: int) -> None:
        """Advance."""
        self.square = (self.square + dies - 1) % 10 + 1
        self.score += self.square


class DeterministicDie:
    """Class for a die."""

    def __init__(self, sides: int) -> None:
        self.sides = sides
        self.nxt = 1
        self.count = 0

    def roll(self) -> int:
        """Roll the die."""
        nxt: int = self.nxt
        self.nxt = 1 if self.nxt == self.sides else self.nxt + 1
        self.count += 1
        return nxt


def part_a(start1: int, start2: int) -> int:
    """Run part A using deterministic dice.

    >>> part_a(4, 8)
    739785
    """
    players: list[Player] = [Player(start1), Player(start2)]
    die: DeterministicDie = DeterministicDie(100)
    player: int = 0
    while True:
        p = players[player]
        die_total: int = sum(die.roll() for _ in range(3))
        p.advance(die_total)
        if p.score >= 1000:
            break
        player = (player + 1) % 2
    return players[(player + 1) % 2].score * die.count


State: TypeAlias = tuple[bool, int, int, int, int]


def part_b(start1: int, start2: int) -> int:
    """Simulate universe splitting on each roll.

    Return number of universes in which the player with more wins wins.

    State of the game is the next player, the two current squares and the two scores

    Suppose winning score is 5 and players start at 1,2.

    Start: 1 universe: 1,1,2,0,0 (player 1 next, squares 1,2, scores 0,0)
    Then: 3 universes: 2,2,2,2,0  2,3,2,3,0  2,4,2,4,0
    Then: 9 universes: 1,2,3,2,3  1,3,3,3,3  1,4,3,4,3
                       1,2,4,2,4  1,3,4,3,4  1,4,4,4,4
                       1,2,5,2,5  1,3,5,3,5  1,4,5,4,5
    Then record 3 wins for player 2 and continue with the 6 universes

    >>> part_b(4, 8)
    444356092776315
    """
    active_states: Counter[State] = Counter([(True, start1, start2, 0, 0)])
    wins1: int = 0
    wins2: int = 0
    roll_outcomes: Counter[int] = Counter(
        i + j + k for i in range(1, 4) for j in range(1, 4) for k in range(1, 4)
    )
    while active_states:
        new_active_states: Counter[State] = Counter()
        for state, count in active_states.items():
            player, square1, square2, score1, score2 = state
            for roll, roll_weight in roll_outcomes.items():
                winner: bool = False
                if player:
                    new_square = ((square1 - 1 + roll) % 10) + 1
                    new_score = score1 + new_square
                    next_state = (False, new_square, square2, new_score, score2)
                    if new_score >= 21:
                        wins1 += count * roll_weight
                        winner = True
                else:
                    new_square = ((square2 - 1 + roll) % 10) + 1
                    new_score = score2 + new_square
                    next_state = (True, square1, new_square, score1, new_score)
                    if new_score >= 21:
                        wins2 += count * roll_weight
                        winner = True
                if not winner:
                    new_active_states[next_state] += count * roll_weight
        active_states = new_active_states
    return max(wins1, wins2)


if __name__ == "__main__":
    testmod()
    input_start1, input_start2 = (int(line.split(": ")[1]) for line in stdin.read().splitlines())
    print(part_a(input_start1, input_start2))
    print(part_b(input_start1, input_start2))
