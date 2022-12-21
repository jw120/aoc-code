"""Advent of Code 2022 - Day 2."""

from doctest import testmod
from enum import Enum
from sys import stdin
from typing import Tuple


class Move(Enum):
    """Which move is being made."""

    ROCK = "ROCK"
    PAPER = "PAPER"
    SCISSORS = "SCISSORS"


beats = {Move.ROCK: Move.SCISSORS, Move.PAPER: Move.ROCK, Move.SCISSORS: Move.PAPER}
beaten = {q: p for p, q in beats.items()}
opponent_moves = {"A": Move.ROCK, "B": Move.PAPER, "C": Move.SCISSORS}
player_moves = {"X": Move.ROCK, "Y": Move.PAPER, "Z": Move.SCISSORS}


class Result(Enum):
    """Result of a game."""

    LOSE = "LOSE"
    DRAW = "DRAW"
    WIN = "WIN"


results = {"X": Result.LOSE, "Y": Result.DRAW, "Z": Result.WIN}


def read_moves(s: str) -> Tuple[Move, Move]:
    """Read a line as a pair of moves."""
    [opponent, player] = s.split()
    return (opponent_moves[opponent], player_moves[player])


def read_result(s: str) -> Tuple[Move, Result]:
    """Read a line as a move and a result."""
    [opponent, result] = s.split()
    return (opponent_moves[opponent], results[result])


def score_moves(moves: Tuple[Move, Move]) -> int:
    """Score a game given the two moves."""
    opponent, player = moves
    shape_score = 1 if player == Move.ROCK else 2 if player == Move.PAPER else 3
    if beats[opponent] == player:
        win_score = 0
    elif beats[player] == opponent:
        win_score = 6
    elif player == opponent:
        win_score = 3
    else:
        raise ValueError("Unknown outcome!")
    return win_score + shape_score


def score_result(game: Tuple[Move, Result]) -> int:
    """Score a game given the opponent moves and desired result."""
    opponent, result = game
    match result:
        case Result.LOSE:
            player = beats[opponent]
        case Result.WIN:
            player = beaten[opponent]
        case Result.DRAW:
            player = opponent
    return score_moves((opponent, player))


if __name__ == "__main__":
    testmod()
    lines = list(stdin)
    print(sum(score_moves(read_moves(line)) for line in lines))
    print(sum(score_result(read_result(line)) for line in lines))
