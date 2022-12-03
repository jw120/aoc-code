"""Advent of Code 2022 - Day 2."""

from doctest import testmod
from enum import Enum
from sys import stdin
from typing import Tuple


class Move(Enum):
    rock = "rock"
    paper = "paper"
    scissors = "scissors"


beats = {Move.rock: Move.scissors, Move.paper: Move.rock, Move.scissors: Move.paper}
beaten = {q: p for p, q in beats.items()}
opponent_moves = {"A": Move.rock, "B": Move.paper, "C": Move.scissors}
player_moves = {"X": Move.rock, "Y": Move.paper, "Z": Move.scissors}


class Result(Enum):
    lose = "lose"
    draw = "draw"
    win = "win"


results = {"X": Result.lose, "Y": Result.draw, "Z": Result.win}


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
    shape_score = 1 if player == Move.rock else 2 if player == Move.paper else 3
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
        case Result.lose:
            player = beats[opponent]
        case Result.win:
            player = beaten[opponent]
        case Result.draw:
            player = opponent
    return score_moves((opponent, player))


if __name__ == "__main__":
    testmod()
    lines = list(stdin)
    print(sum(score_moves(read_moves(line)) for line in lines))
    print(sum(score_result(read_result(line)) for line in lines))
