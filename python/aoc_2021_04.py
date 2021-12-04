"""Advent of Code 2021 - Day 4."""

from doctest import testmod
from sys import stdin
from typing import ClassVar, Optional


class Board:
    """5x5 Bingo board."""

    SIZE: ClassVar[int] = 5

    def __init__(self, s: str) -> None:
        """Create new bingo board based on the provided string."""
        self.board: list[list[int]] = [
            [int(x) for x in row.split()] for row in s.splitlines()
        ]
        assert len(self.board) == Board.SIZE
        assert all(len(row) == Board.SIZE for row in self.board)
        self.marked: list[list[bool]] = [
            [False for _ in range(Board.SIZE)] for _ in range(Board.SIZE)
        ]
        self.last_called: Optional[int] = None

    def call(self, x: int) -> bool:
        """Call the given number. Mark it if it is present. Return true if board is won."""
        self.last_called = x
        for i in range(Board.SIZE):
            for j in range(Board.SIZE):
                if self.board[i][j] == x:
                    self.marked[i][j] = True
        return self.won()

    def won(self) -> bool:
        """Test if the board is winning - i.e., a whole row or column is marked."""
        for i in range(Board.SIZE):
            if all(self.marked[i]) or all(self.marked[j][i] for j in range(Board.SIZE)):
                return True
        return False

    def score(self) -> int:
        """Score for the board (product of last called number and the sum of unmarked numbers)."""
        sum_unmarked = sum(
            self.board[i][j]
            for i in range(Board.SIZE)
            for j in range(Board.SIZE)
            if not self.marked[i][j]
        )
        assert self.last_called is not None
        return sum_unmarked * self.last_called


test_numbers = [
    int(x)
    for x in "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1".split(
        ","
    )
]

test_boards: list[Board] = [
    Board(
        "22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n1 12 20 15 19"
    ),
    Board(
        " 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6"
    ),
    Board(
        "14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"
    ),
]


def part1(numbers: list[int], boards: list[Board]) -> int:
    """Part 1 - return score of first board to win.

    >>> part1(test_numbers, test_boards)
    4512
    """
    for n in numbers:
        for b in boards:
            if b.call(n):
                return b.score()
    raise ValueError("No board wins.")


def part2(numbers: list[int], boards: list[Board]) -> int:
    """Part 2 - return score of last board to win.

    >>> part2(test_numbers, test_boards)
    1924
    """
    for n in numbers:
        if len(boards) == 1:
            if boards[0].call(n):
                return boards[0].score()
        boards = [b for b in boards if not b.call(n)]
    raise ValueError("No board wins.")


if __name__ == "__main__":
    testmod()
    blocks: list[str] = stdin.read().split("\n\n")
    numbers: list[int] = [int(x) for x in blocks[0].split(",")]
    boards: list[Board] = [Board(x) for x in blocks[1:]]
    print(part1(numbers, boards))
    print(part2(numbers, boards))
