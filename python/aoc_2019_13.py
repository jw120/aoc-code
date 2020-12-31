"""Advent of Code 2019 - Day 13."""

from __future__ import annotations

import curses
from sys import stdin
from typing import Any, Dict, List, Optional, Tuple

from IntCode import Machine


def count_blocks(xs: List[int]) -> Tuple[int, Optional[int]]:
    """Count the remaining blocks and extract any score from machine output."""
    screen: Dict[Tuple[int, int], int] = {}
    score: Optional[int] = None
    for x, y, tile_id in [xs[i : i + 3] for i in range(0, len(xs), 3)]:
        if (x, y) == (-1, 0):
            score = tile_id
        else:
            screen[(x, y)] = tile_id
    return (sum(x == 2 for x in screen.values()), score)


def tile(i: int) -> str:
    if i == 0:
        return "."  # Space
    if i == 1:
        return "#"  # Wall
    if i == 2:
        return "*"  # Block
    if i == 3:
        return "-"  # Paddle
    if i == 4:
        return "O"  # Ball
    raise RuntimeError("Bad tile id")


class Player:
    def __init__(self, code: List[int]) -> None:
        self.m = Machine(code)
        self.m.pause_after_output = True
        self.m.pause_before_input = True
        self.ball_position: int = 0
        self.paddle_position: int = 0
        self.animate: bool = False
        self.score: int = 0

    def _run(self, stdscr: Any) -> None:

        if self.animate:
            stdscr.clear()

        while True:
            self.m.run()
            if self.m.halted:
                break
            elif len(self.m.output_vals) == 0:  # Paused for input
                if self.paddle_position < self.ball_position:
                    self.m.input_vals.append(1)
                elif self.paddle_position > self.ball_position:
                    self.m.input_vals.append(-1)
                else:
                    self.m.input_vals.append(0)
            elif len(self.m.output_vals) == 3:  # Output full
                x, y, tile_id = self.m.output_vals
                self.m.output_vals = []
                if (x, y) == (-1, 0):
                    self.score = tile_id
                else:
                    if self.animate:
                        stdscr.addstr(y, x, tile(tile_id))
                    if tile_id == 4:
                        self.ball_position = x
                        if self.animate:
                            stdscr.refresh()
                            stdscr.getkey()
                    elif tile_id == 3:
                        self.paddle_position = x
            else:  # Wait for more output
                pass

        if self.animate:
            stdscr.refresh()
            stdscr.getkey()

    def run(self) -> Player:
        if self.animate:
            curses.wrapper(self._run)
        else:
            self._run(None)
        return self


if __name__ == "__main__":
    # Turning this on, switches to curses animation (can't rely on redirecting stdin
    # as need to take key inputs)
    show_animation: bool = False
    if show_animation:
        with open("../aoc-data/input/2019_13.txt") as f:
            code: List[int] = [int(x) for x in f.read().split(",")]
    else:
        code = [int(x) for x in stdin.read().split(",")]
    blocks, _score = count_blocks(Machine(code).run().output_vals)
    print(blocks)
    code[0] = 2
    p = Player(code)
    p.animate = show_animation
    p.run()
    print(p.score)
