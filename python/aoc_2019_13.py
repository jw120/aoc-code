"""Advent of Code 2019 - Day 13."""

from __future__ import annotations

import curses
from sys import stdin
from typing import Any, Optional

from int_code import Machine


def count_blocks(xs: list[int]) -> tuple[int, Optional[int]]:
    """Count the remaining blocks and extract any score from machine output."""
    screen: dict[tuple[int, int], int] = {}
    score: Optional[int] = None
    for x, y, tile_id in [xs[i : i + 3] for i in range(0, len(xs), 3)]:
        if (x, y) == (-1, 0):
            score = tile_id
        else:
            screen[(x, y)] = tile_id
    return (sum(x == 2 for x in screen.values()), score)


def tile(i: int) -> str:
    """Convert integer to tile string."""
    if i == 0:
        return " "  # Space
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
    """Main class for day 13."""

    def __init__(self, code: list[int]) -> None:
        self.m = Machine(code)
        self.m.pause_after_output = True
        self.m.pause_before_input = True
        self.ball_position: int = 0
        self.paddle_position: int = 0
        self.animate: bool = False
        self.score: int = 0

    def _run(self, stdscr: Any) -> None:
        """Run the player."""

        if self.animate:
            if curses.can_change_color():
                curses.init_color(0, 0, 0, 0)  # Set background (coloru 0) to rgb black
            stdscr.clear()
            curses.curs_set(0)  # Hide the cursor

        while True:
            self.m.run()
            if self.m.halted:
                break
            if len(self.m.output_vals) == 0:  # Paused for input
                if self.paddle_position < self.ball_position:
                    self.m.input_vals.append(1)
                elif self.paddle_position > self.ball_position:
                    self.m.input_vals.append(-1)
                else:
                    self.m.input_vals.append(0)
            elif len(self.m.output_vals) == 3:  # Output full
                match self.m.output_vals:
                    case x, y, tile_id:
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
                                    curses.napms(50)
                                    # stdscr.getkey()
                            elif tile_id == 3:
                                self.paddle_position = x
                    case _:
                        raise ValueError("Unexpected output values." "")
            else:  # Wait for more output
                pass

        if self.animate:
            stdscr.refresh()
            stdscr.getkey()

    def run(self) -> Player:
        """Run the player, starting curses if needed."""
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
        with open("../aoc-data/input/2019_13.txt", encoding="utf-8") as f:
            input_code: list[int] = [int(x) for x in f.read().split(",")]
    else:
        input_code = [int(x) for x in stdin.read().split(",")]
    blocks, _score = count_blocks(Machine(input_code).run().output_vals)
    print(blocks)
    input_code[0] = 2
    p = Player(input_code)
    p.animate = show_animation
    p.run()
    print(p.score)
