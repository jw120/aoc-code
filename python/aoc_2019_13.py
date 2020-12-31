"""Advent of Code 2019 - Day 13."""

# from __future__ import annotations

# from doctest import testmod
from sys import stdin
from typing import Dict, List, Optional, Tuple

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


if __name__ == "__main__":
    #    testmod()
    code: List[int] = [int(x) for x in stdin.read().split(",")]
    blocks, _score = count_blocks(Machine(code).run().output_vals)
    print(blocks)
    code[0] = 2
    m = Machine(code)
    m.input_vals = [0, 1, -1] * 1000
    m.run()
    print(count_blocks(m.output_vals))
