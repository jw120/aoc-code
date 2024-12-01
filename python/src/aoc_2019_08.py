"""Advent of Code 2019 - Day 8."""

from collections import Counter
from operator import itemgetter
from sys import stdin


class Image:
    """Image."""

    def __init__(self, pixels: str, rows: int, cols: int) -> None:
        self.pixels = pixels
        self.rows = rows
        self.cols = cols
        self.layer_size = rows * cols
        self.layers = len(pixels) // self.layer_size
        if self.layers * self.layer_size != len(self.pixels):
            raise RuntimeError("Bad size")

    def pixel(self, layer: int, row: int, col: int) -> str:
        """Return pixel value."""
        return self.pixels[layer * self.layer_size + row * self.cols + col]


def part_one(i: Image) -> int:
    """Solve part one."""
    layer_counts: list[Counter[str]] = []
    for layer in range(i.layers):
        layer_counts.append(Counter())
        for row in range(i.rows):
            for col in range(i.cols):
                p: str = i.pixel(layer, row, col)
                layer_counts[layer][p] += 1
    min_layer = min(layer_counts, key=itemgetter(0))
    return min_layer["1"] * min_layer["2"]


def part_two(i: Image) -> None:
    """Solve part-two."""
    for row in range(i.rows):
        for col in range(i.cols):
            layer = 0
            while i.pixel(layer, row, col) == "2":
                layer += 1
            print("*" if (i.pixel(layer, row, col) == "1") else " ", end="")
        print()


if __name__ == "__main__":
    input_i = Image(stdin.read().strip(), 6, 25)
    print(part_one(input_i))
    part_two(input_i)
