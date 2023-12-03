"""Advent of Code 2019 - Day 8."""

# pylint: disable=missing-function-docstring, missing-class-docstring

from collections import Counter
from sys import stdin


class Image:
    def __init__(self, pixels: str, rows: int, cols: int) -> None:
        self.pixels = pixels
        self.rows = rows
        self.cols = cols
        self.layer_size = rows * cols
        self.layers = len(pixels) // self.layer_size
        if self.layers * self.layer_size != len(self.pixels):
            raise RuntimeError("Bad size")

    def pixel(self, layer: int, row: int, col: int) -> str:
        return self.pixels[layer * self.layer_size + row * self.cols + col]


def part_one(i: Image) -> int:
    layer_counts: list[Counter[str]] = []
    for layer in range(0, i.layers):
        layer_counts.append(Counter())
        for row in range(0, i.rows):
            for col in range(0, i.cols):
                p: str = i.pixel(layer, row, col)
                layer_counts[layer][p] += 1
    min_layer = min(layer_counts, key=lambda x: x["0"])
    return min_layer["1"] * min_layer["2"]


def part_two(i: Image) -> None:
    for row in range(0, i.rows):
        for col in range(0, i.cols):
            layer = 0
            while i.pixel(layer, row, col) == "2":
                layer += 1
            print("*" if (i.pixel(layer, row, col) == "1") else " ", end="")
        print()


if __name__ == "__main__":
    input_i = Image(stdin.read().strip(), 6, 25)
    print(part_one(input_i))
    part_two(input_i)
