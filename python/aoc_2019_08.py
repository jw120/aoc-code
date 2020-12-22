"""Advent of Code 2019 - Day 8."""

from sys import stdin
from typing import Dict, List


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
    layer_counts: List[Dict[str, int]] = []
    for layer in range(0, i.layers):
        layer_counts.append({})
        for row in range(0, i.rows):
            for col in range(0, i.cols):
                p: str = i.pixel(layer, row, col)
                layer_counts[layer][p] = layer_counts[layer].get(p, 0) + 1
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
    i = Image(stdin.read().strip(), 6, 25)
    print(part_one(i))
    part_two(i)