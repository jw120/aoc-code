"""Advent of Code 2021 - Day 20."""

from __future__ import annotations

from doctest import testmod
from functools import reduce
from sys import stdin
from typing import Iterator, Optional

from coord import Coord

Algorithm = list[bool]  # List of len 512


class Image:
    """Main class for day 20."""

    def __init__(self, lines: Optional[list[str]] = None):
        if lines is None:
            self.lit_pixels: frozenset[Coord] = frozenset()
        else:
            assert len(lines) > 0, "No input lines for Image creation"
            assert all(
                len(line) == len(lines[0]) for line in lines[1:]
            ), "Non-matching rows for Image creation"
            self.min_extent = Coord(0, 0)
            self.max_extent = Coord(len(lines), len(lines[0]))
            self.lit_pixels = frozenset(
                c for c in self.coords_with_border(0) if lines[c.y][c.x] == "#"
            )
        self.distant_pixel: bool = False
        self.set_extents()

    def set_extents(self) -> None:
        """Set min_extent and max_extent based on lit_pixels."""
        if self.lit_pixels:
            self.min_extent = Coord(
                min(c.x for c in self.lit_pixels), min(c.y for c in self.lit_pixels)
            )
            self.max_extent = Coord(
                max(c.x for c in self.lit_pixels) + 1,
                max(c.y for c in self.lit_pixels) + 1,
            )
        else:
            self.min_extent = Coord(0, 0)
            self.max_extent = Coord(0, 0)

    def coords_with_border(self, border_width: int) -> Iterator[Coord]:
        """Return an iterator over all possible coordinates (row-wise) including border."""
        for y in range(
            self.min_extent.y - border_width,
            self.max_extent.y + border_width,
        ):
            for x in range(
                self.min_extent.x - border_width,
                self.max_extent.y + border_width,
            ):
                yield Coord(x, y)

    def is_lit(self, c: Coord) -> bool:
        """Test if coordinate is lit."""
        distance_beyond_extent = max(
            max(self.min_extent.x - c.x, 0),
            max(c.x - self.max_extent.x + 1, 0),
            max(self.min_extent.y - c.y, 0),
            max(c.y - self.max_extent.y + 1, 0),
        )
        if distance_beyond_extent == 0:
            return c in self.lit_pixels
        return self.distant_pixel

    def value_around(self, c: Coord) -> int:
        """Return the integer made from the bits around the given coordinate.

        >>> test1.value_around(Coord(2, 2))
        34
        """
        bits: list[bool] = [
            self.is_lit(Coord(x, y))
            for y in range(c.y - 1, c.y + 2)
            for x in range(c.x - 1, c.x + 2)
        ]
        return reduce(lambda acc, x: acc * 2 + x, bits, 0)

    def enhanced_copy(self, algo: Algorithm) -> Image:
        """Create a new image by applying the image enhancement algorithm.

        >>> test1.enhanced_copy(test_algo1).enhanced_copy(test_algo1).number_lit
        35
        """
        other: Image = Image()
        border_width: int = 3 if self.distant_pixel else 2
        other.lit_pixels = frozenset(
            c
            for c in self.coords_with_border(border_width)
            if algo[self.value_around(c)]
        )
        other.distant_pixel = algo[511] if self.distant_pixel else algo[0]
        other.set_extents()
        return other

    def enhanced_repeated_copy(self, algo: Algorithm, n: int) -> Image:
        """Create a new image by applying the algorithm n times.

        >>> test1.enhanced_repeated_copy(test_algo1, 50).number_lit
        3351
        """
        image = self
        for _ in range(n):
            image = image.enhanced_copy(algo)
        return image

    @property
    def number_lit(self) -> int:
        """Count number of lit pixels."""
        assert not self.distant_pixel, "Infinitely many pixels lit"
        return len(self.lit_pixels)

    def show(self) -> None:
        """Print a debugging representation of the image."""
        print("Extent", self.min_extent, self.max_extent)
        for c in self.coords_with_border(4):
            print("#" if self.is_lit(c) else ".", end="")
            if c.x == self.max_extent.x + 3:
                print()


def read_algo(s: str) -> Algorithm:
    """Read an algorithm from a string."""
    assert len(s) == 512, "Wrong length for algorithm (" + str(s) + ")"
    assert all(c in "#." for c in s), "Bad char in algorithm"
    return [c == "#" for c in s]


test1: Image = Image(["#..#.", "#....", "##..#", "..#..", "..###"])

test_algo1: Algorithm = [
    c == "#"
    # pylint: disable-next=line-too-long
    for c in "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
]
assert len(test_algo1) == 512

if __name__ == "__main__":
    testmod()
    # test1.show()
    # test1.enhanced_copy(test_algo1).show()
    algo_block, image_block = stdin.read().split("\n\n")
    input_algo = read_algo(algo_block)
    input_image = Image(image_block.splitlines())
    # image.show()
    # image.enhanced_copy(algo).show()
    # image.enhanced_copy(algo).enhanced_copy(algo).show()
    print(input_image.enhanced_copy(input_algo).enhanced_copy(input_algo).number_lit)
    print(input_image.enhanced_repeated_copy(input_algo, 50).number_lit)
