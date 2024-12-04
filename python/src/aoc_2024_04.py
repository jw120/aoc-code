"""Advent of Code 2024 - Day 4."""

from sys import stdin

from coord import Coord


class LetterGrid:
    """Hold our letter grid and methods."""

    def __init__(self, lines: list[str]) -> None:
        self.height = len(lines)
        self.width = len(lines[0])
        self.lines = lines
        for line in lines:
            assert len(line) == self.width

    def letter(self, c: Coord) -> str:
        """Return letter at given coordinate.

        Return space if out of bounds.
        """
        if c.x < 0 or c.x >= self.width or c.y < 0 or c.y >= self.height:
            return " "
        return self.lines[c.y][c.x]

    def check_xmas(self, x_coord: Coord, direction: Coord) -> bool:
        """Check to see for XMAS in given direction."""
        c = x_coord + direction
        for letter in "MAS":
            if self.letter(c) != letter:
                return False
            c += direction
        return True

    def count_xmas(self) -> int:
        """Count occurrences of XMAS."""
        count = 0
        for x in range(self.width):
            for y in range(self.height):
                c = Coord(x, y)
                if self.letter(c) == "X":
                    for d in (
                        Coord(0, 1),
                        Coord(1, 1),
                        Coord(1, 0),
                        Coord(1, -1),
                        Coord(0, -1),
                        Coord(-1, -1),
                        Coord(-1, 0),
                        Coord(-1, 1),
                    ):
                        count += self.check_xmas(c, d)
        return count

    def check_x_mas(self, a_coord: Coord) -> bool:
        """Check to see for X-MAS."""
        diagonals: list[str] = [
            self.letter(a_coord + d)
            for d in (Coord(1, 1), Coord(1, -1), Coord(-1, -1), Coord(-1, 1))
        ]
        return sorted(diagonals) == ["M", "M", "S", "S"] and diagonals[0] != diagonals[2]

    def count_x_mas(self) -> int:
        """Count occurrences of X-MAS."""
        count = 0
        for x in range(self.width):
            for y in range(self.height):
                c = Coord(x, y)
                if self.letter(c) == "A":
                    count += self.check_x_mas(c)
        return count


if __name__ == "__main__":
    grid = LetterGrid(list(stdin.readlines()))
    print(grid.count_xmas())
    print(grid.count_x_mas())
