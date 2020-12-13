"""Advent of Code 2020 - Day 11."""

from sys import stdin
from typing import Callable, Iterable, List


class Grid:
    """Grid of seats.

    Holds two copies of the grid data so one can be updated while
    the other is being read.
    """

    def __init__(self, lines: Iterable[str]) -> None:
        self.time: int = 0
        self.g: List[List[str]] = [[], []]
        for line in lines:
            self.g[0].append(line.strip())
            self.g[1].append(line.strip())
        self.rows = len(self.g[0])
        self.cols = len(self.g[0][0])

    def get_val(self, x: int, y: int) -> str:
        """Read the value at given coordinates in the active grid."""
        return self.g[self.time % 2][y][x]

    def set_val(self, x: int, y: int, val: bool) -> None:
        """Set the value at the given coordinates to a seat in the inactive grid."""
        self.g[(self.time + 1) % 2][y] = (
            self.g[(self.time + 1) % 2][y][:x]
            + ("#" if val else "L")
            + self.g[(self.time + 1) % 2][y][x + 1 :]
        )

    def is_seat(self, x: int, y: int) -> bool:
        """Is the location a seat."""
        return self.get_val(x, y) == "L" or self.get_val(x, y) == "#"

    def is_empty_seat(self, x: int, y: int) -> bool:
        """Is the location in the active grid an empty seat."""
        return self.get_val(x, y) == "L"

    def is_occupied_seat(self, x: int, y: int) -> bool:
        """Is the location valid and an ocupied seat in the active grid."""
        return (
            x >= 0
            and x < self.cols
            and y >= 0
            and y < self.rows
            and self.get_val(x, y) == "#"
        )

    def neighbouring_occupied_seats(self, x: int, y: int) -> int:
        """Count the occuped seats neighbouring the location in the active grid."""
        return (
            self.is_occupied_seat(x - 1, y - 1)
            + self.is_occupied_seat(x - 1, y)
            + self.is_occupied_seat(x - 1, y + 1)
            + self.is_occupied_seat(x, y - 1)
            + self.is_occupied_seat(x, y + 1)
            + self.is_occupied_seat(x + 1, y - 1)
            + self.is_occupied_seat(x + 1, y)
            + self.is_occupied_seat(x + 1, y + 1)
        )

    def visible_occupied_seat(self, x: int, y: int, dx: int, dy: int) -> bool:
        """Is there an occupied seat visible in the direction given in the active grid.

        Visibility is blocked by any seat.
        """
        seat_x, seat_y = x + dx, y + dy
        while seat_x >= 0 and seat_x < self.cols and seat_y >= 0 and seat_y < self.rows:
            if self.is_occupied_seat(seat_x, seat_y):
                return True
            if self.is_empty_seat(seat_x, seat_y):
                return False
            seat_x, seat_y = seat_x + dx, seat_y + dy
        return False

    def visible_occupied_seats(self, x: int, y: int) -> int:
        """Count the number of occupied seats visible from the location in the active grid."""
        return (
            self.visible_occupied_seat(x, y, -1, -1)
            + self.visible_occupied_seat(x, y, -1, 0)
            + self.visible_occupied_seat(x, y, -1, 1)
            + self.visible_occupied_seat(x, y, 0, -1)
            + self.visible_occupied_seat(x, y, 0, 1)
            + self.visible_occupied_seat(x, y, 1, -1)
            + self.visible_occupied_seat(x, y, 1, 0)
            + self.visible_occupied_seat(x, y, 1, 1)
        )

    def iterate_one(self) -> int:
        """Iterate the grid based on part one rules."""
        changes_made: int = 0
        for x in range(0, self.cols):
            for y in range(0, self.rows):
                if self.is_seat(x, y):
                    self.set_val(x, y, self.is_occupied_seat(x, y))
                nearby: int = self.neighbouring_occupied_seats(x, y)
                if self.is_empty_seat(x, y) and nearby == 0:
                    self.set_val(x, y, True)
                    changes_made += 1
                elif self.is_occupied_seat(x, y) and nearby >= 4:
                    self.set_val(x, y, False)
                    changes_made += 1
        self.time += 1
        return changes_made

    def iterate_two(self) -> int:
        """Iterate the grid based on part two rules."""
        changes_made: int = 0
        for x in range(0, self.cols):
            for y in range(0, self.rows):
                if self.is_seat(x, y):
                    self.set_val(x, y, self.is_occupied_seat(x, y))
                nearby: int = self.visible_occupied_seats(x, y)
                if self.is_empty_seat(x, y) and nearby == 0:
                    self.set_val(x, y, True)
                    changes_made += 1
                elif self.is_occupied_seat(x, y) and nearby >= 5:
                    self.set_val(x, y, False)
                    changes_made += 1
        self.time += 1
        return changes_made

    @property
    def occupied_seat_count(self) -> int:
        """Count the number of occupied seats in the active grid."""
        count: int = 0
        for x in range(0, self.cols):
            for y in range(0, self.rows):
                count += self.is_occupied_seat(x, y)
        return count

    def show(self) -> None:
        """Print the grid (for debugging)."""
        for y in range(0, self.rows):
            for x in range(0, self.cols):
                print(self.get_val(x, y), end="")
            print()


def iterate_until_zero(f: Callable[[], int], quiet: bool = True) -> None:
    """Call the functon repeatedly until it returns zero."""
    while (return_val := f()) != 0:
        if not quiet:
            print(f"Returned {return_val}")


test1: Grid = Grid(
    [
        "L.LL.LL.LL",
        "LLLLLLL.LL",
        "L.L.L..L..",
        "LLLL.LL.LL",
        "L.LL.LL.LL",
        "L.LLLLL.LL",
        "..L.L.....",
        "LLLLLLLLLL",
        "L.LLLLLL.L",
        "L.LLLLL.LL",
    ]
)


if __name__ == "__main__":
    inputs: List[str] = list(stdin)
    grid1: Grid = Grid(inputs)
    iterate_until_zero(grid1.iterate_one)
    print(grid1.occupied_seat_count)
    grid2: Grid = Grid(inputs)
    iterate_until_zero(grid2.iterate_two)
    print(grid2.occupied_seat_count)
