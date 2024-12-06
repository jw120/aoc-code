"""Advent of Code 2024 - Day 6."""

from sys import stdin

from coord import Coord, Extent


def read_grid() -> tuple[list[list[bool]], Coord]:
    """Read grid and starting position from stdin."""
    grid: list[list[bool]] = []
    start_position: Coord | None = None
    for y, line in enumerate(stdin.readlines()):
        grid_line: list[bool] = []
        for x, c in enumerate(line.strip()):
            assert c in ".#^"
            if c == "^":
                assert start_position is None
                start_position = Coord(x, y)
            grid_line.append(c == "#")
        grid.append([c == "#" for c in line.strip()])
    assert start_position is not None
    return (grid, start_position)


def print_grid(grid: list[list[bool]]) -> None:
    """Print grid for debugging."""
    for line in grid:
        for c in line:
            print("#" if c else ".", end="")
        print()


def turn(d: Coord) -> Coord:
    """Turn right 90 degrees."""
    match d:
        case Coord(0, -1):
            return Coord(1, 0)
        case Coord(1, 0):
            return Coord(0, 1)
        case Coord(0, 1):
            return Coord(-1, 0)
        case Coord(-1, 0):
            return Coord(0, -1)
        case _:
            raise ValueError("Bad direction.")


def walk_till_exit(grid: list[list[bool]], start: Coord) -> int:
    """Return number of steps before leaving grid."""
    extent: Extent = Extent(len(grid[0]), len(grid))
    position: Coord = start
    direction: Coord = Coord(0, -1)
    visited: list[list[bool]] = [[False for _ in range(extent.x)] for _ in range(extent.y)]
    while True:
        visited[position.y][position.x] = True
        next_position = position + direction
        if not next_position.in_bounds(extent):
            break
        if grid[next_position.y][next_position.x]:
            direction = turn(direction)
        else:
            position = next_position
    return sum(sum(v for v in line) for line in visited)


if __name__ == "__main__":
    grid, start = read_grid()
    print(walk_till_exit(grid, start))
