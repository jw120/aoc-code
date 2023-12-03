"""Advent of Code 2019 - Day 15."""

from __future__ import annotations

from enum import Enum
from random import randrange
from sys import stdin
from typing import TYPE_CHECKING, assert_never

from coord import Coord
from int_code import Machine

if TYPE_CHECKING:
    from collections.abc import Iterable


def move(c: Coord, d: CompassDirection) -> Coord:
    """Return new coordinate reflecting a move in given direction."""
    if d is CompassDirection.NORTH:
        return c + Coord(0, 1)
    if d is CompassDirection.EAST:
        return c + Coord(1, 0)
    if d is CompassDirection.SOUTH:
        return c + Coord(0, -1)
    if d is CompassDirection.WEST:
        return c + Coord(-1, 0)
    assert_never(d)


def closest(c: Coord, pts: Iterable[Coord]) -> Coord:
    """Return the closest points (or one of them if more than one)."""
    closest_so_far: tuple[int, Coord] | None = None
    for p in pts:
        dist = c.dist(p)
        match closest_so_far:
            case closest_dist, _closest_point:
                if dist < closest_dist:
                    closest_so_far = (dist, p)
            case None:
                closest_so_far = (dist, p)
    match closest_so_far:
        case _closest_dist, closest_point:
            return closest_point
        case None:
            raise RuntimeError("closest failed")


class CompassDirection(Enum):
    """CompassDirection of movement."""

    NORTH = 1
    SOUTH = 2
    WEST = 3
    EAST = 4

    @staticmethod
    def random() -> CompassDirection:
        """Generate a random direction."""
        return CompassDirection(randrange(1, 5))


class Response(Enum):
    """Responses."""

    WALL = 0
    MOVED = 1
    MOVED_OXYGEN = 2


class Controller:
    """Main class for day 15."""

    def __init__(self, code: list[int], *, debug: bool = False) -> None:
        self.m = Machine(code)
        self.m.pause_after_output = True
        self.m.pause_before_input = True
        self.loc: Coord = Coord.origin()
        self.walls: set[Coord] = set()  # Locations we know are walls
        self.open: set[Coord] = {self.loc}  # Locations we know are space
        self.oxygen: Coord | None = None  # Location of oxygen if known
        self.debug: bool = debug

    def try_move(self, d: CompassDirection) -> Response:
        """Try and move to an adjacent cell following the given direction."""
        self.m.input_vals = [d.value]
        self.m.run()
        return Response(self.m.output_vals.pop())

    def try_move_to_adj(self, c: Coord) -> Response:
        """Try and move to a given adjacent cell."""
        for d in CompassDirection:
            if move(self.loc, d) == c:
                return self.try_move(d)
        raise RuntimeError("Adjacent move not found", self.loc, c)

    def _path_to(self, target: Coord) -> list[Coord]:
        """Generate a path to the given cell through open cells."""
        if self.debug:
            print("Routing from", self.loc, "to", target)
        # Flood fill with back links
        frontier: dict[Coord, Coord] = {self.loc: self.loc}
        visited: dict[Coord, Coord] = {}
        while target not in frontier:
            visited.update(frontier)
            new_frontier: dict[Coord, Coord] = {}
            for f in frontier:
                for n in f.adjacents():
                    if n == target or (n in self.open and n not in visited):
                        new_frontier[n] = f
            frontier = new_frontier

        # Build back tracking path
        x = target
        path: list[Coord] = []
        while x != self.loc:
            path.append(x)
            # pylint: disable=consider-using-get
            x = frontier[x] if x in frontier else visited[x]
        if self.debug:
            print("Path", path)
        return path

    def route_to(self, target: Coord) -> Response:
        """Try and move to a given cell going through known open cells."""
        path = self._path_to(target)
        for x in reversed(path[1:]):
            if self.debug:
                print("Move to", x)
            response = self.try_move_to_adj(x)
            if response == Response.WALL:
                raise RuntimeError("Ran into wall during route")
            self.loc = x

        return self.try_move_to_adj(target)

    def steps_to_oxygen(self) -> int:
        """Find the number of steps from the origin to oxygen."""
        self.loc = Coord.origin()
        if self.oxygen is None:
            raise RuntimeError("No oxygen found")
        return len(self._path_to(self.oxygen))

    def oxygen_fill(self) -> int:
        """Find the number of minutes to fill all locations with oxygen."""
        if self.oxygen is None:
            raise RuntimeError("No oxygen found to fill from")
        filled: set[Coord] = {self.oxygen}
        minutes: int = 0
        while self.open - filled:
            additional: set[Coord] = set()
            for f in filled:
                for n in f.adjacents():
                    if n in self.open and n not in filled:
                        additional.add(n)
            filled |= additional
            minutes += 1
        return minutes

    def explore(self) -> Controller:
        """Explore the grid, completing the controllers knowledge of it."""
        # Frontier is all unknown locations adjacent to places we have visited
        frontier: set[Coord] = set(self.loc.adjacents())

        while frontier:
            if self.debug:
                print("Loc:", self.loc)
                print("Fr:", "  ".join([f"{c.x},{c.y}" for c in frontier]))
            target = closest(self.loc, frontier)
            frontier.discard(target)
            if self.debug:
                print("Going to", target)
            response = self.route_to(target)
            if response == Response.WALL:
                if self.debug:
                    print("Wall at", target)
                self.walls.add(target)
            else:
                if self.debug:
                    print("Open at", target)
                self.loc = target
                self.open.add(target)
                frontier |= set(target.adjacents()) - (self.open | self.walls)
                if response == Response.MOVED_OXYGEN:
                    self.oxygen = target

        return self

    def show(self) -> Controller:
        """Provide debugging information."""
        xs = [w.x for w in self.walls]
        ys = [w.y for w in self.walls]
        for y in range(max(ys), min(ys) - 1, -1):
            for x in range(min(xs), max(xs) + 1):
                if Coord(x, y) == Coord.origin():
                    print("X", end="")
                elif Coord(x, y) == self.oxygen:
                    print("O", end="")
                elif Coord(x, y) in self.walls:
                    print("#", end="")
                elif Coord(x, y) in self.open:
                    print(".", end="")
                else:
                    print(" ", end="")
            print()
        return self


if __name__ == "__main__":
    input_code = [int(x) for x in stdin.read().split(",")]
    INPUT_DEBUG = False
    controller = Controller(input_code, debug=INPUT_DEBUG).explore()
    if INPUT_DEBUG:
        controller.show()
    print(controller.steps_to_oxygen())
    print(controller.oxygen_fill())
