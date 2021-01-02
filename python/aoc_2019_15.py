"""Advent of Code 2019 - Day 15."""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from random import randrange
from sys import stdin
from typing import Dict, Iterable, List, NoReturn, Optional, Set

from IntCode import Machine


def assert_never(value: NoReturn) -> NoReturn:
    assert False, f"Unhandled value: {value} ({type(value).__name__})"


@dataclass(eq=True, frozen=True)
class Coord:
    x: int
    y: int

    @staticmethod
    def origin() -> Coord:
        return Coord(0, 0)

    def move(self, d: Direction) -> Coord:
        if d is Direction.NORTH:
            return Coord(self.x, self.y + 1)
        elif d is Direction.EAST:
            return Coord(self.x + 1, self.y)
        if d is Direction.SOUTH:
            return Coord(self.x, self.y - 1)
        elif d is Direction.WEST:
            return Coord(self.x - 1, self.y)
        else:
            assert_never(d)

    def neighbours(self) -> Set[Coord]:
        """Return the four neighbours."""
        return {self.move(d) for d in Direction}

    def dist(self, other: Coord) -> int:
        """Manhattan distance to another point."""
        return abs(self.x - other.x) + abs(self.y - other.y)

    def closest(self, pts: Iterable[Coord]) -> Coord:
        """Return the closest points (or one of them if more than one)."""
        closest_dist: Optional[int] = None
        closest: Optional[Coord] = None
        for p in pts:
            dist = self.dist(p)
            if closest_dist is None or dist < closest_dist:
                closest = p
        if closest is not None:
            return closest
        raise RuntimeError("closest failed")


class Direction(Enum):
    NORTH = 1
    SOUTH = 2
    WEST = 3
    EAST = 4

    @staticmethod
    def random() -> Direction:
        return Direction(randrange(1, 5))


class Response(Enum):
    WALL = 0
    MOVED = 1
    MOVED_OXYGEN = 2


class Controller:
    def __init__(self, code: List[int]) -> None:
        self.m = Machine(code)
        self.m.pause_after_output = True
        self.m.pause_before_input = True
        self.loc: Coord = Coord.origin()
        self.walls: Set[Coord] = set()  # Locations we know are walls
        self.open: Set[Coord] = {self.loc}  # Locations we know are space
        self.oxygen: Optional[Coord] = None  # Location of oxygen if known

    def try_move(self, d: Direction) -> Response:
        """Try and move to an adjacent cell following the given direction."""
        self.m.input_vals = [d.value]
        self.m.run()
        return Response(self.m.output_vals.pop())

    def try_move_to_adj(self, c: Coord) -> Response:
        """Try and move to a given adjacent cell."""
        for d in Direction:
            if self.loc.move(d) == c:
                return self.try_move(d)
        raise RuntimeError("Adjacent move not found", self.loc, c)

    def _path_to(self, target: Coord) -> List[Coord]:
        """Generate a path to the given cell through open cells."""
        print("Routing from", self.loc, "to", target)
        # Flood fill with backlinks
        frontier: Dict[Coord, Coord] = {self.loc: self.loc}
        visited: Dict[Coord, Coord] = {}
        while target not in frontier:
            visited.update(frontier)
            new_frontier: Dict[Coord, Coord] = {}
            for f in frontier.keys():
                for n in f.neighbours():
                    if n == target or (n in self.open and n not in visited):
                        new_frontier[n] = f
            frontier = new_frontier

        # Build back tracking path
        x = target
        path: List[Coord] = []
        while x != self.loc:
            path.append(x)
            if x in frontier:
                x = frontier[x]
            else:
                x = visited[x]
        print("Path", path)
        return path

    def route_to(self, target: Coord) -> Response:
        """Try and move to a given cell going through known open cells."""
        path = self._path_to(target)
        for x in reversed(path[1:]):
            print("Move to", x)
            response = self.try_move_to_adj(x)
            if response == Response.WALL:
                raise RuntimeError("Ran into wall during route")
            self.loc = x

        return self.try_move_to_adj(target)

    def steps_to_oxygen(self) -> int:
        """Number of steps from the origin to oxygen."""
        self.loc = Coord.origin()
        if self.oxygen is None:
            raise RuntimeError("No oxygen found")
        return len(self._path_to(self.oxygen))

    def explore(self) -> Controller:
        """Explore the grid, completing the controllers knowledge of it."""
        # Frontier is all unknown locations adjacent to places we have visited
        frontier: Set[Coord] = self.loc.neighbours()

        while frontier:
            print("Loc:", self.loc)
            print("Fr:", "  ".join([f"{c.x},{c.y}" for c in frontier]))
            target = self.loc.closest(frontier)
            frontier.discard(target)
            print("Going to", target)
            response = self.route_to(target)
            if response == Response.WALL:
                print("Wall at", target)
                self.walls.add(target)
            else:
                print("Open at", target)
                self.loc = target
                self.open.add(target)
                frontier |= target.neighbours() - (self.open | self.walls)
                if response == Response.MOVED_OXYGEN:
                    self.oxygen = target

        return self

    def show(self) -> Controller:
        xs = [w.x for w in self.walls]
        ys = [w.y for w in self.walls]
        x_min = min(xs)
        x_max = max(xs)
        y_min = min(ys)
        y_max = max(ys)
        for y in range(y_max, y_min - 1, -1):
            for x in range(x_min, x_max + 1):
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
    code = [int(x) for x in stdin.read().split(",")]
    print(Controller(code).explore().show().steps_to_oxygen())
