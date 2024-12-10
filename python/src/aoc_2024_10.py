"""Advent of Code 2024 - Day 10."""

from sys import stdin

from coord import Coord, Extent


class Topo:
    """Topographic map."""

    def __init__(self, lines: list[str]) -> None:
        self.extent = Extent(len(lines[0].strip()), len(lines))
        self.h: list[list[int]] = [[int(ch) for ch in line.strip()] for line in lines]
        self.trail_heads: set[Coord] = {crd for crd in self.extent.upto() if self.ht(crd) == 0}

    def ht(self, crd: Coord) -> int:
        """Return height at given coordinate."""
        return self.h[crd.y][crd.x]

    def print(self) -> None:
        """Print heights for debugging."""
        for line in self.h:
            for h in line:
                print(h, end="")
            print()

    def count_trails(self, start: Coord) -> int:
        """Return number of 9-height positions reachable.

        Uses bfs.
        """
        visited: set[Coord] = set()
        frontier: set[Coord] = {start}
        count: int = 0
        while frontier:
            current = frontier.pop()
            visited.add(current)
            current_ht = self.ht(current)
            if current_ht == 9:
                count += 1
            else:
                for adjacent in set(current.adjacents(self.extent)) - visited - frontier:
                    if self.ht(adjacent) == current_ht + 1:
                        frontier.add(adjacent)
        return count

    def rating(self, start: Coord) -> int:
        """Return number of distinct hiking trails from start."""
        current: Coord = start
        rating: int = 0  # Number of paths found so far
        while True:
            current_height = self.ht(current)
            if current_height == 9:
                return rating + 1  # Found one path
            exits = [
                crd for crd in current.adjacents(self.extent) if self.ht(crd) == current_height + 1
            ]
            if not exits:
                return rating  # Reached a dead-end
            rating += sum(self.rating(x) for x in exits[1:])  # Check side-branches
            current = exits[0]  # And keep searching


if __name__ == "__main__":
    topo = Topo(stdin.readlines())
    print(sum(topo.count_trails(t) for t in topo.trail_heads))
    print(sum(topo.rating(t) for t in topo.trail_heads))
