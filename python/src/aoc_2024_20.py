"""Advent of Code 2024 - Day 20."""

from collections import deque
from sys import stdin

from coord import Coord, Extent
from search import bfs


class RaceTrack:
    """Main class."""

    def __init__(self, lines: list[str]) -> None:
        self.extent = Extent(len(lines[0].strip()), len(lines))
        self.wall: list[list[bool]] = []
        start: Coord | None = None
        end: Coord | None = None
        for y, line in enumerate(lines):
            self.wall.append([])
            assert len(line.strip()) == self.extent.x
            for x, ch in enumerate(line.strip()):
                if ch == "#":
                    self.wall[-1].append(True)
                else:
                    self.wall[-1].append(False)
                    if ch == "S":
                        start = Coord(x, y)
                    elif ch == "E":
                        end = Coord(x, y)
                    else:
                        assert ch == ".", f"Bad char '{ch}'"
        assert start is not None
        assert end is not None
        self.start: Coord = start
        self.end: Coord = end

    def __getitem__(self, coord: Coord) -> bool:
        return self.wall[coord.y][coord.x]

    def __setitem__(self, coord: Coord, value: bool) -> None:
        self.wall[coord.y][coord.x] = value

    def shortest_path(self, cheat: Coord | None = None) -> int | None:
        """Return length of shortest path from start to end."""
        return bfs(
            self.start,
            lambda c: c == self.end,
            lambda c: [c for c in c.adjacents(self.extent) if c == cheat or not self[c]],
        )

    def distances(self, reference: Coord) -> dict[Coord, int]:
        """Return distances to reference point for all reachable non-wall coordinates."""
        q: deque[tuple[Coord, int]] = deque([(reference, 0)])
        d: dict[Coord, int] = {reference: 0}
        while q:
            coord, dist = q.popleft()
            new_states = [
                (a, dist + 1) for a in coord.adjacents(self.extent) if not self[a] and a not in d
            ]
            q.extend(new_states)
            d |= dict(new_states)
        return d

    def cheats(self, threshold_saving: int) -> int:
        """Find all cheats with time saving on or above threshold."""
        # For every non-wall square, find distance from start and the end
        start_distances = self.distances(self.start)
        end_distances = self.distances(self.end)
        shortest_path = self.shortest_path()
        assert shortest_path is not None
        assert start_distances[self.end] == shortest_path
        assert end_distances[self.start] == shortest_path
        # for c, d in distances.items():
        #     if d < 10:
        #         print(c, d)
        # Minimum time to qualify
        threshold_distance = shortest_path - threshold_saving
        print("Threshold", threshold_distance)
        # Scan over all squares, look where a cheat would save threshold
        count = 0
        for coord in self.extent.upto_by_y():
            if coord not in start_distances:  # Skip walls and squares unreachable from the start
                continue
            for delta in (Coord(1, 0), Coord(0, 1), Coord(-1, 0), Coord(0, -1)):
                if (
                    self.extent.within(coord + delta)
                    and self[coord + delta]
                    and self.extent.within(coord + delta + delta)
                    and not self[coord + delta + delta]
                    and (coord + delta + delta) in end_distances
                ):
                    distance = start_distances[coord] + end_distances[coord + delta + delta] + 2
                    if distance <= threshold_distance:
                        print(coord, coord + delta + delta, distance)
                        count += 1
        return count

    def print(self) -> None:
        """Print for debugging."""
        for coord in self.extent.upto_by_y():
            if coord == self.start:
                print("S", end="")
            elif coord == self.end:
                print("E", end="")
            else:
                print("#" if self[coord] else ".", end="")
            if coord.x == self.extent.x - 1:
                print()


if __name__ == "__main__":
    race_track = RaceTrack(stdin.readlines())
    print(race_track.cheats(100))
