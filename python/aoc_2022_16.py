"""Advent of Code 2022 - Day 16.

Key insight - dfs where only consider moving to open a valve

"""


from __future__ import annotations

from collections import deque
from dataclasses import dataclass
from doctest import testmod
from re import fullmatch
from sys import stdin
from typing import cast, Iterable, TypeVar, TypeAlias


ValveName: TypeAlias = str


@dataclass
class Valve:
    """Data class for each colour valve."""

    name: ValveName
    flow_rate: int
    tunnels: list[ValveName]  # Exit tunnels

    @staticmethod
    def read(s: str) -> Valve:
        """Parse a value from a string.

        >>> Valve.read("Valve BB has flow rate=13; tunnels lead to valves CC, AA")
        Valve(name='BB', flow_rate=13, tunnels=['AA', 'CC'])
        """
        m = fullmatch(
            r"Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.+)", s
        )
        assert m, f"read_value match failed on '{s}'"
        name = m.group(1)
        flow_rate = int(m.group(2))
        tunnels = sorted(cast(list[str], m.group(3).split(", ")))
        assert len(tunnels) > 0
        return Valve(name, flow_rate, tunnels)

    @staticmethod
    def read_multi(s: str) -> list[Valve]:
        """Parse multiple valves from a multi-line string."""
        return [Valve.read(line.strip()) for line in s.splitlines()]


T = TypeVar("T")


def node_adj_to_dist(adj: dict[T, Iterable[T]], node: T) -> dict[T, int]:
    """Derive distances for one node for graph from node adjacencies.

    >>> a = {v.name: v.tunnels for v in Valve.read_multi(TEST_DATA)}
    >>> distances = node_adj_to_dist(a, "AA")
    >>> [distances[k] for k in sorted(distances.keys())]
    [0, 1, 2, 1, 2, 3, 4, 5, 1, 2]
    """
    distances: dict[T, int] = {}
    queue: deque[tuple[T, int]] = deque([(node, 0)])
    while queue:
        n, d = queue.popleft()
        distances[n] = d
        for x in adj[n]:
            if x not in distances:
                queue.append((x, d + 1))
    return distances


def graph_adj_to_dist(adj: dict[T, Iterable[T]]) -> dict[tuple[T, T], int]:
    """Derive distance matrix for graph from node adjacencies.

    >>> a = {v.name: v.tunnels for v in Valve.read_multi(TEST_DATA)}
    >>> d = graph_adj_to_dist(a)
    >>> d[("JJ", "HH")]
    7
    >>> d[("II", "CC")]
    3
    """
    d: dict[tuple[T, T], int] = {}
    for n1 in adj.keys():
        dist1 = node_adj_to_dist(adj, n1)
        for n2 in adj.keys():
            d[(n1, n2)] = dist1[n2]
            d[(n2, n2)] = dist1[n2]
    return d


@dataclass(frozen=True)
class WalkState:
    """State for depth-first walk of available paths"""

    current_valve_name: list[ValveName]
    time_remaining: list[int]  # for each walker
    path: list[list[ValveName]]  # for each walker (for debugging)
    unopened: frozenset[ValveName]  # Valves (with non-zero flow) still to be opened
    score: int  # Flow released over all available time from valves opened so far


class Volcano:
    """Main state for day 16."""

    def __init__(self, s: str, start_valve_name: str) -> None:
        valve_list: list[Valve] = [Valve.read(line.strip()) for line in s.splitlines()]
        self.valves: dict[str, Valve] = {v.name: v for v in valve_list}
        self.distances: dict[tuple[str, str], int] = graph_adj_to_dist(
            {v.name: v.tunnels for v in valve_list}
        )
        self.non_zero_valve_names: frozenset[str] = frozenset(
            v.name for v in self.valves.values() if v.flow_rate > 0
        )
        self.start_valve_name: str = start_valve_name

    def solve(self, time_available: int, walkers: int) -> int:
        """Find best scoring path.

        Depth-first search using the adjacency matrix.

        >>> v = Volcano(TEST_DATA, "AA")
        >>> v.solve(30, 1)
        1651
        >>> v.solve(26, 2)
        1707
        """
        stack: deque[WalkState] = deque([])

        stack.append(
            WalkState(
                current_valve_name=[self.start_valve_name] * walkers,
                time_remaining=[time_available] * walkers,
                path=[[] for _ in range(walkers)],
                unopened=self.non_zero_valve_names,
                score=0,
            )
        )
        best_score: int = 0

        while True:
            if not stack:
                break
            state = stack.pop()
            if state.score > best_score:
                best_score = state.score
                print(best_score, state.path)
            possible_flow_rate = sum(self.valves[v].flow_rate for v in state.unopened)
            for walker in range(walkers):
                #                print(state)
                assert set(state.path[walker]).isdisjoint(state.unopened)
                for destination in state.unopened:
                    if destination == state.current_valve_name[walker]:
                        continue
                    distance = self.distances[
                        state.current_valve_name[walker], destination
                    ]
                    walker_new_time_remaining = (
                        state.time_remaining[walker] - distance - 1
                    )
                    if walker_new_time_remaining > 0:
                        flow_rate = self.valves[destination].flow_rate
                        new_score = state.score + flow_rate * walker_new_time_remaining
                        possible_score = state.score + possible_flow_rate * max(
                            t - distance - 1 for t in state.time_remaining
                        )
                        if possible_score > best_score:
                            new_current_valve_name = state.current_valve_name.copy()
                            new_current_valve_name[walker] = destination
                            new_time_remaining = state.time_remaining.copy()
                            new_time_remaining[walker] = walker_new_time_remaining
                            new_path = [p.copy() for p in state.path]
                            new_path[walker].append(destination)
                            new_state = WalkState(
                                current_valve_name=new_current_valve_name,
                                time_remaining=new_time_remaining,
                                path=new_path,
                                unopened=state.unopened - frozenset([destination]),
                                score=new_score,
                            )
                            stack.append(new_state)
        return best_score


TEST_DATA = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"""


if __name__ == "__main__":
    #    testmod()
    # v = Volcano(TEST_DATA, "AA")
    v = Volcano(stdin.read(), "AA")
    print(v.solve(30, 1))
    print(v.solve(26, 2))
