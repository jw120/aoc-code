"""Advent of Code 2022 - Day 16."""

# TODO
# Only consider moves which open a valve immediately

from __future__ import annotations

from collections import deque
from dataclasses import dataclass
from doctest import testmod
from re import fullmatch
from sys import stdin
from typing import cast


@dataclass
class Valve:
    """Data class for each valve."""

    name: str
    flow_rate: int
    tunnels: dict[str, int]  # Exit tunnels and distances


def read_valve(s: str) -> Valve:
    """Parse a value from a string.

    >>> read_valve("Valve BB has flow rate=13; tunnels lead to valves CC, AA")
    Valve(name='BB', flow_rate=13, tunnels={'CC': 1, 'AA': 1})
    """
    m = fullmatch(
        r"Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.+)", s
    )
    assert m, f"read_value match failed on '{s}'"
    name = m.group(1)
    flow_rate = int(m.group(2))
    tunnels = {s: 1 for s in cast(list[str], m.group(3).split(", "))}
    assert len(tunnels) > 0
    return Valve(name, flow_rate, tunnels)


@dataclass(frozen=True)
class WalkState:
    """State for depth-first walk of available paths"""

    current_valve_name: str  # Current position
    unopened: frozenset[str]  # Valves (with non-zero flow) still to be opened
    path: list[str]
    time_remaining: int
    score: int  # Flow released over all available time from valves opened so far


class Volcano:
    """Main state for day 16."""

    def __init__(self, s: str, start_valve_name: str) -> None:
        valve_list: list[Valve] = [read_valve(line.strip()) for line in s.splitlines()]
        self.valves: dict[str, Valve] = {v.name: v for v in valve_list}
        self.non_zero_valve_names: frozenset[str] = frozenset(
            v.name for v in self.valves.values() if v.flow_rate > 0
        )
        self.start_valve = self.valves[start_valve_name]
        self.stack: deque[WalkState] = deque()
        self.distances: dict[tuple[str, str], int] = {}
        updated = True
        while updated:
            updated = False
            for v_name in self.valves.keys():
                self.distances[(v_name, v_name)] = 0
                for t_name, t_distance in self.valves[v_name].tunnels.items():
                    for in_name in self.valves.keys():
                        if (in_name, v_name) in self.distances:
                            new_distance: int = (
                                self.distances[(in_name, v_name)] + t_distance
                            )
                            if (in_name, t_name) in self.distances:
                                old_distance = self.distances[(in_name, t_name)]
                                if old_distance <= new_distance:
                                    continue
                            updated = True
                            self.distances[(in_name, t_name)] = new_distance
                            self.distances[(t_name, in_name)] = new_distance
        for v1 in self.valves.keys():
            for v2 in self.valves.keys():
                print(v1, v2, self.distances[(v1, v2)])

    def push_move(
        self, state: WalkState, tunnel: str, distance: int, threshold_score: int
    ) -> None:
        """Push state that would be reached after moving to given tunnel from state."""

        assert state.time_remaining >= distance, f"Time up for move at {state}"
        #        if len(state.path) > 1 and tunnel == state.path[-2]:
        # print("Skipped double-back to", tunnel, "from", state.path)
        #            return
        possible_score = state.score + (state.time_remaining - distance) * sum(
            self.valves[v].flow_rate for v in state.unopened
        )
        if possible_score > threshold_score:
            new_path = state.path.copy()
            new_path.append(tunnel)
            self.stack.append(
                WalkState(
                    tunnel,
                    state.unopened,
                    new_path,
                    state.time_remaining - distance,
                    state.score,
                )
            )

    def push_open(self, state: WalkState, threshold_score: int) -> None:
        """Push state that would be reached by opening value from state."""
        if state.current_valve_name not in state.unopened:
            return
        # assert state.current_valve_name in state.unopened, f"Not unopened at {state}"
        assert state.time_remaining > 0, f"Time up for open at {state}"
        possible_score = state.score + (state.time_remaining - 1) * sum(
            self.valves[v].flow_rate for v in state.unopened
        )
        # print(
        #     "Considering open",
        #     state.current_valve_name,
        #     state.score,
        #     possible_score,
        #     threshold_score,
        #     state.unopened,
        # )
        if possible_score > threshold_score:
            flow_rate = self.valves[state.current_valve_name].flow_rate
            assert flow_rate > 0
            extra_score = flow_rate * (state.time_remaining - 1)
            new_path = state.path.copy()
            new_path.append("OPEN")
            self.stack.append(
                WalkState(
                    state.current_valve_name,
                    state.unopened - frozenset([state.current_valve_name]),
                    new_path,
                    state.time_remaining - 1,
                    state.score + extra_score,
                )
            )

    def generate_paths(self, time_available: int) -> int:
        """Generate all possible paths, returning the best score"""

        self.stack.append(
            WalkState(
                self.start_valve.name,
                self.non_zero_valve_names,
                [self.start_valve.name],
                time_available,
                0,
            )
        )
        best_score: int = 0

        while True:
            if not self.stack:
                break
            state = self.stack.pop()
            # print("Popped", state.score, state.path, state.unopened)
            if state.score > best_score:
                best_score = state.score
                print("Best", state.score, state.path)
            for destination in state.unopened:
                if destination == state.current_valve_name:
                    continue
                distance = self.distances[state.current_valve_name, destination]
                if distance < state.time_remaining:
                    self.push_move(state, destination, distance, best_score)
            self.push_open(state, best_score)
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
    testmod()
    # v = Volcano(TEST_DATA, "AA")
    v = Volcano(stdin.read(), "AA")
    print(v.generate_paths(30))
