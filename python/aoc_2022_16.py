"""Advent of Code 2022 - Day 16."""

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
    tunnels: list[str]


def read_valve(s: str) -> Valve:
    """Parse a value from a string.

    >>> read_valve("Valve BB has flow rate=13; tunnels lead to valves CC, AA")
    Valve(name='BB', flow_rate=13, tunnels=['CC', 'AA'])
    """
    m = fullmatch(
        r"Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.+)", s
    )
    assert m, f"read_value match failed on '{s}'"
    name = m.group(1)
    flow_rate = int(m.group(2))
    tunnels = cast(list[str], m.group(3).split(", "))
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

    def __init__(self, s: str) -> None:
        self.valves: list[Valve] = [read_valve(line.strip()) for line in s.splitlines()]
        self.non_zero_valve_names: frozenset[str] = frozenset(
            v.name for v in self.valves if v.flow_rate > 0
        )
        self.valve_dict: dict[str, Valve] = {v.name: v for v in self.valves}
        self.stack: deque[WalkState] = deque()

    def push_move(self, state: WalkState, tunnel: str, threshold_score: int) -> None:
        """Push state that would be reached after moving to given tunnel from state."""

        assert state.time_remaining > 0, f"Time up for move at {state}"
        if len(state.path) > 1 and tunnel == state.path[-2]:
            # print("Skipped double-back to", tunnel, "from", state.path)
            return
        possible_score = state.score + (state.time_remaining - 1) * sum(
            self.valve_dict[v].flow_rate for v in state.unopened
        )
        if possible_score > threshold_score:
            new_path = state.path.copy()
            new_path.append(tunnel)
            self.stack.append(
                WalkState(
                    tunnel,
                    state.unopened,
                    new_path,
                    state.time_remaining - 1,
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
            self.valve_dict[v].flow_rate for v in state.unopened
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
            flow_rate = self.valve_dict[state.current_valve_name].flow_rate
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

    def generate_paths(self, start_valve_name: str, time_available: int) -> int:
        """Generate all possible paths, returning the best score"""

        self.stack.append(
            WalkState(
                start_valve_name,
                self.non_zero_valve_names,
                [start_valve_name],
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
            if state.time_remaining == 0 or not state.unopened:
                # print("End of path", state.score, state.path)
                continue
            for tunnel in self.valve_dict[state.current_valve_name].tunnels:
                self.push_move(state, tunnel, best_score)
            # Explore opening the current valve
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
    # v = Volcano(TEST_DATA)
    v = Volcano(stdin.read())
    print(v.generate_paths("AA", 30))
