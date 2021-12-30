"""Advent of Code 2021 - Day 23."""

from __future__ import annotations

from collections import Counter
from dataclasses import dataclass, field
from doctest import testmod
from heapq import heappop, heappush
from sys import stdin
from typing import Any, Iterable, Tuple, Union


"""

We hard-code the shape of the burrow, numbering the hall positions
and labelling the rooms by the type of the Amphipod which want to be there.

Positions are then either a room kind and an upper position flag or an
index for the hallway.

We number the Amphipods as 0..7 where 0..1 are type A, 2..3 are type B etc.

#############
#01234567890#
###A#B#C#D###
  #A#B#C#D#
  #########

"""

AmphipodKind = str
HallPosition = int
RoomPosition = Tuple[AmphipodKind, bool]
Position = Union[HallPosition, RoomPosition]
Amphipod = int
State = dict[Position, Amphipod]

# Cap number of hallway pods to speed up algorithm
max_hallway_number = 3

# Hallway positions by the exits of each room
room_exits: dict[AmphipodKind, HallPosition] = {"A": 2, "B": 4, "C": 6, "D": 8}

# Valid hallway positions are those which are not exits
valid_hallway_positions: set[HallPosition] = {0, 1, 3, 5, 7, 9, 10}


# Cost of moving each kind of amphipod
kind_cost: dict[AmphipodKind, int] = {"A": 1, "B": 10, "C": 100, "D": 1000}


def pod_kind(pod: int) -> AmphipodKind:
    """Return the kind of the given amphipod.

    >>> [pod_kind(pod) for pod in range(8)]
    ['A', 'A', 'B', 'B', 'C', 'C', 'D', 'D']
    """
    assert pod >= 0 and pod < 8
    return "ABCD"[pod // 2]


def room_available(state: State, target_kind: AmphipodKind) -> bool:
    """Is the target room available for an amphipod to move into.

    Available if only occupied by an amphipod of the same kind.

    >>> [room_available(test1, room) for room in "ABCD"]
    [False, False, False, False]

    >>> [room_available(test2, c) for c in "ABCD"]
    [False, False, True, False]
    """
    for position, pod in state.items():
        if not isinstance(position, int):
            room_kind, room_upper = position
            if room_kind == target_kind and pod_kind(pod) != target_kind:
                return False
    return True


def room_empty(state: State, kind: AmphipodKind) -> bool:
    """Is the given room empty (which allows an entering amphipod to take the bottom slot).

    >>> [room_empty(test1, room) for room in "ABCD"]
    [False, False, False, False]
    >>> [room_empty(test2, room) for room in "ABCD"]
    [False, False, False, False]
    >>> [room_empty(test_empty, room) for room in "ABCD"]
    [False, True, False, False]
    """
    for position, pod in state.items():
        if not isinstance(position, int):
            room_kind, room_upper = position
            if room_kind == kind:
                return False
    return True


def hallway_number(state: State) -> int:
    """Return the number of pods in the hallway.

    >>> [hallway_number(s) for s in [test1, test2, test3]]
    [0, 1, 2]
    """
    return sum(isinstance(pos, int) for pos in state.keys())


def path_available(state: State, p: RoomPosition, q: HallPosition) -> bool:
    """Is a path available between the room and hallway (no blocking amphipods).

    Does not test if either end-state is occupied.

    >>> [path_available(test1, (room, upper), 0) for room in "ABCD" for upper in [True, False]]
    [True, False, True, False, True, False, True, False]
    >>> [path_available(test2, (room, upper), 0) for room in "ABCD" for upper in [True, False]]
    [True, False, False, False, False, False, False, False]
    >>> [path_available(test2, (room, upper), 9) for room in "ABCD" for upper in [True, False]]
    [False, False, True, False, True, True, True, False]
    >>> [path_available(test3, (room, upper), 3) for room in "ABCD" for upper in [True, False]]
    [True, False, True, True, False, False, False, False]
    """
    p_room, p_upper = p
    # Check to see if blocked by upper position in the room
    if not p_upper and (p_room, True) in state:
        return False
    room_exit: HallPosition = room_exits[p_room]
    path = range(room_exit + 1, q) if room_exit < q else range(q + 1, room_exit)
    return all(p not in state for p in path)


def available_moves(state: State) -> Iterable[Tuple[Position, Position]]:
    """Return all available moves.

    >>> expected_moves = {((p, True), h) for p in "ABCD" for h in valid_hallway_positions}
    >>> set(available_moves(test1)) == expected_moves
    True
    >>> expected_moves =  {(('A', True),  h) for h in [0,1]}
    >>> expected_moves |= {(('B', True),  h) for h in [5, 7, 9, 10]}
    >>> expected_moves |= {(('D', True),  h) for h in [5, 7, 9, 10]}
    >>> set(available_moves(test2)) == expected_moves
    True
    >>> expected_moves = {(3, ('B', False))}
    >>> expected_moves |= (set() if max_hallway_number <= 2 else {(('A', True),  h) for h in [0,1]})
    >>> expected_moves |= (set() if max_hallway_number <= 2 else {(('D', True),  h) for h in [7, 9, 10]})
    >>> set(available_moves(test3)) == expected_moves
    True
    """
    for start_position, pod in state.items():
        kind = pod_kind(pod)
        # Amphipod is in the hallway, can only move to a room
        if isinstance(start_position, int):
            if room_available(state, kind):
                end_room: RoomPosition = (kind, not room_empty(state, kind))
                if path_available(state, end_room, start_position):
                    yield (start_position, end_room)
        # Amphipod in room moves to hallway
        else:
            if hallway_number(state) < max_hallway_number:
                start_kind, start_upper = start_position
                # Only try and move if pod is in the wrong room (or in the right room but above a wrong pod)
                if start_kind != kind or (
                    start_upper and pod_kind(state[(start_kind, False)]) != kind
                ):
                    for end_hall in valid_hallway_positions:
                        if end_hall not in state and path_available(
                            state, start_position, end_hall
                        ):
                            yield (start_position, end_hall)


def move(state: State, p: Position, q: Position) -> Tuple[State, int]:
    """Return a new state applying the given move and the cost of the move."""
    assert p in state, "Can't move from an empty position"
    assert q not in state, "Can't move to a non-empty position"

    def hallway_and_extra(x: Position) -> Tuple[int, int]:
        """Return the hallway index of the position and extra moves need in the room."""
        if isinstance(x, int):
            return x, 0
        else:
            kind, upper = x
            return room_exits[kind], 1 if upper else 2

    p_hallway, p_extra = hallway_and_extra(p)
    q_hallway, q_extra = hallway_and_extra(q)
    distance = abs(p_hallway - q_hallway) + p_extra + q_extra
    cost = distance * kind_cost[pod_kind(state[p])]
    # print("Moved", p, q, pod_kind(state[p]), cost)

    new_state = state.copy()
    new_state[q] = new_state[p]
    del new_state[p]

    return new_state, cost


def organized(state: State) -> bool:
    """Test if all the amphipods have been organized.

    >>> organized(test1)
    False
    >>> organized(test_organized)
    True
    """
    for position, pod in state.items():
        if isinstance(position, int):
            return False
        room_kind, _room_upper = position
        if room_kind != pod_kind(pod):
            return False
    return True


@dataclass(order=True)
class WrappedState:
    state_hash: StateHash = field(compare=False)
    cost: int


StateHash = frozenset[Tuple[Position, Amphipod]]


def solve(start_state: State) -> None:
    """Solve the state (BFS a la Wikipedia)."""
    pq: Any = []  # heapq is an untyped module
    heappush(pq, WrappedState(frozenset(start_state.items()), 0))
    explored: dict[StateHash, int] = {frozenset(start_state.items()): 0}
    while pq:
        wrapped_state = heappop(pq)
        state = dict(wrapped_state.state_hash)
        cost = explored[wrapped_state.state_hash]
        if cost in [240, 440]:
            print(f"Cost: {cost}, {len(pq)} in pq")
            show_state(state)
        if organized(state):
            print("Found solution", cost)
            show_state(state)
            # return
        for from_position, to_position in available_moves(state):
            new_state, extra_cost = move(state, from_position, to_position)
            new_state_hash = frozenset(new_state.items())
            new_cost = cost + extra_cost
            if new_state_hash in explored:
                if new_cost < explored[new_state_hash]:
                    explored[new_state_hash] = new_cost
            else:
                explored[new_state_hash] = new_cost
                heappush(pq, WrappedState(new_state_hash, new_cost))
    print("Finished")


state_template: str = (
    "#############\n#...........#\n###A#B#C#D###\n  #a#b#c#d#\n  #########"
)


def read_initial_state(s: str) -> State:
    """Read an initial state (all amphipods in rooms).

    >>> [pod_kind(test1[(k, u)]) for k in "ABCD" for u in [True, False]]
    ['B', 'A', 'C', 'D', 'B', 'C', 'D', 'A']
    >>> sorted(test1.values())
    [0, 1, 2, 3, 4, 5, 6, 7]
    >>> sorted(test1.keys())
    [('A', False), ('A', True), ('B', False), ('B', True), ('C', False), ('C', True), ('D', False), ('D', True)]
    """
    assert len(s) == len(
        state_template
    ), f"Wrong length for state {len(s)} vs. {len(state_template)}"
    state: State = {}
    amphipod_counts: Counter[AmphipodKind] = Counter()
    for ch, template in zip(s, state_template):
        if ch in "ABCD" and template in "AaBbCcDd":
            position = (template.upper(), template.isupper())
            pod = "ABCD".index(ch) * 2 + amphipod_counts[ch]
            amphipod_counts[ch] += 1
            state[position] = pod
        else:
            assert ch in "#.\n " and ch == template, (
                "Bad character: '" + ch + template + "'"
            )
    assert len(state) == 8, "Wrong number of items in state"
    assert all(
        amphipod_counts[c] == 2 for c in "ABCD"
    ), "Wrong number of kinds in state"
    return state


def show_state(state: State) -> None:
    print("".join(pod_kind(state[x]) if x in state else "." for x in range(11)))
    print("  ", end="")
    for kind in "ABCD":
        pod = state.get((kind, True), None)
        print(pod_kind(pod) + " " if pod is not None else ". ", end="")
    print("\n  ", end="")
    for kind in "ABCD":
        pod = state.get((kind, False), None)
        print(pod_kind(pod) + " " if pod is not None else ". ", end="")
    print()


""" test1 is the starting state for the example problem

#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########

"""
test1: State = read_initial_state(
    "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########"
)

""" test2 is the second step for the example problem

#############
#...B.......#
###B#C#.#D###
  #A#D#C#A#
  #########

"""
test2: State = test1.copy()
test2[3] = test2[("C", True)]
del test2[("C", True)]

""" test3 is an intermediate step shown from the example problem

#############
#...B.D.....#
###B#.#C#D###
  #A#.#C#A#
  #########

"""
test3: State = test2.copy()
test3[("C", True)] = test3[("B", True)]
test3[5] = test3[("B", False)]
del test3[("B", True)]
del test3[("B", False)]


# test_empty has an empty room
test_empty: State = test1.copy()
test_empty[0] = test_empty[("B", True)]
test_empty[1] = test_empty[("B", False)]
del test_empty[("B", True)]
del test_empty[("B", False)]

# test_organized is a fully organized_state
test_organized: State = dict(
    zip([(room, upper) for room in "ABCD" for upper in [True, False]], range(8))
)


class TestMove:
    def __init__(self) -> None:
        self.state = test1
        self.total_cost = 0

    def move(self, a: Position, b: Position) -> TestMove:
        assert (a, b) in available_moves(self.state)
        self.state, extra_cost = move(self.state, a, b)
        self.total_cost += extra_cost
        # print(self.total_cost)
        # show_state(self.state)
        return self


if __name__ == "__main__":
    testmod()

    t = (
        TestMove()
        .move(("C", True), 3)
        .move(("B", True), 5)
        .move(5, ("C", True))
        .move(("B", False), 5)
        .move(3, ("B", False))
        .move(("A", True), 3)
        .move(3, ("B", True))
        .move(("D", True), 7)
        .move(("D", False), 9)
        .move(7, ("D", False))
        .move(5, ("D", True))
        .move(9, ("A", True))
    )
    assert organized(t.state)
    assert t.total_cost == 12521

    # s0 = test1
    # cost = 0
    # show_state(s0)

    # move1 = (("C", True), 3)
    # assert move1 in available_moves(s0)
    # s1, extra_cost = move(s0, move1[0], move1[1])
    # cost += extra_cost
    # print(cost)
    # show_state(s1)

    # move2 = (("B", True), 5)
    # assert move2 in available_moves(s1)
    # s2, extra_cost = move(s1, move2[0], move2[1])
    # cost += extra_cost
    # print(cost)
    # show_state(s2)

    # move3 = (5, ("C", True))
    # assert move3 in available_moves(s2)
    # s3, extra_cost = move(s2, move3[0], move3[1])
    # cost += extra_cost
    # print(cost)
    # show_state(s3)

    # move4 = (("B", False), 5)
    # assert move4 in available_moves(s3)
    # s4, extra_cost = move(s3, move4[0], move4[1])
    # cost += extra_cost
    # print(cost)
    # show_state(s4)

    # move5 = (3, ("B", False))
    # assert move5 in available_moves(s4)
    # s5, extra_cost = move(s4, move5[0], move5[1])
    # cost += extra_cost
    # print(cost)
    # show_state(s5)

    # move6 = (("A", True), 3)
    # assert move6 in available_moves(s5)
    # s6, extra_cost = move(s5, move6[0], move6[1])
    # cost += extra_cost
    # print(cost)
    # show_state(s6)

    # move7 = (3, ("B", True))
    # assert move7 in available_moves(s6)
    # s7, extra_cost = move(s6, move7[0], move7[1])
    # cost += extra_cost
    # print(cost)
    # show_state(s7)

    solve(test1)

    state = read_initial_state(stdin.read().strip())
    solve(state)
