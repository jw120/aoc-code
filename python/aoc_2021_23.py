"""Advent of Code 2021 - Day 23."""

# from __future__ import annotations

from collections import Counter
from doctest import testmod
from typing import Iterable, Tuple, Union

# from sys import stdin


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

# Hallway positions by the exits of each room
room_exits: dict[AmphipodKind, HallPosition] = {"A": 2, "B": 4, "C": 6, "D": 8}


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


def path_available(state: State, p: RoomPosition, q: HallPosition) -> bool:
    """Is a path available between the room and hallway (no blocking amphipods).

    >>> path_available(test1, ('A', True), 0)
    True
    """
    p_room, p_upper = p
    # Check to see if blocked by upper position in the room
    if not p_upper and (p_room, True) in state:
        return False
    room_exit: HallPosition = room_exits[p_room]
    path = range(room_exit, q + 1) if room_exit < 1 else range(q, room_exit + 1)
    return all(p not in state for p in path)


def available_moves(state: State) -> Iterable[Tuple[Amphipod, Position]]:
    for position, pod in state.items():
        # If the Amphipod is in the hallway, can only move to a room
        if isinstance(position, int):
            kind = pod_kind(pod)
            if room_available(state, kind):
                new_position: RoomPosition = (kind, not room_empty(state, kind))
                if path_available(state, new_position, position):
                    yield (pod, (kind, not room_empty(state, kind)))


#            room_kind, room_upper = position.room


# def organized(state) -> bool:
#     """Test if all the amphipods have been organized."""
#     for a in state.keys:
#         if a.type:
#             pass


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


# Test 1 is the starting state for the example problem
test1: State = read_initial_state(
    "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########"
)

# Test 2 is the second state for the example problem
test2: State = test1.copy()
test2[4] = test2[("C", True)]
del test2[("C", True)]

# Test empty has an empty room
test_empty: State = test1.copy()
test_empty[0] = test_empty[("B", True)]
test_empty[1] = test_empty[("B", False)]
del test_empty[("B", True)]
del test_empty[("B", False)]

# class State:
#     def __init__(lines: list[str]) -> None:
#         assert len(lines) > 0, "No lines in maze"
#         assert all(
#             len(line) == len(line[0]) for line in lines[1:]
#         ), "Uneven lines in maze"
#         # self[]
#         # self.extent = Extent(len(lines[0]), len(lines))
#         # self.walls: dict[Coord, bool] = {c: lines[c.y][c.x] == "#"


if __name__ == "__main__":
    testmod()
