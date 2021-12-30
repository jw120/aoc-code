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

# Valid hallway positions are those which are not exits
valid_hallway_positions: set[HallPosition] = {0, 1, 3, 5, 7, 9, 10}


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
    >>> expected_moves =  {(('A', True),  h) for h in [0,1]}
    >>> expected_moves |= {(('D', True),  h) for h in [7, 9, 10]}
    >>> expected_moves |= {(3, ('B', False))}
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

if __name__ == "__main__":
    testmod()
