"""Advent of Code 2021 - Day 23."""

# from __future__ import annotations

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

room_exits: dict[AmphipodKind, HallPosition] = {"A": 2, "B": 4, "C": 6, "D": 8}


def pod_kind(pod: int) -> AmphipodKind:
    """Return the kind of the given amphipod.

    >>> [target_room(pod) for pod in range(8)]
    ['A', 'A', 'B', 'B', 'C', 'C', 'D', 'D']
    """
    assert pod >= 0 and pod < 8
    return "ABCD"[pod // 2]


def room_available(state: State, kind: AmphipodKind) -> bool:
    """Is the room available for an amphipod to move into.

    Available if only occupied by an amphipod of the same kind.

    >>> room_available(test1, ")

    """
    for pod, position in state.items():
        if not isinstance(position, int):
            room_kind, room_upper = position
            if room_kind == kind and pod_kind(pod) != kind:
                return False
    return True


def room_empty(state: State, kind: AmphipodKind) -> bool:
    """Is the given room empty (which allows an entering amphipod to take the bottom slot)."""
    for pod, position in state.items():
        if not isinstance(position, int):
            room_kind, room_upper = position
            if room_kind == kind:
                assert not room_upper, "Don't expect only upper position filled"
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

test1 = {
    0: ("A", False),
    1: ("D", False),
    2: ("A", True),
    3: ("C", True),
    4: ("B", True),
    5: ("C", False),
    6: ("B", False),
    7: ("D", True),
}


def read_state(s: list[str]) -> State:
    pass


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
