"""Advent of Code 2021 - Day 23."""

from __future__ import annotations

from collections import Counter
from doctest import testmod
from sys import stdin
from typing import Any, ClassVar, Iterable, Optional, Tuple

# from heapq import heappop, heappush


class Kind:
    """Amphipod kind and index for the rooms."""

    _indices: ClassVar[dict[str, int]] = {"A": 0, "B": 1, "C": 2, "D": 3}
    _costs: ClassVar[dict[str, int]] = {"A": 1, "B": 10, "C": 100, "D": 1000}
    _hallway_exits: ClassVar[dict[str, int]] = {"A": 2, "B": 4, "C": 6, "D": 8}

    def __init__(self, kind: str) -> None:
        assert kind in "ABCD", f"Invalid kind: '{kind}'"
        self.char: str = kind

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, Kind) and self.char == other.char

    def __hash__(self) -> int:
        return hash(self.char)

    def __str__(self) -> str:
        return self.char

    def __repr__(self) -> str:
        return f"Kind({self.char})"

    @staticmethod
    def every() -> Iterable[Kind]:
        for c in "ABCD":
            yield Kind(c)

    @property
    def hallway_exit(self) -> int:
        """Hallway position that is the exit from this kind's room."""
        return Kind._hallway_exits[self.char]

    @property
    def cost(self) -> int:
        """Cost of moving an amphipod of this kind."""
        return Kind._costs[self.char]

    @property
    def index(self) -> int:
        return Kind._indices[self.char]


class Amphipod:

    _kind_counts: ClassVar[Counter[Kind]] = Counter()
    _room_size: ClassVar[int] = 0

    def __init__(self, room_size: int, kind: Kind) -> None:
        assert room_size in [2, 4], f"Invalid room size: {room_size}"
        self.kind = kind
        self.index = kind.index * room_size + Amphipod._kind_counts[kind]
        Amphipod._kind_counts[kind] += 1
        Amphipod._room_size = room_size

    def __eq__(self, other: Any) -> bool:
        return (
            isinstance(other, Amphipod)
            and self.kind == other.kind
            and self.index == other.index
        )

    def __str__(self) -> str:
        return self.kind.char + "_" + str(self.index)

    def __repr__(self) -> str:
        return f"Amphipod({self._room_size}, '{self.kind.char}')"

    @classmethod
    def reset(cls) -> None:
        assert not cls._kind_counts or all(
            cls._kind_counts[kind] == cls._room_size for kind in Kind.every()
        ), ("Wrong number of pod kinds" + str(cls._kind_counts) + str(cls._room_size))
        cls._kind_counts = Counter()


class Position:

    hallway_size: ClassVar[int] = 11
    valid_hallway_indices: ClassVar[set[int]] = {0, 1, 3, 5, 7, 9, 10}

    def __init__(self, room: Optional[Kind], room_index: int) -> None:
        self.room = room
        self.index = room_index

    def __eq__(self, other: Any) -> bool:
        return (
            isinstance(other, Position)
            and self.room == other.room
            and self.index == other.index
        )

    def __hash__(self) -> int:
        return hash((self.room, self.index))

    def __str__(self) -> str:
        return "H" if self.room is None else self.room.char + "-" + str(self.index)

    def __repr__(self) -> str:
        room = "None" if self.room is None else "'" + self.room.char + "'"
        return f"Position({room}, {self.index})"


class State:
    """State holds a (mutable) mapping from positions to amphipods."""

    template: ClassVar[
        str
    ] = "#############\n#...........#\n###A#B#C#D###\n  #a#b#c#d#\n  #########"

    def __init__(
        self,
        room_size: int,
        s: Optional[str],
    ) -> None:
        """Initialize a state (all amphipods in rooms).

        >>> test1.show()
        -----------
          B C B D
          A D C A
        """
        self.room_size = room_size
        self.mapping: dict[Position, Amphipod] = {}
        if s is not None:
            Amphipod.reset()
            assert len(s) == len(
                State.template
            ), f"Wrong length {len(s)} vs. {len(State.template)}"
            for ch, template in zip(s, State.template):
                if ch in "ABCD" and template in "AaBbCcDd":
                    position = Position(
                        Kind(template.upper()),
                        0 if template.isupper() else (room_size - 1),
                    )
                    self.mapping[position] = Amphipod(room_size, Kind(ch))
                else:
                    assert ch in "#.\n " and ch == template, (
                        "Bad character: '" + ch + template + "'"
                    )
            assert len(self.mapping) == 4 * room_size, "Wrong number of items in state"

    def room_available(self, target_kind: Kind) -> bool:
        """Is the target room available for an amphipod to move into.

        Available if only occupied by amphipods of the same kind.

        >>> [test1.room_available(kind) for kind in Kind.every()]
        [False, False, False, False]

        >>> [test2.room_available(kind) for kind in Kind.every()]
        [False, False, True, False]
        """
        for position, pod in self.mapping.items():
            if position.room == target_kind and pod.kind != target_kind:
                return False
        return True

    @property
    def hallway_number(self) -> int:
        """Return the number of pods in the hallway.

        >>> [s.hallway_number for s in [test1, test2, test3]]
        [0, 1, 2]
        """
        return sum(position.room is None for position in self.mapping.keys())

    def path_available(self, p: Position, q: Position) -> bool:
        """Is a path available between the room and hallway (no blocking amphipods).

        Does not test if either end-state is occupied.

        >>> [test1.path_available(Position(r, i), Position(None, 0)) for r in Kind.every() for i in range(2)]
        [True, False, True, False, True, False, True, False]
        >>> [test2.path_available(Position(r, i), Position(None, 0)) for r in Kind.every() for i in range(2)]
        [True, False, False, False, False, False, False, False]
        >>> [test2.path_available(Position(r, i), Position(None, 9)) for r in Kind.every() for i in range(2)]
        [False, False, True, False, True, True, True, False]
        >>> [test3.path_available(Position(r, i), Position(None, 3)) for r in Kind.every() for i in range(2)]
        [True, False, True, True, False, False, False, False]
        """
        assert p.room is not None, "path_available start must be a room"
        assert q.room is None, "path_available destination must be a hallway"
        # Check to see if blocked by upper position in the room
        if any(Position(p.room, i) in self.mapping for i in range(p.index)):
            return False
        room_exit_index: int = p.room.hallway_exit
        if room_exit_index < q.index:
            path = range(room_exit_index + 1, q.index)
        else:
            path = range(q.index + 1, room_exit_index)
        return all(Position(None, i) not in self.mapping for i in path)

    def available_moves(
        self: State, max_hallway_number: int
    ) -> Iterable[Tuple[Position, Position]]:
        """Return all available moves.

        >>> expected_moves = {(Position(k, 0), Position(None, h)) for k in Kind.every() for h in Position.valid_hallway_indices}
        >>> expected_moves
        QQ
        >>> list(test1.available_moves(3))
        QQQ
        >>> set(test1.available_moves(3)) == expected_moves
        True
        # >>> expected_moves =  {(('A', True),  h) for h in [0,1]}
        # >>> expected_moves |= {(('B', True),  h) for h in [5, 7, 9, 10]}
        # >>> expected_moves |= {(('D', True),  h) for h in [5, 7, 9, 10]}
        # >>> set(available_moves(test2, 3)) == expected_moves
        # True
        # >>> expected_moves = {(3, ('B', False))}
        # >>> expected_moves |= {(('A', True),  h) for h in [0,1]}
        # >>> expected_moves |= {(('D', True),  h) for h in [7, 9, 10]}
        # >>> set(available_moves(test3, 3)) == expected_moves
        # True
        """
        # for start_position, pod in state.items():
        #     kind = pod_kind(pod)
        #     # Amphipod is in the hallway, can only move to a room
        #     if isinstance(start_position, int):
        #         if room_available(state, kind):
        #             end_room: RoomPosition = (kind, not room_empty(state, kind))
        #             if path_available(state, end_room, start_position):
        #                 yield (start_position, end_room)
        #     # Amphipod in room moves to hallway
        #     else:
        #         if hallway_number(state) < max_hallway_number:
        #             start_kind, start_upper = start_position
        #             # Only try and move if pod is in the wrong room (or in the right room but above a wrong pod)
        #             if start_kind != kind or (
        #                 start_upper and pod_kind(state[(start_kind, False)]) != kind
        #             ):
        #                 for end_hall in valid_hallway_positions:
        #                     if end_hall not in state and path_available(
        #                         state, start_position, end_hall
        #                     ):
        #                         yield (start_position, end_hall)
        pass

    def copy(self) -> State:
        """Return a copy of the state."""
        other = State(self.room_size, None)
        other.mapping = self.mapping.copy()
        return other

    def move(self, p: Position, q: Position) -> Tuple[State, int]:
        """Return a new state applying the given move and the cost of the move."""
        assert p in self.mapping, "Can't move from an empty position"
        pod = self.mapping[p]
        assert q not in self.mapping, "Can't move to a non-empty position"

        def hallway_and_extra(x: Position) -> Tuple[int, int]:
            """Return the hallway index of the position and extra moves need in the room."""
            if x.room is None:
                return x.index, 0
            else:
                return x.room.hallway_exit, x.index

        p_hallway, p_extra = hallway_and_extra(p)
        q_hallway, q_extra = hallway_and_extra(q)
        distance = abs(p_hallway - q_hallway) + p_extra + q_extra
        cost = distance * pod.kind.cost

        new_state = self.copy()
        new_state.mapping[q] = pod
        del new_state.mapping[p]

        return new_state, cost

    @property
    def organized(self) -> bool:
        """Test if all the amphipods have been organized.

        #>>> test1.organized
        #False
        >>> test_organized.organized
        True
        """
        for position, pod in self.mapping.items():
            if position.room is None or position.room != pod.kind:
                return False
        return True

    def show(self) -> None:
        print(
            "".join(
                self.mapping[Position(None, i)].kind.char
                if Position(None, i) in self.mapping
                else "-"
                for i in range(Position.hallway_size)
            )
        )
        for room_index in range(self.room_size):
            row: str = ""
            for kind in Kind.every():
                pod = self.mapping.get(Position(kind, room_index), None)
                row += pod.kind.char if pod is not None else "-"
            print(" ", " ".join(row))


StateHashable = frozenset[Tuple[Position, Amphipod]]


class WrappedState:
    def __init__(
        self, state_hashable: StateHashable, cost_lookup: dict[StateHashable, int]
    ) -> None:
        self.state_hashable = state_hashable
        self.cost_lookup = cost_lookup

    def cost(self) -> int:
        return self.cost_lookup[self.state_hashable]

    def __hash__(self) -> int:
        return hash(self.state_hashable)

    def __lt__(self, other: Any) -> bool:
        if isinstance(other, WrappedState):
            return self.cost() < other.cost()
        return NotImplemented


def solve(start_state: State, max_hallway_number: int) -> int:
    """Solve the state (BFS a la Wikipedia).

    >>> solve(test1, 3)
    12521
    """
    # start_state_hashable = frozenset(start_state.items())
    # pq: Any = []  # heapq is an untyped module
    # explored: dict[StateHashable, int] = {start_state_hashable: 0}
    # heappush(pq, WrappedState(start_state_hashable, explored))
    # while pq:
    #     wrapped_state = heappop(pq)
    #     state = dict(wrapped_state.state_hashable)
    #     cost = explored[wrapped_state.state_hashable]
    #     if organized(state):
    #         # print("Found solution", cost)
    #         # show_state(state)
    #         return cost
    #     for from_position, to_position in available_moves(state, max_hallway_number):
    #         new_state, extra_cost = move(state, from_position, to_position)
    #         new_state_hash = frozenset(new_state.items())
    #         new_cost = cost + extra_cost
    #         if new_state_hash in explored:
    #             if new_cost < explored[new_state_hash]:
    #                 explored[new_state_hash] = new_cost
    #         else:
    #             explored[new_state_hash] = new_cost
    #             heappush(pq, WrappedState(new_state_hash, explored))
    # raise RuntimeError("No solution found")


""" test1 is the starting state for the example problem

#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########

"""
test1: State = State(
    2, "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########"
)

""" test2 is the second step for the example problem

#############
#...B.......#
###B#C#.#D###
  #A#D#C#A#
  #########

"""
test2: State = test1.move(Position(Kind("C"), 0), Position(None, 3))[0]

""" test3 is an intermediate step shown from the example problem

#############
#...B.D.....#
###B#.#C#D###
  #A#.#C#A#
  #########

"""
test3: State = test2.copy()
test3.mapping[Position(Kind("C"), 0)] = test3.mapping[Position(Kind("B"), 0)]
test3.mapping[Position(None, 5)] = test3.mapping[Position(Kind("B"), 1)]
del test3.mapping[Position(Kind("B"), 0)]
del test3.mapping[Position(Kind("B"), 1)]


# test_organized is a fully organized_state
test_organized: State = test1.copy()
Amphipod.reset()
test_organized.mapping = dict(
    zip(
        [
            Position(kind, i)
            for kind in Kind.every()
            for i in range(test_organized.room_size)
        ],
        [
            Amphipod(test_organized.room_size, kind)
            for kind in Kind.every()
            for _ in range(test_organized.room_size)
        ],
    )
)


# class TestMove:
#     def __init__(self) -> None:
#         self.state = test1
#         self.total_cost = 0

#     def move(self, a: Position, b: Position) -> TestMove:
#         assert (a, b) in available_moves(self.state)
#         self.state, extra_cost = move(self.state, a, b)
#         self.total_cost += extra_cost
#         # print(self.total_cost)
#         # show_state(self.state)
#         return self


# t = (
#     TestMove()
#     .move(("C", True), 3)
#     .move(("B", True), 5)
#     .move(5, ("C", True))
#     .move(("B", False), 5)
#     .move(3, ("B", False))
#     .move(("A", True), 3)
#     .move(3, ("B", True))
#     .move(("D", True), 7)
#     .move(("D", False), 9)
#     .move(7, ("D", False))
#     .move(5, ("D", True))
#     .move(9, ("A", True))
# )
# assert organized(t.state)
# assert t.total_cost == 12521


if __name__ == "__main__":
    testmod()
#    state = State(2, stdin.read().strip())
#    print(state.solve(3))
