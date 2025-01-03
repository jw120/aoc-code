"""Advent of Code 2021 - Day 23."""

from __future__ import annotations

from collections import Counter
from doctest import testmod
from enum import Enum
from heapq import heappop, heappush
from sys import stdin
from typing import TYPE_CHECKING, Any, ClassVar

if TYPE_CHECKING:
    from collections.abc import Iterable


class Kind(Enum):
    """Amphipod kind and index for the rooms."""

    A = 0
    B = 1
    C = 2
    D = 3

    @property
    def hallway_exit(self) -> int:
        """Return the hallway index or the exit from the room of this kind."""
        return (self.value + 1) * 2

    @property
    def cost(self) -> int:
        """Return the cost of a one step move of an amphipod of this kind."""
        return {"A": 1, "B": 10, "C": 100, "D": 1000}[self.name]


class Amphipod:
    """Main class for day 23."""

    _kind_counts: ClassVar[Counter[Kind]] = Counter()
    _room_size: ClassVar[int] = 0

    def __init__(self, room_size: int, kind: Kind) -> None:
        assert room_size in {2, 4}, f"Invalid room size: {room_size}"
        self.kind = kind
        self.index = kind.value * room_size + Amphipod._kind_counts[kind]
        Amphipod._kind_counts[kind] += 1
        Amphipod._room_size = room_size

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Amphipod) and self.kind == other.kind and self.index == other.index

    def __hash__(self) -> int:
        return hash(self.index)

    def __str__(self) -> str:
        return self.kind.name + "_" + str(self.index)

    def __repr__(self) -> str:
        return f"Amphipod({self._room_size}, '{self.kind.name}')"

    @classmethod
    def reset(cls) -> None:
        """Reset the amphipod."""
        assert not cls._kind_counts or all(
            cls._kind_counts[kind] == cls._room_size for kind in Kind
        ), "Wrong number of pod kinds" + str(cls._kind_counts) + str(cls._room_size)
        cls._kind_counts = Counter()


class Position:
    """Position class."""

    hallway_size: ClassVar[int] = 11
    valid_hallway_indices: ClassVar[list[int]] = [0, 1, 3, 5, 7, 9, 10]

    def __init__(self, room: Kind | None, room_index: int) -> None:
        self.room = room
        self.index = room_index

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Position) and self.room == other.room and self.index == other.index

    def __hash__(self) -> int:
        return hash((self.room, self.index))

    def __str__(self) -> str:
        return "H" if self.room is None else self.room.name + "-" + str(self.index)

    def __repr__(self) -> str:
        room = "None" if self.room is None else "'" + self.room.name + "'"
        return f"Position({room}, {self.index})"


class State:
    """State holds a (mutable) mapping from positions to amphipods."""

    template: ClassVar[str] = (
        "#############\n#...........#\n###A#B#C#D###\n  #a#b#c#d#\n  #########"
    )

    def __init__(
        self,
        room_size: int,
        s: str | None,
    ) -> None:
        """Initialize a state (all amphipods in rooms).

        >>> test1.show()
        -----------
          B C B D
          A D C A
        """
        self.room_size = room_size
        self.mapping: dict[Position, Amphipod] = {}
        self.cost: int = 0  # Used only within .solve()
        assert room_size in {2, 4}, "Unknown room size"
        if s is not None:
            Amphipod.reset()
            assert len(s) == len(State.template), f"Wrong length {len(s)} vs. {len(State.template)}"
            for char, template in zip(s, State.template, strict=True):
                if char in "ABCD" and template in "AaBbCcDd":
                    position = Position(
                        Kind[template.upper()],
                        0 if template.isupper() else (room_size - 1),
                    )
                    self.mapping[position] = Amphipod(room_size, Kind[char])
                else:
                    assert char in "#.\n " and char == template, (
                        "Bad character: '" + char + template + "'"
                    )
            if room_size == 4:
                self.mapping[Position(Kind.A, 1)] = Amphipod(room_size, Kind.D)
                self.mapping[Position(Kind.B, 1)] = Amphipod(room_size, Kind.C)
                self.mapping[Position(Kind.C, 1)] = Amphipod(room_size, Kind.B)
                self.mapping[Position(Kind.D, 1)] = Amphipod(room_size, Kind.A)
                self.mapping[Position(Kind.A, 2)] = Amphipod(room_size, Kind.D)
                self.mapping[Position(Kind.B, 2)] = Amphipod(room_size, Kind.B)
                self.mapping[Position(Kind.C, 2)] = Amphipod(room_size, Kind.A)
                self.mapping[Position(Kind.D, 2)] = Amphipod(room_size, Kind.C)
            assert len(self.mapping) == 4 * room_size, "Wrong number of items in state"

    def room_available(self, target_kind: Kind) -> Position | None:
        """If the target room available for an amphipod to move into, return the open position.

        Available if only occupied by amphipods of the same kind.

        >>> [test1.room_available(kind) for kind in Kind]
        [None, None, None, None]
        >>> [test2.room_available(kind) for kind in Kind]
        [None, None, Position('C', 0), None]
        >>> [test3.room_available(kind) for kind in Kind if kind != Kind.C]
        [None, Position('B', 1), None]
        """
        # Target room is available if only occupied by pods of target kind
        for position, pod in self.mapping.items():
            if position.room == target_kind and pod.kind != target_kind:
                return None
        # Return highest index open space
        for i in range(self.room_size - 1, -1, -1):
            if (p := Position(target_kind, i)) not in self.mapping:
                return p
        raise RuntimeError("Unexpected request for room_available when room complete")

    @property
    def hallway_number(self) -> int:
        """Return the number of pods in the hallway.

        >>> [s.hallway_number for s in [test1, test2, test3]]
        [0, 1, 2]
        """
        return sum(position.room is None for position in self.mapping)

    def path_available(self, p: Position, q: Position) -> bool:
        """Is a path available between two positions (no blocking amphipods).

        Does not test if either end-state is occupied. Only considers paths from a room to a hallway
        (or vice versa).

        >>> [test1.path_available(Position(r, i), Position(None, 0)) for r in Kind for i in range(2)]
        [True, False, True, False, True, False, True, False]
        >>> [test2.path_available(Position(r, i), Position(None, 0)) for r in Kind for i in range(2)]
        [True, False, False, False, False, False, False, False]
        >>> [test2.path_available(Position(r, i), Position(None, 9)) for r in Kind for i in range(2)]
        [False, False, True, False, True, True, True, False]
        >>> [test3.path_available(Position(r, i), Position(None, 3)) for r in Kind for i in range(2)]
        [True, False, True, True, False, False, False, False]
        >>> [test3.path_available(Position(None, 3), Position(r, i)) for r in Kind for i in range(2)]
        [True, False, True, True, False, False, False, False]
        """
        if p.room is None:
            p, q = q, p
        assert p.room is not None, "path_available cannot be hallway to hallway"
        assert q.room is None, "path_available cannot be room to room"
        # Now we have p is a room position and q is a hallway position
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
    ) -> Iterable[tuple[Position, Position]]:
        """Return all available moves.

        >>> expected_moves = {(Position(k, 0), Position(None, h)) for k in Kind for h in Position.valid_hallway_indices}
        >>> set(test1.available_moves(3)) == expected_moves
        True
        >>> expected_moves =  {(Position(Kind.A, 0),  Position(None, h)) for h in [0,1]}
        >>> expected_moves |= {(Position(Kind.B, 0),  Position(None, h)) for h in [5, 7, 9, 10]}
        >>> expected_moves |= {(Position(Kind.D, 0),  Position(None, h)) for h in [5, 7, 9, 10]}
        >>> set(test2.available_moves(3)) == expected_moves
        True
        >>> expected_moves = {(Position(None, 3), Position(Kind.B, 1))}
        >>> expected_moves |= {(Position(Kind.A, 0),  Position(None, h)) for h in [0,1]}
        >>> expected_moves |= {(Position(Kind.D, 0),  Position(None, h)) for h in [7, 9, 10]}
        >>> set(test3.available_moves(3)) == expected_moves
        True
        """
        for start, pod in self.mapping.items():
            # Amphipod is in the hallway, can only move to the room of its own kind
            if start.room is None:
                end = self.room_available(pod.kind)
                if end is not None and self.path_available(start, end):
                    yield (start, end)
            # Amphipod in room moves to hallway
            elif self.hallway_number < max_hallway_number:
                # Only try and move if pod is in the wrong room or in the right room but above a wrong pod
                if start.room != pod.kind or any(
                    self.mapping.get(p, pod).kind != pod.kind
                    for p in (
                        Position(start.room, i) for i in range(start.index + 1, self.room_size)
                    )
                ):
                    for end in (Position(None, i) for i in Position.valid_hallway_indices):
                        if end not in self.mapping and self.path_available(start, end):
                            yield (start, end)

    def copy(self) -> State:
        """Return a copy of the state."""
        other = State(self.room_size, None)
        other.mapping = self.mapping.copy()
        other.cost = self.cost
        return other

    def move(self, p: Position, q: Position) -> State:
        """Return a new state applying the given move and the cost of the move."""
        assert p in self.mapping, "Can't move from an empty position"
        pod = self.mapping[p]
        assert q not in self.mapping, "Can't move to a non-empty position"

        def hallway_and_extra(x: Position) -> tuple[int, int]:
            """Return the hallway index of the position and extra moves need in the room."""
            if x.room is None:
                return x.index, 0
            return x.room.hallway_exit, x.index + 1

        p_hallway, p_extra = hallway_and_extra(p)
        q_hallway, q_extra = hallway_and_extra(q)
        distance = abs(p_hallway - q_hallway) + p_extra + q_extra
        extra_cost = distance * pod.kind.cost

        new_state = self.copy()
        new_state.mapping[q] = pod
        new_state.cost = self.cost + extra_cost
        del new_state.mapping[p]

        return new_state

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
        """Print debugging information."""
        print(
            "".join(
                self.mapping[Position(None, i)].kind.name
                if Position(None, i) in self.mapping
                else "-"
                for i in range(Position.hallway_size)
            )
        )
        for room_index in range(self.room_size):
            row: str = ""
            for kind in Kind:
                pod = self.mapping.get(Position(kind, room_index), None)
                row += pod.kind.name if pod is not None else "-"
            print(" ", " ".join(row))

    def __hash__(self) -> int:
        """Hash provided to allow set and dict lookup - based only on mapping."""
        return hash(frozenset(self.mapping.items()))

    def __eq__(self, other: object) -> bool:
        """Hash provided to allow set and dict lookup - based only on mapping."""
        return isinstance(other, State) and self.mapping == other.mapping


class QueuedState:
    """Wrapped version of state with an ordering.

    Provided to  allow use in a priority queue based only on cost.
    """

    def __init__(self, state: State) -> None:
        self.state = state

    def __lt__(self, other: object) -> bool:
        if isinstance(other, QueuedState):
            return self.state.cost < other.state.cost
        return NotImplemented


def solve(start_state: State, max_hallway_number: int) -> int:
    """Solve the state.

    >>> solve(test1, 3)
    12521
    >>> solve(test1_4, 8)
    44169
    """
    # Keep track of states to be explored in a priority queue of Tuple[State, Cost]
    # Complication is that we may find a lower cost to the state after it has been queued, to
    # take care of that we also keep track of the costs of queued items in a dictionary
    # and adjust the pq when we find a state with a lower cost.
    # Also note we used 'Any' as heapq is an untyped module
    p_q: Any = []  # heapq is an untyped module
    heappush(p_q, QueuedState(start_state))
    # Costs of everything in the p_q
    p_q_costs: dict[State, int] = {start_state: 0}
    # We also keep track of all states either visited or in the queue
    explored: set[State] = {start_state}
    while p_q:
        queued_state: QueuedState = heappop(p_q)
        state = queued_state.state
        del p_q_costs[state]
        if state.organized:
            return state.cost
        for from_position, to_position in state.available_moves(max_hallway_number):
            new_state = state.move(from_position, to_position)
            if new_state not in explored:  # First time seeing this state
                heappush(p_q, QueuedState(new_state))
                assert new_state not in p_q_costs
                p_q_costs[new_state] = new_state.cost
                explored.add(new_state)
            elif new_state in p_q_costs and new_state.cost < p_q_costs[new_state]:
                # State already in priority queue but now found with a lower cost
                p_q_removed: list[QueuedState] = []
                while (qs := heappop(p_q)).state != new_state:
                    p_q_removed.append(qs)
                heappush(p_q, QueuedState(new_state))
                for qs in p_q_removed:
                    heappush(p_q, qs)
                p_q_costs[new_state] = new_state.cost
    raise RuntimeError("No solution found")


test1: State = State(2, "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########")

""" test2 is the second step for the example problem

#############
#...B.......#
###B#C#.#D###
  #A#D#C#A#
  #########

"""
test2: State = test1.move(Position(Kind.C, 0), Position(None, 3))

""" test3 is an intermediate step shown from the example problem

#############
#...B.D.....#
###B#.#C#D###
  #A#.#C#A#
  #########

"""
test3: State = test2.copy()
test3.mapping[Position(Kind.C, 0)] = test3.mapping[Position(Kind.B, 0)]
test3.mapping[Position(None, 5)] = test3.mapping[Position(Kind.B, 1)]
del test3.mapping[Position(Kind.B, 0)]
del test3.mapping[Position(Kind.B, 1)]


# test_organized is a fully organized_state
test_organized: State = test1.copy()
Amphipod.reset()
test_organized.mapping = dict(
    zip(
        [Position(kind, i) for kind in Kind for i in range(test_organized.room_size)],
        [
            Amphipod(test_organized.room_size, kind)
            for kind in Kind
            for _ in range(test_organized.room_size)
        ],
        strict=True,
    )
)


test1_4: State = State(4, "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########")


class TestMove:
    """Test move class."""

    def __init__(self, max_hallway_number: int) -> None:
        self.state = test1
        self.max_hallway_number = max_hallway_number

    def move(self, a: Position, b: Position) -> TestMove:
        """Move."""
        assert (a, b) in self.state.available_moves(self.max_hallway_number)
        self.state = self.state.move(a, b)
        print(self.state.cost)
        self.state.show()
        return self


if __name__ == "__main__":
    testmod()
    input_data = stdin.read().strip()
    print(solve(State(2, input_data), 3))
    print(solve(State(4, input_data), 8))
