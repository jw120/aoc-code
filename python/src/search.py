"""Basic search functions."""

from collections import deque
from collections.abc import Callable
from typing import TypeVar

S = TypeVar("S")


def bfs(start: S, at_goal: Callable[[S], bool], available: Callable[[S], list[S]]) -> int | None:
    """Conduct basic BFS search and return length of minimum path."""
    q: deque[tuple[S, int]] = deque([(start, 0)])
    distance: dict[S, int] = {start: 0}
    while q:
        s, dist = q.popleft()
        if at_goal(s):
            return dist
        new_states = [(a, dist + 1) for a in available(s) if a not in distance]
        q.extend(new_states)
        distance |= dict(new_states)
    return None
