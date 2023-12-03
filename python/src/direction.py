"""Simple direction class."""

from __future__ import annotations

from enum import Enum


class Direction(Enum):
    """Direction of movement."""

    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT = 3

    def left(self) -> Direction:
        """Return direction after rotating 90 degrees left."""
        return Direction((self.value - 1) % 4)

    def right(self) -> Direction:
        """Return direction after rotating 90 degrees right."""
        return Direction((self.value + 1) % 4)

    def opposite(self) -> Direction:
        """Return the opposite direction."""
        return Direction((self.value + 2) % 4)
