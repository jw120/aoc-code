"""Utility functions for Advent of Code."""

from typing import NoReturn


def assert_never(value: NoReturn) -> NoReturn:
    """Marker for unreachable code."""
    assert False, f"This code should never be reached, got: {value}"
