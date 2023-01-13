"""Utility functions for Advent of Code."""

from typing import Iterable, NoReturn, TypeVar


def assert_never(value: NoReturn) -> NoReturn:
    """Marker for unreachable code."""
    assert False, f"This code should never be reached, got: {value}"


T = TypeVar("T")


def set_union(xs: Iterable[set[T]]) -> set[T]:
    """Combine sets by union.

    Provided as using set.union(*xs) does not type-check.
    """
    x: set[T] = set()
    return x.union(*xs)


def set_intersection(xs: Iterable[set[T]]) -> set[T]:
    """Combine sets by union.

    Provided as using set.intersection(*xs) does not type-check.
    """
    x: set[T] = set()
    return x.intersection(*xs)


def sign(x: int) -> int:
    """Return the sign of an integer -1, 0, or 1."""
    if x > 0:
        return 1
    if x < 0:
        return -1
    return 0
