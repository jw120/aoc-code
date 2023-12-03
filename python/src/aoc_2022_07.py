"""Advent of Code 2022 - Day 7."""

from __future__ import annotations

from dataclasses import dataclass
from sys import stdin
from typing import Union


@dataclass
class File:
    """A file in the directory tree."""

    size: int


@dataclass
class NonRootDir:
    """A directory (other than '/') in the directory tree."""

    contents: dict[str, Content]
    parent: Dir


@dataclass
class RootDir:
    """The root directory in the directory tree."""

    contents: dict[str, Content]


Dir = Union[NonRootDir, RootDir]
Content = Union[NonRootDir, File]


def build(terminal_output: list[str]) -> RootDir:
    """Build root directory storage from terminal output."""
    root_dir: RootDir = RootDir(contents={})
    current_dir: Dir = root_dir
    listing_mode: bool = True

    for line in terminal_output:

        if listing_mode:
            match line.split():
                case ["dir", name]:
                    current_dir.contents[name] = NonRootDir(
                        contents={}, parent=current_dir
                    )
                    continue
                case [size, name]:
                    current_dir.contents[name] = File(size=int(size))
                    continue
                case ["$", *_]:
                    listing_mode = False
                case _:
                    raise ValueError(f"Bad listing line: {line}")

        match line.split():
            case ["$", "cd", "/"]:
                current_dir = root_dir
            case ["$", "cd", ".."]:
                if isinstance(current_dir, NonRootDir):
                    current_dir = current_dir.parent
            case ["$", "cd", name]:
                node = current_dir.contents[name]
                if not isinstance(node, NonRootDir):
                    raise ValueError(f"Can't cd to non-directory {line}")
                current_dir = node
            case ["$", "ls"]:
                listing_mode = True
            case _:
                raise ValueError(f"Bad command: {line}")

    return root_dir


def walk_sizes(d: Dir) -> list[int]:
    """Return sizes of all sub-directories and this directory.

    This directory is the last element of the list.
    """
    total_size = 0
    dir_sizes: list[int] = []
    for node in d.contents.values():
        match node:
            case File(size=size):
                total_size += size
            case NonRootDir() as sub_dir:
                sub_dir_sizes = walk_sizes(sub_dir)
                total_size += sub_dir_sizes[-1]
                dir_sizes += sub_dir_sizes
    dir_sizes.append(total_size)
    return dir_sizes


if __name__ == "__main__":
    input_terminal_output = [line.strip() for line in stdin]
    input_dir_sizes = walk_sizes(build(input_terminal_output))
    print(sum(size for size in input_dir_sizes if size <= 100000))
    current_space = 70000000 - input_dir_sizes[-1]
    space_needed = 30000000 - current_space
    print(min(size for size in input_dir_sizes if size >= space_needed))
