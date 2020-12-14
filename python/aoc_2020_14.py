"""Advent of Code 2020 - Day 14."""


from dataclasses import dataclass
from doctest import testmod
from re import compile
from sys import stdin
from typing import Dict, List, Pattern, Union


@dataclass(frozen=True)
class UpdateBitmaskCommand:
    mask: str


@dataclass(frozen=True)
class WriteCommand:
    address: int
    value: int


Command = Union[UpdateBitmaskCommand, WriteCommand]


def parse_command(s: str) -> Command:

    update_pattern: Pattern[str] = compile(r"mask = ([01X]+)\s*")
    write_pattern: Pattern[str] = compile(r"mem\[(\d+)\] = (\d+)\s*")

    if m := update_pattern.fullmatch(s):
        return UpdateBitmaskCommand(mask=m.group(1))
    if m := write_pattern.fullmatch(s):
        return WriteCommand(address=int(m.group(1)), value=int(m.group(2)))
    raise RuntimeError("Could not parse command", s)


def apply_mask(value: int, mask: str) -> int:
    """Apply a mask to the given value.

    >>> apply_mask(11, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X')
    73
    >>> apply_mask(101, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X')
    101
    >>> apply_mask(0, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X')
    64
    """
    all_ones = (1 << 35) - 1
    if len(mask) != 36:
        raise RuntimeError("Bad mask", mask)
    for i in range(0, 36):
        if mask[-i - 1] == "1":
            value |= 1 << i
        elif mask[-i - 1] == "0":
            value &= all_ones ^ (1 << i)
        elif mask[-i - 1] == "X":
            pass
        else:
            raise RuntimeError("Could not parse mask value", mask)

    return value


def run(commands: List[Command]) -> int:
    memory: Dict[int, int] = {}
    mask: str = "X" * 36
    for cmd in commands:
        if isinstance(cmd, UpdateBitmaskCommand):
            mask = cmd.mask
        elif isinstance(cmd, WriteCommand):
            memory[cmd.address] = apply_mask(cmd.value, mask)
        else:
            raise RuntimeError("Bad command", cmd)

    return sum(memory.values())


if __name__ == "__main__":
    testmod()
    commands: List[Command] = [parse_command(line) for line in stdin]
    print(run(commands))
