"""Advent of Code 2020 - Day 14."""


from dataclasses import dataclass
from doctest import testmod
from re import Pattern, compile
from sys import stdin
from typing import Union


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


def set_bit(x: int, n: int, val: bool) -> int:
    """Return a copy of x with the n'th bit set to val.

    >>> [set_bit(5, 1, True), set_bit(5, 1, False), set_bit(5, 2, True), set_bit(5, 2, False)]
    [7, 5, 5, 1]
    """
    target_bit: int = 1 << n
    if bool(x & target_bit) == val:
        return x
    else:
        return x ^ target_bit


def apply_mask1(value: int, mask: str) -> int:
    """Apply version one mask to the given value.

    >>> apply_mask1(11, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X')
    73
    >>> apply_mask1(101, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X')
    101
    >>> apply_mask1(0, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X')
    64
    """
    if len(mask) != 36:
        raise RuntimeError("Bad mask", mask)
    for i in range(0, 36):
        if mask[-i - 1] == "1":
            value = set_bit(value, i, True)
        elif mask[-i - 1] == "0":
            value = set_bit(value, i, False)
        elif mask[-i - 1] == "X":
            pass
        else:
            raise RuntimeError("Could not parse mask value", mask)

    return value


def apply_mask2(address: int, mask: str) -> list[int]:
    """Apply version two mask to the given value.

    Returns a list of addresses.
    >>> sorted(apply_mask2(42, '000000000000000000000000000000X1001X'))
    [26, 27, 58, 59]
    >>> sorted(apply_mask2(26, '00000000000000000000000000000000X0XX'))
    [16, 17, 18, 19, 24, 25, 26, 27]
    """
    free_bits: list[int] = []
    if len(mask) != 36:
        raise RuntimeError("Bad mask", mask)
    for i in range(0, 36):
        if mask[-i - 1] == "1":
            address = set_bit(address, i, True)
        elif mask[-i - 1] == "0":
            pass
        elif mask[-i - 1] == "X":
            free_bits.append(i)
        if len(mask) != 36:
            raise RuntimeError("Bad mask", mask)
    results: list[int] = []
    for x in range(0, 2 ** len(free_bits)):
        for j in range(0, len(free_bits)):
            address = set_bit(address, free_bits[-j], bool(x & (1 << j)))
        results.append(address)
    return results


def run1(commands: list[Command]) -> int:
    memory: dict[int, int] = {}
    mask: str = "X" * 36
    for cmd in commands:
        if isinstance(cmd, UpdateBitmaskCommand):
            mask = cmd.mask
        elif isinstance(cmd, WriteCommand):
            memory[cmd.address] = apply_mask1(cmd.value, mask)
        else:
            raise RuntimeError("Bad command", cmd)

    return sum(memory.values())


test2: list[Command] = [
    parse_command(s)
    for s in [
        "mask = 000000000000000000000000000000X1001X",
        "mem[42] = 100",
        "mask = 00000000000000000000000000000000X0XX",
        "mem[26] = 1",
    ]
]


def run2(commands: list[Command]) -> int:
    """Run using version 2.

    >>> run2(test2)
    208
    """
    memory: dict[int, int] = {}
    mask: str = ""
    for cmd in commands:
        if isinstance(cmd, UpdateBitmaskCommand):
            mask = cmd.mask
        elif isinstance(cmd, WriteCommand):
            for address in apply_mask2(cmd.address, mask):
                memory[address] = cmd.value
        else:
            raise RuntimeError("Bad command", cmd)

    return sum(memory.values())


if __name__ == "__main__":
    testmod()
    commands: list[Command] = [parse_command(line) for line in stdin]
    print(run1(commands))
    print(run2(commands))
