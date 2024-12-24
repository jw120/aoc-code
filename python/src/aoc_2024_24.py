"""Advent of Code 2024 - Day 24."""

from sys import stdin
from typing import Literal


type Gate = tuple[str, Literal["AND", "OR", "XOR"], str, str]


def apply(in1: bool, op: Literal["AND", "OR", "XOR"], in2: bool) -> bool:  # noqa: FBT001
    """Apply given gate."""
    match op:
        case "AND":
            return in1 and in2
        case "OR":
            return in1 or in2
        case "XOR":
            return in1 != in2


def parse(s: str) -> tuple[dict[str, bool], list[Gate]]:
    """Read initial assignments and gates."""
    s1, s2 = s.split("\n\n")
    assignments: dict[str, bool] = {}
    for line in s1.splitlines():
        gate, value = line.strip().split(": ")
        assert value in {"0", "1"}
        assignments[gate] = value == "1"
    gates: list[Gate] = []
    for line in s2.splitlines():
        in1, op, in2, arrow, out = line.strip().split()
        assert op in {"AND", "OR", "XOR"}
        assert arrow == "->"
        gates.append((in1, op, in2, out))
    return assignments, gates


def run(initial_assignments: dict[str, bool], gates: list[Gate]) -> dict[str, bool]:
    """Run the system to completion, return final assignments."""
    assignments: dict[str, bool] = initial_assignments.copy()
    remaining_gates: set[Gate] = set(gates)
    while remaining_gates:
        gates_to_drop: set[Gate] = set()
        for gate in remaining_gates:
            in1, op, in2, out = gate
            if out in assignments:
                gates_to_drop.add(gate)
                continue
            if in1 in assignments and in2 in assignments:
                out_value = apply(assignments[in1], op, assignments[in2])
                assignments[out] = out_value
                gates_to_drop.add(gate)
        remaining_gates -= gates_to_drop
    return assignments


def z(assignments: dict[str, bool]) -> int:
    """Extract binary integer from z-wires."""
    z_wires = sorted((k for k in assignments if k.startswith("z")), reverse=True)
    x = 0
    for wire in z_wires:
        x = (x << 1) | assignments[wire]
    return x


if __name__ == "__main__":
    initial_assignments, gates = parse(stdin.read())
    assignments = run(initial_assignments, gates)
    print(z(assignments))
