"""Advent of Code 2024 - Day 13."""

from sys import stdin

from coord import Coord


class Machine:
    """Claw Machine."""

    def __init__(self, lines: list[str]) -> None:
        a, b, prize = lines
        self.a: Coord = parse_button(a, "A")
        self.b: Coord = parse_button(b, "B")
        self.prize: Coord = parse_prize(prize)


def parse_button(s: str, letter: str) -> Coord:
    """Parse a button description string."""
    x, rest = s.removeprefix("Button " + letter + ": X+").split(", ")
    y = rest.removeprefix("Y+")
    return Coord(int(x), int(y))


def parse_prize(s: str) -> Coord:
    """Parse a prize description string."""
    x, rest = s.removeprefix("Prize: X=").split(", ")
    y = rest.removeprefix("Y=")
    return Coord(int(x), int(y))


def lowest_cost(m: Machine) -> int:
    """Find lowest cost solution (or 0 if no solution)."""
    c = Coord(0, 0)
    best_cost = None
    for a_presses in range(101):
        delta = m.prize - c
        if delta.x % m.b.x == 0 and delta.y % m.b.y == 0:
            b_presses = delta.x // m.b.x
            if b_presses == delta.y // m.b.y:
                cost = 3 * a_presses + b_presses
                best_cost = cost if best_cost is None else min(cost, best_cost)
        c += m.a
    return 0 if best_cost is None else best_cost


if __name__ == "__main__":
    machines = [Machine(block.splitlines()) for block in stdin.read().split("\n\n")]
    print(sum(lowest_cost(m) for m in machines))
