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


def lowest_cost_trial(m: Machine) -> int:
    """Find lowest cost solution by trial and error (or 0 if no solution)."""
    c = Coord(0, 0)
    best_cost = None
    b_presses = None
    for a_presses in range(101):
        delta = m.prize - c
        if delta.x % m.b.x == 0 and delta.y % m.b.y == 0:
            b_presses = delta.x // m.b.x
            if b_presses == delta.y // m.b.y:
                cost = 3 * a_presses + b_presses
                best_cost = cost if best_cost is None else min(cost, best_cost)
        c += m.a
    return 0 if best_cost is None else best_cost


def lowest_cost_analytic(m: Machine) -> int:
    """Find lowest cost solution analytically (or 0 if no solution)."""
    a, b, prize = m.a, m.b, m.prize
    # Need to solve:
    #   a.x * A + b.x * B = prize.x
    #   a.y * A + b.y * B = prize.y
    #
    # Take b.y times first equation minus b.x times second equation:
    #  (a.x * b.y - a.y * b.x) * A = prize.x * b.y - prize.y * b.x
    # Then solve for A
    if (a.x * b.y - a.y * b.x) == 0:
        return 0
    if (prize.x * b.y - prize.y * b.x) % (a.x * b.y - a.y * b.x) != 0:
        return 0
    a_solution = (prize.x * b.y - prize.y * b.x) // (a.x * b.y - a.y * b.x)
    # And then solve for B:
    if (prize.x - a.x * a_solution) % b.x != 0:
        return 0
    b_solution = (prize.x - a.x * a_solution) // b.x
    # Check it's right
    assert a.x * a_solution + b.x * b_solution == prize.x
    assert a.y * a_solution + b.y * b_solution == prize.y
    # Not clear that this will always be the lowest cost solution - but seems to work
    return a_solution * 3 + b_solution


if __name__ == "__main__":
    machines = [Machine(block.splitlines()) for block in stdin.read().split("\n\n")]
    print(sum(lowest_cost_trial(m) for m in machines))
    for m in machines:
        m.prize += Coord(10_000_000_000_000, 10_000_000_000_000)
    print(sum(lowest_cost_analytic(m) for m in machines))
