"""Advent of Code 2024 - Day 13."""

from math import gcd
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
    print("b_presses", b_presses, "cost", best_cost)
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
        print("No solution - zero denominator")
        return 0
    if (prize.x * b.y - prize.y * b.x) % (a.x * b.y - a.y * b.x) != 0:
        print("No solution - mod A")
        return 0
    a_solution = (prize.x * b.y - prize.y * b.x) // (a.x * b.y - a.y * b.x)
    # And then solve for B:
    if (prize.x - a.x * a_solution) % b.x != 0:
        print("No solution - mod B")
        return 0
    b_solution = (prize.x - a.x * a_solution) // b.x
    # Check it's right
    print(f"A={a_solution}, B={b_solution}")
    assert a.x * a_solution + b.x * b_solution == prize.x
    assert a.y * a_solution + b.y * b_solution == prize.y
    # Now we need to see if this is the best solution
    # We can shift solutions:
    gcd_x = gcd(a.x, b.x)
    a_delta_x = b.x // gcd_x
    b_delta_x = a.x // gcd_x
    assert a.x * (a_solution - a_delta_x) + b.x * (b_solution + b_delta_x) == prize.x
    cost_delta_x = -3 * a_delta_x + b_delta_x
    print(f"For x can shift a by {-a_delta_x} and b by {b_delta_x} for cost delta {cost_delta_x}")
    gcd_y = gcd(a.y, b.y)
    a_delta_y = b.y // gcd_y
    b_delta_y = a.y // gcd_y
    assert a.y * (a_solution - a_delta_y) + b.y * (b_solution + b_delta_y) == prize.y
    cost_delta_y = -3 * a_delta_y + b_delta_y
    print(f"For y can shift a by {-a_delta_y} and b by {b_delta_y} for cost delta {cost_delta_y}")
    # a_delta_both = a_delta_x * a_delta_y * b_delta_x * b_delta_y
    # b_delta_both = b_delta_x * b_delta_y * b_delta_x * b_delta_y
    # assert a.x * (a_solution - a_delta_both) + b.x * (b_solution + b_delta_both) == prize.x
    # assert a.y * (a_solution - a_delta_both) + b.y * (b_solution + b_delta_both) == prize.y
    # cost_delta_both = -3 * a_delta_both + b_delta_both
    # print(
    #     f"For both can shift a by {-a_delta_both} and b by {b_delta_both} for cost delta {cost_delta_both}"
    # )

    # print(a_solution, b_solution, a_solution * 3 + b_solution)
    return a_solution * 3 + b_solution


if __name__ == "__main__":
    machines = [Machine(block.splitlines()) for block in stdin.read().split("\n\n")]
    # print("Trial")
    # print(sum(lowest_cost_trial(m) for m in machines))
    # print("Analytic")
    # print(sum(lowest_cost_analytic(m) for m in machines))
    total = 0
    for m in machines:
        m.prize += Coord(10_000_000_000_000, 10_000_000_000_000)
        # trial_cost = lowest_cost_trial(m)
        analytic_cost = lowest_cost_analytic(m)
        # if trial_cost == analytic_cost:
        print("OK", m.a, m.b, m.prize, analytic_cost)
        total += analytic_cost
        # else:
        #     print("Failed", m.a, m.b, m.prize, trial_cost, analytic_cost)
    print("Total", total)
