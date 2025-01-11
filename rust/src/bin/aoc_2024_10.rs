// Advent of Code 2024 - Day 10.

use aoc_rust::{ucoord, UCoord};
use grid::Grid;
use std::collections::HashSet;
use std::io;

fn read_topo_map() -> (Grid<u32>, HashSet<UCoord>) {
    let mut h: Grid<u32> = Grid::new(0, 0);
    for line in io::stdin().lines() {
        h.push_row(
            line.unwrap()
                .trim()
                .chars()
                .map(|ch| ch.to_digit(10).unwrap())
                .collect(),
        );
    }
    let trail_heads: HashSet<UCoord> = h
        .indexed_iter()
        .filter(|(_, h)| **h == 0)
        .map(|((row, col), _)| ucoord(row, col))
        .collect();

    (h, trail_heads)
}

// class Topo:
//     """Topographic map."""

//     def __init__(self, lines: list[str]) -> None:
//         self.extent = Extent(len(lines[0].strip()), len(lines))
//         self.h: list[list[int]] = [[int(ch) for ch in line.strip()] for line in lines]
//         self.trail_heads: set[Coord] = {crd for crd in self.extent.upto() if self.ht(crd) == 0}

//     def ht(self, crd: Coord) -> int:
//         """Return height at given coordinate."""
//         return self.h[crd.y][crd.x]

//     def print(self) -> None:
//         """Print heights for debugging."""
//         for line in self.h:
//             for h in line:
//                 print(h, end="")
//             print()

fn count_trails(_h: &Grid<u32>, _start: UCoord) -> usize {
    0
}

//     def count_trails(self, start: Coord) -> int:
//         """Return number of 9-height positions reachable."""
//         frontier: set[Coord] = {start}
//         reached: set[Coord] = set()
//         while frontier:
//             current = frontier.pop()
//             current_ht = self.ht(current)
//             if current_ht == 9:
//                 reached.add(current)
//             else:
//                 for adjacent in set(current.adjacents(self.extent)) - frontier:
//                     if self.ht(adjacent) == current_ht + 1:
//                         frontier.add(adjacent)
//         return len(reached)

//     def rating(self, start: Coord) -> int:
//         """Return number of distinct hiking trails from start."""
//         current: Coord = start
//         rating: int = 0  # Number of paths found so far
//         while True:
//             current_height = self.ht(current)
//             if current_height == 9:
//                 return rating + 1  # Found one path
//             exits = [
//                 crd for crd in current.adjacents(self.extent) if self.ht(crd) == current_height + 1
//             ]
//             if not exits:
//                 return rating  # Reached a dead-end
//             rating += sum(self.rating(x) for x in exits[1:])  # Check side-branches
//             current = exits[0]  # And keep searching

// if __name__ == "__main__":
//     topo = Topo(stdin.readlines())
//     print(sum(topo.count_trails(t) for t in topo.trail_heads))
//     print(sum(topo.rating(t) for t in topo.trail_heads))

fn main() {
    let (h, trail_heads) = read_topo_map();
    let score: usize = trail_heads.iter().map(|t| count_trails(&h, *t)).sum();
    println!("{score}");
}
