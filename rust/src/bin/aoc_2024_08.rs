// Advent of Code 2024 - Day 8.

use aoc_rust::{coord, Coord};
use itertools::Itertools;
use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::io;

struct Map {
    height: i32,
    width: i32,
    antenna: HashMap<char, Vec<Coord>>,
}

impl Map {
    fn read() -> Self {
        let mut height: i32 = 0;
        let mut width: i32 = 0;
        let mut antenna: HashMap<char, Vec<Coord>> = HashMap::new();
        for (line, row) in io::stdin().lines().zip(0..) {
            for (ch, col) in line.unwrap().trim().chars().zip(0..) {
                if ch != '.' {
                    antenna
                        .entry(ch)
                        .and_modify(|v| v.push(coord(row, col)))
                        .or_insert(vec![coord(row, col)]);
                }
                height = max(height, row + 1);
                width = max(width, col + 1);
            }
        }
        Map {
            height,
            width,
            antenna,
        }
    }

    fn check_bounds(&self, coord: Coord) -> Option<Coord> {
        if coord.row >= 0 && coord.row < self.height && coord.col >= 0 && coord.col < self.width {
            Some(coord)
        } else {
            None
        }
    }

    fn anti_nodes(&self) -> HashSet<Coord> {
        let mut locations: HashSet<Coord> = HashSet::new();
        for nodes in self.antenna.values() {
            for v in nodes.iter().combinations(2) {
                let [n1, n2] = v[..] else {
                    panic!("Expected 2-vector from combinations(2)")
                };
                let diff = *n2 - *n1;
                self.check_bounds(*n1 - diff).map(|n| locations.insert(n));
                self.check_bounds(*n2 + diff).map(|n| locations.insert(n));
            }
        }
        locations
    }
}

//     def antinodes_b(self) -> set[Coord]:
//         """Return antinode locations for part b."""
//         locations: set[Coord] = set()
//         for _frequency, nodes in self.antenna.items():
//             for n1, n2 in combinations(nodes, 2):
//                 d = n2 - n1
//                 d //= gcd(abs(d.x), abs(d.y))
//                 c = n1
//                 while c.in_bounds(self.extent):
//                     locations.add(c)
//                     c += d
//                 c = n1 - d
//                 while c.in_bounds(self.extent):
//                     locations.add(c)
//                     c -= d
//         return locations

// if __name__ == "__main__":
//     m = Map(stdin.readlines())
//     print(len(m.antinodes()))
//     print(len(m.antinodes_b()))

fn main() {
    let map = Map::read();
    println!("{}", map.anti_nodes().len());
}
