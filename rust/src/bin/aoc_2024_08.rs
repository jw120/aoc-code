// Advent of Code 2024 - Day 8.

use itertools::Itertools;
use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::io;

struct Map {
    height: usize,
    width: usize,
    antenna: HashMap<char, Vec<(usize, usize)>>,
}

impl Map {
    fn read() -> Self {
        let mut height: usize = 0;
        let mut width: usize = 0;
        let mut antenna: HashMap<char, Vec<(usize, usize)>> = HashMap::new();
        for (y, line) in io::stdin().lines().enumerate() {
            for (x, ch) in line.unwrap().trim().chars().enumerate() {
                if ch != '.' {
                    antenna
                        .entry(ch)
                        .and_modify(|v| v.push((x, y)))
                        .or_insert(vec![(x, y)]);
                }
                height = max(height, y + 1);
                width = max(width, x + 1);
            }
        }
        Map {
            height,
            width,
            antenna,
        }
    }

    fn check_bounds(&self, x: i32, y: i32) -> Option<(usize, usize)> {
        let x = usize::try_from(x).ok()?;
        let y = usize::try_from(y).ok()?;
        if x < self.width && y < self.height {
            Some((x, y))
        } else {
            None
        }
    }

    fn anti_nodes(&self) -> HashSet<(usize, usize)> {
        let mut locations: HashSet<(usize, usize)> = HashSet::new();
        for nodes in self.antenna.values() {
            for v in nodes.iter().combinations(2) {
                let [(n1_x, n1_y), (n2_x, n2_y)] = v[..] else {
                    panic!("Expected 2-vector from combinations(2)")
                };
                let (n1_x, n1_y) = (i32::try_from(*n1_x).unwrap(), i32::try_from(*n1_y).unwrap());
                let (n2_x, n2_y) = (i32::try_from(*n2_x).unwrap(), i32::try_from(*n2_y).unwrap());
                let (diff_x, diff_y) = (n2_x - n1_x, n2_y - n1_y);
                if let Some(anti_node) = self.check_bounds(n1_x - diff_x, n1_y - diff_y) {
                    locations.insert(anti_node);
                }
                if let Some(anti_node) = self.check_bounds(n2_x + diff_x, n2_y + diff_y) {
                    locations.insert(anti_node);
                }
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
