// Advent of Code, 2025 day XX

use std::cmp;
use std::io;

#[derive(Debug)]
struct Coord {
    x: u64,
    y: u64,
}

impl Coord {
    fn new(s: &str) -> Self {
        let Some((x, y)) = s.trim().split_once(',') else {
            panic!("Bad coord '{s}'");
        };
        Self {
            x: x.parse().unwrap(),
            y: y.parse().unwrap(),
        }
    }

    fn area(&self, other: &Coord) -> u64 {
        (self.x.abs_diff(other.x) + 1) * (self.y.abs_diff(other.y) + 1)
    }
}

fn part_a(tiles: &[Coord]) -> u64 {
    let mut best_area: u64 = 0;
    for (i, c) in tiles.iter().enumerate() {
        for d in &tiles[i + 1..] {
            let area = c.area(d);
            best_area = cmp::max(area, best_area);
        }
    }
    best_area
}

fn main() {
    let tiles: Vec<Coord> = io::stdin()
        .lines()
        .map(|s| Coord::new(&s.unwrap()))
        .collect();

    println!("{}", part_a(&tiles));
}
