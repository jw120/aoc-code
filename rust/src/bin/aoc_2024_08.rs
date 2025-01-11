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

fn read_map() -> Map {
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

fn check_bounds(m: &Map, coord: Coord) -> Option<Coord> {
    if coord.row >= 0 && coord.row < m.height && coord.col >= 0 && coord.col < m.width {
        Some(coord)
    } else {
        None
    }
}

fn anti_nodes_a(m: &Map) -> HashSet<Coord> {
    let mut locations: HashSet<Coord> = HashSet::new();
    for nodes in m.antenna.values() {
        for v in nodes.iter().combinations(2) {
            let [n1, n2] = v[..] else {
                panic!("Expected 2-vector from combinations(2)")
            };
            let diff = *n2 - *n1;
            check_bounds(m, *n1 - diff).map(|n| locations.insert(n));
            check_bounds(m, *n2 + diff).map(|n| locations.insert(n));
        }
    }
    locations
}

fn anti_nodes_b(m: &Map) -> HashSet<Coord> {
    let mut locations: HashSet<Coord> = HashSet::new();
    for nodes in m.antenna.values() {
        for v in nodes.iter().combinations(2) {
            let [n1, n2] = v[..] else {
                panic!("Expected 2-vector from combinations(2)")
            };
            let diff = *n2 - *n1;
            let mut n = *n1;
            while let Some(c) = check_bounds(m, n) {
                locations.insert(c);
                n += diff;
            }
            n = *n1 - diff;
            while let Some(c) = check_bounds(m, n) {
                locations.insert(c);
                n -= diff;
            }
        }
    }
    locations
}

fn main() {
    let m = read_map();
    println!("{}", anti_nodes_a(&m).len());
    println!("{}", anti_nodes_b(&m).len());
}
