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

fn adjacents(c: UCoord, (rows, cols): (usize, usize)) -> Vec<UCoord> {
    let mut v = Vec::with_capacity(4);
    if c.row > 0 {
        v.push(ucoord(c.row - 1, c.col));
    }
    if c.col > 0 {
        v.push(ucoord(c.row, c.col - 1));
    }
    if c.row < rows - 1 {
        v.push(ucoord(c.row + 1, c.col));
    }
    if c.col < cols - 1 {
        v.push(ucoord(c.row, c.col + 1));
    }
    v
}

fn count_trails(h: &Grid<u32>, start: UCoord) -> usize {
    let mut frontier: HashSet<UCoord> = HashSet::new();
    frontier.insert(start);
    let mut reached: HashSet<UCoord> = HashSet::new();
    while !frontier.is_empty() {
        let current = frontier.iter().next().copied().unwrap();
        frontier.remove(&current);
        let current_height = h[(current.row, current.col)];
        if current_height == 9 {
            reached.insert(current);
        } else {
            for a in adjacents(current, h.size()) {
                if !frontier.contains(&a) && h[(a.row, a.col)] == current_height + 1 {
                    frontier.insert(a);
                }
            }
        }
    }
    reached.len()
}

fn rating(h: &Grid<u32>, start: UCoord) -> usize {
    let mut current: UCoord = start;
    let mut paths: usize = 0;
    loop {
        let current_height = h[(current.row, current.col)];
        if current_height == 9 {
            return paths + 1;
        }
        let adjs = adjacents(current, h.size());
        let mut exits = adjs
            .iter()
            .filter(|c| h[(c.row, c.col)] == current_height + 1);
        if let Some(first_exit) = exits.next() {
            paths += exits.map(|x| rating(h, *x)).sum::<usize>();
            current = *first_exit;
        } else {
            return paths;
        }
    }
}

fn main() {
    let (h, trail_heads) = read_topo_map();
    let a: usize = trail_heads.iter().map(|t| count_trails(&h, *t)).sum();
    let b: usize = trail_heads.iter().map(|t| rating(&h, *t)).sum();
    print!("{a}\n{b}\n");
}
