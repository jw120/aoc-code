// Advent of Code, 2023 day 21

use aoc_rust::{stdin_lines, UCoord};
use grid::Grid;
use std::collections::HashSet;

struct Garden {
    plots: Grid<bool>,
    start: UCoord,
}

impl Garden {
    fn read() -> Garden {
        let mut grid: Grid<bool> = Grid::new(0, 0);
        let mut start: Option<UCoord> = None;
        for (row, line) in stdin_lines().enumerate() {
            let mut grid_row: Vec<bool> = Vec::new();
            for (col, ch) in line.chars().enumerate() {
                if ch == 'S' {
                    start = Some(UCoord { row, col });
                    grid_row.push(true);
                } else {
                    grid_row.push(match ch {
                        '#' => false,
                        '.' => true,
                        _ => panic!("Unknown character '{ch}'"),
                    });
                }
            }
            grid.push_row(grid_row);
        }
        Garden {
            plots: grid,
            start: start.unwrap(),
        }
    }

    fn valid_moves(&self, c: UCoord) -> impl Iterator<Item = UCoord> {
        let xs: Vec<UCoord> = Vec::new();
        xs.into_iter()
    }

    fn count_paths(&self, n: usize) -> usize {
        let mut current: HashSet<UCoord> = HashSet::from([self.start]);
        for _step in 0..n {
            let previous = current.clone();
            current.clear();
            for source in previous {
                for dest in self.valid_moves(source) {
                    current.insert(dest);
                }
            }
        }
        current.len()
    }
}

fn main() {
    let garden = Garden::read();

    let part_a: usize = garden.count_paths(6);

    println!("{part_a}");
}
