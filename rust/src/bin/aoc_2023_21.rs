// Advent of Code, 2023 day 21

use aoc_rust::{stdin_lines, Coord, UCoord};
use grid::Grid;
use std::collections::{HashMap, HashSet};

// Helper to construct UCoords
fn ucoord(row: usize, col: usize) -> UCoord {
    UCoord { row, col }
}

fn coord(row: i32, col: i32) -> Coord {
    Coord { row, col }
}

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

    // Helper function to look up UCoord in plots
    fn plot(&self, c: UCoord) -> bool {
        self.plots[(c.row, c.col)]
    }

    // Count paths on simple grid
    fn count_paths(&self, n: usize) -> usize {
        let mut current: HashSet<UCoord> = HashSet::from([self.start]);
        for _step in 0..n {
            let previous = current.clone();
            current.clear();
            for source in previous {
                let r = source.row;
                let c = source.col;
                if r > 0 && self.plots[(r - 1, c)] {
                    current.insert(UCoord { row: r - 1, col: c });
                }
                if r < self.plots.rows() - 1 && self.plots[(r + 1, c)] {
                    current.insert(UCoord { row: r + 1, col: c });
                }
                if c > 0 && self.plots[(r, c - 1)] {
                    current.insert(UCoord { row: r, col: c - 1 });
                }
                if c < self.plots.cols() - 1 && self.plots[(r, c + 1)] {
                    current.insert(UCoord { row: r, col: c + 1 });
                }
            }
        }
        current.len()
    }

    // Count paths on repeating grid
    fn count_paths_repeating(&self, n: usize) -> usize {
        let rows = self.plots.rows();
        let cols = self.plots.cols();
        println!("{rows} {cols}");
        assert_eq!(rows % 2, 0);
        assert_eq!(cols % 2, 0);

        // Represent plots reached after each step by the offsets of
        // the repeating plot that are reached for each plot in the original
        // garden. We note that on even steps the elf can only be at plots
        // with the same parity (row + col even/odd) as the start location
        let mut state: HashMap<UCoord, HashSet<Coord>> = HashMap::new();
        for row in 0..self.plots.rows() {
            for col in 0..self.plots.cols() {
                state.insert(UCoord { row, col }, HashSet::new());
            }
        }
        state
            .get_mut(&self.start)
            .unwrap()
            .insert(Coord { row: 0, col: 0 });

        for step in 0..n {
            // Clear all the state elements for the opposite parity
            for row in 0..self.plots.rows() {
                for col in 0..self.plots.cols() {
                    if (row + col) % 2 != (self.start.row + self.start.col + step) % 2 {
                        state.get_mut(&UCoord { row, col }).unwrap().clear();
                    }
                }
            }

            // Update
            for row in 0..self.plots.rows() {
                for col in 0..self.plots.cols() {
                    if (row + col) % 2 == (self.start.row + self.start.col + step) % 2 {
                        let mut targets: Vec<(UCoord, Coord)> = Vec::new();
                        let target = ucoord(if row == 0 { rows - 1 } else { row - 1 }, col);
                        let offset_delta = coord(if row == 0 { -1 } else { 0 }, 0);
                        if self.plot(target) {
                            targets.push((target, offset_delta));
                        }
                        let target = ucoord(if row == rows - 1 { 0 } else { row + 1 }, col);
                        let offset_delta = coord(if row == rows - 1 { -1 } else { 0 }, 0);
                        if self.plot(target) {
                            targets.push((target, offset_delta));
                        }
                        let target = ucoord(row, if col == 0 { cols - 1 } else { col - 1 });
                        let offset_delta = coord(0, if col == 0 { -1 } else { 0 });
                        if self.plot(target) {
                            targets.push((target, offset_delta));
                        }
                        let target = ucoord(row, if col == cols - 1 { 0 } else { col + 1 });
                        #[allow(clippy::bool_to_int_with_if)]
                        let offset_delta = coord(0, if col == cols - 1 { 1 } else { 0 });
                        if self.plot(target) {
                            targets.push((target, offset_delta));
                        }
                        let offsets = state.get(&UCoord { row, col }).unwrap().clone();
                        for (target, offset_delta) in &targets {
                            for offset in &offsets {
                                state
                                    .get_mut(target)
                                    .unwrap()
                                    .insert(*offset + *offset_delta);
                            }
                        }
                    }
                }
            }
        }

        // Count final states
        let mut count: usize = 0;
        for row in 0..self.plots.rows() {
            for col in 0..self.plots.cols() {
                if (row + col) % 2 == (self.start.row + self.start.col + n) % 2 {
                    count += state.get(&ucoord(row, col)).unwrap().len();
                }
            }
        }
        count
    }
}

fn main() {
    let garden = Garden::read();

    println!("{}", garden.count_paths(64));
    println!("{}", garden.count_paths_repeating(50));
}
