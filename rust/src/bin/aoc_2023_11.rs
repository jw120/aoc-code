// Advent of Code, 2023 day XX

use aoc_rust::{stdin_lines, UCoord};
use std::cmp::max;

fn read_galaxies() -> (usize, usize, Vec<UCoord>) {
    // Read galaxy positions, capturing size of the grid
    let mut galaxies: Vec<UCoord> = Vec::new();
    let mut rows: usize = 0;
    let mut cols: usize = 0;
    for (row, line) in stdin_lines().enumerate() {
        rows = row + 1;
        for (col, ch) in line.chars().enumerate() {
            cols = max(cols, col + 1);
            match ch {
                '#' => galaxies.push(UCoord { row, col }),
                '.' => {}
                _ => panic!("Unknown character '{}'", ch),
            }
        }
    }
    (rows, cols, galaxies)
}

fn shift(rows: usize, cols: usize, galaxies: &[UCoord], z: usize) -> Vec<UCoord> {
    let empty_rows: Vec<usize> = (0..rows)
        .filter(|row| galaxies.iter().all(|c| c.row != *row))
        .collect();
    let empty_cols: Vec<usize> = (0..rows)
        .filter(|col| galaxies.iter().all(|c| c.col != *col))
        .collect();
    let row_shift: Vec<usize> = (0..rows)
        .map(|row| row + (z - 1) * empty_rows.iter().filter(|r| **r < row).count())
        .collect();
    let col_shift: Vec<usize> = (0..cols)
        .map(|col| col + (z - 1) * empty_cols.iter().filter(|c| **c < col).count())
        .collect();
    galaxies
        .iter()
        .map(|g| UCoord {
            row: row_shift[g.row],
            col: col_shift[g.col],
        })
        .collect()
}

fn pair_distances(xs: &[UCoord]) -> usize {
    let mut distance: usize = 0;
    for (i, x) in xs.iter().enumerate() {
        for y in xs.iter().skip(i + 1) {
            distance += x.manhattan(y)
        }
    }
    distance
}

fn main() {
    let (rows, cols, galaxies) = read_galaxies();

    // part (a)
    let a_galaxies = shift(rows, cols, &galaxies, 2);
    println!("{}", pair_distances(&a_galaxies));

    // part (b)
    let b_galaxies = shift(rows, cols, &galaxies, 1_000_000);
    println!("{}", pair_distances(&b_galaxies));
}
