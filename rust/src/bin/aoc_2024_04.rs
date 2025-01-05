// Advent of Code 2024 - Day 4

use grid::Grid;
use std::io;

fn read_puzzle() -> Grid<char> {
    let mut puzzle: Grid<char> = Grid::new(0, 0);
    for line in io::stdin().lines() {
        let line = line.unwrap();
        let row: Vec<char> = line.chars().collect();
        puzzle.push_row(row);
    }
    puzzle
}

fn apply_delta(
    (c_row, c_col): (usize, usize),
    (delta_row, delta_col): (i32, i32),
    (size_row, size_col): (usize, usize),
) -> Option<(usize, usize)> {
    let row: i32 = i32::try_from(c_row).unwrap() + delta_row;
    let col: i32 = i32::try_from(c_col).unwrap() + delta_col;
    let row_max: i32 = i32::try_from(size_row).unwrap();
    let col_max: i32 = i32::try_from(size_col).unwrap();
    if row >= 0 && row < row_max && col >= 0 && col < col_max {
        Some((usize::try_from(row).unwrap(), usize::try_from(col).unwrap()))
    } else {
        None
    }
}

fn check_xmas(puzzle: &Grid<char>, x: (usize, usize), delta: (i32, i32)) -> bool {
    let mut c = x;
    for letter in ['M', 'A', 'S'] {
        if let Some(c_delta) = apply_delta(c, delta, puzzle.size()) {
            c = c_delta;
            if puzzle.get(c_delta.0, c_delta.1) != Some(&letter) {
                return false;
            }
        } else {
            return false;
        }
    }
    true
}

const DIRECTIONS: [(i32, i32); 8] = [
    (0, 1),
    (1, 1),
    (1, 0),
    (1, -1),
    (0, -1),
    (-1, -1),
    (-1, 0),
    (-1, 1),
];

fn count_xmas(puzzle: &Grid<char>) -> i32 {
    let mut count: i32 = 0;
    for (x_coord, x_letter) in puzzle.indexed_iter() {
        if x_letter == &'X' {
            for d in DIRECTIONS {
                if check_xmas(puzzle, x_coord, d) {
                    count += 1;
                }
            }
        }
    }
    count
}

const DIAGONALS: [(i32, i32); 4] = [(1, 1), (1, -1), (-1, -1), (-1, 1)];

fn check_x_mas(puzzle: &Grid<char>, a: (usize, usize)) -> bool {
    let mut diagonals: Vec<char> = Vec::new();
    for d in DIAGONALS {
        if let Some((c_row, c_col)) = apply_delta(a, d, puzzle.size()) {
            if let Some(c_char) = puzzle.get(c_row, c_col) {
                diagonals.push(*c_char);
            }
        }
    }
    if diagonals.len() < 4 || diagonals[0] == diagonals[2] {
        return false;
    }
    diagonals.sort();
    diagonals == vec!['M', 'M', 'S', 'S']
}

fn count_x_mas(puzzle: &Grid<char>) -> i32 {
    let mut count: i32 = 0;
    for (a_coord, a_letter) in puzzle.indexed_iter() {
        if a_letter == &'A' && check_x_mas(puzzle, a_coord) {
            count += 1;
        }
    }
    count
}

fn main() {
    let puzzle = read_puzzle();
    println!("{}", count_xmas(&puzzle));
    println!("{}", count_x_mas(&puzzle));
}
