// Advent of Code, 2023 day 13

use aoc_rust::stdin_lines;
use grid::Grid;

fn read_pattern(lines: &[String]) -> Grid<bool> {
    let mut pattern: Grid<bool> = Grid::new(0, 0);
    for line in lines {
        let row: Vec<bool> = line
            .chars()
            .map(|ch| {
                assert!(ch == '#' || ch == '.');
                ch == '#'
            })
            .collect();
        pattern.push_row(row);
    }
    pattern
}

// Return number of columns to left of vertical mirror if found
fn has_col_mirror(pattern: &Grid<bool>, errors: usize) -> Option<usize> {
    let rows: usize = pattern.rows();
    let cols: usize = pattern.cols();
    for col in 0..(cols - 1) {
        let mut left: usize = col;
        let mut right: usize = col + 1;
        let mut error_count: usize = 0;
        loop {
            error_count += (0..rows)
                .filter(|row| pattern[(*row, left)] != pattern[(*row, right)])
                .count();
            if error_count > errors {
                break;
            }
            if left > 0 && right < cols - 1 {
                left -= 1;
                right += 1;
            } else if error_count == errors {
                return Some(col + 1);
            } else {
                break;
            }
        }
    }
    None
}

// Return 100x number of rows above a horizontal mirror if found
fn has_row_mirror(pattern: &Grid<bool>, errors: usize) -> Option<usize> {
    let rows: usize = pattern.rows();
    let cols: usize = pattern.cols();
    for row in 0..(rows - 1) {
        let mut top: usize = row;
        let mut bottom: usize = row + 1;
        let mut error_count: usize = 0;
        loop {
            error_count += (0..cols)
                .filter(|col| pattern[(top, *col)] != pattern[(bottom, *col)])
                .count();
            if error_count > errors {
                break;
            }
            if top > 0 && bottom < rows - 1 {
                top -= 1;
                bottom += 1;
            } else if error_count == errors {
                return Some(100 * (row + 1));
            } else {
                break;
            }
        }
    }
    None
}

// Find a column or row mirror with given number of errors
fn mirror_value(pattern: &Grid<bool>, errors: usize) -> usize {
    has_col_mirror(pattern, errors)
        .or(has_row_mirror(pattern, errors))
        .unwrap()
}

fn main() {
    let lines: Vec<String> = stdin_lines().collect();
    let patterns: Vec<Grid<bool>> = lines
        .split(std::string::String::is_empty)
        .map(read_pattern)
        .collect();

    let part_a: usize = patterns
        .iter()
        .map(|pattern| mirror_value(pattern, 0))
        .sum();
    let part_b: usize = patterns
        .iter()
        .map(|pattern| mirror_value(pattern, 1))
        .sum();

    println!("{part_a}");
    println!("{part_b}");
}
