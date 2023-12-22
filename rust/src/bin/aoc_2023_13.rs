// Advent of Code, 2023 day XX

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
fn has_col_mirror(pattern: &Grid<bool>) -> Option<usize> {
    let rows: usize = pattern.rows();
    let cols: usize = pattern.cols();
    for col in 0..(cols - 1) {
        println!("Testing col {}", col);
        let mut left: usize = col;
        let mut right: usize = col + 1;
        loop {
            if !((0..rows).all(|row| pattern[(row, left)] == pattern[(row, right)])) {
                println!("No match {} {}", left, right);
                break;
            }
            println!("Match {} {}", left, right);
            if left > 0 && right < cols - 1 {
                left -= 1;
                right += 1;
            } else {
                println!("passed col {}", col);
                return Some(col + 1);
            }
        }
    }
    None
}

// Return 100x number of rows above a horizontal mirror if found
fn has_row_mirror(pattern: &Grid<bool>) -> Option<usize> {
    let rows: usize = pattern.rows();
    let cols: usize = pattern.cols();
    for row in 0..(rows - 1) {
        println!("Testing row {}", row);
        let mut top: usize = row;
        let mut bottom: usize = row + 1;
        loop {
            if !((0..cols).all(|col| pattern[(top, col)] == pattern[(bottom, col)])) {
                println!("No match {} {}", top, bottom);
                break;
            }
            println!("Match {} {}", top, bottom);
            if top > 0 && bottom < rows - 1 {
                top -= 1;
                bottom += 1;
            } else {
                println!("passed row {}", row);
                return Some(100 * (row + 1));
            }
        }
    }
    None
}

fn mirror_value(pattern: &Grid<bool>) -> usize {
    if let Some(score) = has_col_mirror(pattern) {
        return score;
    }
    if let Some(score) = has_row_mirror(pattern) {
        score
    } else {
        panic!("No mirror found");
    }
}

fn main() {
    let lines: Vec<String> = stdin_lines().collect();
    let patterns: Vec<Grid<bool>> = lines
        .split(|line| line.is_empty())
        .map(read_pattern)
        .collect();

    let part_a: usize = patterns.iter().map(mirror_value).sum();

    println!("{}", part_a);
}
