// Advent of Code, 2023 day 14

use aoc_rust::stdin_lines;
use grid::Grid;

// Read pattern of rocks. Some(true) is a cube-shaped (fixed) rock,
// Some(false) is a round (mobile) rock. None is empty space.
fn read_pattern() -> Grid<Option<bool>> {
    let mut pattern: Grid<Option<bool>> = Grid::new(0, 0);
    for line in stdin_lines() {
        let row: Vec<Option<bool>> = line
            .chars()
            .map(|ch| match ch {
                '#' => Some(true),
                'O' => Some(false),
                '.' => None,
                _ => panic!("Unknown character '{}'", ch),
            })
            .collect();
        pattern.push_row(row);
    }
    pattern
}

fn show_pattern(pattern: &Grid<Option<bool>>) {
    for row in pattern.iter_rows() {
        for x in row {
            print!(
                "{}",
                match x {
                    Some(true) => '#',
                    Some(false) => 'O',
                    None => '.',
                }
            );
        }
        println!();
    }
}

fn tilt_north(pattern: &Grid<Option<bool>>) -> Grid<Option<bool>> {
    let mut p = pattern.clone();
    for row in 1..p.rows() {
        for col in 0..p.cols() {
            if p[(row, col)] == Some(false) {
                let mut target: usize = row;
                while target > 0 && p[(target - 1, col)].is_none() {
                    target -= 1;
                }
                if target != row {
                    p[(row, col)] = None;
                    p[(target, col)] = Some(false);
                }
            }
        }
    }
    p
}

fn load(pattern: &Grid<Option<bool>>) -> usize {
    let mut total: usize = 0;
    for (i, row) in pattern.iter_rows().enumerate() {
        total += row.filter(|x| **x == Some(false)).count() * (pattern.rows() - i);
    }
    total
}

fn main() {
    let pattern: Grid<Option<bool>> = read_pattern();

    let pattern_a = tilt_north(&pattern);
    show_pattern(&pattern_a);
    println!("{}", load(&pattern_a));

    // for _ in 0..3 {
    //     let pattern_b = tilt_cycle(&pattern);
    //     show_pattern(&pattern_b);
    // }
}
