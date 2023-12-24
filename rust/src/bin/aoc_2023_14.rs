// Advent of Code, 2023 day 14

use aoc_rust::stdin_lines;
use grid::Grid;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Copy, Clone, Debug, Default, Hash, PartialEq)]
enum Square {
    Round,
    Cube,
    #[default]
    Empty,
}

fn read_pattern() -> Grid<Square> {
    let mut pattern: Grid<Square> = Grid::new(0, 0);
    for line in stdin_lines() {
        let row: Vec<Square> = line
            .chars()
            .map(|ch| match ch {
                '#' => Square::Cube,
                'O' => Square::Round,
                '.' => Square::Empty,
                _ => panic!("Unknown character '{}'", ch),
            })
            .collect();
        pattern.push_row(row);
    }
    pattern
}

fn _show_pattern(pattern: &Grid<Square>) {
    for row in pattern.iter_rows() {
        for x in row {
            print!(
                "{}",
                match x {
                    Square::Cube => '#',
                    Square::Round => 'O',
                    Square::Empty => '.',
                }
            );
        }
        println!();
    }
    println!();
}

fn tilt_north(p: &mut Grid<Square>) {
    for row in 1..p.rows() {
        for col in 0..p.cols() {
            if p[(row, col)] == Square::Round {
                let mut target: usize = row;
                while target > 0 && p[(target - 1, col)] == Square::Empty {
                    target -= 1;
                }
                if target != row {
                    p[(row, col)] = Square::Empty;
                    p[(target, col)] = Square::Round;
                }
            }
        }
    }
}

fn tilt_south(p: &mut Grid<Square>) {
    for row in (0..(p.rows() - 1)).rev() {
        for col in 0..p.cols() {
            if p[(row, col)] == Square::Round {
                let mut target: usize = row;
                while target < p.rows() - 1 && p[(target + 1, col)] == Square::Empty {
                    target += 1;
                }
                if target != row {
                    p[(row, col)] = Square::Empty;
                    p[(target, col)] = Square::Round;
                }
            }
        }
    }
}

fn tilt_east(p: &mut Grid<Square>) {
    for col in (0..(p.cols() - 1)).rev() {
        for row in 0..p.rows() {
            if p[(row, col)] == Square::Round {
                let mut target: usize = col;
                while target < p.cols() - 1 && p[(row, target + 1)] == Square::Empty {
                    target += 1;
                }
                if target != col {
                    p[(row, col)] = Square::Empty;
                    p[(row, target)] = Square::Round;
                }
            }
        }
    }
}

fn tilt_west(p: &mut Grid<Square>) {
    for col in 1..p.cols() {
        for row in 0..p.rows() {
            if p[(row, col)] == Square::Round {
                let mut target: usize = col;
                while target > 0 && p[(row, target - 1)] == Square::Empty {
                    target -= 1;
                }
                if target != col {
                    p[(row, col)] = Square::Empty;
                    p[(row, target)] = Square::Round;
                }
            }
        }
    }
}

fn tilt_cycle(p: &mut Grid<Square>) {
    tilt_north(p);
    tilt_west(p);
    tilt_south(p);
    tilt_east(p);
}

fn load(pattern: &Grid<Square>) -> usize {
    let mut total: usize = 0;
    for (i, row) in pattern.iter_rows().enumerate() {
        total += row.filter(|x| **x == Square::Round).count() * (pattern.rows() - i);
    }
    total
}

fn main() {
    let mut pattern: Grid<Square> = read_pattern();

    // Part (a)
    let mut pattern_a: Grid<Square> = pattern.clone();
    tilt_north(&mut pattern_a);
    println!("{}", load(&pattern_a));

    // Part (b)
    let cycles: usize = 1000000000;
    let mut hash_memory: HashMap<u64, usize> = HashMap::new();
    let mut load_memory: HashMap<usize, usize> = HashMap::new();
    for i in 0..cycles {
        tilt_cycle(&mut pattern);

        // Keep track of load values for each loop index
        let load_value = load(&pattern);
        load_memory.insert(i, load_value);

        // Keep track of hash values we have seen before (and their loop index)
        let mut hasher = DefaultHasher::new();
        pattern.hash(&mut hasher);
        let hash_value = hasher.finish();

        // If find a loop - extract appropriate load value
        if let Some(previous_i) = hash_memory.insert(hash_value, i) {
            let period = i - previous_i;
            let offset = (cycles - 1 - i) % period;
            let value = load_memory.get(&(i - period + offset)).unwrap();
            println!("{}", value);
            break;
        }
    }
}
