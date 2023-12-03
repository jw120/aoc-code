// Advent of Code, 2022 day 9

use aoc_rust::stdin_lines;
use aoc_rust::Coord;
use std::collections::HashSet;

enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    fn to_coord(&self) -> Coord {
        match self {
            Direction::Up => Coord { row: 0, col: -1 },
            Direction::Right => Coord { row: 1, col: 0 },
            Direction::Down => Coord { row: 0, col: 1 },
            Direction::Left => Coord { row: -1, col: 0 },
        }
    }
}

struct Move {
    direction: Direction,
    distance: u32,
}

// return new tail position
fn update(head: Coord, tail: Coord) -> Coord {
    let diff = tail - head;
    tail - match diff.mag2() {
        0..=2 => Coord::origin(), // If touching distance, no move needed
        4 => diff.div(2),         // If two steps left/right/up/down, then move one step
        5 | 8 => diff.signum(),
        _ => panic!("unexpected difference"),
    }
}

// return number of coords visited during given moves with two knots
fn walk2(moves: &[Move]) -> usize {
    let mut visited = HashSet::new();
    let mut head = Coord::origin();
    let mut tail = Coord::origin();

    for m in moves {
        for _ in 0..m.distance {
            head += m.direction.to_coord();
            tail = update(head, tail);
            visited.insert(tail);
        }
    }

    visited.len()
}

// return number of coords visited during given moves with 10 knots
fn walk10(moves: &[Move]) -> usize {
    let mut visited = HashSet::new();
    let mut knots = [Coord::origin(); 10];

    for m in moves {
        for _ in 0..m.distance {
            knots[0] += m.direction.to_coord();
            for i in 1..10 {
                knots[i] = update(knots[i - 1], knots[i]);
            }
            visited.insert(knots[9]);
        }
    }

    visited.len()
}

fn parse_line(line: &str) -> Move {
    let mut words_iter = line.split_whitespace();
    let direction = match words_iter.next().unwrap() {
        "U" => Direction::Up,
        "R" => Direction::Right,
        "D" => Direction::Down,
        "L" => Direction::Left,
        &_ => panic!("Unrecognised direction"),
    };
    let distance = words_iter.next().unwrap().parse().unwrap();
    assert!(words_iter.next().is_none());

    Move {
        direction,
        distance,
    }
}

fn main() {
    let moves: Vec<Move> = stdin_lines().map(|s| parse_line(&s)).collect();

    println!("{}", walk2(&moves));
    println!("{}", walk10(&moves));
}
