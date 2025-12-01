// Advent of Code, 2023 day XX

use std::io;

const START_POSITION: i32 = 50;
const DIAL_SIZE: i32 = 100;

#[derive(Debug)]
enum Move {
    Left(i32),
    Right(i32),
}

impl Move {
    fn new(line: &str) -> Self {
        let num: i32 = line[1..].parse().unwrap();
        match &line[..1] {
            "L" => Self::Left(num),
            "R" => Self::Right(num),
            _ => panic!("Bad move line {line}"),
        }
    }

    fn apply(&self, position: i32) -> i32 {
        match self {
            Self::Left(n) => (position - n) % DIAL_SIZE,
            Self::Right(n) => (position + n) % DIAL_SIZE,
        }
    }
}

fn part_a(moves: &[Move]) -> usize {
    let mut position: i32 = START_POSITION;
    let mut zero_count: usize = 0;
    for m in moves {
        position = m.apply(position);
        if position == 0 {
            zero_count += 1;
        }
        println!("{m:?} {position} {zero_count}");
    }
    zero_count
}

fn main() {
    let moves: Vec<Move> = io::stdin()
        .lines()
        .map(|s| Move::new(&s.unwrap()))
        .collect();

    println!("{}", part_a(&moves));
}
