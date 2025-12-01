// Advent of Code, 2025 day 01

use std::io;

const START_POSITION: i32 = 50;
const DIAL_SIZE: i32 = 100;

fn parse_move(line: &str) -> i32 {
    let num: i32 = line[1..].parse().unwrap();
    match &line[..1] {
        "L" => -num,
        "R" => num,
        _ => panic!("Bad move line {line}"),
    }
}

fn run(moves: &[i32]) -> (i32, i32) {
    let mut position: i32 = START_POSITION;
    let mut zero_finishes: i32 = 0;
    let mut zero_passes: i32 = 0;
    for mv in moves {
        zero_passes += (mv.abs()) / DIAL_SIZE;
        let m: i32 = (mv.abs() % DIAL_SIZE) * mv.signum();
        position += m;
        if position % DIAL_SIZE == 0 {
            zero_finishes += 1;
            position = 0;
        } else if position > DIAL_SIZE {
            zero_passes += 1;
            position -= DIAL_SIZE;
        } else if position < 0 {
            zero_passes += i32::from(position != m);
            position += DIAL_SIZE;
        }
    }
    (zero_finishes, zero_passes + zero_finishes)
}

fn main() {
    let moves: Vec<i32> = io::stdin()
        .lines()
        .map(|s| parse_move(&s.unwrap()))
        .collect();

    let (part_a, part_b) = run(&moves);
    println!("{part_a}");
    println!("{part_b}");
}
