// Advent of Code, 2023 day XX

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

fn run(moves: &[i32]) -> (usize, usize) {
    let mut position: i32 = START_POSITION;
    let mut zero_finishes: usize = 0;
    let mut zero_passes: usize = 0;
    for mm in moves {
        let mut m: i32 = *mm;
        let old_position = position;
        while m != 0 {
            if m >= DIAL_SIZE {
                m -= DIAL_SIZE;
                zero_passes += 1;
            } else if m >= 0 {
                position += m;
                if position == DIAL_SIZE {
                    zero_finishes += 1;
                    position = 0;
                }
                if position > DIAL_SIZE {
                    position -= DIAL_SIZE;
                    zero_passes += 1;
                }
                m = 0;
            } else if m <= -DIAL_SIZE {
                m += DIAL_SIZE;
                zero_passes += 1;
            } else {
                position += m;
                if position == 0 {
                    zero_finishes += 1;
                    position = 0;
                }
                if position < 0 {
                    position += DIAL_SIZE;
                    zero_passes += usize::from(old_position != 0);
                }
                m = 0;
            }
        }
        assert!(position >= 0);
        assert!(position < DIAL_SIZE);
        println!("{mm} {position} {zero_finishes} {zero_passes}");
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
