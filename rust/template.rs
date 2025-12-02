// Advent of Code, 2025 day XX

use std::io;

fn main() {
    let moves: Vec<i32> = io::stdin()
        .lines()
        .map(|s| parse_move(&s.unwrap()))
        .collect();

    let (part_a, part_b) = run(&moves);
    println!("{part_a}");
    println!("{part_b}");
}
