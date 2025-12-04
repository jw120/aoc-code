// Advent of Code, 2025 day XX

use std::io;

fn main() {
    let moves: Vec<i32> = io::stdin()
        .lines()
        .map(|s| parse_move(&s.unwrap()))
        .collect();

    println!("{}", part_a(moves));
}
