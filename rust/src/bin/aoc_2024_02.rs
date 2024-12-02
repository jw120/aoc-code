// Advent of Code, 2024 day 02

use std::io;

fn parse(line: &str) -> Vec<i32> {
    line.split_ascii_whitespace()
        .map(|s| s.parse().unwrap())
        .collect()
}

fn is_safe(xs: &[i32]) -> bool {
    let first_increasing: bool = xs[1] > xs[0];
    for pair in xs.windows(2) {
        let diff = (pair[1] - pair[0]).abs();
        let increasing = pair[1] > pair[0];
        if increasing != first_increasing || !(1..=3).contains(&diff) {
            return false;
        }
    }
    true
}

fn is_safe_one(xs: &[i32]) -> bool {
    if is_safe(xs) {
        return true;
    }
    for i in 0..xs.len() {
        let mut ys: Vec<i32> = Vec::new();
        ys.extend_from_slice(&xs[..i]);
        ys.extend_from_slice(&xs[(i + 1)..]);
        if is_safe(&ys) {
            return true;
        }
    }
    false
}

fn main() {
    let lines: Vec<Vec<i32>> = io::stdin()
        .lines()
        .map(|line| parse(&line.unwrap()))
        .collect();

    println!("{}", lines.iter().filter(|line| is_safe(line)).count());
    println!("{}", lines.iter().filter(|line| is_safe_one(line)).count());
}
