// Advent of Code, 2025 day 6

use std::io;

fn read_input() -> (Vec<Vec<u64>>, Vec<bool>) {
    let mut numbers: Vec<Vec<u64>> = Vec::new();
    for line in io::stdin().lines() {
        let line = line.unwrap();
        if line.starts_with(['+', '*']) {
            let ops: Vec<bool> = line.split_whitespace().map(|s| s == "*").collect();
            return (numbers, ops);
        }
        numbers.push(
            line.split_whitespace()
                .map(|s| s.parse().unwrap())
                .collect(),
        );
    }
    panic!("Ops not found");
}

fn part_a(numbers: &[Vec<u64>], ops: &[bool]) -> u64 {
    let mut grand_total: u64 = 0;
    for (i, op) in ops.iter().enumerate() {
        let column = numbers.iter().map(|v| v[i]);
        let problem: u64 = if *op { column.product() } else { column.sum() };
        grand_total += problem;
    }
    grand_total
}

fn main() {
    let (numbers, ops) = read_input();

    println!("{}", part_a(&numbers, &ops));
}
