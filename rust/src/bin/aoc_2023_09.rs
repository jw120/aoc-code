// Advent of Code, 2023 day 09

use aoc_rust::stdin_lines;

fn parse_sequence(s: &str) -> Vec<i64> {
    s.split_ascii_whitespace()
        .map(|n| n.parse().unwrap())
        .collect()
}

fn extrapolate(sequence: &[i64]) -> i64 {
    if sequence.iter().all(|x| *x == 0) {
        0
    } else {
        let sub_sequence: Vec<i64> = sequence.windows(2).map(|w| w[1] - w[0]).collect();
        println!("{:?} -> {:?}", sequence, sub_sequence);
        sequence.last().unwrap() + extrapolate(&sub_sequence)
    }
}

fn main() {
    let sequences: Vec<Vec<i64>> = stdin_lines().map(|s| parse_sequence(&s)).collect();

    let part_a: i64 = sequences.iter().map(|seq| extrapolate(seq)).sum();
    let part_b: i32 = 0;

    println!("{}", part_a);
    println!("{}", part_b);
}
