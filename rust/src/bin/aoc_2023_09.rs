// Advent of Code, 2023 day 09

use aoc_rust::stdin_lines;

fn parse_sequence(s: &str) -> Vec<i64> {
    s.split_ascii_whitespace()
        .map(|n| n.parse().unwrap())
        .collect()
}

fn extrapolate(sequence: &[i64], front: bool) -> i64 {
    if sequence.iter().all(|x| *x == 0) {
        0
    } else {
        let sub_sequence: Vec<i64> = sequence.windows(2).map(|w| w[1] - w[0]).collect();
        let sub_value = extrapolate(&sub_sequence, front);
        if front {
            sequence.first().unwrap() - sub_value
        } else {
            sequence.last().unwrap() + sub_value
        }
    }
}

fn main() {
    let sequences: Vec<Vec<i64>> = stdin_lines().map(|s| parse_sequence(&s)).collect();

    let part_a: i64 = sequences.iter().map(|seq| extrapolate(seq, false)).sum();
    let part_b: i64 = sequences.iter().map(|seq| extrapolate(seq, true)).sum();

    println!("{part_a}");
    println!("{part_b}");
}
