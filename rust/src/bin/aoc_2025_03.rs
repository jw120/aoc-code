// Advent of Code, 2025 day 3

use std::io;

fn parse_bank(s: &str) -> Vec<u32> {
    s.trim().chars().map(|c| c.to_digit(10).unwrap()).collect()
}

// Return index and value of the first occurrence of the maximum value in the slice
fn first_max(xs: &[u32]) -> Option<(usize, u32)> {
    let mut best: Option<(usize, u32)> = None;
    for (i, x) in xs.iter().enumerate() {
        if let Some((_, b)) = best
            && *x <= b
        {
            continue;
        }
        best = Some((i, *x));
    }
    best
}

fn part_a(banks: &[Vec<u32>]) -> u32 {
    let mut result: u32 = 0;
    for bank in banks {
        let (i, x) = first_max(&bank[..]).unwrap();
        if i == bank.len() - 1 {
            let (_, y) = first_max(&bank[..i]).unwrap();
            result += y * 10 + x;
        } else {
            let (_, y) = first_max(&bank[i + 1..]).unwrap();
            result += x * 10 + y;
        }
    }
    result
}

fn main() {
    let banks: Vec<Vec<u32>> = io::stdin()
        .lines()
        .map(|s| parse_bank(&s.unwrap()))
        .collect();

    println!("{}", part_a(&banks));
}
