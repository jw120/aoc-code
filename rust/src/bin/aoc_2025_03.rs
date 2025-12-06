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

fn part_b(banks: &[Vec<u32>], num_digits: usize) -> u64 {
    let mut total: u64 = 0;
    for bank in banks {
        let mut n: usize = num_digits; // number of digits left to find
        let mut b: &[u32] = bank; // remaining digits to search
        let mut result: u64 = 0; // accumulated digits taken
        while n > 0 {
            // If we have to take n digits, then we need at least one in
            // the first N-(n-1) digits.
            let (i, x) = first_max(&b[..=(b.len() - n)]).unwrap();
            result = 10 * result + u64::from(x);
            b = &b[i + 1..];
            n -= 1;
        }
        total += result;
    }
    total
}

fn main() {
    let banks: Vec<Vec<u32>> = io::stdin()
        .lines()
        .map(|s| parse_bank(&s.unwrap()))
        .collect();

    println!("{}", part_a(&banks));
    println!("{}", part_b(&banks, 12));
}
