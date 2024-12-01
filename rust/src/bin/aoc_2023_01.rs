// Advent of Code, 2023 day 01

use aoc_rust::stdin_lines;
use itertools::enumerate;

// Convert string to a list of its digits
fn to_digits(s: &str) -> Vec<u32> {
    s.chars().filter_map(|c| c.to_digit(10)).collect()
}

const WORD_DIGITS: [&str; 9] = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

// Convert string to a list of its digits, allowing word forms
fn to_digits_words(s: &str) -> Vec<u32> {
    let mut digits: Vec<u32> = Vec::new();
    for (i, c) in s.char_indices() {
        if let Some(d) = c.to_digit(10) {
            digits.push(d);
        } else {
            let suffix = s.get(i..).unwrap();
            for (digit_i, digit_s) in enumerate(WORD_DIGITS) {
                if suffix.starts_with(digit_s) {
                    digits.push((digit_i + 1).try_into().unwrap());
                    break;
                }
            }
        }
    }
    digits
}

fn end_digits(xs: &[u32]) -> u32 {
    xs[0] * 10 + xs[xs.len() - 1]
}

fn main() {
    let lines: Vec<String> = stdin_lines().collect();

    let part_a: u32 = lines
        .iter()
        .map(|s| to_digits(s))
        .map(|xs| end_digits(&xs))
        .sum();

    let part_b: u32 = lines
        .iter()
        .map(|s| to_digits_words(s))
        .map(|xs| end_digits(&xs))
        .sum();

    println!("{part_a}");
    println!("{part_b}");
}
