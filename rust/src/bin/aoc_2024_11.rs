// Advent of Code 2024 - Day 11.

use std::collections::HashMap;
use std::io;

fn from_digits(ds: &[u32]) -> u64 {
    let mut acc: u64 = 0;
    let mut place: u64 = 1;
    for d in ds.iter().rev() {
        acc += u64::from(*d) * place;
        place *= 10;
    }
    // println!("{:?} -> {}", ds, acc);
    acc
}

fn to_digits(x: u64) -> Vec<u32> {
    x.to_string()
        .chars()
        .map(|s| s.to_digit(10).unwrap())
        .collect()
}

fn blink(x: u64) -> Vec<u64> {
    if x == 0 {
        return vec![1];
    }
    let digits: Vec<u32> = to_digits(x);
    // println!("{} {:?}", x, digits);
    let n: usize = digits.len();
    if n % 2 == 0 {
        // println!("{} {:?} {:?}", n / 2, &digits[..n / 2], &digits[n / 2..]);
        vec![from_digits(&digits[..n / 2]), from_digits(&digits[n / 2..])]
    } else {
        vec![x * 2024]
    }
}

fn blink_line(counts: &HashMap<u64, usize>) -> HashMap<u64, usize> {
    let mut new_counts: HashMap<u64, usize> = HashMap::new();
    for (x, n) in counts {
        for y in blink(*x) {
            *new_counts.entry(y).or_default() += n;
        }
    }
    new_counts
}

fn main() {
    let mut counts: HashMap<u64, usize> = HashMap::new();
    for s in io::read_to_string(io::stdin())
        .unwrap()
        .trim()
        .split_ascii_whitespace()
    {
        let x: u64 = s.parse().unwrap();
        *counts.entry(x).or_default() += 1;
    }
    for i in 0..76 {
        if i == 25 || i == 75 {
            println!("{}", counts.values().sum::<usize>());
        }
        counts = blink_line(&counts);
    }
}
