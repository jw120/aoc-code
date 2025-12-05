// Advent of Code, 2025 day 05

use std::cmp;
use std::io;

fn read_data() -> (Vec<(u64, u64)>, Vec<u64>) {
    let mut ranges: Vec<(u64, u64)> = Vec::new();
    let mut ingredients: Vec<u64> = Vec::new();
    let mut in_ranges: bool = true;
    for line in io::stdin().lines() {
        let line = line.unwrap();
        let line = line.trim();
        if in_ranges {
            if line.is_empty() {
                in_ranges = false;
                continue;
            }
            let (a, b) = line.split_once('-').unwrap();
            ranges.push((a.parse().unwrap(), b.parse().unwrap()));
        } else {
            ingredients.push(line.parse().unwrap());
        }
    }
    (ranges, ingredients)
}

fn part_a(ranges: &[(u64, u64)], ingredients: &[u64]) -> u64 {
    let mut count: u64 = 0;
    // simple brute force
    for i in ingredients {
        if ranges.iter().any(|(a, b)| a <= i && i <= b) {
            count += 1;
        }
    }
    count
}

fn part_b(ranges: &[(u64, u64)]) -> u64 {
    // sort ranges by lower bound
    let mut sorted_ranges: Vec<(u64, u64)> = Vec::from(ranges);
    sorted_ranges.sort_unstable_by(|(a, _), (c, _)| a.cmp(c));

    // merge ranges so no overlaps
    let mut merged_ranges: Vec<(u64, u64)> = Vec::new();
    let (mut current_a, mut current_b) = sorted_ranges[0];
    for (r_a, r_b) in sorted_ranges.iter().skip(1) {
        // if new range does not overlap, add the current one and start on new range
        if *r_a > current_b {
            merged_ranges.push((current_a, current_b));
            (current_a, current_b) = (*r_a, *r_b);
        } else {
            // if new range overlaps, merge it in
            current_b = cmp::max(current_b, *r_b);
        }
    }
    merged_ranges.push((current_a, current_b));

    // count the contents of each range
    merged_ranges.iter().map(|(a, b)| b - a + 1).sum()
}

fn main() {
    let (ranges, ingredients) = read_data();

    println!("{}", part_a(&ranges, &ingredients));
    println!("{}", part_b(&ranges));
}
