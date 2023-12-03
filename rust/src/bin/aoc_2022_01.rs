// Advent of Code, 2022 day 1

use aoc_rust::stdin_lines;

fn to_sums(xs: impl Iterator<Item = Option<i32>>) -> Vec<i32> {
    let mut sums = Vec::new();
    let mut current_sum = 0;
    let mut current_sum_open = false;
    for x in xs {
        match x {
            Some(value) => {
                current_sum += value;
                current_sum_open = true;
            }
            None => {
                sums.push(current_sum);
                current_sum = 0;
                current_sum_open = false;
            }
        }
    }
    if current_sum_open {
        sums.push(current_sum);
    }
    sums
}

fn read_line(s: &str) -> Option<i32> {
    let s_trimmed = s.trim();
    if s_trimmed.is_empty() {
        None
    } else {
        Some(s_trimmed.parse().unwrap())
    }
}

fn main() {
    let numbers = stdin_lines().map(|s| read_line(&s));

    let mut sums: Vec<i32> = to_sums(numbers);

    sums.sort();
    sums.reverse();

    println!("{}", sums[0]);
    println!("{}", sums[0] + sums[1] + sums[2]);
}
