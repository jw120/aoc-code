// Advent of Code, 2023 day 06

use std::io::stdin;
use std::iter::zip;

fn read_row(prefix: &str) -> String {
    stdin()
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .strip_prefix(prefix)
        .unwrap()
        .trim()
        .to_string()
}

fn split_row(s: &str) -> Vec<isize> {
    s.split_whitespace().map(|s| s.parse().unwrap()).collect()
}

fn parse_digits(s: &str) -> isize {
    let digits: String = s.chars().filter(|c| c.is_digit(10)).collect();
    digits.parse().unwrap()
}

fn ways_to_win(time: isize, distance: isize) -> isize {
    let mut count = 0;
    for t in 1..time {
        if t * (time - t) > distance {
            count += 1;
        }
    }
    count
}

fn main() {
    let times_str = read_row("Time:");
    let distances_str = read_row("Distance:");

    // part (a)
    let times: Vec<isize> = split_row(&times_str);
    let distances: Vec<isize> = split_row(&distances_str);
    let ways: isize = zip(times.iter(), distances.iter())
        .map(|(t, d)| ways_to_win(*t, *d))
        .product();
    println!("{}", ways);

    // part (b)
    let time = parse_digits(&times_str);
    let distance = parse_digits(&distances_str);
    println!("{}", ways_to_win(time, distance));
}
