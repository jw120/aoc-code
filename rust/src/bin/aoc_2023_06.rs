// Advent of Code, 2023 day 06

use std::io::stdin;
use std::iter::zip;

fn read_row(prefix: &str) -> Vec<isize> {
    stdin()
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .strip_prefix(prefix)
        .unwrap()
        .trim()
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect()
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
    let times: Vec<isize> = read_row("Time:");
    let distances: Vec<isize> = read_row("Distance:");

    let ways: Vec<isize> = zip(times.iter(), distances.iter())
        .map(|(t, d)| ways_to_win(*t, *d))
        .collect();

    println!("{:?}", times);
    println!("{:?}", distances);
    println!("{:?}", ways);
    println!("{}", ways.iter().product::<isize>());
}
