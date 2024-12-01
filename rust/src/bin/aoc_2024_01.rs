// Advent of Code, 2024 day 01

use std::collections::HashMap;
use std::io;
use std::iter::zip;

fn parse(r: Result<String, io::Error>) -> (i32, i32) {
    let s: String = r.unwrap();
    let mut iter = s.split_ascii_whitespace();
    let x: i32 = iter.next().unwrap().parse().unwrap();
    let y: i32 = iter.next().unwrap().parse().unwrap();
    assert_eq!(None, iter.next());
    (x, y)
}

fn total_distance(pairs: &[(i32, i32)]) -> i32 {
    let mut xs: Vec<i32> = pairs.iter().map(|(x, _)| *x).collect();
    let mut ys: Vec<i32> = pairs.iter().map(|(_, y)| *y).collect();
    xs.sort_unstable();
    ys.sort_unstable();
    zip(xs, ys).map(|(x, y)| (x - y).abs()).sum()
}

fn similarity(pairs: &[(i32, i32)]) -> i32 {
    let mut y_counts: HashMap<i32, i32> = HashMap::new();
    for (_, y) in pairs {
        y_counts.entry(*y).and_modify(|n| *n += 1).or_insert(1);
    }
    pairs
        .iter()
        .map(|(x, _y)| x * y_counts.get(x).unwrap_or(&0))
        .sum()
}

fn main() {
    let pairs: Vec<(i32, i32)> = io::stdin().lines().map(parse).collect();

    println!("{}", total_distance(&pairs));
    println!("{}", similarity(&pairs));
}
