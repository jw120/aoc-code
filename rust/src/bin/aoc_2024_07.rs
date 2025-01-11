// Advent of Code 2024 - Day 7

use std::io;

struct Calibration {
    target: u64,
    numbers: Vec<u64>,
}

fn parse(s: &str) -> Calibration {
    let mut i = s.split(": ");
    let target: u64 = i.next().unwrap().parse().unwrap();
    let numbers: Vec<u64> = i
        .next()
        .unwrap()
        .split_ascii_whitespace()
        .map(|t| t.parse().unwrap())
        .collect();
    assert!(i.next().is_none());
    Calibration { target, numbers }
}

fn check(calibration: &Calibration, value: u64, index: usize, allow_combine: bool) -> bool {
    if index == calibration.numbers.len() {
        return value == calibration.target;
    }
    if value > calibration.target {
        return false;
    }
    check(
        calibration,
        value + calibration.numbers[index],
        index + 1,
        allow_combine,
    ) || check(
        calibration,
        value * calibration.numbers[index],
        index + 1,
        allow_combine,
    ) || (allow_combine
        && check(
            calibration,
            combine(value, calibration.numbers[index]),
            index + 1,
            allow_combine,
        ))
}

fn combine(a: u64, b: u64) -> u64 {
    (a.to_string() + &b.to_string()).parse().unwrap()
}

fn main() {
    let calibrations: Vec<Calibration> = io::stdin().lines().map(|s| parse(&s.unwrap())).collect();
    let part_a: u64 = calibrations
        .iter()
        .filter(|c| check(c, c.numbers[0], 1, false))
        .map(|c| c.target)
        .sum::<u64>();
    let part_b: u64 = calibrations
        .iter()
        .filter(|c| check(c, c.numbers[0], 1, true))
        .map(|c| c.target)
        .sum();
    print!("{part_a}\n{part_b}\n");
}
