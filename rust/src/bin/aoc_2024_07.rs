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

fn check2(calibration: &Calibration, value: u64, index: usize) -> bool {
    if index == calibration.numbers.len() {
        value == calibration.target
    } else {
        check2(calibration, value + calibration.numbers[index], index + 1)
            || check2(calibration, value * calibration.numbers[index], index + 1)
    }
}

fn test2(calibration: &Calibration) -> u64 {
    if check2(calibration, calibration.numbers[0], 1) {
        calibration.target
    } else {
        0
    }
}

fn check3(calibration: &Calibration, value: u64, index: usize) -> bool {
    if index == calibration.numbers.len() {
        value == calibration.target
    } else {
        check3(calibration, value + calibration.numbers[index], index + 1)
            || check3(calibration, value * calibration.numbers[index], index + 1)
            || check3(
                calibration,
                combine(value, calibration.numbers[index]),
                index + 1,
            )
    }
}

fn combine(a: u64, b: u64) -> u64 {
    (a.to_string() + &b.to_string()).parse().unwrap()
}

fn test3(calibration: &Calibration) -> u64 {
    if check3(calibration, calibration.numbers[0], 1) {
        calibration.target
    } else {
        0
    }
}

fn main() {
    let calibrations: Vec<Calibration> = io::stdin().lines().map(|s| parse(&s.unwrap())).collect();
    print!(
        "{}\n{}\n",
        calibrations.iter().map(test2).sum::<u64>(),
        calibrations.iter().map(test3).sum::<u64>()
    )
}
