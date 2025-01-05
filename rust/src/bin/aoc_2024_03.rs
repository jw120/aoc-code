// Advent of Code 2024 - Day 3.

use regex::Regex;
use std::io;

fn sum_uncorrupted(data: &str) -> i32 {
    Regex::new(r"mul\((\d+),(\d+)\)")
        .unwrap()
        .captures_iter(data)
        .map(|capture| capture[1].parse::<i32>().unwrap() * capture[2].parse::<i32>().unwrap())
        .sum()
}

fn sum_enabled(data: &str) -> i32 {
    let r = Regex::new(r"(do\(\))|(don't\(\))|mul\((\d+),(\d+)\)").unwrap();

    let mut enabled: bool = true;
    let mut total: i32 = 0;

    for capture in r.captures_iter(data) {
        if &capture[0] == "do()" {
            enabled = true;
        } else if &capture[0] == "don't()" {
            enabled = false;
        } else if enabled {
            total += capture[3].parse::<i32>().unwrap() * capture[4].parse::<i32>().unwrap();
        }
    }
    total
}

fn main() {
    let data: String = io::read_to_string(io::stdin()).unwrap();

    println!("{}", sum_uncorrupted(&data));
    println!("{}", sum_enabled(&data));
}
