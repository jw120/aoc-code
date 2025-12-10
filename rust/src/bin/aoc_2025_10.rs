// Advent of Code, 2025 day XX

use std::io;

#[derive(Debug)]
struct Problem {
    lights: Vec<bool>,
    buttons: Vec<Vec<usize>>,
    joltages: Vec<u32>,
}

fn parse_line(s: &str) -> Problem {
    let words: Vec<&str> = s.split_ascii_whitespace().collect();

    let w: Vec<char> = words[0].chars().collect();
    assert!(w[0] == '[');
    assert!(w[w.len() - 1] == ']');
    let lights: Vec<bool> = w[1..w.len() - 1].iter().map(|c| *c == '#').collect();

    let mut buttons: Vec<Vec<usize>> = Vec::new();
    for t in words[1..words.len() - 1].iter().map(ToString::to_string) {
        let button: Vec<usize> = t
            .strip_prefix('(')
            .unwrap()
            .strip_suffix(')')
            .unwrap()
            .split(',')
            .map(|s| s.parse().unwrap())
            .collect();
        assert!(button.iter().all(|i| *i < lights.len()));
        buttons.push(button);
    }

    let joltages: Vec<u32> = words[words.len() - 1]
        .to_string()
        .strip_prefix('{')
        .unwrap()
        .strip_suffix('}')
        .unwrap()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect();

    Problem {
        lights,
        buttons,
        joltages,
    }
}

fn part_a(problems: &[Problem]) -> u32 {
    todo!();
}

fn main() {
    let problems: Vec<Problem> = io::stdin()
        .lines()
        .map(|s| parse_line(&s.unwrap()))
        .collect();

    println!("{}", part_a(&problems));
}
