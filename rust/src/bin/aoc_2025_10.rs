// Advent of Code, 2025 day XX

use std::collections::{HashSet, VecDeque};
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

// BFS for the target
fn part_a(problems: &[Problem]) -> u32 {
    let mut total: u32 = 0;
    'outer: for problem in problems {
        let target = &problem.lights;
        let mut visited: HashSet<Vec<bool>> = HashSet::new();
        let mut queue: VecDeque<(u32, Vec<bool>)> = VecDeque::new();
        queue.push_back((0, vec![false; target.len()]));
        while let Some((n, v)) = queue.pop_front() {
            if &v == target {
                total += n;
                continue 'outer;
            }
            for button in &problem.buttons {
                let mut next = v.clone();
                for i in button {
                    next[*i] = !next[*i];
                }
                if !visited.contains(&next) {
                    visited.insert(next.clone());
                    queue.push_back((n + 1, next));
                }
            }
        }
    }
    total
}

fn part_b(problems: &[Problem]) -> u32 {
    let mut total: u32 = 0;
    'outer: for problem in problems {
        println!("{problem:?}");
        let target = &problem.joltages;
        let mut visited: HashSet<Vec<u32>> = HashSet::new();
        let mut queue: VecDeque<(u32, Vec<u32>)> = VecDeque::new();
        queue.push_back((0, vec![0; target.len()]));
        while let Some((n, v)) = queue.pop_front() {
            // println!("({n},{v:?}) q={} v={}", queue.len(), visited.len());
            if &v == target {
                total += n;
                continue 'outer;
            }
            for button in &problem.buttons {
                let mut next = v.clone();
                for i in button {
                    next[*i] += 1;
                }
                if next.iter().zip(target).all(|(i, t)| i <= t) && !visited.contains(&next) {
                    visited.insert(next.clone());
                    queue.push_back((n + 1, next));
                }
            }
        }
    }
    total
}

fn main() {
    let problems: Vec<Problem> = io::stdin()
        .lines()
        .map(|s| parse_line(&s.unwrap()))
        .collect();

    println!("{}", part_a(&problems));
    println!("{}", part_b(&problems));
}
