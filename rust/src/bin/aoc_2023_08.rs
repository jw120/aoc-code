// Advent of Code, 2023 day XX

use std::cmp::{max, min};
use std::collections::HashMap;
use std::io::stdin;

#[derive(Clone, Copy, Debug)]
enum Step {
    Left,
    Right,
}

impl Step {
    fn parse(ch: char) -> Step {
        match ch {
            'L' => Step::Left,
            'R' => Step::Right,
            _ => panic!("Bad step '{}'", ch),
        }
    }
}

fn parse() -> (Vec<Step>, HashMap<String, (String, String)>) {
    let mut lines_iter = stdin().lines();

    let path_str = lines_iter.next().unwrap().unwrap();
    let path = path_str.chars().map(Step::parse).collect();

    assert_eq!(lines_iter.next().unwrap().unwrap(), "");

    let mut routes: HashMap<String, (String, String)> = HashMap::new();
    for line in lines_iter {
        let line = line.unwrap();
        assert_eq!(line.len(), 16);
        let from = line[0..=2].to_string();
        let left = line[7..=9].to_string();
        let right = line[12..=14].to_string();
        routes.insert(from, (left, right));
    }
    (path, routes)
}

// Simply run to completion
fn run(path: &[Step], routes: &HashMap<String, (String, String)>) -> usize {
    let mut count: usize = 0;
    let mut location: &str = "AAA";
    while location != "ZZZ" {
        location = match path[count % path.len()] {
            Step::Left => &routes[location].0,
            Step::Right => &routes[location].1,
        };
        count += 1;
    }
    count
}

// Run multiple paths in parallel. Implement by running each in series and
// finding the repeating period.
fn run_multi(path: &[Step], routes: &HashMap<String, (String, String)>) -> usize {
    let path_length = path.len();
    let start_locations: Vec<&str> = routes
        .keys()
        .filter(|s| s.ends_with('A'))
        .map(|s| s.as_str())
        .collect();
    let mut periods: Vec<usize> = Vec::new();
    for location in start_locations {
        let mut count: usize = 0;
        let mut current_location = location;
        let mut visited: HashMap<(&str, usize), usize> = HashMap::new();
        loop {
            let path_index = count % path_length;
            if current_location.ends_with('Z') {
                let current_location_index = (current_location, path_index);
                if let Some(prev_count) = visited.get(&current_location_index) {
                    let period = count - prev_count;
                    periods.push(period);
                    break;
                } else {
                    visited.insert(current_location_index, count);
                }
            }
            current_location = match path[path_index] {
                Step::Left => &routes[current_location].0,
                Step::Right => &routes[current_location].1,
            };
            count += 1;
        }
    }
    lcm_multi(&periods)
}

fn lcm_multi(xs: &[usize]) -> usize {
    let mut x = xs[0];
    for y in &xs[1..] {
        x = lcm(x, *y)
    }
    x
}

fn lcm(a: usize, b: usize) -> usize {
    if a == 0 && b == 0 {
        0
    } else {
        a * (b / gcd(a, b))
    }
}

fn gcd(a: usize, b: usize) -> usize {
    let mut x = max(a, b);
    let mut y = min(a, b);
    while y > 0 {
        (x, y) = (y, x % y)
    }
    x
}

fn main() {
    let (path, routes) = parse();

    println!("{}", run(&path, &routes));
    println!("{}", run_multi(&path, &routes));
}
