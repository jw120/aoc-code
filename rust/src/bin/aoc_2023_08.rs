// Advent of Code, 2023 day XX

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

fn run(path: &[Step], routes: &HashMap<String, (String, String)>) -> usize {
    let mut count: usize = 0;
    let mut location: &str = "AAA";
    while location != "ZZZ" {
        println!("{}", location);
        location = match path[count % path.len()] {
            Step::Left => &routes[location].0,
            Step::Right => &routes[location].1,
        };
        count += 1;
    }
    count
}

fn run_multi(path: &[Step], routes: &HashMap<String, (String, String)>) -> usize {
    let mut count: usize = 0;
    let mut locations: Vec<&str> = routes
        .keys()
        .filter(|s| s.ends_with('A'))
        .map(|s| s.as_str())
        .collect();
    let mut finished: bool = false;
    while !finished {
        finished = true;
        for i in 0..locations.len() {
            let location = locations[i];
            let new_location = match path[count % path.len()] {
                Step::Left => &routes[location].0,
                Step::Right => &routes[location].1,
            };
            locations[i] = new_location;
            if !new_location.ends_with('Z') {
                finished = false;
            }
        }
        count += 1;
    }
    count
}

fn main() {
    let (path, routes) = parse();
    println!("{:?} {:?}", path, routes);

    //    println!("{}", run(&path, &routes));
    println!("{}", run_multi(&path, &routes));
}
