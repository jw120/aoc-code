// Advent of Code, 2023 day 19

use aoc_rust::stdin_lines;
use std::collections::HashMap;

#[derive(Debug)]
struct Workflow {
    rules: Vec<Rule>,
    default: Destination,
}

#[derive(Debug)]
enum Category {
    X,
    M,
    A,
    S,
}

#[derive(Debug)]
enum Comparison {
    LT,
    GT,
}

#[derive(Debug)]
enum Destination {
    Workflow(String),
    Accept,
    Reject,
}

fn parse_destination(s: &str) -> Destination {
    match s {
        "A" => Destination::Accept,
        "R" => Destination::Reject,
        _ => Destination::Workflow(s.to_string()),
    }
}

#[derive(Debug)]
struct Rule {
    category: Category,
    greater: bool,
    limit: usize,
    destination: Destination,
}

fn parse_rule(s: &str) -> Rule {
    let mut s_iter = s.chars();
    let category = match s_iter.next().unwrap() {
        'x' => Category::X,
        'm' => Category::M,
        'a' => Category::A,
        's' => Category::S,
        other => panic!("Unknown category '{other}'"),
    };
    let greater = match s_iter.next().unwrap() {
        '>' => true,
        '<' => false,
        other => panic!("Unknown condition '{other}'"),
    };
    let colon: usize = s.find(':').unwrap();
    Rule {
        category,
        greater,
        limit: s[3..colon].parse().unwrap(),
        destination: parse_destination(&s[colon + 1..]),
    }
}

#[derive(Debug)]
struct Part {
    x: usize,
    m: usize,
    a: usize,
    s: usize,
}

fn parse_part(s: &str) -> Part {
    let s_trimmed: &str = s.strip_prefix('{').unwrap().strip_suffix('}').unwrap();
    let mut iter = s_trimmed.split(',');
    let x = iter
        .next()
        .unwrap()
        .strip_prefix("x=")
        .unwrap()
        .parse()
        .unwrap();
    let m = iter
        .next()
        .unwrap()
        .strip_prefix("m=")
        .unwrap()
        .parse()
        .unwrap();
    let a = iter
        .next()
        .unwrap()
        .strip_prefix("a=")
        .unwrap()
        .parse()
        .unwrap();
    let s = iter
        .next()
        .unwrap()
        .strip_prefix("s=")
        .unwrap()
        .parse()
        .unwrap();
    Part { x, m, a, s }
}

fn read_input() -> (HashMap<String, Workflow>, Vec<Part>) {
    let mut h: HashMap<String, Workflow> = HashMap::new();
    let mut ps: Vec<Part> = Vec::new();
    let mut parsing_workflows: bool = true;

    for line in stdin_lines() {
        if line.is_empty() {
            assert!(parsing_workflows);
            parsing_workflows = false;
            continue;
        }
        if parsing_workflows {
            let open_brace: usize = line.find('{').unwrap();
            let name: &str = &line[..open_brace];
            let contents: &str = line[open_brace + 1..].strip_suffix('}').unwrap();
            let last_comma: usize = contents.rfind(',').unwrap();
            let default: Destination = parse_destination(&contents[last_comma + 1..]);
            let rules: Vec<Rule> = contents[..last_comma].split(',').map(parse_rule).collect();
            h.insert(name.to_string(), Workflow { rules, default });
        } else {
            ps.push(parse_part(&line));
        }
    }
    (h, ps)
}

fn main() {
    let (workflows, parts) = read_input();
    let part_a: usize = workflows.len();

    for (name, workflow) in workflows {
        println!("{}: {:?}", name, workflow);
    }
    for p in parts {
        println!("{:?}", p);
    }

    println!("{}", part_a);
}
