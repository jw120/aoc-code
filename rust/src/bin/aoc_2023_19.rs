// Advent of Code, 2023 day 19

use aoc_rust::stdin_lines;
use std::collections::HashMap;

#[derive(Debug)]
struct Workflow {
    rules: Vec<Rule>,
    default: Destination,
}

#[derive(Clone, Debug)]
enum Category {
    X,
    M,
    A,
    S,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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
        limit: s[2..colon].parse().unwrap(),
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

fn apply(rule: &Rule, part: &Part) -> Option<Destination> {
    let value = match rule.category {
        Category::X => part.x,
        Category::M => part.m,
        Category::A => part.a,
        Category::S => part.s,
    };
    if (rule.greater && value > rule.limit) || (!rule.greater && value < rule.limit) {
        Some(rule.destination.clone())
    } else {
        None
    }
}

fn run_part(part: &Part, workflows: &HashMap<String, Workflow>) -> Option<usize> {
    let mut workflow_name: String = "in".to_string();
    loop {
        let workflow = workflows.get(&workflow_name).unwrap();
        let mut destination: Option<Destination> = None;
        for rule in workflow.rules.clone() {
            destination = apply(&rule, part);
            if destination.is_some() {
                break;
            }
        }
        match destination.unwrap_or(workflow.default.clone()) {
            Destination::Accept => return Some(part.x + part.m + part.a + part.s),
            Destination::Reject => return None,
            Destination::Workflow(s) => workflow_name = s.to_string(),
        }
    }
}

fn main() {
    let (workflows, parts) = read_input();

    let part_a: usize = parts.iter().filter_map(|p| run_part(p, &workflows)).sum();

    println!("{part_a}");
}
