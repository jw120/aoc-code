// Advent of Code, 2023 day 19

use aoc_rust::stdin_lines;
use std::collections::HashMap;

//
// Data structures
//

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

//
// Part (a) data structure - Parts

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

//
// Part (a) logic
//

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

//
// Part (b) logic
//

// Possible part values (inclusive)
#[derive(Clone, Debug)]
struct Set {
    x: (usize, usize),
    m: (usize, usize),
    a: (usize, usize),
    s: (usize, usize),
}

fn overlap(s1: &Set, s2: &Set) -> bool {
    range_overlap(s1.x, s2.x)
        || range_overlap(s1.m, s2.m)
        || range_overlap(s1.a, s2.a)
        || range_overlap(s1.s, s2.s)
}

fn range_overlap((a_min, a_max): (usize, usize), (b_min, b_max): (usize, usize)) -> bool {
    !(a_max < b_min || b_max < a_min)
}

// Split set based on given rule - returning sets that pass, fail
fn split(rule: &Rule, set: Set) -> (Option<Set>, Option<Set>) {
    let (value_min, value_max) = match rule.category {
        Category::X => set.x,
        Category::M => set.m,
        Category::A => set.a,
        Category::S => set.s,
    };
    let limit = rule.limit;
    let (pass, fail) = if rule.greater {
        if limit > value_max {
            // None of the values pass
            (None, Some((value_min, value_max)))
        } else if limit > value_min {
            // Some values pass
            (Some((limit, value_max)), Some((value_min, limit - 1)))
        } else {
            // All values pass
            (Some((value_min, value_max)), None)
        }
    } else if limit < value_min {
        // None of the values pass
        (None, Some((value_min, value_max)))
    } else if limit < value_max {
        // Some values pass
        (Some((value_min, limit - 1)), Some((limit, value_max)))
    } else {
        // All values pass
        (Some((value_min, value_max)), None)
    };
    match rule.category {
        Category::X => (
            pass.map(|p| Set { x: p, ..set }),
            fail.map(|p| Set { x: p, ..set }),
        ),
        Category::M => (
            pass.map(|p| Set { m: p, ..set }),
            fail.map(|p| Set { m: p, ..set }),
        ),
        Category::A => (
            pass.map(|p| Set { a: p, ..set }),
            fail.map(|p| Set { a: p, ..set }),
        ),
        Category::S => (
            pass.map(|p| Set { s: p, ..set }),
            fail.map(|p| Set { s: p, ..set }),
        ),
    }
}

fn run_set(set: Set, workflow_name: String, workflows: &HashMap<String, Workflow>) -> Vec<Set> {
    let mut accepted: Vec<Set> = Vec::new();
    let workflow = workflows.get(&workflow_name).unwrap();
    let mut remaining_set: Set = set;
    let mut nothing_remaining: bool = false;
    for rule in workflow.rules.clone() {
        let (pass, fail) = split(&rule, remaining_set.clone());
        if let Some(pass_set) = pass {
            match rule.destination {
                Destination::Accept => accepted.push(pass_set),
                Destination::Reject => {}
                Destination::Workflow(w) => {
                    for x in run_set(pass_set, w, workflows) {
                        accepted.push(x);
                    }
                }
            }
        }
        if let Some(fail_set) = fail {
            remaining_set = fail_set;
        } else {
            nothing_remaining = true;
        }
    }
    if !nothing_remaining {
        match &workflow.default {
            Destination::Accept => accepted.push(remaining_set),
            Destination::Reject => {}
            Destination::Workflow(w) => {
                for x in run_set(remaining_set, w.to_string(), workflows) {
                    accepted.push(x);
                }
            }
        }
    }
    accepted
}

fn set_value(set: &Set) -> usize {
    assert!(set.x.1 >= set.x.0);
    assert!(set.m.1 >= set.m.0);
    assert!(set.a.1 >= set.a.0);
    assert!(set.s.1 >= set.s.0);
    (set.x.1 - set.x.0 + 1)
        * (set.m.1 - set.m.0 + 1)
        * (set.a.1 - set.a.0 + 1)
        * (set.s.1 - set.s.0 + 1)
}

fn sum_valid(valid: &[Set]) -> usize {
    println!("Valid {:?}", valid);
    for s in valid {
        println!("{:?}", s);
    }
    for s1 in valid {
        for s2 in valid {
            assert!(!overlap(s1, s2), "{:?} {:?}", s1, s2);
        }
    }

    valid.iter().map(set_value).sum()
}

//
// Top-level code
//

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

    let part_a: usize = parts.iter().filter_map(|p| run_part(p, &workflows)).sum();
    println!("{part_a}");

    let start: Set = Set {
        x: (1, 4000),
        m: (1, 4000),
        a: (1, 4000),
        s: (1, 4000),
    };
    let valid: Vec<Set> = run_set(start, "in".to_string(), &workflows);
    let part_b: usize = sum_valid(&valid);
    println!("{part_b}");
}
