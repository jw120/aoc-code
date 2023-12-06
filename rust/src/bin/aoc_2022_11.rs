// Advent of Code, 2022 day 11

use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::Regex;
use std::io;

#[derive(Clone, Debug)]
enum Operation {
    Add(usize),
    Multiply(usize),
    Square,
}

impl Operation {
    fn apply(&self, old: usize) -> usize {
        match self {
            Operation::Add(x) => old + x,
            Operation::Multiply(x) => old * x,
            Operation::Square => old * old,
        }
    }

    fn parse(s: &str) -> Operation {
        let mut words_iter = s.split_whitespace();
        match words_iter.next().unwrap() {
            "+" => {
                let x = words_iter.next().unwrap().parse().unwrap();
                assert!(words_iter.next().is_none());
                Operation::Add(x)
            }
            "*" => {
                let arg = words_iter.next().unwrap();
                assert!(words_iter.next().is_none());
                if arg == "old" {
                    Operation::Square
                } else {
                    Operation::Multiply(arg.parse().unwrap())
                }
            }
            &_ => panic!("Unknown operation"),
        }
    }
}

#[derive(Clone, Debug)]
struct Monkey {
    items: Vec<usize>,
    operation: Operation,
    divisor: usize,
    dest_true: usize,
    dest_false: usize,
    count: usize,
}

impl Monkey {
    fn parse(expected_index: usize, mut line_iter: impl Iterator<Item = String>) -> Monkey {
        static INDEX_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^Monkey (\d+):$").unwrap());
        static ITEMS_RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"^\s+Starting items: (.+)$").unwrap());
        static OP_RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"^\s+Operation: new = old (.+)$").unwrap());
        static TRUE_RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"^\s+If true: throw to monkey (\d+)$").unwrap());
        static FALSE_RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"^\s+If false: throw to monkey (\d+)$").unwrap());
        static DIVISOR_RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"^\s+Test: divisible by (\d+)$").unwrap());

        let index: usize = extract_num(&INDEX_RE, &line_iter.next().unwrap());
        assert_eq!(expected_index, index);

        let items: Vec<usize> = ITEMS_RE
            .captures(&line_iter.next().unwrap())
            .unwrap()
            .get(1)
            .unwrap()
            .as_str()
            .split(", ")
            .map(|s| s.parse().unwrap())
            .collect();

        let operation: Operation = Operation::parse(
            OP_RE
                .captures(&line_iter.next().unwrap())
                .unwrap()
                .get(1)
                .unwrap()
                .as_str(),
        );

        let divisor: usize = extract_num(&DIVISOR_RE, &line_iter.next().unwrap());
        let dest_true: usize = extract_num(&TRUE_RE, &line_iter.next().unwrap());
        let dest_false: usize = extract_num(&FALSE_RE, &line_iter.next().unwrap());

        Monkey {
            items,
            operation,
            divisor,
            dest_true,
            dest_false,
            count: 0,
        }
    }
}

// Given a regex with one usize capture return the value
fn extract_num(r: &Lazy<Regex>, line: &str) -> usize {
    r.captures(line)
        .unwrap()
        .get(1)
        .unwrap()
        .as_str()
        .parse()
        .unwrap()
}

// Update (mutating) monkeys given number of times.
// If mode is present then we take all items modules its value, if not
// then all items are divided by 3 (for the first part of the problem).
fn step(monkeys: &mut [Monkey], steps: usize, mode: Option<usize>) {
    for _ in 0..steps {
        for m in 0..monkeys.len() {
            for i in 0..monkeys[m].items.len() {
                let item: usize = monkeys[m].items[i];
                let new_item_raw: usize = monkeys[m].operation.apply(item);
                let new_item: usize = match mode {
                    Some(value) => new_item_raw % value,
                    None => new_item_raw / 3,
                };
                let dest: usize = if new_item % monkeys[m].divisor == 0 {
                    monkeys[m].dest_true
                } else {
                    monkeys[m].dest_false
                };
                monkeys[dest].items.push(new_item);
                monkeys[m].count += 1;
            }
            monkeys[m].items.clear();
        }
    }
}

// Run number of steps and return most active monkeys.
// Simple mode is for first part of the problem where items are divided by 3. For
// non-simple mode we treat all numbers modulus the product of all the divisors.
fn monkey_business(monkeys: &mut [Monkey], steps: usize, simple_mode: bool) -> usize {
    let mode: Option<usize> = if simple_mode {
        None
    } else {
        Some(monkeys.iter().map(|m| m.divisor).product())
    };
    step(monkeys, steps, mode);
    let mut counts: Vec<usize> = monkeys.iter().map(|m| m.count).collect();
    counts.sort_unstable();
    counts[counts.len() - 1] * counts[counts.len() - 2]
}

fn main() {
    let monkeys: Vec<Monkey> = io::stdin()
        .lines()
        .map(Result::unwrap)
        .chunks(7)
        .into_iter()
        .enumerate()
        .map(|(i, m)| Monkey::parse(i, m))
        .collect();

    println!("{}", monkey_business(&mut monkeys.clone(), 20, true));
    println!("{}", monkey_business(&mut monkeys.clone(), 10000, false));
}
