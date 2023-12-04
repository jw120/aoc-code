// Advent of Code, 2022 day 11

use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::Regex;
use std::io;

#[derive(Clone, Debug)]
enum Operation {
    Add(u64),
    Multiply(u64),
    Square,
}

impl Operation {
    fn apply(&self, old: u64) -> u64 {
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
    items: Vec<u64>,
    operation: Operation,
    divisor: u64,
    dest_true: usize,
    dest_false: usize,
    count: u64,
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

        let index: u64 = extract_num(&INDEX_RE, &line_iter.next().unwrap());
        assert_eq!(expected_index, index as usize);

        let items: Vec<u64> = ITEMS_RE
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

        let divisor: u64 = extract_num(&DIVISOR_RE, &line_iter.next().unwrap());
        let dest_true: u64 = extract_num(&TRUE_RE, &line_iter.next().unwrap());
        let dest_false: u64 = extract_num(&FALSE_RE, &line_iter.next().unwrap());

        Monkey {
            items,
            operation,
            divisor,
            dest_true: dest_true as usize,
            dest_false: dest_false as usize,
            count: 0,
        }
    }
}

// Given a regex with one u64 capture return the value
fn extract_num(r: &Lazy<Regex>, line: &str) -> u64 {
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
fn step(monkeys: &mut [Monkey], steps: u64, mode: Option<u64>) {
    for _ in 0..steps {
        for m in 0..monkeys.len() {
            for i in 0..monkeys[m].items.len() {
                let item: u64 = monkeys[m].items[i];
                let new_item_raw: u64 = monkeys[m].operation.apply(item);
                let new_item: u64 = match mode {
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
fn monkey_business(monkeys: &mut [Monkey], steps: u64, simple_mode: bool) -> u64 {
    let mode: Option<u64> = if simple_mode {
        None
    } else {
        Some(monkeys.iter().map(|m| m.divisor).product())
    };
    step(monkeys, steps, mode);
    let mut counts: Vec<u64> = monkeys.iter().map(|m| m.count).collect();
    counts.sort();
    counts[counts.len() - 1] * counts[counts.len() - 2]
}

fn main() {
    let monkeys: Vec<Monkey> = io::stdin()
        .lines()
        .map(|r| r.unwrap())
        .chunks(7)
        .into_iter()
        .enumerate()
        .map(|(i, m)| Monkey::parse(i, m))
        .collect();

    println!("{}", monkey_business(&mut monkeys.clone(), 20, true));
    println!("{}", monkey_business(&mut monkeys.clone(), 10000, false));
}
