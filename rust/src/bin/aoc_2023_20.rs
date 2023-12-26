// Advent of Code, 2023 day 20

use aoc_rust::stdin_lines;
use std::collections::HashMap;

#[derive(Debug)]
enum ModuleType {
    Broadcaster,
    FlipFlop,
    Conjunction,
}

type Module = (ModuleType, Vec<String>);

fn parse_module(s: &str) -> (String, Module) {
    let mut arrow_iter = s.split(" -> ");
    let module_name = arrow_iter.next().unwrap();
    let module_type = match module_name.chars().next().unwrap() {
        'b' => ModuleType::Broadcaster,
        '%' => ModuleType::FlipFlop,
        '&' => ModuleType::Conjunction,
        ch => panic!("Unknown module type character '{ch}'"),
    };
    let destinations: Vec<String> = arrow_iter
        .next()
        .unwrap()
        .split(", ")
        .map(|s| s.to_string())
        .collect();
    (module_name.to_string(), (module_type, destinations))
}

fn main() {
    let mut modules: HashMap<String, Module> = HashMap::new();
    for line in stdin_lines() {
        let (module_name, module) = parse_module(&line);
        modules.insert(module_name, module);
    }

    let part_a: usize = modules.len();

    println!("{part_a}");
}
