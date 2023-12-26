// Advent of Code, 2023 day 20

use aoc_rust::stdin_lines;
use std::collections::HashMap;
use std::collections::VecDeque;

#[derive(Debug, PartialEq)]
enum ModuleType {
    Broadcaster,
    FlipFlop,
    Conjunction,
}

type Module = (ModuleType, Vec<String>);

#[derive(Clone, Copy, Debug, PartialEq)]
enum Pulse {
    Low,
    High,
}

impl Pulse {
    fn show(self) -> &'static str {
        match self {
            Pulse::High => "high",
            Pulse::Low => "low",
        }
    }
}

fn parse_module(s: &str) -> (String, Module) {
    let mut arrow_iter = s.split(" -> ");
    let module_key = arrow_iter.next().unwrap();
    let module_type = match module_key.chars().next().unwrap() {
        'b' => ModuleType::Broadcaster,
        '%' => ModuleType::FlipFlop,
        '&' => ModuleType::Conjunction,
        ch => panic!("Unknown module type character '{ch}'"),
    };
    let module_name = if module_type == ModuleType::Broadcaster {
        assert_eq!(module_key, "broadcaster");
        module_key
    } else {
        &module_key[1..]
    };
    let destinations: Vec<String> = arrow_iter
        .next()
        .unwrap()
        .split(", ")
        .map(|s| s.to_string())
        .collect();
    (module_name.to_string(), (module_type, destinations))
}

fn run(modules: &HashMap<String, Module>) {
    // Set up states
    let mut flipflop_states: HashMap<String, bool> = HashMap::new();
    let mut conjunction_states: HashMap<String, HashMap<String, Pulse>> = HashMap::new();
    for (module_name, (module_type, _outputs)) in modules {
        match module_type {
            ModuleType::FlipFlop => {
                flipflop_states.insert(module_name.to_string(), false);
            }
            ModuleType::Conjunction => {
                conjunction_states.insert(module_name.to_string(), HashMap::new());
            }
            ModuleType::Broadcaster => {}
        }
    }
    println!("Flipflops {:?}", flipflop_states);
    for (module_name, (_module_type, outputs)) in modules {
        for output in outputs {
            if let Some((connection_type, _connection_outputs)) = modules.get(output) {
                if *connection_type == ModuleType::Conjunction {
                    let conjunction_map = conjunction_states.get_mut(output).unwrap();
                    conjunction_map.insert(module_name.to_string(), Pulse::Low);
                }
            }
        }
    }
    println!("Conjunctions {:?}", conjunction_states);

    // Set up queue to hold pulses
    let mut pulse_queue: VecDeque<(Pulse, String, String)> =
        VecDeque::from([(Pulse::Low, "button".to_string(), "broadcaster".to_string())]);
    while let Some((pulse, source, destination)) = pulse_queue.pop_front() {
        println!("{} -{}-> {}", source, pulse.show(), destination);
        let (module_type, outputs) = modules.get(&destination).unwrap();
        match module_type {
            ModuleType::Broadcaster => {
                for output in outputs {
                    pulse_queue.push_back((pulse, destination.clone(), output.to_string()));
                }
            }
            ModuleType::FlipFlop => {
                if pulse == Pulse::Low {
                    let state = flipflop_states.get_mut(&destination).unwrap();
                    let output_pulse = if *state { Pulse::Low } else { Pulse::High };
                    *state = !*state;
                    for output in outputs {
                        pulse_queue.push_back((
                            output_pulse,
                            destination.clone(),
                            output.to_string(),
                        ));
                    }
                }
            }
            ModuleType::Conjunction => {}
        }
    }
}

fn main() {
    let mut modules: HashMap<String, Module> = HashMap::new();
    for line in stdin_lines() {
        let (module_name, module) = parse_module(&line);
        modules.insert(module_name, module);
    }
    println!("Modules {:?}", modules);
    run(&modules);

    let part_a: usize = modules.len();

    println!("{part_a}");
}
