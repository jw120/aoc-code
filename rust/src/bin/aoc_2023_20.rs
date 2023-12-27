// Advent of Code, 2023 day 20

use aoc_rust::stdin_lines;
use std::collections::{HashMap, VecDeque};

#[derive(Debug, PartialEq)]
enum ModuleType {
    Broadcaster,
    FlipFlop,
    Conjunction,
    Output,
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

fn read_modules() -> HashMap<String, Module> {
    let mut h: HashMap<String, Module> = HashMap::new();
    for line in stdin_lines() {
        let (module_name, module) = parse_module(&line);
        h.insert(module_name, module);
    }
    // Add output modules (which don't appear on left and side of inputs)
    let mut output_modules: Vec<String> = Vec::new();
    for (_module_type, module_outputs) in h.values() {
        for output in module_outputs {
            if !h.contains_key(output) {
                output_modules.push(output.to_string());
            }
        }
    }
    for output in output_modules {
        h.insert(output, (ModuleType::Output, Vec::new()));
    }
    h
}

// Run the system by pushing the button n times
// If n is zero then run until rx receives a pulse
fn run(modules: &HashMap<String, Module>, n: usize) -> usize {
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
            ModuleType::Broadcaster | ModuleType::Output => {}
        }
    }
    println!("Flipflop {:?}", flipflop_states);
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
    println!("Conjunction {:?}", conjunction_states);

    let mut high_count: usize = 0;
    let mut low_count: usize = 0;
    let mut button_push: usize = 0;
    loop {
        // Set up queue to hold pulses
        let mut pulse_queue: VecDeque<(Pulse, String, String)> =
            VecDeque::from([(Pulse::Low, "button".to_string(), "broadcaster".to_string())]);
        low_count += 1;

        // Run pulses from queue
        while let Some((pulse, source, destination)) = pulse_queue.pop_front() {
            // println!("{} -{}-> {}", source, pulse.show(), destination);

            if n == 0 && destination == "rx" && pulse == Pulse::Low {
                return button_push + 1;
            }
            let (module_type, outputs) = modules.get(&destination).unwrap();
            let output_pulse: Option<Pulse> = match module_type {
                ModuleType::Broadcaster => Some(pulse),
                ModuleType::FlipFlop => {
                    if pulse == Pulse::Low {
                        let state = flipflop_states.get_mut(&destination).unwrap();
                        *state = !*state;
                        Some(if *state { Pulse::High } else { Pulse::Low })
                    } else {
                        None
                    }
                }
                ModuleType::Conjunction => {
                    let memory = conjunction_states.get_mut(&destination).unwrap();
                    memory.insert(source, pulse);
                    if memory.values().all(|p| *p == Pulse::High) {
                        Some(Pulse::Low)
                    } else {
                        Some(Pulse::High)
                    }
                }
                ModuleType::Output => None,
            };
            if let Some(p) = output_pulse {
                for output in outputs {
                    pulse_queue.push_back((p, destination.clone(), output.to_string()));
                    if p == Pulse::High {
                        high_count += 1;
                    } else {
                        low_count += 1;
                    }
                }
            }
        }
        button_push += 1;
        if n > 0 && button_push >= n {
            return low_count * high_count;
        }
    }
}

fn main() {
    let modules: HashMap<String, Module> = read_modules();

    // println!("{}", run(&modules, 1000));
    if modules.keys().any(|name| name == "rx") {
        println!("{}", run(&modules, 0));
    }
}
