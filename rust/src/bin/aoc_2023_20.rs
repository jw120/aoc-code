// Advent of Code, 2023 day 20

// Using bool to represent pulses - true=low, false=high

// Number of flips for each bit during 1_000_000 runs

// bx 999755
// tn 999751
// nx 999744
// nr 999736

// dj 500127
// jp 499868
// jg 499878
// md 499876

// ps 250197
// hh 250186
// tv 250183
// qm 249808

// mj 124904
// tc 124845
// fh 124835
// qx 125091

// dp 62680
// td 62670
// rt 62545
// zv 62452

// bm 31335
// kg 31272
// tk 31226
// bt 31077

// mc 15868
// rz 15801
// lg 15636
// mr 15419

// rs 7957
// gq 7900
// qj 7818
// kh 7679

// ck 4094
// dh 3978
// qt 3909
// hc 3687

// hv 2106
// sr 2047
// lt 1989
// gb 1954

// bz 1053
// jh 1023
// cq 994
// qs 977

// rb 526
// pt 511
// kx 497
// bl 488

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

fn _show_pulse(p: bool) -> &'static str {
    if p {
        "low"
    } else {
        "high"
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
        .map(std::string::ToString::to_string)
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
    let mut conjunction_states: HashMap<String, HashMap<String, bool>> = HashMap::new();
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
    // println!("Flipflop {:?}", flipflop_states);
    for (module_name, (_module_type, outputs)) in modules {
        for output in outputs {
            if let Some((connection_type, _connection_outputs)) = modules.get(output) {
                if *connection_type == ModuleType::Conjunction {
                    let conjunction_map = conjunction_states.get_mut(output).unwrap();
                    conjunction_map.insert(module_name.to_string(), true);
                }
            }
        }
    }
    // println!("Conjunction {:?}", conjunction_states);

    let mut flipflop_counts: HashMap<String, usize> = HashMap::new();
    for flipflop_name in flipflop_states.keys() {
        flipflop_counts.insert(flipflop_name.to_string(), 0);
    }
    let mut flipflop_previous: HashMap<String, bool> = HashMap::new();

    let mut high_count: usize = 0;
    let mut low_count: usize = 0;
    let mut button_push: usize = 0;
    loop {
        // Set up queue to hold pulses
        let mut pulse_queue: VecDeque<(bool, String, String)> =
            VecDeque::from([(true, "button".to_string(), "broadcaster".to_string())]);
        low_count += 1;

        // Run pulses from queue
        while let Some((pulse, source, destination)) = pulse_queue.pop_front() {
            // println!("{} -{}-> {}", source, pulse.show(), destination);

            if n == 0 && destination == "rx" && pulse {
                return button_push + 1;
            }
            let (module_type, outputs) = modules.get(&destination).unwrap();
            let output_pulse: Option<bool> = match module_type {
                ModuleType::Broadcaster => Some(pulse),
                ModuleType::FlipFlop => {
                    if pulse {
                        let state = flipflop_states.get_mut(&destination).unwrap();
                        *state = !*state;
                        Some(!*state)
                    } else {
                        None
                    }
                }
                ModuleType::Conjunction => {
                    let memory = conjunction_states.get_mut(&destination).unwrap();
                    memory.insert(source, pulse);
                    Some(memory.values().all(|p| !*p))
                }
                ModuleType::Output => None,
            };
            if let Some(p) = output_pulse {
                for output in outputs {
                    pulse_queue.push_back((p, destination.clone(), output.to_string()));
                    if p {
                        low_count += 1;
                    } else {
                        high_count += 1;
                    }
                }
            }
        }

        if !flipflop_previous.is_empty() {
            for (flipflop_name, flipflop_state) in &flipflop_states {
                if flipflop_state != flipflop_previous.get(flipflop_name).unwrap() {
                    *flipflop_counts.get_mut(flipflop_name).unwrap() += 1;
                }
            }
        }
        flipflop_previous = flipflop_states.clone();

        print!("{button_push}: ");
        // for (_flipflop_name, flipflop_state) in &flipflop_states {
        //     print!("{}", if *flipflop_state { '1' } else { '0' });
        // }
        // println!();

        for conjunction_name in ["kc", "", "vn", "kt", "hn", "ph", "", "mf", "fd", "kb", "nh"] {
            if conjunction_name.is_empty() {
                println!();
            } else {
                println!(
                    "{} {:?}",
                    conjunction_name,
                    conjunction_states.get(conjunction_name).unwrap()
                );
            };
        }

        button_push += 1;
        if n > 0 && button_push >= n {
            break;
        }
    }

    for (flipflop_name, count) in flipflop_counts {
        println!("{flipflop_name} {count}");
    }
    low_count * high_count
}

fn main() {
    let modules: HashMap<String, Module> = read_modules();

    println!("{}", run(&modules, 1_000_000));
    // if modules.keys().any(|name| name == "rx") {
    //     println!("{}", run(&modules, 0));
    // }
}
