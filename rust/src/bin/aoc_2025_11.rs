// Advent of Code, 2025 day 11

use std::collections::HashSet;
use std::collections::hash_map::HashMap;
use std::io;

fn read_input() -> HashMap<String, Vec<String>> {
    let mut h: HashMap<String, Vec<String>> = HashMap::new();
    for line in io::stdin().lines() {
        let line = line.unwrap();
        let (device, outputs) = line.split_once(": ").unwrap();
        assert!(
            h.insert(
                device.to_string(),
                outputs
                    .split_ascii_whitespace()
                    .map(ToString::to_string)
                    .collect()
            )
            .is_none()
        );
    }
    h
}

fn part_a(connections: &HashMap<String, Vec<String>>) -> u32 {
    let mut out_count: u32 = 0;
    let mut active_counts: HashMap<String, u32> = HashMap::new();
    let mut active_names: HashSet<String> = HashSet::new();
    active_counts.insert("you".to_string(), 1);
    active_names.insert("you".to_string());
    loop {
        // Pop a device and its count from the active devices (those we
        // are due to visit.)
        let Some(device_name) = active_names.iter().next() else {
            return out_count;
        };
        let device_name = device_name.clone();
        active_names.remove(&device_name);
        let device_count = active_counts.remove(&device_name).unwrap();

        // For each exit, either increment the number of routes to "out", or
        // add to the active devices
        for output_name in &connections[&device_name] {
            if output_name == "out" {
                out_count += device_count;
            } else {
                active_names.insert(output_name.clone());
                active_counts
                    .entry(output_name.clone())
                    .and_modify(|n| *n += device_count)
                    .or_insert(device_count);
            }
        }
    }
}

fn main() {
    let connections = read_input();

    println!("{}", part_a(&connections));
}
