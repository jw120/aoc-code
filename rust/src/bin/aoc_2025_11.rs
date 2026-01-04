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

// return number of routes from source to target keys
fn route_count(connections: &HashMap<String, Vec<String>>, source: &str, target: &str) -> u64 {
    println!("route_count {source}->{target}");
    let mut target_count: u64 = 0;
    let mut active_counts: HashMap<String, u64> = HashMap::new();
    let mut active_names: HashSet<String> = HashSet::new();
    active_counts.insert(source.to_string(), 1);
    active_names.insert(source.to_string());
    let mut highest_count: u64 = 0;
    loop {
        // Pop a device and its count from the active devices (those we
        // are due to visit.)
        let Some(device_name) = active_names.iter().next() else {
            println!("=> {target_count}");
            return target_count;
        };
        let device_name = device_name.clone();
        active_names.remove(&device_name);
        let device_count = active_counts.remove(&device_name).unwrap();
        if device_count > highest_count {
            println!("{device_name}:{device_count}");
            highest_count = device_count;
        }

        // For each exit, either increment the number of routes to "out", or
        // add to the active devices
        for output_name in &connections[&device_name] {
            assert!(*output_name != *device_name);
            if output_name == target {
                target_count += device_count;
            } else if output_name != "out" {
                active_names.insert(output_name.clone());
                active_counts
                    .entry(output_name.clone())
                    .and_modify(|n| *n += device_count)
                    .or_insert(device_count);
            }
        }
    }
}

fn route_count_no_loop(
    connections: &HashMap<String, Vec<String>>,
    source: &str,
    target: &str,
) -> u64 {
    println!("route_count_no_loop {source}->{target}");
    let mut target_count: u64 = 0;
    let mut active_counts: HashMap<String, (Vec<String>, u64)> = HashMap::new();
    let mut active_names: HashSet<String> = HashSet::new();
    active_counts.insert(source.to_string(), (vec![], 1));
    active_names.insert(source.to_string());
    // let mut highest_count: u64 = 0;
    loop {
        // Pop a device and its count from the active devices (those we
        // are due to visit.)
        let Some(device_name) = active_names.iter().next() else {
            println!("=> {target_count}");
            return target_count;
        };
        let device_name = device_name.clone();
        active_names.remove(&device_name);
        let (device_path, device_count) = active_counts.remove(&device_name).unwrap();
        // if device_count > highest_count {
        //     // println!("{device_name}:{device_count}");
        //     highest_count = device_count;
        // }
        // println!("{device_name}:{device_count} {device_path:?}");
        if device_name == target {
            println!("{device_name}:{device_count} {device_path:?}");
        }

        // For each exit, either increment the number of routes to "out", or
        // add to the active devices
        for output_name in &connections[&device_name] {
            if output_name == target {
                target_count += device_count;
            } else if output_name != "out" {
                assert!(!device_path.contains(output_name));
                let mut output_path = device_path.clone();
                output_path.push(device_name.clone());
                if !active_names.insert(output_name.clone()) {
                    println!(
                        "{output_name} {} {}",
                        active_names.len(),
                        active_counts.len()
                    );
                }
                active_counts
                    .entry(output_name.clone())
                    .and_modify(|x| *x = (output_path.clone(), x.1 + device_count))
                    .or_insert((output_path, device_count));
            }
        }
    }
}

fn part_a(connections: &HashMap<String, Vec<String>>) -> u64 {
    route_count(connections, "you", "out")
}

fn part_b(connections: &HashMap<String, Vec<String>>) -> u64 {
    route_count_no_loop(connections, "svr", "fft")

    // // route_count(connections, "svr", "fft")
    // route_count(connections, "fft", "dac") * route_count(connections, "dac", "out")
    //     + route_count(connections, "svr", "dac")
    //         * route_count(connections, "dac", "fft")
    //         * route_count(connections, "fft", "out")
}

fn main() {
    let connections = read_input();

    // println!("{}", part_a(&connections));
    println!("{}", part_b(&connections));
}
