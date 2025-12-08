// Advent of Code, 2025 day 8

use std::io;

#[derive(Debug)]
struct Point {
    x: u64,
    y: u64,
    z: u64,
}

impl Point {
    fn new(s: &str) -> Self {
        match s.trim().split(',').collect::<Vec<&str>>()[..] {
            [x, y, z] => Self {
                x: x.parse().unwrap(),
                y: y.parse().unwrap(),
                z: z.parse().unwrap(),
            },
            _ => panic!("Bad coordinate '{s}'"),
        }
    }

    fn distance_squared(&self, other: &Point) -> u64 {
        self.x.abs_diff(other.x).strict_pow(2)
            + self.y.abs_diff(other.y).strict_pow(2)
            + self.z.abs_diff(other.z).strict_pow(2)
    }
}

// Run until either given number of connections are made or until all
// junction boxes in one circuit
fn run(junction_boxes: &[Point], connections: Option<usize>) -> u64 {
    // Get the distances between each pair of points
    let mut distances: Vec<((usize, usize), u64)> = Vec::new();
    for (i, p) in junction_boxes.iter().enumerate() {
        for (j, q) in junction_boxes.iter().enumerate().skip(i + 1) {
            distances.push(((i, j), p.distance_squared(q)));
        }
    }
    distances.sort_by_key(|((_, _), d)| *d);

    // Assign to circuits
    let mut junction_circuits: Vec<Option<usize>> = Vec::with_capacity(junction_boxes.len());
    junction_circuits.resize(junction_boxes.len(), None);
    let mut circuit_counts: Vec<usize> = Vec::new();
    let mut new_count: usize = 0;
    for (connection_count, ((i, j), _)) in distances.iter().enumerate() {
        match (junction_circuits[*i], junction_circuits[*j]) {
            (None, None) => {
                junction_circuits[*i] = Some(circuit_counts.len());
                junction_circuits[*j] = Some(circuit_counts.len());
                circuit_counts.push(2);
                new_count = 2;
            }
            (Some(m), Some(n)) if m == n => {}
            (Some(m), Some(n)) => {
                for (index, circuit) in junction_circuits.clone().iter().enumerate() {
                    if *circuit == Some(m) {
                        junction_circuits[index] = Some(n);
                    }
                }
                circuit_counts[n] += circuit_counts[m];
                circuit_counts[m] = 0;
                new_count = circuit_counts[n];
            }
            (Some(m), None) => {
                junction_circuits[*j] = Some(m);
                circuit_counts[m] += 1;
                new_count = circuit_counts[m];
            }
            (None, Some(m)) => {
                junction_circuits[*i] = Some(m);
                circuit_counts[m] += 1;
                new_count = circuit_counts[m];
            }
        }
        // Part (a) end condition
        if let Some(limit) = connections
            && connection_count + 1 == limit
        {
            break;
        }
        // Part (b) end condition
        if new_count == junction_boxes.len() {
            return junction_boxes[*i].x * junction_boxes[*j].x;
        }
    }

    circuit_counts.sort_unstable();
    u64::try_from(
        circuit_counts[circuit_counts.len() - 3..]
            .iter()
            .product::<usize>(),
    )
    .unwrap()
}

fn main() {
    let junction_boxes: Vec<Point> = io::stdin()
        .lines()
        .map(|s| Point::new(&s.unwrap()))
        .collect();

    // For the example we run part (a) for 10 connections, for real input 1000
    let connections = if junction_boxes.len() == 20 { 10 } else { 1000 };

    println!("{}", run(&junction_boxes, Some(connections)));
    println!("{}", run(&junction_boxes, None));
}
