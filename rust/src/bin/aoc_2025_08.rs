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

fn part_a(junction_boxes: &[Point], connections: usize) -> usize {
    // Get the distances between each pair of points
    let mut distances: Vec<((usize, usize), u64)> = Vec::new();
    for (i, p) in junction_boxes.iter().enumerate() {
        for (j, q) in junction_boxes.iter().enumerate().skip(i + 1) {
            distances.push(((i, j), p.distance_squared(q)));
        }
    }
    distances.sort_by_key(|((_, _), d)| *d);
    // for ((i, j), d) in &distances[..10] {
    //     println!("{:?} {:?} {d:?}", junction_boxes[*i], junction_boxes[*j]);
    // }

    // Assign to circuits
    let mut junction_circuits: Vec<Option<usize>> = Vec::with_capacity(junction_boxes.len());
    junction_circuits.resize(junction_boxes.len(), None);
    let mut next_circuit: usize = 0;
    for ((i, j), _) in &distances[..connections] {
        match (junction_circuits[*i], junction_circuits[*j]) {
            (None, None) => {
                junction_circuits[*i] = Some(next_circuit);
                junction_circuits[*j] = Some(next_circuit);
                next_circuit += 1;
            }
            (Some(m), Some(n)) if m == n => {}
            (Some(m), Some(n)) => {
                for (index, circuit) in junction_circuits.clone().iter().enumerate() {
                    if *circuit == Some(m) {
                        junction_circuits[index] = Some(n);
                    }
                }
            }
            (Some(m), None) => {
                junction_circuits[*j] = Some(m);
            }
            (None, Some(m)) => {
                junction_circuits[*i] = Some(m);
            }
        }
    }

    // Count the number of boxes in each circuit
    let mut counts: Vec<usize> = vec![0; next_circuit];
    for circuit in junction_circuits.into_iter().flatten() {
        counts[circuit] += 1;
    }
    counts.sort_unstable();

    counts[counts.len() - 3..].iter().product()
}

fn main() {
    let junction_boxes: Vec<Point> = io::stdin()
        .lines()
        .map(|s| Point::new(&s.unwrap()))
        .collect();

    let connections = if junction_boxes.len() == 20 { 10 } else { 1000 };

    println!("{}", part_a(&junction_boxes, connections));
}
