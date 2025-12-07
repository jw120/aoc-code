// Advent of Code, 2025 day 7

use std::io;

fn read_input() -> (Vec<Vec<bool>>, usize) {
    let mut start: usize = 999;
    let mut diagram: Vec<Vec<bool>> = Vec::new();
    for line in io::stdin().lines() {
        let line: String = line.unwrap();
        if diagram.is_empty() {
            start = line.find('S').unwrap();
        }
        diagram.push(line.chars().map(|c| c == '^').collect());
    }
    (diagram, start)
}

fn part_a(diagram: &[Vec<bool>], start: usize) -> u32 {
    let n: usize = diagram[0].len();
    let mut beams: Vec<bool> = vec![false; n];
    beams[start] = true;

    let mut split_count: u32 = 0;
    let mut new_beams: Vec<bool> = vec![false; n];
    for row in &diagram[1..] {
        for (i, _) in beams.iter().enumerate().filter(|(_, b)| **b) {
            if row[i] {
                new_beams[i - 1] = true;
                new_beams[i + 1] = true;
                split_count += 1;
            } else {
                new_beams[i] = true;
            }
        }
        beams.clone_from(&new_beams);
        new_beams.fill(false);
    }
    split_count
}

fn part_b(diagram: &[Vec<bool>], start: usize) -> u64 {
    let n: usize = diagram[0].len();
    let mut beams: Vec<u64> = vec![0; n];
    beams[start] = 1;

    let mut new_beams: Vec<u64> = vec![0; n];
    for row in &diagram[1..] {
        for (i, n) in beams.iter().enumerate().filter(|(_, n)| **n > 0) {
            if row[i] {
                new_beams[i - 1] += n;
                new_beams[i + 1] += n;
            } else {
                new_beams[i] += n;
            }
        }
        beams.clone_from(&new_beams);
        new_beams.fill(0);
    }
    beams.iter().sum()
}

fn main() {
    let (diagram, start) = read_input();

    println!("{}", part_a(&diagram, start));
    println!("{}", part_b(&diagram, start));
}
