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
    let mut beams_even: Vec<bool> = vec![false; n];
    let mut beams_odd: Vec<bool> = vec![false; n];
    beams_even[start] = true;

    let mut split_count: u32 = 0;
    for (r, row) in diagram.iter().enumerate() {
        let (current_beams, next_beams) = if r.is_multiple_of(2) {
            (&mut beams_even, &mut beams_odd)
        } else {
            (&mut beams_odd, &mut beams_even)
        };
        next_beams.fill(false);
        for (i, _) in current_beams.iter().enumerate().filter(|(_, b)| **b) {
            if row[i] {
                next_beams[i - 1] = true;
                next_beams[i + 1] = true;
                split_count += 1;
            } else {
                next_beams[i] = true;
            }
        }
    }
    split_count
}

fn part_b(diagram: &[Vec<bool>], start: usize) -> u64 {
    let n: usize = diagram[0].len();
    let mut beams_even: Vec<u64> = vec![0; n];
    let mut beams_odd: Vec<u64> = vec![0; n];
    beams_even[start] = 1;

    for (r, row) in diagram.iter().enumerate() {
        let (current_beams, next_beams) = if r.is_multiple_of(2) {
            (&mut beams_even, &mut beams_odd)
        } else {
            (&mut beams_odd, &mut beams_even)
        };
        next_beams.fill(0);
        for (i, n) in current_beams.iter().enumerate().filter(|(_, n)| **n > 0) {
            if row[i] {
                next_beams[i - 1] += n;
                next_beams[i + 1] += n;
            } else {
                next_beams[i] += n;
            }
        }
    }
    beams_even.iter().sum()
}

fn main() {
    let (diagram, start) = read_input();

    println!("{}", part_a(&diagram, start));
    println!("{}", part_b(&diagram, start));
}
