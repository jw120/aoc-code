// Advent of Code 2024 - Day 5.

use itertools::Itertools;
use std::collections::HashSet;
use std::io;

fn read_pages() -> (HashSet<(i32, i32)>, Vec<Vec<i32>>) {
    let mut orderings: HashSet<(i32, i32)> = HashSet::new();
    let mut updates: Vec<Vec<i32>> = Vec::new();
    let mut in_orderings = true;
    for raw_line in io::stdin().lines() {
        let line = raw_line.unwrap();
        let line = line.trim();
        if line.is_empty() {
            assert!(in_orderings);
            in_orderings = false;
            continue;
        }
        if in_orderings {
            let mut iter = line.split('|');
            let a: i32 = iter.next().unwrap().parse().unwrap();
            let b: i32 = iter.next().unwrap().parse().unwrap();
            assert!(iter.next().is_none());
            orderings.insert((a, b));
        } else {
            updates.push(line.split(',').map(|s| s.parse().unwrap()).collect());
        }
    }
    (orderings, updates)
}

fn valid_middle(update: &[i32], orderings: &HashSet<(i32, i32)>) -> i32 {
    for v in update.iter().combinations(2) {
        let [i, j] = v[..] else {
            panic!("Expected 2-vector from combinations(2)")
        };
        if orderings.contains(&(*j, *i)) {
            return 0;
        }
    }
    update[update.len() / 2]
}

fn fixed_middle(update: &[i32], orderings: &HashSet<(i32, i32)>) -> i32 {
    let mut any_invalid = false;
    let mut u: Vec<i32> = Vec::from(update);
    for v in (0..u.len()).combinations(2) {
        let [i, j] = v[..] else {
            panic!("Expected 2-vector from combinations(2)")
        };
        if orderings.contains(&(u[j], u[i])) {
            any_invalid = true;
            u.swap(i, j);
        }
    }
    if any_invalid {
        u[u.len() / 2]
    } else {
        0
    }
}

fn main() {
    let (orderings, updates) = read_pages();
    println!(
        "{}",
        updates
            .iter()
            .map(|u| valid_middle(u, &orderings))
            .sum::<i32>()
    );
    println!(
        "{}",
        updates
            .iter()
            .map(|u| fixed_middle(u, &orderings))
            .sum::<i32>()
    );
}
