// Advent of Code, 2023 day XX

use aoc_rust::stdin_lines;

fn parse_spring(ch: char) -> Option<bool> {
    match ch {
        '.' => Some(false),
        '#' => Some(true),
        '?' => None,
        _ => panic!("Unknown spring char '{}'", ch),
    }
}

fn read_records() -> Vec<(Vec<Option<bool>>, Vec<usize>)> {
    let mut acc: Vec<(Vec<Option<bool>>, Vec<usize>)> = Vec::new();
    for line in stdin_lines() {
        let mut space_iter = line.split_whitespace();
        let template: Vec<Option<bool>> = space_iter
            .next()
            .unwrap()
            .chars()
            .map(parse_spring)
            .collect();
        let groups = space_iter
            .next()
            .unwrap()
            .split(',')
            .map(|x| x.parse().unwrap())
            .collect();
        assert!(space_iter.next().is_none());
        acc.push((template, groups));
    }
    acc
}

// Call given function on all possible slices of given length with n ones
fn generate_bits<F>(n: usize, length: usize, callback: &mut F)
where
    F: FnMut(&[bool]),
{
    let mut xs: Vec<bool> = vec![false; length];
    gen(&mut xs, n, 0, callback);
}

// Helper function for generate_bits
fn gen<F>(xs: &mut [bool], n: usize, k: usize, callback: &mut F)
where
    F: FnMut(&[bool]),
{
    if n > xs.len() - k {
        return;
    }
    if n == 0 {
        callback(&*xs);
        return;
    }
    xs[k] = true;
    gen(xs, n - 1, k + 1, callback);
    xs[k] = false;
    gen(xs, n, k + 1, callback);
}

// Count lengths of contiguous groups
fn count_groups(xs: &[bool]) -> Vec<usize> {
    let mut groups: Vec<usize> = Vec::new();
    let mut current_group: usize = 0;
    for x in xs {
        if *x {
            current_group += 1;
        } else if current_group > 0 {
            groups.push(current_group);
            current_group = 0;
        }
    }
    if current_group > 0 {
        groups.push(current_group);
    }
    groups
}

// See if applying the arrangement of unknown springs to the template has expected groups
fn test_fit(template: &[Option<bool>], groups: &[usize], arrangement: &[bool]) -> bool {
    let mut arrangement_iter = arrangement.iter();
    let ys: Vec<bool> = template
        .iter()
        .map(|x| match x {
            Some(z) => *z,
            None => *arrangement_iter.next().unwrap(),
        })
        .collect();
    count_groups(&ys) == groups
}

// Count number of arrangements of unknown springs that match
fn count_fit(template: &[Option<bool>], groups: &[usize]) -> usize {
    let unknown_springs: usize = template.iter().filter(|x| x.is_none()).count();
    let unknown_broken_springs: usize =
        groups.iter().sum::<usize>() - template.iter().filter(|x| **x == Some(true)).count();
    let mut count: usize = 0;
    generate_bits(unknown_broken_springs, unknown_springs, &mut |xs| {
        if test_fit(template, groups, xs) {
            count += 1;
        }
    });
    count
}

fn main() {
    let records: Vec<(Vec<Option<bool>>, Vec<usize>)> = read_records();

    let part_a: usize = records
        .iter()
        .map(|(template, groups)| count_fit(template, groups))
        .sum();

    println!("{}", part_a);
}
