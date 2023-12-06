// Advent of Code, 2022 day XX

use aoc_rust::stdin_lines;
use std::collections::HashSet;

fn char_score(c: char) -> u32 {
    let c_ord: u32 = c.into();
    let a_ord: u32 = 'a'.into();
    let a_cap_ord: u32 = 'A'.into();
    if c.is_ascii_lowercase() {
        c_ord - a_ord + 1
    } else if c.is_ascii_uppercase() {
        c_ord - a_cap_ord + 27
    } else {
        panic!("Unrecognized character");
    }
}

fn backpack_score(s: &str) -> u32 {
    assert!(s.len() % 2 == 0);
    let half_length = s.len() / 2;
    let left: &HashSet<char> = &HashSet::from_iter(s[..half_length].chars());
    let right: &HashSet<char> = &HashSet::from_iter(s[half_length..].chars());
    let intersection: HashSet<char> = left & right;
    let mut iter = intersection.iter();
    let x: &char = iter.next().unwrap();
    assert!(iter.next().is_none());
    char_score(*x)
}

fn part_b(lines: &[String]) -> u32 {
    let mut lines_iter = lines.iter();
    let mut score = 0;
    while let (Some(a), Some(b), Some(c)) =
        (lines_iter.next(), lines_iter.next(), lines_iter.next())
    {
        let a_set: &HashSet<char> = &HashSet::from_iter(a.chars());
        let b_set: &HashSet<char> = &HashSet::from_iter(b.chars());
        let c_set: &HashSet<char> = &HashSet::from_iter(c.chars());
        let ab_set: HashSet<char> = a_set & b_set;
        let abc_set: HashSet<char> = &ab_set & c_set;
        let mut combined = abc_set.iter();
        let x = combined.next().unwrap();
        assert!(combined.next().is_none());
        score += char_score(*x);
    }
    score
}

fn main() {
    let backpacks: Vec<String> = stdin_lines().collect();

    let part_a: u32 = backpacks.iter().map(|s| backpack_score(s)).sum();

    println!("{}", part_a);
    println!("{}", part_b(&backpacks));
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::zip;
    #[test]
    fn test_char_score() {
        assert_eq!(char_score('b'), 2);
        assert_eq!(char_score('C'), 29);
    }

    #[should_panic]
    #[test]
    fn test_char_score_invalid() {
        char_score('5');
    }

    // cspell: disable
    const BACKPACK_EXAMPLES: [&str; 6] = [
        "vJrwpWtwJgWrhcsFMMfFFhFp",
        "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
        "PmmdzqPrVvPwwTWBwg",
        "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
        "ttgJtRGJQctTZtZT",
        "CrZsJsPPZsGzwwsLwLmpwMDw",
    ];
    const BACKPACK_EXPECTED: [u32; 6] = [16, 38, 42, 22, 20, 19];
    // cspell: enable

    #[test]
    fn test_backpack_score() {
        for (s, x) in zip(BACKPACK_EXAMPLES.iter(), BACKPACK_EXPECTED.iter()) {
            assert_eq!(backpack_score(s), *x);
        }
    }
}
