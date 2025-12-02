// Advent of Code, 2025 day XX

use std::{collections::HashSet, io};

/// Parse a string to a range
fn parse_range(s: &str) -> (u64, u64) {
    let (a, b) = s.split_once('-').unwrap();
    (a.parse().unwrap(), b.parse().unwrap())
}

/// Parse input into ranges
fn parse_ranges(line: &str) -> Vec<(u64, u64)> {
    line.trim().split(',').map(parse_range).collect()
}

/// Count number of digits in an integer (return 0 for 0)
fn count_digits(x: u64) -> u32 {
    let mut t = x;
    let mut digits: u32 = 0;
    while t > 0 {
        t /= 10;
        digits += 1;
    }
    digits
}

/// Split an integer with an even number of digits in two: 1234 -> (12, 34)
fn digit_split(x: u64) -> (u64, u64) {
    let z = 10u64.pow(count_digits(x) / 2);
    (x / z, x % z)
}

/// Return an integer that repeats the digits of the input: 123 -> 123123
fn double_digits(x: u64) -> u64 {
    let z = 10u64.pow(count_digits(x));
    z * x + x
}

/// Return the sum of numbers in the range that are invalid via a doubled sequence
fn sum_double_invalids((a, b): (u64, u64)) -> u64 {
    let a_digits = count_digits(a);
    let b_digits = count_digits(b);
    // If range spans different numbers of digits, split it
    // 995-1006 => 995-999,1000-1006
    if a_digits != b_digits {
        let c: u64 = 10u64.pow(b_digits - 1);
        return sum_double_invalids((a, c - 1)) + sum_double_invalids((c, b));
    }
    // Odd number of digits can't have anything invalid
    if !a_digits.is_multiple_of(2) {
        return 0;
    }
    // Range is now a1a2..=b1b2, its invalid numbers are the intersection
    // of the range a1..=b1 and a2..=b2
    let (a1, _a2) = digit_split(a);
    let (b1, _b2) = digit_split(b);
    (a1..=b1)
        .map(double_digits)
        .filter(|x| a <= *x && *x <= b)
        .sum()
}

/// Return the first n digits of a number:
///
/// ```
/// first_digits(12345, 2) // 12
/// ```
fn first_digits(x: u64, n: u32) -> u64 {
    x / 10u64.pow(count_digits(x) - n)
}

/// Return an integer that repeats the digits of the input n times:
///
/// ```
/// repeat_digits(12, 3) // 121212
/// ```
fn repeat_digits(x: u64, n: u32) -> u64 {
    let z = 10u64.pow(count_digits(x));
    let mut result: u64 = 0;
    for _ in 0..n {
        result = result * z + x;
    }
    result
}

/// Return the sum of numbers in the range that are invalid via a repeated sequence
fn sum_repeated_invalids((a, b): (u64, u64)) -> u64 {
    let a_digits = count_digits(a);
    let b_digits = count_digits(b);
    // If range spans different numbers of digits, split it
    // 995-1006 => 995-999,1000-1006
    if a_digits != b_digits {
        let c: u64 = 10u64.pow(b_digits - 1);
        return sum_repeated_invalids((a, c - 1)) + sum_repeated_invalids((c, b));
    }
    let mut invalids: HashSet<u64> = HashSet::new();
    for sequence_length in 1..=(a_digits / 2) {
        if !a_digits.is_multiple_of(sequence_length) {
            continue;
        }
        let repeats = a_digits / sequence_length;
        let a1 = first_digits(a, sequence_length);
        let b1 = first_digits(b, sequence_length);
        for y in a1..=b1 {
            let x = repeat_digits(y, repeats);
            if a <= x && x <= b {
                invalids.insert(x);
            }
        }
    }
    invalids.iter().sum()
}

/// Solve part a
fn part_a(ranges: &[(u64, u64)]) -> u64 {
    ranges.iter().map(|r| sum_double_invalids(*r)).sum()
}

/// Solve part b
fn part_b(ranges: &[(u64, u64)]) -> u64 {
    ranges.iter().map(|r| sum_repeated_invalids(*r)).sum()
}

fn main() {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    let ranges = parse_ranges(&line);

    println!("{}", part_a(&ranges));
    println!("{}", part_b(&ranges));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(
            parse_ranges("11-22,95-115,998-1012"),
            vec![(11, 22), (95, 115), (998, 1012)]
        );
    }

    #[test]
    fn test_count_digits() {
        assert_eq!(count_digits(0), 0);
        assert_eq!(count_digits(5), 1);
        assert_eq!(count_digits(42), 2);
        assert_eq!(count_digits(352), 3);
        assert_eq!(count_digits(12543), 5);
        assert_eq!(count_digits(1254), 4);
    }

    #[test]
    fn test_digit_split() {
        assert_eq!(digit_split(12), (1, 2));
        assert_eq!(digit_split(1234), (12, 34));
        assert_eq!(digit_split(654_321), (654, 321));
        assert_eq!(digit_split(65_499_321), (6549, 9321));
    }

    #[test]
    fn test_first_digits() {
        assert_eq!(first_digits(87_654_321, 1), 8);
        assert_eq!(first_digits(87_654_321, 3), 876);
        assert_eq!(first_digits(87_654_321, 5), 87654);
    }

    #[test]
    fn test_double_digits() {
        assert_eq!(double_digits(1), 11);
        assert_eq!(double_digits(12), 1212);
        assert_eq!(double_digits(143), 143_143);
        assert_eq!(double_digits(1234), 12_341_234);
    }

    #[test]
    fn test_repeat_digits() {
        assert_eq!(repeat_digits(12, 1), 12);
        assert_eq!(repeat_digits(12, 2), 1_212);
        assert_eq!(repeat_digits(12, 3), 121_212);
        assert_eq!(repeat_digits(54321, 3), 543_215_432_154_321);
    }
}
