// Advent of Code, 2025 day XX

use std::io;

/// Parse a string to a range
fn parse_range(s: &str) -> (u64, u64) {
    let (a, b) = s.split_once('-').unwrap();
    (a.parse().unwrap(), b.parse().unwrap())
}

/// Parse input into ranges
fn parse_ranges(line: &str) -> Vec<(u64, u64)> {
    line.split(',').map(parse_range).collect()
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

/// Return an integer that repeats the digits of the input: 123 -> 123456
fn double_digits(x: u64) -> u64 {
    let z = 10u64.pow(count_digits(x));
    z * x + x
}

/// Return the intersection of two ranges
fn intersect((_p, _q): (u64, u64), (_r, _s): (u64, u64)) -> Option<(u64, u64)> {
    todo!()
}

/// Return the sum of the invalid numbers in the given range
fn sum_invalids((a, b): (u64, u64)) -> u64 {
    let a_digits = count_digits(a);
    let b_digits = count_digits(b);
    // If range spans different numbers of digits, split it
    // 995-1006 => 995-999,1000-1006
    if a_digits != b_digits {
        let c: u64 = 10u64.pow(b_digits - 1);
        return sum_invalids((a, c - 1)) + sum_invalids((c, b));
    }
    // Odd number of digits can't have anything invalid
    if !a_digits.is_multiple_of(2) {
        return 0;
    }
    // Range is now a1a2..=b1b2, its invalid numbers are the intersection
    // of the range a1..=b1 and a2..=b2
    let (a1, a2) = digit_split(a);
    let (b1, b2) = digit_split(b);
    match intersect((a1, b1), (a2, b2)) {
        None => 0,
        Some((i1, i2)) => (i1..=i2).map(double_digits).sum(),
    }
}

/// Solve part a
fn part_a(ranges: &[(u64, u64)]) -> u64 {
    ranges.iter().map(|r| sum_invalids(*r)).sum()
}

fn main() {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    let ranges = parse_ranges(&line);

    println!("{}", part_a(&ranges));
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
    fn test_double_digits() {
        assert_eq!(double_digits(1), 11);
        assert_eq!(double_digits(12), 1212);
        assert_eq!(double_digits(143), 143_143);
        assert_eq!(double_digits(1234), 12_341_234);
    }

    #[test]
    fn test_intersect() {
        assert_eq!(intersect((1, 3), (4, 5)), None);
        assert_eq!(intersect((6, 8), (4, 5)), None);
        assert_eq!(intersect((1, 8), (4, 5)), Some((4, 5)));
        assert_eq!(intersect((4, 6), (1, 8)), Some((4, 5)));
        assert_eq!(intersect((1, 6), (5, 8)), Some((5, 6)));
        assert_eq!(intersect((5, 9), (2, 7)), Some((5, 7)));
        assert_eq!(intersect((1, 5), (5, 8)), Some((5, 5)));
        assert_eq!(intersect((5, 9), (2, 5)), Some((5, 5)));
    }
}
