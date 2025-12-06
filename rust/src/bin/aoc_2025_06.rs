// Advent of Code, 2025 day 6

use std::io;

// read number strings and the operator string from stdin. No processing as part_b is whitespace-sensitive.
fn read_input() -> (Vec<String>, String) {
    let mut lines: Vec<String> = io::stdin().lines().map(|line| line.unwrap()).collect();
    let ops_line: String = lines.pop().unwrap();
    (lines, ops_line)
}

fn part_a(number_strings: &[String], ops_string: &str) -> u64 {
    let numbers: Vec<Vec<u64>> = number_strings
        .iter()
        .map(|s| s.split_whitespace().map(|s| s.parse().unwrap()).collect())
        .collect();
    let ops: Vec<bool> = ops_string.split_whitespace().map(|s| s == "*").collect();
    let mut grand_total: u64 = 0;
    for (i, op) in ops.iter().enumerate() {
        let column = numbers.iter().map(|v| v[i]);
        let problem: u64 = if *op { column.product() } else { column.sum() };
        grand_total += problem;
    }
    grand_total
}

fn part_b(number_strings: &[String], ops_string: &str) -> u64 {
    // convert strings to individual digits, using None for spaces
    let numbers: Vec<Vec<Option<u64>>> = number_strings
        .iter()
        .map(|s| {
            s.chars()
                .map(|c| {
                    if c == ' ' {
                        None
                    } else {
                        Some(c.to_string().parse().unwrap())
                    }
                })
                .collect()
        })
        .collect();
    // represent operators as bool (true for multiply), using None for spaces
    let ops: Vec<Option<bool>> = ops_string
        .chars()
        .map(|c| if c == ' ' { None } else { Some(c == '*') })
        .collect();

    let mut grand_total: u64 = 0;
    let mut current_problem: Option<u64> = None; // running total for the current problem
    let mut current_op: Option<bool> = None; // the operator for the current problem
    // iterate horizontally across all the rows with a column index
    for i in 0..numbers[0].len() {
        let mut number: Option<u64> = None; // number we are building for this column
        // iterate vertically over the rows, looking at the number at that column index
        for s in &numbers {
            // if we have a digit, combine it with the current_number
            if let Some(d) = s[i] {
                number = match number {
                    None => Some(d),
                    Some(t) => Some(10 * t + d),
                }
            }
        }
        match (number, current_op, current_problem) {
            // blank vertical column means we finished this problem
            (None, Some(_), Some(t)) => {
                grand_total += t;
                current_problem = None;
                current_op = None;
            }
            // first number in the problem
            (Some(n), None, None) => {
                current_problem = Some(n);
                current_op = ops[i];
                assert!(current_op.is_some());
            }
            // subsequent number in the problem
            (Some(n), Some(op), Some(t)) => {
                current_problem = Some(if op { t * n } else { t + n });
            }
            _ => panic!("Bad state: {number:?} {current_op:?} {current_problem:?}"),
        }
    }
    grand_total + current_problem.unwrap()
}

fn main() {
    let (numbers, ops) = read_input();

    println!("{}", part_a(&numbers, &ops));
    println!("{}", part_b(&numbers, &ops));
}
