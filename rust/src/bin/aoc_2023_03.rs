// Advent of Code, 2023 day 03

use aoc_rust::UCoord;
use std::io::stdin;

struct Number {
    value: u32,
    position: UCoord, // position of first digit
    length: usize,    // number of digits
}

// subtract one unless value is zero
fn dec(x: usize) -> usize {
    if x > 0 {
        x - 1
    } else {
        x
    }
}

impl Number {
    // Is the number adjacent to the coordinate
    fn is_adjacent(&self, c: UCoord) -> bool {
        c.col >= dec(self.position.col)
            && c.col <= self.position.col + self.length
            && c.row >= dec(self.position.row)
            && c.row <= self.position.row + 1
    }

    // Is the number adjacent to any of the coordinates
    fn is_any_adjacent(&self, cs: &[UCoord]) -> bool {
        cs.iter().any(|c| self.is_adjacent(*c))
    }
}

fn gear_ratios(stars: &[UCoord], numbers: &[Number]) -> u32 {
    let mut sum = 0;
    for star in stars {
        let adjacent_numbers: Vec<&Number> =
            numbers.iter().filter(|n| n.is_adjacent(*star)).collect();
        match adjacent_numbers[..] {
            [n1, n2] => {
                sum += n1.value * n2.value;
            }
            _ => {}
        }
    }
    sum
}

fn read_schematic() -> (Vec<Number>, Vec<UCoord>, Vec<UCoord>) {
    let mut numbers: Vec<Number> = Vec::new();
    let mut symbols: Vec<UCoord> = Vec::new();
    let mut stars: Vec<UCoord> = Vec::new();
    let mut current_number: Option<Number> = None;

    for (row, line) in stdin().lines().enumerate() {
        for (col, ch) in line.unwrap().chars().enumerate() {
            if let Some(ch_value) = ch.to_digit(10) {
                current_number = current_number.map_or(
                    Some(Number {
                        value: ch_value,
                        position: UCoord { row, col },
                        length: 1,
                    }),
                    |n| {
                        Some(Number {
                            value: n.value * 10 + ch_value,
                            length: n.length + 1,
                            ..n
                        })
                    },
                )
            } else {
                if let Some(n) = current_number {
                    numbers.push(n);
                    current_number = None;
                }
                if ch != '.' {
                    symbols.push(UCoord { row, col });
                    if ch == '*' {
                        stars.push(UCoord { row, col });
                    }
                }
            }
        }
        if let Some(n) = current_number {
            numbers.push(n);
            current_number = None;
        }
    }
    (numbers, symbols, stars)
}

fn main() {
    let (numbers, symbols, stars) = read_schematic();

    let part_a: u32 = numbers
        .iter()
        .filter(|n| (*n).is_any_adjacent(&symbols))
        .map(|n| n.value)
        .sum();

    println!("{}", part_a);
    println!("{}", gear_ratios(&stars, &numbers));
}
