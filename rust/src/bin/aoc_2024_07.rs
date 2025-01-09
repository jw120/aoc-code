// Advent of Code 2024 - Day 7

use std::io;

struct Calibration {
    target: u64,
    numbers: Vec<u64>,
}

fn parse(s: &str) -> Calibration {
    let mut i = s.split(": ");
    let target: u64 = i.next().unwrap().parse().unwrap();
    let numbers: Vec<u64> = i
        .next()
        .unwrap()
        .split_ascii_whitespace()
        .map(|t| t.parse().unwrap())
        .collect();
    assert!(i.next().is_none());
    Calibration { target, numbers }
}

fn check(calibration: &Calibration, value: u64, index: usize) -> bool {
    if index == calibration.numbers.len() {
        value == calibration.target
    } else {
        check(calibration, value + calibration.numbers[index], index + 1)
            || check(calibration, value * calibration.numbers[index], index + 1)
    }
}

fn test(calibration: &Calibration) -> u64 {
    if check(calibration, calibration.numbers[0], 1) {
        calibration.target
    } else {
        0
    }
}

//     def check(self, value: int, index: int) -> bool:
//         """Check for solution with given input value and starting index."""
//         if index == len(self.numbers):
//             return value == self.target
//         return any(self.check(op(value, self.numbers[index]), index + 1) for op in (add, mul))

//     def test(self) -> int:
//         """Return target if valid, zero otherwise."""
//         return self.target if self.check(self.numbers[0], 1) else 0

//     def check3(self, value: int, index: int) -> bool:
//         """Check for solution with given input value and starting index."""
//         if index == len(self.numbers):
//             return value == self.target
//         for op in ["+", "*", "|"]:
//             match op:
//                 case "+":
//                     if self.check3(value + self.numbers[index], index + 1):
//                         return True
//                 case "*":
//                     if self.check3(value * self.numbers[index], index + 1):
//                         return True
//                 case "|":
//                     if self.check3(combine(value, self.numbers[index]), index + 1):
//                         return True
//                 case _:
//                     raise ValueError("Bad op")
//         return False

//     def test3(self) -> int:
//         """Return target if valid, zero otherwise."""
//         return self.target if self.check3(self.numbers[0], 1) else 0

// def combine(a: int, b: int) -> int:
//     """Combine digits of two integers."""
//     return int(str(a) + str(b))

// if __name__ == "__main__":
//     calibrations = [Calibration(s) for s in stdin.readlines()]
//     print(sum(c.test() for c in calibrations))
//     print(sum(c.test3() for c in calibrations))

fn main() {
    let calibrations: Vec<Calibration> = io::stdin().lines().map(|s| parse(&s.unwrap())).collect();
    println!(
        "{}",
        calibrations.into_iter().map(|c| test(&c)).sum::<u64>()
    )
}
