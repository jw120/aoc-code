// Advent of Code, 2022 day 4

use aoc_rust::stdin_lines;

struct Pair {
    a: (i32, i32),
    b: (i32, i32),
}

impl Pair {
    fn contains(&self) -> bool {
        (self.a.0 <= self.b.0 && self.a.1 >= self.b.1)
            || (self.b.0 <= self.a.0 && self.b.1 >= self.a.1)
    }

    fn overlaps(&self) -> bool {
        !(self.a.1 < self.b.0 || self.b.1 < self.a.0)
    }

    fn parse(s: &str) -> Pair {
        let mut iter = s.trim().split(',');
        let a = Pair::parse_range(iter.next().unwrap());
        let b = Pair::parse_range(iter.next().unwrap());
        assert!(iter.next().is_none());
        Pair { a, b }
    }

    fn parse_range(s: &str) -> (i32, i32) {
        let mut iter = s.trim().split('-');
        let lo: i32 = iter.next().unwrap().parse().unwrap();
        let hi: i32 = iter.next().unwrap().parse().unwrap();
        assert!(iter.next().is_none());
        (lo, hi)
    }
}

fn main() {
    let pairs: Vec<Pair> = stdin_lines().map(|s| Pair::parse(&s)).collect();

    println!("{}", pairs.iter().filter(|p| p.contains()).count());
    println!("{}", pairs.iter().filter(|p| p.overlaps()).count());
}
