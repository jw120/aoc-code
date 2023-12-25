// Advent of Code, 2023 day 18

use aoc_rust::stdin_lines;
use aoc_rust::Coord;
use std::collections::HashSet;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
enum Direction {
    U,
    R,
    D,
    L,
}

impl Direction {
    fn parse(s: &str) -> Direction {
        assert!(s.len() == 1);
        match s.chars().nth(0).unwrap() {
            'U' => Direction::U,
            'R' => Direction::R,
            'D' => Direction::D,
            'L' => Direction::L,
            _ => panic!("Unknown direction '{s}'"),
        }
    }

    fn step(self, c: Coord) -> Coord {
        match self {
            Direction::U => c + Coord { row: -1, col: 0 },
            Direction::R => c + Coord { row: 0, col: 1 },
            Direction::D => c + Coord { row: 1, col: 0 },
            Direction::L => c + Coord { row: 0, col: -1 },
        }
    }
}

fn parse_line(s: &str) -> (Direction, u32, u32) {
    let mut space_iter = s.split_ascii_whitespace();
    let direction: Direction = Direction::parse(space_iter.next().unwrap());
    let distance: u32 = space_iter.next().unwrap().parse().unwrap();
    let colour: u32 = u32::from_str_radix(
        space_iter
            .next()
            .unwrap()
            .strip_prefix("(#")
            .unwrap()
            .strip_suffix(')')
            .unwrap(),
        16,
    )
    .unwrap();
    (direction, distance, colour)
}

fn dig(steps: &[(Direction, u32, u32)]) -> HashSet<Coord> {
    let mut c: Coord = Coord { row: 0, col: 0 };
    let mut trench: HashSet<Coord> = HashSet::new();

    // We check that we only overlap an existing square once at the end
    let mut last_overlap: bool = false;
    let mut overlap_count: usize = 0;
    trench.insert(c);
    for (direction, distance, _colour) in steps {
        for _ in 0..*distance {
            c = direction.step(c);
            last_overlap = !trench.insert(c);
            overlap_count += if last_overlap { 1 } else { 0 };
        }
    }
    assert_eq!(overlap_count, 1);
    assert!(last_overlap);
    trench
}

fn main() {
    let dig_plan: Vec<(Direction, u32, u32)> = stdin_lines().map(|s| parse_line(&s)).collect();
    let ground: HashSet<Coord> = dig(&dig_plan);

    let part_a: usize = dig_plan.len();
    let part_b: usize = ground.len();

    println!("{}", part_a);
    println!("{}", part_b);
}
