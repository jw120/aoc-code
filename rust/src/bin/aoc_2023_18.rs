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

// number of coordinates enclosed by the boundary
fn interior(boundary: &HashSet<Coord>) -> usize {
    // Find extent of boundary
    let row_min: i32 = boundary.iter().map(|c| c.row).min().unwrap();
    let row_max: i32 = boundary.iter().map(|c| c.row).max().unwrap();
    let col_min: i32 = boundary.iter().map(|c| c.col).min().unwrap();
    let col_max: i32 = boundary.iter().map(|c| c.col).max().unwrap();

    // Start filling from around the extent
    let mut queue: Vec<Coord> = Vec::new();
    for row in (row_min - 1)..=(row_max + 1) {
        queue.push(Coord {
            row,
            col: col_min - 1,
        });
        queue.push(Coord {
            row,
            col: col_max + 1,
        });
    }
    for col in col_min..=col_max {
        queue.push(Coord {
            row: row_min - 1,
            col,
        });
        queue.push(Coord {
            row: row_max + 1,
            col,
        });
    }

    // Flood fill
    let mut filled: HashSet<Coord> = HashSet::from_iter(queue.clone());
    while let Some(c) = queue.pop() {
        for direction in [Direction::U, Direction::R, Direction::D, Direction::L] {
            let adj = direction.step(c);
            if adj.row >= row_min
                && adj.row <= row_max
                && adj.col >= col_min
                && adj.col <= col_max
                && !boundary.contains(&adj)
                && !filled.contains(&adj)
            {
                queue.push(adj);
                filled.insert(adj);
            }
        }
    }

    // boundary and interior is area around extent less wat we filled
    let around: usize = ((row_max - row_min + 3) * (col_max - col_min + 3))
        .try_into()
        .unwrap();
    around - filled.len()
}

fn main() {
    let dig_plan: Vec<(Direction, u32, u32)> = stdin_lines().map(|s| parse_line(&s)).collect();
    let ground: HashSet<Coord> = dig(&dig_plan);

    let part_a: usize = interior(&ground);

    println!("{}", part_a);
}
