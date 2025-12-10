// Advent of Code, 2025 day XX

use std::cmp;
use std::io;

use grid::Grid;

#[derive(Debug)]
struct Coord {
    x: usize,
    y: usize,
}

impl Coord {
    fn new(s: &str) -> Self {
        let Some((x, y)) = s.trim().split_once(',') else {
            panic!("Bad coord '{s}'");
        };
        Self {
            x: x.parse().unwrap(),
            y: y.parse().unwrap(),
        }
    }

    fn area(&self, other: &Coord) -> usize {
        (self.x.abs_diff(other.x) + 1) * (self.y.abs_diff(other.y) + 1)
    }
}

fn part_a(red_tiles: &[Coord]) -> usize {
    let mut best_area: usize = 0;
    for (i, c) in red_tiles.iter().enumerate() {
        for d in &red_tiles[i + 1..] {
            let area = c.area(d);
            best_area = cmp::max(area, best_area);
        }
    }
    best_area
}

fn part_b(red_tiles: &[Coord]) -> usize {
    let x_max = red_tiles.iter().map(|c| c.x).max().unwrap();
    let y_max = red_tiles.iter().map(|c| c.x).max().unwrap();
    let mut green_tiles: Grid<bool> = Grid::new(x_max + 1, y_max + 1);

    for (i, t1) in red_tiles.iter().enumerate() {
        let t2: &Coord = &red_tiles[(i + 1) % red_tiles.len()];
        if t1.x == t2.x {
            let y1 = cmp::min(t1.y, t2.y);
            let y2 = cmp::max(t1.y, t2.y);
            for y in y1 + 1..y2 {
                green_tiles[(t1.x, y)] = true;
            }
        } else {
            assert!(t1.y == t2.y);
            let x1 = cmp::min(t1.x, t2.x);
            let x2 = cmp::max(t1.x, t2.x);
            for x in x1 + 1..x2 {
                green_tiles[(x, t1.y)] = true;
            }
        }
    }

    todo!()
}

fn main() {
    let red_tiles: Vec<Coord> = io::stdin()
        .lines()
        .map(|s| Coord::new(&s.unwrap()))
        .collect();

    println!("{}", part_a(&red_tiles));
    println!("{}", part_b(&red_tiles));
}
