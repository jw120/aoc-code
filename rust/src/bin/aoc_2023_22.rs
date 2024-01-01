// Advent of Code, 2023 day 22

use aoc_rust::stdin_lines;
use aoc_rust::{ucoord3, UCoord3};
use std::cmp::max;

#[derive(Debug)]
enum Direction {
    X,
    Y,
    Z,
}

#[derive(Debug)]
struct Brick {
    start: UCoord3, // smaller coordinate end
    end: UCoord3,   // larger coordinate end
    direction: Direction,
    length: usize, // difference to other end
}

impl Brick {
    fn new(a: UCoord3, b: UCoord3) -> Brick {
        let (start, end) = if a.x < b.x || a.y < b.y || a.z < b.z {
            (a, b)
        } else {
            (b, a)
        };
        let direction = if start.x != end.x {
            Direction::X
        } else if start.y != end.y {
            Direction::Y
        } else {
            Direction::Z
        };
        let length = 1 + (end.x - start.x) + (end.y - start.y) + (end.z - start.z);
        Brick {
            start,
            end,
            direction,
            length,
        }
    }

    fn for_each<F>(&self, mut f: F)
    where
        F: FnMut(UCoord3),
    {
        match self.direction {
            Direction::X => (0..self.length)
                .for_each(|d| f(ucoord3(self.start.x + d, self.start.y, self.start.z))),
            Direction::Y => (0..self.length)
                .for_each(|d| f(ucoord3(self.start.x, self.start.y + d, self.start.z))),
            Direction::Z => (0..self.length)
                .for_each(|d| f(ucoord3(self.start.x, self.start.y, self.start.z + d))),
        }
    }
}

// Vector to hold which brick (index) occupies each space
struct Occupancies {
    pos: Vec<Option<usize>>,
    // x_max: usize,
    // y_max: usize,
    // z_max: usize,
    y_factor: usize,
    z_factor: usize,
}

impl Occupancies {
    fn new(x_max: usize, y_max: usize, z_max: usize) -> Occupancies {
        let pos: Vec<Option<usize>> = vec![None; x_max * y_max * z_max];
        let y_factor: usize = 1 + x_max;
        let z_factor: usize = y_factor * (1 + y_max);
        Occupancies {
            pos,
            // x_max,
            // y_max,
            // z_max,
            y_factor,
            z_factor,
        }
    }

    fn get(&self, position: &UCoord3) -> &Option<usize> {
        let offset = position.x + position.y * self.y_factor + (position.z - 1) * self.z_factor;
        self.pos.get(offset).unwrap()
    }

    fn set(&mut self, position: &UCoord3, value: Option<usize>) {
        let offset = position.x + position.y * self.y_factor + (position.z - 1) * self.z_factor;
        println!(
            "Set {} {} {} = {} -> {:?}",
            position.x, position.y, position.z, offset, value
        );
        *self.pos.get_mut(offset).unwrap() = value;
    }
}

fn read_ucoord3(s: &str) -> UCoord3 {
    let pieces: Vec<&str> = s.split(',').collect();
    match pieces[..] {
        [x, y, z] => UCoord3 {
            x: x.parse().unwrap(),
            y: y.parse().unwrap(),
            z: z.parse().unwrap(),
        },
        _ => panic!("Bad coordinate '{s}'"),
    }
}

fn read_brick<S: AsRef<str>>(s: S) -> Brick {
    let (a, b) = s.as_ref().split_once('~').unwrap();
    Brick::new(read_ucoord3(a), read_ucoord3(b))
}

fn run(bricks: &[Brick]) {
    let mut x_max: usize = 0;
    let mut y_max: usize = 0;
    let mut z_max: usize = 0;
    for brick in bricks {
        x_max = max(x_max, brick.end.x);
        y_max = max(y_max, brick.end.y);
        z_max = max(z_max, brick.end.z);
    }
    println!("Extent {x_max} {y_max} {z_max}");

    let mut occupancies: Occupancies = Occupancies::new(x_max, y_max, z_max);
    for (brick_index, brick) in bricks.iter().enumerate() {
        brick.for_each(|pos| occupancies.set(&pos, Some(brick_index)));
    }

    for z in 0..=z_max {
        for x in 0..=x_max {
            for y in 0..=y_max {
                if let Some(brick_index) = occupancies.get(&ucoord3(x, y, z)) {
                    println!("({x}, {y}, {z}): {brick_index}");
                }
            }
        }
    }
}

fn main() {
    let bricks: Vec<Brick> = stdin_lines().map(read_brick).collect();

    for (i, brick) in bricks.iter().enumerate() {
        println!("{i}: {brick:?}");
    }
    run(&bricks);

    let part_a: usize = 0;

    println!("{part_a}");
}
