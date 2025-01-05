// Advent of Code, 2023 day 22

use aoc_rust::{stdin_lines, ucoord3, UCoord3};
use std::{cmp::max, collections::HashSet};

#[derive(Copy, Clone, Debug, PartialEq)]
enum Direction {
    X,
    Y,
    Z,
}

#[derive(Copy, Clone, Debug, PartialEq)]
struct Brick {
    start: UCoord3, // smaller coordinate end
    end: UCoord3,   // larger coordinate end
    direction: Direction,
    length: usize,
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
    bricks: Vec<Brick>,
    pos: Vec<Option<usize>>,
    // x_max: usize,
    // y_max: usize,
    // z_max: usize,
    y_factor: usize,
    z_factor: usize,
}

impl Occupancies {
    // x runs 0..x_size, y 0..y_size, z 1..=z_size
    fn new() -> Occupancies {
        let bricks: Vec<Brick> = stdin_lines().map(read_brick).collect();

        let mut x_max: usize = 0;
        let mut y_max: usize = 0;
        let mut z_max: usize = 0;
        for brick in &bricks {
            x_max = max(x_max, brick.end.x);
            y_max = max(y_max, brick.end.y);
            z_max = max(z_max, brick.end.z);
        }
        let x_size = 1 + x_max;
        let y_size = 1 + y_max;
        let z_size = z_max;

        let pos: Vec<Option<usize>> = vec![None; x_size * y_size * z_size];
        let y_factor: usize = x_size;
        let z_factor: usize = x_size * y_size;

        Occupancies {
            bricks,
            pos,
            y_factor,
            z_factor,
        }
    }

    fn build(&mut self) {
        for brick_index in 0..self.bricks.len() {
            let brick = self.bricks[brick_index];
            brick.for_each(|pos| self.set(&pos, Some(brick_index)));
        }
    }

    fn offset(&self, position: &UCoord3) -> usize {
        position.x + position.y * self.y_factor + (position.z - 1) * self.z_factor
    }

    #[allow(clippy::ref_option)]
    fn get(&self, position: &UCoord3) -> &Option<usize> {
        self.pos.get(self.offset(position)).unwrap()
    }

    fn set(&mut self, position: &UCoord3, value: Option<usize>) {
        let offset = self.offset(position);
        println!(
            "Set {} {} {} = {} -> {:?}",
            position.x, position.y, position.z, offset, value
        );
        *(self.pos.get_mut(offset).unwrap()) = value;
    }

    // Return indices of the bricks that are supporting the given brick
    // and indicate if the given brick rests on the floor
    fn supported_by(&self, brick: &Brick) -> (HashSet<usize>, bool) {
        let mut support_set: HashSet<usize> = HashSet::new();
        let mut on_floor: bool = false;
        brick.for_each(|position| {
            if position.z == 1 {
                on_floor = true;
            } else if let Some(b) = self.get(&(position - ucoord3(0, 0, 1))) {
                if self.bricks[*b] != *brick {
                    support_set.insert(*b);
                }
            }
        });
        (support_set, on_floor)
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

fn main() {
    let mut occupancies: Occupancies = Occupancies::new();
    occupancies.build();

    println!("Supported by");
    for (brick_index, brick) in occupancies.bricks.iter().enumerate() {
        println!("{}: {:?}", brick_index, occupancies.supported_by(brick));
    }
}
