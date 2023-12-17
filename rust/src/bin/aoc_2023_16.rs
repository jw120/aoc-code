// Advent of Code, 2023 day 16

#![allow(clippy::upper_case_acronyms)]

use aoc_rust::{stdin_lines, Coord};
use grid::{grid, Grid};
use std::collections::HashSet;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
enum Direction {
    N,
    E,
    S,
    W,
}

impl Direction {
    fn step(&self) -> Coord {
        match self {
            Direction::N => Coord { row: -1, col: 0 },
            Direction::E => Coord { row: 0, col: 1 },
            Direction::S => Coord { row: 1, col: 0 },
            Direction::W => Coord { row: 0, col: -1 },
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
enum Square {
    #[default]
    Empty,
    SWNE,
    NWSE,
    WE,
    NS,
}

impl Square {
    fn parse(ch: char) -> Square {
        match ch {
            '.' => Square::Empty,
            '/' => Square::SWNE,
            '\\' => Square::NWSE,
            '-' => Square::WE,
            '|' => Square::NS,
            _ => panic!("Unknown char: '{}'", ch),
        }
    }
}

// Python-style singleton object
struct Contraption {
    tiles: Grid<Square>,
    visited: Grid<HashSet<Direction>>,
}

impl Contraption {
    // Construct contraption from stdin
    fn read() -> Contraption {
        let mut grid: Grid<Square> = grid![];
        for line in stdin_lines() {
            let row_vec: Vec<Square> = line.chars().map(Square::parse).collect();
            grid.push_row(row_vec);
        }
        let rows = grid.rows();
        let cols = grid.cols();
        Contraption {
            tiles: grid,
            visited: Grid::new(rows, cols),
        }
    }

    // Square at given coordinate
    fn get(&self, c: Coord) -> Square {
        self.tiles[(c.row as usize, c.col as usize)]
    }

    // Test if the coordinate is within bounds
    fn valid(&self, c: Coord) -> bool {
        c.row >= 0
            && (c.row as usize) < self.tiles.rows()
            && c.col >= 0
            && (c.col as usize) < self.tiles.cols()
    }

    // Add position and direction to visited, true if not already visited
    // if coordinate is off-grid always return true
    fn add_visited(&mut self, c: Coord, d: Direction) -> bool {
        !self.valid(c) || self.visited[(c.row as usize, c.col as usize)].insert(d)
    }

    // number of squares visited from given starting point
    fn beam(&mut self, start_position: Coord, start_direction: Direction) -> usize {
        let mut beams: Vec<(Coord, Direction)> = vec![(start_position, start_direction)];
        for i in self.visited.iter_mut() {
            i.clear()
        }

        while let Some((position, direction)) = beams.pop() {
            // println!("{:?} {:?}", position, direction);
            // skip if we have already been here
            if !self.add_visited(position, direction) {
                continue;
            }

            let next_position = position + direction.step();
            // println!("next {:?}", next_position);

            // skip if would move out of bounds
            if !self.valid(next_position) {
                continue;
            }

            let next_direction: Direction = match (self.get(next_position), direction) {
                (Square::Empty, _)
                | (Square::NS, Direction::N | Direction::S)
                | (Square::WE, Direction::E | Direction::W) => direction,
                (Square::NS, Direction::E | Direction::W) => {
                    beams.push((next_position, Direction::S));
                    Direction::N
                }
                (Square::WE, Direction::N | Direction::S) => {
                    beams.push((next_position, Direction::W));
                    Direction::E
                }
                (Square::SWNE, Direction::N) => Direction::E,
                (Square::SWNE, Direction::E) => Direction::N,
                (Square::SWNE, Direction::S) => Direction::W,
                (Square::SWNE, Direction::W) => Direction::S,
                (Square::NWSE, Direction::N) => Direction::W,
                (Square::NWSE, Direction::E) => Direction::S,
                (Square::NWSE, Direction::S) => Direction::E,
                (Square::NWSE, Direction::W) => Direction::N,
            };
            beams.push((next_position, next_direction))
        }

        self.visited.iter().filter(|s| !s.is_empty()).count()
    }
}

fn main() {
    let mut contraption = Contraption::read();

    let part_a: usize = contraption.beam(Coord { row: 0, col: -1 }, Direction::E);

    let n: i32 = contraption.tiles.rows() as i32;
    // iterator assumes grid is square
    assert_eq!(n, contraption.tiles.cols() as i32);
    let part_b = (0..n)
        .flat_map(|i| {
            [
                (Coord { row: n, col: i }, Direction::N),
                (Coord { row: i, col: -1 }, Direction::E),
                (Coord { row: -1, col: i }, Direction::S),
                (Coord { row: i, col: n }, Direction::W),
            ]
        })
        .map(|(c, d)| contraption.beam(c, d))
        .max()
        .unwrap();

    println!("{}", part_a);
    println!("{}", part_b);
}
