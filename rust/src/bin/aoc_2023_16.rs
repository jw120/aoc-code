// Advent of Code, 2023 day 16

#![allow(clippy::upper_case_acronyms)]

use aoc_rust::{stdin_lines, UCoord};
use grid::{grid, Grid};
use std::collections::HashSet;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
enum Direction {
    N,
    E,
    S,
    W,
}

fn step<X>(
    position: UCoord,
    direction: Direction,
    grid: &Grid<X>,
    first_step: &mut bool,
) -> Option<UCoord> {
    if *first_step {
        *first_step = false;
        return Some(UCoord { row: 0, col: 0 });
    }
    match direction {
        Direction::N => {
            if position.row > 0 {
                Some(position - UCoord { row: 1, col: 0 })
            } else {
                None
            }
        }
        Direction::E => {
            if position.col < grid.cols() - 1 {
                Some(position + UCoord { row: 0, col: 1 })
            } else {
                None
            }
        }
        Direction::S => {
            if position.row < grid.rows() - 1 {
                Some(position + UCoord { row: 1, col: 0 })
            } else {
                None
            }
        }
        Direction::W => {
            if position.col > 0 {
                Some(position - UCoord { row: 0, col: 1 })
            } else {
                None
            }
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

fn read_grid() -> Grid<Square> {
    let mut grid: Grid<Square> = grid![];
    for line in stdin_lines() {
        let row_vec: Vec<Square> = line.chars().map(Square::parse).collect();
        grid.push_row(row_vec);
    }
    grid
}

fn beam(grid: &Grid<Square>) -> usize {
    let mut beams: Vec<(UCoord, Direction)> = vec![(UCoord { row: 0, col: 0 }, Direction::E)];
    let mut visited: Grid<HashSet<Direction>> = Grid::new(grid.rows(), grid.cols());
    let mut first_step: bool = true; // Ugly hack to fix first step starting at (0, -1)
    while let Some((position, direction)) = beams.pop() {
        if !first_step && !visited[(position.row, position.col)].insert(direction) {
            continue;
        }

        if let Some(next_position) = step(position, direction, grid, &mut first_step) {
            let next_square: Square = grid[(next_position.row, next_position.col)];
            let next_direction: Direction = match (next_square, direction) {
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
    }

    visited.iter().filter(|s| !s.is_empty()).count()
}

fn main() {
    let grid: Grid<Square> = read_grid();

    let part_b: usize = grid.cols();

    println!("{}", beam(&grid));
    println!("{}", part_b);
}
