// Advent of Code 2024 - Day 6.

use aoc_rust::{ucoord, UCoord};
use grid::Grid;
use std::collections::HashSet;
use std::io;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

fn turn(d: &Direction) -> Direction {
    match d {
        Direction::Up => Direction::Right,
        Direction::Right => Direction::Down,
        Direction::Down => Direction::Left,
        Direction::Left => Direction::Up,
    }
}

fn apply(c: &UCoord, d: &Direction) -> Option<UCoord> {
    match d {
        Direction::Left => c.col.checked_sub(1).map(|col| ucoord(c.row, col)),
        Direction::Down => Some(ucoord(c.row + 1, c.col)),
        Direction::Right => Some(ucoord(c.row, c.col + 1)),
        Direction::Up => c.row.checked_sub(1).map(|row| ucoord(row, c.col)),
    }
}

fn read_grid() -> (Grid<bool>, UCoord) {
    let mut start: Option<UCoord> = None;
    let mut grid: Grid<bool> = Grid::new(0, 0);
    for (row, raw_line) in io::stdin().lines().enumerate() {
        let line = raw_line.unwrap();
        let line = line.trim();
        for (col, ch) in line.chars().enumerate() {
            if ch == '^' {
                assert!(start.is_none());
                start = Some(ucoord(row, col));
            }
        }
        grid.push_row(line.chars().map(|c| c == '#').collect());
    }
    (grid, start.unwrap_or_else(|| panic!("No start position")))
}

fn walk_till_exit(grid: &Grid<bool>, start: &UCoord) -> Grid<bool> {
    let mut position: UCoord = *start;
    let mut direction: Direction = Direction::Up;
    let mut visited: Grid<bool> = Grid::new(grid.rows(), grid.cols());
    loop {
        visited[(position.row, position.col)] = true;
        if let Some(next_position) = apply(&position, &direction) {
            if let Some(next_square) = grid.get(next_position.row, next_position.col) {
                if *next_square {
                    direction = turn(&direction);
                } else {
                    position = next_position;
                }
            } else {
                break;
            }
        } else {
            break;
        }
    }
    visited
}

fn walk_till_loop(grid: &Grid<bool>, extra: &UCoord, start: &UCoord) -> bool {
    if extra == start {
        return false;
    }
    let mut position: UCoord = *start;
    let mut direction: Direction = Direction::Up;
    let mut visited: HashSet<(UCoord, Direction)> = HashSet::new();
    loop {
        if visited.contains(&(position, direction)) {
            return true;
        }
        visited.insert((position, direction));
        if let Some(next_position) = apply(&position, &direction) {
            if let Some(next_square) = grid.get(next_position.row, next_position.col) {
                if *next_square || next_position == *extra {
                    direction = turn(&direction);
                } else {
                    position = next_position;
                }
                continue;
            }
        }
        return false;
    }
}

fn count_looping_barriers(grid: &Grid<bool>, visited: &Grid<bool>, start: &UCoord) -> usize {
    visited
        .indexed_iter()
        .filter(|((row, col), square)| **square && walk_till_loop(grid, &ucoord(*row, *col), start))
        .count()
}

fn main() {
    let (grid, start) = read_grid();
    let visited = walk_till_exit(&grid, &start);
    println!("{}", visited.iter().filter(|b| **b).count());
    println!("{}", count_looping_barriers(&grid, &visited, &start));
}
