// Advent of Code 2024 - Day 6.

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

fn apply((start_row, start_col): (usize, usize), d: &Direction) -> Option<(usize, usize)> {
    match d {
        Direction::Left => start_col.checked_sub(1).map(|col| (start_row, col)),
        Direction::Down => Some((start_row + 1, start_col)),
        Direction::Right => Some((start_row, start_col + 1)),
        Direction::Up => start_row.checked_sub(1).map(|row| (row, start_col)),
    }
}

fn read_grid() -> (Grid<bool>, (usize, usize)) {
    let mut start: Option<(usize, usize)> = None;
    let mut grid: Grid<bool> = Grid::new(0, 0);
    for (row, raw_line) in io::stdin().lines().enumerate() {
        let line = raw_line.unwrap();
        let line = line.trim();
        for (col, ch) in line.chars().enumerate() {
            if ch == '^' {
                assert!(start.is_none());
                start = Some((row, col));
            }
        }
        grid.push_row(line.chars().map(|c| c == '#').collect());
    }
    (grid, start.unwrap_or_else(|| panic!("No start position")))
}

fn walk_till_exit(grid: &Grid<bool>, start: (usize, usize)) -> Grid<bool> {
    let mut position: (usize, usize) = start;
    let mut direction: Direction = Direction::Up;
    let mut visited: Grid<bool> = Grid::new(grid.rows(), grid.cols());
    loop {
        visited[position] = true;
        if let Some(next_position) = apply(position, &direction) {
            if let Some(next_square) = grid.get(next_position.0, next_position.1) {
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

fn walk_till_loop(grid: &Grid<bool>, extra: (usize, usize), start: (usize, usize)) -> bool {
    if extra == start {
        return false;
    }
    let mut position: (usize, usize) = start;
    let mut direction: Direction = Direction::Up;
    let mut visited: HashSet<((usize, usize), Direction)> = HashSet::new();
    loop {
        if visited.contains(&(position, direction)) {
            return true;
        }
        visited.insert((position, direction));
        if let Some(next_position) = apply(position, &direction) {
            if let Some(next_square) = grid.get(next_position.0, next_position.1) {
                if *next_square || next_position == extra {
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

fn count_looping_barriers(grid: &Grid<bool>, visited: &Grid<bool>, start: (usize, usize)) -> usize {
    visited
        .indexed_iter()
        .filter(|((row, col), square)| **square && walk_till_loop(grid, (*row, *col), start))
        .count()
}

fn main() {
    let (grid, start) = read_grid();
    let visited = walk_till_exit(&grid, start);
    println!("{}", visited.iter().filter(|b| **b).count());
    println!("{}", count_looping_barriers(&grid, &visited, start));
}
