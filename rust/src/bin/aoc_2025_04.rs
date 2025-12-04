// Advent of Code, 2025 day XX

use std::{cmp, io};

use grid::Grid;

fn read_grid(lines: &[String]) -> Grid<bool> {
    let mut g = Grid::new(lines.len(), lines[0].len());
    for (r, row) in lines.iter().enumerate() {
        for (c, ch) in row.trim().chars().enumerate() {
            *g.get_mut(r, c).unwrap() = match ch {
                '@' => true,
                '.' => false,
                _ => panic!("Bad char '{ch}'"),
            }
        }
    }
    g
}

fn _print_grid(grid: &Grid<bool>) {
    for r in 0..grid.rows() {
        for c in 0..grid.cols() {
            print!("{}", if grid[(r, c)] { '@' } else { '.' });
        }
        println!();
    }
}

fn _print_grid_accessible(grid: &Grid<bool>) {
    for r in 0..grid.rows() {
        for c in 0..grid.cols() {
            let ch = match neighbours(grid, r, c) {
                Some(n) => {
                    if n < 4 {
                        'x'
                    } else {
                        '@'
                    }
                }
                None => '.',
            };
            print!("{ch}");
        }
        println!();
    }
}

fn neighbours(grid: &Grid<bool>, r: usize, c: usize) -> Option<usize> {
    if !grid[(r, c)] {
        return None;
    }
    let r_lo = r.saturating_sub(1);
    let r_hi = cmp::min(r + 2, grid.rows());
    let c_lo = c.saturating_sub(1);
    let c_hi = cmp::min(c + 2, grid.cols());
    let mut count: usize = 0;
    // println!("neighbours ({r}, {c})");
    for p in r_lo..r_hi {
        for q in c_lo..c_hi {
            // print!("({p}, {q})");
            if (p, q) != (r, c) && grid[(p, q)] {
                count += 1;
                // print!("X");
            }
            // println!();
        }
    }
    // println!("({r},{c}) {count}");
    Some(count)
}

fn part_a(grid: &Grid<bool>) -> usize {
    grid.indexed_iter()
        .filter_map(|((r, c), _)| neighbours(grid, r, c))
        .filter(|n| *n < 4)
        .count()
}

fn main() {
    let lines: Vec<String> = io::stdin().lines().map(|s| s.unwrap()).collect();
    let grid = read_grid(&lines);
    // print_grid(&grid);

    println!("{}", part_a(&grid));
    //    print_grid_accessible(&grid);
}
