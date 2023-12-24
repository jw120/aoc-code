// Advent of Code, 2022 day 8

use aoc_rust::stdin_lines;
use grid::Grid;
use itertools::Itertools;

// Create grid of heights from stdin lines
fn height_from_stdin() -> Grid<u8> {
    let ss: Vec<String> = stdin_lines().collect();
    let cols = ss[0].len();
    let rows = ss.len();
    let mut grid = Grid::new(rows, cols);

    for (r, row) in ss.iter().enumerate() {
        for (c, ch) in row.chars().enumerate() {
            let i: u32 = ch.into();
            let zero_ord: u32 = '0'.into();
            assert!(i >= '0'.into() && i <= '9'.into());
            grid[(r, c)] = u8::try_from(i - zero_ord).unwrap();
        }
    }
    grid
}

// Count all the visible trees
fn count_visible(heights: &Grid<u8>) -> usize {
    // start with all trees invisible
    let (rows, cols) = heights.size();
    let mut visible = Grid::new(rows, cols); // defaults to false

    // Reveal trees in each row
    for r in 0..rows {
        visible[(r, 0)] = true;
        visible[(r, cols - 1)] = true;
        let mut max_lr = heights[(r, 0)];
        let mut max_rl = heights[(r, cols - 1)];
        for c in 1..cols - 1 {
            let height_lr = heights[(r, c)];
            if height_lr > max_lr {
                max_lr = height_lr;
                visible[(r, c)] = true;
            }
            let c_rl = cols - 1 - c;
            let height_rl = heights[(r, c_rl)];
            if height_rl > max_rl {
                max_rl = height_rl;
                visible[(r, c_rl)] = true;
            }
        }
    }

    // Reveal trees in each column
    for c in 0..cols {
        visible[(0, c)] = true;
        visible[(rows - 1, c)] = true;
        let mut max_ud = heights[(0, c)];
        let mut max_du = heights[(rows - 1, c)];
        for r in 1..rows - 1 {
            let height_ud = heights[(r, c)];
            if height_ud > max_ud {
                max_ud = height_ud;
                visible[(r, c)] = true;
            }
            let r_du = rows - 1 - r;
            let height_du = heights[(r_du, c)];
            if height_du > max_du {
                max_du = height_du;
                visible[(r_du, c)] = true;
            }
        }
    }

    visible.iter().filter(|v| **v).count()
}

fn scenic(heights: &Grid<u8>, r: usize, c: usize) -> usize {
    let (rows, cols) = heights.size();

    // Edge trees are not scenic
    if r == 0 || r == rows - 1 || c == 0 || c == cols - 1 {
        return 0;
    }

    let h = heights[(r, c)];
    let mut score = 1;

    // look up
    let mut d: usize = 1;
    while r > d && heights[(r - d, c)] < h {
        d += 1;
    }
    score *= d;

    // look down
    d = 1;
    while r + d + 1 < rows && heights[(r + d, c)] < h {
        d += 1;
    }
    score *= d;

    // look left
    d = 1;
    while c > d && heights[(r, c - d)] < h {
        d += 1;
    }
    score *= d;

    // look right
    d = 1;
    while c + d + 1 < cols && heights[(r, c + d)] < h {
        d += 1;
    }
    score *= d;

    score
}

fn most_scenic(heights: &Grid<u8>) -> usize {
    let (rows, cols) = heights.size();
    (0..rows)
        .cartesian_product(0..cols)
        .map(|(r, c)| scenic(heights, r, c))
        .max()
        .unwrap()
}

fn main() {
    let heights = height_from_stdin();

    println!("{}", count_visible(&heights));
    println!("{}", most_scenic(&heights));
}
