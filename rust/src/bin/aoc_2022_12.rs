// Advent of Code, 2022 day 12

use aoc_rust::stdin_lines;
use aoc_rust::UCoord;
use grid::Grid;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;

// Read height map, start and end from stdin
fn read_height_map(lines: &Vec<String>) -> (Grid<u8>, UCoord, UCoord) {
    let rows = lines.len();
    let cols = lines[0].len();
    let mut grid: Grid<u8> = Grid::new(rows, cols);
    let mut start: Option<UCoord> = None;
    let mut end: Option<UCoord> = None;
    for (row, line) in lines.iter().enumerate() {
        for (col, ch) in line.chars().enumerate() {
            let height = match ch {
                'S' => {
                    start = Some(UCoord { row, col });
                    0
                }
                'E' => {
                    end = Some(UCoord { row, col });
                    25
                }
                _ => (ch as u8) - b'a',
            };
            assert!(height < 26);
            grid[(row, col)] = height;
        }
    }
    (grid, start.unwrap(), end.unwrap())
}

// Conduct basic BFS search and return length of minimum path.
fn bfs<S: Copy + Eq + Hash>(
    start: S,
    at_goal: impl Fn(S) -> bool,
    available: impl Fn(S) -> Vec<S>,
) -> Option<usize> {
    let mut queue: VecDeque<(S, usize)> = VecDeque::from([(start, 0)]);
    let mut visited: HashSet<S> = HashSet::from([start]);
    while !queue.is_empty() {
        let (s, dist) = queue.pop_front().unwrap();
        if at_goal(s) {
            return Some(dist);
        }
        for t in available(s) {
            if !visited.contains(&t) {
                queue.push_back((t, dist + 1));
                visited.insert(t);
            }
        }
    }
    None
}

// Return coords reachable forward from given state
fn available_forward(heights: &Grid<u8>, s: UCoord) -> Vec<UCoord> {
    let mut reachable: Vec<UCoord> = Vec::new();
    let rows = heights.rows();
    let cols = heights.cols();
    let h = heights[(s.row, s.col)];
    if (s.row + 1 < rows) && (heights[(s.row + 1, s.col)] <= h + 1) {
        reachable.push(UCoord {
            row: s.row + 1,
            col: s.col,
        });
    }
    if (s.row > 0) && (heights[(s.row - 1, s.col)] <= h + 1) {
        reachable.push(UCoord {
            row: s.row - 1,
            col: s.col,
        });
    }
    if (s.col + 1 < cols) && (heights[(s.row, s.col + 1)] <= h + 1) {
        reachable.push(UCoord {
            row: s.row,
            col: s.col + 1,
        });
    }
    if (s.col > 0) && (heights[(s.row, s.col - 1)] <= h + 1) {
        reachable.push(UCoord {
            row: s.row,
            col: s.col - 1,
        });
    }
    reachable
}

// Return coords reachable forward from given state
fn available_reverse(heights: &Grid<u8>, s: UCoord) -> Vec<UCoord> {
    let mut reachable: Vec<UCoord> = Vec::new();
    let rows = heights.rows();
    let cols = heights.cols();
    let h = heights[(s.row, s.col)];
    if (s.row + 1 < rows) && (heights[(s.row + 1, s.col)] >= h - 1) {
        reachable.push(UCoord {
            row: s.row + 1,
            col: s.col,
        });
    }
    if (s.row > 0) && (heights[(s.row - 1, s.col)] >= h - 1) {
        reachable.push(UCoord {
            row: s.row - 1,
            col: s.col,
        });
    }
    if (s.col + 1 < cols) && (heights[(s.row, s.col + 1)] >= h - 1) {
        reachable.push(UCoord {
            row: s.row,
            col: s.col + 1,
        });
    }
    if (s.col > 0) && (heights[(s.row, s.col - 1)] >= h - 1) {
        reachable.push(UCoord {
            row: s.row,
            col: s.col - 1,
        });
    }
    reachable
}

fn main() {
    let lines: Vec<String> = stdin_lines().collect();
    let (heights, start, end) = read_height_map(&lines);

    println!(
        "{}",
        bfs(start, |s| s == end, |s| available_forward(&heights, s)).unwrap()
    );
    println!(
        "{}",
        bfs(
            end,
            |s| heights[(s.row, s.col)] == 0,
            |s| available_reverse(&heights, s)
        )
        .unwrap()
    );
}
