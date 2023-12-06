// Advent of Code, 2022 day 14

use aoc_rust::stdin_lines;
use aoc_rust::UCoord;
use grid::Grid;
use std::iter::zip;

// We add sand in two modes
#[derive(Clone, Copy, PartialEq)]

enum AddSandMode {
    UntilOverflow,
    UntilReachTop,
}

#[derive(Clone, Copy, Default, PartialEq)]
enum Square {
    #[default]
    Empty,
    Sand,
    Rock,
}

// Start point must be at row 0, col START_COL
const START_COL: usize = 500;

struct Board {
    rock_row_max: usize, // lowest non-floor rock
    row_max: usize,      // floor row
    col_min: usize,
    cols: usize,
    finished: bool,
    grid: Grid<Square>,
}

impl Board {
    fn new(lines: &[String]) -> Board {
        let paths: Vec<Vec<UCoord>> = lines.iter().map(|s| parse_line(s)).collect();

        // Find extent of rocks
        let mut rock_row_min: usize = 0;
        let mut rock_row_max: usize = 0;
        let mut rock_col_min: usize = START_COL;
        let mut rock_col_max: usize = START_COL;
        for path in &paths {
            for c in path {
                rock_row_min = rock_row_min.min(c.row);
                rock_row_max = rock_row_max.max(c.row);
                rock_col_min = rock_col_min.min(c.col);
                rock_col_max = rock_col_max.max(c.col);
            }
        }
        assert!(rock_row_min == 0);

        // Define extent of board grid
        let row_max: usize = rock_row_max + 2;
        let rows: usize = row_max - rock_row_min + 1;
        let col_delta: usize = rows + 1;
        let col_min: usize = rock_col_min - col_delta;
        let col_max: usize = rock_col_max + col_delta;
        let cols: usize = col_max - col_min + 1;

        // Create board
        let mut board = Board {
            rock_row_max,
            row_max,
            col_min,
            cols,
            finished: false,
            grid: Grid::new(rows, cols),
        };

        // Fill board grid with rocks
        for path in paths {
            for (c1, c2) in zip(path.iter(), path[1..].iter()) {
                board.add_rock_line(c1, c2);
            }
        }
        // Add rocks to the floor
        for c in col_min..=col_max {
            board.set(
                UCoord {
                    row: row_max,
                    col: c,
                },
                Square::Rock,
            );
        }
        board
    }

    fn add(&mut self, add_sand_mode: AddSandMode) {
        let mut c = UCoord {
            row: 0,
            col: START_COL,
        };
        assert!(self.get(c) == Square::Empty);
        loop {
            // for part (a), finish when overflow lowest rock
            if add_sand_mode == AddSandMode::UntilOverflow && c.row >= self.rock_row_max {
                self.finished = true;
                return;
            }
            // Check down
            let mut d = c + UCoord { row: 1, col: 0 };
            if self.get(d) == Square::Empty {
                c = d;
                continue;
            }
            // Check down-left
            d -= UCoord { row: 0, col: 1 };
            if self.get(d) == Square::Empty {
                c = d;
                continue;
            }
            // Check down-right
            d += UCoord { row: 0, col: 2 };
            if self.get(d) == Square::Empty {
                c = d;
                continue;
            }
            // Settle
            self.set(c, Square::Sand);
            // for part(b), finish when reach top
            if add_sand_mode == AddSandMode::UntilReachTop && c.row == 0 {
                self.finished = true;
            }
            break;
        }
    }

    fn add_until_overflow(&mut self, add_sand_mode: AddSandMode) -> usize {
        let mut sand_count: usize = 0;
        self.finished = false;
        while !self.finished {
            self.add(add_sand_mode);
            sand_count += 1;
        }
        sand_count
    }

    // Add rocks to all points between the two coordinates (inclusive)
    fn add_rock_line(&mut self, c1: &UCoord, c2: &UCoord) {
        if c2.col > c1.col {
            assert!(c2.row == c1.row);
            for c in c1.col..=c2.col {
                self.set(
                    UCoord {
                        row: c1.row,
                        col: c,
                    },
                    Square::Rock,
                );
            }
        } else if c2.col < c1.col {
            assert!(c2.row == c1.row);
            for c in c2.col..=c1.col {
                self.set(
                    UCoord {
                        row: c1.row,
                        col: c,
                    },
                    Square::Rock,
                );
            }
        } else if c2.row > c1.row {
            assert!(c2.col == c1.col);
            for r in c1.row..=c2.row {
                self.set(
                    UCoord {
                        row: r,
                        col: c1.col,
                    },
                    Square::Rock,
                );
            }
        } else {
            assert!(c2.row < c1.row);
            assert!(c2.col == c1.col);
            for r in c2.row..=c1.row {
                self.set(
                    UCoord {
                        row: r,
                        col: c1.col,
                    },
                    Square::Rock,
                );
            }
        }
    }

    fn get(&self, c: UCoord) -> Square {
        assert!(c.row <= self.row_max);
        assert!(c.col >= self.col_min);
        assert!(c.col < self.col_min + self.cols);
        self.grid[c.row][c.col - self.col_min]
    }

    fn set(&mut self, c: UCoord, value: Square) {
        assert!(c.row <= self.row_max);
        assert!(c.col >= self.col_min);
        assert!(c.col < self.col_min + self.cols);
        self.grid[c.row][c.col - self.col_min] = value;
    }
}

fn parse_coord(s: &str) -> UCoord {
    let mut iter = s.split(',');
    let word1 = iter.next().unwrap();
    let word2 = iter.next().unwrap();
    assert!(iter.next().is_none());
    UCoord {
        row: word2.parse().unwrap(),
        col: word1.parse().unwrap(),
    }
}

fn parse_line(s: &str) -> Vec<UCoord> {
    s.split(" -> ").map(parse_coord).collect()
}

fn main() {
    let lines: Vec<String> = stdin_lines().collect();
    let mut board = Board::new(&lines);
    let overflow_sand = board.add_until_overflow(AddSandMode::UntilOverflow) - 1;
    println!("{overflow_sand}");
    let extra_sand = board.add_until_overflow(AddSandMode::UntilReachTop);
    println!("{}", overflow_sand + extra_sand);
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse_coord() {
        assert_eq!(parse_coord("497,67"), UCoord { row: 67, col: 497 });
    }
}
