use std::io;
use std::ops::{Add, AddAssign, Sub, SubAssign};

fn unwrap_result(r: Result<String, io::Error>) -> String {
    r.unwrap()
}

// Return iterator to lines in stdin as Strings
pub fn stdin_lines() -> impl Iterator<Item = String> {
    io::stdin().lines().map(unwrap_result)
}

// 2-d coordinate type

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub struct Coord {
    pub row: i32,
    pub col: i32,
}

impl Coord {
    #[must_use]
    pub fn mag2(&self) -> i32 {
        self.row * self.row + self.col * self.col
    }

    #[must_use]
    pub fn origin() -> Coord {
        Coord { row: 0, col: 0 }
    }

    #[must_use]
    pub fn div(&self, z: i32) -> Coord {
        Coord {
            row: self.row / z,
            col: self.col / z,
        }
    }

    #[must_use]
    pub fn signum(&self) -> Coord {
        Coord {
            row: self.row.signum(),
            col: self.col.signum(),
        }
    }

    #[must_use]
    pub fn manhattan(&self, other: &Coord) -> i32 {
        (self.row - other.row).abs() + (self.col - other.col).abs()
    }
}

impl Add for Coord {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            row: self.row + other.row,
            col: self.col + other.col,
        }
    }
}

impl AddAssign for Coord {
    fn add_assign(&mut self, other: Self) {
        *self = Self {
            row: self.row + other.row,
            col: self.col + other.col,
        };
    }
}

impl Sub for Coord {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            row: self.row - other.row,
            col: self.col - other.col,
        }
    }
}

impl SubAssign for Coord {
    fn sub_assign(&mut self, other: Self) {
        *self = Self {
            row: self.row - other.row,
            col: self.col - other.col,
        };
    }
}

// 2-d coordinate type with usize

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub struct UCoord {
    pub row: usize,
    pub col: usize,
}

impl UCoord {
    #[must_use]
    pub fn mag2(&self) -> usize {
        self.row * self.row + self.col * self.col
    }

    #[must_use]
    pub fn origin() -> UCoord {
        UCoord { row: 0, col: 0 }
    }

    #[must_use]
    pub fn div(&self, z: usize) -> UCoord {
        UCoord {
            row: self.row / z,
            col: self.col / z,
        }
    }

    #[must_use]
    pub fn manhattan(&self, other: &UCoord) -> usize {
        let row_diff: usize = if self.row > other.row {
            self.row - other.row
        } else {
            other.row - self.row
        };
        let col_diff: usize = if self.col > other.col {
            self.col - other.col
        } else {
            other.col - self.col
        };
        row_diff + col_diff
    }
}

impl Add for UCoord {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            row: self.row + other.row,
            col: self.col + other.col,
        }
    }
}

impl AddAssign for UCoord {
    fn add_assign(&mut self, other: Self) {
        *self = Self {
            row: self.row + other.row,
            col: self.col + other.col,
        };
    }
}

impl Sub for UCoord {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            row: self.row - other.row,
            col: self.col - other.col,
        }
    }
}

impl SubAssign for UCoord {
    fn sub_assign(&mut self, other: Self) {
        *self = Self {
            row: self.row - other.row,
            col: self.col - other.col,
        };
    }
}
