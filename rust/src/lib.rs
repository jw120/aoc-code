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

#[must_use]
pub fn coord(row: i32, col: i32) -> Coord {
    Coord { row, col }
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

#[must_use]
pub fn ucoord(row: usize, col: usize) -> UCoord {
    UCoord { row, col }
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
        let row_diff: usize = self.row.abs_diff(other.row);
        let col_diff: usize = self.col.abs_diff(other.col);
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

// 3-d coordinate type

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub struct Coord3 {
    pub x: i32,
    pub y: i32,
    pub z: i32,
}

#[must_use]
pub fn coord3(x: i32, y: i32, z: i32) -> Coord3 {
    Coord3 { x, y, z }
}

impl Coord3 {
    #[must_use]
    pub fn mag2(&self) -> i32 {
        self.x * self.x + self.y * self.y
    }

    #[must_use]
    pub fn origin() -> Coord3 {
        Coord3 { x: 0, y: 0, z: 0 }
    }

    #[must_use]
    pub fn div(&self, d: i32) -> Coord3 {
        Coord3 {
            x: self.x / d,
            y: self.y / d,
            z: self.z / d,
        }
    }

    #[must_use]
    pub fn signum(&self) -> Coord3 {
        Coord3 {
            x: self.x.signum(),
            y: self.y.signum(),
            z: self.z.signum(),
        }
    }

    #[must_use]
    pub fn manhattan(&self, other: &Coord3) -> i32 {
        (self.x - other.x).abs() + (self.y - other.y).abs() + (self.z - other.z).abs()
    }
}

impl Add for Coord3 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl AddAssign for Coord3 {
    fn add_assign(&mut self, other: Self) {
        *self = Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        };
    }
}

impl Sub for Coord3 {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
}

impl SubAssign for Coord3 {
    fn sub_assign(&mut self, other: Self) {
        *self = Self {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        };
    }
}

// 3-d unsigned coordinate type

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub struct UCoord3 {
    pub x: usize,
    pub y: usize,
    pub z: usize,
}

#[must_use]
pub fn ucoord3(x: usize, y: usize, z: usize) -> UCoord3 {
    UCoord3 { x, y, z }
}

impl UCoord3 {
    #[must_use]
    pub fn mag2(&self) -> usize {
        self.x * self.x + self.y * self.y
    }

    #[must_use]
    pub fn origin() -> UCoord3 {
        UCoord3 { x: 0, y: 0, z: 0 }
    }

    #[must_use]
    pub fn div(&self, d: usize) -> UCoord3 {
        UCoord3 {
            x: self.x / d,
            y: self.y / d,
            z: self.z / d,
        }
    }

    #[must_use]
    pub fn manhattan(&self, other: &UCoord3) -> usize {
        let x_diff: usize = self.x.abs_diff(other.x);
        let y_diff: usize = self.y.abs_diff(other.y);
        let z_diff: usize = self.z.abs_diff(other.z);
        x_diff + y_diff + z_diff
    }
}

impl Add for UCoord3 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl AddAssign for UCoord3 {
    fn add_assign(&mut self, other: Self) {
        *self = Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        };
    }
}

impl Sub for UCoord3 {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
}

impl SubAssign for UCoord3 {
    fn sub_assign(&mut self, other: Self) {
        *self = Self {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        };
    }
}
