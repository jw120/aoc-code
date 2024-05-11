// 2-d coordinate

const std = @import("std");
const expect = std.testing.expect;

pub const Direction = enum(i8) {
    up,
    right,
    down,
    left,

    pub fn to_coord(self: Direction, comptime T: type) Coord(T) {
        return switch (self) {
            .up => Coord(T){ .row = 0, .col = -1 },
            .right => Coord(T){ .row = 1, .col = 0 },
            .down => Coord(T){ .row = 0, .col = 1 },
            .left => Coord(T){ .row = -1, .col = 0 },
        };
    }

    pub fn parse(c: u8) !Direction {
        return switch (c) {
            'U' => .up,
            'R' => .right,
            'D' => .down,
            'L' => .left,
            else => error.InvalidCharacter,
        };
    }
};

pub fn Coord(comptime T: type) type {
    return struct {
        const This = @This();
        row: T,
        col: T,

        pub fn init(row: T, col: T) This {
            return This{ .row = row, .col = col };
        }

        /// Value-equality
        pub fn eql(self: This, other: This) bool {
            return self.row == other.row and self.col == other.col;
        }

        /// Addition
        pub fn add(self: This, other: This) This {
            return This{ .row = self.row + other.row, .col = self.col + other.col };
        }

        /// Subtraction
        pub fn sub(self: This, other: This) This {
            return This{ .row = self.row - other.row, .col = self.col - other.col };
        }

        /// Division
        pub fn divTrunc(self: This, scale: T) This {
            return This{ .row = @divTrunc(self.row, scale), .col = @divTrunc(self.col, scale) };
        }

        /// Sign
        pub fn sign(self: This) This {
            return This{ .row = std.math.sign(self.row), .col = std.math.sign(self.col) };
        }

        /// Origin
        pub fn origin() This {
            return This{ .row = 0, .col = 0 };
        }

        /// Magnitude-squared
        pub fn mag2(self: This) T {
            return self.row * self.row + self.col * self.col;
        }

        // Manhattan distance
        pub fn dist(self: This) T {
            return (if (self.row > 0) self.row else -self.row) + (if (self.col > 0) self.col else -self.col);
        }

        // Move one step in given direction
        pub fn move(self: This, d: Direction) This {
            return self.add(d.to_coord(T));
        }

        // Move one step in given direction checking within bounds
        pub fn move_bounds(self: This, d: Direction, min: This, max: This) ?This {
            return switch (d) {
                .up => if (self.row > min.row) This{ .row = self.row - 1, .col = self.col } else null,
                .down => if (self.row + 1 < max.row) This{ .row = self.row + 1, .col = self.col } else null,
                .right => if (self.col > min.col) This{ .row = self.row, .col = self.col - 1 } else null,
                .left => if (self.col + 1 < max.col) This{ .row = self.row, .col = self.col + 1 } else null,
            };
        }
        // Move one step in given direction checking within bounds
        pub fn move_bounds_unsigned(self: This, d: Direction, rows: T, cols: T) ?This {
            return switch (d) {
                .up => if (self.row > 0) This{ .row = self.row - 1, .col = self.col } else null,
                .down => if (self.row + 1 < rows) This{ .row = self.row + 1, .col = self.col } else null,
                .right => if (self.col > 0) This{ .row = self.row, .col = self.col - 1 } else null,
                .left => if (self.col + 1 < cols) This{ .row = self.row, .col = self.col + 1 } else null,
            };
        }
    };
}

const test_coord1 = Coord(i64){ .row = 2, .col = 3 };
const test_coord2 = Coord(i64){ .row = -3, .col = -4 };

test "eql" {
    try expect(test_coord1.eql(test_coord1));
    try expect(!test_coord1.eql(test_coord2));
}

test "add" {
    try expect(test_coord1.add(test_coord2).eql(Coord{ .row = -1, .col = -1 }));
}

test "sub" {
    try expect(test_coord1.sub(test_coord2).eql(Coord{ .row = 5, .col = 7 }));
}

test "divTrunc" {
    try expect(test_coord1.divTrunc(2).eql(Coord{ .row = 1, .col = 1 }));
    try expect(test_coord2.divTrunc(2).eql(Coord{ .row = -1, .col = -2 }));
}

test "origin" {
    try expect(Coord.origin().eql(Coord{ .row = 0, .col = 0 }));
}

test "mag2" {
    try expect(test_coord1.mag2() == 13);
    try expect(test_coord2.mag2() == 25);
}

test "dist" {
    try expect(test_coord1.dist() == 5);
    try expect(test_coord2.dist() == 7);
}
