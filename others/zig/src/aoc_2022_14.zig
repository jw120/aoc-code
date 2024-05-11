// Advent of Code, 2022 day 14

const std = @import("std");
const rw = @import("rw");
const Coord = @import("coord").Coord(i64);

const Square = enum { empty, sand, rock };

const Grid = struct {
    rock_row_max: i64, // lowest non-floor rock
    row_max: i64, // floor row
    rows: usize,
    col_min: i64,
    cols: usize,
    data: []Square,
    allocator: std.mem.Allocator,
    finished: bool,

    const start = Coord.init(0, 500);

    fn get(self: *Grid, c: Coord) Square {
        std.debug.assert(c.row >= Grid.start.row);
        std.debug.assert(c.col >= self.col_min);
        const row_diff: usize = @intCast(c.row - Grid.start.row);
        const col_diff: usize = @intCast(c.col - self.col_min);
        std.debug.assert(row_diff < self.rows);
        std.debug.assert(col_diff < self.cols);
        return self.data[row_diff * self.cols + col_diff];
    }

    fn set(self: *Grid, c: Coord, value: Square) void {
        const row_diff: usize = @intCast(c.row - Grid.start.row);
        const col_diff: usize = @intCast(c.col - self.col_min);
        self.data[row_diff * self.cols + col_diff] = value;
    }

    fn init(allocator: std.mem.Allocator, lines: [][]const u8) !Grid {

        // Convert text lines to paths
        var paths = std.ArrayList([]Coord).init(allocator);
        for (lines) |line| {
            try paths.append(try parse_line(allocator, line));
        }
        defer {
            for (paths.items) |path| {
                allocator.free(path);
            }
            paths.deinit();
        }

        // Find extent of board
        var rock_row_min: i64 = Grid.start.row;
        var rock_row_max: i64 = Grid.start.row;
        var rock_col_min: i64 = Grid.start.col;
        var rock_col_max: i64 = Grid.start.col;
        for (paths.items) |path| {
            for (path) |c| {
                rock_row_min = @min(rock_row_min, c.row);
                rock_row_max = @max(rock_row_max, c.row);
                rock_col_min = @min(rock_col_min, c.col);
                rock_col_max = @max(rock_col_max, c.col);
            }
        }
        std.debug.assert(rock_row_min == Grid.start.row);
        const row_max: i64 = rock_row_max + 2;
        const rows: usize = @intCast(row_max - rock_row_min + 1);
        const col_delta: i64 = @intCast(rows + 1);
        const col_min: i64 = rock_col_min - col_delta;
        const col_max: i64 = rock_col_max + col_delta;
        const cols: usize = @intCast(col_max - col_min + 1);

        // Create board
        var grid = Grid{
            .rock_row_max = rock_row_max,
            .row_max = row_max,
            .rows = rows,
            .col_min = col_min,
            .cols = cols,
            .data = try allocator.alloc(Square, rows * cols),
            .finished = false,
            .allocator = allocator,
        };

        // Fill board
        for (grid.data) |*d| {
            d.* = Square.empty;
        }
        for (paths.items) |path| {
            for (path[0 .. path.len - 1], path[1..path.len]) |c1, c2| {
                grid.add_rock_line(c1, c2);
            }
        }
        var c = col_min;
        while (c <= col_max) {
            grid.set(Coord.init(row_max, c), Square.rock);
            c += 1;
        }

        return grid;
    }

    fn deinit(self: *Grid) void {
        self.allocator.free(self.data);
    }

    fn add_rock_line(self: *Grid, a: Coord, b: Coord) void {
        const delta = b.sub(a).sign();
        var c = a;
        while (true) {
            self.set(c, Square.rock);
            if (c.eql(b)) {
                break;
            } else {
                c = c.add(delta);
            }
        }
    }

    // Add one unit of sand
    fn add(self: *Grid, floor: bool) void {
        var c = Grid.start;
        std.debug.assert(self.get(c) == Square.empty);
        while (true) {
            if (!floor and c.row >= self.rock_row_max) {
                self.finished = true;
                break;
            }
            var d = c.add(Coord.init(1, 0));
            if (self.get(d) == Square.empty) {
                c = d;
                continue;
            }
            d = c.add(Coord.init(1, -1));
            if (self.get(d) == Square.empty) {
                c = d;
                continue;
            }
            d = c.add(Coord.init(1, 1));
            if (self.get(d) == Square.empty) {
                c = d;
                continue;
            }
            self.set(c, Square.sand);
            if (floor and c.eql(Grid.start)) {
                self.finished = true;
                break;
            }
            break;
        }
    }
};

fn parse_coord(s: []const u8) !Coord {
    var iter = std.mem.splitScalar(u8, s, ',');
    const word1 = iter.next().?;
    const word2 = iter.next().?;
    std.debug.assert(iter.next() == null);
    return Coord{
        .row = try std.fmt.parseInt(i64, word2, 10),
        .col = try std.fmt.parseInt(i64, word1, 10),
    };
}

fn parse_line(allocator: std.mem.Allocator, s: []const u8) ![]Coord {
    var iter = std.mem.splitSequence(u8, s, " -> ");
    var list = std.ArrayList(Coord).init(allocator);
    while (iter.next()) |word| {
        try list.append(try parse_coord(word));
    }
    return list.toOwnedSlice();
}

fn add_until_overflow(grid: *Grid, floor: bool) usize {
    var sand_count: usize = 0;
    grid.finished = false;
    while (!grid.finished) {
        grid.add(floor);
        sand_count += 1;
    }
    return sand_count;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read text input lines
    const lines: [][]const u8 = try rw.read_lines(allocator);
    defer {
        for (lines) |line| {
            allocator.free(line);
        }
        allocator.free(lines);
    }

    var grid = try Grid.init(allocator, lines);
    defer grid.deinit();

    const overflow_sand: usize = add_until_overflow(&grid, false) - 1;
    try rw.write_int(usize, overflow_sand);
    const extra_sand: usize = add_until_overflow(&grid, true);
    try rw.write_int(usize, overflow_sand + extra_sand);
}
