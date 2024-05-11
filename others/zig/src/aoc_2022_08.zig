/// Advent of Code 2022, Day 08
const std = @import("std");
const rw = @import("rw");

// Create 2-d array of ints from 2-d array of digits
fn to_heights(allocator: std.mem.Allocator, css: [][]const u8) ![][]u64 {
    const rows: usize = css.len;
    const cols: usize = css[0].len;

    const hss: [][]u64 = try allocator.alloc([]u64, rows);

    for (css, hss) |line, *hs| {
        std.debug.assert(line.len == cols);
        hs.* = try allocator.alloc(u64, cols);
        for (line, hs.*) |c, *h| {
            std.debug.assert(c >= '0' and c <= '9');
            h.* = c - '0';
        }
    }
    return hss;
}

// Count number of visible trees
fn count_visible(allocator: std.mem.Allocator, heights: [][]const u64) !usize {
    const rows: usize = heights.len;
    const cols: usize = heights[0].len;

    // Start with all trees visible
    const vss: [][]bool = try allocator.alloc([]bool, rows);
    defer free_2d(bool, allocator, vss);
    for (vss) |*vs| {
        vs.* = try allocator.alloc(bool, cols);
        for (vs.*) |*v| {
            v.* = false;
        }
    }

    // Reveal trees in each row
    for (heights, vss) |row, vs| {
        var max_lr: u64 = row[0];
        var max_rl: u64 = row[cols - 1];
        vs[0] = true;
        vs[cols - 1] = true;
        for (1..cols) |j| {
            if (row[j] > max_lr) {
                vs[j] = true;
                max_lr = row[j];
            }
            if (row[cols - 1 - j] > max_rl) {
                vs[cols - 1 - j] = true;
                max_rl = row[cols - 1 - j];
            }
        }
    }

    // Reveal trees in each column
    for (0..cols) |j| {
        var max_ud: u64 = heights[0][j];
        var max_du: u64 = heights[rows - 1][j];
        vss[0][j] = true;
        vss[cols - 1][j] = true;
        for (1..rows) |i| {
            if (heights[i][j] > max_ud) {
                vss[i][j] = true;
                max_ud = heights[i][j];
            }
            if (heights[rows - 1 - i][j] > max_du) {
                vss[rows - 1 - i][j] = true;
                max_du = heights[rows - 1 - i][j];
            }
        }
    }

    // Count visible trees
    var count: usize = 0;
    for (vss) |vs| {
        for (vs) |v| {
            count += @intFromBool(v);
        }
    }
    return count;
}

fn scenic(heights: [][]const u64, x: usize, y: usize) usize {
    const rows: usize = heights.len;
    const cols: usize = heights[0].len;

    // Edge trees are not scenic
    if (x == 0 or x == rows - 1 or y == 0 or y == cols - 1) {
        return 0;
    }

    const h: u64 = heights[x][y];

    var score: usize = 1;

    // look up
    var d: usize = 1;
    while (x > d and heights[x - d][y] < h) {
        d += 1;
    }
    score *= d;

    // look down
    d = 1;
    while (x + d + 1 < rows and heights[x + d][y] < h) {
        d += 1;
    }
    score *= d;

    // look left
    d = 1;
    while (y > d and heights[x][y - d] < h) {
        d += 1;
    }
    score *= d;

    // look right
    d = 1;
    while (y + d + 1 < cols and heights[x][y + d] < h) {
        d += 1;
    }
    score *= d;

    return score;
}

// Return most scenic tree
fn most_scenic(heights: [][]const u64) u64 {
    const rows: usize = heights.len;
    const cols: usize = heights[0].len;

    var best_value: usize = 0;

    for (0..rows) |i| {
        for (0..cols) |j| {
            const scenic_value: u64 = scenic(heights, i, j);
            if (scenic_value > best_value) {
                best_value = scenic_value;
            }
        }
    }

    return best_value;
}

// Free a 2-d array
fn free_2d(comptime T: type, allocator: std.mem.Allocator, xxs: [][]const T) void {
    for (xxs) |xs| {
        allocator.free(xs);
    }
    allocator.free(xxs);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read text input lines
    const lines: [][]const u8 = try rw.read_lines(allocator);
    defer free_2d(u8, allocator, lines);

    // Set up 2-d array with height
    const heights: [][]const u64 = try to_heights(allocator, lines);
    defer free_2d(u64, allocator, heights);

    try rw.write_int(usize, try count_visible(allocator, heights));
    try rw.write_int(usize, most_scenic(heights));
}
