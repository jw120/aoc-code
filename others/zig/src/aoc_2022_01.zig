/// Advent of Code 2022, Day 01
const std = @import("std");
const rw = @import("rw");

// Parse input lines as an optional int (null for blank lines)
fn parse_line(s: []const u8) std.fmt.ParseIntError!?i64 {
    return if (s.len == 0) null else try std.fmt.parseInt(i64, s, 10);
}

// Calculate sums of list of i64s with null as separator
fn to_sums(allocator: std.mem.Allocator, xs: []const ?i64) ![]i64 {
    var list: std.ArrayList(i64) = std.ArrayList(i64).init(allocator);
    errdefer list.deinit();
    var sum: i64 = 0;
    var sum_open = false;
    for (xs) |x| {
        if (x) |value| {
            sum += value;
            sum_open = true;
        } else {
            try list.append(sum);
            sum = 0;
            sum_open = false;
        }
    }
    if (sum_open) {
        try list.append(sum);
    }
    return list.toOwnedSlice();
}

test "to_sums with null-termination of final sum" {
    const input = [_]?i64{ 1, 2, null, 3, 4, null, 5, 6, null };
    const output = try to_sums(std.testing.allocator, &input);
    defer std.testing.allocator.free(output);
    const expected = [_]i64{ 3, 7, 11 };
    for (output, expected) |x, y| {
        try std.testing.expect(x == y);
    }
}

test "to_sums without null-termination of final sum" {
    const input = [_]?i64{ 1, 2, null, 3, 4, null, null, 5, 8 };
    const output = try to_sums(std.testing.allocator, &input);
    defer std.testing.allocator.free(output);
    const expected = [_]i64{ 3, 7, 0, 13 };
    for (output, expected) |x, y| {
        try std.testing.expect(x == y);
    }
}

test "to_sums with empty slice" {
    const input = [_]?i64{};
    const output = try to_sums(std.testing.allocator, &input);
    defer std.testing.allocator.free(output);
    try std.testing.expect(output.len == 0);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    // Read optional numbers
    const numbers: []const ?i64 = try rw.read_parse_lines(?i64, allocator, parse_line);
    defer allocator.free(numbers);

    // Create list of their sums
    const sums: []i64 = try to_sums(allocator, numbers);
    defer allocator.free(sums);

    // Sort in-place
    std.sort.heap(i64, sums, {}, std.sort.desc(i64));

    if (sums.len >= 1) {
        try rw.write_int(i64, sums[0]);
    }
    if (sums.len >= 3) {
        try rw.write_int(i64, sums[0] + sums[1] + sums[2]);
    }
}
