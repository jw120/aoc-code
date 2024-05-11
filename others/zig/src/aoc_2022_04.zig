/// Advent of Code 2022, Day 04
const std = @import("std");
const rw = @import("rw");

// Errors from our input parsing code
const ParseError = error{ InvalidChar, InvalidPart, InvalidRange } || std.fmt.ParseIntError;

// Parse a integer-range "123-456"
fn parse_range(s: []const u8) ParseError!struct { i64, i64 } {
    var iter = std.mem.splitScalar(u8, s, '-');
    const s_lo = iter.next() orelse return ParseError.InvalidRange;
    const lo = try std.fmt.parseInt(i64, s_lo, 10);
    const s_hi = iter.next() orelse return ParseError.InvalidRange;
    const hi = try std.fmt.parseInt(i64, s_hi, 10);
    return if (iter.next() == null) .{ lo, hi } else ParseError.InvalidRange;
}

const Assignment = struct {
    lo1: i64,
    hi1: i64,
    lo2: i64,
    hi2: i64,
    pub fn parse(s: []const u8) ParseError!Assignment {
        var iter = std.mem.splitScalar(u8, s, ',');
        const s1 = iter.next() orelse return ParseError.InvalidPart;
        const range1 = try parse_range(s1);
        const s2 = iter.next() orelse return ParseError.InvalidPart;
        const range2 = try parse_range(s2);
        if (iter.next() == null) {
            return Assignment{
                .lo1 = range1[0],
                .hi1 = range1[1],
                .lo2 = range2[0],
                .hi2 = range2[1],
            };
        } else {
            return ParseError.InvalidPart;
        }
    }
    // Does one range contain the other
    fn contains(self: Assignment) bool {
        return (self.lo1 <= self.lo2 and self.hi1 >= self.hi2) or (self.lo2 <= self.lo1 and self.hi2 >= self.hi1);
    }
    // Does one range overlap the other
    fn overlaps(self: Assignment) bool {
        return !(self.hi1 < self.lo2 or self.hi2 < self.lo1);
    }
};

// Part (a) solution
fn part_a(inputs: []Assignment) i64 {
    var total: i64 = 0;
    for (inputs) |input| {
        total += @intFromBool(input.contains());
    }
    return total;
}

// Part (b) solution
fn part_b(inputs: []Assignment) i64 {
    var total: i64 = 0;
    for (inputs) |input| {
        total += @intFromBool(input.overlaps());
    }
    return total;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const assignments: []Assignment = try rw.read_parse_lines(Assignment, allocator, Assignment.parse);
    defer allocator.free(assignments);

    try rw.write_int(i64, part_a(assignments));
    try rw.write_int(i64, part_b(assignments));
}
