/// Advent of Code 2022, Day 09
const std = @import("std");
const rw = @import("rw");
const Coord = @import("coord").Coord(i64);
const Direction = @import("coord").Direction;

const ParseError = error{ InvalidCharacter, InvalidFormat } || std.fmt.ParseIntError;

const Move = struct {
    direction: Direction,
    distance: usize,
};

fn parse_row(s: []const u8) ParseError!Move {
    var iter = std.mem.splitScalar(u8, s, ' ');
    var move = Move{ .direction = .up, .distance = 0 };

    if (iter.next()) |d| {
        if (d.len == 1) {
            move.direction = try Direction.parse(d[0]);
        } else {
            return ParseError.InvalidFormat;
        }
    } else {
        return ParseError.InvalidFormat;
    }

    if (iter.next()) |i| {
        move.distance = try std.fmt.parseInt(usize, i, 10);
    } else {
        return ParseError.InvalidFormat;
    }

    return move;
}

// return new tail position
fn update(head: Coord, tail: Coord) Coord {
    const relative = tail.sub(head);
    return tail.sub(switch (relative.mag2()) {
        0, 1, 2 => Coord.origin(), // If touching distance, no move needed
        4 => relative.divTrunc(2), // If two steps left/right/up/down, then move one step
        5, 8 => Coord{ .row = std.math.sign(relative.row), .col = std.math.sign(relative.col) },
        else => unreachable,
    });
}

// return number of coords visited during given moves with two knots
fn walk2(allocator: std.mem.Allocator, moves: []Move) !usize {
    var visited = std.AutoHashMap(Coord, void).init(allocator);
    defer visited.deinit();

    var head: Coord = Coord.origin();
    var tail: Coord = Coord.origin();

    for (moves) |m| {
        for (0..m.distance) |_| {
            head = head.add(m.direction.to_coord(i64));
            tail = update(head, tail);
            try visited.put(tail, {});
        }
    }
    return visited.count();
}

// return number of coords visited during given moves with 10 knots
fn walk10(allocator: std.mem.Allocator, moves: []Move) !usize {
    var visited = std.AutoHashMap(Coord, void).init(allocator);
    defer visited.deinit();

    const n = 10;

    var knots: [n]Coord = .{Coord{ .row = 0, .col = 0 }} ** n;

    for (moves) |m| {
        for (0..m.distance) |_| {
            knots[0] = knots[0].add(m.direction.to_coord(i64));
            for (1..n) |i| {
                knots[i] = update(knots[i - 1], knots[i]);
            }
            try visited.put(knots[n - 1], {});
        }
    }
    return visited.count();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read text input lines
    const moves: []Move = try rw.read_parse_lines(Move, allocator, parse_row);
    defer allocator.free(moves);

    try rw.write_int(usize, try walk2(allocator, moves));
    try rw.write_int(usize, try walk10(allocator, moves));
}
