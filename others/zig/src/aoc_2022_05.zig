/// Advent of Code 2022, Day 05
const std = @import("std");
const rw = @import("rw");

// Errors from our input parsing code
const ParseError = error{InvalidMove} || std.fmt.ParseIntError;

const Stack = std.ArrayList(u8);

const Move = struct {
    quantity: usize,
    from: usize, // 0-index
    to: usize, // 0-index
    fn parse(s: []const u8) ParseError!Move {
        var iter = std.mem.splitScalar(u8, s, ' ');

        var word = iter.next() orelse return ParseError.InvalidMove;
        if (!std.mem.eql(u8, word, "move")) {
            return ParseError.InvalidMove;
        }

        word = iter.next() orelse return ParseError.InvalidMove;
        const quantity = try std.fmt.parseInt(usize, word, 10);

        word = iter.next() orelse return ParseError.InvalidMove;
        if (!std.mem.eql(u8, word, "from")) {
            return ParseError.InvalidMove;
        }

        word = iter.next() orelse return ParseError.InvalidMove;
        const from = try std.fmt.parseInt(usize, word, 10);

        word = iter.next() orelse return ParseError.InvalidMove;
        if (!std.mem.eql(u8, word, "to")) {
            return ParseError.InvalidMove;
        }

        word = iter.next() orelse return ParseError.InvalidMove;
        const to = try std.fmt.parseInt(usize, word, 10);

        if (iter.next() != null) return ParseError.InvalidMove;
        return Move{ .quantity = quantity, .from = from - 1, .to = to - 1 };
    }
    fn eql(self: Move, other: Move) bool {
        return self.quantity == other.quantity and self.from == other.from and self.to == other.to;
    }
};

test "Move.parse" {
    const actual = try Move.parse("move 1 from 2 to 3");
    const expected = Move{ .quantity = 1, .from = 1, .to = 2 };
    try std.testing.expect(actual.eql(expected));
}

/// Pare the input lines
fn parse(allocator: std.mem.Allocator, lines: [][]const u8) !struct { []Stack, []Move } {
    // Find the blank dividing line
    var blank_line_index: usize = 0;
    while (lines[blank_line_index].len > 0) {
        blank_line_index += 1;
    }

    // Get number of stacks from line before blank line
    var stack_number: usize = 0;
    var number_iter = std.mem.tokenizeScalar(u8, lines[blank_line_index - 1], ' ');
    while (number_iter.next() != null) {
        stack_number += 1;
    }
    const stacks: []Stack = try allocator.alloc(Stack, stack_number);
    for (stacks) |*stack_ptr| {
        stack_ptr.* = std.ArrayList(u8).init(allocator);
    }

    // Add to stacks by reading lines before blank line in reverse order
    for (0..blank_line_index - 1) |i| {
        for (0..stack_number) |stack| {
            const c = lines[blank_line_index - 2 - i][1 + stack * 4];
            if (c != ' ') {
                try stacks[stack].append(c);
            }
        }
    }

    // Read moves
    const move_number: usize = lines.len - blank_line_index - 1;
    const moves: []Move = try allocator.alloc(Move, move_number);
    for (moves, lines[blank_line_index + 1 .. lines.len]) |*move_ptr, line| {
        move_ptr.* = try Move.parse(line);
    }
    return .{ stacks, moves };
}

/// Return contents of top of each stack
fn stack_tops(allocator: std.mem.Allocator, stacks: []Stack) ![]u8 {
    const s = try allocator.alloc(u8, stacks.len);
    for (0..stacks.len) |i| {
        s[i] = stacks[i].getLast();
    }
    return s;
}

// Run the simulation using part (a) method
fn run_a(stacks: []Stack, moves: []Move) !void {
    for (moves) |move| {
        for (0..move.quantity) |_| {
            const c = stacks[move.from].pop();
            try stacks[move.to].append(c);
        }
    }
}

// Run the simulation using part (b) method
fn run_b(stacks: []Stack, moves: []Move) !void {
    for (moves) |move| {
        std.debug.assert(move.from != move.to);
        for (0..move.quantity) |i| {
            const c = stacks[move.from].items[stacks[move.from].items.len - move.quantity + i];
            try stacks[move.to].append(c);
        }
        try stacks[move.from].resize(stacks[move.from].items.len - move.quantity);
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    // Read all the input as strings
    const lines = try rw.read_lines(allocator);
    defer allocator.free(lines);
    defer {
        for (lines) |line| {
            allocator.free(line);
        }
    }

    // Parse the input lines into stacks and moves
    const parsed = try parse(allocator, lines);
    const stacks = parsed[0];
    defer allocator.free(stacks);
    defer {
        for (stacks) |stack| {
            stack.deinit();
        }
    }
    const moves = parsed[1];
    defer allocator.free(moves);

    // Stash a copy of the stacks (as running is destructive)
    const stacks_copy = try allocator.alloc(Stack, stacks.len);
    defer allocator.free(stacks_copy);
    for (stacks, 0..) |stack, i| {
        stacks_copy[i] = std.ArrayList(u8).init(allocator);
        try stacks_copy[i].appendSlice(stack.items);
    }
    defer {
        for (stacks_copy) |stack| {
            stack.deinit();
        }
    }

    // Run stacks with part a version
    try run_a(stacks, moves);
    const stack_tops_a = try stack_tops(allocator, stacks);
    defer allocator.free(stack_tops_a);

    try run_b(stacks_copy, moves);
    const stack_tops_b = try stack_tops(allocator, stacks_copy);
    defer allocator.free(stack_tops_b);

    try rw.write_str(stack_tops_a);
    try rw.write_str(stack_tops_b);
}
