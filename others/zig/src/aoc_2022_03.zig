/// Advent of Code 2022, Day 03
const std = @import("std");
const rw = @import("rw");

// Errors from our input parsing code
const ParseError = error{InvalidChar};

// Hold our rucksack as a bit set
const RuckSack = std.bit_set.IntegerBitSet(53);

const Input = struct {
    left: RuckSack,
    right: RuckSack,
    whole: RuckSack,

    pub fn parse(s: []const u8) ParseError!Input {
        var left = RuckSack.initEmpty();
        var right = RuckSack.initEmpty();
        var whole = RuckSack.initEmpty();
        std.debug.assert(s.len % 2 == 0);
        for (s, 0..) |c, i| {
            const b = switch (c) {
                'a'...'z' => c - 'a',
                'A'...'Z' => c - 'A' + 26,
                else => return ParseError.InvalidChar,
            };
            whole.set(b);
            if (i < s.len / 2) {
                left.set(b);
            } else {
                right.set(b);
            }
        }
        return Input{ .left = left, .right = right, .whole = whole };
    }
};

// Return priority of only item in a rucksack
fn sole_priority(r: RuckSack) usize {
    std.debug.assert(r.count() == 1);
    return r.findFirstSet().? + 1;
}

// Part (a) solution
fn part_a(inputs: []Input) usize {
    var total: usize = 0;
    for (inputs) |input| {
        total += sole_priority(input.left.intersectWith(input.right));
    }
    return total;
}

// Part (b) solution
fn part_b(inputs: []Input) usize {
    var total: usize = 0;
    var i: usize = 0;
    std.debug.assert(inputs.len % 3 == 0);
    while (i < inputs.len) {
        total += sole_priority(inputs[i].whole.intersectWith(inputs[i + 1].whole).intersectWith(inputs[i + 2].whole));
        i += 3;
    }
    return total;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const inputs: []Input = try rw.read_parse_lines(Input, allocator, Input.parse);
    defer allocator.free(inputs);

    try rw.write_int(usize, part_a(inputs));
    try rw.write_int(usize, part_b(inputs));
}
