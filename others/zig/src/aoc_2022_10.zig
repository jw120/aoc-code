/// Advent of Code 2022, Day 10
const std = @import("std");
const rw = @import("rw");

const InstructionTag = enum { addx, noop };

const ParseError = error{ UnknownInstruction, InvalidFormat } || std.fmt.ParseIntError;

const Instruction = union(InstructionTag) {
    addx: i64,
    noop: void,

    fn parse(s: []const u8) ParseError!Instruction {
        var iter = std.mem.splitScalar(u8, s, ' ');
        if (iter.next()) |word1| {
            if (std.mem.eql(u8, word1, "noop")) {
                return Instruction{ .noop = {} };
            }
            if (std.mem.eql(u8, word1, "addx")) {
                if (iter.next()) |word2| {
                    return Instruction{ .addx = try std.fmt.parseInt(i64, word2, 10) };
                } else {
                    return ParseError.InvalidFormat;
                }
            }
            return ParseError.UnknownInstruction;
        }
        return ParseError.InvalidFormat;
    }
};

fn run(allocator: std.mem.Allocator, instructions: []const Instruction) !struct { i64, []const u8 } {
    var cycle: i64 = 1;
    var signal_strength: i64 = 0;
    var x: i64 = 1;
    var crt = std.ArrayList(u8).init(allocator);

    for (instructions) |ins| {
        const cycles_needed: usize = switch (ins) {
            .noop => 1,
            .addx => |_| 2,
        };
        const x_delta: i64 = switch (ins) {
            .noop => 0,
            .addx => |delta| delta,
        };
        for (0..cycles_needed) |_| {
            if (@mod(cycle - 20, 40) == 0) {
                signal_strength += cycle * x;
            }
            const crt_position: i64 = @mod(cycle - 1, 40);
            const crt_diff: i64 = try std.math.absInt(crt_position - x);
            try crt.append(if (crt_diff <= 1) '#' else '.');
            cycle += 1;
        }
        x += x_delta;
    }
    return .{ signal_strength, try crt.toOwnedSlice() };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read text input lines
    const instructions: []Instruction = try rw.read_parse_lines(Instruction, allocator, Instruction.parse);
    defer allocator.free(instructions);

    const result = try run(allocator, instructions);
    const signal = result[0];
    const crt = result[1];
    defer allocator.free(crt);

    try rw.write_int(i64, signal);

    for (crt, 1..) |c, i| {
        try rw.write_char(c);
        if (@mod(i, 40) == 0) {
            try rw.write_char('\n');
        }
    }
}
