// Advent of Code, 2022 day 11

const std = @import("std");
const rw = @import("rw");
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const OperationTag = enum { add, multiply, square };

const ParseError = error{BadInput} || std.fmt.ParseIntError;

// Return slice of string after skipping given prefix (or fail if no prefix)
fn skip_prefix(s: []const u8, prefix: []const u8) ParseError![]const u8 {
    std.debug.assert(s.len >= prefix.len);
    if (std.mem.startsWith(u8, s, prefix)) {
        return s[prefix.len..];
    }
    return ParseError.BadInput;
}

test "skip_prefix" {
    try expectEqualStrings(try skip_prefix("abcde", "abc"), "de");
}

const Operation = union(OperationTag) {
    add: u64,
    multiply: u64,
    square: void,

    fn apply(self: Operation, old: u64) u64 {
        return switch (self) {
            .add => |value| old + value,
            .multiply => |value| old * value,
            .square => old * old,
        };
    }

    fn parse(s: []const u8) ParseError!Operation {
        const rest = try skip_prefix(s, "  Operation: new = old ");
        var iter = std.mem.splitScalar(u8, rest, ' ');
        const operator = iter.next() orelse return ParseError.BadInput;
        const argument = iter.next() orelse return ParseError.BadInput;
        if (iter.next() != null) return ParseError.BadInput;
        if (std.mem.eql(u8, operator, "+")) {
            return Operation{ .add = try std.fmt.parseInt(u64, argument, 10) };
        }
        if (!std.mem.eql(u8, operator, "*")) {
            return ParseError.BadInput;
        }
        if (std.mem.eql(u8, argument, "old")) {
            return Operation{ .square = {} };
        }
        return Operation{ .multiply = try std.fmt.parseInt(u64, argument, 10) };
    }
};

test "Operation::apply" {
    const add_op = Operation{ .add = 3 };
    try expectEqual(add_op.apply(10), 13);
    const multiply_op = Operation{ .multiply = 2 };
    try expectEqual(multiply_op.apply(10), 20);
    const square_op = Operation{ .square = {} };
    try expectEqual(square_op.apply(10), 100);
}

test "Operation::parse" {
    try expectEqual(try Operation.parse("  Operation: new = old * 19"), .{ .multiply = 19 });
    try expectEqual(try Operation.parse("  Operation: new = old + 5"), .{ .add = 5 });
    try expectEqual(try Operation.parse("  Operation: new = old * old"), .{ .square = {} });
    try expectEqual(Operation.parse("NoprefixOperation: new = old * old"), ParseError.BadInput);
    try expectEqual(Operation.parse("  Operation: new = old ! old"), ParseError.BadInput);
    try expectEqual(Operation.parse("  Operation: new = old + Q"), std.fmt.ParseIntError.InvalidCharacter);
}

const Monkey = struct {
    items: std.ArrayList(usize),
    operation: Operation,
    divisor: u64,
    dest_true: usize,
    dest_false: usize,
    count: u64,
};

const ReadState = struct {
    // full monkeys read
    monkeys: std.ArrayList(Monkey),
    // partial monkey info
    header: bool,
    items: std.ArrayList(usize),
    operation: ?Operation,
    divisor: ?u64,
    dest_true: ?usize,
    dest_false: ?usize,
};

fn read_monkey_line(allocator: std.mem.Allocator, state: *ReadState, line: []const u8) (ParseError || std.mem.Allocator.Error)!void {
    if (!state.header) {
        const rest = try skip_prefix(line, "Monkey ");
        _ = rest;
        state.header = true;
    } else if (state.items.items.len == 0) {
        const rest = try skip_prefix(line, "  Starting items: ");
        var iter = std.mem.splitSequence(u8, rest, ", ");
        while (iter.next()) |item_str| {
            try state.items.append(try std.fmt.parseInt(usize, item_str, 10));
        }
    } else if (state.operation == null) {
        state.operation = try Operation.parse(line);
    } else if (state.divisor == null) {
        const rest = try skip_prefix(line, "  Test: divisible by ");
        state.divisor = try std.fmt.parseInt(u64, rest, 10);
    } else if (state.dest_true == null) {
        const rest = try skip_prefix(line, "    If true: throw to monkey ");
        state.dest_true = try std.fmt.parseInt(usize, rest, 10);
    } else if (state.dest_false == null) {
        const rest = try skip_prefix(line, "    If false: throw to monkey ");
        state.dest_false = try std.fmt.parseInt(usize, rest, 10);
    } else {
        std.debug.assert(line.len == 0); // final line must be blank
        var new_monkey = Monkey{
            .items = std.ArrayList(usize).init(allocator),
            .operation = state.operation.?,
            .divisor = state.divisor.?,
            .dest_true = state.dest_true.?,
            .dest_false = state.dest_false.?,
            .count = 0,
        };
        for (state.items.items) |item| {
            try new_monkey.items.append(item);
        }
        try state.monkeys.append(new_monkey);
        state.items.clearRetainingCapacity();
        state.header = false;
        state.operation = null;
        state.divisor = null;
        state.dest_true = null;
        state.dest_false = null;
    }
}

// Update (mutating) monkeys given number of times.
// If mode is present then we take all items modulo its value, if not
// then all items are divided by 3 (for the first part of the problem).
fn step(monkeys: []Monkey, steps: u64, mode: ?u64) !void {
    for (0..steps) |_| {
        for (monkeys) |*m| {
            for (m.*.items.items) |item| {
                var new_item: u64 = m.*.operation.apply(item);
                if (mode) |modulo_value| {
                    new_item = new_item % modulo_value;
                } else {
                    new_item /= 3;
                }
                const dest: usize = if (new_item % m.*.divisor == 0) m.*.dest_true else m.*.dest_false;
                try monkeys[dest].items.append(new_item);
                m.*.count += 1;
            }
            m.items.clearRetainingCapacity();
        }
    }
}

// Run number of steps and return most active monkeys.
// Simple mode is for first part of the problem where items are divided by 3. For
// non-simple mode we treat all numbers modulus the product of all the divisors.
fn monkey_business(monkeys: []Monkey, steps: u64, simple_mode: bool) !u64 {
    var mode: ?u64 = null;
    if (!simple_mode) {
        var modulo_value: u64 = 1;
        for (monkeys) |m| {
            modulo_value *= m.divisor;
        }
        mode = modulo_value;
    }

    try step(monkeys, steps, mode);

    var count1: u64 = 0; // highest count monkey
    var count2: u64 = 0; // second highest count monkey
    for (monkeys) |m| {
        if (m.count > count1) {
            count2 = count1;
            count1 = m.count;
        } else if (m.count > count2) {
            count2 = m.count;
        }
    }
    return count1 * count2;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read text input lines
    var read_state = ReadState{
        .monkeys = std.ArrayList(Monkey).init(allocator),
        .header = false,
        .items = std.ArrayList(usize).init(allocator),
        .operation = null,
        .divisor = null,
        .dest_true = null,
        .dest_false = null,
    };
    defer {
        read_state.monkeys.deinit();
        read_state.items.deinit();
    }
    try rw.fold_lines(ReadState, allocator, &read_state, read_monkey_line);
    defer {
        for (read_state.monkeys.items) |m| {
            m.items.deinit();
        }
    }
    // If there is an unfinished monkey, finish it by passing a blank line
    if (read_state.dest_false != null) {
        try read_monkey_line(allocator, &read_state, "");
    }

    // Stash a copy of the monkeys as they are mutated by running
    var monkeys_copy: []Monkey = try allocator.alloc(Monkey, read_state.monkeys.items.len);
    defer allocator.free(monkeys_copy);
    for (read_state.monkeys.items, monkeys_copy) |original, *copy| {
        copy.* = original;
        copy.*.items = try original.items.clone();
    }
    defer {
        for (monkeys_copy) |m| {
            m.items.deinit();
        }
    }

    try rw.write_int(u64, try monkey_business(monkeys_copy, 20, true));
    try rw.write_int(u64, try monkey_business(read_state.monkeys.items, 10000, false));
}
