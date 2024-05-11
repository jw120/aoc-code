// Advent of Code, 2022 day 13

const std = @import("std");
const rw = @import("rw");

const ChunkTag = enum { open, close, number };

const Chunk = union(ChunkTag) {
    open: void,
    close: void,
    number: usize,
};

const ChunkIterator = struct {
    buffer: []const u8,
    index: usize,
    last: ?usize,
    next_chunk: ?Chunk,
    closes_due: usize,

    const Self = @This();

    fn next(self: *Self) !?Chunk {
        if (self.next_chunk) |next_value| {
            if (@as(ChunkTag, next_value) == ChunkTag.close) {
                if (self.closes_due == 1) {
                    self.next_chunk = null;
                } else {
                    self.closes_due -= 1;
                }
            } else {
                std.debug.assert(@as(ChunkTag, next_value) == ChunkTag.number);
                self.next_chunk = Chunk{ .close = {} };
            }
            return next_value;
        }
        if (self.index < self.buffer.len and self.buffer[self.index] == ',') {
            self.index += 1;
        }
        if (self.index >= self.buffer.len) {
            return null;
        }
        if (self.buffer[self.index] == '[') {
            self.index += 1;
            self.last = null;
            return Chunk{ .open = {} };
        }
        if (self.buffer[self.index] == ']') {
            self.index += 1;
            self.last = null;
            return Chunk{ .close = {} };
        }
        var number_of_digits: usize = 0;
        while (std.ascii.isDigit(self.buffer[self.index + number_of_digits])) {
            number_of_digits += 1;
        }
        std.debug.assert(number_of_digits >= 1);
        const value = try std.fmt.parseUnsigned(usize, self.buffer[self.index .. self.index + number_of_digits], 10);
        self.index += number_of_digits;
        self.last = value;
        return Chunk{ .number = value };
    }

    fn promote(self: *Self) void {
        self.next_chunk = Chunk{ .number = self.last orelse unreachable };
        self.closes_due += 1;
    }
};

// Return iterator over list chunks
fn chunk(s: []const u8) ChunkIterator {
    return ChunkIterator{
        .buffer = s,
        .index = 0,
        .last = null,
        .next_chunk = null,
        .closes_due = 0,
    };
}

test "chunk" {
    var iter = chunk("[1,[234],56]");
    try std.testing.expectEqualDeep(Chunk{ .open = {} }, (try iter.next()).?);
    try std.testing.expectEqualDeep(Chunk{ .number = 1 }, (try iter.next()).?);
    try std.testing.expectEqualDeep(Chunk{ .open = {} }, (try iter.next()).?);
    try std.testing.expectEqualDeep(Chunk{ .number = 234 }, (try iter.next()).?);
    try std.testing.expectEqualDeep(Chunk{ .close = {} }, (try iter.next()).?);
    try std.testing.expectEqualDeep(Chunk{ .number = 56 }, (try iter.next()).?);
    try std.testing.expectEqualDeep(Chunk{ .close = {} }, (try iter.next()).?);
    try std.testing.expect(try iter.next() == null);
}

const Comparison = enum {
    less,
    equal,
    greater,
};

const ComparisonError = error{ UnexpectedEnd, Malformed };

fn compare(a: []const u8, b: []const u8) !Comparison {
    var i: ChunkIterator = chunk(a);
    var j: ChunkIterator = chunk(b);
    var open_lists: usize = 0;

    while (true) {
        var x: Chunk = (try i.next()) orelse return ComparisonError.UnexpectedEnd;
        var y: Chunk = (try j.next()) orelse return ComparisonError.UnexpectedEnd;
        switch (x) {
            ChunkTag.open => {
                switch (y) {
                    ChunkTag.open => open_lists += 1,
                    ChunkTag.close => return Comparison.greater,
                    ChunkTag.number => j.promote(),
                }
            },
            ChunkTag.close => {
                switch (y) {
                    ChunkTag.open => return Comparison.less,
                    ChunkTag.close => if (open_lists == 1) {
                        return Comparison.equal;
                    } else {
                        open_lists -= 1;
                    },
                    ChunkTag.number => return Comparison.less,
                }
            },
            ChunkTag.number => |x_value| {
                switch (y) {
                    ChunkTag.open => i.promote(),
                    ChunkTag.close => return Comparison.greater,
                    ChunkTag.number => |y_value| {
                        if (x_value < y_value) {
                            return Comparison.less;
                        }
                        if (x_value > y_value) {
                            return Comparison.greater;
                        }
                    },
                }
            },
        }
    }
}

test "compare" {
    try std.testing.expectEqual(
        Comparison.less,
        try compare(
            "[1,1,3,1,1]",
            "[1,1,5,1,1]",
        ),
    );

    try std.testing.expectEqual(
        Comparison.less,
        try compare(
            "[[1],[2,3,4]]",
            "[[1],4]",
        ),
    );

    try std.testing.expectEqual(
        Comparison.greater,
        try compare(
            "[9]",
            "[[8,7,6]]",
        ),
    );

    try std.testing.expectEqual(
        Comparison.less,
        try compare(
            "[[4,4],4,4]",
            "[[4,4],4,4,4]",
        ),
    );

    try std.testing.expectEqual(
        Comparison.greater,
        try compare(
            "[7,7,7,7]",
            "[7,7,7]",
        ),
    );

    try std.testing.expectEqual(
        Comparison.less,
        try compare(
            "[]",
            "[3]",
        ),
    );

    try std.testing.expectEqual(
        Comparison.greater,
        try compare(
            "[[[]]]",
            "[[]]",
        ),
    );

    try std.testing.expectEqual(
        Comparison.greater,
        try compare(
            "[1,[2,[3,[4,[5,6,7]]]],8,9]",
            "[1,[2,[3,[4,[5,6,0]]]],8,9]",
        ),
    );
}

test "compare_extra" {
    try std.testing.expectEqual(
        Comparison.less,
        try compare(
            "[[8,[[2,6,0,9],[4,9,5,5,3],[8],8,3],[1]],[0,[7,[8,8,8,1]],[],[[0],[8,6,9,9,1],2,6,2]],[],[1,[2,1,[1],8],[],8],[[9,7],[7,10],[[],9,9,7,3],[2,[],6],[[],5,[7,6,7,6,0],[3],[3,10,10,10]]]]",
            "[[[[8],1,[1,1,3,5,1],[0,3]],[],4,0,5],[],[3,3,[9,4,5,[4,2,3]],7,9],[7],[[[2,1]],[[2,9,10,0,6],[3,0,8],[0,3,2,9,1]]]]",
        ),
    );
}

fn sum_ordered(lines: [][]const u8) !usize {
    var pair: usize = 1;
    var sum: usize = 0;
    while (pair * 3 - 2 < lines.len) : (pair += 1) {
        const result = try compare(lines[pair * 3 - 3], lines[pair * 3 - 2]);

        if (result != Comparison.greater) {
            sum += pair;
        }
    }
    return sum;
}

fn less_than(_: void, a: []const u8, b: []const u8) bool {
    const result = compare(a, b) catch return false;
    return result == Comparison.less;
}

fn divider_positions(allocator: std.mem.Allocator, lines: [][]const u8) !usize {

    // Copy only non-blank lines
    var packets = std.ArrayList([]const u8).init(allocator);
    defer packets.deinit();
    for (lines) |line| {
        if (line.len > 0) {
            try packets.append(line);
        }
    }

    // Add dividers
    const divider1 = "[[2]]";
    const divider2 = "[[6]]";
    try packets.append(divider1);
    try packets.append(divider2);

    std.sort.insertion([]const u8, packets.items, {}, less_than);

    var product: usize = 1;
    for (packets.items, 1..) |packet, i| {
        if (std.mem.eql(u8, packet, divider1) or std.mem.eql(u8, packet, divider2)) {
            product *= i;
        }
    }
    return product;
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

    try rw.write_int(usize, try sum_ordered(lines));
    try rw.write_int(usize, try divider_positions(allocator, lines));
}
