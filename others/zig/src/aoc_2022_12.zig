// Advent of Code, 2022 day 12

const std = @import("std");
const rw = @import("rw");
const Coord = @import("coord").Coord;
const Direction = @import("coord").Direction;
const Queue = @import("queue").Queue;

const HeightMap = struct {
    allocator: std.mem.Allocator,
    heights: [][]u8,
    rows: usize,
    cols: usize,
    start: Coord(usize),
    end: Coord(usize),
    available: std.ArrayList(Coord(usize)), // used to hold possible moves

    fn parse(allocator: std.mem.Allocator, lines: [][]const u8) !HeightMap {
        const rows: usize = lines.len;
        const cols: usize = lines[0].len;
        var start: ?Coord(usize) = null;
        var end: ?Coord(usize) = null;

        var heights: [][]u8 = try allocator.alloc([]u8, rows);
        for (lines, heights, 0..) |line, *hs, row| {
            std.debug.assert(line.len == cols);
            hs.* = try allocator.alloc(u8, cols);
            for (line, hs.*, 0..) |c, *h, col| {
                if (c == 'S') {
                    start = Coord(usize).init(row, col);
                    h.* = 'a' - 'a';
                } else if (c == 'E') {
                    end = Coord(usize).init(row, col);
                    h.* = 'z' - 'a';
                } else {
                    std.debug.assert(c >= 'a' and c <= 'z');
                    h.* = c - 'a';
                }
            }
        }

        return HeightMap{
            .allocator = allocator,
            .heights = heights,
            .rows = rows,
            .cols = cols,
            .start = start.?,
            .end = end.?,
            .available = try std.ArrayList(Coord(usize)).initCapacity(allocator, 4),
        };
    }

    fn deinit(self: HeightMap) void {
        for (self.heights) |row| {
            self.allocator.free(row);
        }
        self.allocator.free(self.heights);
        self.available.deinit();
    }

    // return length of minimum path from start to goal
    fn path_length(self: *HeightMap, forward: bool) !?usize {
        const CoordTuple = std.meta.Tuple(&.{ Coord(usize), usize });
        var queue = Queue(CoordTuple).init(self.allocator);
        defer queue.deinit();
        var visited = std.AutoHashMap(Coord(usize), void).init(self.allocator);
        defer visited.deinit();

        const path_start = if (forward) self.start else self.end;
        try queue.enqueue(.{ path_start, 0 });
        try visited.put(path_start, {});

        while (queue.dequeue()) |coord_tuple| {
            const s = coord_tuple.@"0";
            const d = coord_tuple.@"1";
            if ((forward and s.eql(self.end)) or (!forward and self.heights[s.row][s.col] == 0)) {
                return d;
            }
            try self.update_available(s, forward);
            for (self.available.items) |t| {
                if (!visited.contains(t)) {
                    try queue.enqueue(.{ t, d + 1 });
                    try visited.put(t, {});
                }
            }
        }
        return null;
    }

    // Update self.available based on valid moves from s
    fn update_available(self: *HeightMap, s: Coord(usize), forward: bool) !void {
        self.available.clearRetainingCapacity();
        for ([_]Direction{ Direction.up, Direction.right, Direction.down, Direction.left }) |d| {
            if (s.move_bounds_unsigned(d, self.rows, self.cols)) |t| {
                if ((forward and (self.heights[t.row][t.col] <= self.heights[s.row][s.col] + 1)) or
                    (!forward and (self.heights[t.row][t.col] + 1 >= self.heights[s.row][s.col])))
                {
                    try self.available.append(t);
                }
            }
        }
    }
};

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

    // Convert to heights
    var height_map = try HeightMap.parse(allocator, lines);
    defer height_map.deinit();

    try rw.write_int(usize, (try height_map.path_length(true)).?);
    try rw.write_int(usize, (try height_map.path_length(false)).?);
}
