// Advent of Code, 2022 day 15

const std = @import("std");
const rw = @import("rw");
const Coord = @import("coord").Coord(i64);

const ParseError = error{BadInput};

const Zone = struct {
    beacons: std.ArrayList(Coord),
    sensors: std.ArrayList(Coord),
    col_min: i64,
    col_max: i64,

    const Self = @This();

    fn init(allocator: std.mem.Allocator) Self {
        var sensors = std.ArrayList(Coord).init(allocator);
        var beacons = std.ArrayList(Coord).init(allocator);
        return Zone{
            .beacons = beacons,
            .sensors = sensors,
            .col_min = 0,
            .col_max = 0,
        };
    }

    fn deinit(self: *Self) void {
        self.beacons.deinit();
        self.sensors.deinit();
    }

    fn add_line(self: *Self, s: []const u8) !void {
        var iter = std.mem.splitAny(u8, s, "=,:");
        std.debug.assert(std.mem.eql(u8, iter.next() orelse return ParseError.BadInput, "Sensor at x"));
        const sensor_x: i64 = try std.fmt.parseInt(i64, iter.next() orelse return ParseError.BadInput, 10);
        std.debug.assert(std.mem.eql(u8, iter.next() orelse return ParseError.BadInput, " y"));
        const sensor_y: i64 = try std.fmt.parseInt(i64, iter.next() orelse return ParseError.BadInput, 10);
        std.debug.assert(std.mem.eql(u8, iter.next() orelse return ParseError.BadInput, " closest beacon is at x"));
        const beacon_x: i64 = try std.fmt.parseInt(i64, iter.next() orelse return ParseError.BadInput, 10);
        std.debug.assert(std.mem.eql(u8, iter.next() orelse return ParseError.BadInput, " y"));
        const beacon_y: i64 = try std.fmt.parseInt(i64, iter.next() orelse return ParseError.BadInput, 10);
        std.debug.assert(iter.next() == null);

        const sensor = Coord.init(sensor_y, sensor_x);
        const beacon = Coord.init(beacon_y, beacon_x);
        const d: i64 = sensor.sub(beacon).dist();
        const col_min: i64 = sensor.col - d;
        const col_max: i64 = sensor.col + d;

        // std.debug.print("col range {d} {d}\n", .{ col_min, col_max });

        if (self.sensors.items.len == 0) {
            self.col_min = col_min;
            self.col_max = col_max;
        } else {
            self.col_min = @min(self.col_min, col_min);
            self.col_max = @max(self.col_max, col_max);
        }

        try self.sensors.append(sensor);
        try self.beacons.append(beacon);
    }

    fn must_be_empty(self: *Self, test_beacon: Coord) bool {
        for (self.sensors.items, self.beacons.items) |sensor, closest_beacon| {
            if (test_beacon.eql(closest_beacon)) {
                return false;
            }
            const closest_beacon_distance = sensor.sub(closest_beacon).dist();
            const test_beacon_distance = sensor.sub(test_beacon).dist();
            if (test_beacon_distance <= closest_beacon_distance) {
                // std.debug.print("true {?:}\n", .{test_beacon});
                return true;
            }
        }
        // std.debug.print("false {?:}\n", .{test_beacon});
        return false;
    }

    fn empty_in_row(self: *Self, y: i64) usize {
        var count: usize = 0;
        var col: i64 = self.col_min;
        while (col < self.col_max) : (col += 1) {
            if (self.must_be_empty(Coord.init(y, col))) {
                count += 1;
            }
        }
        return count;
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

    // Parse
    var zone = Zone.init(allocator);
    defer zone.deinit();
    for (lines) |line| {
        try zone.add_line(line);
    }
    // std.debug.print("overall col range {d} {d}\n", .{ zone.col_min, zone.col_max });

    for (zone.sensors.items, zone.beacons.items) |sensor, beacon| {
        std.debug.print("{?:} {?:}\n", .{ sensor, beacon });
    }

    try rw.write_int(usize, zone.empty_in_row(2000000));
}
