/// Advent of Code 2022, Day 06
const std = @import("std");
const rw = @import("rw");

fn find_header(datastream: []const u8, package_length: u8) usize {
    var counts: [26]u8 = [_]u8{0} ** 26;
    var dupe_count: u8 = 0;

    for (0..datastream.len) |i| {
        const incoming = datastream[i] - 'a';
        counts[incoming] += 1;
        if (counts[incoming] > 1) {
            dupe_count += 1;
        }

        if (i >= package_length) {
            const outgoing = datastream[i - package_length] - 'a';
            counts[outgoing] -= 1;
            if (counts[outgoing] > 0) {
                dupe_count -= 1;
            }
            if (dupe_count == 0) {
                return i + 1;
            }
        }
    }
    unreachable;
}

test "a examples" {
    try std.testing.expect(find_header("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4) == 7);
    try std.testing.expect(find_header("bvwbjplbgvbhsrlpgdmjqwftvncz", 4) == 5);
    try std.testing.expect(find_header("nppdvjthqldpwncqszvftbrmjlhg", 4) == 6);
    try std.testing.expect(find_header("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4) == 10);
    try std.testing.expect(find_header("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4) == 11);
}

test "b examples" {
    try std.testing.expect(find_header("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14) == 19);
    try std.testing.expect(find_header("bvwbjplbgvbhsrlpgdmjqwftvncz", 14) == 23);
    try std.testing.expect(find_header("nppdvjthqldpwncqszvftbrmjlhg", 14) == 23);
    try std.testing.expect(find_header("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14) == 29);
    try std.testing.expect(find_header("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14) == 26);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const datastream = try rw.read_line(allocator);
    defer allocator.free(datastream);

    try rw.write_int(usize, find_header(datastream, 4));
    try rw.write_int(usize, find_header(datastream, 14));
}
