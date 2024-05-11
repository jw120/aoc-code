/// Support code for reading/writing Advent of Code problems
const std = @import("std");

// Used for multi-line functions
const line_buffer_length = 255;

// Used for single-line functions
const single_line_buffer_length = 32767;

/// Read and parse lines from stdin. Returns owned slice.
/// Ignores text after final new line.
pub fn read_parse_lines(comptime T: type, allocator: std.mem.Allocator, parse: fn (s: []const u8) anyerror!T) ![]T {
    const stdin_reader: std.fs.File.Reader = std.io.getStdIn().reader();

    // Initialise our list (and deinit it if we have an error)
    var list = std.ArrayList(T).init(allocator);
    errdefer list.deinit();

    // Set up a buffer to hold each line of input
    var line_buffer: [line_buffer_length]u8 = undefined;
    var line_buffer_stream = std.io.fixedBufferStream(&line_buffer);
    var line_buffer_writer = line_buffer_stream.writer();

    // Loop over lines, (resetting line_buffer each line)
    while (true) : (line_buffer_stream.reset()) {

        // Read a line into our buffer
        stdin_reader.streamUntilDelimiter(line_buffer_writer, '\n', null) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        // Remove any trailing CR
        const line: []const u8 = std.mem.trimRight(u8, line_buffer_stream.getWritten(), "\r");

        // Parse the line
        const x: T = try parse(line);

        // Add to list
        try list.append(x);
    }
    return list.toOwnedSlice();
}

/// Apply function to each line and some state, return final state
/// Allows final line to be EOF-terminated
pub fn fold_lines(comptime T: type, allocator: std.mem.Allocator, state: *T, process_line: fn (a: std.mem.Allocator, s: *T, l: []const u8) anyerror!void) !void {
    const stdin_reader: std.fs.File.Reader = std.io.getStdIn().reader();

    // Set up a buffer to hold each line of input
    var line_buffer: [line_buffer_length]u8 = undefined;
    var line_buffer_stream = std.io.fixedBufferStream(&line_buffer);
    var line_buffer_writer = line_buffer_stream.writer();

    // Loop over lines, (resetting line_buffer each line)
    var found_eof: bool = false;
    while (!found_eof) : (line_buffer_stream.reset()) {

        // Read a line into our buffer
        stdin_reader.streamUntilDelimiter(line_buffer_writer, '\n', null) catch |err| switch (err) {
            error.EndOfStream => found_eof = true,
            else => return err,
        };

        // If file ends with new-line and EOF, don't process the empty line after the last new-line
        if (found_eof and try line_buffer_stream.getPos() == 0) {
            break;
        }

        // Remove any trailing CR
        const line: []const u8 = std.mem.trimRight(u8, line_buffer_stream.getWritten(), "\r");

        // Copy the line
        try process_line(allocator, state, line);
    }
}

/// Read lines from stdin adding to array list.
/// Allows final line to be EOF-terminated
pub fn read_lines_array_list(allocator: std.mem.Allocator, list: *std.ArrayList([]const u8)) !void {
    const stdin_reader: std.fs.File.Reader = std.io.getStdIn().reader();

    // Set up a buffer to hold each line of input
    var line_buffer: [line_buffer_length]u8 = undefined;
    var line_buffer_stream = std.io.fixedBufferStream(&line_buffer);
    var line_buffer_writer = line_buffer_stream.writer();

    // Loop over lines, (resetting line_buffer each line)
    var found_eof: bool = false;
    while (!found_eof) : (line_buffer_stream.reset()) {

        // Read a line into our buffer
        stdin_reader.streamUntilDelimiter(line_buffer_writer, '\n', null) catch |err| switch (err) {
            error.EndOfStream => found_eof = true,
            else => return err,
        };

        // If file ends with new-line and EOF, don't process the empty line after the last new-line
        if (found_eof and try line_buffer_stream.getPos() == 0) {
            break;
        }

        // Remove any trailing CR
        const line: []const u8 = std.mem.trimRight(u8, line_buffer_stream.getWritten(), "\r");

        // Copy the line
        const line_copy: []const u8 = try std.mem.Allocator.dupe(allocator, u8, line);

        // Add to list
        try list.append(line_copy);
    }
}

// Read lines from stdin. Returns owned slice with owned strings.
/// Allows final line to be EOF-terminated
pub fn read_lines(allocator: std.mem.Allocator) ![][]const u8 {
    var list = std.ArrayList([]const u8).init(allocator);
    try read_lines_array_list(allocator, &list);
    return list.toOwnedSlice();
}

/// Read single line from stdin. Returns owned string
/// Accepts end-of-file as well as newline as terminator
pub fn read_line(allocator: std.mem.Allocator) ![]const u8 {
    const stdin_reader: std.fs.File.Reader = std.io.getStdIn().reader();

    // Set up a buffer to hold each line of input
    var line_buffer: [single_line_buffer_length]u8 = undefined;
    var line_buffer_stream = std.io.fixedBufferStream(&line_buffer);
    var line_buffer_writer = line_buffer_stream.writer();

    stdin_reader.streamUntilDelimiter(line_buffer_writer, '\n', null) catch |err| if (err != error.EndOfStream) return err;
    const line: []const u8 = std.mem.trimRight(u8, line_buffer_stream.getWritten(), "\r");
    return std.mem.Allocator.dupe(allocator, u8, line);
}

/// Write integer on one line to stdout.
pub fn write_int(comptime T: type, x: T) !void {
    const stdout_writer = std.io.getStdOut().writer();
    try stdout_writer.print("{d}\n", .{x});
}

/// Write string on one line to stdout.
pub fn write_str(s: []const u8) !void {
    const stdout_writer = std.io.getStdOut().writer();
    try stdout_writer.print("{s}\n", .{s});
}

/// Write a character to stdout (without a new-line)
pub fn write_char(c: u8) !void {
    const stdout_writer = std.io.getStdOut().writer();
    try stdout_writer.print("{c}", .{c});
}
