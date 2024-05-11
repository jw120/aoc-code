/// Advent of Code 2022, Day 07
const std = @import("std");
const rw = @import("rw");

// Types to hold directory information we read in

const ItemTag = enum { file, dir };

const Item = union(ItemTag) {
    file: usize,
    dir: *Dir,
};

const Dir = struct {
    items: std.StringHashMap(Item),
    parent: ?*Dir,

    /// Allocate and initialize a Dir
    fn init(allocator: std.mem.Allocator, parent: ?*Dir) !*Dir {
        const d: *Dir = try allocator.create(Dir);
        d.items = std.StringHashMap(Item).init(allocator);
        d.parent = parent;
        return d;
    }

    /// De-allocate a Dir
    fn deinit(self: *Dir, allocator: std.mem.Allocator) void {
        var iter: std.StringHashMap(Item).Iterator = self.items.iterator();
        while (iter.next()) |entry| {
            switch (entry.value_ptr.*) {
                .file => {},
                .dir => |subdir| subdir.deinit(allocator),
            }
            allocator.free(entry.key_ptr.*);
        }
        self.items.deinit();
        allocator.destroy(self);
    }
};

// Process each line of input

const LineError = error{
    EmptyLine,
    SingleWordLine,
    MissingDirectory,
    ExtraText,
    RootParent,
    UnknownDir,
    FileNotDir,
    BadSyntax,
};

const ProcessState = struct {
    listing_mode: bool,
    root_dir: *Dir,
    current_dir: *Dir,

    fn init(root: *Dir, current: *Dir) ProcessState {
        return ProcessState{
            .listing_mode = false,
            .root_dir = root,
            .current_dir = current,
        };
    }
};

pub fn process_line(allocator: std.mem.Allocator, s: *ProcessState, line: []const u8) (LineError || error{ OutOfMemory, Overflow, InvalidCharacter })!void {
    var iter: std.mem.SplitIterator(u8, .scalar) = std.mem.splitScalar(u8, line, ' ');
    const word1: []const u8 = iter.next() orelse return LineError.EmptyLine;
    const word2: []const u8 = iter.next() orelse return LineError.SingleWordLine;

    // $ ls
    if (std.mem.eql(u8, word1, "$") and std.mem.eql(u8, word2, "ls")) {
        s.listing_mode = true;
        if (iter.next() == null) return else return LineError.ExtraText;
    }

    // $ cd
    if (std.mem.eql(u8, word1, "$") and std.mem.eql(u8, word2, "cd")) {
        const word3: []const u8 = iter.next() orelse return LineError.MissingDirectory;
        s.listing_mode = false;

        if (std.mem.eql(u8, word3, "/")) { // $ cd /
            s.current_dir = s.root_dir;
        } else if (std.mem.eql(u8, word3, "..")) { // $ cd ..
            if (s.current_dir.parent) |parent_value| {
                s.current_dir = parent_value;
            } else {
                return LineError.RootParent;
            }
        } else { // $ cd dir
            const item: Item = s.current_dir.items.get(word3) orelse return LineError.UnknownDir;
            switch (item) {
                .file => return LineError.FileNotDir,
                .dir => |d| s.current_dir = d,
            }
        }
        if (iter.next() == null) return else return LineError.ExtraText;
    }

    // Make sure we are in listing mode
    if (!s.listing_mode) {
        return LineError.BadSyntax;
    }

    // directory items
    if (std.mem.eql(u8, word1, "dir")) {
        const d: *Dir = try Dir.init(allocator, s.current_dir);
        const cpy = try allocator.dupe(u8, word2);
        try s.current_dir.items.put(cpy, Item{ .dir = d });
    } else {
        const n: usize = try std.fmt.parseInt(usize, word1, 10);
        const cpy = try allocator.dupe(u8, word2);
        try s.current_dir.items.put(cpy, Item{ .file = n });
    }

    if (iter.next() == null) return else return LineError.ExtraText;
}

// Helper function to recursively walk over directories and add to sizes
fn dir_size(d: *Dir, list_ptr: *std.ArrayList(usize)) (LineError || error{OutOfMemory})!usize {
    var size: usize = 0;
    var iter: std.StringHashMap(Item).Iterator = d.items.iterator();
    while (iter.next()) |entry| {
        size += switch (entry.value_ptr.*) {
            .file => |file_size| file_size,
            .dir => |sub_dir| try dir_size(sub_dir, list_ptr),
        };
    }
    try list_ptr.append(size);
    return size;
}

// Return sorted sizes of directories (as a caller-owned slice)
pub fn dir_sizes(allocator: std.mem.Allocator, root: *Dir) ![]usize {
    var sizes = std.ArrayList(usize).init(allocator);
    _ = try dir_size(root, &sizes);
    var sizes_slice: []usize = try sizes.toOwnedSlice();
    std.sort.insertion(usize, sizes_slice, {}, std.sort.asc(usize));
    return sizes_slice;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var root: *Dir = try Dir.init(allocator, null);
    defer root.deinit(allocator);

    var state = ProcessState.init(root, root);

    // Parse input to build our directory tree
    try rw.fold_lines(ProcessState, allocator, &state, process_line);

    // Get the sizes of all of the directories
    const sizes: []usize = try dir_sizes(allocator, root);
    defer allocator.free(sizes);

    // Part (a): sum of sizes of directories with size <= 100_000
    var size_sum: usize = 0;
    for (sizes) |size| {
        if (size <= 100_000) {
            size_sum += size;
        }
    }
    try rw.write_int(usize, size_sum);

    // Part (b): smallest director providing space needed
    const current_used: usize = sizes[sizes.len - 1];
    const current_space = 70_000_000 - current_used;
    const space_needed = 30_000_000 - current_space;
    for (sizes) |size| {
        if (size >= space_needed) {
            try rw.write_int(usize, size);
            return;
        }
    }
    unreachable;
}
