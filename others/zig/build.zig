const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const coord = b.addModule("coord", .{ .source_file = .{ .path = "src/coord.zig" } });
    const queue = b.addModule("queue", .{ .source_file = .{ .path = "src/queue.zig" } });
    const rw = b.addModule("rw", .{ .source_file = .{ .path = "src/rw.zig" } });

    const names = [_][]const u8{
        "aoc_2022_01",
        "aoc_2022_02",
        "aoc_2022_03",
        "aoc_2022_04",
        "aoc_2022_05",
        "aoc_2022_06",
        "aoc_2022_07",
        "aoc_2022_08",
        "aoc_2022_09",
        "aoc_2022_10",
        "aoc_2022_11",
        "aoc_2022_12",
        "aoc_2022_13",
        "aoc_2022_14",
        "aoc_2022_15",
    };

    const paths = [_][]const u8{
        "src/aoc_2022_01.zig",
        "src/aoc_2022_02.zig",
        "src/aoc_2022_03.zig",
        "src/aoc_2022_04.zig",
        "src/aoc_2022_05.zig",
        "src/aoc_2022_06.zig",
        "src/aoc_2022_07.zig",
        "src/aoc_2022_08.zig",
        "src/aoc_2022_09.zig",
        "src/aoc_2022_10.zig",
        "src/aoc_2022_11.zig",
        "src/aoc_2022_12.zig",
        "src/aoc_2022_13.zig",
        "src/aoc_2022_14.zig",
        "src/aoc_2022_15.zig",
    };

    for (names, paths) |name, path| {
        var exe = b.addExecutable(.{
            .name = name,
            .root_source_file = .{ .path = path },
            .target = target,
            .optimize = optimize,
        });
        exe.addModule("coord", coord);
        exe.addModule("queue", queue);
        exe.addModule("rw", rw);
        b.installArtifact(exe);
        var run_cmd = b.addRunArtifact(exe);
        run_cmd.step.dependOn(b.getInstallStep());
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }
    }

    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/aoc_2022_01.zig" },
        .target = target,
        .optimize = optimize,
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
