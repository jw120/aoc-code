// Queue from https://ziglang.org/learn/samples/#generic-types

const std = @import("std");

pub fn Queue(comptime Child: type) type {
    return struct {
        const This = @This();
        const Node = struct {
            data: Child,
            next: ?*Node,
        };
        allocator: std.mem.Allocator,
        start: ?*Node,
        end: ?*Node,

        pub fn init(allocator: std.mem.Allocator) This {
            return This{
                .allocator = allocator,
                .start = null,
                .end = null,
            };
        }
        pub fn enqueue(this: *This, value: Child) !void {
            const node = try this.allocator.create(Node);
            node.* = .{ .data = value, .next = null };
            if (this.end) |end| end.next = node //
            else this.start = node;
            this.end = node;
        }
        pub fn dequeue(this: *This) ?Child {
            const start = this.start orelse return null;
            defer this.allocator.destroy(start);
            if (start.next) |next|
                this.start = next
            else {
                this.start = null;
                this.end = null;
            }
            return start.data;
        }
        pub fn is_empty(this: *This) bool {
            return this.start == null;
        }
        pub fn deinit(this: *This) void {
            var n: ?*Node = this.start;
            while (n) |n_value| {
                const next_n = n_value.next;
                this.allocator.destroy(n_value);
                n = next_n;
            }
        }
    };
}

test "queue" {
    var int_queue = Queue(i32).init(std.testing.allocator);

    try std.testing.expect(int_queue.is_empty());
    try int_queue.enqueue(25);
    try int_queue.enqueue(50);
    try std.testing.expect(!int_queue.is_empty());
    try int_queue.enqueue(75);
    try int_queue.enqueue(100);

    try std.testing.expectEqual(int_queue.dequeue(), 25);
    try std.testing.expectEqual(int_queue.dequeue(), 50);
    try std.testing.expectEqual(int_queue.dequeue(), 75);
    try std.testing.expectEqual(int_queue.dequeue(), 100);
    try std.testing.expectEqual(int_queue.dequeue(), null);

    try int_queue.enqueue(5);
    try std.testing.expectEqual(int_queue.dequeue(), 5);
    try std.testing.expectEqual(int_queue.dequeue(), null);
}
