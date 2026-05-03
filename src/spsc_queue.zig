//! Taken from https://github.com/freref/spsc-queue

const std = @import("std");
const cache_line = std.atomic.cache_line;

/// We align the producer and consumer to different cache lines to avoid false
/// sharing between them. We Pad the producer and consumer to ensure that they
/// take up a full cache line each.
fn Pad(comptime N: usize, comptime T: type) type {
    const sz = @sizeOf(T);
    const rem = sz % N;
    return [if (rem == 0) 0 else N - rem]u8;
}

const Producer = struct {
    push_cursor: std.atomic.Value(usize) = .{ .raw = 0 },
    _pad: Pad(cache_line, std.atomic.Value(usize)) = undefined,
};

const Consumer = struct {
    pop_cursor: std.atomic.Value(usize) = .{ .raw = 0 },
    _pad: Pad(cache_line, std.atomic.Value(usize)) = undefined,
};

/// A single-producer, single-consumer lock-free queue using a ring buffer.
/// Following the conventions from the Zig standard library.
pub fn SpscQueue(comptime T: type) type {
    return struct {
        const Self = @This();

        items: []T,
        len: usize,
        mask: usize,

        producer: Producer align(cache_line) = .{},
        consumer: Consumer align(cache_line) = .{},

        pop_cursor_cache: usize = 0,
        push_cursor_cache: usize = 0,

        pub fn initBuffer(buffer: []T) Self {
            std.debug.assert(std.math.isPowerOfTwo(buffer.len));
            return Self{
                .items = buffer,
                .len = buffer.len,
                .mask = buffer.len - 1,
            };
        }

        pub fn initCapacity(allocator: std.mem.Allocator, num: usize) !Self {
            std.debug.assert(std.math.isPowerOfTwo(num));
            const items = try allocator.alloc(T, num);
            return .{
                .items = items,
                .len = items.len,
                .mask = items.len - 1,
            };
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            allocator.free(self.items);
        }

        /// Returns true if the queue is empty.
        pub fn isEmpty(self: *Self) bool {
            const r = self.consumer.pop_cursor.load(.acquire);
            const w = self.producer.push_cursor.load(.acquire);
            return r == w;
        }

        pub fn size(self: *Self) usize {
            const r = self.consumer.pop_cursor.load(.acquire);
            const w = self.producer.push_cursor.load(.acquire);
            return w - r;
        }

        /// Blocking push, spins until there is room in the queue.
        pub fn push(self: *Self, value: T) void {
            const w = self.producer.push_cursor.load(.monotonic);

            // Spin while full (w - r == len)
            while ((w - self.pop_cursor_cache) == self.len) {
                self.pop_cursor_cache = self.consumer.pop_cursor.load(.acquire);
                if ((w - self.pop_cursor_cache) == self.len) {
                    std.atomic.spinLoopHint();
                }
            }

            self.items[w & self.mask] = value;
            self.producer.push_cursor.store(w + 1, .release);
        }

        /// Non-blocking push, returns false if the queue is full.
        pub fn tryPush(self: *Self, value: T) bool {
            const w = self.producer.push_cursor.load(.monotonic);

            if ((w - self.pop_cursor_cache) == self.len) {
                self.pop_cursor_cache = self.consumer.pop_cursor.load(.acquire);
                if ((w - self.pop_cursor_cache) == self.len) return false;
            }

            self.items[w & self.mask] = value;
            self.producer.push_cursor.store(w + 1, .release);
            return true;
        }

        /// Returns a pointer to the front item, or null if the queue is empty.
        pub fn front(self: *Self) ?*T {
            const r = self.consumer.pop_cursor.load(.monotonic);

            if (r == self.push_cursor_cache) {
                self.push_cursor_cache = self.producer.push_cursor.load(.acquire);
                if (self.push_cursor_cache == r) return null;
            }

            return &self.items[r & self.mask];
        }

        /// IMPORTANT: pop must only be called after front() returned non-null.
        /// The consumer is responsible for cleaning up the item if needed.
        pub fn pop(self: *Self) void {
            const r = self.consumer.pop_cursor.load(.monotonic);
            std.debug.assert(self.producer.push_cursor.load(.acquire) != r);

            self.consumer.pop_cursor.store(r + 1, .release);
        }
    };
}
