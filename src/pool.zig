const std = @import("std");
const GC = @import("GC.zig");
const builtin = @import("builtin");

/// An ArrayList in which slots can be considered freed and reused later by new items
pub fn Pool(comptime T: type) type {
    return struct {
        const Self = @This();

        pub const Error = error{
            OutOfMemory,
        };

        /// A shallow struct around the index so we can't mix up a index to different types
        pub const Idx = struct {
            index: u43,

            pub fn idx(raw_idx: u43) Idx {
                return .{ .index = raw_idx };
            }

            pub fn eql(self: Idx, other: Idx) bool {
                return self.index == other.index;
            }

            pub inline fn get(self: Idx, gc: *GC) *T {
                return gc.ptr(T, self).?;
            }
        };

        pub const empty = Self{};

        slots: std.ArrayList(?T) = .empty,
        free_slots: std.ArrayList(Idx) = .empty,

        /// In debug mode, when the pool needs to be resized, instead of doing so, we allocate a whole new pool
        /// and keep the old one alive and reset all its slots to null. That way if a invalidated pointer is used
        /// we will get a runtime error with a stack trace.
        old_slots: std.ArrayList(?T) = .empty,

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.slots.deinit(allocator);
            self.free_slots.deinit(allocator);
            self.old_slots.deinit(allocator);
        }

        pub fn append(self: *Self, allocator: std.mem.Allocator, item: T) Error!Idx {
            if (self.free_slots.pop()) |slot| {
                self.slots.items[slot.index] = item;

                return slot;
            }

            // If debug and we were about to resize the pool and invalidate pointers to it,
            // we clone the pool, reset the old one to `null`s so that any access to invalidated pointers fails with a trace
            if (builtin.mode == .Debug and self.slots.items.len + 1 >= self.slots.capacity) {
                self.old_slots = self.slots;
                self.slots = try self.slots.clone(allocator);

                for (self.old_slots.items) |*slot| {
                    slot.* = null;
                }

                std.debug.print(
                    "Resizing pool of {s}, {} + 1, {*} -> {*}\n",
                    .{
                        @typeName(T),
                        self.old_slots.items.len,
                        self.old_slots.items,
                        self.slots.items,
                    },
                );
            }

            try self.slots.append(allocator, item);

            return .{ .index = @intCast(self.slots.items.len - 1) };
        }

        pub fn remove(self: *Self, allocator: std.mem.Allocator, idx: Idx) Error!void {
            self.slots.items[idx.index] = null;
            try self.free_slots.append(allocator, idx);
        }
    };
}

/// Maintain Pools for a list of types
pub fn MultiPool(comptime Types: []const type) type {
    var fields: [Types.len]std.builtin.Type.StructField = undefined;
    inline for (Types, 0..) |T, i| {
        fields[i] = .{
            .name = @typeName(T),
            .type = Pool(T),
            .default_value_ptr = @ptrCast(&Pool(T).empty),
            .is_comptime = false,
            .alignment = @alignOf(Pool(T)),
        };
    }

    const Pools = @Type(
        .{
            .@"struct" = .{
                .layout = .auto,
                .fields = &fields,
                .decls = &.{},
                .is_tuple = false,
            },
        },
    );

    return struct {
        const Self = @This();

        pools: Pools = .{},

        pub const empty = Self{};

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            inline for (Types) |T|
                @field(self.pools, @typeName(T)).deinit(allocator);
        }

        pub fn poolOf(self: *Self, comptime T: type) *Pool(T) {
            return &@field(self.pools, @typeName(T));
        }

        pub inline fn get(self: *Self, comptime T: type, index: Pool(T).Idx) ?*T {
            if (@field(self.pools, @typeName(T)).slots.items[index.index] != null) {
                return &@field(self.pools, @typeName(T)).slots.items[index.index].?;
            }

            if (builtin.mode == .Debug) {
                std.debug.print(
                    "Access to nulled out slot Pool({s})[{}]\n",
                    .{
                        @typeName(T),
                        index.index,
                    },
                );

                unreachable;
            }

            return null;
        }

        pub fn append(
            self: *Self,
            allocator: std.mem.Allocator,
            comptime T: type,
            item: T,
        ) Pool(T).Error!Pool(T).Idx {
            return @field(self.pools, @typeName(T)).append(allocator, item);
        }

        pub fn remove(
            self: *Self,
            allocator: std.mem.Allocator,
            comptime T: type,
            index: Pool(T).Idx,
        ) Pool(T).Error!void {
            return @field(self.pools, @typeName(T)).remove(allocator, index);
        }
    };
}
