const std = @import("std");

/// An ArrayList in which slots can be considered freed and reused later by new items
pub fn Pool(comptime T: type) type {
    return struct {
        const Self = @This();

        pub const Error = error{
            OutOfMemory,
        };

        pub const Idx = struct {
            index: u43,

            pub fn idx(raw_idx: u43) Idx {
                return .{ .index = raw_idx };
            }

            pub fn eql(self: Idx, other: Idx) bool {
                return self.index == other.index;
            }
        };

        pub const empty = Self{};

        slots: std.ArrayList(?T) = .empty,
        free_slots: std.ArrayList(Idx) = .empty,

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.slots.deinit(allocator);
            self.free_slots.deinit(allocator);
        }

        pub fn append(self: *Self, allocator: std.mem.Allocator, item: T) Error!Idx {
            if (self.free_slots.pop()) |slot| {
                self.slots.items[slot.index] = item;

                return slot;
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

        pub fn get(self: *Self, comptime T: type, index: Pool(T).Idx) ?*T {
            if (@field(self.pools, @typeName(T)).slots.items[index.index] != null) {
                return &@field(self.pools, @typeName(T)).slots.items[index.index].?;
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

test "MultiPool" {
    var multi = MultiPool(&.{ bool, u64, f64 }).empty;
    defer multi.deinit(std.testing.allocator);

    std.debug.assert(
        (try multi.append(std.testing.allocator, u64, 32)).index == 0,
    );
    std.debug.assert(
        (try multi.append(std.testing.allocator, u64, 33)).index == 1,
    );
    std.debug.assert(
        (try multi.append(std.testing.allocator, u64, 34)).index == 2,
    );

    try multi.remove(std.testing.allocator, u64, .idx(1));
    std.debug.assert(multi.pools.u64.free_slots.items.len == 1);
    std.debug.assert(
        (try multi.append(std.testing.allocator, u64, 35)).index == 1,
    );

    std.debug.assert(
        multi.poolOf(u64).slots.items[1] == @as(u64, @intCast(35)),
    );

    std.debug.assert(
        multi.get(u64, .idx(1)).* == @as(u64, @intCast(35)),
    );
}
