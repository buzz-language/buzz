const std = @import("std");

/// A fixed-capacity page that can reuse freed slots without moving live items
pub fn PoolPage(comptime T: type, comptime size: usize) type {
    return struct {
        const Self = @This();

        /// Errors returned by page allocation and insertion operations
        pub const Error = error{
            PoolPageFull,
            OutOfMemory,
        };

        /// Stable slot identifier used to address values stored in a page
        pub const Idx = struct {
            index: u64,

            /// Wraps a raw integer as an index
            pub fn idx(raw_idx: u64) Idx {
                return .{ .index = raw_idx };
            }
        };

        /// Zero-value page suitable for static initialization
        pub const empty = Self{};

        /// Creates a page whose returned indices are offset by the given global base
        pub fn withOffset(offset: usize) Self {
            return .{
                .offset = offset,
            };
        }

        // Fixed array when data is actually stored
        page: [size]T = undefined,
        /// Slice over `page`
        slots: []T = &.{},
        /// Reusable slot indices stored as a LIFO stack
        free_slots: [size]Idx = undefined,
        free_slots_count: usize = 0,
        // Offset by which the index must be incremented
        offset: usize = 0,

        /// Inserts an item, reusing a freed slot when possible
        pub fn append(self: *Self, item: T) Error!Idx {
            // Use a free slot if available
            if (self.free_slots_count > 0) {
                self.free_slots_count -= 1;
                const slot = self.free_slots[self.free_slots_count];
                self.slots[slot.index] = item;

                return .{ .index = slot.index + self.offset };
            }

            // If still room in the page, append the new item
            if (self.slots.len < size) {
                self.slots = self.page[0 .. self.slots.len + 1];
                self.slots[self.slots.len - 1] = item;

                return .{ .index = self.slots.len - 1 + self.offset };
            }

            // User is supposed to catch this and allocate a new PoolPage
            return Error.PoolPageFull;
        }

        /// Marks a page-local slot as reusable
        pub fn remove(self: *Self, index: Idx) void {
            self.free_slots[self.free_slots_count] = index;
            self.free_slots_count += 1;
        }

        /// Returns whether the page currently has no live items
        pub fn isEmpty(self: *Self) bool {
            return self.slots.len == 0 or self.free_slots_count == self.slots.len;
        }

        /// Returns whether the page has no spare capacity or reusable slots
        pub fn isFull(self: *Self) bool {
            return self.slots.len == size and self.free_slots_count == 0;
        }
    };
}

/// Page list for a single type, with stable page storage across list growth
pub fn Pool(comptime T: type, comptime page_size: usize) type {
    // List of pointers to heap-allocated PoolPage so that adding or removing pages doesn't invalidate any pointer
    // to remaining pages.
    return std.ArrayList(*PoolPage(T, page_size));
}

/// Paged pool for multiple types where item pointers stay stable across page growth
pub fn MultiPool(comptime Types: []const type, comptime page_size: usize) type {
    var field_names: [Types.len][:0]const u8 = undefined;
    var field_types: [Types.len]type = undefined;
    var field_attrs: [Types.len]std.builtin.Type.StructField.Attributes = undefined;
    inline for (Types, 0..) |T, i| {
        const P = Pool(T, page_size);
        field_names[i] = @typeName(T);
        field_types[i] = P;
        field_attrs[i] = .{
            .default_value_ptr = @ptrCast(&P.empty),
        };
    }

    const Pools = @Struct(
        .auto,
        null,
        &field_names,
        &field_types,
        &field_attrs,
    );

    return struct {
        const Self = @This();

        pools: Pools = .{},

        /// Zero-value multipool suitable for static initialization
        pub const empty = Self{};

        /// Frees all pages owned by the multipool
        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            inline for (Types) |T| {
                var field = @field(self.pools, @typeName(T));
                for (field.items) |page| {
                    allocator.destroy(page);
                }
                field.deinit(allocator);
            }
        }

        /// Returns a stable pointer to the item addressed by the given index
        pub fn get(self: *Self, comptime T: type, index: PoolPage(T, page_size).Idx) *T {
            const pool = &@field(self.pools, @typeName(T));
            const page_idx = (index.index / page_size);
            const idx = index.index % page_size;

            return &pool.items[page_idx].slots[idx];
        }

        /// Appends an item to the pool for the given type and returns its global index
        pub fn append(
            self: *Self,
            allocator: std.mem.Allocator,
            comptime T: type,
            item: T,
        ) PoolPage(T, page_size).Error!PoolPage(T, page_size).Idx {
            const pool = &@field(self.pools, @typeName(T));

            // Search for the first non-full page
            var page_idx: ?usize = null;
            for (pool.items, 0..) |page, pidx| {
                if (!page.isFull()) {
                    page_idx = pidx;
                    break;
                }
            }

            if (page_idx) |pidx| {
                return pool.items[pidx].append(item);
            }

            // Everything is full, append a new page and append to it
            const page = try allocator.create(PoolPage(T, page_size));
            page.* = .withOffset(pool.items.len * page_size);
            errdefer allocator.destroy(page);
            try pool.append(allocator, page);

            return pool.items[pool.items.len - 1].append(item);
        }

        /// Removes the item at the given global index and may trim an empty trailing page
        pub fn remove(
            self: *Self,
            allocator: std.mem.Allocator,
            comptime T: type,
            index: PoolPage(T, page_size).Idx,
        ) PoolPage(T, page_size).Error!void {
            const pool = &@field(self.pools, @typeName(T));
            const page_idx = (index.index / page_size);
            const idx = index.index % page_size;
            const page = pool.items[page_idx];

            page.remove(.idx(idx));

            // If the two last pages are empty, remove the last one
            // We don't do this for the last page only to avoid allocating/deallocating it too fast when
            // we sit at the edge of its capacity
            if (pool.items.len > 1 and pool.items[pool.items.len - 1].isEmpty() and pool.items[pool.items.len - 2].isEmpty()) {
                allocator.destroy(pool.pop().?);
            }
        }
    };
}

test "MultiPool" {
    const gpa = std.testing.allocator;

    var multi = MultiPool(
        &.{ bool, u64, f64 },
        3,
    ).empty;
    defer multi.deinit(gpa);

    _ = try multi.append(gpa, u64, 0);
    _ = try multi.append(gpa, u64, 1);
    _ = try multi.append(gpa, u64, 2);

    std.debug.assert(multi.pools.u64.items.len == 1);
    std.debug.assert(multi.pools.u64.items[0].isFull());

    // A new item should trigger a new page
    const last = try multi.append(gpa, u64, 3);

    std.debug.assert(multi.pools.u64.items.len == 2);
    std.debug.assert(multi.pools.u64.items[0].isFull());
    std.debug.assert(!multi.pools.u64.items[1].isFull());

    // Retrieving the item fetches the appropriate page
    std.debug.assert(multi.get(u64, last).* == 3);

    // Fill one more page, empty the last two which should trigger the last page to be removed
    _ = try multi.append(gpa, u64, 4);
    _ = try multi.append(gpa, u64, 5);
    _ = try multi.append(gpa, u64, 6);

    std.debug.assert(multi.pools.u64.items.len == 3);

    for ([_]u64{ 6, 5, 4, 3 }) |idx|
        try multi.remove(gpa, u64, .idx(idx));

    // We should have one less page
    std.debug.assert(multi.pools.u64.items.len == 2);
}

test "PoolPage reuses freed slots" {
    var page = PoolPage(u64, 3).empty;

    const idx0 = try page.append(10);
    const idx1 = try page.append(20);
    const idx2 = try page.append(30);

    std.debug.assert(idx0.index == 0);
    std.debug.assert(idx1.index == 1);
    std.debug.assert(idx2.index == 2);
    std.debug.assert(page.isFull());

    page.remove(idx1);

    std.debug.assert(!page.isFull());

    const reused = try page.append(99);

    std.debug.assert(reused.index == idx1.index);
    std.debug.assert(page.slots[1] == 99);
    std.debug.assert(page.isFull());
}

test "PoolPage withOffset returns global indices" {
    var page = PoolPage(u64, 3).withOffset(6);

    const idx0 = try page.append(10);
    const idx1 = try page.append(20);

    std.debug.assert(idx0.index == 6);
    std.debug.assert(idx1.index == 7);
}

test "PoolPage withOffset reuses freed slots as global indices" {
    var page = PoolPage(u64, 3).withOffset(6);

    _ = try page.append(10);
    const idx1 = try page.append(20);
    _ = try page.append(30);

    page.remove(.idx(idx1.index - page.offset));

    const reused = try page.append(99);

    std.debug.assert(reused.index == idx1.index);
}

test "MultiPool reuses freed slots and keeps types separated" {
    const gpa = std.testing.allocator;

    var multi = MultiPool(
        &.{ bool, u64, f64 },
        2,
    ).empty;
    defer multi.deinit(gpa);

    const bool_idx = try multi.append(gpa, bool, true);
    const u64_idx = try multi.append(gpa, u64, 10);
    const f64_idx = try multi.append(gpa, f64, 1.5);

    std.debug.assert(multi.get(bool, bool_idx).* == true);
    std.debug.assert(multi.get(u64, u64_idx).* == 10);
    std.debug.assert(multi.get(f64, f64_idx).* == 1.5);

    try multi.remove(gpa, u64, u64_idx);

    const reused_u64_idx = try multi.append(gpa, u64, 99);

    std.debug.assert(reused_u64_idx.index == u64_idx.index);
    std.debug.assert(multi.get(bool, bool_idx).* == true);
    std.debug.assert(multi.get(u64, reused_u64_idx).* == 99);
    std.debug.assert(multi.get(f64, f64_idx).* == 1.5);
}

test "MultiPool keeps an empty trailing page available for reuse" {
    const gpa = std.testing.allocator;

    var multi = MultiPool(
        &.{u64},
        2,
    ).empty;
    defer multi.deinit(gpa);

    _ = try multi.append(gpa, u64, 0);
    _ = try multi.append(gpa, u64, 1);
    const trailing = try multi.append(gpa, u64, 2);

    std.debug.assert(multi.pools.u64.items.len == 2);

    try multi.remove(gpa, u64, trailing);

    // The final page is kept around until the previous page is also empty.
    std.debug.assert(multi.pools.u64.items.len == 2);

    const reused = try multi.append(gpa, u64, 3);

    std.debug.assert(reused.index == trailing.index);
    std.debug.assert(multi.pools.u64.items.len == 2);
    std.debug.assert(multi.get(u64, reused).* == 3);
}

test "MultiPool pointers stay stable across page growth" {
    const gpa = std.testing.allocator;

    var multi = MultiPool(
        &.{u64},
        1,
    ).empty;
    defer multi.deinit(gpa);

    const first = try multi.append(gpa, u64, 10);
    const first_addr = @intFromPtr(multi.get(u64, first));

    for (0..64) |i| {
        _ = try multi.append(gpa, u64, @intCast(i + 11));
    }

    const first_ptr_after_growth = multi.get(u64, first);

    std.debug.assert(@intFromPtr(first_ptr_after_growth) == first_addr);
    std.debug.assert(first_ptr_after_growth.* == 10);
}
