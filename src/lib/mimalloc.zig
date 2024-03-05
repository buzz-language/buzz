const std = @import("std");
const mem = std.mem;
const math = std.math;
const debug = std.debug;

const Allocator = mem.Allocator;

const mi = @cImport(@cInclude("mimalloc.h"));

const MimAllocator = struct {
    fn alloc(
        _: *anyopaque,
        len: usize,
        log2_align: u8,
        _: usize,
    ) ?[*]u8 {
        return @ptrCast(
            mi.mi_malloc_aligned(
                len,
                @as(usize, 1) << @as(u6, @intCast(log2_align)),
            ),
        );
    }

    fn resize(
        _: *anyopaque,
        buf: []u8,
        _: u8,
        new_len: usize,
        _: usize,
    ) bool {
        if (new_len > buf.len) {
            const available = mi.mi_usable_size(buf.ptr);
            if (available > new_len) {
                if (mi.mi_expand(buf.ptr, new_len)) |_| {
                    return true;
                }
            }
            return false;
        } else {
            return true;
        }
    }

    fn free(
        _: *anyopaque,
        buf: []u8,
        _: u8,
        _: usize,
    ) void {
        mi.mi_free(buf.ptr);
    }
};

pub const mim_allocator = Allocator{
    .ptr = undefined,
    .vtable = &mim_allocator_vtable,
};
const mim_allocator_vtable = Allocator.VTable{
    .alloc = MimAllocator.alloc,
    .resize = MimAllocator.resize,
    .free = MimAllocator.free,
};
