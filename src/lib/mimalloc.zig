const std = @import("std");
const mem = std.mem;
const math = std.math;
const debug = std.debug;

const Allocator = mem.Allocator;

extern fn mi_malloc_aligned(size: usize, alignment: usize) ?*anyopaque;
extern fn mi_usable_size(p: ?*const anyopaque) usize;
extern fn mi_expand(p: ?*anyopaque, newsize: usize) ?*anyopaque;
extern fn mi_free(p: ?*anyopaque) void;

const MimAllocator = struct {
    fn alloc(
        _: *anyopaque,
        len: usize,
        log2_align: mem.Alignment,
        _: usize,
    ) ?[*]u8 {
        return @ptrCast(
            mi_malloc_aligned(
                len,
                log2_align.toByteUnits(),
            ),
        );
    }

    fn resize(
        _: *anyopaque,
        buf: []u8,
        _: mem.Alignment,
        new_len: usize,
        _: usize,
    ) bool {
        if (new_len > buf.len) {
            const available = mi_usable_size(buf.ptr);
            if (available > new_len) {
                if (mi_expand(buf.ptr, new_len)) |_| {
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
        _: mem.Alignment,
        _: usize,
    ) void {
        mi_free(buf.ptr);
    }

    fn remap(
        ctx: *anyopaque,
        buf: []u8,
        log2_align: mem.Alignment,
        new_len: usize,
        return_address: usize,
    ) ?[*]u8 {
        return if (resize(ctx, buf, log2_align, new_len, return_address))
            buf.ptr
        else
            null;
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
    .remap = MimAllocator.remap,
};
