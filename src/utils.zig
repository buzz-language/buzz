const std = @import("std");

// TODO: pass allocator and still be C ABI compliant?

pub fn toSlice(c_string: [*:0]const u8) []const u8 {
    var c_slice: [:0]const u8 = std.mem.sliceTo(c_string, 0);

    return c_slice[0..c_slice.len];
}

pub fn toNullTerminated(allocator: std.mem.Allocator, string: []const u8) ?[:0]const u8 {
    return allocator.dupeZ(u8, string) catch null;
}

// TODO: maybe use [:0]u8 throughout so we don't have to do this
// FIXME: leaks memory
pub fn toCString(allocator: std.mem.Allocator, string: []const u8) ?[*:0]const u8 {
    var c_string: ?[]u8 = allocator.dupeZ(u8, string) catch {
        return null;
    };

    return @ptrCast([*:0]u8, c_string.?);
}
