const std = @import("std");
const api = @import("./buzz_api.zig");

export fn alignOf(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const zig_type_str = ctx.vm.bz_peek(0).bz_valueToString(&len).?;

    var expected_type: api.Value = undefined;
    const zig_type = ctx.vm.bz_zigType(zig_type_str, len, &expected_type);

    if (zig_type) |ztype| {
        ctx.vm.bz_pushInteger(@intCast(ztype.bz_zigTypeAlignment()));
    } else {
        var msg = std.ArrayList(u8).init(api.VM.allocator);
        defer msg.deinit();

        msg.writer().print("Could not parse zig type `{s}`", .{zig_type_str[0..len]}) catch @panic("Out of memory");

        ctx.vm.pushError("ffi.FFIZigTypeParseError", msg.items);

        return -1;
    }

    return 1;
}

export fn sizeOf(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const zig_type_str = ctx.vm.bz_peek(0).bz_valueToString(&len).?;

    var expected_type: api.Value = undefined;
    const zig_type = ctx.vm.bz_zigType(zig_type_str, len, &expected_type);

    if (zig_type) |ztype| {
        ctx.vm.bz_pushInteger(@intCast(ztype.bz_zigTypeSize()));
    } else {
        var msg = std.ArrayList(u8).init(api.VM.allocator);
        defer msg.deinit();

        msg.writer().print("Could not parse zig type `{s}`", .{zig_type_str[0..len]}) catch @panic("Out of memory");

        ctx.vm.pushError("ffi.FFIZigTypeParseError", msg.items);

        return -1;
    }

    return 1;
}
