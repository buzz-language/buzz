const std = @import("std");
const api = @import("buzz_api.zig");

pub export fn alignOf(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const zig_type_str = ctx.vm.bz_peek(0).bz_valueToString(&len).?;

    var expected_type: api.Value = undefined;
    const zig_type = ctx.vm.bz_zigType(zig_type_str, len, &expected_type);

    if (zig_type) |ztype| {
        ctx.vm.bz_push(api.Value.fromInteger(@intCast(ztype.bz_zigTypeAlignment())));
    } else {
        var msg = std.Io.Writer.Allocating.init(api.VM.allocator);
        defer msg.deinit();

        msg.writer.print(
            "Could not parse zig type `{s}`",
            .{
                zig_type_str[0..len],
            },
        ) catch {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        };

        ctx.vm.pushError(
            "ffi.FFIZigTypeParseError",
            msg.toOwnedSlice() catch {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        );

        return -1;
    }

    return 1;
}

pub export fn sizeOf(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const zig_type_str = ctx.vm.bz_peek(0).bz_valueToString(&len).?;

    var expected_type: api.Value = undefined;
    const zig_type = ctx.vm.bz_zigType(zig_type_str, len, &expected_type);

    if (zig_type) |ztype| {
        ctx.vm.bz_push(api.Value.fromInteger(@intCast(ztype.bz_zigTypeSize())));
    } else {
        var msg = std.Io.Writer.Allocating.init(api.VM.allocator);
        defer msg.deinit();

        msg.writer.print(
            "Could not parse zig type `{s}`",
            .{
                zig_type_str[0..len],
            },
        ) catch {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        };

        ctx.vm.pushError(
            "ffi.FFIZigTypeParseError",
            msg.toOwnedSlice() catch {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        );

        return -1;
    }

    return 1;
}

// FIXME: raise error if typedef is not .ForeignContainer
pub export fn sizeOfStruct(ctx: *api.NativeCtx) callconv(.c) c_int {
    const type_def = ctx.vm.bz_peek(0);

    ctx.vm.bz_push(
        api.Value.fromInteger(@intCast(type_def.bz_containerTypeSize())),
    );

    return 1;
}

pub export fn alignOfStruct(ctx: *api.NativeCtx) callconv(.c) c_int {
    const type_def = ctx.vm.bz_peek(0);

    ctx.vm.bz_push(
        api.Value.fromInteger(@intCast(type_def.bz_containerTypeAlign())),
    );

    return 1;
}

pub export fn rawData(ctx: *api.NativeCtx) callconv(.c) c_int {
    const data = ctx.vm.bz_peek(0);

    if (!data.bz_valueIsForeignContainer()) {
        ctx.vm.pushError("ffi.ValueNotForeignContainer", null);

        return -1;
    }

    var len: usize = 0;
    const data_ptr = data.bz_foreignContainerSlice(&len);

    ctx.vm.bz_push(
        ctx.vm.bz_stringToValue(data_ptr, len),
    );

    return 1;
}
