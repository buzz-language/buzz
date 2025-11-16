const std = @import("std");
const api = @import("buzz_api.zig");

pub export fn alignOf(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const zig_type_str = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    ).?;

    var expected_type: api.Value = undefined;
    const zig_type = api.bz_zigType(
        ctx.vm,
        zig_type_str,
        len,
        &expected_type,
    );

    if (zig_type) |ztype| {
        api.bz_push(
            ctx.vm,
            .fromInteger(
                @intCast(api.bz_zigTypeAlignment(ctx.vm, ztype)),
            ),
        );
    } else {
        var msg = std.Io.Writer.Allocating.init(api.VM.allocator);
        defer msg.deinit();

        msg.writer.print(
            "Could not parse zig type `{s}`",
            .{
                zig_type_str[0..len],
            },
        ) catch {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        };

        api.pushError(
            ctx.vm,
            "ffi.FFIZigTypeParseError",
            msg.toOwnedSlice() catch {
                api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                unreachable;
            },
        );

        return -1;
    }

    return 1;
}

pub export fn sizeOf(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const zig_type_str = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    ).?;

    var expected_type: api.Value = undefined;
    const zig_type = api.bz_zigType(
        ctx.vm,
        zig_type_str,
        len,
        &expected_type,
    );

    if (zig_type) |ztype| {
        api.bz_push(
            ctx.vm,
            .fromInteger(
                @intCast(api.bz_zigTypeSize(ctx.vm, ztype)),
            ),
        );
    } else {
        var msg = std.Io.Writer.Allocating.init(api.VM.allocator);
        defer msg.deinit();

        msg.writer.print(
            "Could not parse zig type `{s}`",
            .{
                zig_type_str[0..len],
            },
        ) catch {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        };

        api.pushError(
            ctx.vm,
            "ffi.FFIZigTypeParseError",
            msg.toOwnedSlice() catch {
                api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                unreachable;
            },
        );

        return -1;
    }

    return 1;
}

// FIXME: raise error if typedef is not .ForeignContainer
pub export fn sizeOfStruct(ctx: *api.NativeCtx) callconv(.c) c_int {
    const type_def = api.bz_peek(ctx.vm, 0);

    api.bz_push(
        ctx.vm,
        .fromInteger(
            @intCast(api.bz_containerTypeSize(ctx.vm, type_def)),
        ),
    );

    return 1;
}

pub export fn alignOfStruct(ctx: *api.NativeCtx) callconv(.c) c_int {
    const type_def = api.bz_peek(ctx.vm, 0);

    api.bz_push(
        ctx.vm,
        .fromInteger(
            @intCast(api.bz_containerTypeAlign(ctx.vm, type_def)),
        ),
    );

    return 1;
}

pub export fn rawData(ctx: *api.NativeCtx) callconv(.c) c_int {
    const data = api.bz_peek(ctx.vm, 0);

    if (!api.bz_valueIsForeignContainer(ctx.vm, data)) {
        api.pushError(ctx.vm, "ffi.ValueNotForeignContainer", null);

        return -1;
    }

    var len: usize = 0;
    const data_ptr = api.bz_foreignContainerSlice(ctx.vm, data, &len);

    api.bz_push(
        ctx.vm,
        api.bz_stringToValue(ctx.vm, data_ptr, len),
    );

    return 1;
}
