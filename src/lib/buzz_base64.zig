const api = @import("buzz_api.zig");
const std = @import("std");

pub export fn base64Encode(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const data_ptr = ctx.vm.bz_peek(0).bz_valueToString(&len);
    const data = data_ptr.?[0..len];

    const encoded = std.base64.standard.encode(data);

    ctx.vm.bz_push(
        api.VM.bz_stringToValue(ctx.vm, encoded.ptr, encoded.len),
    );

    return 1;
}

pub export fn base64Decode(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const encoded_ptr = ctx.vm.bz_peek(0).bz_valueToString(&len);
    const encoded = encoded_ptr.?[0..len];

    const decoded = std.base64.standard.decode(encoded) catch {
        ctx.vm.pushError("errors.InvalidArgumentError", "Invalid Base64 string");
        return -1;
    };

    ctx.vm.bz_push(
        api.VM.bz_stringToValue(ctx.vm, decoded.ptr, decoded.len),
    );

    return 1;
}

pub const library = api.BuzzApi(
    "base64",
    &.{
        &.{ "encode", base64Encode },
        &.{ "decode", base64Decode },
    },
){};
