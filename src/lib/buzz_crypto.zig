const api = @import("buzz_api.zig");
const std = @import("std");

const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

fn bin2hex(allocator: std.mem.Allocator, input: []const u8) std.ArrayList(u8) {
    var result = std.ArrayList(u8).init(allocator);
    var writer = result.writer();

    for (input) |byte| {
        writer.print("{x:0>2}", .{byte}) catch @panic("Could not convert string to hex");
    }

    return result;
}

pub export fn hash(ctx: *api.NativeCtx) c_int {
    const algo_index = ctx.vm.bz_peek(1).bz_getEnumInstanceValue().integer();
    var data_len: usize = 0;
    const data = ctx.vm.bz_peek(0).bz_valueToString(&data_len) orelse @panic("Could not hash data");

    // Since alog_index is not static, we're forced to repeat ourselves...
    var result_hash: []u8 = undefined;
    switch (algo_index) {
        0 => {
            var h: [std.crypto.hash.Md5.digest_length]u8 = undefined;
            std.crypto.hash.Md5.hash(data[0..data_len], &h, .{});
            result_hash = h[0..];
        },
        1 => {
            var h: [std.crypto.hash.Sha1.digest_length]u8 = undefined;
            std.crypto.hash.Sha1.hash(data[0..data_len], &h, .{});
            result_hash = h[0..];
        },
        2 => {
            var h: [std.crypto.hash.sha2.Sha224.digest_length]u8 = undefined;
            std.crypto.hash.sha2.Sha224.hash(data[0..data_len], &h, .{});
            result_hash = h[0..];
        },
        3 => {
            var h: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
            std.crypto.hash.sha2.Sha256.hash(data[0..data_len], &h, .{});
            result_hash = h[0..];
        },
        4 => {
            var h: [std.crypto.hash.sha2.Sha384.digest_length]u8 = undefined;
            std.crypto.hash.sha2.Sha384.hash(data[0..data_len], &h, .{});
            result_hash = h[0..];
        },
        5 => {
            var h: [std.crypto.hash.sha2.Sha512.digest_length]u8 = undefined;
            std.crypto.hash.sha2.Sha512.hash(data[0..data_len], &h, .{});
            result_hash = h[0..];
        },
        6 => {
            var h: [std.crypto.hash.sha2.Sha512_224.digest_length]u8 = undefined;
            std.crypto.hash.sha2.Sha512_224.hash(data[0..data_len], &h, .{});
            result_hash = h[0..];
        },
        7 => {
            var h: [std.crypto.hash.sha2.Sha512_256.digest_length]u8 = undefined;
            std.crypto.hash.sha2.Sha512_256.hash(data[0..data_len], &h, .{});
            result_hash = h[0..];
        },
        8 => {
            var h: [std.crypto.hash.sha2.Sha512T256.digest_length]u8 = undefined;
            std.crypto.hash.sha2.Sha512T256.hash(data[0..data_len], &h, .{});
            result_hash = h[0..];
        },
        9 => {
            var h: [std.crypto.hash.sha3.Sha3_224.digest_length]u8 = undefined;
            std.crypto.hash.sha3.Sha3_224.hash(data[0..data_len], &h, .{});
            result_hash = h[0..];
        },
        10 => {
            var h: [std.crypto.hash.sha3.Sha3_256.digest_length]u8 = undefined;
            std.crypto.hash.sha3.Sha3_256.hash(data[0..data_len], &h, .{});
            result_hash = h[0..];
        },
        11 => {
            var h: [std.crypto.hash.sha3.Sha3_384.digest_length]u8 = undefined;
            std.crypto.hash.sha3.Sha3_384.hash(data[0..data_len], &h, .{});
            result_hash = h[0..];
        },
        12 => {
            var h: [std.crypto.hash.sha3.Sha3_512.digest_length]u8 = undefined;
            std.crypto.hash.sha3.Sha3_512.hash(data[0..data_len], &h, .{});
            result_hash = h[0..];
        },

        else => @panic("Unknown hash algorithm"),
    }

    ctx.vm.bz_push(
        api.VM.bz_stringToValue(
            ctx.vm,
            result_hash.ptr,
            result_hash.len,
        ),
    );

    return 1;
}
