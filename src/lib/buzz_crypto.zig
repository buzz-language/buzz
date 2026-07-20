const api = @import("buzz_api.zig");
const std = @import("std");

const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

pub export fn hash(ctx: *api.NativeCtx) callconv(.c) c_int {
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

pub export fn randomBytes(ctx: *api.NativeCtx) callconv(.c) c_int {
  var len: usize = 0;
  const len_val = ctx.vm.bz_peek(0);
  len = @as(usize, @intCast(len_val.integer()));
  const buffer: []u8 = api.VM.allocator.alloc(u8, len) catch {
    ctx.vm.pushError("errors.OutOfMemoryError", null);
     return -1;
   };
  defer api.VM.allocator.free(buffer);
  std.Io.random(ctx.getIo(), buffer);
  ctx.vm.bz_push(
    api.VM.bz_stringToValue(ctx.vm, buffer.ptr, buffer.len),
    );
  return 1;
}


pub export fn argon2(ctx: *api.NativeCtx) callconv(.c) c_int {
    var pass_len: usize = 0;
    const pass_ptr = ctx.vm.bz_peek(0).bz_valueToString(&pass_len);
    const password = pass_ptr.?[0..pass_len];
    const t_cost: u32 = 3;
    const m_cost: u32 = 65536;
    const parallelism: u24 = 1;

    var hash_buf: [256]u8 = undefined;
    const io = ctx.getIo();
    const hash_len = std.crypto.pwhash.argon2.strHash(
      password,
      .{ .allocator = api.VM.allocator,
         .params = .{ .t = t_cost, .m = m_cost, .p = parallelism },
       },
      &hash_buf,
      io
      )  catch {
        ctx.vm.pushError("errors.AuthenticationFailed", "argon2 failed");
        return -1;
      };
    ctx.vm.bz_push(
        api.VM.bz_stringToValue(ctx.vm, hash_buf[0..hash_len].ptr, hash_len),
    );

    return 1;
}

pub export fn verifyArgon2(ctx: *api.NativeCtx) callconv(.c) c_int {
    var pass_len: usize = 0;
    const pass_ptr = ctx.vm.bz_peek(1).bz_valueToString(&pass_len);
    const password = pass_ptr.?[0..pass_len];

    var hash_len: usize = 0;
    const hash_ptr = ctx.vm.bz_peek(0).bz_valueToString(&hash_len);
    const encoded_hash = hash_ptr.?[0..hash_len];

    const valid = std.crypto.pwhash.argon2.strVerify(encoded_hash, password, .{ .allocator = api.VM.allocator }, ctx.getIo()) catch {
        ctx.vm.pushError("errors.AuthenticationFailed", "argon2 verification failed");
        return -1;
    };

    ctx.vm.bz_push(api.Value.fromBoolean(valid));

    return 1;
}
 

pub const library = api.BuzzApi(
    "crypto",
    &.{
        &.{ "hash", hash },
        &.{ "randomBytes", randomBytes },
        &.{ "argon2", argon2 },
        &.{ "verifyArgon2", verifyArgon2 },
    },
){};
