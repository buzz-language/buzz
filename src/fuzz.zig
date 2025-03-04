const std = @import("std");
const run = @import("run.zig");

export fn zig_fuzz_init() callconv(.c) void {}

export fn zig_fuzz_test(buf: [*]u8, len: isize) callconv(.c) void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    const allocator = gpa.allocator();

    const src = buf[0..@intCast(len)];

    run.run(
        allocator,
        .Run,
        "fuzz",
        src,
        null,
    ) catch |err| {
        if (err != error.CanCompile) {
            std.debug.print(
                "Fuzz test with followint input failed with error {!}\n```\n{s}\n```",
                .{
                    err,
                    src,
                },
            );
        }
    };
}
