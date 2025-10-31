const std = @import("std");
const Runner = @import("Runner.zig");

export fn zig_fuzz_init() callconv(.c) void {}

export fn zig_fuzz_test(buf: [*]u8, len: isize) callconv(.c) void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    const allocator = gpa.allocator();

    const src = buf[0..@intCast(len)];

    var runner: Runner = undefined;
    runner.init(allocator, .Test, null) catch @panic("Could not initialize runner");
    defer runner.deinit();

    _ = runner.runSource(
        src,
        "fuzz",
    ) catch {
        // TODO: is there some catchable errors we would like AFL to consider as a crash?
    };
}
