const std = @import("std");
const runFile = @import("main.zig").runFile;
const io = @import("io.zig");
const Parser = @import("Parser.zig");

/// Because of https://github.com/ziglang/zig/issues/15091 test that write to stdout will hang
/// However I think its completely legitimate for a tested code to output to stdout and I
/// don't really get why zig test needs to use stdout anyway
pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .safety = true,
    }){};
    var allocator = gpa.allocator();
    var count: usize = 0;
    var fail_count: usize = 0;
    {
        var test_dir = try std.fs.cwd().openDir(
            "tests",
            .{
                .iterate = true,
            },
        );
        var it = test_dir.iterate();

        while (try it.next()) |file| : (count += 1) {
            if (file.kind == .file and std.mem.endsWith(u8, file.name, ".buzz")) {
                const file_name = try allocator.alloc(u8, 6 + file.name.len);
                defer allocator.free(file_name);

                var had_error: bool = false;
                runFile(
                    allocator,
                    try std.fmt.bufPrint(file_name, "tests/{s}", .{file.name}),
                    &[_][:0]u8{},
                    .Test,
                ) catch {
                    io.print("\u{001b}[31m[{s} ✕]\u{001b}[0m\n", .{file.name});
                    had_error = true;
                    fail_count += 1;
                };

                if (!had_error) {
                    io.print("\u{001b}[32m[{s} ✓]\u{001b}[0m\n", .{file.name});
                }
            }
        }
    }

    {
        var test_dir = try std.fs.cwd().openDir(
            "tests/compile_errors",
            .{
                .iterate = true,
            },
        );
        var it = test_dir.iterate();

        while (try it.next()) |file| : (count += 1) {
            if (file.kind == .file and std.mem.endsWith(u8, file.name, ".buzz")) {
                const file_name: []u8 = try allocator.alloc(u8, 21 + file.name.len);
                defer allocator.free(file_name);
                _ = try std.fmt.bufPrint(file_name, "tests/compile_errors/{s}", .{file.name});

                // First line of test file is expected error message
                const test_file = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
                const reader = test_file.reader();
                var first_line = try reader.readUntilDelimiterAlloc(allocator, '\n', std.math.maxInt(usize));
                first_line = @constCast(std.mem.trim(u8, first_line, "\r\n"));
                defer allocator.free(first_line);
                const arg0 = std.fmt.allocPrintZ(
                    allocator,
                    "{s}/bin/buzz",
                    .{
                        try Parser.buzzPrefix(allocator),
                    },
                ) catch unreachable;
                defer allocator.free(arg0);

                const result = try std.process.Child.run(
                    .{
                        .allocator = allocator,
                        .argv = ([_][]const u8{
                            arg0,
                            "-t",
                            file_name,
                        })[0..],
                    },
                );

                if (!std.mem.containsAtLeast(u8, result.stderr, 1, first_line[2..])) {
                    fail_count += 1;
                    io.print(
                        "Expected error `{s}` got `{s}`\n",
                        .{
                            first_line[2..],
                            result.stderr,
                        },
                    );

                    io.print("\u{001b}[31m[{s}... ✕]\u{001b}[0m\n", .{file.name});
                } else {
                    io.print("\u{001b}[32m[{s}... ✓]\u{001b}[0m\n", .{file.name});
                }
            }
        }
    }

    if (fail_count == 0) {
        io.print("\n\u{001b}[32m", .{});
    } else {
        io.print("\n\u{001b}[31m", .{});
    }

    io.print("Ran {}, Failed: {}\u{001b}[0m\n", .{
        count,
        fail_count,
    });

    try std.testing.expect(fail_count == 0);

    return if (fail_count > 0) 1 else 0;
}
