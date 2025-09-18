const std = @import("std");
const runFile = @import("main.zig").runFile;
const io = @import("io.zig");
const Parser = @import("Parser.zig");

// Because of https://github.com/ziglang/zig/issues/15091 test that write to stdout will hang
// However I think its completely legitimate for a tested code to output to stdout and I
// don't really get why zig test needs to use stdout anyway

const black_listed_tests = std.StaticStringMap(void).initComptime(
    .{
        // .{ "tests/022-io.buzz", {} },
    },
);

const Result = struct {
    total: usize,
    failed: usize,
    skipped: usize,
};

fn testBehaviors(allocator: std.mem.Allocator) !Result {
    var count: usize = 0;
    var fail_count: usize = 0;
    var skipped: usize = 0;

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
            _ = try std.fmt.bufPrint(file_name, "tests/{s}", .{file.name});
            defer allocator.free(file_name);

            if (black_listed_tests.has(file_name)) {
                skipped += 1;
                io.print("\u{001b}[33m[{s} >>]\u{001b}[0m\n", .{file_name});
                continue;
            }

            var had_error: bool = false;
            runFile(
                allocator,
                file_name,
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

    return .{
        .total = count,
        .failed = fail_count,
        .skipped = skipped,
    };
}

fn testCompileErrors(allocator: std.mem.Allocator) !Result {
    var count: usize = 0;
    var fail_count: usize = 0;
    var skipped: usize = 0;

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

            if (black_listed_tests.has(file_name)) {
                skipped += 1;
                io.print("\u{001b}[33m[{s} >>]\u{001b}[0m\n", .{file.name});
                continue;
            }

            // First line of test file is expected error message
            const test_file = try std.fs.cwd().openFile(
                file_name,
                .{
                    .mode = .read_only,
                },
            );
            var buffer = [_]u8{0} ** 255;
            var file_reader = test_file.reader(buffer[0..]);
            var reader = io.AllocatedReader{
                .reader = &file_reader.interface,
            };

            const first_line = (try reader.readUntilDelimiterOrEof(allocator, '\n')).?;
            defer allocator.free(first_line);

            test_file.close();

            const arg0 = std.fmt.allocPrint(
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
                    .argv = &.{
                        arg0,
                        "-t",
                        file_name,
                    },
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

    return .{
        .total = count,
        .failed = fail_count,
        .skipped = skipped,
    };
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .safety = true,
    }){};
    const allocator = gpa.allocator();
    var count: usize = 0;
    var fail_count: usize = 0;
    var skipped: usize = 0;

    const behaviors_result = try testBehaviors(allocator);
    const comp_result = try testCompileErrors(allocator);

    count += comp_result.total + behaviors_result.total;
    fail_count += comp_result.failed + behaviors_result.failed;
    skipped += comp_result.skipped + behaviors_result.skipped;

    if (fail_count == 0) {
        io.print("\n\u{001b}[32m", .{});
    } else {
        io.print("\n\u{001b}[31m", .{});
    }

    io.print("Ran {}, Failed: {}, Skipped {}\u{001b}[0m\n", .{
        count,
        fail_count,
        skipped,
    });

    try std.testing.expect(fail_count == 0);

    return if (fail_count > 0) 1 else 0;
}
