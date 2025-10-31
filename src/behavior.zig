const std = @import("std");
const Runner = @import("Runner.zig");
const io = @import("io.zig");
const Parser = @import("Parser.zig");
const BuildOptions = @import("build_options");
const clap = @import("clap");

// Because of https://github.com/ziglang/zig/issues/15091 test that write to stdout will hang
// However I think its completely legitimate for a tested code to output to stdout and I
// don't really get why zig test needs to use stdout anyway

const black_listed_tests = std.StaticStringMap(void).initComptime(
    .{
        .{ "tests/behavior/027-run-file.buzz", {} },
    },
);

const Result = struct {
    total: usize = 0,
    failed: usize = 0,
    skipped: usize = 0,
};

fn testBehaviors(allocator: std.mem.Allocator, fail_fast: bool) !Result {
    var count: usize = 0;
    var fail_count: usize = 0;
    var skipped: usize = 0;

    const dirs = [_][]const u8{ "tests/behavior", "tests" };

    for (dirs) |dir| {
        var test_dir = try std.fs.cwd().openDir(
            dir,
            .{
                .iterate = true,
            },
        );
        var it = test_dir.iterate();

        while (try it.next()) |file| : (count += 1) {
            if (file.kind == .file and std.mem.endsWith(u8, file.name, ".buzz")) {
                var file_name = std.Io.Writer.Allocating.init(allocator);
                defer file_name.deinit();
                try file_name.writer.print(
                    "{s}/{s}",
                    .{
                        dir,
                        file.name,
                    },
                );

                if (black_listed_tests.has(file_name.written())) {
                    skipped += 1;
                    io.print("\u{001b}[33m[{s} >>]\u{001b}[0m\n", .{file_name.written()});
                    continue;
                }

                io.print("\u{001b}[33m[{s} ...]\u{001b}[0m\n", .{file_name.written()});

                var had_error: bool = false;
                var runner: Runner = undefined;
                try runner.init(allocator, .Test, null);

                runner.runFile(
                    file_name.written(),
                    &[_][:0]u8{},
                ) catch {
                    io.print("\u{001b}[31m[{s} ✕]\u{001b}[0m\n", .{file.name});
                    had_error = true;
                    fail_count += 1;

                    if (fail_fast) {
                        break;
                    }
                };

                if (!had_error) {
                    io.print("\u{001b}[32m[{s} ✓]\u{001b}[0m\n", .{file.name});
                }
            }
        }
    }

    return .{
        .total = count,
        .failed = fail_count,
        .skipped = skipped,
    };
}

fn testCompileErrors(allocator: std.mem.Allocator, fail_fast: bool) !Result {
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
            var file_name = std.Io.Writer.Allocating.init(allocator);
            defer file_name.deinit();
            try file_name.writer.print("tests/compile_errors/{s}", .{file.name});

            if (black_listed_tests.has(file_name.written())) {
                skipped += 1;
                io.print("\u{001b}[33m[{s} >>]\u{001b}[0m\n", .{file.name});
                continue;
            }

            // First line of test file is expected error message
            const test_file = try std.fs.cwd().openFile(
                file_name.written(),
                .{
                    .mode = .read_only,
                },
            );
            var buffer = [_]u8{0} ** 255;
            var file_reader = test_file.reader(buffer[0..]);
            var reader = io.AllocatedReader.init(
                allocator,
                &file_reader.interface,
                null,
            );

            const first_line = (try reader.readUntilDelimiterOrEof('\n')).?;
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
                        file_name.written(),
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

                if (fail_fast) {
                    break;
                }
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

fn testFuzzCrashes(allocator: std.mem.Allocator, fail_fast: bool) !Result {
    var count: usize = 0;
    var fail_count: usize = 0;
    var skipped: usize = 0;

    // Get resolved tests
    var resolved = std.StringArrayHashMapUnmanaged(void).empty;

    var resolved_file = try std.fs.cwd().createFile(
        "dist/resolved.txt",
        .{
            .truncate = false,
            .read = true,
        },
    );

    const raw = try allocator.alloc(u8, (try resolved_file.stat()).size);
    defer allocator.free(raw);

    _ = try resolved_file.readAll(raw);

    {
        var it = std.mem.splitAny(u8, raw, "\n");
        while (it.next()) |r| {
            try resolved.put(allocator, r, {});
        }
    }

    resolved_file.close();

    // Re open in to write new resolved tests
    resolved_file = try std.fs.cwd().openFile(
        "dist/resolved.txt",
        .{ .mode = .write_only },
    );
    defer resolved_file.close();
    try resolved_file.seekFromEnd(0);

    var resolved_writer = resolved_file.writer(&.{});

    const exists = if (std.fs.cwd().access(
        "dist/default/crashes",
        .{ .mode = .read_only },
    )) |_|
        true
    else |_|
        false;

    if (exists) {
        var test_dir = try std.fs.cwd().openDir(
            "dist/default/crashes",
            .{
                .iterate = true,
            },
        );
        var it = test_dir.iterate();

        while (try it.next()) |file| : (count += 1) {
            if (file.kind == .file) {
                // Was it resolved?
                if (resolved.get(file.name) != null) {
                    io.print("\u{001b}[33m[{s} ✓]\u{001b}[0m (previously resolved)\n", .{file.name});
                    continue;
                }

                var file_name = std.Io.Writer.Allocating.init(allocator);
                defer file_name.deinit();
                try file_name.writer.print("dist/default/crashes/{s}", .{file.name});

                if (black_listed_tests.has(file_name.written())) {
                    skipped += 1;
                    io.print("\u{001b}[33m[{s} >>]\u{001b}[0m\n", .{file.name});
                    continue;
                }

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
                            file_name.written(),
                        },
                    },
                );

                if (result.term != .Exited) {
                    fail_count += 1;
                    io.print("\u{001b}[31m[{s}... ✕]\u{001b}[0m\n", .{file.name});

                    if (fail_fast) {
                        break;
                    }
                } else {
                    try resolved_writer.interface.print("{s}\n", .{file.name});
                    try resolved_writer.interface.flush();
                    io.print("\u{001b}[32m[{s}... ✓]\u{001b}[0m\n", .{file.name});
                }
            }
        }
    } else {
        io.print("\u{001b}[33mNo fuzz tests to check\u{001b}[0m\n", .{});
    }

    return .{
        .total = count,
        .failed = fail_count,
        .skipped = skipped,
    };
}

pub fn main() !u8 {
    // DebugAllocator recently got super slow, will put this back on once its fixed
    // var gpa = std.heap.DebugAllocator(.{ .safety = builtin.mode == .Debug }){};
    // const allocator = if (builtin.mode == .Debug or is_wasm)
    //     gpa.allocator()
    // else
    const allocator = if (BuildOptions.mimalloc)
        @import("mimalloc.zig").mim_allocator
    else
        std.heap.c_allocator;

    const params = comptime clap.parseParamsComptime(
        \\-h, --help          Show help and exit
        \\-a, --all           Run all tests
        \\-b, --behavior      Run behavior tests
        \\-c, --compile-error Run compile error tests
        \\-f, --fuzz          Run fuzz tests
        \\--fast              Fail fast
        \\
    );

    var diag = clap.Diagnostic{};
    var res = clap.parse(
        clap.Help,
        &params,
        clap.parsers.default,
        .{
            .allocator = allocator,
            .diagnostic = &diag,
        },
    ) catch |err| {
        // Report useful error and exit
        diag.report(io.stderrWriter, err) catch {};
        return 1;
    };
    defer res.deinit();

    if (res.args.help == 1) {
        io.print("👨‍🚀 Behavior tests for the buzz programming language\n\nUsage: buzz_behavior ", .{});

        clap.usage(
            io.stderrWriter,
            clap.Help,
            &params,
        ) catch return 1;

        io.print("\n\n", .{});

        clap.help(
            io.stderrWriter,
            clap.Help,
            &params,
            .{
                .description_on_new_line = false,
                .description_indent = 4,
                .spacing_between_parameters = 0,
            },
        ) catch return 1;

        return 0;
    }

    var count: usize = 0;
    var fail_count: usize = 0;
    var skipped: usize = 0;

    const do_all = res.args.all == 1 or (res.args.behavior != 1 and res.args.@"compile-error" != 1 and res.args.fuzz != 1);

    const behaviors_result: Result = if (do_all or res.args.behavior == 1)
        try testBehaviors(allocator, res.args.fast == 1)
    else
        .{};

    const comp_result: Result = if (do_all or res.args.@"compile-error" == 1)
        try testCompileErrors(allocator, res.args.fast == 1)
    else
        .{};

    const fuzz_result: Result = if (do_all or res.args.fuzz == 1)
        try testFuzzCrashes(allocator, res.args.fast == 1)
    else
        .{};

    count += comp_result.total + behaviors_result.total + fuzz_result.total;
    fail_count += comp_result.failed + behaviors_result.failed + fuzz_result.failed;
    skipped += comp_result.skipped + behaviors_result.skipped + fuzz_result.skipped;

    if (fail_count == 0) {
        io.print("\n\u{001b}[32m", .{});
    } else {
        io.print("\n\u{001b}[31m", .{});
    }

    io.print("Ran {}, Ok: {}, Failed: {}, Skipped {}\u{001b}[0m\n", .{
        count,
        count - fail_count,
        fail_count,
        skipped,
    });

    return if (fail_count > 0) 1 else 0;
}
