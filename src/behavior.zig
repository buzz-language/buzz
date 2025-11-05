//! Because of https://github.com/ziglang/zig/issues/15091 test that write to stdout will hang
//! However I think its completely legitimate for a tested code to output to stdout and I
//! don't really get why zig test needs to use stdout anyway

const std = @import("std");
const Runner = @import("Runner.zig");
const io = @import("io.zig");
const Parser = @import("Parser.zig");
const BuildOptions = @import("build_options");
const clap = @import("clap");

const black_listed_tests = std.StaticStringMap(void).initComptime(
    .{
        .{ "tests/behavior/027-run-file.buzz", {} },
    },
);

const Result = struct {
    total: usize = 0,
    failed: std.ArrayList([]const u8) = .empty,
    skipped: usize = 0,
    hanged: std.ArrayList([]const u8) = .empty,

    pub fn hasFailed(self: *@This()) bool {
        return self.failed.items.len > 0 or self.hanged.items.len > 0;
    }

    pub fn merge(self: *@This(), allocator: std.mem.Allocator, other: *@This()) error{OutOfMemory}!void {
        self.total += other.total;
        self.skipped += other.skipped;

        try self.failed.appendSlice(allocator, other.failed.items);
        try self.hanged.appendSlice(allocator, other.hanged.items);
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        for (self.failed.items) |f| {
            allocator.free(f);
        }

        for (self.hanged.items) |f| {
            allocator.free(f);
        }

        self.failed.deinit(allocator);
        self.hanged.deinit(allocator);
    }
};

fn testBehaviors(allocator: std.mem.Allocator, fail_fast: bool) !Result {
    var result = Result{};

    const dirs = [_][]const u8{ "tests/behavior", "tests" };

    for (dirs) |dir| {
        var test_dir = try std.fs.cwd().openDir(
            dir,
            .{
                .iterate = true,
            },
        );
        var it = test_dir.iterate();

        while (try it.next()) |file| : (result.total += 1) {
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
                    result.skipped += 1;
                    continue;
                }

                // io.print("\u{001b}[33m[{s} ...]\u{001b}[0m\n", .{file_name.written()});

                var had_error: bool = false;
                var runner: Runner = undefined;
                try runner.init(allocator, .Test, null);

                runner.runFile(
                    file_name.written(),
                    &[_][:0]u8{},
                ) catch {
                    had_error = true;
                    try result.failed.append(allocator, try file_name.toOwnedSlice());

                    if (fail_fast) {
                        break;
                    }
                };
            }
        }
    }

    return result;
}

fn testCompileErrors(allocator: std.mem.Allocator, fail_fast: bool) !Result {
    var result = Result{};

    var test_dir = try std.fs.cwd().openDir(
        "tests/compile_errors",
        .{
            .iterate = true,
        },
    );
    var it = test_dir.iterate();

    while (try it.next()) |file| : (result.total += 1) {
        if (file.kind == .file and std.mem.endsWith(u8, file.name, ".buzz")) {
            var file_name = std.Io.Writer.Allocating.init(allocator);
            defer file_name.deinit();
            try file_name.writer.print("tests/compile_errors/{s}", .{file.name});

            if (black_listed_tests.has(file_name.written())) {
                result.skipped += 1;
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

            const run_result = try std.process.Child.run(
                .{
                    .allocator = allocator,
                    .argv = &.{
                        arg0,
                        "-t",
                        file_name.written(),
                    },
                },
            );

            if (!std.mem.containsAtLeast(u8, run_result.stderr, 1, first_line[2..])) {
                // io.print(
                //     "Expected error `{s}` got `{s}`\n",
                //     .{
                //         first_line[2..],
                //         run_result.stderr,
                //     },
                // );

                try result.failed.append(allocator, try file_name.toOwnedSlice());

                if (fail_fast) {
                    break;
                }
            }
        }
    }

    return result;
}

fn testFuzzCrashes(allocator: std.mem.Allocator, fail_fast: bool) !Result {
    var result = Result{};

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

    const dir = "tests/fuzzed";
    var test_dir = try std.fs.cwd().openDir(
        dir,
        .{
            .iterate = true,
        },
    );
    var it = test_dir.iterate();

    while (try it.next()) |file| : (result.total += 1) {
        if (file.kind == .file) {
            // Was it resolved?
            if (resolved.get(file.name) != null) {
                continue;
            }

            var file_name = std.Io.Writer.Allocating.init(allocator);
            defer file_name.deinit();
            try file_name.writer.print("{s}/{s}", .{ dir, file.name });

            if (black_listed_tests.has(file_name.written())) {
                result.skipped += 1;
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

            var child = std.process.Child.init(
                &.{
                    arg0,
                    "-t",
                    file_name.written(),
                },
                allocator,
            );
            child.stdout_behavior = .Ignore;
            child.stderr_behavior = .Ignore;

            try child.spawn();

            // Run in a thread so we can abort hanging tests
            var done = false;
            const thread = try std.Thread.spawn(
                .{
                    .allocator = allocator,
                },
                struct {
                    fn wait(process: *std.process.Child, over: *bool) void {
                        _ = process.wait() catch @panic("Could not wait for buzz process");

                        over.* = true;
                    }
                }.wait,
                .{
                    &child,
                    &done,
                },
            );

            var timer = try std.time.Timer.start();
            while (!done and timer.read() < 2 * std.time.ns_per_s) {}

            if (child.term) |term| {
                thread.join();

                const uterm = term catch null;
                if (uterm == null or uterm.? != .Exited) {
                    try result.failed.append(allocator, try file_name.toOwnedSlice());

                    if (fail_fast) {
                        break;
                    }
                } else {
                    try resolved_writer.interface.print("{s}\n", .{file.name});
                    try resolved_writer.interface.flush();
                }
            } else {
                try result.hanged.append(allocator, try file_name.toOwnedSlice());
                std.posix.kill(child.id, std.posix.SIG.TERM) catch {};
                // FIXME: Not sure i understands this but it seems that child.kill() kills the process and then wait for the now
                // non existing process?
                // _ = child.kill() catch {};
                thread.join();

                if (fail_fast) {
                    break;
                }
            }
        }
    }

    return result;
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

    var result: Result = .{};
    defer result.deinit(allocator);

    const do_all = res.args.all == 1 or (res.args.behavior != 1 and res.args.@"compile-error" != 1 and res.args.fuzz != 1);

    if (do_all or res.args.behavior == 1) {
        var tests_result = try testBehaviors(allocator, res.args.fast == 1);
        try result.merge(
            allocator,
            &tests_result,
        );
    }

    if (do_all or res.args.@"compile-error" == 1) {
        var tests_result = try testCompileErrors(allocator, res.args.fast == 1);
        try result.merge(
            allocator,
            &tests_result,
        );
    }

    if (do_all or res.args.fuzz == 1) {
        var tests_result = try testFuzzCrashes(allocator, res.args.fast == 1);
        try result.merge(
            allocator,
            &tests_result,
        );
    }

    if (result.failed.items.len > 0) {
        io.print("Failed tests:\n", .{});
        for (result.failed.items) |failed| {
            io.print("  \u{001b}[31m{s}\u{001b}[0m\n", .{failed});
        }
    }

    if (result.hanged.items.len > 0) {
        io.print("Hanged tests:\n", .{});
        for (result.hanged.items) |hanged| {
            io.print("  \u{001b}[31m{s}\u{001b}[0m\n", .{hanged});
        }
    }

    if (result.hasFailed()) {
        io.print("\n\u{001b}[31m", .{});
    } else {
        io.print("\n\u{001b}[32m", .{});
    }

    io.print("Ran {}, Ok: {}, Failed: {}, Hanged {}, Skipped {}\u{001b}[0m\n", .{
        result.total,
        result.total - result.failed.items.len - result.hanged.items.len,
        result.failed.items.len,
        result.hanged.items.len,
        result.skipped,
    });

    return if (result.hasFailed()) 1 else 0;
}
