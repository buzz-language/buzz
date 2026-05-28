//! Because of https://github.com/ziglang/zig/issues/15091 test that write to stdout will hang
//! However I think it's completely legitimate for a tested code to output to stdout and I
//! don't really get why zig test needs to use stdout anyway

const std = @import("std");
const Runner = @import("Runner.zig");
const bz_io = @import("io.zig");
const Parser = @import("Parser.zig");
const BuildOptions = @import("build_options");
const clap = @import("clap");
const Perf = @import("Perf.zig");
const is_windows = @import("builtin").os.tag == .windows;
const is_linux = @import("builtin").os.tag == .linux;

const black_listed_tests = std.StaticStringMap(void).initComptime(
    if (is_windows)
        .{
            .{ "tests/fuzzed/id_000434,sig_06,src_000723,time_202384530,execs_828228,op_arith8,pos_276,val_-1.buzz", {} },
            .{ "tests/behavior/ffi.buzz", {} },
            .{ "tests/behavior/types-as-value.buzz", {} },
        }
    else if (is_linux)
        .{
            .{ "tests/fuzzed/id_000434,sig_06,src_000723,time_202384530,execs_828228,op_arith8,pos_276,val_-1.buzz", {} },
            .{ "tests/behavior/toml.buzz", {} }, // FIXME: find out why this fails in the CI only
        }
    else
        .{
            .{ "tests/fuzzed/id_000434,sig_06,src_000723,time_202384530,execs_828228,op_arith8,pos_276,val_-1.buzz", {} },
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

fn testBehaviors(process: std.process.Init, allocator: std.mem.Allocator, fail_fast: bool, perf: ?*Perf) !Result {
    var result = Result{};

    const dirs = [_][]const u8{ "tests/behavior", "tests" };

    for (dirs) |dir| {
        var test_dir = try std.Io.Dir.cwd().openDir(process.io, dir, .{ .iterate = true });
        defer test_dir.close(process.io);
        var it = test_dir.iterate();

        while (try it.next(process.io)) |file| : (result.total += 1) {
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
                    printFileStatus(process.io, file.name, .skipped);
                    result.skipped += 1;
                    continue;
                }

                printFileStatus(process.io, file.name, .started);

                var had_error: bool = false;
                var runner: Runner = undefined;
                try runner.init(process, allocator, .Test, null, perf);
                defer runner.deinit();

                var failed = false;
                _ = runner.runFile(
                    file_name.written(),
                    &[_][:0]u8{},
                ) catch {
                    failed = true;
                    had_error = true;
                    try result.failed.append(allocator, try file_name.toOwnedSlice());

                    if (fail_fast) {
                        break;
                    }
                };

                printFileStatus(process.io, file.name, if (failed) .failed else .succeeded);
            }
        }
    }

    return result;
}

fn testCompileErrors(process: std.process.Init, allocator: std.mem.Allocator, fail_fast: bool) !Result {
    var result = Result{};

    var test_dir = try std.Io.Dir.cwd().openDir(process.io, "tests/compile_errors", .{ .iterate = true });
    defer test_dir.close(process.io);
    var it = test_dir.iterate();

    while (try it.next(process.io)) |file| : (result.total += 1) {
        if (file.kind == .file and std.mem.endsWith(u8, file.name, ".buzz")) {
            var file_name = std.Io.Writer.Allocating.init(allocator);
            defer file_name.deinit();
            try file_name.writer.print("tests/compile_errors/{s}", .{file.name});

            if (black_listed_tests.has(file_name.written())) {
                printFileStatus(process.io, file.name, .skipped);
                result.skipped += 1;
                continue;
            }

            printFileStatus(process.io, file.name, .started);

            // First line of test file is expected error message
            const test_file = try std.Io.Dir.cwd().openFile(
                process.io,
                file_name.written(),
                .{ .mode = .read_only },
            );
            var buffer = [_]u8{0} ** 255;
            var file_reader = test_file.reader(process.io, buffer[0..]);
            var reader = bz_io.AllocatedReader.init(
                allocator,
                &file_reader.interface,
                null,
            );

            const first_line = (try reader.readUntilDelimiterOrEof('\n')).?;
            defer allocator.free(first_line);
            const expected_error = std.mem.trim(u8, first_line[2..], "\r");

            test_file.close(process.io);

            const arg0 = std.fmt.allocPrint(
                allocator,
                "{s}/bin/buzz",
                .{
                    Parser.buzzPrefix(process.io, process.environ_map),
                },
            ) catch unreachable;
            defer allocator.free(arg0);

            const run_result = try std.process.run(allocator, process.io, .{
                .argv = &.{
                    arg0,
                    "-t",
                    file_name.written(),
                },
            });
            defer allocator.free(run_result.stdout);
            defer allocator.free(run_result.stderr);

            if (!std.mem.containsAtLeast(u8, run_result.stderr, 1, expected_error)) {
                printFileStatus(process.io, file.name, .failed);
                try result.failed.append(allocator, try file_name.toOwnedSlice());

                if (fail_fast) {
                    break;
                }
            } else {
                printFileStatus(process.io, file.name, .succeeded);
            }
        }
    }

    return result;
}

fn testFuzzCrashes(process: std.process.Init, allocator: std.mem.Allocator, fail_fast: bool) !Result {
    var result = Result{};

    // Re open in to write new resolved tests
    const dir = "tests/fuzzed";
    var test_dir = try std.Io.Dir.cwd().openDir(process.io, dir, .{ .iterate = true });
    defer test_dir.close(process.io);
    var it = test_dir.iterate();

    while (try it.next(process.io)) |file| : (result.total += 1) {
        if (file.kind == .file) {
            var file_name = std.Io.Writer.Allocating.init(allocator);
            defer file_name.deinit();
            try file_name.writer.print("{s}/{s}", .{ dir, file.name });

            if (black_listed_tests.has(file_name.written())) {
                result.skipped += 1;
                printFileStatus(process.io, file.name, .skipped);
                continue;
            }

            printFileStatus(process.io, file.name, .started);

            const arg0 = std.fmt.allocPrint(
                allocator,
                "{s}/bin/buzz",
                .{
                    Parser.buzzPrefix(process.io, process.environ_map),
                },
            ) catch unreachable;
            defer allocator.free(arg0);

            const run_result = std.process.run(
                allocator,
                process.io,
                .{
                    .argv = &.{
                        arg0,
                        "-t",
                        file_name.written(),
                    },
                    .stdout_limit = .unlimited,
                    .stderr_limit = .unlimited,
                    .timeout = .{
                        .duration = .{
                            .clock = .awake,
                            .raw = .fromSeconds(10),
                        },
                    },
                },
            ) catch |err| switch (err) {
                error.Timeout => {
                    try result.hanged.append(allocator, try file_name.toOwnedSlice());
                    if (fail_fast) break;
                    continue;
                },
                else => return err,
            };
            defer allocator.free(run_result.stdout);
            defer allocator.free(run_result.stderr);

            switch (run_result.term) {
                .exited => {
                    printFileStatus(process.io, file.name, .succeeded);
                },
                else => {
                    printFileStatus(process.io, file.name, .failed);
                    try result.failed.append(allocator, try file_name.toOwnedSlice());
                    if (fail_fast) break;
                },
            }
        }
    }

    return result;
}

const Status = enum {
    started,
    skipped,
    failed,
    succeeded,
};

fn printFileStatus(io: std.Io, file_name: []const u8, status: Status) void {
    if (is_windows) {
        bz_io.print(
            io,
            "[{s} {s}]\n",
            .{
                file_name,
                switch (status) {
                    .started => "...",
                    .failed => "FAILED",
                    .succeeded => "OK",
                    .skipped => "SKIP",
                },
            },
        );
        return;
    }

    bz_io.print(
        io,
        "{s}\x1b[{s}m[{s} {s}]\x1b[0m\n",
        .{
            switch (status) {
                .failed, .succeeded, .skipped => "\x1b[1A\r\x1b[2K",
                else => "",
            },
            switch (status) {
                .started => "2",
                .failed => "31",
                .succeeded => "32",
                .skipped => "33",
            },
            file_name,
            switch (status) {
                .started => "...",
                .failed => "✘",
                .succeeded => "✔",
                .skipped => "○",
            },
        },
    );
}

pub fn main(init: std.process.Init) !u8 {
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

    var stderr_writer = bz_io.stderrWriter(init.io);

    var diag = clap.Diagnostic{};
    var res = clap.parse(
        clap.Help,
        &params,
        clap.parsers.default,
        init.minimal.args,
        .{
            .allocator = allocator,
            .diagnostic = &diag,
        },
    ) catch |err| {
        // Report useful error and exit
        diag.report(&stderr_writer.interface, err) catch {};
        return 1;
    };
    defer res.deinit();

    if (res.args.help == 1) {
        bz_io.print(
            init.io,
            if (is_windows)
                "Behavior tests for the buzz programming language\n\nUsage: buzz_behavior "
            else
                "👨‍🚀 Behavior tests for the buzz programming language\n\nUsage: buzz_behavior ",
            .{},
        );

        clap.usage(
            &stderr_writer.interface,
            clap.Help,
            &params,
        ) catch return 1;

        bz_io.print(init.io, "\n\n", .{});

        clap.help(
            &stderr_writer.interface,
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

    var perf: ?Perf = if (BuildOptions.show_perf) Perf.init(init.io) else null;
    defer if (perf) |*p| p.report();

    const do_all = res.args.all == 1 or (res.args.behavior != 1 and res.args.@"compile-error" != 1 and res.args.fuzz != 1);

    if (do_all or res.args.behavior == 1) {
        bz_io.print(
            init.io,
            if (is_windows) "\nBehavior tests...\n" else "\n\x1b[34m■ Behavior tests\x1b[0m...\n",
            .{},
        );
        var tests_result = try testBehaviors(init, allocator, res.args.fast == 1, if (perf) |*p| p else null);
        try result.merge(
            allocator,
            &tests_result,
        );
    }

    if (do_all or res.args.@"compile-error" == 1) {
        bz_io.print(
            init.io,
            if (is_windows) "\nCompile errors...\n" else "\n\x1b[34m■ Compile errors\x1b[0m...\n",
            .{},
        );
        var tests_result = try testCompileErrors(init, allocator, res.args.fast == 1);
        try result.merge(
            allocator,
            &tests_result,
        );
    }

    if (!is_windows and (do_all or res.args.fuzz == 1)) {
        bz_io.print(init.io, "\n\x1b[34m■ Fuzz tests\x1b[0m...\n", .{});
        var tests_result = try testFuzzCrashes(init, allocator, res.args.fast == 1);
        try result.merge(
            allocator,
            &tests_result,
        );
    }

    if (result.failed.items.len > 0) {
        bz_io.print(init.io, "Failed tests:\n", .{});
        for (result.failed.items) |failed| {
            bz_io.print(init.io, if (is_windows) "  {s}\n" else "  \x1b[31m{s}\x1b[0m\n", .{failed});
        }
    }

    if (result.hanged.items.len > 0) {
        bz_io.print(init.io, "Hanged tests:\n", .{});
        for (result.hanged.items) |hanged| {
            bz_io.print(init.io, if (is_windows) "  {s}\n" else "  \x1b[31m{s}\x1b[0m\n", .{hanged});
        }
    }

    if (is_windows) {
        bz_io.print(init.io, "\nRan {}, Ok: {}, Failed: {}, Hanged {}, Skipped {}\n", .{
            result.total,
            result.total - result.failed.items.len - result.hanged.items.len,
            result.failed.items.len,
            result.hanged.items.len,
            result.skipped,
        });
    } else {
        bz_io.print(init.io, "\n\x1b[{s}mRan {}, Ok: {}, Failed: {}, Hanged {}, Skipped {}\x1b[0m\n", .{
            if (result.hasFailed()) "31" else "32",
            result.total,
            result.total - result.failed.items.len - result.hanged.items.len,
            result.failed.items.len,
            result.hanged.items.len,
            result.skipped,
        });
    }

    return if (result.hasFailed()) 1 else 0;
}
