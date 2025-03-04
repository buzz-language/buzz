const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const RunFlavor = @import("vm.zig").RunFlavor;
const Parser = @import("Parser.zig");
const BuildOptions = @import("build_options");
const clap = @import("clap");
const is_wasm = builtin.cpu.arch.isWasm();
const repl = if (!is_wasm) @import("repl.zig").repl else void;
const wasm_repl = @import("wasm_repl.zig");
const io = @import("io.zig");
const run = @import("run.zig");

pub export const initRepl_export = wasm_repl.initRepl;
pub export const runLine_export = wasm_repl.runLine;

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

fn printBanner(out: anytype, full: bool) void {
    out.print(
        "👨‍🚀 buzz {}-{s} Copyright (C) 2021-present Benoit Giannangeli\n",
        .{
            BuildOptions.version,
            BuildOptions.sha,
        },
    ) catch unreachable;

    if (full) {
        out.print(
            "Built with Zig {} {s}\nAllocator: {s}, Memory limit: {} {s}\nJIT: {s}, CPU limit: {} {s}\n",
            .{
                builtin.zig_version,
                @tagName(builtin.mode),
                if (builtin.mode == .Debug)
                    "gpa"
                else if (BuildOptions.mimalloc) "mimalloc" else "c_allocator",
                if (BuildOptions.memory_limit) |ml|
                    ml
                else
                    0,
                if (BuildOptions.memory_limit != null)
                    "bytes"
                else
                    "(unlimited)",
                if (BuildOptions.jit and BuildOptions.cycle_limit == null)
                    "on"
                else
                    "off",
                if (BuildOptions.cycle_limit) |cl| cl else 0,
                if (BuildOptions.cycle_limit != null) "cycles" else "(unlimited)",
            },
        ) catch unreachable;
    }
}

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = builtin.mode == .Debug }){};
    const allocator: std.mem.Allocator = if (builtin.mode == .Debug or is_wasm)
        gpa.allocator()
    else if (BuildOptions.mimalloc)
        @import("mimalloc.zig").mim_allocator
    else
        std.heap.c_allocator;

    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Show help and exit
        \\-t, --test             Run test blocks in provided script
        \\-c, --check            Check script for error without running it
        \\-v, --version          Print version and exit
        \\-L, --library <str>... Add search path for external libraries
        \\<str>...               Script to run followed by its eventual arguments
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
        diag.report(io.stdErrWriter, err) catch {};
        return 1;
    };
    defer res.deinit();

    if (res.args.version == 1) {
        printBanner(io.stdOutWriter, true);

        return 0;
    }

    if (res.args.help == 1) {
        io.print("👨‍🚀 buzz A small/lightweight typed scripting language\n\nUsage: buzz ", .{});

        clap.usage(
            io.stdErrWriter,
            clap.Help,
            &params,
        ) catch return 1;

        io.print("\n\n", .{});

        clap.help(
            io.stdErrWriter,
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

    if (res.args.library.len > 0) {
        var list = std.ArrayList([]const u8).init(allocator);

        for (res.args.library) |path| {
            list.append(path) catch return 1;
        }

        Parser.user_library_paths = list.toOwnedSlice() catch return 1;
    }

    const flavor: RunFlavor = if (res.args.check == 1)
        .Check
    else if (res.args.@"test" == 1)
        .Test
    else if (res.positionals[0].len == 0)
        .Repl
    else
        .Run;

    if (!is_wasm and flavor == .Repl) {
        repl(allocator) catch {
            return 1;
        };
    } else if (!is_wasm and res.positionals[0].len > 0) {
        run.runFile(
            allocator,
            flavor,
            res.positionals[0][0],
            res.positionals[0],
        ) catch {
            return 1;
        };
    } else if (is_wasm) {
        io.print("NYI wasm repl", .{});
    } else {
        io.print("Nothing to run", .{});
    }

    return 0;
}

test "Testing behavior" {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .safety = builtin.mode == .Debug,
    }){};
    var allocator: Allocator = gpa.allocator();

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
                run.runFile(
                    allocator,
                    .Test,
                    try std.fmt.bufPrint(file_name, "tests/{s}", .{file.name}),
                    &[_][:0]u8{},
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

    std.process.exit(if (fail_count > 0) 1 else 0);
}
