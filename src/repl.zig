const std = @import("std");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const BuildOptions = @import("build_options");

const v = @import("vm.zig");
const obj = @import("obj.zig");
const Parser = @import("Parser.zig");
const JIT = @import("Jit.zig");
const ln = if (builtin.os.tag != .windows) @import("linenoise.zig") else void;
const Value = @import("value.zig").Value;
const disassembler = @import("disassembler.zig");
const CodeGen = @import("Codegen.zig");
const Scanner = @import("Scanner.zig");
const io = @import("io.zig");
const GC = @import("GC.zig");
const TypeRegistry = @import("TypeRegistry.zig");
const Runner = @import("Runner.zig");

pub const PROMPT = ">>> ";
pub const MULTILINE_PROMPT = "... ";

pub fn printBanner(out: *std.Io.Writer, full: bool) void {
    out.print(
        "👨‍🚀 buzz {f}-{s} Copyright (C) 2021-present Benoit Giannangeli\n",
        .{
            BuildOptions.version,
            BuildOptions.sha,
        },
    ) catch unreachable;

    if (full) {
        out.print(
            "Built with Zig {f} {s}\nAllocator: {s}, Memory limit: {} {s}\nJIT: {s}, CPU limit: {} {s}\n",
            .{
                builtin.zig_version,
                switch (builtin.mode) {
                    .ReleaseFast => "release-fast",
                    .ReleaseSafe => "release-safe",
                    .ReleaseSmall => "release-small",
                    .Debug => "debug",
                },
                if (builtin.mode == .Debug or is_wasm)
                    "gpa"
                else if (BuildOptions.mimalloc)
                    "mimalloc"
                else
                    "c_allocator",
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

pub fn repl(allocator: std.mem.Allocator) !void {
    var envMap = try std.process.getEnvMap(allocator);
    defer envMap.deinit();
    const colorterm = envMap.get("COLORTERM");
    const true_color = if (colorterm) |ct|
        std.mem.eql(u8, ct, "24bit") or std.mem.eql(u8, ct, "truecolor")
    else
        false;

    var runner: Runner = undefined;
    try runner.init(allocator, .Repl, null);
    defer runner.deinit();

    var stdout = io.stdoutWriter;
    var stderr = io.stderrWriter;
    printBanner(stdout, false);

    var buzz_history_path = std.Io.Writer.Allocating.init(allocator);
    defer buzz_history_path.deinit();

    try buzz_history_path.writer.print(
        "{s}/.buzz_history\x00",
        .{envMap.get("HOME") orelse "."},
    );

    // Setup linenoise
    if (ln != void) {
        _ = ln.linenoiseHistorySetMaxLen(100);
        _ = ln.linenoiseHistoryLoad(@ptrCast(buzz_history_path.written().ptr));
    }

    // Import std and debug as commodity
    _ = runner.runSource("import \"std\"; import \"debug\";", "REPL") catch unreachable;

    var previous_global_top = runner.vm.globals_count;
    var previous_parser_globals = try runner.parser.globals.clone(allocator);
    var previous_globals = try runner.vm.globals.clone(allocator);
    var previous_type_registry = try runner.gc.type_registry.registry.clone(allocator);
    var previous_input: ?[]u8 = null;

    var reader_buffer = [_]u8{0};
    var stdin_reader = std.fs.File.stdin().reader(reader_buffer[0..]);
    var reader = io.AllocatedReader.init(
        allocator,
        &stdin_reader.interface,
        null,
    );

    while (true) {
        if (builtin.os.tag == .windows) {
            std.io.getStdOut().writeAll(
                if (previous_input != null)
                    MULTILINE_PROMPT
                else
                    PROMPT,
            ) catch @panic("Could not write to stdout");
        }

        const read_source = if (ln != void)
            ln.linenoise(
                if (previous_input != null)
                    MULTILINE_PROMPT
                else
                    PROMPT,
            )
        else // FIXME: in that case, at least use an arena?
            reader.readUntilDelimiterOrEof('\n') catch @panic("Could not read stdin");

        if (read_source == null) {
            std.process.exit(0);
        }

        var source = if (builtin.os.tag == .windows) read_source.? else std.mem.span(read_source.?);
        const original_source = source;

        if (source.len > 0) {
            // Highlight input
            var source_scanner = Scanner.init(
                runner.gc.allocator,
                "REPL",
                original_source,
            );
            // Go up one line, erase it
            stdout.print(
                if (builtin.os.tag == .windows)
                    "{s}"
                else
                    "\x1b[1A\r\x1b[2K{s}",
                .{
                    if (previous_input != null)
                        MULTILINE_PROMPT
                    else
                        PROMPT,
                },
            ) catch unreachable;
            // Output highlighted user input
            source_scanner.highlight(stdout, true_color);
            stdout.writeAll("\n") catch unreachable;

            if (previous_input) |previous| {
                source = std.mem.concatWithSentinel(
                    runner.gc.allocator,
                    u8,
                    &.{
                        previous,
                        source,
                    },
                    0,
                ) catch @panic("Out of memory");
                runner.gc.allocator.free(previous);
                previous_input = null;
            }

            const expr = runner.runSource(source, "REPL") catch |err| failed: {
                if (BuildOptions.debug) {
                    stderr.print("Failed with error {}\n", .{err}) catch unreachable;
                }

                break :failed null;
            };

            if (runner.parser.reporter.last_error == null and runner.codegen.reporter.last_error == null) {
                if (ln != void) {
                    _ = ln.linenoiseHistoryAdd(source);
                    _ = ln.linenoiseHistorySave(@ptrCast(buzz_history_path.written().ptr));
                }
                // FIXME: why can't I deinit those?
                // previous_parser_globals.deinit();
                previous_parser_globals = try runner.parser.globals.clone(allocator);
                // previous_globals.deinit();
                previous_globals = try runner.vm.globals.clone(allocator);
                // previous_type_registry.deinit();
                previous_type_registry = try runner.gc.type_registry.registry.clone(allocator);

                // Dump top of stack
                if (previous_global_top != runner.vm.globals_count or expr != null) {
                    previous_global_top = runner.vm.globals_count;

                    const value = expr orelse runner.vm.globals.items[previous_global_top];

                    var value_str = std.Io.Writer.Allocating.init(runner.vm.gc.allocator);
                    defer value_str.deinit();

                    var state = disassembler.DumpState{
                        .vm = &runner.vm,
                    };

                    state.valueDump(
                        value,
                        &value_str.writer,
                        false,
                    );

                    var scanner = Scanner.init(
                        runner.gc.allocator,
                        "REPL",
                        value_str.written(),
                    );
                    scanner.highlight(stdout, true_color);

                    stdout.writeAll("\n") catch unreachable;
                }
            } else {
                // We might have declared new globals, types, etc. and encounter an error
                // FIXME: why can't I deinit those?
                // parser.globals.deinit();
                runner.parser.globals = previous_parser_globals;

                // vm.globals.deinit();
                runner.vm.globals = previous_globals;
                runner.vm.globals_count = previous_global_top;

                // gc.type_registry.registry.deinit();
                runner.gc.type_registry.registry = previous_type_registry;

                // If syntax error was unclosed block, keep previous input
                if (runner.parser.reporter.last_error == .unclosed) {
                    previous_input = runner.gc.allocator.alloc(u8, source.len) catch @panic("Out of memory");
                    std.mem.copyForwards(u8, previous_input.?, source);
                } else if (ln != void) {
                    _ = ln.linenoiseHistoryAdd(source);
                    _ = ln.linenoiseHistorySave(@ptrCast(buzz_history_path.written().ptr));
                }
            }

            runner.parser.reporter.last_error = null;
            runner.parser.reporter.panic_mode = false;
            runner.codegen.reporter.last_error = null;
            runner.codegen.reporter.panic_mode = false;
        }
    }
}
