const std = @import("std");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const BuildOptions = @import("build_options");

const v = @import("vm.zig");
const obj = @import("obj.zig");
const Parser = @import("Parser.zig");
const JIT = @import("Jit.zig");
const ln = if (builtin.os.tag != .windows) @import("linenoise.zig") else void;
const disassembler = @import("disassembler.zig");
const CodeGen = @import("Codegen.zig");
const Scanner = @import("Scanner.zig");
const Renderer = @import("renderer.zig").Renderer;
const io = @import("io.zig");
const GC = @import("GC.zig");
const TypeRegistry = @import("TypeRegistry.zig");
const Runner = @import("Runner.zig");
const QualifiedNameContext = @import("Ast.zig").QualifiedName.Context;
const Perf = @import("Perf.zig");

pub const PROMPT = ">>> ";
pub const MULTILINE_PROMPT = "... ";
const linenoise_max_line = 4096;

pub fn printBanner(out: *std.Io.Writer, full: bool) void {
    out.print(
        "👨‍🚀 buzz {f} Copyright (C) 2021-present Benoit Giannangeli\n",
        .{
            BuildOptions.version,
        },
    ) catch unreachable;

    if (full) {
        out.print(
            "Built with Zig {f} {s}\nAllocator: {s}, Memory limit: {} {s}\nJIT: {s}, CPU limit: {} {s}\n",
            .{
                builtin.zig_version,
                @tagName(builtin.mode),
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

fn refreshHighlightedLine(
    allocator: std.mem.Allocator,
    out: *std.Io.Writer,
    state: *ln.linenoiseState,
    true_color: bool,
) !void {
    // Keep Buzz's redraw atomic from the terminal's point of view so raw
    // linenoise cursor movement is not visible between highlight passes.
    var refresh = std.Io.Writer.Allocating.init(allocator);
    defer refresh.deinit();

    // Match linenoise single-line scrolling: leave room for the prompt,
    // shift right only when the cursor would hit the terminal edge, and
    // render no more than the visible input window.
    const cols = @max(state.cols, state.plen + 1);
    const visible_capacity = cols - state.plen;
    const visible_start = if (state.pos >= visible_capacity)
        state.pos - visible_capacity + 1
    else
        0;
    const visible_len = @min(state.len - visible_start, visible_capacity);
    const visible_pos = state.pos - visible_start;

    const state_prompt = std.mem.span(state.prompt);
    const visible_source = state.buf[visible_start..][0..visible_len];

    try refresh.writer.writeAll("\r");
    try refresh.writer.writeAll(state_prompt);

    var scanner = Scanner.init(
        allocator,
        "REPL",
        visible_source,
    );
    scanner.highlight(&refresh.writer, true_color);

    try refresh.writer.writeAll("\x1b[0K");

    const cursor_column = @min(state.plen + visible_pos, cols - 1);
    if (cursor_column > 0) {
        try refresh.writer.print("\r\x1b[{}C", .{cursor_column});
    } else {
        try refresh.writer.writeAll("\r");
    }

    try out.writeAll(refresh.written());
}

fn readHighlightedLine(
    allocator: std.mem.Allocator,
    out: *std.Io.Writer,
    process: std.process.Init,
    prompt: [*:0]const u8,
    true_color: bool,
) ?[*:0]const u8 {
    if (ln == void) unreachable;

    if (process.environ_map.get("TERM")) |term| {
        if (std.ascii.eqlIgnoreCase(term, "dumb") or
            std.ascii.eqlIgnoreCase(term, "cons25") or
            std.ascii.eqlIgnoreCase(term, "emacs"))
        {
            return ln.linenoise(prompt);
        }
    }

    var null_file = std.Io.Dir.openFileAbsolute(process.io, "/dev/null", .{ .mode = .write_only }) catch {
        return ln.linenoise(prompt);
    };
    defer null_file.close(process.io);

    var buffer: [linenoise_max_line]u8 = undefined;
    var state: ln.linenoiseState = undefined;
    // Enter linenoise nonblocking edit mode; it owns raw terminal state and
    // mutates `state`/`buffer` as keys arrive. Its own rendering is pointed at
    // /dev/null so only the highlighted Buzz refresh reaches the terminal.
    if (ln.linenoiseEditStart(
        &state,
        -1,
        null_file.handle,
        buffer[0..].ptr,
        buffer.len,
        prompt,
    ) != 0) {
        return ln.linenoise(prompt);
    }
    // Leave raw edit mode and restore the terminal.
    defer ln.linenoiseEditStop(&state);

    refreshHighlightedLine(allocator, out, &state, true_color) catch unreachable;

    while (true) {
        // Consume one key sequence. `linenoiseEditMore` means keep editing;
        // any other non-null pointer is the submitted NUL-terminated line.
        const result = ln.linenoiseEditFeed(&state) orelse return null;
        if (@intFromPtr(result) != @intFromPtr(ln.linenoiseEditMore)) {
            return result;
        }

        refreshHighlightedLine(allocator, out, &state, true_color) catch unreachable;
    }
}

/// Replace the submitted edit line with highlighted source.
fn rewriteSubmittedSource(
    allocator: std.mem.Allocator,
    out: *std.Io.Writer,
    prompt: []const u8,
    source: []const u8,
    true_color: bool,
) !void {
    try out.print(
        if (builtin.os.tag == .windows)
            "{s}"
        else
            "\x1b[1A\r\x1b[2K{s}",
        .{prompt},
    );

    var source_scanner = Scanner.init(
        allocator,
        "REPL",
        source,
    );
    source_scanner.highlight(out, true_color);
    try out.writeAll("\n");
}

pub fn repl(process: std.process.Init, allocator: std.mem.Allocator) !void {
    const colorterm = process.environ_map.get("COLORTERM");
    const true_color = if (colorterm) |ct|
        std.mem.eql(u8, ct, "24bit") or std.mem.eql(u8, ct, "truecolor")
    else
        false;

    var runner: Runner = undefined;
    var perf: ?Perf = if (BuildOptions.show_perf) Perf.init(process.io) else null;
    defer if (perf) |*p| p.report();

    try runner.init(
        process,
        allocator,
        .Repl,
        null,
        if (perf) |*p| p else null,
    );
    defer runner.deinit();

    var stdout = io.stdoutWriter(process.io);
    var stderr = io.stderrWriter(process.io);
    printBanner(&stdout.interface, false);

    var buzz_history_path = std.Io.Writer.Allocating.init(allocator);
    defer buzz_history_path.deinit();

    try buzz_history_path.writer.print(
        "{s}/.buzz_history\x00",
        .{process.environ_map.get("HOME") orelse "."},
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
    var previous_globals_lookup = try runner.parser.globals_lookup.cloneContext(
        allocator,
        QualifiedNameContext{ .ast = &runner.parser.ast },
    );
    var previous_globals = try runner.vm.globals.clone(allocator);
    var previous_type_registry = try runner.gc.type_registry.registry.clone(allocator);
    var previous_input: ?[]u8 = null;

    var reader_buffer = [_]u8{0};
    var stdin_reader = std.Io.File.stdin().reader(process.io, reader_buffer[0..]);
    var reader = io.AllocatedReader.init(
        allocator,
        &stdin_reader.interface,
        null,
    );

    while (true) {
        const prompt: [*:0]const u8 = if (previous_input != null)
            MULTILINE_PROMPT
        else
            PROMPT;

        if (builtin.os.tag == .windows) {
            stdout.interface.writeAll(
                std.mem.span(prompt),
            ) catch @panic("Could not write to stdout");
        }

        const read_source = if (ln != void)
            readHighlightedLine(
                runner.gc.allocator,
                &stdout.interface,
                process,
                prompt,
                true_color,
            )
        else // FIXME: in that case, at least use an arena?
            reader.readUntilDelimiterOrEof('\n') catch @panic("Could not read stdin");

        if (read_source == null) {
            std.process.exit(0);
        }

        var source = if (builtin.os.tag == .windows) read_source.? else std.mem.span(read_source.?);
        const original_source = source;
        const submitted_prompt = std.mem.span(prompt);
        const continued_submission = previous_input != null;

        if (source.len > 0) {
            rewriteSubmittedSource(
                runner.gc.allocator,
                &stdout.interface,
                submitted_prompt,
                original_source,
                true_color,
            ) catch unreachable;

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

            const expr = expr: {
                const maybe_ast = runner.parser.parse(source, null, "REPL") catch |err| {
                    if (BuildOptions.debug) {
                        stderr.print("Failed with error {}\n", .{err}) catch unreachable;
                    }

                    break :expr null;
                };

                const ast = maybe_ast orelse {
                    if (runner.parser.reporter.last_error != .unclosed and BuildOptions.debug) {
                        stderr.print("Failed with error {}\n", .{Parser.CompileError.Recoverable}) catch unreachable;
                    }

                    break :expr null;
                };

                if (builtin.os.tag != .windows and !continued_submission) format_echo: {
                    var formatted = std.Io.Writer.Allocating.init(runner.gc.allocator);
                    defer formatted.deinit();

                    Renderer.render(
                        runner.gc.allocator,
                        &formatted.writer,
                        ast,
                        .{},
                    ) catch break :format_echo;

                    const rendered = std.mem.trimEnd(u8, formatted.written(), "\n");
                    if (std.mem.eql(u8, rendered, source)) {
                        break :format_echo;
                    }

                    rewriteSubmittedSource(
                        runner.gc.allocator,
                        &stdout.interface,
                        submitted_prompt,
                        rendered,
                        true_color,
                    ) catch break :format_echo;
                }

                const ast_slice = ast.slice();
                if (runner.codegen.generate(ast_slice) catch |err| {
                    if (BuildOptions.debug) {
                        stderr.print("Failed with error {}\n", .{err}) catch unreachable;
                    }

                    break :expr null;
                }) |function| {
                    runner.vm.interpret(
                        ast_slice,
                        function,
                        null,
                    ) catch |err| {
                        if (BuildOptions.debug) {
                            stderr.print("Failed with error {}\n", .{err}) catch unreachable;
                        }

                        break :expr null;
                    };

                    const fnode = ast.nodes.items(.components)[ast.root.?].Function;
                    const statements = ast.nodes.items(.components)[fnode.body.?].Block;
                    const last_statement = if (statements.len > 0) statements[statements.len - 1] else null;
                    if (last_statement != null and ast.nodes.items(.tag)[last_statement.?] == .Expression) {
                        break :expr runner.vm.pop();
                    }
                } else if (BuildOptions.debug) {
                    stderr.print("Failed with error {}\n", .{Parser.CompileError.Recoverable}) catch unreachable;
                }

                break :expr null;
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
                previous_globals_lookup = try runner.parser.globals_lookup.cloneContext(
                    allocator,
                    QualifiedNameContext{ .ast = &runner.parser.ast },
                );
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
                    scanner.highlight(&stdout.interface, true_color);

                    stdout.interface.writeAll("\n") catch unreachable;
                }
            } else {
                // We might have declared new globals, types, etc. and encounter an error
                // FIXME: why can't I deinit those?
                // parser.globals.deinit();
                runner.parser.globals = try previous_parser_globals.clone(allocator);
                runner.parser.globals_lookup = try previous_globals_lookup.cloneContext(
                    allocator,
                    QualifiedNameContext{ .ast = &runner.parser.ast },
                );

                // vm.globals.deinit();
                runner.vm.globals = try previous_globals.clone(allocator);
                runner.vm.globals_count = previous_global_top;

                // gc.type_registry.registry.deinit();
                runner.gc.type_registry.registry = try previous_type_registry.clone(allocator);

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
