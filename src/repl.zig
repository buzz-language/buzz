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
    // Windows console hosts do not consistently render the banner's ZWJ
    // emoji sequence, even when the surrounding text is written as UTF-16.
    // Omit the emoji on Windows rather than showing a host-dependent rendering.
    const banner_prefix = if (builtin.os.tag == .windows) "" else "👨‍🚀 ";
    out.print(
        "{s}buzz {f} Copyright (C) 2021-present Benoit Giannangeli\n",
        .{
            banner_prefix,
            BuildOptions.version,
        },
    ) catch unreachable;

    if (full) {
        out.print(
            "Built with Zig {f} {s}\nAllocator: {s}, Memory limit: {} {s}\nJIT: {s}, CPU limit: {} {s}\n",
            .{
                builtin.zig_version,
                @tagName(builtin.mode),
                if (builtin.mode == .debug or is_wasm)
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
            "\r\x1b[2K{s}"
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
    const true_color = if (builtin.os.tag == .windows)
        false
    else if (colorterm) |ct|
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

    var stdout = if (builtin.os.tag == .windows)
        WindowsReplWriter.init(runner.gc.allocator, process.io, std.Io.File.stdout())
    else
        io.stdoutWriter(process.io);
    var stderr = if (builtin.os.tag == .windows)
        WindowsReplWriter.init(runner.gc.allocator, process.io, std.Io.File.stderr())
    else
        io.stderrWriter(process.io);
    defer if (builtin.os.tag == .windows) stdout.deinit();
    defer if (builtin.os.tag == .windows) stderr.deinit();
    printBanner(&stdout.interface, false);

    const buzz_history_path = try getBuzzHistoryPath(allocator, process);
    defer allocator.free(buzz_history_path);

    // Setup linenoise
    if (ln != void) {
        _ = ln.linenoiseHistorySetMaxLen(100);
        _ = ln.linenoiseHistoryLoad(@ptrCast(buzz_history_path.ptr));
    }

    // Import std and debug as commodity
    _ = runner.runSource("import \"buzz:std\"; import \"buzz:debug\";", "REPL") catch unreachable;

    var previous_global_top = runner.vm.globals_count;
    var previous_parser_globals = try runner.parser.globals.clone(allocator);
    var previous_globals_lookup = try runner.parser.globals_lookup.cloneContext(
        allocator,
        QualifiedNameContext{ .ast = &runner.parser.ast },
    );
    var previous_globals = try runner.vm.globals.clone(allocator);
    var previous_type_registry = try runner.gc.type_registry.registry.clone(allocator);
    var previous_input: ?[]u8 = null;
    var windows_history = std.ArrayList([]u8).empty;
    defer {
        for (windows_history.items) |entry| runner.gc.allocator.free(entry);
        windows_history.deinit(runner.gc.allocator);
    }
    if (builtin.os.tag == .windows) {
        loadWindowsHistory(
            process.io,
            runner.gc.allocator,
            &windows_history,
            buzz_history_path,
        ) catch {};
    }

    var reader_buffer = [_]u8{0};
    var stdin_file = std.Io.File.stdin();
    var windows_stdin_is_console = isWindowsConsole(stdin_file);
    var owns_windows_stdin = false;
    if (builtin.os.tag == .windows and !windows_stdin_is_console) {
        if (openWindowsConsoleFile(windows_repl_api.GENERIC_READ, "CONIN$")) |console_input| {
            stdin_file = console_input;
            windows_stdin_is_console = true;
            owns_windows_stdin = true;
        }
    }
    defer closeWindowsConsoleFile(stdin_file, owns_windows_stdin);
    var stdin_reader = stdin_file.reader(process.io, reader_buffer[0..]);
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
        else if (windows_stdin_is_console)
            readWindowsLine(
                runner.gc.allocator,
                &stdout.interface,
                stdin_file,
                std.mem.span(prompt),
                &windows_history,
            ) catch |err| switch (err) {
                error.Interrupted => std.process.exit(130),
                else => @panic("Could not read stdin"),
            }
        else // FIXME: in that case, at least use an arena?
            reader.readUntilDelimiterOrEof('\n') catch @panic("Could not read stdin");

        if (read_source == null) {
            std.process.exit(0);
        }

        var source = if (builtin.os.tag == .windows) read_source.? else std.mem.span(read_source.?);
        const original_source = source;
        defer if (windows_stdin_is_console)
            runner.gc.allocator.free(@constCast(read_source.?[0..original_source.len]));
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
                const maybe_ast = runner.parser.parse(source, runner.parser.root_dir, null, "REPL") catch |err| {
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
                if (builtin.os.tag == .windows) {
                    addWindowsHistory(runner.gc.allocator, &windows_history, source);
                    saveWindowsHistory(
                        process.io,
                        runner.gc.allocator,
                        buzz_history_path,
                        &windows_history,
                    ) catch {};
                } else if (ln != void) {
                    _ = ln.linenoiseHistoryAdd(source);
                    _ = ln.linenoiseHistorySave(@ptrCast(buzz_history_path.ptr));
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
                } else if (builtin.os.tag == .windows) {
                    addWindowsHistory(runner.gc.allocator, &windows_history, source);
                    saveWindowsHistory(
                        process.io,
                        runner.gc.allocator,
                        buzz_history_path,
                        &windows_history,
                    ) catch {};
                } else if (ln != void) {
                    _ = ln.linenoiseHistoryAdd(source);
                    _ = ln.linenoiseHistorySave(@ptrCast(buzz_history_path.ptr));
                }
            }

            runner.parser.reporter.last_error = null;
            runner.parser.reporter.panic_mode = false;
            runner.codegen.reporter.last_error = null;
            runner.codegen.reporter.panic_mode = false;
        }
    }
}

/// Returns the platform-specific location used to persist REPL history.
fn getBuzzHistoryPath(allocator: std.mem.Allocator, process: std.process.Init) ![:0]u8 {
    const base = if (builtin.os.tag == .windows)
        process.environ_map.get("APPDATA") orelse
            process.environ_map.get("USERPROFILE") orelse
            process.environ_map.get("HOME") orelse
            "."
    else
        process.environ_map.get("HOME") orelse ".";

    const joined = try std.fs.path.join(allocator, &.{ base, ".buzz_history" });
    defer allocator.free(joined);
    return allocator.dupeSentinel(u8, joined, 0);
}

/// Loads one UTF-8 history entry per line from the Windows REPL history file.
fn loadWindowsHistory(
    process_io: std.Io,
    allocator: std.mem.Allocator,
    history: *std.ArrayList([]u8),
    path: []const u8,
) !void {
    var file = (if (std.fs.path.isAbsolute(path))
        std.Io.Dir.openFileAbsolute(process_io, path, .{})
    else
        std.Io.Dir.cwd().openFile(process_io, path, .{})) catch return;
    defer file.close(process_io);

    var file_reader = file.reader(process_io, &.{});
    const contents = file_reader.interface.allocRemaining(allocator, .unlimited) catch return;
    defer allocator.free(contents);

    var entries = std.mem.splitScalar(u8, contents, '\n');
    while (entries.next()) |entry| {
        const without_cr = std.mem.trimEnd(u8, entry, "\r");
        if (without_cr.len > 0) addWindowsHistory(allocator, history, without_cr);
    }
}

/// Persists the current Windows REPL history as UTF-8 text.
fn saveWindowsHistory(
    process_io: std.Io,
    allocator: std.mem.Allocator,
    path: []const u8,
    history: *const std.ArrayList([]u8),
) !void {
    var contents = std.Io.Writer.Allocating.init(allocator);
    defer contents.deinit();

    for (history.items, 0..) |entry, index| {
        if (index != 0) try contents.writer.writeByte('\n');
        try contents.writer.writeAll(entry);
    }

    if (std.fs.path.isAbsolute(path)) {
        var file = try std.Io.Dir.createFileAbsolute(process_io, path, .{});
        defer file.close(process_io);
        try file.writeStreamingAll(process_io, contents.written());
    } else {
        try std.Io.Dir.cwd().writeFile(process_io, .{
            .sub_path = path,
            .data = contents.written(),
        });
    }
}

/// Windows API entry points used to write Unicode text to an interactive console.
const windows_repl_api = if (builtin.os.tag == .windows) struct {
    const windows = std.os.windows;

    const KeyEventRecord = extern struct {
        key_down: windows.BOOL,
        repeat_count: windows.WORD,
        virtual_key_code: windows.WORD,
        virtual_scan_code: windows.WORD,
        unicode_char: windows.WCHAR,
        control_key_state: windows.DWORD,
    };

    const InputRecord = extern struct {
        event_type: windows.WORD,
        event: extern union {
            key_event: KeyEventRecord,
        },
    };

    const ENABLE_PROCESSED_INPUT: windows.DWORD = 0x0001;
    const ENABLE_LINE_INPUT: windows.DWORD = 0x0002;
    const ENABLE_ECHO_INPUT: windows.DWORD = 0x0004;
    const ENABLE_QUICK_EDIT_MODE: windows.DWORD = 0x0040;
    const ENABLE_EXTENDED_FLAGS: windows.DWORD = 0x0080;
    const KEY_EVENT: windows.WORD = 0x0001;
    const LEFT_CTRL_PRESSED: windows.DWORD = 0x0008;
    const RIGHT_CTRL_PRESSED: windows.DWORD = 0x0004;
    const GENERIC_READ: windows.DWORD = 0x80000000;
    const GENERIC_WRITE: windows.DWORD = 0x40000000;
    const FILE_SHARE_READ: windows.DWORD = 0x00000001;
    const FILE_SHARE_WRITE: windows.DWORD = 0x00000002;
    const OPEN_EXISTING: windows.DWORD = 3;

    const VK_BACK: windows.WORD = 0x08;
    const VK_C: windows.WORD = 0x43;
    const VK_RETURN: windows.WORD = 0x0d;
    const VK_ESCAPE: windows.WORD = 0x1b;
    const VK_UP: windows.WORD = 0x26;
    const VK_DOWN: windows.WORD = 0x28;

    extern "kernel32" fn WriteConsoleW(
        handle: windows.HANDLE,
        buffer: [*]const windows.WCHAR,
        length: windows.DWORD,
        written: ?*windows.DWORD,
        reserved: ?*anyopaque,
    ) callconv(.winapi) windows.BOOL;

    extern "kernel32" fn GetConsoleMode(
        handle: windows.HANDLE,
        mode: *windows.DWORD,
    ) callconv(.winapi) windows.BOOL;

    extern "kernel32" fn SetConsoleMode(
        handle: windows.HANDLE,
        mode: windows.DWORD,
    ) callconv(.winapi) windows.BOOL;

    extern "kernel32" fn ReadConsoleInputW(
        handle: windows.HANDLE,
        records: [*]InputRecord,
        length: windows.DWORD,
        read: *windows.DWORD,
    ) callconv(.winapi) windows.BOOL;

    extern "kernel32" fn CreateFileW(
        name: [*:0]const windows.WCHAR,
        desired_access: windows.DWORD,
        share_mode: windows.DWORD,
        security_attributes: ?*anyopaque,
        creation_disposition: windows.DWORD,
        flags_and_attributes: windows.DWORD,
        template_file: ?windows.HANDLE,
    ) callconv(.winapi) windows.HANDLE;

    extern "kernel32" fn CloseHandle(handle: windows.HANDLE) callconv(.winapi) windows.BOOL;
} else void;

/// Writes REPL-owned text using the representation expected by the host terminal.
const WindowsReplWriter = if (builtin.os.tag == .windows) struct {
    allocator: std.mem.Allocator,
    process_io: std.Io,
    file: std.Io.File,
    owns_file: bool,
    is_console: bool,
    ansi_enabled: bool,
    interface: std.Io.Writer,

    /// Initializes a writer for a Windows stdout or stderr handle.
    pub fn init(
        allocator: std.mem.Allocator,
        process_io: std.Io,
        file: std.Io.File,
    ) @This() {
        var output_file = file;
        var owns_file = false;
        var is_console = isWindowsConsole(file);
        if (!is_console) {
            if (openWindowsConsoleFile(windows_repl_api.GENERIC_WRITE, "CONOUT$")) |console_output| {
                output_file = console_output;
                owns_file = true;
                is_console = true;
            }
        }
        var ansi_enabled = false;
        if (is_console) {
            if (output_file.enableAnsiEscapeCodes(process_io)) |_| {
                ansi_enabled = true;
            } else |_| {}
        }

        return .{
            .allocator = allocator,
            .process_io = process_io,
            .file = output_file,
            .owns_file = owns_file,
            .is_console = is_console,
            .ansi_enabled = ansi_enabled,
            .interface = .{
                .buffer = &.{},
                .vtable = &.{
                    .drain = drain,
                },
            },
        };
    }

    /// Closes a console handle opened through CONOUT$.
    pub fn deinit(self: *@This()) void {
        if (self.owns_file) _ = windows_repl_api.CloseHandle(self.file.handle);
    }

    /// Writes one logical writer drain to either the console or the raw handle.
    fn drain(
        writer: *std.Io.Writer,
        data: []const []const u8,
        splat: usize,
    ) std.Io.Writer.Error!usize {
        const self: *@This() = @alignCast(@fieldParentPtr("interface", writer));

        var output = std.Io.Writer.Allocating.init(self.allocator);
        defer output.deinit();

        const header = writer.buffered();
        output.writer.writeAll(header) catch return error.WriteFailed;
        for (data[0 .. data.len - 1]) |bytes| {
            output.writer.writeAll(bytes) catch return error.WriteFailed;
        }
        for (0..splat) |_| {
            output.writer.writeAll(data[data.len - 1]) catch return error.WriteFailed;
        }

        const input_len = output.written().len;
        const bytes = if (self.is_console) blk: {
            const console_bytes = if (self.ansi_enabled)
                output.written()
            else
                stripAnsiEscapeCodes(output.written());
            break :blk console_bytes;
        } else output.written();

        if (!self.is_console) {
            std.Io.File.writeStreamingAll(self.file, self.process_io, bytes) catch return error.WriteFailed;
            return input_len;
        }

        const utf16 = std.unicode.utf8ToUtf16LeAlloc(self.allocator, bytes) catch return error.WriteFailed;
        defer self.allocator.free(utf16);

        var offset: usize = 0;
        while (offset < utf16.len) {
            const count = @min(utf16.len - offset, std.math.maxInt(std.os.windows.DWORD));
            var written: std.os.windows.DWORD = 0;
            if (!windows_repl_api.WriteConsoleW(
                self.file.handle,
                utf16[offset..].ptr,
                @intCast(count),
                &written,
                null,
            ).toBool() or written == 0) {
                return error.WriteFailed;
            }
            offset += written;
        }

        return input_len;
    }
} else void;

/// Removes CSI escape sequences when a legacy Windows console cannot interpret them.
fn stripAnsiEscapeCodes(bytes: []u8) []u8 {
    var read: usize = 0;
    var write: usize = 0;

    while (read < bytes.len) {
        if (bytes[read] == 0x1b and read + 1 < bytes.len and bytes[read + 1] == '[') {
            read += 2;
            while (read < bytes.len and !(bytes[read] >= 0x40 and bytes[read] <= 0x7e)) {
                read += 1;
            }
            if (read < bytes.len) read += 1;
            continue;
        }

        bytes[write] = bytes[read];
        write += 1;
        read += 1;
    }

    return bytes[0..write];
}

/// Checks whether a Windows handle is backed by a console screen or input buffer.
fn isWindowsConsole(file: std.Io.File) bool {
    if (builtin.os.tag != .windows) return false;

    var mode: std.os.windows.DWORD = undefined;
    return windows_repl_api.GetConsoleMode(file.handle, &mode).toBool();
}

/// Opens the process console directly when a shell has supplied pipe handles.
fn openWindowsConsoleFile(
    desired_access: std.os.windows.DWORD,
    comptime console_name_utf8: []const u8,
) ?std.Io.File {
    if (builtin.os.tag != .windows) return null;

    const console_name = std.unicode.utf8ToUtf16LeStringLiteral(console_name_utf8);
    const handle = windows_repl_api.CreateFileW(
        console_name,
        desired_access,
        windows_repl_api.FILE_SHARE_READ | windows_repl_api.FILE_SHARE_WRITE,
        null,
        windows_repl_api.OPEN_EXISTING,
        0,
        null,
    );
    if (handle == std.os.windows.INVALID_HANDLE_VALUE) return null;
    return .{
        .handle = handle,
        .flags = .{ .nonblocking = false },
    };
}

/// Closes a console file only when this REPL opened its handle.
fn closeWindowsConsoleFile(file: std.Io.File, owned: bool) void {
    if (builtin.os.tag == .windows and owned) {
        _ = windows_repl_api.CloseHandle(file.handle);
    }
}

/// Redraws the current Windows console input line after an editing action.
fn refreshWindowsInput(
    allocator: std.mem.Allocator,
    out: *std.Io.Writer,
    prompt: []const u8,
    line: []const u16,
) !void {
    const utf8 = try std.unicode.utf16LeToUtf8Alloc(allocator, line);
    defer allocator.free(utf8);

    try out.writeAll("\r\x1b[2K");
    try out.writeAll(prompt);
    try out.writeAll(utf8);
}

/// Echoes only the newly completed UTF-16 character while the user types.
fn echoWindowsInputCharacter(
    allocator: std.mem.Allocator,
    out: *std.Io.Writer,
    line: []const u16,
) !void {
    if (line.len == 0 or std.unicode.utf16IsHighSurrogate(line[line.len - 1])) return;

    const start = if (std.unicode.utf16IsLowSurrogate(line[line.len - 1]) and
        line.len > 1 and
        std.unicode.utf16IsHighSurrogate(line[line.len - 2]))
        line.len - 2
    else
        line.len - 1;
    const utf8 = try std.unicode.utf16LeToUtf8Alloc(allocator, line[start..]);
    defer allocator.free(utf8);
    try out.writeAll(utf8);
}

/// Reads one line from a Windows console and handles basic editing and history navigation.
fn readWindowsLine(
    allocator: std.mem.Allocator,
    out: *std.Io.Writer,
    input: std.Io.File,
    prompt: []const u8,
    history: *std.ArrayList([]u8),
) !?[]u8 {
    const windows = std.os.windows;
    var mode: windows.DWORD = undefined;
    if (!windows_repl_api.GetConsoleMode(input.handle, &mode).toBool()) {
        return error.InputOutput;
    }
    defer _ = windows_repl_api.SetConsoleMode(input.handle, mode);

    const input_mode = (mode | windows_repl_api.ENABLE_EXTENDED_FLAGS) &
        ~(windows_repl_api.ENABLE_PROCESSED_INPUT | windows_repl_api.ENABLE_LINE_INPUT |
            windows_repl_api.ENABLE_ECHO_INPUT | windows_repl_api.ENABLE_QUICK_EDIT_MODE);
    if (!windows_repl_api.SetConsoleMode(input.handle, input_mode).toBool()) {
        return error.InputOutput;
    }

    var line = std.array_list.Managed(u16).init(allocator);
    defer line.deinit();
    var history_index: ?usize = null;

    while (true) {
        var record: windows_repl_api.InputRecord = undefined;
        var read: windows.DWORD = 0;
        if (!windows_repl_api.ReadConsoleInputW(input.handle, @ptrCast(&record), 1, &read).toBool()) {
            return error.InputOutput;
        }
        if (read == 0 or record.event_type != windows_repl_api.KEY_EVENT) continue;

        const key = record.event.key_event;
        if (!key.key_down.toBool()) continue;

        if ((key.control_key_state &
            (windows_repl_api.LEFT_CTRL_PRESSED | windows_repl_api.RIGHT_CTRL_PRESSED)) != 0 and
            key.virtual_key_code == windows_repl_api.VK_C)
        {
            try out.writeAll("^C\r\n");
            return error.Interrupted;
        }

        switch (key.virtual_key_code) {
            windows_repl_api.VK_RETURN => {
                const result = try std.unicode.utf16LeToUtf8Alloc(allocator, line.items);
                return result;
            },
            windows_repl_api.VK_BACK => {
                if (line.items.len > 0) {
                    line.items.len -= 1;
                    if (line.items.len > 0 and
                        line.items[line.items.len - 1] >= 0xdc00 and
                        line.items[line.items.len - 1] <= 0xdfff)
                    {
                        line.items.len -= 1;
                    }
                    try refreshWindowsInput(allocator, out, prompt, line.items);
                }
            },
            windows_repl_api.VK_ESCAPE => {
                line.clearRetainingCapacity();
                history_index = null;
                try refreshWindowsInput(allocator, out, prompt, line.items);
            },
            windows_repl_api.VK_UP => {
                if (history.items.len > 0) {
                    history_index = if (history_index) |index|
                        if (index > 0) index - 1 else 0
                    else
                        history.items.len - 1;
                    line.clearRetainingCapacity();
                    try std.unicode.utf8ToUtf16LeArrayList(&line, history.items[history_index.?]);
                    try refreshWindowsInput(allocator, out, prompt, line.items);
                }
            },
            windows_repl_api.VK_DOWN => {
                if (history_index) |index| {
                    if (index + 1 < history.items.len) {
                        history_index = index + 1;
                        line.clearRetainingCapacity();
                        try std.unicode.utf8ToUtf16LeArrayList(&line, history.items[history_index.?]);
                    } else {
                        history_index = null;
                        line.clearRetainingCapacity();
                    }
                    try refreshWindowsInput(allocator, out, prompt, line.items);
                }
            },
            else => if (key.unicode_char >= 0x20) {
                try line.append(key.unicode_char);
                history_index = null;
                try echoWindowsInputCharacter(allocator, out, line.items);
            },
        }
    }
}

/// Stores a Windows REPL input in the session history.
fn addWindowsHistory(
    allocator: std.mem.Allocator,
    history: *std.ArrayList([]u8),
    source: []const u8,
) void {
    const copy = allocator.dupe(u8, source) catch @panic("Out of memory");
    history.append(allocator, copy) catch @panic("Out of memory");
}
