const std = @import("std");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const BuildOptions = @import("build_options");

const _vm = @import("vm.zig");
const VM = _vm.VM;
const ImportRegistry = _vm.ImportRegistry;
const _mem = @import("memory.zig");
const GarbageCollector = _mem.GarbageCollector;
const TypeRegistry = _mem.TypeRegistry;
const _obj = @import("obj.zig");
const Obj = _obj.Obj;
const ObjString = _obj.ObjString;
const ObjPattern = _obj.ObjPattern;
const ObjMap = _obj.ObjMap;
const ObjUpValue = _obj.ObjUpValue;
const ObjEnum = _obj.ObjEnum;
const ObjEnumInstance = _obj.ObjEnumInstance;
const ObjObject = _obj.ObjObject;
const ObjObjectInstance = _obj.ObjObjectInstance;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjFunction = _obj.ObjFunction;
const ObjList = _obj.ObjList;
const ObjUserData = _obj.ObjUserData;
const ObjClosure = _obj.ObjClosure;
const ObjNative = _obj.ObjNative;
const ObjBoundMethod = _obj.ObjBoundMethod;
const ObjFiber = _obj.ObjFiber;
const ObjForeignContainer = _obj.ObjForeignContainer;
const Parser = @import("Parser.zig");
const CompileError = Parser.CompileError;
const JIT = @import("Jit.zig");
const ln = if (builtin.os.tag != .windows) @import("linenoise.zig") else void;
const Value = @import("value.zig").Value;
const disassembler = @import("disassembler.zig");
const dumpStack = disassembler.dumpStack;
const DumpState = disassembler.DumpState;
const CodeGen = @import("Codegen.zig");
const Scanner = @import("Scanner.zig");
const io = @import("io.zig");

pub const PROMPT = ">>> ";
pub const MULTILINE_PROMPT = "... ";

pub fn printBanner(out: anytype, full: bool) void {
    out.print(
        "\n👨‍🚀 buzz {}-{s} Copyright (C) 2021-present Benoit Giannangeli\n",
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

    var import_registry = ImportRegistry.init(allocator);
    var gc = try GarbageCollector.init(allocator);
    gc.type_registry = try TypeRegistry.init(&gc);
    var imports = std.StringHashMap(Parser.ScriptImport).init(allocator);
    var vm = try VM.init(&gc, &import_registry, .Repl);
    vm.jit = if (BuildOptions.jit and BuildOptions.cycle_limit == null)
        JIT.init(&vm)
    else
        null;
    defer {
        if (vm.jit != null) {
            vm.jit.?.deinit();
            vm.jit = null;
        }
    }
    var parser = Parser.init(
        &gc,
        &imports,
        false,
        .Repl,
    );
    var codegen = CodeGen.init(
        &gc,
        &parser,
        .Repl,
        if (vm.jit) |*jit| jit else null,
    );
    defer {
        codegen.deinit();
        vm.deinit();
        parser.deinit();
        // gc.deinit();
        var it = imports.iterator();
        while (it.next()) |kv| {
            kv.value_ptr.*.globals.deinit();
        }
        imports.deinit();
        // TODO: free type_registry and its keys which are on the heap
    }

    var stdout = io.stdOutWriter;
    var stderr = io.stdErrWriter;
    printBanner(stdout, false);

    var buzz_history_path = std.ArrayList(u8).init(allocator);
    defer buzz_history_path.deinit();

    try buzz_history_path.writer().print(
        "{s}/.buzz_history\x00",
        .{envMap.get("HOME") orelse "."},
    );

    if (builtin.os.tag != .windows) {
        _ = ln.linenoiseHistorySetMaxLen(100);
        _ = ln.linenoiseHistoryLoad(@ptrCast(buzz_history_path.items.ptr));
    }

    // Import std and debug as commodity
    _ = runSource(
        "import \"std\"; import \"debug\";",
        "REPL",
        &vm,
        &codegen,
        &parser,
        &gc,
    ) catch unreachable;

    var previous_global_top = vm.globals_count;
    var previous_parser_globals = try parser.globals.clone();
    var previous_globals = try vm.globals.clone();
    var previous_type_registry = try gc.type_registry.registry.clone();
    var previous_input: ?[]u8 = null;
    const stdin_buffer = if (builtin.os.tag == .windows)
        gc.allocator.alloc(u8, 2048) catch @panic("Out of memory")
    else
        null;

    while (true) {
        if (builtin.os.tag == .windows) {
            std.io.getStdOut().writeAll(
                if (previous_input != null)
                    MULTILINE_PROMPT
                else
                    PROMPT,
            ) catch @panic("Could not write to stdout");
        }

        const read_source = if (builtin.os.tag != .windows)
            ln.linenoise(
                if (previous_input != null)
                    MULTILINE_PROMPT
                else
                    PROMPT,
            )
        else
            std.io.getStdIn().reader()
                .readUntilDelimiterOrEof(stdin_buffer, '\n') catch @panic("Could not read stdin");

        if (read_source == null) {
            std.process.exit(0);
        }

        var source = if (builtin.os.tag == .windows) read_source.? else std.mem.span(read_source.?);
        const original_source = source;

        if (source.len > 0) {
            // Highlight input
            var source_scanner = Scanner.init(
                gc.allocator,
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
                    gc.allocator,
                    u8,
                    &.{
                        previous,
                        source,
                    },
                    0,
                ) catch @panic("Out of memory");
                gc.allocator.free(previous);
                previous_input = null;
            }

            const expr = runSource(
                source,
                "REPL",
                &vm,
                &codegen,
                &parser,
                &gc,
            ) catch |err| failed: {
                if (BuildOptions.debug) {
                    stderr.print("Failed with error {}\n", .{err}) catch unreachable;
                }

                break :failed null;
            };

            if (parser.reporter.last_error == null and codegen.reporter.last_error == null) {
                if (builtin.os.tag != .windows) {
                    _ = ln.linenoiseHistoryAdd(source);
                    _ = ln.linenoiseHistorySave(@ptrCast(buzz_history_path.items.ptr));
                }
                // FIXME: why can't I deinit those?
                // previous_parser_globals.deinit();
                previous_parser_globals = try parser.globals.clone();
                // previous_globals.deinit();
                previous_globals = try vm.globals.clone();
                // previous_type_registry.deinit();
                previous_type_registry = try gc.type_registry.registry.clone();

                // Dump top of stack
                if (previous_global_top != vm.globals_count or expr != null) {
                    previous_global_top = vm.globals_count;

                    const value = expr orelse vm.globals.items[previous_global_top];

                    var value_str = std.ArrayList(u8).init(vm.gc.allocator);
                    defer value_str.deinit();
                    var state = DumpState.init(&vm);

                    state.valueDump(
                        value,
                        value_str.writer(),
                        false,
                    );

                    var scanner = Scanner.init(
                        gc.allocator,
                        "REPL",
                        value_str.items,
                    );
                    scanner.highlight(stdout, true_color);

                    stdout.writeAll("\n") catch unreachable;
                }
            } else {
                // We might have declared new globals, types, etc. and encounter an error
                // FIXME: why can't I deinit those?
                // parser.globals.deinit();
                parser.globals = previous_parser_globals;

                // vm.globals.deinit();
                vm.globals = previous_globals;
                vm.globals_count = previous_global_top;

                // gc.type_registry.registry.deinit();
                gc.type_registry.registry = previous_type_registry;

                // If syntax error was unclosed block, keep previous input
                if (parser.reporter.last_error == .unclosed) {
                    previous_input = gc.allocator.alloc(u8, source.len) catch @panic("Out of memory");
                    std.mem.copyForwards(u8, previous_input.?, source);
                } else if (builtin.os.tag != .windows) {
                    _ = ln.linenoiseHistoryAdd(source);
                    _ = ln.linenoiseHistorySave(@ptrCast(buzz_history_path.items.ptr));
                }
            }

            parser.reporter.last_error = null;
            parser.reporter.panic_mode = false;
            codegen.reporter.last_error = null;
            codegen.reporter.panic_mode = false;
        }
    }
}

fn runSource(
    source: []const u8,
    file_name: []const u8,
    vm: *VM,
    codegen: *CodeGen,
    parser: *Parser,
    gc: *GarbageCollector,
) !?Value {
    var total_timer = std.time.Timer.start() catch unreachable;
    var timer = try std.time.Timer.start();
    var parsing_time: u64 = undefined;
    var codegen_time: u64 = undefined;
    var running_time: u64 = undefined;

    if (try parser.parse(source, file_name)) |ast| {
        parsing_time = timer.read();
        timer.reset();

        const ast_slice = ast.slice();
        if (try codegen.generate(ast_slice)) |function| {
            codegen_time = timer.read();
            timer.reset();

            try vm.interpret(
                ast_slice,
                function,
                null,
            );

            // Does the user code ends with a lone expression?
            const fnode = ast.nodes.items(.components)[ast.root.?].Function;
            const statements = ast.nodes.items(.components)[fnode.body.?].Block;
            const last_statement = if (statements.len > 0) statements[statements.len - 1] else null;
            if (last_statement != null and ast.nodes.items(.tag)[last_statement.?] == .Expression) {
                return vm.pop();
            }

            running_time = timer.read();
        } else {
            return CompileError.Recoverable;
        }

        if (BuildOptions.show_perf) {
            io.print(
                "\u{001b}[2mParsing: {d}\nCodegen: {d}\nRun: {d}\nJIT: {d}\nGC: {d}\nTotal: {d}\nFull GC: {} | GC: {} | Max allocated: {} bytes\n\u{001b}[0m",
                .{
                    std.fmt.fmtDuration(parsing_time),
                    std.fmt.fmtDuration(codegen_time),
                    std.fmt.fmtDuration(running_time),
                    std.fmt.fmtDuration(if (vm.jit) |jit| jit.jit_time else 0),
                    std.fmt.fmtDuration(gc.gc_time),
                    std.fmt.fmtDuration(total_timer.read()),
                    gc.full_collection_count,
                    gc.light_collection_count,
                    std.fmt.fmtIntSizeDec(gc.max_allocated),
                },
            );
        }
    } else if (parser.reporter.last_error == .unclosed) {
        return null;
    } else {
        return CompileError.Recoverable;
    }

    return null;
}
