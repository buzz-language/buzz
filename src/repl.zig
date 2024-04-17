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
const ln = @import("linenoise.zig");
const Value = @import("value.zig").Value;
const disassembler = @import("disassembler.zig");
const dumpStack = disassembler.dumpStack;
const DumpState = disassembler.DumpState;
const CodeGen = @import("Codegen.zig");
const Scanner = @import("Scanner.zig");

pub fn printBanner(out: std.fs.File.Writer, full: bool) void {
    out.print(
        "\n👨‍🚀 buzz {s}-{s} Copyright (C) 2021-present Benoit Giannangeli\n",
        .{
            if (BuildOptions.version.len > 0) BuildOptions.version else "unreleased",
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
    const colorterm = std.posix.getenv("COLORTERM");
    const true_color = if (colorterm) |ct|
        std.mem.eql(u8, ct, "24bit") or std.mem.eql(u8, ct, "truecolor")
    else
        false;

    var import_registry = ImportRegistry.init(allocator);
    var gc = GarbageCollector.init(allocator);
    gc.type_registry = TypeRegistry{
        .gc = &gc,
        .registry = std.StringHashMap(*ObjTypeDef).init(allocator),
    };
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

    var stdout = std.io.getStdOut().writer();
    var stderr = std.io.getStdErr().writer();
    printBanner(stdout, false);

    var buzz_history_path = std.ArrayList(u8).init(allocator);
    defer buzz_history_path.deinit();

    try buzz_history_path.writer().print(
        "{s}/.buzz_history\x00",
        .{std.posix.getenv("HOME") orelse "."},
    );

    _ = ln.linenoiseHistorySetMaxLen(100);
    _ = ln.linenoiseHistoryLoad(@ptrCast(buzz_history_path.items.ptr));

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
    while (true) {
        const read_source = ln.linenoise("> ");
        const source = std.mem.span(read_source);

        _ = ln.linenoiseHistoryAdd(source);
        _ = ln.linenoiseHistorySave(@ptrCast(buzz_history_path.items.ptr));

        if (source.len > 0) {
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

            if (!parser.reporter.had_error and !codegen.reporter.had_error) {
                // var source_scanner = Scanner.init(
                //     gc.allocator,
                //     "REPL",
                //     source,
                // );
                // source_scanner.highlight(stdout, true_color);
                // stdout.writeAll("\n") catch unreachable;

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
            }

            parser.reporter.had_error = false;
            parser.reporter.panic_mode = false;
            codegen.reporter.had_error = false;
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

        if (try codegen.generate(ast)) |function| {
            codegen_time = timer.read();
            timer.reset();

            try vm.interpret(
                ast,
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
            const parsing_ms: f64 = @as(f64, @floatFromInt(parsing_time)) / 1000000;
            const codegen_ms: f64 = @as(f64, @floatFromInt(codegen_time)) / 1000000;
            const running_ms: f64 = @as(f64, @floatFromInt(running_time)) / 1000000;
            const gc_ms: f64 = @as(f64, @floatFromInt(gc.gc_time)) / 1000000;
            const jit_ms: f64 = if (vm.jit) |jit|
                @as(f64, @floatFromInt(jit.jit_time)) / 1000000
            else
                0;
            std.debug.print(
                "\u{001b}[2mParsing: {d} ms\nCodegen: {d} ms\nRun: {d} ms\nJIT: {d} ms\nGC: {d} ms\nTotal: {d} ms\nFull GC: {} | GC: {} | Max allocated: {} bytes\n\u{001b}[0m",
                .{
                    parsing_ms,
                    codegen_ms,
                    running_ms,
                    jit_ms,
                    gc_ms,
                    @as(f64, @floatFromInt(total_timer.read())) / 1000000,
                    gc.full_collection_count,
                    gc.light_collection_count,
                    gc.max_allocated,
                },
            );
        }
    } else {
        return CompileError.Recoverable;
    }

    return null;
}
