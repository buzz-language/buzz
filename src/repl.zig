const std = @import("std");
const builtin = @import("builtin");
const BuildOptions = @import("build_options");

const _vm = @import("vm.zig");
const VM = _vm.VM;
const ImportRegistry = _vm.ImportRegistry;
const _mem = @import("memory.zig");
const GarbageCollector = _mem.GarbageCollector;
const TypeRegistry = _mem.TypeRegistry;
const _obj = @import("obj.zig");
const ObjString = _obj.ObjString;
const ObjTypeDef = _obj.ObjTypeDef;
const _parser = @import("parser.zig");
const Parser = _parser.Parser;
const CompileError = _parser.CompileError;
const MIRJIT = @import("mirjit.zig");
const ln = @import("linenoise.zig");
const Value = @import("value.zig").Value;
const valueDump = @import("buzz_api.zig").bz_valueDump;
const dumpStack = @import("disassembler.zig").dumpStack;
const CodeGen = @import("codegen.zig").CodeGen;
const FunctionNode = @import("node.zig").FunctionNode;

pub fn printBanner(out: std.fs.File.Writer, full: bool) void {
    out.print(
        "\n👨‍🚀 buzz {s}-{s} Copyright (C) 2021-2023 Benoit Giannangeli\n",
        .{
            if (BuildOptions.version.len > 0) BuildOptions.version else "unreleased",
            BuildOptions.sha,
        },
    ) catch unreachable;

    if (full) {
        out.print(
            "Built with Zig {} {s}\nAllocator: {s}\nJIT: {s}\n",
            .{
                builtin.zig_version,
                switch (builtin.mode) {
                    .ReleaseFast => "release-fast",
                    .ReleaseSafe => "release-safe",
                    .ReleaseSmall => "release-small",
                    .Debug => "debug",
                },
                if (builtin.mode == .Debug)
                    "gpa"
                else if (BuildOptions.mimalloc) "mimalloc" else "c_allocator",
                if (BuildOptions.jit)
                    "on"
                else
                    "off",
            },
        ) catch unreachable;
    }
}

pub fn repl(allocator: std.mem.Allocator) !void {
    var import_registry = ImportRegistry.init(allocator);
    var gc = GarbageCollector.init(allocator);
    gc.type_registry = TypeRegistry{
        .gc = &gc,
        .registry = std.StringHashMap(*ObjTypeDef).init(allocator),
    };
    var imports = std.StringHashMap(Parser.ScriptImport).init(allocator);
    var vm = try VM.init(&gc, &import_registry, .Repl);
    vm.mir_jit = if (BuildOptions.jit)
        MIRJIT.init(&vm)
    else
        null;
    defer {
        if (vm.mir_jit != null) {
            vm.mir_jit.?.deinit();
            vm.mir_jit = null;
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
        if (vm.mir_jit) |*jit| jit else null,
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

    _ = ln.linenoiseHistorySetMaxLen(100);
    _ = ln.linenoiseHistoryLoad("./buzz_history");

    // Import std and debug as commodity
    runSource(
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
        // _ = stdout.write("\n→ ") catch unreachable;
        // const read_source = stdin.readUntilDelimiterOrEofAlloc(
        //     gc.allocator,
        //     '\n',
        //     16 * 8 * 64,
        // ) catch unreachable;

        const read_source = ln.linenoise("> ");
        const source = std.mem.span(read_source);

        _ = ln.linenoiseHistoryAdd(source);
        _ = ln.linenoiseHistorySave("./buzz_history");

        if (source.len > 0) {
            runSource(
                source,
                "REPL",
                &vm,
                &codegen,
                &parser,
                &gc,
            ) catch |err| {
                if (BuildOptions.debug) {
                    stderr.print("Failed with error {}\n", .{err}) catch unreachable;
                }
            };

            if (!parser.reporter.had_error and !codegen.reporter.had_error) {
                // FIXME: why can't I deinit those?
                // previous_parser_globals.deinit();
                previous_parser_globals = try parser.globals.clone();
                // previous_globals.deinit();
                previous_globals = try vm.globals.clone();
                // previous_type_registry.deinit();
                previous_type_registry = try gc.type_registry.registry.clone();

                // Dump top of stack
                if (previous_global_top != vm.globals_count) {
                    previous_global_top = vm.globals_count;

                    // TODO: use pretty dumper with highlighting and tabulations
                    std.debug.print("\u{001b}[2m", .{});
                    valueDump(vm.globals.items[previous_global_top], &vm);
                    std.debug.print("\u{001b}[0m\n", .{});
                }
            } else {
                // We might have declared new globals, types, etc. and encounter an error
                parser.globals.deinit();
                parser.globals = previous_parser_globals;

                vm.globals.deinit();
                vm.globals = previous_globals;
                vm.globals_count = previous_global_top;

                gc.type_registry.registry.deinit();
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
) !void {
    var total_timer = std.time.Timer.start() catch unreachable;
    var timer = try std.time.Timer.start();
    var parsing_time: u64 = undefined;
    var codegen_time: u64 = undefined;
    var running_time: u64 = undefined;

    if (try parser.parse(source, file_name)) |function_node| {
        parsing_time = timer.read();
        timer.reset();

        if (try codegen.generate(FunctionNode.cast(function_node).?)) |function| {
            codegen_time = timer.read();
            timer.reset();

            try vm.interpret(
                function,
                null,
            );

            running_time = timer.read();
        } else {
            return CompileError.Recoverable;
        }

        if (BuildOptions.show_perf) {
            const parsing_ms: f64 = @as(f64, @floatFromInt(parsing_time)) / 1000000;
            const codegen_ms: f64 = @as(f64, @floatFromInt(codegen_time)) / 1000000;
            const running_ms: f64 = @as(f64, @floatFromInt(running_time)) / 1000000;
            const gc_ms: f64 = @as(f64, @floatFromInt(gc.gc_time)) / 1000000;
            const jit_ms: f64 = if (vm.mir_jit) |jit|
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
}
