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
const _parser = @import("parser.zig");
const Parser = _parser.Parser;
const CompileError = _parser.CompileError;
const MIRJIT = @import("mirjit.zig");
const ln = @import("linenoise.zig");
const _value = @import("value.zig");
const Value = _value.Value;
const valueToStringAlloc = _value.valueToStringAlloc;
const dumpStack = @import("disassembler.zig").dumpStack;
const CodeGen = @import("codegen.zig").CodeGen;
const FunctionNode = @import("node.zig").FunctionNode;
const Scanner = @import("scanner.zig").Scanner;

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
    const colorterm = std.os.getenv("COLORTERM");
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
                if (previous_global_top != vm.globals_count) {
                    previous_global_top = vm.globals_count;

                    var value_str = std.ArrayList(u8).init(vm.gc.allocator);
                    defer value_str.deinit();
                    var state = DumpState.init(&vm);

                    state.valueDump(
                        vm.globals.items[previous_global_top],
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

const DumpState = struct {
    const Self = @This();

    vm: *VM,
    seen: std.AutoHashMap(*_obj.Obj, void),
    depth: usize = 0,
    tab: usize = 0,

    pub fn init(vm: *VM) Self {
        return Self{
            .vm = vm,
            .seen = std.AutoHashMap(*_obj.Obj, void).init(vm.gc.allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.seen.deinit();
    }

    pub fn valueDump(state: *DumpState, value: Value, out: anytype, same_line: bool) void {
        if (state.depth > 50) {
            out.print("...", .{}) catch unreachable;
            return;
        }

        state.depth += 1;

        if (!same_line) {
            for (0..state.tab) |_| {
                out.writeAll("    ") catch unreachable;
            }
        }

        if (value.isNull()) {
            out.print("null", .{}) catch unreachable;
        } else if (!value.isObj() or state.seen.get(value.obj()) != null) {
            const string = valueToStringAlloc(state.vm.gc.allocator, value) catch std.ArrayList(u8).init(state.vm.gc.allocator);
            defer string.deinit();

            out.print("{s}", .{string.items}) catch unreachable;
        } else {
            state.seen.put(value.obj(), {}) catch unreachable;

            switch (value.obj().obj_type) {
                .Type,
                .Closure,
                .Function,
                .Bound,
                .Native,
                .UserData,
                .Fiber,
                .EnumInstance,
                => {
                    const string = valueToStringAlloc(state.vm.gc.allocator, value) catch std.ArrayList(u8).init(state.vm.gc.allocator);
                    defer string.deinit();

                    out.print("{s}", .{string.items}) catch unreachable;
                },

                .UpValue => {
                    const upvalue = ObjUpValue.cast(value.obj()).?;

                    state.valueDump(
                        if (upvalue.closed != null)
                            upvalue.closed.?
                        else
                            upvalue.location.*,
                        out,
                        false,
                    );
                },

                .String => {
                    const string = ObjString.cast(value.obj()).?;

                    out.print("\"{s}\"", .{string.string}) catch unreachable;
                },

                .Pattern => {
                    const pattern = ObjPattern.cast(value.obj()).?;

                    out.print("$\"{s}\"", .{pattern.source}) catch unreachable;
                },

                .List => {
                    const list = ObjList.cast(value.obj()).?;

                    out.print(
                        "[{s}",
                        .{
                            if (list.items.items.len > 0)
                                "\n"
                            else
                                "",
                        },
                    ) catch unreachable;
                    state.tab += 1;
                    for (list.items.items) |item| {
                        state.valueDump(
                            item,
                            out,
                            false,
                        );
                        out.print(",\n", .{}) catch unreachable;
                    }
                    state.tab -= 1;
                    out.print("]", .{}) catch unreachable;
                },

                .Map => {
                    const map = ObjMap.cast(value.obj()).?;

                    out.print(
                        "{{{s}",
                        .{
                            if (map.map.count() > 0)
                                "\n"
                            else
                                "",
                        },
                    ) catch unreachable;
                    state.tab += 1;
                    var it = map.map.iterator();
                    while (it.next()) |kv| {
                        const key = kv.key_ptr.*;

                        state.valueDump(
                            key,
                            out,
                            false,
                        );
                        out.writeAll(": ") catch unreachable;
                        state.valueDump(
                            kv.value_ptr.*,
                            out,
                            true,
                        );
                        out.writeAll(",\n") catch unreachable;
                    }
                    state.tab -= 1;
                    out.print("}}", .{}) catch unreachable;
                },

                .Enum => {
                    const enumeration = ObjEnum.cast(value.obj()).?;
                    const enum_type_def = enumeration.type_def.resolved_type.?.Enum;
                    const enum_value_type_def = enumeration.type_def.resolved_type.?.Enum.enum_type.toStringAlloc(state.vm.gc.allocator) catch unreachable;
                    defer enum_value_type_def.deinit();

                    out.print(
                        "enum({s}) {s} {{\n",
                        .{
                            enum_value_type_def.items,
                            enumeration.name.string,
                        },
                    ) catch unreachable;
                    state.tab += 1;
                    for (enum_type_def.cases.items, 0..) |case, i| {
                        out.print("    {s} -> ", .{case}) catch unreachable;
                        state.valueDump(
                            enumeration.cases.items[i],
                            out,
                            true,
                        );
                        out.writeAll(",\n") catch unreachable;
                    }
                    state.tab -= 1;
                    out.print("}}", .{}) catch unreachable;
                },

                .Object => {
                    const object = ObjObject.cast(value.obj()).?;
                    const object_def = object.type_def.resolved_type.?.Object;

                    out.print("object", .{}) catch unreachable;
                    if (object_def.conforms_to.count() > 0) {
                        out.print("(", .{}) catch unreachable;
                        var it = object_def.conforms_to.iterator();
                        while (it.next()) |kv| {
                            out.print("{s}, ", .{kv.key_ptr.*.resolved_type.?.Protocol.name.string}) catch unreachable;
                        }
                        out.print(")", .{}) catch unreachable;
                    }

                    out.print(" {s} {{\n", .{object_def.name.string}) catch unreachable;
                    state.tab += 1;

                    var it = object_def.static_fields.iterator();
                    while (it.next()) |kv| {
                        const static_field_type_str = kv.value_ptr.*.toStringAlloc(state.vm.gc.allocator) catch std.ArrayList(u8).init(state.vm.gc.allocator);
                        defer static_field_type_str.deinit();

                        out.print(
                            "    static {s} {s}",
                            .{
                                static_field_type_str.items,
                                kv.key_ptr.*,
                            },
                        ) catch unreachable;

                        var static_it = object.static_fields.iterator();
                        while (static_it.next()) |static_kv| {
                            if (std.mem.eql(u8, static_kv.key_ptr.*.string, kv.key_ptr.*)) {
                                out.print(" = ", .{}) catch unreachable;
                                state.valueDump(
                                    static_kv.value_ptr.*,
                                    out,
                                    true,
                                );
                                break;
                            }
                        }

                        out.print(";\n", .{}) catch unreachable;
                    }

                    it = object_def.fields.iterator();
                    while (it.next()) |kv| {
                        const field_type_str = kv.value_ptr.*.toStringAlloc(state.vm.gc.allocator) catch std.ArrayList(u8).init(state.vm.gc.allocator);
                        defer field_type_str.deinit();

                        out.print(
                            "    {s} {s}",
                            .{
                                field_type_str.items,
                                kv.key_ptr.*,
                            },
                        ) catch unreachable;

                        if (object.fields.get(state.vm.gc.copyString(kv.key_ptr.*) catch unreachable)) |v| {
                            out.print(" = ", .{}) catch unreachable;
                            state.valueDump(
                                v,
                                out,
                                true,
                            );
                        }

                        out.print(",\n", .{}) catch unreachable;
                    }

                    it = object_def.methods.iterator();
                    while (it.next()) |kv| {
                        const method_type_str = kv.value_ptr.*.toStringAlloc(state.vm.gc.allocator) catch std.ArrayList(u8).init(state.vm.gc.allocator);
                        defer method_type_str.deinit();

                        out.print("    {s}\n", .{method_type_str.items}) catch unreachable;
                    }

                    state.tab -= 1;
                    out.print("}}", .{}) catch unreachable;
                },

                .ObjectInstance => {
                    const object_instance = ObjObjectInstance.cast(value.obj()).?;

                    out.print(
                        "{s}{{\n",
                        .{
                            if (object_instance.object) |object|
                                object.type_def.resolved_type.?.Object.name.string
                            else
                                ".",
                        },
                    ) catch unreachable;
                    state.tab += 1;
                    var it = object_instance.fields.iterator();
                    while (it.next()) |kv| {
                        out.print(
                            "    {s} = ",
                            .{
                                kv.key_ptr.*.string,
                            },
                        ) catch unreachable;
                        state.valueDump(
                            kv.value_ptr.*,
                            out,
                            true,
                        );
                        out.print(",\n", .{}) catch unreachable;
                    }
                    state.tab -= 1;
                    out.print("}}", .{}) catch unreachable;
                },

                .ForeignContainer => {
                    const foreign = ObjForeignContainer.cast(value.obj()).?;
                    const foreign_def = foreign.type_def.resolved_type.?.ForeignContainer;

                    out.print(
                        "{s}{{\n",
                        .{foreign_def.name.string},
                    ) catch unreachable;

                    var it = foreign_def.fields.iterator();
                    while (it.next()) |kv| {
                        out.print("    {s} = ", .{kv.key_ptr.*}) catch unreachable;
                        state.valueDump(
                            kv.value_ptr.*.getter(
                                state.vm,
                                foreign.data.ptr,
                            ),
                            out,
                            true,
                        );
                        out.print(",\n", .{}) catch unreachable;
                    }
                    out.print("}}", .{}) catch unreachable;
                },
            }

            _ = state.seen.remove(value.obj());
        }

        state.depth -= 1;
    }
};
