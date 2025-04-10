const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const _vm = @import("vm.zig");
const VM = _vm.VM;
const RunFlavor = _vm.RunFlavor;
const ImportRegistry = _vm.ImportRegistry;
const Parser = @import("Parser.zig");
const CodeGen = @import("Codegen.zig");
const _obj = @import("obj.zig");
const ObjString = _obj.ObjString;
const ObjTypeDef = _obj.ObjTypeDef;
const TypeRegistry = @import("memory.zig").TypeRegistry;
const Ast = @import("Ast.zig");
const BuildOptions = @import("build_options");
const clap = @import("clap");
const GarbageCollector = @import("memory.zig").GarbageCollector;
const JIT = if (!is_wasm) @import("Jit.zig") else void;
const is_wasm = builtin.cpu.arch.isWasm();
const repl = if (!is_wasm) @import("repl.zig").repl else void;
const wasm_repl = @import("wasm_repl.zig");
const Renderer = @import("renderer.zig").Renderer;
const io = @import("io.zig");

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

pub fn runFile(allocator: Allocator, file_name: []const u8, args: []const []const u8, flavor: RunFlavor) !void {
    var total_timer = if (!is_wasm) std.time.Timer.start() catch unreachable else {};
    var import_registry = ImportRegistry{};
    var gc = try GarbageCollector.init(allocator);
    gc.type_registry = try TypeRegistry.init(&gc);
    var imports = std.StringHashMapUnmanaged(Parser.ScriptImport){};
    var vm = try VM.init(&gc, &import_registry, flavor);
    vm.jit = if (BuildOptions.jit and BuildOptions.cycle_limit == null)
        JIT.init(&vm)
    else
        null;
    defer {
        if (!is_wasm and vm.jit != null) {
            vm.jit.?.deinit(vm.gc.allocator);
            vm.jit = null;
        }
    }
    var parser = Parser.init(
        &gc,
        &imports,
        false,
        flavor,
    );
    var codegen = CodeGen.init(
        &gc,
        &parser,
        flavor,
        if (vm.jit) |*jit| jit else null,
    );
    defer {
        codegen.deinit();
        vm.deinit();
        parser.deinit();
        // gc.deinit();
        var it = imports.iterator();
        while (it.next()) |kv| {
            kv.value_ptr.*.globals.deinit(allocator);
        }
        imports.deinit(allocator);
        // TODO: free type_registry and its keys which are on the heap
    }

    var file = (if (std.fs.path.isAbsolute(file_name))
        std.fs.openFileAbsolute(file_name, .{})
    else
        std.fs.cwd().openFile(file_name, .{})) catch {
        io.print("File not found", .{});
        return;
    };
    defer file.close();

    const source = try allocator.alloc(u8, (try file.stat()).size);
    defer allocator.free(source);

    _ = try file.readAll(source);

    var timer = try std.time.Timer.start();
    var parsing_time: u64 = undefined;
    var codegen_time: u64 = undefined;
    var running_time: u64 = undefined;

    if (try parser.parse(source, null, file_name)) |ast| {
        if (!is_wasm) {
            parsing_time = timer.read();
            timer.reset();
        }

        if (flavor == .Run or flavor == .Test) {
            const ast_slice = ast.slice();

            if (try codegen.generate(ast_slice)) |function| {
                if (!is_wasm) {
                    codegen_time = timer.read();
                    timer.reset();
                }

                try vm.interpret(
                    ast_slice,
                    function,
                    args,
                );

                if (!is_wasm) {
                    running_time = timer.read();
                }
            } else {
                return Parser.CompileError.Recoverable;
            }

            if (BuildOptions.show_perf and flavor != .Check and flavor != .Fmt) {
                io.print(
                    if (builtin.os.tag != .windows)
                        "\x1b[2mParsing: {[parsing]d}\nCodegen: {[codegen]d}\nRun: {[running]d}\nJIT: {[jit]d}\nGC: {[gc]d}\nTotal: {[total]}\nFull GC: {[gc_full]} | GC: {[gc_light]} | Max allocated: {[max_alloc]}\n\x1b[0m"
                    else
                        "Parsing: {[parsing]d}\nCodegen: {[codegen]d}\nRun: {[running]d}\nJIT: {[jit]d}\nGC: {[gc]d}\nTotal: {[total]}\nFull GC: {[gc_full]} | GC: {[gc_light]} | Max allocated: {[max_alloc]}\n",
                    .{
                        .parsing = std.fmt.fmtDuration(parsing_time),
                        .codegen = std.fmt.fmtDuration(codegen_time),
                        .running = std.fmt.fmtDuration(running_time),
                        .jit = std.fmt.fmtDuration(if (vm.jit) |jit| jit.jit_time else 0),
                        .gc = std.fmt.fmtDuration(gc.gc_time),
                        .total = std.fmt.fmtDuration(if (!is_wasm) total_timer.read() else 0),
                        .gc_full = gc.full_collection_count,
                        .gc_light = gc.light_collection_count,
                        .max_alloc = std.fmt.fmtIntSizeDec(gc.max_allocated),
                    },
                );
            }
        } else if (flavor == .Fmt) {
            var arena = std.heap.ArenaAllocator.init(allocator);
            defer arena.deinit();

            try Renderer(std.fs.File.Writer).render(
                arena.allocator(),
                std.io.getStdOut().writer(),
                ast,
            );
        } else {
            io.print("Formatting and Ast dump is deactivated", .{});
        }
    } else {
        return Parser.CompileError.Recoverable;
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
        \\-f, --fmt              Reformat script, output the result to stdout
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
    else if (res.args.fmt == 1)
        .Fmt
    else if (res.positionals[0].len == 0)
        .Repl
    else
        .Run;

    if (!is_wasm and flavor == .Repl) {
        repl(allocator) catch {
            return 1;
        };
    } else if (!is_wasm and res.positionals[0].len > 0) {
        runFile(
            allocator,
            res.positionals[0][0],
            res.positionals[0][1..],
            flavor,
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

test {
    _ = @import("tests/fmt.zig");
}
