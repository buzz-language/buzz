const std = @import("std");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const BuildOptions = @import("build_options");
const v = @import("vm.zig");
const memory = @import("memory.zig");
const Parser = @import("Parser.zig");
const Jit = @import("Jit.zig");
const Codegen = @import("Codegen.zig");
const io = @import("io.zig");

pub fn run(allocator: std.mem.Allocator, flavor: v.RunFlavor, script_name: []const u8, source: []const u8, cli_args: ?[]const []const u8) !void {
    var total_timer = if (!is_wasm)
        std.time.Timer.start() catch unreachable
    else {};

    var import_registry = v.ImportRegistry.init(allocator);
    var gc = try memory.GarbageCollector.init(allocator);
    gc.type_registry = try memory.TypeRegistry.init(&gc);
    var imports = std.StringHashMapUnmanaged(Parser.ScriptImport){};
    var vm = try v.VM.init(&gc, &import_registry, flavor);
    vm.jit = if (BuildOptions.jit and BuildOptions.cycle_limit == null)
        Jit.init(&vm)
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
    var codegen = Codegen.init(
        &gc,
        &parser,
        flavor,
        if (vm.jit) |*jit| jit else null,
    );
    defer {
        codegen.deinit();
        vm.deinit();
        parser.deinit();
        var it = imports.iterator();
        while (it.next()) |kv| {
            kv.value_ptr.*.globals.deinit(allocator);
        }
        imports.deinit(allocator);
        gc.deinit();
    }

    var timer = try std.time.Timer.start();
    var parsing_time: u64 = undefined;
    var codegen_time: u64 = undefined;
    var running_time: u64 = undefined;

    if (try parser.parse(source, null, script_name)) |ast| {
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
                    cli_args,
                );

                if (!is_wasm) {
                    running_time = timer.read();
                }
            } else {
                return Parser.CompileError.Recoverable;
            }

            if (BuildOptions.show_perf and flavor != .Check and flavor != .Fmt) {
                std.io.print(
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
        } else {
            io.print("Formatting and Ast dump is deactivated", .{});
        }
    } else {
        return Parser.CompileError.Recoverable;
    }
}

pub fn runFile(allocator: std.mem.Allocator, flavor: v.RunFlavor, file_name: []const u8, cli_args: ?[]const []const u8) !void {
    var file = (if (std.fs.path.isAbsolute(file_name))
        std.fs.openFileAbsolute(file_name, .{})
    else
        std.fs.cwd().openFile(file_name, .{})) catch {
        io.print("File not found", .{});
        return error.FileNotFound;
    };
    defer file.close();

    const source = try allocator.alloc(u8, (try file.stat()).size);
    defer allocator.free(source);

    _ = try file.readAll(source);

    try run(allocator, flavor, file_name, source, cli_args);
}
