const std = @import("std");
const is_wasm = builtin.cpu.arch.isWasm();
const builtin = @import("builtin");
const _vm = @import("vm.zig");
const VM = _vm.VM;
const RunFlavor = _vm.RunFlavor;
const ImportRegistry = _vm.ImportRegistry;
const Parser = @import("Parser.zig");
const CodeGen = @import("Codegen.zig");
const Debugger = @import("Debugger.zig");
const GC = @import("GC.zig");
const JIT = if (!is_wasm) @import("Jit.zig") else void;
const TypeRegistry = @import("TypeRegistry.zig");
const Ast = @import("Ast.zig");
const BuildOptions = @import("build_options");
const io = @import("io.zig");
const Renderer = @import("renderer.zig").Renderer;

const Runner = @This();

vm: VM,
gc: GC,
parser: Parser,
codegen: CodeGen,
import_registry: ImportRegistry = .empty,
imports: std.StringHashMapUnmanaged(Parser.ScriptImport) = .empty,

pub fn deinit(self: *Runner) void {
    self.codegen.deinit();
    self.parser.deinit();
    // self.gc.deinit();
    var it = self.imports.iterator();
    while (it.next()) |kv| {
        kv.value_ptr.*.globals.deinit(self.gc.allocator);
    }
    self.imports.deinit(self.gc.allocator);
    // TODO: free type_registry and its keys which are on the heap
    if (!is_wasm and self.vm.jit != null) {
        self.vm.jit.?.deinit(self.gc.allocator);
        self.vm.jit = null;
    }
    self.vm.deinit();
}

pub fn runFile(
    runner: *Runner,
    allocator: std.mem.Allocator,
    file_name: []const u8,
    args: []const []const u8,
    flavor: RunFlavor,
    debugger: ?*Debugger,
) !void {
    var total_timer = if (!is_wasm) std.time.Timer.start() catch unreachable else {};

    runner.* = .{
        .gc = try GC.init(allocator),
        .vm = undefined,
        .parser = undefined,
        .codegen = undefined,
    };
    defer if (debugger == null) runner.deinit();

    runner.gc.type_registry = try TypeRegistry.init(&runner.gc);
    runner.vm = try VM.init(
        &runner.gc,
        &runner.import_registry,
        flavor,
        debugger,
    );

    runner.vm.jit = if (BuildOptions.jit and BuildOptions.cycle_limit == null and debugger == null)
        JIT.init(&runner.vm)
    else
        null;

    runner.parser = Parser.init(
        &runner.gc,
        &runner.imports,
        false,
        flavor,
    );

    runner.codegen = CodeGen.init(
        &runner.gc,
        &runner.parser,
        flavor,
        if (runner.vm.jit) |*jit| jit else null,
        debugger != null,
    );

    var file = (if (std.fs.path.isAbsolute(file_name))
        std.fs.openFileAbsolute(file_name, .{})
    else
        std.fs.cwd().openFile(file_name, .{})) catch {
        io.print("File not found", .{});
        return;
    };
    defer file.close();

    const source = try allocator.alloc(u8, (try file.stat()).size);
    defer if (debugger == null) allocator.free(source);

    _ = try file.readAll(source);

    var timer = try std.time.Timer.start();
    var parsing_time: u64 = 0;
    var codegen_time: u64 = 0;
    var running_time: u64 = 0;

    if (try runner.parser.parse(source, null, file_name)) |ast| {
        if (!is_wasm) {
            parsing_time = timer.read();
            timer.reset();
        }

        if (flavor != .Fmt) {
            const ast_slice = ast.slice();

            if (try runner.codegen.generate(ast_slice)) |function| {
                if (!is_wasm) {
                    codegen_time = timer.read();
                    timer.reset();
                }

                if (flavor.runs()) {
                    try runner.vm.interpret(
                        ast_slice,
                        function,
                        args,
                    );
                }

                if (!is_wasm) {
                    running_time = timer.read();
                }
            } else {
                return Parser.CompileError.Recoverable;
            }

            if (BuildOptions.show_perf and flavor != .Check and flavor != .Fmt) {
                io.print(
                    if (builtin.os.tag != .windows)
                        "\x1b[2mParsing: {[parsing]D}\nCodegen: {[codegen]D}\nRun: {[running]D}\nJIT: {[jit]D}\nGC: {[gc]D}\nTotal: {[total]D}\nFull GC: {[gc_full]} | GC: {[gc_light]} | Max allocated: {[max_alloc]B}\n\x1b[0m"
                    else
                        "Parsing: {[parsing]D}\nCodegen: {[codegen]D}\nRun: {[running]D}\nJIT: {[jit]D}\nGC: {[gc]D}\nTotal: {[total]D}\nFull GC: {[gc_full]} | GC: {[gc_light]} | Max allocated: {[max_alloc]B}\n",
                    .{
                        .parsing = parsing_time,
                        .codegen = codegen_time,
                        .running = running_time,
                        .jit = if (runner.vm.jit) |jit| jit.jit_time else 0,
                        .gc = runner.gc.gc_time,
                        .total = if (!is_wasm) total_timer.read() else 0,
                        .gc_full = runner.gc.full_collection_count,
                        .gc_light = runner.gc.light_collection_count,
                        .max_alloc = runner.gc.max_allocated,
                    },
                );
            }
        } else {
            var arena = std.heap.ArenaAllocator.init(allocator);
            defer arena.deinit();

            try Renderer.render(
                arena.allocator(),
                io.stdoutWriter,
                ast,
            );
        }
    } else {
        return Parser.CompileError.Recoverable;
    }
}
