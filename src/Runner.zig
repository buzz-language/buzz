const std = @import("std");
const is_wasm = builtin.cpu.arch.isWasm();
const builtin = @import("builtin");
const _vm = @import("vm.zig");
const VM = _vm.VM;
const Init = _vm.Init;
const CallFrame = _vm.CallFrame;
const Fiber = _vm.Fiber;
const RunFlavor = _vm.RunFlavor;
const ImportRegistry = _vm.ImportRegistry;
const Parser = @import("Parser.zig");
const CodeGen = @import("Codegen.zig");
const Debugger = if (!is_wasm) @import("Debugger.zig") else void;
const GC = @import("GC.zig");
const JIT = if (!is_wasm) @import("Jit.zig") else void;
const TypeRegistry = @import("TypeRegistry.zig");
const Ast = @import("Ast.zig");
const BuildOptions = @import("build_options");
const bz_io = @import("io.zig");
const Renderer = @import("renderer.zig").Renderer;
const Value = @import("value.zig").Value;
const o = @import("obj.zig");
const disassembler = @import("disassembler.zig");
const Perf = @import("Perf.zig");
const Package = @import("Package.zig");

const Runner = @This();

process: Init,
vm: VM,
gc: GC,
parser: Parser,
codegen: CodeGen,
perf: ?*Perf = null,
/// Formatter options used when the runner is executing in `.Fmt` mode.
renderer_options: Renderer.Options = .{},
import_registry: ImportRegistry = .empty,
imports: std.StringHashMapUnmanaged(Parser.ScriptImport) = .empty,
/// DynLib lookup cache
dlib_symbols: std.StringHashMapUnmanaged(Parser.Dlib) = .empty,

pub fn deinit(self: *Runner) void {
    if (!is_wasm and self.vm.jit != null) {
        self.vm.jit.?.deinit();
        self.vm.jit = null;
    }
    self.codegen.deinit();
    self.parser.deinit();
    var it = self.dlib_symbols.valueIterator();
    while (it.next()) |dlib| {
        dlib.deinit(self.gc.allocator);
    }
    self.dlib_symbols.deinit(self.gc.allocator);
    // self.gc.deinit();
    var it2 = self.imports.valueIterator();
    while (it2.next()) |import| {
        import.deinit(self.gc.allocator);
    }
    self.imports.deinit(self.gc.allocator);
    // TODO: free type_registry and its keys which are on the heap
    self.vm.deinit();
}

/// Runner must, most of the time be on the stack, and it contains several circular references
/// So the use provides the ptr to it and this function populates it
pub fn init(runner_ptr: *Runner, process: Init, allocator: std.mem.Allocator, flavor: RunFlavor, debugger: ?*Debugger, perf: ?*Perf) !void {
    runner_ptr.* = .{
        .process = process,
        .gc = try GC.init(allocator),
        .vm = undefined,
        .parser = undefined,
        .codegen = undefined,
        .perf = perf,
    };

    runner_ptr.gc.perf = perf;
    runner_ptr.gc.type_registry = try TypeRegistry.init(&runner_ptr.gc);
    runner_ptr.vm = try VM.init(
        process,
        &runner_ptr.gc,
        &runner_ptr.import_registry,
        flavor,
        debugger,
    );
    runner_ptr.vm.perf = perf;

    runner_ptr.vm.jit = if (BuildOptions.jit and BuildOptions.cycle_limit == null and debugger == null)
        try JIT.init(process, &runner_ptr.gc)
    else
        null;
    if (runner_ptr.vm.jit) |*jit| {
        jit.perf = perf;
    }

    runner_ptr.parser = Parser.init(
        process,
        &runner_ptr.gc,
        &runner_ptr.imports,
        &runner_ptr.dlib_symbols,
        false,
        flavor,
    );
    runner_ptr.parser.perf = perf;
    runner_ptr.parser.root_dir = try runner_ptr.cwdRootDir();

    runner_ptr.codegen = CodeGen.init(
        process,
        &runner_ptr.gc,
        &runner_ptr.parser,
        flavor,
        if (runner_ptr.vm.jit) |*jit| jit else null,
        debugger != null,
    );
    runner_ptr.codegen.perf = perf;
}

/// Returns the absolute current directory to use for source-only parsing.
fn cwdRootDir(runner: *Runner) ![]const u8 {
    return try std.Io.Dir.cwd().realPathFileAlloc(
        runner.process.io,
        ".",
        runner.gc.allocator,
    );
}

/// Resolves the package root used for deterministic package imports.
fn resolveRootDir(
    runner: *Runner,
    provided_root_dir: ?[]const u8,
    absolute_file_path: []const u8,
) ![]const u8 {
    if (provided_root_dir) |root_dir| {
        return try std.Io.Dir.cwd().realPathFileAlloc(
            runner.process.io,
            root_dir,
            runner.gc.allocator,
        );
    }

    // If the entry point lives under a `src` directory, its parent is the
    // package root. This lets `buzz run src/main.buzz` work without `-r`.
    if (std.fs.path.dirname(absolute_file_path)) |dir| {
        var it = std.fs.path.componentIterator(dir);
        var maybe_component = it.last();
        while (maybe_component) |prev_dir| : (maybe_component = it.previous()) {
            if (std.mem.eql(u8, prev_dir.name, "src")) {
                return try runner.gc.allocator.dupe(u8, std.fs.path.dirname(prev_dir.path) orelse ".");
            }
        }
    }

    return try runner.cwdRootDir();
}

pub fn runFile(
    runner: *Runner,
    provided_root_dir: ?[]const u8,
    file_name: []const u8,
    args: []const []const u8,
) !u8 {
    var file_io_scope = Perf.start(runner.perf, .file_io);
    defer file_io_scope.end();

    var file = (if (std.fs.path.isAbsolute(file_name))
        std.Io.Dir.openFileAbsolute(runner.process.io, file_name, .{})
    else
        std.Io.Dir.cwd().openFile(runner.process.io, file_name, .{})) catch {
        bz_io.print(runner.process.io, "File not found", .{});
        return 1;
    };
    defer file.close(runner.process.io);

    var absolute_file_path_buffer: [std.Io.Dir.max_path_bytes]u8 = undefined;
    const absolute_file_path_len = try file.realPath(runner.process.io, &absolute_file_path_buffer);
    const absolute_file_path = try runner.gc.allocator.dupe(
        u8,
        absolute_file_path_buffer[0..absolute_file_path_len],
    );

    const root_dir = try runner.resolveRootDir(provided_root_dir, absolute_file_path);

    const source = try runner.gc.allocator.alloc(u8, (try file.stat(runner.process.io)).size);
    defer if (runner.vm.debugger == null) runner.gc.allocator.free(source);

    _ = try file.readPositionalAll(runner.process.io, source, 0);
    file_io_scope.end();

    if (try runner.parser.parse(source, root_dir, null, absolute_file_path)) |ast| {
        if (runner.vm.flavor != .Fmt) {
            const ast_slice = ast.slice();

            if (try runner.codegen.generate(ast_slice)) |function| {
                if (runner.vm.flavor.runs()) {
                    try runner.vm.interpret(
                        ast_slice,
                        function,
                        args,
                    );
                }
            } else {
                return Parser.CompileError.Recoverable;
            }
        } else {
            var arena = std.heap.ArenaAllocator.init(runner.gc.allocator);
            defer arena.deinit();

            var stdout = bz_io.stdoutWriter(runner.process.io);
            try Renderer.render(
                arena.allocator(),
                &stdout.interface,
                ast,
                runner.renderer_options,
            );
        }
    } else {
        return Parser.CompileError.Recoverable;
    }

    return runner.vm.exit_code;
}

/// Evaluate source using the current parser and vm state and return the value produced if any
/// Used by REPL and Debugger
pub fn runSource(self: *Runner, source: []const u8, name: []const u8) !?Value {
    if (try self.parser.parse(source, self.parser.root_dir, null, name)) |ast| {
        const ast_slice = ast.slice();
        if (try self.codegen.generate(ast_slice)) |function| {
            try self.vm.interpret(
                ast_slice,
                function,
                null,
            );

            // Does the user code ends with a lone expression?
            const fnode = ast.nodes.items(.components)[ast.root.?].Function;
            const statements = ast.nodes.items(.components)[fnode.body.?].Block;
            const last_statement = if (statements.len > 0) statements[statements.len - 1] else null;
            if (last_statement != null and ast.nodes.items(.tag)[last_statement.?] == .Expression) {
                return self.vm.pop();
            }
        } else {
            return Parser.CompileError.Recoverable;
        }
    } else if (self.parser.reporter.last_error == .unclosed) {
        return null;
    } else {
        return Parser.CompileError.Recoverable;
    }

    return null;
}

/// Run a manifest.buzz and get the produced manifest object
pub fn runManifest(self: *Runner, source: []const u8, name: []const u8) !?Value {
    if (try self.parser.parse(source, self.parser.root_dir, null, name)) |ast| {
        const ast_slice = ast.slice();
        if (try self.codegen.generate(ast_slice)) |function| {
            try self.vm.interpret(
                ast_slice,
                function,
                null,
            );

            return self.vm.globals.items[self.vm.globals_count];
        }
    }

    return null;
}

pub fn frameTop(self: *Runner, fiber: *Fiber, frame: *CallFrame) [*]Value {
    return if (self.vm.currentFrame() == frame)
        fiber.stack_top
    else top: {
        var idx: ?usize = 0;
        for (fiber.frames.items, 0..) |*f, i| {
            if (frame == f) {
                idx = i;
                break;
            }
        }

        std.debug.assert(idx != null and idx.? < fiber.frames.items.len);

        break :top fiber.frames.items[idx.? + 1].slots;
    };
}

/// Evaluate expression in its own fiber, child of the selected fiber, and copying the selected frame's locals.
/// Can only work while debugging because local names are not available otherwise
pub fn evaluate(self: *Runner, parent_fiber: *Fiber, parent_frame: *CallFrame, expr: []const u8) !Value {
    std.debug.assert(self.vm.debugger != null);

    // We wrap the expression into a function that takes parent_frame's locals as arguments
    // (we don't need to do it for globals, they'll be accessible by the current state of the parser)
    var source = std.Io.Writer.Allocating.init(self.gc.allocator);
    defer source.deinit();

    try source.writer.print("fun eval(", .{});

    const stack: [*]Value = @ptrCast(parent_fiber.stack);
    const frame_base_idx = parent_frame.slots - stack;
    const top = self.frameTop(parent_fiber, parent_frame);
    const top_idx = top - stack;
    const local_count = @min(top_idx - 1, parent_fiber.locals_dbg.items.len);
    for (frame_base_idx..local_count) |i| {
        const local_dbg = parent_fiber.locals_dbg.items[i];

        if (local_dbg.isObj()) {
            const name = o.ObjString.cast(local_dbg.obj()).?.string;

            // "Hidden" locals start with `$`
            if (name[0] != '$') {
                try source.writer.print(
                    "{s}: {s}, ",
                    .{
                        name,
                        try (try parent_fiber.stack[i + 1].typeOf(&self.gc))
                            .toStringAlloc(self.gc.allocator, false),
                    },
                );
            }
        }
    }

    // Body
    try source.writer.print(") > void !> any => ({s});", .{expr});

    // We put temporarly the parser/codegen in repl flavor to avoid generating a call to main
    const previous_flavor = self.parser.flavor;
    self.parser.flavor = .Repl;
    self.codegen.flavor = .Repl;
    self.vm.flavor = .Repl;
    // We don't want the evaluated expression to be considered by the debugger
    const previous_debugger = self.vm.debugger;
    self.codegen.debugging = false;
    self.vm.debugger = null;
    const previous_current_ast = self.vm.current_ast;
    const previous_global_count = self.parser.globals.items.len;
    defer {
        self.parser.flavor = previous_flavor;
        self.codegen.flavor = previous_flavor;
        self.vm.flavor = previous_flavor;
        self.codegen.debugging = previous_debugger != null;
        self.vm.debugger = previous_debugger;
        self.vm.current_ast = previous_current_ast;
        self.codegen.reporter.last_error = null;
        self.codegen.reporter.panic_mode = false;
        self.parser.reporter.last_error = null;
        self.parser.reporter.panic_mode = false;
    }
    errdefer { // Do the same in case of error
        self.parser.flavor = previous_flavor;
        self.codegen.flavor = previous_flavor;
        self.vm.flavor = previous_flavor;
        self.codegen.debugging = previous_debugger != null;
        self.vm.debugger = previous_debugger;
        self.codegen.reporter.last_error = null;
        self.codegen.reporter.panic_mode = false;
        self.parser.reporter.last_error = null;
        self.parser.reporter.panic_mode = false;
        self.removeParserGlobalsFrom(previous_global_count);
    }

    // Compile it
    if (try self.parser.parse(source.written(), self.parser.root_dir, null, "__eval")) |ast| {
        const ast_slice = ast.slice();
        self.vm.current_ast = ast_slice; // We still use the same AST, but the slice has changed
        if (try self.codegen.generate(ast_slice)) |function| {
            // Create a new fiber run the function into it with the selected frame's locals as arguments
            var fiber = Fiber.init(
                self.gc.allocator,
                try self.gc.type_registry.getTypeDef(
                    .{
                        .optional = false,
                        .def_type = .Fiber,
                        .resolved_type = .{
                            .Fiber = .{
                                .return_type = self.gc.type_registry.void_type,
                                .yield_type = self.gc.type_registry.void_type,
                            },
                        },
                    },
                ),
                null,
                null,
                undefined,
                null,
            ) catch return error.OutOfMemory;
            defer fiber.deinit();

            fiber.is_eval = true;

            const previous_fiber = self.vm.current_fiber;
            defer self.vm.current_fiber = previous_fiber;
            // If program fails, return to original fiber
            errdefer {
                self.vm.current_fiber = previous_fiber;
            }
            self.vm.current_fiber = &fiber;
            fiber.status = .Running;

            // Push function and locals
            self.vm.push(
                (try self.gc.allocateObject(
                    try o.ObjClosure.init(
                        self.gc.allocator,
                        &self.vm,
                        function,
                    ),
                )).toValue(),
            );

            // Call
            try self.vm.callValue(
                self.vm.peek(0),
                0,
                null,
            );

            // Run it
            try self.vm.run();

            // This will only have defined our eval function as a new global,
            // now we get that global and call it with the locals as arguments
            // We also remove it from the globals to avoid polluting the globals and avoid collision with other evaluates
            const eval_value = self.vm.globals.pop().?;
            const eval_global = self.parser.globals.pop().?;
            _ = self.parser.globals_lookup.removeContext(
                eval_global.qualified_name,
                .{ .ast = &self.parser.ast },
            );
            _ = self.vm.globals_dbg.pop();
            self.vm.globals_count -= 1;

            // Should be our eval function
            std.debug.assert(
                eval_value.isObj() and
                    o.ObjClosure.cast(eval_value.obj()) != null and
                    std.mem.eql(
                        u8,
                        o.ObjClosure.cast(eval_value.obj()).?.function.type_def.resolved_type.?.Function.name.string,
                        "eval",
                    ),
            );

            self.vm.push(eval_value);

            // Push locals
            for (frame_base_idx..local_count) |i| {
                self.vm.push(parent_fiber.stack[i + 1]);
            }

            // Call
            const arg_count: u8 = @intCast(local_count - frame_base_idx);
            try self.vm.callValue(
                self.vm.peek(arg_count),
                arg_count,
                null,
            );

            // Run again
            try self.vm.run();

            // We always return something, even void
            return self.vm.pop();
        }
    }

    return Parser.CompileError.Recoverable;
}

/// Removes parser globals and lookup entries added after `start`.
fn removeParserGlobalsFrom(self: *Runner, start: usize) void {
    for (self.parser.globals.items[start..]) |global| {
        _ = self.parser.globals_lookup.removeContext(
            global.qualified_name,
            .{ .ast = &self.parser.ast },
        );
    }

    self.parser.globals.shrinkRetainingCapacity(start);
}
