const std = @import("std");
const is_wasm = builtin.cpu.arch.isWasm();
const builtin = @import("builtin");
const _vm = @import("vm.zig");
const VM = _vm.VM;
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
const io = @import("io.zig");
const Renderer = @import("renderer.zig").Renderer;
const Value = @import("value.zig").Value;
const o = @import("obj.zig");
const disassembler = @import("disassembler.zig");

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

/// Runner must, most of the time be on the stack, and it contains several circular references
/// So the use provides the ptr to it and this function populates it
pub fn init(runner_ptr: *Runner, allocator: std.mem.Allocator, flavor: RunFlavor, debugger: ?*Debugger) !void {
    runner_ptr.* = .{
        .gc = try GC.init(allocator),
        .vm = undefined,
        .parser = undefined,
        .codegen = undefined,
    };

    runner_ptr.gc.type_registry = try TypeRegistry.init(&runner_ptr.gc);
    runner_ptr.vm = try VM.init(
        &runner_ptr.gc,
        &runner_ptr.import_registry,
        flavor,
        debugger,
    );

    runner_ptr.vm.jit = if (BuildOptions.jit and BuildOptions.cycle_limit == null and debugger == null)
        JIT.init(&runner_ptr.vm)
    else
        null;

    runner_ptr.parser = Parser.init(
        &runner_ptr.gc,
        &runner_ptr.imports,
        false,
        flavor,
    );

    runner_ptr.codegen = CodeGen.init(
        &runner_ptr.gc,
        &runner_ptr.parser,
        flavor,
        if (runner_ptr.vm.jit) |*jit| jit else null,
        debugger != null,
    );
}

pub fn runFile(
    runner: *Runner,
    file_name: []const u8,
    args: []const []const u8,
) !void {
    var file = (if (std.fs.path.isAbsolute(file_name))
        std.fs.openFileAbsolute(file_name, .{})
    else
        std.fs.cwd().openFile(file_name, .{})) catch {
        io.print("File not found", .{});
        return;
    };
    defer file.close();

    const source = try runner.gc.allocator.alloc(u8, (try file.stat()).size);
    defer if (runner.vm.debugger == null) runner.gc.allocator.free(source);

    _ = try file.readAll(source);

    if (try runner.parser.parse(source, null, file_name)) |ast| {
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

/// Evaluate source using the current parser and vm state and return the value produced if any
/// Used by REPL and Debugger
pub fn runSource(self: *Runner, source: []const u8, name: []const u8) !?Value {
    if (try self.parser.parse(source, null, name)) |ast| {
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
    const previous_global_count = self.parser.globals.items.len;
    defer {
        self.parser.flavor = previous_flavor;
        self.codegen.flavor = previous_flavor;
        self.vm.flavor = previous_flavor;
        self.codegen.debugging = previous_debugger != null;
        self.vm.debugger = previous_debugger;
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
        self.parser.globals.shrinkRetainingCapacity(previous_global_count);
    }

    // Compile it
    if (try self.parser.parse(source.written(), null, "__eval")) |ast| {
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
            self.vm.run();

            // This will only have defined our eval function as a new global,
            // now we get that global and call it with the locals as arguments
            // We also remove it from the globals to avoid polluting the globals and avoid collision with other evaluates
            const eval_value = self.vm.globals.pop().?;
            _ = self.parser.globals.pop();
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
            self.vm.run();

            // We always return something, even void
            return self.vm.pop();
        }
    }

    return Parser.CompileError.Recoverable;
}
