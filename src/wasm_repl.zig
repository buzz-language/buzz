const std = @import("std");
const builtin = @import("builtin");
const _vm = @import("vm.zig");
const VM = _vm.VM;
const ImportRegistry = _vm.ImportRegistry;
const wasm = @import("wasm.zig");
const BuildOptions = @import("build_options");
const GC = @import("GC.zig");
const TypeRegistry = @import("TypeRegistry.zig");
const _obj = @import("obj.zig");
const ObjTypeDef = _obj.ObjTypeDef;
const Parser = @import("Parser.zig");
const CompileError = Parser.CompileError;
const CodeGen = @import("Codegen.zig");
const Value = @import("value.zig").Value;
const disassembler = @import("disassembler.zig");
const DumpState = disassembler.DumpState;
const Scanner = @import("Scanner.zig");
const printBanner = @import("repl.zig").printBanner;
const io = @import("io.zig");

pub const ReplCtx = extern struct {
    vm: *VM,
    parser: *Parser,
    codegen: *CodeGen,
};

var gpa = std.heap.GeneralPurposeAllocator(.{
    .safety = builtin.mode == .Debug,
}){};
const allocator = gpa.allocator();

pub export fn initRepl() *ReplCtx {
    const import_registry = allocator.create(ImportRegistry) catch unreachable;
    import_registry.* = .{};

    const gc = allocator.create(GC) catch unreachable;
    gc.* = GC.init(allocator) catch unreachable;
    gc.type_registry = TypeRegistry.init(gc) catch unreachable;

    const imports = allocator.create(std.StringHashMapUnmanaged(Parser.ScriptImport)) catch unreachable;
    imports.* = .{};

    const vm = allocator.create(VM) catch unreachable;
    vm.* = try VM.init(
        gc,
        import_registry,
        .Repl,
    );

    const parser = vm.gc.allocator.create(Parser) catch unreachable;
    parser.* = Parser.init(
        gc,
        imports,
        false,
        .Repl,
    );

    const codegen = vm.gc.allocator.create(CodeGen) catch unreachable;
    codegen.* = CodeGen.init(
        gc,
        parser,
        .Repl,
        null,
    );

    printBanner(io.stdoutWriter, true);

    // Import std and debug as commodity
    _ = runSource(
        "import \"std\";import \"debug\";",
        "REPL",
        vm,
        codegen,
        parser,
    ) catch |err| {
        io.print("Failed with error: {}\n", .{err});

        unreachable;
    };

    const ctx = vm.gc.allocator.create(ReplCtx) catch unreachable;
    ctx.* = .{
        .vm = vm,
        .parser = parser,
        .codegen = codegen,
    };

    return ctx;
}

pub export fn runLine(ctx: *ReplCtx) void {
    var stdout = io.stdoutWriter;
    var stderr = io.stderrWriter;

    var reader_buffer = [_]u8{0};
    var stdin_reader = io.stdinReader(reader_buffer[0..]);
    var stdin = io.AllocatedReader{
        .reader = &stdin_reader,
    };

    var previous_global_top = ctx.vm.globals_count;
    var previous_parser_globals = ctx.parser.globals.clone(ctx.parser.gc.allocator) catch unreachable;
    var previous_globals = ctx.vm.globals.clone(ctx.parser.gc.allocator) catch unreachable;
    var previous_type_registry = ctx.vm.gc.type_registry.registry.clone(ctx.parser.gc.allocator) catch unreachable;

    const source = stdin.readAll(ctx.vm.gc.allocator) catch unreachable;
    errdefer ctx.vm.gc.allocator.free(source);

    if (source.len == 0) {
        ctx.vm.gc.allocator.free(source);
        return;
    }

    const expr = runSource(
        source,
        "REPL",
        ctx.vm,
        ctx.codegen,
        ctx.parser,
    ) catch |err| failed: {
        if (BuildOptions.debug) {
            stderr.print("Failed with error {}\n", .{err}) catch unreachable;
        }

        break :failed null;
    };

    if (ctx.parser.reporter.last_error == null and ctx.codegen.reporter.last_error == null) {
        // FIXME: why can't I deinit those?
        // previous_parser_globals.deinit();
        previous_parser_globals = ctx.parser.globals.clone(ctx.parser.gc.allocator) catch unreachable;
        // previous_globals.deinit();
        previous_globals = ctx.vm.globals.clone(ctx.parser.gc.allocator) catch unreachable;
        // previous_type_registry.deinit();
        previous_type_registry = ctx.vm.gc.type_registry.registry.clone(ctx.parser.gc.allocator) catch unreachable;

        // Dump top of stack
        if (previous_global_top != ctx.vm.globals_count or expr != null) {
            previous_global_top = ctx.vm.globals_count;

            const value = expr orelse ctx.vm.globals.items[previous_global_top];

            var value_str = std.array_list.Managed(u8).init(ctx.vm.gc.allocator);
            defer value_str.deinit();
            var state = DumpState.init(ctx.vm);

            state.valueDump(
                value,
                value_str.writer(),
                false,
            );

            var scanner = Scanner.init(
                ctx.vm.gc.allocator,
                "REPL",
                value_str.items,
            );
            scanner.highlight(stdout, false);

            stdout.writeAll("\n") catch unreachable;
        }
    } else {
        // We might have declared new globals, types, etc. and encounter an error
        // FIXME: why can't I deinit those?
        // parser.globals.deinit();
        ctx.parser.globals = previous_parser_globals;

        // vm.globals.deinit();
        ctx.vm.globals = previous_globals;
        ctx.vm.globals_count = previous_global_top;

        // gc.type_registry.registry.deinit();
        ctx.vm.gc.type_registry.registry = previous_type_registry;
    }

    ctx.parser.reporter.last_error = null;
    ctx.parser.reporter.panic_mode = false;
    ctx.codegen.reporter.last_error = null;
    ctx.codegen.reporter.panic_mode = false;
}

fn runSource(
    source: []const u8,
    file_name: []const u8,
    vm: *VM,
    codegen: *CodeGen,
    parser: *Parser,
) !?Value {
    if (try parser.parse(source, null, file_name)) |ast| {
        const ast_slice = ast.slice();
        if (try codegen.generate(ast_slice)) |function| {
            try vm.interpret(
                ast_slice,
                function,
                null,
            );

            // Does the user code ends with a lone expression?
            const fnode = ast_slice.nodes.items(.components)[ast.root.?].Function;
            const statements = ast_slice.nodes.items(.components)[fnode.body.?].Block;
            const last_statement = if (statements.len > 0) statements[statements.len - 1] else null;
            if (last_statement != null and ast_slice.nodes.items(.tag)[last_statement.?] == .Expression) {
                return vm.pop();
            }
        } else {
            return CompileError.Recoverable;
        }
    } else {
        return CompileError.Recoverable;
    }

    return null;
}
