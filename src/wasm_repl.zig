const std = @import("std");
const builtin = @import("builtin");
const _vm = @import("vm.zig");
const VM = _vm.VM;
const ImportRegistry = _vm.ImportRegistry;
const wasm = @import("wasm.zig");
const BuildOptions = @import("build_options");
const _mem = @import("memory.zig");
const GarbageCollector = _mem.GarbageCollector;
const TypeRegistry = _mem.TypeRegistry;
const _obj = @import("obj.zig");
const ObjTypeDef = _obj.ObjTypeDef;
const Parser = @import("Parser.zig");
const CompileError = Parser.CompileError;
const CodeGen = @import("Codegen.zig");
const Value = @import("value.zig").Value;
const disassembler = @import("disassembler.zig");
const DumpState = disassembler.DumpState;
const Scanner = @import("scanner.zig").Scanner;
const printBanner = @import("repl.zig").printBanner;

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
    import_registry.* = ImportRegistry.init(allocator);

    const gc = allocator.create(GarbageCollector) catch unreachable;
    gc.* = GarbageCollector.init(allocator);
    gc.type_registry = TypeRegistry{
        .gc = gc,
        .registry = std.StringHashMap(*ObjTypeDef).init(allocator),
    };

    const imports = allocator.create(std.StringHashMap(Parser.ScriptImport)) catch unreachable;
    imports.* = std.StringHashMap(Parser.ScriptImport).init(allocator);

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

    printBanner(std.io.getStdOut().writer(), true);

    // Import std and debug as commodity
    _ = runSource(
        "import \"std\";import \"debug\";",
        "REPL",
        vm,
        codegen,
        parser,
    ) catch |err| {
        std.debug.print("Failed with error: {}\n", .{err});

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
    var stdout = std.io.getStdOut().writer();
    var stderr = std.io.getStdErr().writer();
    var stdin = std.io.getStdIn().reader();

    var previous_global_top = ctx.vm.globals_count;
    var previous_parser_globals = ctx.parser.globals.clone() catch unreachable;
    var previous_globals = ctx.vm.globals.clone() catch unreachable;
    var previous_type_registry = ctx.vm.gc.type_registry.registry.clone() catch unreachable;

    const source_buffer = ctx.vm.gc.allocator.alloc(u8, 1024) catch unreachable;
    errdefer ctx.vm.gc.allocator.free(source_buffer);
    const len = stdin.readAll(source_buffer) catch unreachable;

    if (len == 0) {
        ctx.vm.gc.allocator.free(source_buffer);
        return;
    }

    const source = source_buffer[0..len];

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

    if (!ctx.parser.reporter.had_error and !ctx.codegen.reporter.had_error) {
        // FIXME: why can't I deinit those?
        // previous_parser_globals.deinit();
        previous_parser_globals = ctx.parser.globals.clone() catch unreachable;
        // previous_globals.deinit();
        previous_globals = ctx.vm.globals.clone() catch unreachable;
        // previous_type_registry.deinit();
        previous_type_registry = ctx.vm.gc.type_registry.registry.clone() catch unreachable;

        // Dump top of stack
        if (previous_global_top != ctx.vm.globals_count or expr != null) {
            previous_global_top = ctx.vm.globals_count;

            const value = expr orelse ctx.vm.globals.items[previous_global_top];

            var value_str = std.ArrayList(u8).init(ctx.vm.gc.allocator);
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

    ctx.parser.reporter.had_error = false;
    ctx.parser.reporter.panic_mode = false;
    ctx.codegen.reporter.had_error = false;
    ctx.codegen.reporter.panic_mode = false;
}

fn runSource(
    source: []const u8,
    file_name: []const u8,
    vm: *VM,
    codegen: *CodeGen,
    parser: *Parser,
) !?Value {
    if (try parser.parse(source, file_name)) |ast| {
        if (try codegen.generate(ast)) |function| {
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
        } else {
            return CompileError.Recoverable;
        }
    } else {
        return CompileError.Recoverable;
    }

    return null;
}
