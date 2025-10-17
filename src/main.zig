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
const TypeRegistry = @import("TypeRegistry.zig");
const Ast = @import("Ast.zig");
const BuildOptions = @import("build_options");
const clap = @import("clap");
const GC = @import("GC.zig");
const JIT = if (!is_wasm) @import("Jit.zig") else void;
const is_wasm = builtin.cpu.arch.isWasm();
const _repl = if (!is_wasm) @import("repl.zig") else void;
const repl = if (!is_wasm) _repl.repl else void;
const wasm_repl = @import("wasm_repl.zig");
const Renderer = @import("renderer.zig").Renderer;
const io = @import("io.zig");
const Runner = @import("Runner.zig");

pub export const initRepl_export = wasm_repl.initRepl;
pub export const runLine_export = wasm_repl.runLine;

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

pub fn main() u8 {
    if (is_wasm) {
        return 1;
    }

    var gpa = std.heap.DebugAllocator(.{ .safety = builtin.mode == .Debug }){};
    const allocator = if (builtin.mode == .Debug or is_wasm)
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
        diag.report(io.stderrWriter, err) catch {};
        return 1;
    };
    defer res.deinit();

    if (res.args.version == 1) {
        _repl.printBanner(io.stdoutWriter, true);

        return 0;
    }

    if (res.args.help == 1) {
        io.print("👨‍🚀 buzz A small/lightweight typed scripting language\n\nUsage: buzz ", .{});

        clap.usage(
            io.stderrWriter,
            clap.Help,
            &params,
        ) catch return 1;

        io.print("\n\n", .{});

        clap.help(
            io.stderrWriter,
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
        var list = std.ArrayList([]const u8).empty;

        for (res.args.library) |path| {
            list.append(allocator, path) catch return 1;
        }

        Parser.user_library_paths = list.toOwnedSlice(allocator) catch return 1;
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
        var runner: Runner = undefined;

        runner.runFile(
            allocator,
            res.positionals[0][0],
            res.positionals[0][1..],
            flavor,
            null,
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
