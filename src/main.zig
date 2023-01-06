const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const _vm = @import("./vm.zig");
const VM = _vm.VM;
const ImportRegistry = _vm.ImportRegistry;
const _parser = @import("./parser.zig");
const Parser = _parser.Parser;
const CompileError = _parser.CompileError;
const CodeGen = @import("./codegen.zig").CodeGen;
const _obj = @import("./obj.zig");
const ObjString = _obj.ObjString;
const ObjTypeDef = _obj.ObjTypeDef;
const TypeRegistry = @import("./memory.zig").TypeRegistry;
const FunctionNode = @import("./node.zig").FunctionNode;
const BuildOptions = @import("build_options");
const clap = @import("ext/clap/clap.zig");
const GarbageCollector = @import("./memory.zig").GarbageCollector;
const JIT = @import("./jit.zig").JIT;

fn toNullTerminated(allocator: std.mem.Allocator, string: []const u8) ![:0]u8 {
    return allocator.dupeZ(u8, string);
}

const RunFlavor = enum {
    Run,
    Test,
    Check,
    Fmt,
    Ast,
};

fn runFile(allocator: Allocator, file_name: []const u8, args: [][:0]u8, flavor: RunFlavor) !void {
    var import_registry = ImportRegistry.init(allocator);
    var gc = GarbageCollector.init(allocator);
    gc.type_registry = TypeRegistry{
        .gc = &gc,
        .registry = std.StringHashMap(*ObjTypeDef).init(allocator),
    };
    var imports = std.StringHashMap(Parser.ScriptImport).init(allocator);
    var vm = try VM.init(&gc, &import_registry, flavor == .Test);
    try vm.initJIT();
    var parser = Parser.init(&gc, &imports, false);
    var codegen = CodeGen.init(&gc, &parser, flavor == .Test);
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

    var file = (if (std.fs.path.isAbsolute(file_name)) std.fs.openFileAbsolute(file_name, .{}) else std.fs.cwd().openFile(file_name, .{})) catch {
        std.debug.print("File not found", .{});
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

    if (try parser.parse(source, file_name)) |function_node| {
        parsing_time = timer.read();
        timer.reset();

        if (try codegen.generate(FunctionNode.cast(function_node).?)) |function| {
            codegen_time = timer.read();
            timer.reset();

            switch (flavor) {
                .Run, .Test => try vm.interpret(
                    function,
                    args,
                ),
                .Fmt => {
                    var formatted = std.ArrayList(u8).init(allocator);
                    defer formatted.deinit();

                    try function_node.render(function_node, &formatted.writer(), 0);

                    std.debug.print("{s}", .{formatted.items});
                },
                .Ast => {
                    var json = std.ArrayList(u8).init(allocator);
                    defer json.deinit();

                    try function_node.toJson(function_node, &json.writer());

                    var without_nl = try std.mem.replaceOwned(u8, allocator, json.items, "\n", " ");
                    defer allocator.free(without_nl);

                    _ = try std.io.getStdOut().write(without_nl);
                },
                else => {},
            }

            running_time = timer.read();
        } else {
            return CompileError.Recoverable;
        }

        if (BuildOptions.show_perf and flavor != .Check and flavor != .Fmt) {
            const parsing_ms: f64 = @intToFloat(f64, parsing_time) / 1000000;
            const codegen_ms: f64 = @intToFloat(f64, codegen_time) / 1000000;
            const running_ms: f64 = @intToFloat(f64, running_time) / 1000000;
            const gc_ms: f64 = @intToFloat(f64, gc.gc_time) / 1000000;
            std.debug.print(
                "\u{001b}[2mParsing: {d} ms | Codegen: {d} ms | Run: {d} ms | Total: {d} ms\nGC: {d} ms | Full GC: {} | GC: {} | Max allocated: {} bytes\u{001b}[0m\n",
                .{
                    parsing_ms,
                    codegen_ms,
                    running_ms,
                    parsing_ms + codegen_ms + running_ms,
                    gc_ms,
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

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    var allocator: std.mem.Allocator = if (builtin.mode == .Debug)
        gpa.allocator()
    else if (BuildOptions.use_mimalloc)
        @import("./mimalloc.zig").mim_allocator
    else
        std.heap.c_allocator;

    const params = comptime clap.parseParamsComptime(
        \\-h, --help    Show help and exit
        \\-t, --test    Run test blocks in provided script
        \\-c, --check   Check script for error without running it
        \\-f, --fmt     Format script
        \\-a, --tree    Dump AST as JSON
        \\-v, --version Print version and exit
        \\<str>...
        \\
    );

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
    }) catch |err| {
        // Report useful error and exit
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer res.deinit();

    if (res.args.version) {
        std.debug.print(
            "👨‍🚀 buzz {s}-{s} Copyright (C) 2021-2022 Benoit Giannangeli\nBuilt with Zig {} {s}\nAllocator: {s}\n",
            .{
                if (BuildOptions.version.len > 0) BuildOptions.version else "unreleased",
                BuildOptions.sha,
                builtin.zig_version,
                switch (builtin.mode) {
                    .ReleaseFast => "release-fast",
                    .ReleaseSafe => "release-safe",
                    .ReleaseSmall => "release-small",
                    .Debug => "debug",
                },
                if (builtin.mode == .Debug)
                    "gpa"
                else if (BuildOptions.use_mimalloc) "mimalloc" else "c_allocator",
            },
        );

        std.os.exit(0);
    }

    if (res.args.help or res.positionals.len == 0) {
        std.debug.print("👨‍🚀 buzz A small/lightweight typed scripting language\n\nUsage: buzz ", .{});

        try clap.usage(
            std.io.getStdErr().writer(),
            clap.Help,
            &params,
        );

        std.debug.print("\n\n", .{});

        try clap.help(
            std.io.getStdErr().writer(),
            clap.Help,
            &params,
            .{
                .description_on_new_line = false,
                .description_indent = 4,
                .spacing_between_parameters = 0,
            },
        );

        std.os.exit(0);
    }

    var positionals = std.ArrayList([:0]u8).init(allocator);
    for (res.positionals) |pos| {
        try positionals.append(try toNullTerminated(allocator, pos));
    }
    defer {
        for (positionals.items) |pos| {
            allocator.free(pos);
        }
        positionals.deinit();
    }

    const flavor: RunFlavor = if (res.args.check) RunFlavor.Check else if (res.args.@"test") RunFlavor.Test else if (res.args.fmt) RunFlavor.Fmt else if (res.args.tree) RunFlavor.Ast else RunFlavor.Run;

    runFile(
        allocator,
        res.positionals[0],
        positionals.items[1..],
        flavor,
    ) catch {
        // TODO: should probably choses appropriate error code
        std.os.exit(1);
    };

    std.os.exit(0);
}

test "Testing behavior" {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .safety = true,
    }){};
    var allocator: Allocator = gpa.allocator();

    var count: usize = 0;
    var fail_count: usize = 0;
    {
        var test_dir = try std.fs.cwd().openIterableDir("tests", .{});
        var it = test_dir.iterate();

        while (try it.next()) |file| : (count += 1) {
            if (file.kind == .File and std.mem.endsWith(u8, file.name, ".buzz")) {
                var file_name: []u8 = try allocator.alloc(u8, 6 + file.name.len);
                defer allocator.free(file_name);

                var had_error: bool = false;
                runFile(allocator, try std.fmt.bufPrint(file_name, "tests/{s}", .{file.name}), null, .Test) catch {
                    std.debug.print("\u{001b}[31m[{s} ✕]\u{001b}[0m\n", .{file.name});
                    had_error = true;
                    fail_count += 1;
                };

                if (!had_error) {
                    std.debug.print("\u{001b}[32m[{s} ✓]\u{001b}[0m\n", .{file.name});
                }
            }
        }
    }

    {
        var test_dir = try std.fs.cwd().openIterableDir("tests/compile_errors", .{});
        var it = test_dir.iterate();

        while (try it.next()) |file| : (count += 1) {
            if (file.kind == .File and std.mem.endsWith(u8, file.name, ".buzz")) {
                var file_name: []u8 = try allocator.alloc(u8, 21 + file.name.len);
                defer allocator.free(file_name);
                _ = try std.fmt.bufPrint(file_name, "tests/compile_errors/{s}", .{file.name});

                // First line of test file is expected error message
                const test_file = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
                const reader = test_file.reader();
                const first_line = try reader.readUntilDelimiterAlloc(allocator, '\n', 16 * 8 * 64);
                defer allocator.free(first_line);

                const result = try std.ChildProcess.exec(
                    .{
                        .allocator = allocator,
                        .argv = ([_][]const u8{
                            "./zig-out/bin/buzz",
                            "-t",
                            file_name,
                        })[0..],
                    },
                );

                if (!std.mem.containsAtLeast(u8, result.stderr, 1, first_line[2..])) {
                    fail_count += 1;
                    std.debug.print(
                        "Expected error `{s}` got `{s}`\n",
                        .{
                            first_line[2..],
                            result.stderr,
                        },
                    );

                    std.debug.print("\u{001b}[31m[{s}... ✕]\u{001b}[0m\n", .{file.name});
                } else {
                    std.debug.print("\u{001b}[32m[{s}... ✓]\u{001b}[0m\n", .{file.name});
                }
            }
        }
    }

    if (fail_count == 0) {
        std.debug.print("\n\u{001b}[32m", .{});
    } else {
        std.debug.print("\n\u{001b}[31m", .{});
    }

    std.debug.print("Ran {}, Failed: {}\u{001b}[0m\n", .{
        count,
        fail_count,
    });

    std.os.exit(if (fail_count == 0) 0 else 1);
}
