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
var Config = @import("./config.zig").Config;
const clap = @import("ext/clap/clap.zig");
const GarbageCollector = @import("./memory.zig").GarbageCollector;

fn toNullTerminated(allocator: std.mem.Allocator, string: []const u8) ![:0]u8 {
    return allocator.dupeZ(u8, string);
}

fn print_usage(params: []const clap.Param(clap.Help)) !void {
    std.debug.print("👨‍🚀 buzz A small/lightweight typed scripting language\n\nUsage: buzz ", .{});

    try clap.usage(
        std.io.getStdErr().writer(),
        clap.Help,
        params,
    );

    std.debug.print("\n\n", .{});

    try clap.help(
        std.io.getStdErr().writer(),
        clap.Help,
        params,
        .{
            .description_on_new_line = false,
            .description_indent = 4,
            .spacing_between_parameters = 0,
        },
    );
}

const RunFlavor = enum {
    Run,
    Test,
    Check,
};

fn runFile(allocator: Allocator, file_name: []const u8, args: ?[][:0]u8, flavor: RunFlavor) !void {
    var import_registry = ImportRegistry.init(allocator);
    var gc = GarbageCollector.init(allocator);
    gc.type_registry = TypeRegistry{
        .gc = &gc,
        .registry = std.StringHashMap(*ObjTypeDef).init(allocator),
    };
    var imports = std.StringHashMap(Parser.ScriptImport).init(allocator);
    var vm = try VM.init(&gc, &import_registry);
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

            if (flavor != .Check) {
                _ = try vm.interpret(
                    function,
                    args,
                );
            }

            running_time = timer.read();
        } else {
            return CompileError.Recoverable;
        }

        if (Config.debug_perf) {
            const parsing_ms: f64 = @intToFloat(f64, parsing_time) / 1000000;
            const codegen_ms: f64 = @intToFloat(f64, codegen_time) / 1000000;
            const running_ms: f64 = @intToFloat(f64, running_time) / 1000000;
            std.debug.print(
                "\u{001b}[2mParsing: {d} ms | Codegen: {d} ms | Run: {d} ms | Total: {d} ms | Full GC: {} | GC: {} | Max allocated: {} bytes\u{001b}[0m\n",
                .{
                    parsing_ms,
                    codegen_ms,
                    running_ms,
                    parsing_ms + codegen_ms + running_ms,
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
    var allocator: Allocator = if (builtin.mode == .Debug)
        gpa.allocator()
    else
        std.heap.c_allocator;

    const params = comptime clap.parseParamsComptime(
        \\-h, --help    Show help and exit
        \\-t, --test    Run test blocks in provided script
        \\-c, --check   Check script for error without running it
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

    if (res.args.help) {
        try print_usage(&params);

        std.os.exit(0);
    }

    if (res.args.version) {
        std.debug.print(
            "👨‍🚀 buzz {s} Copyright (C) 2021-2022 Benoit Giannangeli\nBuilt with Zig {}\n",
            .{
                Config.version,
                builtin.zig_version,
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

    if (res.positionals.len <= 0) {
        try print_usage(&params);
        std.os.exit(1);
    }

    const flavor: RunFlavor = if (res.args.check) RunFlavor.Check else if (res.args.@"test") RunFlavor.Test else RunFlavor.Run;

    runFile(allocator, res.positionals[0], positionals.items[1..], flavor) catch {
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
