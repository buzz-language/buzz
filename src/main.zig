const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const VM = @import("./vm.zig").VM;
const _parser = @import("./parser.zig");
const Parser = _parser.Parser;
const CompileError = _parser.CompileError;
const CodeGen = @import("./codegen.zig").CodeGen;
const _obj = @import("./obj.zig");
const ObjString = _obj.ObjString;
const ObjTypeDef = _obj.ObjTypeDef;
const TypeRegistry = _obj.TypeRegistry;
const FunctionNode = @import("./node.zig").FunctionNode;
var Config = @import("./config.zig").Config;
const clap = @import("ext/clap/clap.zig");

fn toNullTerminated(allocator: std.mem.Allocator, string: []const u8) ![:0]u8 {
    return allocator.dupeZ(u8, string);
}

fn runFile(allocator: Allocator, file_name: []const u8, args: ?[][:0]u8, testing: bool) !void {
    var strings = std.StringHashMap(*ObjString).init(allocator);
    var imports = std.StringHashMap(Parser.ScriptImport).init(allocator);
    var type_registry = TypeRegistry{
        .allocator = allocator,
        .registry = std.StringHashMap(*ObjTypeDef).init(allocator),
    };
    var vm = try VM.init(allocator, &strings);
    var parser = Parser.init(allocator, &strings, &imports, &type_registry, false);
    var codegen = CodeGen.init(allocator, &parser, &strings, &type_registry, testing);
    defer {
        codegen.deinit();
        vm.deinit();
        parser.deinit();
        strings.deinit();
        var it = imports.iterator();
        while (it.next()) |kv| {
            kv.value_ptr.*.globals.deinit();
        }
        imports.deinit();
        // TODO: free type_registry and its keys which are on the heap
    }

    var file = std.fs.cwd().openFile(file_name, .{}) catch {
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

            _ = try vm.interpret(
                function,
                args,
            );

            running_time = timer.read();
        }

        if (Config.debug_perf) {
            std.debug.print(
                "\u{001b}[2mParsing: {} ms | Codegen: {} ms | Run: {} ms | Total: {} ms\u{001b}[0m\n",
                .{
                    parsing_time / 1000000,
                    codegen_time / 1000000,
                    running_time / 1000000,
                    parsing_time / 1000000 + codegen_time / 1000000 + running_time / 1000000,
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

    runFile(allocator, res.positionals[0], positionals.items[1..], res.args.@"test") catch {
        // TODO: should probably choses appropriate error code
        std.os.exit(1);
    };

    std.os.exit(0);
}

test "Testing buzz" {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .safety = true,
    }){};
    var allocator: Allocator = if (builtin.mode == .Debug)
        gpa.allocator()
    else
        std.heap.c_allocator;

    var test_dir = try std.fs.cwd().openIterableDir("tests", .{});
    var it = test_dir.iterate();

    var success = true;
    var count: usize = 0;
    var fail_count: usize = 0;
    while (try it.next()) |file| : (count += 1) {
        if (file.kind == .File) {
            var file_name: []u8 = try allocator.alloc(u8, 6 + file.name.len);
            defer allocator.free(file_name);

            var had_error: bool = false;
            runFile(allocator, try std.fmt.bufPrint(file_name, "tests/{s}", .{file.name}), null, true) catch {
                std.debug.print("\u{001b}[31m[{s}... ✕]\u{001b}[0m\n", .{file.name});
                had_error = true;
                success = false;
                fail_count += 1;
            };

            if (!had_error) {
                std.debug.print("\u{001b}[32m[{s}... ✓]\u{001b}[0m\n", .{file.name});
            }
        }
    }

    if (success) {
        std.debug.print("\n\u{001b}[32m", .{});
    } else {
        std.debug.print("\n\u{001b}[31m", .{});
    }

    std.debug.print("Ran {}, Failed: {}\u{001b}[0m\n", .{
        count,
        fail_count,
    });

    std.os.exit(if (success) 0 else 1);
}
