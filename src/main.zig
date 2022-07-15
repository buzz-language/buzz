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
const FunctionNode = @import("./node.zig").FunctionNode;
const Config = @import("./config.zig").Config;

fn runFile(allocator: Allocator, file_name: []const u8, args: ?[][:0]u8, testing: bool) !void {
    var strings = std.StringHashMap(*ObjString).init(allocator);
    var imports = std.StringHashMap(Parser.ScriptImport).init(allocator);
    var type_defs = std.StringHashMap(*ObjTypeDef).init(allocator);
    var vm = try VM.init(allocator, &strings);
    var parser = Parser.init(allocator, &strings, &imports, &type_defs, false);
    var codegen = CodeGen.init(allocator, &parser, &strings, &type_defs, testing);
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
        // TODO: free type_defs and its keys which are on the heap
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
                    (parsing_time + codegen_time + running_time) / 1000000,
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

    var args: [][:0]u8 = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // TODO: use https://github.com/Hejsil/zig-clap
    var testing: bool = false;
    for (args) |arg, index| {
        if (index > 0) {
            if (index == 1 and std.mem.eql(u8, arg, "test")) {
                testing = true;
            } else {
                runFile(allocator, arg, args[index..], testing) catch {
                    // TODO: should probably choses appropriate error code
                    std.os.exit(1);
                };

                std.os.exit(0);
            }
        }
    }
}

test "Testing buzz" {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .safety = true,
    }){};
    var allocator: Allocator = if (builtin.mode == .Debug)
        gpa.allocator()
    else
        std.heap.c_allocator;

    var test_dir = try std.fs.cwd().openDir("tests", .{ .iterate = true });
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
