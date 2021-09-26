// zig fmt: off
const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const VM = @import("vm.zig").VM;
const Compiler = @import("compiler.zig").Compiler;
const ObjString = @import("./obj.zig").ObjString;

pub const Result = VM.InterpretResult;

pub fn runString(allocator: *Allocator, string: []const u8) !Result {
    var strings = std.StringHashMap(*ObjString).init(allocator);
    var vm = try VM.init(allocator, &strings, null);
    var compiler = Compiler.init(allocator, &strings, false);
    defer {
        vm.deinit();
        compiler.deinit();
        strings.deinit();
    }

    if (try compiler.compile(string[0..], "<test>", true)) |function| {
        return (try vm.interpret(function)) orelse Result.RuntimeError;
    } else {
        return Result.CompileError;
    }
}

fn runFile(allocator: *Allocator, file_name: []const u8) !Result {
    var strings = std.StringHashMap(*ObjString).init(allocator);
    var vm = try VM.init(allocator, &strings, null);
    var compiler = Compiler.init(allocator, &strings, false);
    defer {
        vm.deinit();
        compiler.deinit();
        strings.deinit();
    }

    var file = std.fs.cwd().openFile(file_name, .{}) catch {
        std.debug.warn("File not found", .{});
        return Result.RuntimeError;
    };
    defer file.close();

    const source = try allocator.alloc(u8, (try file.stat()).size);
    defer allocator.free(source);

    _ = try file.readAll(source);

    if (try compiler.compile(source, file_name, true)) |function| {
        return (try vm.interpret(function)) orelse Result.RuntimeError;
    } else {
        return Result.CompileError;
    }
}

test "Testing buzz" {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .safety = true,
    }){};
    var allocator: *Allocator = if (builtin.mode == .Debug)
        &gpa.allocator
    else
        std.heap.c_allocator;

    var test_dir = try std.fs.cwd().openDir("tests", .{ .iterate = true });
    var it = test_dir.iterate();

    while (try it.next()) |file| {
        if (file.kind == .File) {
            var file_name: []u8 = try allocator.alloc(u8, 6 + file.name.len);
            defer allocator.free(file_name);

            if (runFile(allocator, try std.fmt.bufPrint(file_name, "tests/{s}", .{file.name})) catch Result.RuntimeError == Result.Ok) {
                std.debug.warn("\u{001b}[32m[{s}... ✔️]\u{001b}[0m\n", .{file.name});
            } else {
                std.debug.warn("\u{001b}[31m[{s}... ✕]\u{001b}[0m\n", .{file.name});
            }
        }
    }
}
