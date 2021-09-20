const std = @import("std");
const assert = std.debug.assert;
const VM = @import("vm.zig").VM;
const Compiler = @import("compiler.zig").Compiler;

pub const Result = VM.InterpretResult;

pub fn runString(string: []const u8) !Result {
    var vm = try VM.init(std.heap.c_allocator);
    defer vm.deinit();
    var compiler = Compiler.init(&vm);
    defer compiler.deinit();

    if (try compiler.compile(string[0..], "<test>", true)) |function| {
        return (try vm.interpret(function)) orelse Result.RuntimeError;
    } else {
        return Result.CompileError;
    }
}

fn runFile(file_name: []const u8) !Result {
    var vm = try VM.init(std.heap.c_allocator);
    defer vm.deinit();
    var compiler = Compiler.init(&vm);
    defer compiler.deinit();

    const allocator: *std.mem.Allocator = std.heap.c_allocator;
    
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
    var test_dir = try std.fs.cwd().openDir("tests", .{ .iterate = true });
    var it = test_dir.iterate();

    while (try it.next()) |file| {
        if (file.kind == .File) {
            var file_name: []u8 = try std.heap.c_allocator.alloc(u8, 6 + file.name.len);
            defer std.heap.c_allocator.free(file_name);

            assert(runFile(try std.fmt.bufPrint(file_name, "tests/{s}", .{ file.name })) catch Result.RuntimeError == Result.Ok);
        }
    }
}