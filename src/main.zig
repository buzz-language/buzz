const std = @import("std");
const Allocator = std.mem.Allocator;
const VM = @import("./vm.zig").VM;
const Compiler = @import("./compiler.zig").Compiler;

// Using a global because of vm.stack which would overflow zig's stack

fn repl() !void {
    var vm = try VM.init(std.heap.c_allocator);
    defer vm.deinit();
    var compiler = Compiler.init(&vm);

    std.debug.print("👨‍🚀 buzz 0.0.1 (C) 2021 Benoit Giannangeli\n", .{});
    while (true) {
        std.debug.print("→ ", .{});

        var line = [_]u8{0} ** 1024;
        _ = try std.io.getStdIn().read(line[0..]);

        if (line.len > 0) {
            if (try compiler.compile(line[0..], "<repl>")) |function| {
                _ = try vm.interpret(function);
            } else {
                // TODO: Print compile error
                std.debug.warn("Compile error\n", .{});
            }
        }
    }
}

fn runFile(file_name: []const u8) !void {
    var vm = try VM.init(std.heap.c_allocator);
    defer vm.deinit();
    var compiler = Compiler.init(&vm);

    const allocator: *Allocator = std.heap.c_allocator;
    
    var file = std.fs.cwd().openFile(file_name, .{}) catch {
        std.debug.warn("File not found", .{});
        return;
    };
    defer file.close();
    
    const source = try allocator.alloc(u8, (try file.stat()).size);
    defer allocator.free(source);
    
    _ = try file.readAll(source);

    // TODO: print value
    if (try compiler.compile(source, file_name)) |function| {
        _ = try vm.interpret(function);
    } else {
        // TODO: Print compile error
    }
}

pub fn main() !void {
    var arg_it = try std.process.argsAlloc(std.heap.c_allocator);
    defer std.process.argsFree(std.heap.c_allocator, arg_it);

    for (arg_it) |arg, index| {
        if (index > 0) {
            try runFile(arg);

            return;
        }
    }

    try repl();
}