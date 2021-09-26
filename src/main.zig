const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const VM = @import("./vm.zig").VM;
const Compiler = @import("./compiler.zig").Compiler;
const ObjString = @import("./obj.zig").ObjString;

// Using a global because of vm.stack which would overflow zig's stack

fn repl(allocator: *Allocator) !void {
    var strings = std.StringHashMap(*ObjString).init(allocator);
    var imports = std.StringHashMap(Compiler.ScriptImport).init(allocator);
    var vm = try VM.init(allocator, &strings, null);
    var compiler = Compiler.init(allocator, &strings, &imports, false);
    defer {
        vm.deinit();
        compiler.deinit();
        strings.deinit();
        for (imports.items) |import| {
            import.globals.deinit();
        }
        imports.deinit();
    }

    std.debug.print("👨‍🚀 buzz 0.0.1 (C) 2021 Benoit Giannangeli\n", .{});
    while (true) {
        std.debug.print("→ ", .{});

        var line = [_]u8{0} ** 1024;
        _ = try std.io.getStdIn().read(line[0..]);

        if (line.len > 0) {
            if (try compiler.compile(line[0..], "<repl>", false)) |function| {
                _ = try vm.interpret(function);
            }
        }
    }
}

fn runFile(allocator: *Allocator, file_name: []const u8, testing: bool) !void {
    var strings = std.StringHashMap(*ObjString).init(allocator);
    var imports = std.StringHashMap(Compiler.ScriptImport).init(allocator);
    var vm = try VM.init(allocator, &strings, null);
    var compiler = Compiler.init(allocator, &strings, &imports, false);
    defer {
        vm.deinit();
        compiler.deinit();
        strings.deinit();
        for (imports.items) |import| {
            import.globals.deinit();
        } 
        imports.deinit();
    }
    
    var file = std.fs.cwd().openFile(file_name, .{}) catch {
        std.debug.warn("File not found", .{});
        return;
    };
    defer file.close();
    
    const source = try allocator.alloc(u8, (try file.stat()).size);
    defer allocator.free(source);
    
    _ = try file.readAll(source);

    if (try compiler.compile(source, file_name, testing)) |function| {
        _ = try vm.interpret(function);
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    var allocator: *Allocator = if (builtin.mode == .Debug)
            &gpa.allocator
        else
            std.heap.c_allocator;

    var arg_it = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, arg_it);

    // TODO: use https://github.com/Hejsil/zig-clap
    var testing: bool = false;
    for (arg_it) |arg, index| {
        if (index > 0) {
            if (index == 1 and std.mem.eql(u8, arg, "test")) {
                testing = true;
            } else {
                try runFile(allocator, arg, testing);

                return;
            }
        }
    }

    try repl(allocator);
}