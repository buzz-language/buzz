const std = @import("std");
const Allocator = std.mem.Allocator;
const VM = @import("./vm.zig").VM;
const Compiler = @import("./compiler.zig").Compiler;

pub fn main() !void {
    const allocator: *Allocator = std.heap.c_allocator;

    var file = std.fs.cwd().openFile("samples/first.buzz", .{}) catch {
        std.debug.warn("File not found", .{});

        return;
    };
    defer file.close();

    const source = try allocator.alloc(u8, (try file.stat()).size);
    defer allocator.free(source);

    _ = try file.readAll(source);

    var vm = VM.init(allocator);
    defer vm.deinit();

    var compiler = Compiler.init(&vm);
    // defer compiler.deinit();

    // TODO: print value
    if (try compiler.compile(source)) |function| {
        _ = try vm.interpret(function);
    } else {
        // TODO: Print compile error
    }
}