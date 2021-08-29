const std = @import("std");
const Allocator = std.mem.Allocator;
const VM = @import("./vm.zig").VM;
const Compiler = @import("./compiler.zig").Compiler;

// Using a global because of vm.stack which would overflow zig's stack
var vm = VM.init(std.heap.c_allocator);

pub fn main() !void {
    defer vm.deinit();

    const allocator: *Allocator = std.heap.c_allocator;

    const file_name = "samples/first.buzz";
    var file = std.fs.cwd().openFile(file_name, .{}) catch {
        std.debug.warn("File not found", .{});

        return;
    };
    defer file.close();

    const source = try allocator.alloc(u8, (try file.stat()).size);
    defer allocator.free(source);

    _ = try file.readAll(source);

    var compiler = Compiler.init(&vm);
    // defer compiler.deinit();

    // TODO: print value
    if (try compiler.compile(source, file_name)) |function| {
        _ = try vm.interpret(function);
    } else {
        // TODO: Print compile error
    }
}