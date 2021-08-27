const std = @import("std");
const Allocator = std.mem.Allocator;
const VM = @import("./vm.zig").VM;
const Compiler = @import("./compiler.zig").Compiler;

pub fn main() !void {
    const allocator: *Allocator = std.heap.c_allocator;

    var file = std.fs.cwd().openFile("design/example.buzz", .{}) catch {
        std.debug.warn("File not found", .{});

        return;
    };
    defer file.close();

    const source = try allocator.alloc(u8, (try file.stat()).size);
    defer allocator.free(source);

    _ = try file.readAll(source);

    var scanner = Scanner.init(allocator, source);
    defer scanner.deinit();

    try scanner.scan();

    var line: usize = 0;
    for (scanner.tokens.items) |token| {
        if (token.line > line) {
            std.debug.print("\n{}\t", .{ line });
        }

        line = token.line;

        std.debug.print("<{s}> ", .{
            token.lexeme,
        });
    }

    var vm = VM.init(allocator);
    defer vm.deinit();

    var compiler = Compiler.init(allocator, vm);
    defer compiler.deinit();

    // TODO: print value
    if (try compiler.compile(source)) |function| {
        _ = try vm.interpret(function);
    } else {
        // TODO: Print compile error
    }
}