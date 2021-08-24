const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;

pub fn main() !void {
    var file = std.fs.cwd().openFile("design/example.buzz", .{}) catch {
        std.debug.warn("File not found", .{});

        return;
    };
    defer file.close();

    const source = try std.heap.c_allocator.alloc(u8, (try file.stat()).size);
    defer std.heap.c_allocator.free(source);

    _ = try file.readAll(source);

    var scanner = Scanner.init(std.heap.c_allocator, source);
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
}