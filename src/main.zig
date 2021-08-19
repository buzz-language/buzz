const std = @import("std");
const Parser = @import("./parser.zig").Parser;

pub fn main() !void {
    var file = std.fs.cwd().openFile("design/example.buzz", .{}) catch {
        std.debug.warn("File not found", .{});

        return;
    };
    defer file.close();

    const source = try std.heap.c_allocator.alloc(u8, (try file.stat()).size);
    defer std.heap.c_allocator.free(source);

    _ = try file.readAll(source);

    var parser = Parser.init(std.heap.c_allocator, source);
    defer parser.deinit();

    try parser.parse();
}