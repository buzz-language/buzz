const std = @import("std");
const Runner = @import("Runner.zig");
const o = @import("obj.zig");

const MANIFEST = "manifest.buzz";

pub fn loadManifest(process: std.process.Init, gpa: std.mem.Allocator) !void {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    var allocator = arena.allocator();

    var file = try std.Io.Dir.cwd().openFile(process.io, MANIFEST, .{});
    defer file.close(process.io);

    const raw_source = try allocator.alloc(u8, (try file.stat(process.io)).size);
    const source = std.mem.trim(u8, raw_source, " \n\r\t");
    _ = try file.readPositionalAll(process.io, source, 0);

    const manifest_source = std.ArrayList(u8).manifest_source;
    // Wrap into a script that will leave the manifest as the last global of the VM
    // We type the variable so that if user gives anything else, we get an error
    try manifest_source.appendSlice(
        allocator,
        \\import "manifest" as _;
        \\
        \\final manifest: Manifest = 
        ,
    );
    try manifest_source.appendSlice(allocator, source);

    if (!std.mem.endsWith(u8, source, ";")) {
        try manifest_source.append(allocator, ';');
    }

    var runner: Runner = undefined;
    try runner.init(
        process,
        gpa, // We want the parsed value to outlive this function
        .Repl,
        null,
        null,
    );

    if (try runner.runSource(manifest_source.items, "manifest")) |manifest| {
        // Buzz manifest to zig representation
        if (manifest.obj().cast(o.ObjObjectInstance, .ObjectInstance)) |instance| {
            const name = instance.get([]const u8, "name");

            std.debug.print("Package name is {s}\n", .{name});
        }
    }

    return error.ManifestNotProduced;
}
