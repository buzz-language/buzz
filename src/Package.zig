const std = @import("std");
const Runner = @import("Runner.zig");
const o = @import("obj.zig");
const builtin = @import("builtin");
const is_windows = builtin.os.tag != .windows;
const ln = if (is_windows) @import("linenoise.zig") else void;
const bzio = @import("io.zig");

const MANIFEST = "manifest.buzz";
const input_whitespace = " \n\r\t";

pub fn wrapManifest(allocator: std.mem.Allocator, raw_source: []const u8) ![]const u8 {
    const source = std.mem.trim(u8, raw_source, " \n\r\t");

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

    return try manifest_source.toOwnedSlice(allocator);
}

pub fn loadManifest(process: std.process.Init, gpa: std.mem.Allocator) !void {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    var allocator = arena.allocator();

    var file = try std.Io.Dir.cwd().openFile(
        process.io,
        MANIFEST,
        .{
            .mode = .read_only,
        },
    );
    defer file.close(process.io);

    const raw_source = try allocator.alloc(u8, (try file.stat(process.io)).size);
    _ = try file.readPositionalAll(process.io, raw_source, 0);
    const manifest_source = try wrapManifest(allocator, raw_source);

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

/// Init a new buzz package: writes a `manifest.buzz` and a minimal set of example files
pub fn init(process: std.process.Init) !void {
    var already_there = true;
    std.Io.Dir.cwd().access(process.io, "./" ++ MANIFEST, .{ .read = true }) catch {
        already_there = false;
    };

    if (already_there) {
        return error.ManifestAlreadyCreated;
    }

    const full_cwd = try std.Io.Dir.cwd().realPathFileAlloc(
        process.io,
        ".",
        process.gpa,
    );
    defer process.gpa.free(full_cwd);
    const cwd = std.Io.Dir.path.basename(full_cwd[0..]);

    var arena = std.heap.ArenaAllocator.init(process.gpa);
    defer arena.deinit();
    const allocator = arena.allocator();

    const package_name = try ask(
        process,
        allocator,
        "package name: ",
        cwd,
        true,
    );

    const version = try ask(
        process,
        allocator,
        "version: ",
        "1.0.0",
        true,
    );

    const description = try ask(
        process,
        allocator,
        "description: ",
        null,
        false,
    );

    const root_dir = try ask(
        process,
        allocator,
        "root directory: ",
        "src",
        true,
    );

    const git_repo = try ask(
        process,
        allocator,
        "git repository: ",
        null,
        true,
    );

    const tags = try ask(
        process,
        allocator,
        "tags (comma separated): ",
        null,
        false,
    );

    const author = try ask(
        process,
        allocator,
        "author: ",
        null,
        false,
    );

    const license = try ask(
        process,
        allocator,
        "license: ",
        "MIT",
        false,
    );

    // The manifest is intentionally a bare object: loadManifest wraps it with
    // the import and typed assignment needed to validate it as Buzz code.
    var version_parts = [_]u32{ 0, 0, 0 };
    var version_it = std.mem.splitScalar(u8, version, '.');
    var version_index: usize = 0;
    while (version_it.next()) |part| {
        if (version_index == version_parts.len) {
            return error.InvalidVersion;
        }

        const trimmed_part = std.mem.trim(u8, part, input_whitespace);
        if (trimmed_part.len == 0) {
            return error.InvalidVersion;
        }

        version_parts[version_index] = try std.fmt.parseInt(u32, trimmed_part, 10);
        version_index += 1;
    }
    if (version_index != version_parts.len) {
        return error.InvalidVersion;
    }

    var package_name_literal = std.ArrayList(u8).empty;
    try appendBuzzStringLiteral(allocator, &package_name_literal, package_name);

    var root_dir_literal = std.ArrayList(u8).empty;
    try appendBuzzStringLiteral(allocator, &root_dir_literal, root_dir);

    var git_repo_literal = std.ArrayList(u8).empty;
    try appendBuzzStringLiteral(allocator, &git_repo_literal, git_repo);

    var description_field = std.ArrayList(u8).empty;
    try appendOptionalStringField(allocator, &description_field, "description", description);

    var authors_field = std.ArrayList(u8).empty;
    const trimmed_author = std.mem.trim(u8, author, input_whitespace);
    if (trimmed_author.len > 0) {
        try authors_field.appendSlice(allocator, "authors = [ ");
        try appendBuzzStringLiteral(allocator, &authors_field, trimmed_author);
        try authors_field.appendSlice(allocator, " ],\n");
    }

    var license_field = std.ArrayList(u8).empty;
    try appendOptionalStringField(allocator, &license_field, "license", license);

    var tag_items = std.ArrayList(u8).empty;
    var tag_it = std.mem.splitScalar(u8, tags, ',');
    while (tag_it.next()) |tag| {
        const trimmed_tag = std.mem.trim(u8, tag, input_whitespace);
        if (trimmed_tag.len == 0) {
            continue;
        }

        if (tag_items.items.len > 0) {
            try tag_items.appendSlice(allocator, ", ");
        }
        try appendBuzzStringLiteral(allocator, &tag_items, trimmed_tag);
    }

    var tags_field = std.ArrayList(u8).empty;
    if (tag_items.items.len > 0) {
        try tags_field.appendSlice(allocator, "tags = [ ");
        try tags_field.appendSlice(allocator, tag_items.items);
        try tags_field.appendSlice(allocator, " ],\n");
    }

    var file = try std.Io.Dir.cwd().createFile(process.io, MANIFEST, .{});
    defer file.close(process.io);

    var writer = file.writer(process.io, &.{});

    try writer.interface.print(
        \\.{{
        \\    name = {s},
        \\    version = .{{ {}, {}, {} }},
        \\{s}{s}{s}{s}{s}{s}{s}{s}    source = .{{
        \\        url = {s},
        \\    }},
        \\    rootDir = {s},
        \\}}
    ,
        .{
            package_name_literal.items,
            version_parts[0],
            version_parts[1],
            version_parts[2],
            if (description_field.items.len > 0) "    " else "",
            description_field.items,
            if (authors_field.items.len > 0) "    " else "",
            authors_field.items,
            if (license_field.items.len > 0) "    " else "",
            license_field.items,
            if (tags_field.items.len > 0) "    " else "",
            tags_field.items,
            git_repo_literal.items,
            root_dir_literal.items,
        },
    );

    var stdout = bzio.stdoutWriter(process.io);

    stdout.interface.print("Buzz package `manifest.buzz` created\n", .{}) catch {};
}

/// Appends a complete Buzz string literal when the value can be written unescaped.
fn appendBuzzStringLiteral(
    allocator: std.mem.Allocator,
    buffer: *std.ArrayList(u8),
    value: []const u8,
) !void {
    const trimmed_value = std.mem.trim(u8, value, input_whitespace);

    try buffer.append(allocator, '"');
    try buffer.appendSlice(allocator, trimmed_value);
    try buffer.append(allocator, '"');
}

/// Appends an optional one-line string field without leading indentation.
fn appendOptionalStringField(
    allocator: std.mem.Allocator,
    buffer: *std.ArrayList(u8),
    comptime name: []const u8,
    value: []const u8,
) !void {
    try buffer.appendSlice(allocator, name ++ " = ");
    try appendBuzzStringLiteral(allocator, buffer, value);
    try buffer.appendSlice(allocator, ",\n");
}

/// Prompts until the response is valid, and until a value is present for required prompts.
fn ask(
    process: std.process.Init,
    allocator: std.mem.Allocator,
    comptime question: []const u8,
    default: ?[]const u8,
    required: bool,
) ![]const u8 {
    var stdin_buffer: [1024]u8 = undefined;
    var stdin = bzio.stdinReader(process.io, stdin_buffer[0..]);
    var stdin_reader = bzio.AllocatedReader.init(
        allocator,
        &stdin.interface,
        null,
    );
    var stdout = bzio.stdoutWriter(process.io);

    while (true) {
        try stdout.interface.writeAll(question);
        if (default) |d| {
            try stdout.interface.print("({s}) ", .{d});
        }

        const answer = if (!is_windows) answer: {
            const line = ln.linenoise(question) orelse break :answer null;
            defer ln.linenoiseFree(@ptrCast(@constCast(line)));

            break :answer try allocator.dupe(u8, std.mem.span(line));
        } else try stdin_reader.readUntilDelimiterOrEof('\n');

        const value = if (answer) |a| value: {
            const trimmed_answer = std.mem.trim(u8, a, input_whitespace);
            if (trimmed_answer.len > 0) {
                break :value trimmed_answer;
            }

            break :value default orelse "";
        } else default orelse "";

        if (value.len == 0 and required) {
            try stdout.interface.writeAll("A value is required.\n");

            continue;
        }

        if (std.mem.findAny(u8, value, "\\\"\n\r\t") != null) {
            try stdout.interface.writeAll("Input cannot contain quotes, backslashes, tabs, or newlines.\n");

            continue;
        }

        return value;
    }
}
