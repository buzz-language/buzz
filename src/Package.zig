const std = @import("std");
const Runner = @import("Runner.zig");
const o = @import("obj.zig");
const builtin = @import("builtin");
const is_windows = builtin.os.tag != .windows;
const ln = if (is_windows) @import("linenoise.zig") else void;
const bzio = @import("io.zig");
const v = @import("value.zig");

/// Standard package manifest filename.
pub const MANIFEST = "manifest.buzz";
const manifest_wrapper_prefix = "import \"manifest\" as _;final manifest: Manifest = ";
const input_whitespace = " \n\r\t";

/// Must match src/lib/manifest.buzz
pub const Manifest = struct {
    name: []const u8,
    version: std.SemanticVersion,
    source: Source,
    dependencies: std.StringHashMapUnmanaged(Source) = .empty,
    dev_dependencies: std.StringHashMapUnmanaged(Source) = .empty,
    build: std.StringHashMapUnmanaged([]const []const u8) = .empty,
    description: ?[]const u8 = null,
    authors: [][]const u8 = &.{},
    tags: [][]const u8 = &.{},
    license: ?[]const u8 = null,
    homepage: ?[]const u8 = null,

    pub fn fromValue(value: v.Value, allocator: std.mem.Allocator) !Manifest {
        const instance = value.obj().cast(o.ObjObjectInstance, .ObjectInstance).?;
        const version = instance.getFieldValue("version").obj().cast(o.ObjObjectInstance, .ObjectInstance).?;

        const dependencies = instance.getFieldValue("dependencies").obj().cast(o.ObjMap, .Map).?;
        var dep_list = std.StringHashMapUnmanaged(Source).empty;
        var it = dependencies.map.iterator();
        while (it.next()) |entry| {
            try dep_list.put(
                allocator,
                entry.key_ptr.*.obj().cast(o.ObjString, .String).?.string,
                .fromValue(entry.value_ptr.*),
            );
        }

        const dev_dependencies = instance.getFieldValue("devDependencies").obj().cast(o.ObjMap, .Map).?;
        var dev_dep_list = std.StringHashMapUnmanaged(Source).empty;
        it = dev_dependencies.map.iterator();
        while (it.next()) |entry| {
            try dev_dep_list.put(
                allocator,
                entry.key_ptr.*.obj().cast(o.ObjString, .String).?.string,
                .fromValue(entry.value_ptr.*),
            );
        }

        var build = instance.getFieldValue("build").obj().cast(o.ObjMap, .Map).?;
        var build_map = std.StringHashMapUnmanaged([]const []const u8).empty;
        it = build.map.iterator();
        while (it.next()) |entry| {
            const cmds = entry.value_ptr.*.obj().cast(o.ObjList, .List).?;

            var cmd_list = std.ArrayList([]const u8).empty;
            for (cmds.items.items) |item| {
                try cmd_list.append(
                    allocator,
                    item.obj().cast(o.ObjString, .String).?.string,
                );
            }

            try build_map.put(
                allocator,
                entry.key_ptr.*.obj().cast(o.ObjString, .String).?.string,
                try cmd_list.toOwnedSlice(allocator),
            );
        }

        const authors = instance.getFieldValue("authors").obj().cast(o.ObjList, .List).?;
        var author_list = std.ArrayList([]const u8).empty;
        for (authors.items.items) |author| {
            try author_list.append(
                allocator,
                author.obj().cast(o.ObjString, .String).?.string,
            );
        }

        const tags = instance.getFieldValue("tags").obj().cast(o.ObjList, .List).?;
        var tag_list = std.ArrayList([]const u8).empty;
        for (tags.items.items) |tag| {
            try tag_list.append(
                allocator,
                tag.obj().cast(o.ObjString, .String).?.string,
            );
        }

        return .{
            .name = instance.get([]const u8, "name"),
            .description = instance.get([]const u8, "description"),
            .license = instance.get([]const u8, "license"),
            .homepage = instance.get([]const u8, "homepage"),
            .authors = try author_list.toOwnedSlice(allocator),
            .tags = try tag_list.toOwnedSlice(allocator),
            .version = .{
                .major = @intCast(version.get(v.Integer, comptime "0")),
                .minor = @intCast(version.get(v.Integer, comptime "1")),
                .patch = @intCast(version.get(v.Integer, comptime "2")),
            },
            .source = .fromValue(instance.getFieldValue("source")),
            .dependencies = dep_list,
            .dev_dependencies = dev_dep_list,
            .build = build_map,
        };
    }

    pub const Source = struct {
        url: []const u8,
        tag: ?[]const u8,
        hash: ?[]const u8 = null,
        version: std.SemanticVersion,
        constraint: Constraint = .equalTo,

        pub fn fromValue(value: v.Value) Source {
            const instance = value.obj().cast(o.ObjObjectInstance, .ObjectInstance).?;
            const version = instance.getFieldValue("version").obj().cast(o.ObjObjectInstance, .ObjectInstance).?;

            return .{
                .url = instance.get([]const u8, "url"),
                .tag = instance.get(?[]const u8, "tag"),
                .hash = instance.get(?[]const u8, "hash"),
                .version = .{
                    .major = @intCast(version.get(v.Integer, comptime "1")),
                    .minor = @intCast(version.get(v.Integer, comptime "2")),
                    .patch = @intCast(version.get(v.Integer, comptime "3")),
                },
                .constraint = @enumFromInt(@as(u8, @intCast(version.get(v.Integer, "0")))),
            };
        }

        pub const Constraint = enum(u8) {
            lessThan,
            equalOrLessThan,
            equalTo,
            greaterThan,
            equalOrGreater,
            majorLessThan,
            majorEqualOrLessThan,
            majorEqualTo,
            majorGreaterThan,
            majorEqualOrGreater,
            minorLessThan,
            minorEqualOrLessThan,
            minorEqualTo,
            minorGreaterThan,
            minorEqualOrGreater,
        };
    };
};

pub fn wrapManifest(allocator: std.mem.Allocator, raw_source: []const u8) ![]const u8 {
    const source = std.mem.trim(u8, raw_source, " \n\r\t");

    var manifest_source = std.ArrayList(u8).empty;
    // Wrap into a script that will leave the manifest as the last global of the VM
    // We type the variable so that if user gives anything else, we get an error
    try manifest_source.appendSlice(allocator, manifest_wrapper_prefix);
    try manifest_source.appendSlice(allocator, source);

    if (!std.mem.endsWith(u8, source, ";")) {
        try manifest_source.append(allocator, ';');
    }

    return try manifest_source.toOwnedSlice(allocator);
}

pub fn loadManifest(process: std.process.Init, gpa: std.mem.Allocator, manifest_path: []const u8) !Manifest {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    var allocator = arena.allocator();

    var file = try std.Io.Dir.cwd().openFile(
        process.io,
        manifest_path,
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

    if (try runner.runManifest(manifest_source, "manifest")) |manifest| {
        return try .fromValue(manifest, allocator);
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

    var file = try std.Io.Dir.cwd().createFile(process.io, MANIFEST, .{});
    defer file.close(process.io);

    var writer = file.writer(process.io, &.{});

    try writer.interface.print(
        \\.{{
        \\    name = "{s}",
        \\    version = .{{ {}, {}, {} }},
        \\    source = .{{ url = "{s}" }},
        \\
    ,
        .{
            package_name,
            version_parts[0],
            version_parts[1],
            version_parts[2],
            git_repo,
        },
    );

    if (description.len > 0) {
        try writer.interface.print(
            \\    description = "{s}"
            \\
        ,
            .{
                description,
            },
        );
    }

    if (author.len > 0) {
        try writer.interface.print(
            \\    authors = [ "{s}" ],
            \\
        ,
            .{
                author,
            },
        );
    }

    if (license.len > 0) {
        try writer.interface.print(
            \\    licenses = "{s}",
            \\
        ,
            .{
                license,
            },
        );
    }

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
        try tag_items.append(allocator, '"');
        try tag_items.appendSlice(allocator, trimmed_tag);
        try tag_items.append(allocator, '"');
    }

    if (tag_items.items.len > 0) {
        try writer.interface.print(
            \\tags = [ {s} ]
            \\
        ,
            .{
                tag_items.items,
            },
        );
    }

    try writer.interface.print("}}\n", .{});

    var stdout = bzio.stdoutWriter(process.io);

    stdout.interface.print("Buzz package `manifest.buzz` created\n", .{}) catch {};

    // Now we create basic files to get the developer started

    var lib_name = std.ArrayList(u8).empty;
    try lib_name.appendSlice(allocator, package_name);
    try lib_name.appendSlice(allocator, ".buzz");

    // Create root dir
    try std.Io.Dir.cwd().createDir(process.io, "src", .default_dir);
    const src_dir = try std.Io.Dir.cwd().openDir(process.io, "src", .{});

    // Write <name>.buzz, a simple library
    var lib_buzz = try src_dir.createFile(process.io, lib_name.items, .{});
    defer lib_buzz.close(process.io);

    writer = lib_buzz.writer(process.io, &.{});

    try writer.interface.print(
        \\namespace {s};
        \\
        \\import "buzz:std";
        \\
        \\export fun helloWorld(name: str) => std\print("Hello {{name}}");
        \\
    ,
        .{
            package_name,
        },
    );

    // Write main.buzz that uses this library
    var main_buzz = try src_dir.createFile(process.io, "main.buzz", .{});
    defer main_buzz.close(process.io);

    writer = main_buzz.writer(process.io, &.{});

    try writer.interface.print(
        \\import "pkg:{s}/{s}"
        \\
        \\fun main(args: [str]) > int {{
        \\    {s}\helloWorld(args[?0] ?? "nobody");
        \\    return 0;
        \\}}
    ,
        .{
            package_name,
            lib_name.items,
            package_name,
        },
    );
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
