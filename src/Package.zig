const std = @import("std");
const Runner = @import("Runner.zig");
const o = @import("obj.zig");
const builtin = @import("builtin");
const bzio = @import("io.zig");
const v = @import("value.zig");

/// Standard package manifest filename.
pub const MANIFEST = "manifest.buzz";
pub const VENDORS = "vendors";
const manifest_wrapper_prefix = "import \"buzz:manifest\" as _;final manifest: Manifest = ";
const input_whitespace = " \n\r\t";

fn exists(io: std.Io, path: []const u8) bool {
    std.Io.Dir.cwd().access(io, path, .{ .read = true }) catch {
        return false;
    };

    return true;
}

/// Must match src/lib/manifest.buzz
pub const Manifest = struct {
    name: []const u8,
    version: std.SemanticVersion,
    source: Source,
    dependencies: std.StringHashMapUnmanaged(Source) = .empty,
    dev_dependencies: std.StringHashMapUnmanaged(Source) = .empty,
    build: std.StringHashMapUnmanaged([]const []const u8) = .empty,
    root_dir: []const u8 = "src",
    description: ?[]const u8 = null,
    authors: [][]const u8 = &.{},
    tags: [][]const u8 = &.{},
    license: ?[]const u8 = null,
    homepage: ?[]const u8 = null,

    // pub const Dependency = struct {
    //     source: Source,
    //     name: []const u8,
    // };

    // FIXME: we will actually fill this function result to `fetch`
    // Flatten out the dependency graph and errors out if multiple reference to the same dependency are not compatible
    // pub fn resolveDependencies(self: Manifest) error{OutOfMemory}![]const Dependency {}

    pub fn fetch(self: Manifest, process: std.process.Init, root_dir: []const u8) !bool {
        var stdout = bzio.stdoutWriter(process.io);

        const count = self.dependencies.count() + self.dev_dependencies.count();
        if (count == 0) {
            stdout.interface.writeAll("👨‍🚀 Nothing to do\n\n") catch {};
        }

        stdout.interface.writeAll("👨‍🚀 Fetching dependencies...\n\n") catch {};

        var arena = std.heap.ArenaAllocator.init(process.gpa);
        defer arena.deinit();
        const allocator = arena.allocator();

        var progress = InitProgress{
            .out = &stdout.interface,
            .total = count,
        };

        var failed = std.ArrayList(struct { package: []const u8, err: []const u8 }).empty;
        defer failed.deinit(allocator);

        var deps = self.dependencies.iterator();
        while (deps.next()) |entry| {
            entry.value_ptr.fetch(
                process.io,
                allocator,
                root_dir,
                entry.key_ptr.*,
            ) catch |err| {
                try failed.append(
                    allocator,
                    .{
                        .package = entry.key_ptr.*,
                        .err = @errorName(err),
                    },
                );
            };

            progress.advance(entry.key_ptr.*) catch {};
        }

        deps = self.dev_dependencies.iterator();
        while (deps.next()) |entry| {
            entry.value_ptr.fetch(
                process.io,
                allocator,
                root_dir,
                entry.key_ptr.*,
            ) catch |err| {
                try failed.append(
                    allocator,
                    .{
                        .package = entry.key_ptr.*,
                        .err = @errorName(err),
                    },
                );
            };

            progress.advance(entry.key_ptr.*) catch {};
        }

        progress.finish() catch {};

        if (failed.items.len > 0) {
            stdout.interface.print(
                "\n⛔ {} packages could not be fetched.\n\n",
                .{
                    failed.items.len,
                },
            ) catch {};

            for (failed.items) |failure| {
                stdout.interface.print(
                    "\t\x1b[31m{s}: {s}\x1b[0m\n",
                    .{
                        failure.package,
                        failure.err,
                    },
                ) catch {};
            }

            stdout.interface.writeByte('\n') catch {};
        } else {
            stdout.interface.print(
                "\n🎉 {} packages fetched.\n\n",
                .{
                    count,
                },
            ) catch {};
        }

        return failed.items.len == 0;
    }

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
            .description = instance.get(?[]const u8, "description"),
            .license = instance.get(?[]const u8, "license"),
            .homepage = instance.get(?[]const u8, "homepage"),
            .authors = try author_list.toOwnedSlice(allocator),
            .tags = try tag_list.toOwnedSlice(allocator),
            .version = .{
                .major = @intCast(version.get(v.Integer, comptime "0")),
                .minor = @intCast(version.get(v.Integer, comptime "1")),
                .patch = @intCast(version.get(v.Integer, comptime "2")),
            },
            .root_dir = instance.get([]const u8, "rootDir"),
            .source = .fromValue(instance.getFieldValue("source")),
            .dependencies = dep_list,
            .dev_dependencies = dev_dep_list,
            .build = build_map,
        };
    }

    pub const Source = struct {
        url: []const u8,
        ref: ?[]const u8,
        hash: ?[]const u8 = null,
        version: ?std.SemanticVersion = null,
        constraint: Constraint = .equalTo,

        pub fn fetch(self: Source, io: std.Io, allocator: std.mem.Allocator, root_dir: []const u8, name: []const u8) !void {
            var final_url = std.Io.Writer.Allocating.init(allocator);
            defer final_url.deinit();

            if (self.ref) |ref|
                final_url.writer.print("{s}#{s}", .{ self.url, ref }) catch return error.OutOfMemory
            else
                final_url.writer.print("{s}", .{self.url}) catch return error.OutOfMemory;

            var destination = std.Io.Writer.Allocating.init(allocator);
            defer destination.deinit();

            // <root_dir>/vendors/<package name>
            destination.writer.print(
                "{s}{c}{s}{c}{s}",
                .{
                    root_dir,
                    std.Io.Dir.path.sep,
                    VENDORS,
                    std.Io.Dir.path.sep,
                    name,
                },
            ) catch return error.OutOfMemory;

            if (exists(io, destination.written())) {
                return;
            }

            try fetchUrl(
                io,
                allocator,
                final_url.written(),
                destination.written(),
            );
        }

        fn isGitUrl(url: []const u8) bool {
            const query_index = std.mem.indexOfScalar(u8, url, '?') orelse url.len;
            const fragment_index = std.mem.indexOfScalar(u8, url, '#') orelse url.len;
            const source_path = url[0..@min(query_index, fragment_index)];

            return std.ascii.startsWithIgnoreCase(url, "git:") or
                std.ascii.startsWithIgnoreCase(url, "git@") or
                std.ascii.startsWithIgnoreCase(url, "ssh://") or
                std.ascii.startsWithIgnoreCase(url, "git+http://") or
                std.ascii.startsWithIgnoreCase(url, "git+https://") or
                std.ascii.endsWithIgnoreCase(source_path, ".git");
        }

        fn isArchive(url: []const u8) bool {
            const query_index = std.mem.indexOfScalar(u8, url, '?') orelse url.len;
            const fragment_index = std.mem.indexOfScalar(u8, url, '#') orelse url.len;
            const source_path = url[0..@min(query_index, fragment_index)];

            return std.ascii.endsWithIgnoreCase(source_path, ".tar.gz") or
                std.ascii.endsWithIgnoreCase(source_path, ".tgz");
        }

        fn isHttp(url: []const u8) bool {
            return std.ascii.startsWithIgnoreCase(url, "http://") or
                std.ascii.startsWithIgnoreCase(url, "https://");
        }

        /// Fetches `url_or_path` into `destination`.
        fn fetchUrl(io: std.Io, allocator: std.mem.Allocator, url_or_path: []const u8, destination: []const u8) !void {
            const is_http = isHttp(url_or_path);

            if (isArchive(url_or_path)) {
                return try fetchArchive(
                    io,
                    allocator,
                    url_or_path,
                    destination,
                    is_http,
                );
            }

            if (isGitUrl(url_or_path)) {
                return try fetchGit(
                    io,
                    allocator,
                    url_or_path,
                    destination,
                );
            }

            if (is_http) {
                return error.UnsupportedFetchSource;
            }

            return try fetchDirectory(
                io,
                allocator,
                url_or_path,
                destination,
            );
        }

        /// Clones a Git source into `destination` as a bare repository.
        fn fetchGit(io: std.Io, allocator: std.mem.Allocator, url_or_path: []const u8, destination: []const u8) !void {
            if (std.fs.path.dirname(destination)) |parent| {
                if (parent.len > 0) {
                    try std.Io.Dir.cwd().createDirPath(io, parent);
                }
            }

            const git_url = if (std.ascii.startsWithIgnoreCase(url_or_path, "git+"))
                url_or_path["git+".len..]
            else
                url_or_path;

            const result = try std.process.run(
                allocator,
                io,
                .{
                    .argv = &.{ "git", "clone", "--depth", "1", "--single-branch", git_url, destination },
                    .stdout_limit = .limited(1024 * 1024),
                    .stderr_limit = .limited(1024 * 1024),
                },
            );
            defer allocator.free(result.stdout);
            defer allocator.free(result.stderr);

            switch (result.term) {
                .exited => |code| if (code != 0) {
                    return error.GitCloneFailed;
                },
                else => return error.GitCloneFailed,
            }
        }

        /// Downloads a remote archive when needed and extracts it into `destination`.
        fn fetchArchive(io: std.Io, allocator: std.mem.Allocator, url_or_path: []const u8, destination: []const u8, is_http: bool) !void {
            const archive_path = if (is_http) archive: {
                if (std.fs.path.dirname(destination)) |parent| {
                    if (parent.len > 0) {
                        try std.Io.Dir.cwd().createDirPath(io, parent);
                    }
                }

                var random_bytes: [8]u8 = undefined;
                io.random(&random_bytes);
                const temporary_path = try std.fmt.allocPrint(
                    allocator,
                    "{s}.download-{x}.tar.gz",
                    .{
                        destination,
                        std.mem.readInt(u64, &random_bytes, .little),
                    },
                );
                errdefer allocator.free(temporary_path);
                errdefer std.Io.Dir.cwd().deleteFile(io, temporary_path) catch {};

                var file = try std.Io.Dir.cwd().createFile(io, temporary_path, .{ .exclusive = true });
                defer file.close(io);

                var file_buffer: [16 * 1024]u8 = undefined;
                var file_writer = file.writer(io, &file_buffer);

                var client: std.http.Client = .{
                    .allocator = allocator,
                    .io = io,
                };
                defer client.deinit();

                const result = try client.fetch(.{
                    .location = .{ .url = url_or_path },
                    .response_writer = &file_writer.interface,
                });
                try file_writer.interface.flush();
                if (result.status != .ok) {
                    return error.HttpError;
                }

                break :archive temporary_path;
            } else url_or_path;
            defer if (is_http) {
                std.Io.Dir.cwd().deleteFile(io, archive_path) catch {};
                allocator.free(archive_path);
            };

            try std.Io.Dir.cwd().createDirPath(io, destination);

            var destination_dir = try std.Io.Dir.cwd().openDir(io, destination, .{});
            defer destination_dir.close(io);

            var archive_file = try std.Io.Dir.cwd().openFile(io, archive_path, .{ .mode = .read_only });
            defer archive_file.close(io);

            var archive_buffer: [16 * 1024]u8 = undefined;
            var archive_reader = archive_file.reader(io, &archive_buffer);

            const inflate_buffer = try allocator.alloc(u8, std.compress.flate.max_window_len);
            defer allocator.free(inflate_buffer);

            var gzip = std.compress.flate.Decompress.init(
                &archive_reader.interface,
                .gzip,
                inflate_buffer,
            );

            try std.tar.extract(io, destination_dir, &gzip.reader, .{
                .strip_components = 1,
            });
        }

        /// Copies a local directory into `destination`.
        fn fetchDirectory(io: std.Io, allocator: std.mem.Allocator, url_or_path: []const u8, destination: []const u8) !void {
            try std.Io.Dir.cwd().createDirPath(io, destination);

            var source_dir = std.Io.Dir.cwd().openDir(io, url_or_path, .{ .iterate = true }) catch |err| switch (err) {
                error.FileNotFound, error.NotDir => return error.UnsupportedFetchSource,
                else => |e| return e,
            };
            defer source_dir.close(io);

            var destination_dir = try std.Io.Dir.cwd().openDir(io, destination, .{ .iterate = true });
            defer destination_dir.close(io);

            var walker = try source_dir.walk(allocator);
            defer walker.deinit();
            while (try walker.next(io)) |entry| {
                switch (entry.kind) {
                    .directory => try destination_dir.createDirPath(io, entry.path),
                    .file => try entry.dir.copyFile(
                        entry.basename,
                        destination_dir,
                        entry.path,
                        io,
                        .{
                            .make_path = true,
                            .replace = false,
                        },
                    ),
                    .sym_link => {
                        var target_buffer: [std.Io.Dir.max_path_bytes]u8 = undefined;
                        const target_len = try entry.dir.readLink(io, entry.basename, &target_buffer);
                        const target = target_buffer[0..target_len];
                        const stat = entry.dir.statFile(io, entry.basename, .{}) catch null;

                        try destination_dir.symLink(io, target, entry.path, .{
                            .is_directory = if (stat) |s| s.kind == .directory else false,
                        });
                    },
                    else => return error.UnsupportedDirectoryEntry,
                }
            }
        }

        const GitTag = struct {
            tag: []const u8,
            commit: []const u8,

            pub fn deinit(self: *GitTag, allocator: std.mem.Allocator) void {
                allocator.free(self.tag);
                allocator.free(self.commit);
            }
        };

        /// List tags of a git repo
        fn fetchGitTags(io: std.Io, allocator: std.mem.Allocator, url: []const u8) ![]GitTag {
            const git_url = if (std.ascii.startsWithIgnoreCase(url, "git+"))
                url["git+".len..]
            else
                url;

            const result = try std.process.run(
                allocator,
                io,
                .{
                    .argv = &.{ "git", "ls-remote", "--tags", "--refs", git_url },
                    .stdout_limit = .limited(1024 * 1024),
                    .stderr_limit = .limited(1024 * 1024),
                },
            );
            defer allocator.free(result.stdout);
            defer allocator.free(result.stderr);

            switch (result.term) {
                .exited => |code| if (code != 0) {
                    return error.GitLsRemoteFailed;
                },
                else => return error.GitLsRemoteFailed,
            }

            var tags = std.ArrayList(GitTag).empty;
            var lines = std.mem.tokenizeScalar(u8, result.stdout, '\n');
            while (lines.next()) |line| {
                if (line.len == 0) continue;

                var columns = std.mem.splitAny(u8, line, "refs/tags/");

                try tags.append(
                    allocator,
                    .{
                        .commit = try allocator.dupe(
                            u8,
                            std.mem.trim(u8, columns.next().?, " \t"),
                        ),
                        .tag = try allocator.dupe(
                            u8,
                            std.mem.trim(u8, columns.next().?, " \t"),
                        ),
                    },
                );
            }

            return try tags.toOwnedSlice(allocator);
        }

        pub fn fromValue(value: v.Value) Source {
            const instance = value.obj().cast(o.ObjObjectInstance, .ObjectInstance).?;
            const version_value = instance.getFieldValue("version");
            const version = if (version_value.isNull())
                null
            else
                version_value.obj().cast(o.ObjObjectInstance, .ObjectInstance).?;

            return .{
                .url = instance.get([]const u8, "url"),
                .ref = instance.get(?[]const u8, "ref"),
                .hash = instance.get(?[]const u8, "hash"),
                .version = if (version) |version_instance| .{
                    .major = @intCast(version_instance.get(v.Integer, comptime "1")),
                    .minor = @intCast(version_instance.get(v.Integer, comptime "2")),
                    .patch = @intCast(version_instance.get(v.Integer, comptime "3")),
                } else null,
                .constraint = if (version) |version_instance|
                    @enumFromInt(@as(u8, @intCast(version_instance.get(v.Integer, "0"))))
                else
                    .equalTo,
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

/// Number of scaffold files generated by `buzz init`.
const init_progress_file_count = 5;

/// Spinner frames used while rewriting the `buzz init` progress line.
const init_progress_frames = [_][]const u8{ "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" };

/// Tracks and prints `buzz init` file creation progress.
const InitProgress = struct {
    /// Terminal writer used for progress output.
    out: *std.Io.Writer,
    /// Number of scaffold files already written.
    completed: usize = 0,
    /// Number of scaffold files expected.
    total: usize = init_progress_file_count,
    /// Spinner frame to show for the next progress update.
    frame: usize = 0,

    /// Redraws the single-line progress display for a completed scaffold file.
    fn advance(self: *InitProgress, path: []const u8) !void {
        self.completed += 1;

        try self.out.writeAll("\r\x1b[2K");
        try self.out.print("  {s} {d}/{d} ", .{
            init_progress_frames[self.frame % init_progress_frames.len],
            self.completed,
            self.total,
        });
        self.frame += 1;

        try bzio.printProgressBar(
            self.out,
            @intCast(self.completed),
            @intCast(self.total),
            24,
            "\x1b[36m",
        );
        try self.out.print(" {s}", .{path});
        try self.out.flush();
    }

    /// Leaves a final completed progress line before subsequent output.
    fn finish(self: *InitProgress) !void {
        try self.out.writeAll("\r\x1b[2K");
        try self.out.print("  ✓ {d}/{d} ", .{
            self.completed,
            self.total,
        });
        try bzio.printProgressBar(
            self.out,
            @intCast(self.completed),
            @intCast(self.total),
            24,
            "\x1b[36m",
        );
        try self.out.writeAll(" Done\n");
        try self.out.flush();
    }
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

pub fn loadManifest(process: std.process.Init, allocator: std.mem.Allocator, manifest_path: []const u8) !Manifest {
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
        allocator, // We want the parsed value to outlive this function
        .Repl,
        null,
        null,
    );

    if (try runner.runManifest(manifest_source, "manifest")) |manifest| {
        return try .fromValue(manifest, allocator);
    }

    return error.ManifestNotProduced;
}

/// Ensures `vendors/<package_name>` points back to the package root.
pub fn ensureSelfVendorSymlink(
    process: std.process.Init,
    package_root: []const u8,
    package_name: []const u8,
) !void {
    var root_dir = if (std.fs.path.isAbsolute(package_root))
        try std.Io.Dir.openDirAbsolute(process.io, package_root, .{})
    else
        try std.Io.Dir.cwd().openDir(process.io, package_root, .{});
    defer root_dir.close(process.io);

    root_dir.createDir(process.io, VENDORS, .default_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var vendors_dir = try root_dir.openDir(process.io, VENDORS, .{});
    defer vendors_dir.close(process.io);

    vendors_dir.symLink(
        process.io,
        "..",
        package_name,
        .{ .is_directory = true },
    ) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };
}

/// Writes the complete minimal package scaffold under `package_root` and reports file progress when provided.
fn writePackageFiles(
    process: std.process.Init,
    allocator: std.mem.Allocator,
    package_root: []const u8,
    manifest: Manifest,
    progress: ?*InitProgress,
) !void {
    try ensureSelfVendorSymlink(process, package_root, manifest.name);

    // The manifest is intentionally a bare object: loadManifest wraps it with
    // the import and typed assignment needed to validate it as Buzz code.
    var root_dir = if (std.fs.path.isAbsolute(package_root))
        try std.Io.Dir.openDirAbsolute(process.io, package_root, .{})
    else
        try std.Io.Dir.cwd().openDir(process.io, package_root, .{});
    defer root_dir.close(process.io);

    var optional_manifest_fields = std.ArrayList(u8).empty;

    if (manifest.description) |description| {
        try appendOptionalStringField(
            allocator,
            &optional_manifest_fields,
            "description",
            description,
        );
    }

    if (manifest.authors.len > 0) {
        try optional_manifest_fields.appendSlice(allocator, "authors = [ ");
        for (manifest.authors, 0..) |author, index| {
            if (index > 0) {
                try optional_manifest_fields.appendSlice(allocator, ", ");
            }

            try appendBuzzStringLiteral(allocator, &optional_manifest_fields, author);
        }
        try optional_manifest_fields.appendSlice(allocator, " ],\n");
    }

    if (manifest.license) |license| {
        try appendOptionalStringField(
            allocator,
            &optional_manifest_fields,
            "license",
            license,
        );
    }

    if (manifest.tags.len > 0) {
        try optional_manifest_fields.appendSlice(allocator, "tags = [ ");
        for (manifest.tags, 0..) |tag, index| {
            if (index > 0) {
                try optional_manifest_fields.appendSlice(allocator, ", ");
            }

            try appendBuzzStringLiteral(allocator, &optional_manifest_fields, tag);
        }
        try optional_manifest_fields.appendSlice(allocator, " ],\n");
    }

    if (manifest.homepage) |homepage| {
        try appendOptionalStringField(
            allocator,
            &optional_manifest_fields,
            "homepage",
            homepage,
        );
    }

    if (!std.mem.eql(u8, manifest.root_dir, "src")) {
        try optional_manifest_fields.appendSlice(allocator, "rootDir = ");
        try appendBuzzStringLiteral(allocator, &optional_manifest_fields, manifest.root_dir);
        try optional_manifest_fields.appendSlice(allocator, ",\n");
    }

    var manifest_source = std.Io.Writer.Allocating.init(allocator);
    defer manifest_source.deinit();
    try manifest_source.writer.print(
        \\.{{
        \\    name = "{s}",
        \\    version = .{{ {}, {}, {} }},
        \\    source = .{{ url = "{s}" }},
        \\    build = {{
        \\        "{s}": [ "zig build" ],
        \\    }},
        \\
    ,
        .{
            manifest.name,
            manifest.version.major,
            manifest.version.minor,
            manifest.version.patch,
            manifest.source.url,
            manifest.name,
        },
    );

    var optional_it = std.mem.splitScalar(u8, optional_manifest_fields.items, '\n');
    while (optional_it.next()) |field| {
        if (field.len == 0) {
            continue;
        }

        try manifest_source.writer.print("    {s}\n", .{field});
    }

    try manifest_source.writer.writeAll("}\n");
    try root_dir.writeFile(process.io, .{
        .sub_path = MANIFEST,
        .data = manifest_source.written(),
    });
    if (progress) |p| {
        try p.advance(MANIFEST);
    }

    var build_zig = std.Io.Writer.Allocating.init(allocator);
    defer build_zig.deinit();
    try build_zig.writer.print(
        \\const std = @import("std");
        \\
        \\/// Builds the generated Buzz native example library.
        \\pub fn build(b: *std.Build) void {{
        \\    const target = b.standardTargetOptions(.{{}});
        \\    const optimize = b.standardOptimizeOption(.{{}});
        \\
        \\    const lib = b.addLibrary(.{{
        \\        .name = "{s}",
        \\        .linkage = .dynamic,
        \\        .use_llvm = true,
        \\        .root_module = b.createModule(.{{
        \\            .root_source_file = b.path("{s}/{s}.zig"),
        \\            .target = target,
        \\            .optimize = optimize,
        \\            .sanitize_c = .off,
        \\        }}),
        \\    }});
        \\
        \\    const copy_lib = b.addUpdateSourceFiles();
        \\    copy_lib.addCopyFileToSource(lib.getEmittedBin(), lib.out_filename);
        \\    b.getInstallStep().dependOn(&copy_lib.step);
        \\}}
        \\
    ,
        .{
            manifest.name,
            manifest.root_dir,
            manifest.name,
        },
    );
    try root_dir.writeFile(process.io, .{
        .sub_path = "build.zig",
        .data = build_zig.written(),
    });
    if (progress) |p| {
        try p.advance("build.zig");
    }

    try root_dir.createDir(process.io, manifest.root_dir, .default_dir);
    var src_dir = try root_dir.openDir(process.io, manifest.root_dir, .{});
    defer src_dir.close(process.io);

    const lib_buzz_name = try std.fmt.allocPrint(
        allocator,
        "{s}.buzz",
        .{manifest.name},
    );
    const lib_zig_name = try std.fmt.allocPrint(
        allocator,
        "{s}.zig",
        .{manifest.name},
    );
    const lib_buzz_path = try std.fmt.allocPrint(
        allocator,
        "{s}/{s}",
        .{ manifest.root_dir, lib_buzz_name },
    );
    const lib_zig_path = try std.fmt.allocPrint(
        allocator,
        "{s}/{s}",
        .{ manifest.root_dir, lib_zig_name },
    );

    var lib_buzz = std.Io.Writer.Allocating.init(allocator);
    defer lib_buzz.deinit();
    try lib_buzz.writer.print(
        \\namespace {s};
        \\
        \\import "buzz:std";
        \\
        \\/// A simple exported function
        \\export fun helloFromBuzz(name: str) => std\print("Hello from Buzz {{name}}");
        \\
        \\/// Calls into the generated Zig native library.
        \\///
        \\/// The Zig side ignores Buzz's native context in this minimal example.
        \\/// Use buzz_api.zig when a real native function needs arguments,
        \\/// return values, or VM access.
        \\export extern fun helloFromZig() > void;
        \\
    ,
        .{manifest.name},
    );
    try src_dir.writeFile(process.io, .{
        .sub_path = lib_buzz_name,
        .data = lib_buzz.written(),
    });
    if (progress) |p| {
        try p.advance(lib_buzz_path);
    }

    var lib_zig = std.Io.Writer.Allocating.init(allocator);
    defer lib_zig.deinit();
    try lib_zig.writer.print(
        \\//! Minimal native library generated by `buzz init`.
        \\
        \\const std = @import("std");
        \\
        \\/// Opaque Buzz native call context for this no-argument example.
        \\///
        \\/// This template ignores the context, so it does not need Buzz's full native API.
        \\/// Real extern libraries should import `buzz_api.zig` to read arguments, push
        \\/// return values, or interact with the VM.
        \\const NativeCtx = opaque {{}};
        \\
        \\/// Native function signature expected by Buzz.
        \\const Native = fn (*NativeCtx) callconv(.c) c_int;
        \\
        \\/// Pointer to a Buzz native function.
        \\const NativeFn = *const Native;
        \\
        \\/// Minimal native callback invoked from Buzz.
        \\fn helloFromZig(_: *NativeCtx) callconv(.c) c_int {{
        \\    std.debug.print("Hello from Zig\n", .{{}});
        \\
        \\    return 0;
        \\}}
        \\
        \\/// Resolves native symbols requested by the generated Buzz library.
        \\pub export fn @"{s}"(symbol: [*:0]const u8) callconv(.c) ?NativeFn {{
        \\    if (std.mem.eql(u8, std.mem.span(symbol), "helloFromZig")) {{
        \\        return &helloFromZig;
        \\    }}
        \\
        \\    return null;
        \\}}
        \\
    ,
        .{manifest.name},
    );
    try src_dir.writeFile(process.io, .{
        .sub_path = lib_zig_name,
        .data = lib_zig.written(),
    });
    if (progress) |p| {
        try p.advance(lib_zig_path);
    }

    var main_buzz = std.Io.Writer.Allocating.init(allocator);
    defer main_buzz.deinit();
    try main_buzz.writer.print(
        \\import "pkg:{s}/{s}.buzz";
        \\
        \\/// Runs the generated native library example.
        \\fun main(args: [str]) > int {{
        \\    {s}\helloFromBuzz(args[?0] ?? "Mr Nobody");
        \\
        \\    {s}\helloFromZig();
        \\
        \\    return 0;
        \\}}
        \\
    ,
        .{
            manifest.name,
            manifest.name,
            manifest.name,
            manifest.name,
        },
    );
    try src_dir.writeFile(process.io, .{
        .sub_path = "main.buzz",
        .data = main_buzz.written(),
    });
    if (progress) |p| {
        try p.advance(try std.fmt.allocPrint(
            allocator,
            "{s}/main.buzz",
            .{manifest.root_dir},
        ));
    }
}

/// Init a new buzz package: writes a `manifest.buzz` and a minimal set of example files
pub fn init(process: std.process.Init) !void {
    if (exists(process.io, "./" ++ MANIFEST)) {
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

    var stdout = bzio.stdoutWriter(process.io);
    try stdout.interface.writeAll(
        "👨‍🚀 This command will guide you through creating a buzz package.\n\n",
    );

    const package_name = try ask(
        process,
        allocator,
        "📦 package name: ",
        cwd,
        true,
    );

    const version = try ask(
        process,
        allocator,
        "🏷️ version: ",
        "1.0.0",
        true,
    );

    const description = try ask(
        process,
        allocator,
        "📝 description: ",
        null,
        false,
    );

    const git_repo = try ask(
        process,
        allocator,
        "🔗 git repository: ",
        null,
        true,
    );

    const tags = try ask(
        process,
        allocator,
        "🏷️ tags (comma separated): ",
        null,
        false,
    );

    // Git config is only a convenience default; missing config should not block init.
    const git_user_name: ?[]const u8 = if (std.process.run(
        allocator,
        process.io,
        .{
            .argv = &.{ "git", "config", "--get", "user.name" },
            .stdout_limit = .limited(4 * 1024),
            .stderr_limit = .limited(4 * 1024),
        },
    )) |result| name: {
        switch (result.term) {
            .exited => |code| if (code != 0) {
                break :name null;
            },
            else => break :name null,
        }

        const name = std.mem.trim(u8, result.stdout, input_whitespace);
        if (name.len == 0 or std.mem.findAny(u8, name, "\\\"\n\r\t") != null) {
            break :name null;
        }

        break :name name;
    } else |_| null;

    const git_user_email: ?[]const u8 = if (std.process.run(
        allocator,
        process.io,
        .{
            .argv = &.{ "git", "config", "--get", "user.email" },
            .stdout_limit = .limited(4 * 1024),
            .stderr_limit = .limited(4 * 1024),
        },
    )) |result| email: {
        switch (result.term) {
            .exited => |code| if (code != 0) {
                break :email null;
            },
            else => break :email null,
        }

        const email = std.mem.trim(u8, result.stdout, input_whitespace);
        if (email.len == 0 or std.mem.findAny(u8, email, "\\\"\n\r\t") != null) {
            break :email null;
        }

        break :email email;
    } else |_| null;

    const author_default = if (git_user_name != null and git_user_email != null)
        try std.fmt.allocPrint(
            allocator,
            "{s} <{s}>",
            .{
                git_user_name.?,
                git_user_email.?,
            },
        )
    else
        null;

    const author = try ask(
        process,
        allocator,
        "👤 author: ",
        author_default,
        false,
    );

    const license = try ask(
        process,
        allocator,
        "⚖️ license: ",
        "MIT",
        false,
    );

    const version_value = try std.SemanticVersion.parse(version);
    if (version_value.pre != null or version_value.build != null) {
        return error.InvalidVersion;
    }

    var authors = std.ArrayList([]const u8).empty;
    if (author.len > 0) {
        try authors.append(allocator, author);
    }

    var tag_list = std.ArrayList([]const u8).empty;
    var tag_it = std.mem.splitScalar(u8, tags, ',');
    while (tag_it.next()) |tag| {
        const trimmed_tag = std.mem.trim(u8, tag, input_whitespace);
        if (trimmed_tag.len == 0) {
            continue;
        }

        try tag_list.append(allocator, trimmed_tag);
    }

    const manifest: Manifest = .{
        .name = package_name,
        .version = version_value,
        .source = .{
            .url = git_repo,
            .ref = null,
        },
        .description = if (description.len > 0) description else null,
        .authors = try authors.toOwnedSlice(allocator),
        .tags = try tag_list.toOwnedSlice(allocator),
        .license = if (license.len > 0) license else null,
    };

    try stdout.interface.writeAll("\n📥 Preparing package files\n");
    var progress = InitProgress{ .out = &stdout.interface };

    try writePackageFiles(
        process,
        allocator,
        ".",
        manifest,
        &progress,
    );
    try progress.finish();

    try stdout.interface.print(
        \\
        \\🎉 Buzz package created. Run `buzz run` to try it.
        \\.
        \\├── build.zig
        \\├── {s}
        \\├── src
        \\│   ├── main.buzz
        \\│   ├── {s}.buzz
        \\│   └── {s}.zig
        \\└── {s}
        \\    └── {s} -> .
        \\
    ,
        .{
            MANIFEST,
            package_name,
            package_name,
            VENDORS,
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

        const answer = try stdin_reader.readUntilDelimiterOrEof('\n');

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

test "init writes a buildable package with a native extern example" {
    if (builtin.os.tag == .windows) {
        return error.SkipZigTest;
    }

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var environ_map = std.process.Environ.Map.init(allocator);
    try environ_map.put("BUZZ_PATH", "zig-out");

    const process = std.process.Init{
        .minimal = .{
            .environ = .empty,
            .args = .{ .vector = &.{} },
        },
        .arena = &arena,
        .gpa = allocator,
        .io = std.testing.io,
        .environ_map = &environ_map,
        .preopens = .empty,
    };

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    var package_root_buffer: [std.Io.Dir.max_path_bytes]u8 = undefined;
    const package_root_len = try tmp.dir.realPath(std.testing.io, &package_root_buffer);
    const package_root = try allocator.dupe(u8, package_root_buffer[0..package_root_len]);

    try writePackageFiles(
        process,
        allocator,
        package_root,
        .{
            .name = "sample",
            .version = .{
                .major = 1,
                .minor = 2,
                .patch = 3,
            },
            .source = .{
                .url = "git:https://example.com/sample.git",
                .tag = null,
            },
            .description = "sample package",
            .authors = try allocator.dupe([]const u8, &[_][]const u8{"Buzz"}),
            .tags = try allocator.dupe([]const u8, &[_][]const u8{ "sample", "native" }),
            .license = "MIT",
        },
        null,
    );

    for ([_][]const u8{
        MANIFEST,
        "build.zig",
        "src/sample.buzz",
        "src/sample.zig",
        "src/main.buzz",
    }) |path| {
        try tmp.dir.access(std.testing.io, path, .{ .read = true });
    }

    const manifest_path = try std.fs.path.join(
        allocator,
        &.{ package_root, MANIFEST },
    );
    const manifest = try loadManifest(process, allocator, manifest_path);
    try std.testing.expectEqualStrings("sample", manifest.name);

    const build_cmd = manifest.build.get("sample") orelse return error.MissingGeneratedBuildCommand;
    try std.testing.expectEqual(@as(usize, 2), build_cmd.len);
    try std.testing.expectEqualStrings("zig", build_cmd[0]);
    try std.testing.expectEqualStrings("build", build_cmd[1]);

    const build_result = try std.process.run(
        std.testing.allocator,
        std.testing.io,
        .{
            .argv = &.{ "zig", "build" },
            .cwd = .{ .path = package_root },
            .stdout_limit = .limited(128 * 1024),
            .stderr_limit = .limited(128 * 1024),
        },
    );
    defer std.testing.allocator.free(build_result.stdout);
    defer std.testing.allocator.free(build_result.stderr);

    switch (build_result.term) {
        .exited => |code| if (code != 0) {
            std.debug.print(
                "generated package build failed with code {}\nstdout:\n{s}\nstderr:\n{s}\n",
                .{
                    code,
                    build_result.stdout,
                    build_result.stderr,
                },
            );
            return error.GeneratedPackageBuildFailed;
        },
        else => {
            std.debug.print(
                "generated package build terminated unexpectedly: {any}\nstdout:\n{s}\nstderr:\n{s}\n",
                .{
                    build_result.term,
                    build_result.stdout,
                    build_result.stderr,
                },
            );
            return error.GeneratedPackageBuildFailed;
        },
    }

    const library_extension = switch (builtin.os.tag) {
        .windows => "dll",
        .macos, .ios, .tvos, .watchos, .visionos => "dylib",
        else => "so",
    };
    const library_file_name = try std.fmt.allocPrint(
        allocator,
        "{s}sample.{s}",
        .{
            if (builtin.os.tag == .windows) "" else "lib",
            library_extension,
        },
    );
    try tmp.dir.access(std.testing.io, library_file_name, .{ .read = true });

    const main_path = try std.fs.path.join(
        allocator,
        &.{ package_root, "src", "main.buzz" },
    );
    var runner: Runner = undefined;
    try runner.init(process, allocator, .Run, null, null);
    defer runner.deinit();

    try std.testing.expectEqual(
        @as(u8, 0),
        try runner.runFile(package_root, main_path, &.{}),
    );
}
