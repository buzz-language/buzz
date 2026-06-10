const std = @import("std");
const Runner = @import("Runner.zig");
const o = @import("obj.zig");
const builtin = @import("builtin");
const bzio = @import("io.zig");
const v = @import("value.zig");

/// Standard package manifest filename.
pub const MANIFEST = "manifest.buzz";
/// Standard package manifest lock filename.
pub const MANIFEST_LOCK = "manifest.lock.buzz";
pub const VENDORS = "vendors";
const manifest_wrapper_prefix = "import \"buzz:manifest\" as _;final manifest: Manifest = ";
const manifest_lock_wrapper_prefix = "import \"buzz:manifest\" as _;final manifestLock: ManifestLock = ";
const input_whitespace = " \n\r\t";

fn exists(io: std.Io, path: []const u8) bool {
    std.Io.Dir.cwd().access(io, path, .{ .read = true }) catch {
        return false;
    };

    return true;
}

/// Fully resolved package source recorded in `manifest.lock.buzz`.
pub const ResolvedSource = struct {
    /// Original source URL or local path.
    url: []const u8,
    /// Git ref, tag, or commit used to fetch the source.
    ref: ?[]const u8,
    /// SHA-256 content hash of the fetched package tree.
    hash: []const u8,

    /// Parses a `ResolvedSource` buzz object value.
    pub fn fromValue(value: v.Value) ResolvedSource {
        const instance = value.obj().cast(o.ObjObjectInstance, .ObjectInstance).?;

        return .{
            .url = instance.get([]const u8, "url"),
            .ref = instance.get(?[]const u8, "ref"),
            .hash = instance.get([]const u8, "hash"),
        };
    }

    /// Fetches this locked source and verifies its content hash.
    pub fn fetch(
        self: ResolvedSource,
        process: std.process.Init,
        allocator: std.mem.Allocator,
        root_dir: []const u8,
        name: []const u8,
        progress: *InitProgress,
    ) !FetchResult {
        const result = try Manifest.Source.fetchResolved(
            process,
            allocator,
            root_dir,
            name,
            self.url,
            self.ref,
            progress,
        );
        errdefer allocator.free(result.destination);
        defer allocator.free(result.resolved_source.hash);

        if (!std.mem.eql(u8, result.resolved_source.hash, self.hash)) {
            return error.ManifestLockHashMismatch;
        }

        return .{
            .destination = result.destination,
            .downloaded = result.downloaded,
            .resolved_source = self,
        };
    }
};

/// Parsed `manifest.lock.buzz` content.
pub const ManifestLock = struct {
    /// Locked regular dependencies by package name.
    dependencies: std.StringHashMapUnmanaged(ResolvedSource) = .empty,
    /// Locked development dependencies by package name.
    dev_dependencies: std.StringHashMapUnmanaged(ResolvedSource) = .empty,

    /// Parses a `ManifestLock` buzz object value.
    pub fn fromValue(value: v.Value, allocator: std.mem.Allocator) !ManifestLock {
        const instance = value.obj().cast(o.ObjObjectInstance, .ObjectInstance).?;

        const dependencies = instance.getFieldValue("dependencies").obj().cast(o.ObjMap, .Map).?;
        var dep_list = std.StringHashMapUnmanaged(ResolvedSource).empty;
        var it = dependencies.map.iterator();
        while (it.next()) |entry| {
            try dep_list.put(
                allocator,
                entry.key_ptr.*.obj().cast(o.ObjString, .String).?.string,
                .fromValue(entry.value_ptr.*),
            );
        }

        const dev_dependencies = instance.getFieldValue("devDependencies").obj().cast(o.ObjMap, .Map).?;
        var dev_dep_list = std.StringHashMapUnmanaged(ResolvedSource).empty;
        it = dev_dependencies.map.iterator();
        while (it.next()) |entry| {
            try dev_dep_list.put(
                allocator,
                entry.key_ptr.*.obj().cast(o.ObjString, .String).?.string,
                .fromValue(entry.value_ptr.*),
            );
        }

        return .{
            .dependencies = dep_list,
            .dev_dependencies = dev_dep_list,
        };
    }

    /// Serializes the lock file as deterministic bare buzz object source.
    pub fn toSource(self: ManifestLock, allocator: std.mem.Allocator) ![]const u8 {
        var source = std.ArrayList(u8).empty;
        errdefer source.deinit(allocator);

        try source.appendSlice(allocator, ".{\n");
        try appendLockMap(allocator, &source, "dependencies", self.dependencies);
        try appendLockMap(allocator, &source, "devDependencies", self.dev_dependencies);
        try source.appendSlice(allocator, "}\n");

        return try source.toOwnedSlice(allocator);
    }

    /// Writes the lock file at `path`.
    pub fn write(self: ManifestLock, io: std.Io, allocator: std.mem.Allocator, path: []const u8) !void {
        const source = try self.toSource(allocator);
        defer allocator.free(source);

        const parent = std.fs.path.dirname(path) orelse ".";
        const basename = std.fs.path.basename(path);
        var dir = if (std.fs.path.isAbsolute(parent))
            try std.Io.Dir.openDirAbsolute(io, parent, .{})
        else
            try std.Io.Dir.cwd().openDir(io, parent, .{});
        defer dir.close(io);

        try dir.writeFile(io, .{
            .sub_path = basename,
            .data = source,
        });
    }
};

/// Result of fetching a dependency source into the vendors directory.
const FetchResult = struct {
    /// Directory where the package source exists after the fetch attempt.
    destination: []const u8,
    /// Whether this call populated `destination`.
    downloaded: bool,
    /// Source data to persist in the lock file.
    resolved_source: ResolvedSource,
};

/// Path and kind for one deterministic package hash input.
const HashEntry = struct {
    /// Path relative to the hashed package directory.
    path: []const u8,
    /// Filesystem kind to distinguish files from symlinks.
    kind: std.Io.File.Kind,

    /// Sorts hash entries by path for stable hashing.
    fn lessThan(_: void, lhs: HashEntry, rhs: HashEntry) bool {
        return std.mem.lessThan(u8, lhs.path, rhs.path);
    }
};

/// Must match src/lib/manifest.buzz
pub const Manifest = struct {
    name: []const u8,
    version: std.SemanticVersion,
    source: Source,
    dependencies: std.StringHashMapUnmanaged(Source) = .empty,
    dev_dependencies: std.StringHashMapUnmanaged(Source) = .empty,
    build: std.StringArrayHashMapUnmanaged(BuildStep) = .empty,
    root_dir: []const u8 = "src",
    description: ?[]const u8 = null,
    authors: [][]const u8 = &.{},
    tags: [][]const u8 = &.{},
    license: ?[]const u8 = null,
    homepage: ?[]const u8 = null,

    /// One process invocation from a manifest build step.
    pub const BuildCommand = []const []const u8;

    /// Ordered commands attached to one named manifest build step.
    pub const BuildStep = []const BuildCommand;

    pub fn fetch(self: Manifest, process: std.process.Init, root_dir: []const u8) !bool {
        const started_at = std.Io.Clock.Timestamp.now(process.io, .awake);
        var stdout = bzio.stdoutWriter(process.io);

        const count = self.dependencies.count() + self.dev_dependencies.count();
        if (count == 0) {
            stdout.interface.writeAll("👨‍🚀 Nothing to do\n\n") catch {};
        }

        stdout.interface.writeAll("👨‍🚀 Fetching dependencies...\n\n") catch {};

        var arena = std.heap.ArenaAllocator.init(process.gpa);
        defer arena.deinit();
        const allocator = arena.allocator();

        const lock_path = try std.fs.path.join(
            allocator,
            &.{ root_dir, MANIFEST_LOCK },
        );
        var manifest_lock = if (exists(process.io, lock_path))
            try loadManifestLock(process, allocator, lock_path)
        else
            ManifestLock{};
        var lock_dirty = false;

        var progress = InitProgress{
            .out = &stdout.interface,
            .total = count,
        };
        progress.tick("fetching dependencies") catch {};

        var failed = std.ArrayList(struct { package: []const u8, err: []const u8 }).empty;
        defer failed.deinit(allocator);

        var deps = self.dependencies.iterator();
        while (deps.next()) |entry| {
            var errors = std.ArrayList([]const u8).empty;
            defer errors.deinit(allocator);

            fetchDependency(
                entry.value_ptr.*,
                process,
                allocator,
                root_dir,
                entry.key_ptr.*,
                &manifest_lock.dependencies,
                &lock_dirty,
                &progress,
                &errors,
            ) catch |err| {
                try failed.append(
                    allocator,
                    .{
                        .package = entry.key_ptr.*,
                        .err = @errorName(err),
                    },
                );

                for (errors.items) |step_err| {
                    try failed.append(allocator, .{
                        .package = entry.key_ptr.*,
                        .err = step_err,
                    });
                }
            };

            progress.advance(entry.key_ptr.*) catch {};
        }

        deps = self.dev_dependencies.iterator();
        while (deps.next()) |entry| {
            var errors = std.ArrayList([]const u8).empty;
            defer errors.deinit(allocator);

            fetchDependency(
                entry.value_ptr.*,
                process,
                allocator,
                root_dir,
                entry.key_ptr.*,
                &manifest_lock.dev_dependencies,
                &lock_dirty,
                &progress,
                &errors,
            ) catch |err| {
                try failed.append(
                    allocator,
                    .{
                        .package = entry.key_ptr.*,
                        .err = @errorName(err),
                    },
                );

                for (errors.items) |step_err| {
                    try failed.append(allocator, .{
                        .package = entry.key_ptr.*,
                        .err = step_err,
                    });
                }
            };

            progress.advance(entry.key_ptr.*) catch {};
        }

        if (failed.items.len == 0 and lock_dirty) {
            try manifest_lock.write(process.io, allocator, lock_path);
        }

        progress.finish() catch {};

        const elapsed = started_at.untilNow(process.io).raw;
        if (failed.items.len > 0) {
            stdout.interface.print(
                "\n⛔ {} packages could not be fetched in {f}.\n\n",
                .{
                    failed.items.len,
                    elapsed,
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
                "\n🎉 {} packages fetched in {f}.\n\n",
                .{
                    count,
                    elapsed,
                },
            ) catch {};
        }

        return failed.items.len == 0;
    }

    /// Fetches a dependency and runs its manifest build commands when newly downloaded.
    fn fetchDependency(
        source: Source,
        process: std.process.Init,
        allocator: std.mem.Allocator,
        root_dir: []const u8,
        name: []const u8,
        lock_entries: *std.StringHashMapUnmanaged(ResolvedSource),
        lock_dirty: *bool,
        progress: *InitProgress,
        errors: *std.ArrayList([]const u8),
    ) !void {
        const fetched = if (lock_entries.get(name)) |locked| fetched: {
            try source.validateLock(locked);
            break :fetched try locked.fetch(
                process,
                allocator,
                root_dir,
                name,
                progress,
            );
        } else fetched: {
            const result = try source.fetch(
                process,
                allocator,
                root_dir,
                name,
                progress,
            );
            try lock_entries.put(allocator, name, result.resolved_source);
            lock_dirty.* = true;

            break :fetched result;
        };

        if (!fetched.downloaded) {
            return;
        }

        const manifest_path = try std.fs.path.join(
            allocator,
            &.{ fetched.destination, MANIFEST },
        );
        defer allocator.free(manifest_path);

        const dependency_manifest = try loadManifest(
            process,
            allocator,
            manifest_path,
        );

        var build_command_count: usize = 0;
        var steps = dependency_manifest.build.iterator();
        while (steps.next()) |step| {
            build_command_count += step.value_ptr.*.len;
        }
        progress.total += build_command_count;

        steps = dependency_manifest.build.iterator();
        while (steps.next()) |step| {
            for (step.value_ptr.*) |command| {
                if (command.len == 0) {
                    return error.EmptyBuildCommand;
                }

                {
                    const result = try runCommand(
                        process,
                        allocator,
                        command,
                        fetched.destination,
                        progress,
                        step.key_ptr.*,
                    );
                    defer result.deinit(allocator);

                    progress.advance(step.key_ptr.*) catch {};

                    if (result.exit_code == null or result.exit_code.? != 0) {
                        var err = std.Io.Writer.Allocating.init(allocator);

                        err.writer.print(
                            "\x1b[31mFailed build step {s} for package {s}:\n\t{s}\n\x1b[0m",
                            .{
                                step.key_ptr.*,
                                name,
                                result.stderr,
                            },
                        ) catch {};

                        try errors.append(allocator, try err.toOwnedSlice());

                        return error.BuildCommandFailed;
                    }
                }
            }
        }
    }

    /// Captured result of a spawned external command.
    const CommandResult = struct {
        /// Captured standard output owned by the caller.
        stdout: []u8,
        /// Captured standard error owned by the caller.
        stderr: []u8,
        /// Exit code when the command terminated normally.
        exit_code: ?u8,

        /// Releases captured output buffers.
        fn deinit(self: CommandResult, allocator: std.mem.Allocator) void {
            allocator.free(self.stdout);
            allocator.free(self.stderr);
        }
    };

    /// Runs `argv` in `cwd`, captures output, and keeps `progress` animated while the command is alive.
    fn runCommand(
        process: std.process.Init,
        allocator: std.mem.Allocator,
        argv: []const []const u8,
        cwd: []const u8,
        progress: *InitProgress,
        label: []const u8,
    ) !CommandResult {
        const output_limit = 1024 * 1024;
        const tick_timeout: std.Io.Timeout = .{
            .duration = .{
                .clock = .awake,
                .raw = .fromMilliseconds(80),
            },
        };

        var child = try std.process.spawn(process.io, .{
            .argv = argv,
            .cwd = .{ .path = cwd },
            .stdin = .ignore,
            .stdout = .pipe,
            .stderr = .pipe,
        });
        errdefer child.kill(process.io);

        var multi_reader_buffer: std.Io.File.MultiReader.Buffer(2) = undefined;
        var multi_reader: std.Io.File.MultiReader = undefined;
        multi_reader.init(
            allocator,
            process.io,
            multi_reader_buffer.toStreams(),
            &.{ child.stdout.?, child.stderr.? },
        );
        defer multi_reader.deinit();

        const stdout_reader = multi_reader.reader(0);
        const stderr_reader = multi_reader.reader(1);
        while (true) {
            multi_reader.fill(64, tick_timeout) catch |err| switch (err) {
                error.Timeout => {
                    progress.tick(label) catch {};

                    continue;
                },
                error.EndOfStream => break,
                else => |e| return e,
            };

            if (stdout_reader.buffered().len > output_limit or
                stderr_reader.buffered().len > output_limit)
            {
                return error.StreamTooLong;
            }

            progress.tick(label) catch {};
        }

        try multi_reader.checkAnyError();

        const term = try child.wait(process.io);
        const stdout = try multi_reader.toOwnedSlice(0);
        errdefer allocator.free(stdout);
        const stderr = try multi_reader.toOwnedSlice(1);
        errdefer allocator.free(stderr);

        return .{
            .stdout = stdout,
            .stderr = stderr,
            .exit_code = switch (term) {
                .exited => |code| code,
                else => null,
            },
        };
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
        var build_map = std.StringArrayHashMapUnmanaged(BuildStep).empty;
        it = build.map.iterator();
        while (it.next()) |entry| {
            const cmds = entry.value_ptr.*.obj().cast(o.ObjList, .List).?;

            var cmd_list = std.ArrayList(BuildCommand).empty;
            for (cmds.items.items) |cmd| {
                const argv = cmd.obj().cast(o.ObjList, .List).?;
                var argv_list = std.ArrayList([]const u8).empty;
                for (argv.items.items) |item| {
                    try argv_list.append(
                        allocator,
                        item.obj().cast(o.ObjString, .String).?.string,
                    );
                }

                try cmd_list.append(
                    allocator,
                    try argv_list.toOwnedSlice(allocator),
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
        version: ?std.SemanticVersion = null,
        constraint: Constraint = .equalTo,

        /// Fetches this source, resolving version constraints when needed.
        pub fn fetch(
            self: Source,
            process: std.process.Init,
            allocator: std.mem.Allocator,
            root_dir: []const u8,
            name: []const u8,
            progress: *InitProgress,
        ) !FetchResult {
            const resolved_ref = if (self.version != null)
                try self.resolveVersionRef(process.io, allocator)
            else
                self.ref;

            return try fetchResolved(
                process,
                allocator,
                root_dir,
                name,
                self.url,
                resolved_ref,
                progress,
            );
        }

        /// Fetches a URL/ref pair into the package vendors directory.
        fn fetchResolved(
            process: std.process.Init,
            allocator: std.mem.Allocator,
            root_dir: []const u8,
            name: []const u8,
            url: []const u8,
            ref: ?[]const u8,
            progress: *InitProgress,
        ) !FetchResult {
            const io = process.io;
            const destination_path = try destinationPath(allocator, root_dir, name);
            const downloaded = !exists(io, destination_path);

            if (downloaded) {
                try fetchUrl(
                    process,
                    allocator,
                    url,
                    ref,
                    destination_path,
                    progress,
                );
            }

            const hash = try hashPackageTree(io, allocator, destination_path);

            return .{
                .destination = destination_path,
                .downloaded = downloaded,
                .resolved_source = .{
                    .url = url,
                    .ref = ref,
                    .hash = hash,
                },
            };
        }

        /// Returns `<root_dir>/vendors/<package name>`.
        fn destinationPath(allocator: std.mem.Allocator, root_dir: []const u8, name: []const u8) ![]const u8 {
            var destination = std.Io.Writer.Allocating.init(allocator);
            defer destination.deinit();

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

            return try allocator.dupe(u8, destination.written());
        }

        fn isGitUrl(url: []const u8) bool {
            const query_index = std.mem.indexOfScalar(u8, url, '?') orelse url.len;
            const fragment_index = std.mem.indexOfScalar(u8, url, '#') orelse url.len;
            const source_path = url[0..@min(query_index, fragment_index)];

            return std.ascii.startsWithIgnoreCase(url, "git:") or
                std.ascii.startsWithIgnoreCase(url, "git@") or
                std.ascii.startsWithIgnoreCase(url, "git+") or
                std.ascii.startsWithIgnoreCase(url, "ssh://") or
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
        fn fetchUrl(
            process: std.process.Init,
            allocator: std.mem.Allocator,
            url_or_path: []const u8,
            ref: ?[]const u8,
            destination: []const u8,
            progress: *InitProgress,
        ) !void {
            const io = process.io;
            const is_http = isHttp(url_or_path);

            if (isArchive(url_or_path)) {
                if (ref != null) {
                    return error.UnsupportedFetchSource;
                }

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
                    process,
                    allocator,
                    url_or_path,
                    ref,
                    destination,
                    progress,
                );
            }

            if (is_http) {
                return error.UnsupportedFetchSource;
            }

            if (ref != null) {
                return error.UnsupportedFetchSource;
            }

            return try fetchDirectory(
                io,
                allocator,
                url_or_path,
                destination,
            );
        }

        /// Clones a Git source into `destination` and checks out `ref` when provided.
        fn fetchGit(
            process: std.process.Init,
            allocator: std.mem.Allocator,
            url_or_path: []const u8,
            ref: ?[]const u8,
            destination: []const u8,
            progress: *InitProgress,
        ) !void {
            const io = process.io;

            if (std.fs.path.dirname(destination)) |parent| {
                if (parent.len > 0) {
                    try std.Io.Dir.cwd().createDirPath(io, parent);
                }
            }

            const git_url = if (std.ascii.startsWithIgnoreCase(url_or_path, "git+"))
                url_or_path["git+".len..]
            else
                url_or_path;

            if (ref) |git_ref| {
                // Branches and tags can usually be cloned directly at depth 1.
                // This is the fastest path and keeps history out of vendors.
                const clone_result = try Manifest.runCommand(
                    process,
                    allocator,
                    &.{ "git", "clone", "--depth", "1", "--single-branch", "--branch", git_ref, git_url, destination },
                    ".",
                    progress,
                    destination,
                );
                defer clone_result.deinit(allocator);
                const clone_succeeded = clone_result.exit_code != null and clone_result.exit_code.? == 0;

                if (!clone_succeeded) {
                    // `git clone --branch` refuses some valid lock refs, especially raw
                    // commits. Remove any partial clone, then fetch that exact ref into
                    // an empty repository at depth 1 instead of doing a full clone.
                    std.Io.Dir.cwd().deleteTree(io, destination) catch {};
                    try std.Io.Dir.cwd().createDirPath(io, destination);

                    const init_result = try Manifest.runCommand(
                        process,
                        allocator,
                        &.{ "git", "init", destination },
                        ".",
                        progress,
                        destination,
                    );
                    defer init_result.deinit(allocator);

                    if (init_result.exit_code == null or init_result.exit_code.? != 0) {
                        return error.GitCloneFailed;
                    }

                    const remote_result = try Manifest.runCommand(
                        process,
                        allocator,
                        &.{ "git", "-C", destination, "remote", "add", "origin", git_url },
                        ".",
                        progress,
                        destination,
                    );
                    defer remote_result.deinit(allocator);

                    if (remote_result.exit_code == null or remote_result.exit_code.? != 0) {
                        return error.GitFetchFailed;
                    }

                    const fetch_result = try Manifest.runCommand(
                        process,
                        allocator,
                        &.{ "git", "-C", destination, "fetch", "--depth", "1", "origin", git_ref },
                        ".",
                        progress,
                        destination,
                    );
                    defer fetch_result.deinit(allocator);

                    if (fetch_result.exit_code == null or fetch_result.exit_code.? != 0) {
                        return error.GitFetchFailed;
                    }

                    const checkout_result = try Manifest.runCommand(
                        process,
                        allocator,
                        &.{ "git", "-C", destination, "checkout", "--detach", "FETCH_HEAD" },
                        ".",
                        progress,
                        destination,
                    );
                    defer checkout_result.deinit(allocator);

                    if (checkout_result.exit_code == null or checkout_result.exit_code.? != 0) {
                        return error.GitCheckoutFailed;
                    }
                }
            } else {
                // Without an explicit ref, clone the remote HEAD at depth 1
                const result = try Manifest.runCommand(
                    process,
                    allocator,
                    &.{ "git", "clone", "--depth", "1", "--single-branch", git_url, destination },
                    ".",
                    progress,
                    destination,
                );
                defer result.deinit(allocator);

                if (result.exit_code == null or result.exit_code.? != 0) {
                    return error.GitCloneFailed;
                }
            }

            // This is a no-op for repos without submodules, so avoid a separate
            // `.gitmodules` probe and let git populate nested dependencies.
            const submodule_result = try Manifest.runCommand(
                process,
                allocator,
                &.{ "git", "-C", destination, "submodule", "update", "--init", "--recursive", "--depth", "1" },
                ".",
                progress,
                destination,
            );
            defer submodule_result.deinit(allocator);

            if (submodule_result.exit_code == null or submodule_result.exit_code.? != 0) {
                return error.GitSubmoduleUpdateFailed;
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

        /// Lists tags of a git repo, preferring peeled commits for annotated tags.
        fn fetchGitTags(io: std.Io, allocator: std.mem.Allocator, url: []const u8) ![]GitTag {
            const git_url = if (std.ascii.startsWithIgnoreCase(url, "git+"))
                url["git+".len..]
            else
                url;

            const result = try std.process.run(
                allocator,
                io,
                .{
                    .argv = &.{ "git", "ls-remote", "--tags", git_url },
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

                var columns = std.mem.tokenizeAny(u8, line, " \t");
                const commit = columns.next() orelse continue;
                const ref_name = columns.next() orelse continue;
                if (!std.mem.startsWith(u8, ref_name, "refs/tags/")) {
                    continue;
                }

                const raw_tag = ref_name["refs/tags/".len..];
                const peeled = std.mem.endsWith(u8, raw_tag, "^{}");
                const tag = if (peeled) raw_tag[0 .. raw_tag.len - 3] else raw_tag;

                for (tags.items) |*existing| {
                    if (std.mem.eql(u8, existing.tag, tag)) {
                        if (peeled) {
                            allocator.free(existing.commit);
                            existing.commit = try allocator.dupe(u8, commit);
                        }

                        break;
                    }
                } else {
                    try tags.append(
                        allocator,
                        .{
                            .commit = try allocator.dupe(u8, commit),
                            .tag = try allocator.dupe(u8, tag),
                        },
                    );
                }
            }

            return try tags.toOwnedSlice(allocator);
        }

        /// Resolves this source version constraint to the highest matching semver tag.
        fn resolveVersionRef(self: Source, io: std.Io, allocator: std.mem.Allocator) ![]const u8 {
            const requested_version = self.version orelse return error.MissingVersionConstraint;
            if (!isGitUrl(self.url)) {
                return error.UnsupportedVersionConstraint;
            }

            const tags = try fetchGitTags(io, allocator, self.url);
            defer {
                for (tags) |*tag| {
                    tag.deinit(allocator);
                }
                allocator.free(tags);
            }

            var best_index: ?usize = null;
            var best_version: ?std.SemanticVersion = null;
            for (tags, 0..) |tag, index| {
                const tag_version = parseSemverTag(tag.tag) orelse continue;
                if (!self.constraint.matches(requested_version, tag_version)) {
                    continue;
                }

                if (best_version == null or best_version.?.order(tag_version) == .lt) {
                    best_version = tag_version;
                    best_index = index;
                }
            }

            const index = best_index orelse return error.NoMatchingGitTag;

            return try allocator.dupe(u8, tags[index].tag);
        }

        /// Validates that a lock entry still matches this manifest source.
        fn validateLock(self: Source, locked: ResolvedSource) !void {
            if (!std.mem.eql(u8, self.url, locked.url)) {
                return error.ManifestLockDrift;
            }

            if (self.version) |requested_version| {
                const locked_ref = locked.ref orelse return error.ManifestLockDrift;
                const locked_version = parseSemverTag(locked_ref) orelse return error.ManifestLockDrift;
                if (!self.constraint.matches(requested_version, locked_version)) {
                    return error.ManifestLockDrift;
                }

                return;
            }

            if ((self.ref == null and locked.ref != null) or
                (self.ref != null and locked.ref == null) or
                (self.ref != null and locked.ref != null and
                    !std.mem.eql(u8, self.ref.?, locked.ref.?)))
            {
                return error.ManifestLockDrift;
            }
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

            /// Checks whether `candidate` satisfies this constraint against `requested`.
            fn matches(self: Constraint, requested: std.SemanticVersion, candidate: std.SemanticVersion) bool {
                const order = switch (self) {
                    .lessThan,
                    .equalOrLessThan,
                    .equalTo,
                    .greaterThan,
                    .equalOrGreater,
                    => candidate.order(requested),

                    .majorLessThan,
                    .majorEqualOrLessThan,
                    .majorEqualTo,
                    .majorGreaterThan,
                    .majorEqualOrGreater,
                    => std.math.order(candidate.major, requested.major),

                    .minorLessThan,
                    .minorEqualOrLessThan,
                    .minorEqualTo,
                    .minorGreaterThan,
                    .minorEqualOrGreater,
                    => minorVersionOrder(candidate, requested),
                };

                return switch (self) {
                    .lessThan, .majorLessThan, .minorLessThan => order == .lt,
                    .equalOrLessThan, .majorEqualOrLessThan, .minorEqualOrLessThan => order != .gt,
                    .equalTo, .majorEqualTo, .minorEqualTo => order == .eq,
                    .greaterThan, .majorGreaterThan, .minorGreaterThan => order == .gt,
                    .equalOrGreater, .majorEqualOrGreater, .minorEqualOrGreater => order != .lt,
                };
            }
        };
    };
};

/// Tracks and prints single-line package operation progress.
const InitProgress = struct {
    /// Spinner frames used while rewriting the progress line.
    const init_progress_frames = [_][]const u8{ "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" };

    /// Terminal writer used for progress output.
    out: *std.Io.Writer,
    /// Number of completed work units.
    completed: usize = 0,
    /// Number of expected work units.
    total: usize,
    /// Spinner frame to show for the next progress update.
    frame: usize = 0,

    /// Redraws the progress display without completing another unit of work.
    fn tick(self: *InitProgress, path: []const u8) !void {
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

    /// Redraws the single-line progress display for a completed work unit.
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

/// Wraps a bare package object into typed buzz source.
fn wrapManifestObject(
    allocator: std.mem.Allocator,
    raw_source: []const u8,
    wrapper_prefix: []const u8,
) ![]const u8 {
    const source = std.mem.trim(u8, raw_source, " \n\r\t");

    var manifest_source = std.ArrayList(u8).empty;
    // Wrap into a script that will leave the manifest as the last global of the VM
    // We type the variable so that if user gives anything else, we get an error
    try manifest_source.appendSlice(allocator, wrapper_prefix);
    try manifest_source.appendSlice(allocator, source);

    if (!std.mem.endsWith(u8, source, ";")) {
        try manifest_source.append(allocator, ';');
    }

    return try manifest_source.toOwnedSlice(allocator);
}

pub fn wrapManifest(allocator: std.mem.Allocator, raw_source: []const u8) ![]const u8 {
    return try wrapManifestObject(allocator, raw_source, manifest_wrapper_prefix);
}

/// Wraps a bare manifest lock object into valid buzz code.
pub fn wrapManifestLock(allocator: std.mem.Allocator, raw_source: []const u8) ![]const u8 {
    return try wrapManifestObject(allocator, raw_source, manifest_lock_wrapper_prefix);
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

/// Loads and parses a package lock manifest from disk.
pub fn loadManifestLock(process: std.process.Init, allocator: std.mem.Allocator, manifest_lock_path: []const u8) !ManifestLock {
    var file = try std.Io.Dir.cwd().openFile(
        process.io,
        manifest_lock_path,
        .{
            .mode = .read_only,
        },
    );
    defer file.close(process.io);

    const raw_source = try allocator.alloc(u8, (try file.stat(process.io)).size);
    _ = try file.readPositionalAll(process.io, raw_source, 0);
    const manifest_source = try wrapManifestLock(allocator, raw_source);

    var runner: Runner = undefined;
    try runner.init(
        process,
        allocator, // We want the parsed value to outlive this function
        .Repl,
        null,
        null,
    );

    if (try runner.runManifest(manifest_source, "manifestLock")) |manifest_lock| {
        return try .fromValue(manifest_lock, allocator);
    }

    return error.ManifestLockNotProduced;
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
        \\        "{s}": [[ "zig", "build" ]],
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
    var progress = InitProgress{
        .out = &stdout.interface,
        .total = 5,
    };

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

/// Appends one lock map with stable package-name ordering.
fn appendLockMap(
    allocator: std.mem.Allocator,
    buffer: *std.ArrayList(u8),
    comptime name: []const u8,
    map: std.StringHashMapUnmanaged(ResolvedSource),
) !void {
    try buffer.print(allocator, "    {s} = ", .{name});

    if (map.count() == 0) {
        try buffer.appendSlice(allocator, "{},\n");

        return;
    }

    try buffer.appendSlice(allocator, "{\n");

    var keys = std.ArrayList([]const u8).empty;
    defer keys.deinit(allocator);

    var it = map.iterator();
    while (it.next()) |entry| {
        try keys.append(allocator, entry.key_ptr.*);
    }
    std.mem.sort([]const u8, keys.items, {}, stringLessThan);

    for (keys.items) |key| {
        const source = map.get(key).?;

        try buffer.appendSlice(allocator, "        ");
        try appendEscapedBuzzStringLiteral(allocator, buffer, key);
        try buffer.appendSlice(allocator, ": .{\n            url = ");
        try appendEscapedBuzzStringLiteral(allocator, buffer, source.url);
        try buffer.appendSlice(allocator, ",\n");

        if (source.ref) |ref| {
            try buffer.appendSlice(allocator, "            ref = ");
            try appendEscapedBuzzStringLiteral(allocator, buffer, ref);
            try buffer.appendSlice(allocator, ",\n");
        }

        try buffer.appendSlice(allocator, "            hash = ");
        try appendEscapedBuzzStringLiteral(allocator, buffer, source.hash);
        try buffer.appendSlice(allocator, ",\n        },\n");
    }

    try buffer.appendSlice(allocator, "    },\n");
}

/// Appends a buzz string literal, escaping generated values as needed.
fn appendEscapedBuzzStringLiteral(
    allocator: std.mem.Allocator,
    buffer: *std.ArrayList(u8),
    value: []const u8,
) !void {
    try buffer.append(allocator, '"');
    for (value) |byte| {
        switch (byte) {
            '\\' => try buffer.appendSlice(allocator, "\\\\"),
            '"' => try buffer.appendSlice(allocator, "\\\""),
            '{' => try buffer.appendSlice(allocator, "\\{"),
            '\n' => try buffer.appendSlice(allocator, "\\n"),
            '\r' => try buffer.appendSlice(allocator, "\\r"),
            '\t' => try buffer.appendSlice(allocator, "\\t"),
            else => if (byte < 0x20 or byte == 0x7f) {
                try buffer.print(allocator, "\\{d}", .{byte});
            } else {
                try buffer.append(allocator, byte);
            },
        }
    }
    try buffer.append(allocator, '"');
}

/// Sorts byte strings lexicographically.
fn stringLessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
    return std.mem.lessThan(u8, lhs, rhs);
}

/// Parses semver tags with an optional leading `v`.
fn parseSemverTag(tag: []const u8) ?std.SemanticVersion {
    const version_text = if (tag.len > 0 and tag[0] == 'v')
        tag[1..]
    else
        tag;
    const version = std.SemanticVersion.parse(version_text) catch return null;
    if (version.pre != null or version.build != null) {
        return null;
    }

    return version;
}

/// Compares the major/minor prefix of two semantic versions.
fn minorVersionOrder(candidate: std.SemanticVersion, requested: std.SemanticVersion) std.math.Order {
    if (candidate.major < requested.major) return .lt;
    if (candidate.major > requested.major) return .gt;
    if (candidate.minor < requested.minor) return .lt;
    if (candidate.minor > requested.minor) return .gt;

    return .eq;
}

/// Updates a SHA-256 tree hash with a little-endian integer.
fn updatePackageHashInt(hasher: *std.crypto.hash.sha2.Sha256, value: u64) void {
    var buffer: [8]u8 = undefined;
    std.mem.writeInt(u64, &buffer, value, .little);
    hasher.update(&buffer);
}

/// Computes a deterministic SHA-256 hash of package content, excluding `.git`.
fn hashPackageTree(io: std.Io, allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var dir = try std.Io.Dir.cwd().openDir(io, path, .{ .iterate = true });
    defer dir.close(io);

    var entries = std.ArrayList(HashEntry).empty;
    defer {
        for (entries.items) |entry| {
            allocator.free(entry.path);
        }
        entries.deinit(allocator);
    }

    var walker = try dir.walk(allocator);
    defer walker.deinit();
    while (try walker.next(io)) |entry| {
        var components = std.fs.path.componentIterator(entry.path);
        var contains_git_dir = false;
        while (components.next()) |component| {
            if (std.mem.eql(u8, component.name, ".git")) {
                contains_git_dir = true;

                break;
            }
        }
        if (contains_git_dir) {
            continue;
        }

        switch (entry.kind) {
            .directory => {},
            .file, .sym_link => try entries.append(allocator, .{
                .path = try allocator.dupe(u8, entry.path),
                .kind = entry.kind,
            }),
            else => return error.UnsupportedDirectoryEntry,
        }
    }
    std.mem.sort(HashEntry, entries.items, {}, HashEntry.lessThan);

    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    for (entries.items) |entry| {
        const kind_marker: [1]u8 = .{switch (entry.kind) {
            .file => 'F',
            .sym_link => 'L',
            else => unreachable,
        }};
        hasher.update(&kind_marker);
        updatePackageHashInt(&hasher, @intCast(entry.path.len));
        hasher.update(entry.path);

        switch (entry.kind) {
            .file => {
                var file = try dir.openFile(io, entry.path, .{ .mode = .read_only });
                defer file.close(io);

                const stat = try file.stat(io);
                updatePackageHashInt(&hasher, stat.size);

                var file_reader_buffer: [16 * 1024]u8 = undefined;
                var file_reader = file.reader(io, &file_reader_buffer);
                var read_buffer: [16 * 1024]u8 = undefined;
                while (true) {
                    const bytes_read = try file_reader.interface.readSliceShort(&read_buffer);
                    if (bytes_read == 0) {
                        break;
                    }

                    hasher.update(read_buffer[0..bytes_read]);
                }
            },
            .sym_link => {
                var target_buffer: [std.Io.Dir.max_path_bytes]u8 = undefined;
                const target_len = try dir.readLink(io, entry.path, &target_buffer);
                const target = target_buffer[0..target_len];
                updatePackageHashInt(&hasher, @intCast(target.len));
                hasher.update(target);
            },
            else => unreachable,
        }
    }

    var digest: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
    hasher.final(&digest);
    const hex = std.fmt.bytesToHex(digest, .lower);

    return try allocator.dupe(u8, hex[0..]);
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

/// Builds a minimal process init for package tests.
fn testingProcess(
    allocator: std.mem.Allocator,
    process_arena: *std.heap.ArenaAllocator,
    environ_map: *std.process.Environ.Map,
    argv: []const [*:0]const u8,
) !std.process.Init {
    return .{
        .minimal = .{
            .args = .{ .vector = argv },
            .environ = std.testing.environ,
        },
        .arena = process_arena,
        .gpa = allocator,
        .io = std.testing.io,
        .environ_map = environ_map,
        .preopens = try std.process.Preopens.init(process_arena.allocator()),
    };
}

/// Returns the absolute path of a temporary test directory.
fn testingTmpPath(allocator: std.mem.Allocator, dir: std.Io.Dir) ![]const u8 {
    var buffer: [std.Io.Dir.max_path_bytes]u8 = undefined;
    const len = try dir.realPath(std.testing.io, &buffer);

    return try allocator.dupe(u8, buffer[0..len]);
}

/// Runs a test command and fails when it exits unsuccessfully.
fn expectCommandOk(allocator: std.mem.Allocator, argv: []const []const u8, cwd: []const u8) !void {
    const result = try std.process.run(
        allocator,
        std.testing.io,
        .{
            .argv = argv,
            .cwd = .{ .path = cwd },
            .stdout_limit = .limited(1024 * 1024),
            .stderr_limit = .limited(1024 * 1024),
        },
    );
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    switch (result.term) {
        .exited => |code| if (code != 0) {
            std.debug.print(
                "command failed: {s}\nstdout:\n{s}\nstderr:\n{s}\n",
                .{
                    argv[0],
                    result.stdout,
                    result.stderr,
                },
            );

            return error.TestCommandFailed;
        },
        else => return error.TestCommandFailed,
    }
}

test "manifest lock wraps and parses bare object" {
    const allocator = std.testing.allocator;

    var process_arena = std.heap.ArenaAllocator.init(allocator);
    defer process_arena.deinit();

    var environ_map = try std.process.Environ.createMap(std.testing.environ, allocator);
    defer environ_map.deinit();

    const argv = [_][*:0]const u8{"package_test"};
    const process = try testingProcess(allocator, &process_arena, &environ_map, &argv);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const source =
        \\.{
        \\    dependencies = {
        \\        "dep": .{
        \\            url = "somewhere",
        \\            ref = "v1.0.0",
        \\            hash = "abc",
        \\        },
        \\    },
        \\}
    ;

    const wrapped = try wrapManifestLock(allocator, source);
    defer allocator.free(wrapped);
    try std.testing.expect(std.mem.startsWith(
        u8,
        wrapped,
        "import \"buzz:manifest\" as _;final manifestLock: ManifestLock = .{",
    ));

    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = MANIFEST_LOCK,
        .data = source,
    });

    const tmp_path = try testingTmpPath(allocator, tmp.dir);
    defer allocator.free(tmp_path);
    const lock_path = try std.fs.path.join(allocator, &.{ tmp_path, MANIFEST_LOCK });
    defer allocator.free(lock_path);

    const lock = try loadManifestLock(process, process_arena.allocator(), lock_path);
    const dep = lock.dependencies.get("dep").?;
    try std.testing.expectEqualStrings("somewhere", dep.url);
    try std.testing.expectEqualStrings("v1.0.0", dep.ref.?);
    try std.testing.expectEqualStrings("abc", dep.hash);
}

test "manifest lock serialization is deterministic" {
    const allocator = std.testing.allocator;

    var lock = ManifestLock{};
    defer lock.dependencies.deinit(allocator);
    defer lock.dev_dependencies.deinit(allocator);

    try lock.dependencies.put(allocator, "z", .{
        .url = "last",
        .ref = null,
        .hash = "hash-z",
    });
    try lock.dependencies.put(allocator, "a", .{
        .url = "first",
        .ref = "v1.0.0",
        .hash = "hash-a",
    });

    const source = try lock.toSource(allocator);
    defer allocator.free(source);

    try std.testing.expectEqualStrings(
        \\.{
        \\    dependencies = {
        \\        "a": .{
        \\            url = "first",
        \\            ref = "v1.0.0",
        \\            hash = "hash-a",
        \\        },
        \\        "z": .{
        \\            url = "last",
        \\            hash = "hash-z",
        \\        },
        \\    },
        \\    devDependencies = {},
        \\}
        \\
    ,
        source,
    );
}

test "version constraints use full major and minor comparisons" {
    const requested: std.SemanticVersion = .{ .major = 1, .minor = 2, .patch = 3 };

    try std.testing.expect(Manifest.Source.Constraint.equalTo.matches(
        requested,
        .{ .major = 1, .minor = 2, .patch = 3 },
    ));
    try std.testing.expect(!Manifest.Source.Constraint.equalTo.matches(
        requested,
        .{ .major = 1, .minor = 2, .patch = 4 },
    ));
    try std.testing.expect(Manifest.Source.Constraint.majorEqualTo.matches(
        requested,
        .{ .major = 1, .minor = 9, .patch = 0 },
    ));
    try std.testing.expect(!Manifest.Source.Constraint.majorEqualTo.matches(
        requested,
        .{ .major = 2, .minor = 0, .patch = 0 },
    ));
    try std.testing.expect(Manifest.Source.Constraint.minorEqualTo.matches(
        requested,
        .{ .major = 1, .minor = 2, .patch = 99 },
    ));
    try std.testing.expect(!Manifest.Source.Constraint.minorEqualTo.matches(
        requested,
        .{ .major = 1, .minor = 3, .patch = 0 },
    ));
}

test "git version constraint selects highest matching semver tag" {
    const allocator = std.testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const repo_path = try testingTmpPath(allocator, tmp.dir);
    defer allocator.free(repo_path);

    try expectCommandOk(allocator, &.{ "git", "init" }, repo_path);
    try expectCommandOk(allocator, &.{ "git", "config", "user.email", "test@example.com" }, repo_path);
    try expectCommandOk(allocator, &.{ "git", "config", "user.name", "Test User" }, repo_path);

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "value.txt", .data = "one" });
    try expectCommandOk(allocator, &.{ "git", "add", "value.txt" }, repo_path);
    try expectCommandOk(allocator, &.{ "git", "commit", "-m", "one" }, repo_path);
    try expectCommandOk(allocator, &.{ "git", "tag", "v1.0.0" }, repo_path);

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "value.txt", .data = "two" });
    try expectCommandOk(allocator, &.{ "git", "add", "value.txt" }, repo_path);
    try expectCommandOk(allocator, &.{ "git", "commit", "-m", "two" }, repo_path);
    try expectCommandOk(allocator, &.{ "git", "tag", "1.2.0" }, repo_path);

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "value.txt", .data = "three" });
    try expectCommandOk(allocator, &.{ "git", "add", "value.txt" }, repo_path);
    try expectCommandOk(allocator, &.{ "git", "commit", "-m", "three" }, repo_path);
    try expectCommandOk(allocator, &.{ "git", "tag", "v1.3.0" }, repo_path);

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "value.txt", .data = "four" });
    try expectCommandOk(allocator, &.{ "git", "add", "value.txt" }, repo_path);
    try expectCommandOk(allocator, &.{ "git", "commit", "-m", "four" }, repo_path);
    try expectCommandOk(allocator, &.{ "git", "tag", "2.0.0" }, repo_path);

    const source: Manifest.Source = .{
        .url = try std.fmt.allocPrint(allocator, "git+{s}", .{repo_path}),
        .ref = null,
        .version = .{ .major = 1, .minor = 0, .patch = 0 },
        .constraint = .majorEqualTo,
    };
    defer allocator.free(source.url);
    const resolved_ref = try source.resolveVersionRef(std.testing.io, allocator);
    defer allocator.free(resolved_ref);

    try std.testing.expectEqualStrings("v1.3.0", resolved_ref);
}

test "source fetch writes and validates lock file" {
    const allocator = std.testing.allocator;

    var process_arena = std.heap.ArenaAllocator.init(allocator);
    defer process_arena.deinit();

    var environ_map = try std.process.Environ.createMap(std.testing.environ, allocator);
    defer environ_map.deinit();

    const argv = [_][*:0]const u8{"package_test"};
    const process = try testingProcess(allocator, &process_arena, &environ_map, &argv);

    var progress_output = std.Io.Writer.Allocating.init(allocator);
    defer progress_output.deinit();
    var progress = InitProgress{
        .out = &progress_output.writer,
        .total = 1,
    };

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDir(std.testing.io, "dep", .default_dir);
    var dep_dir = try tmp.dir.openDir(std.testing.io, "dep", .{});
    defer dep_dir.close(std.testing.io);

    try dep_dir.writeFile(std.testing.io, .{
        .sub_path = "data.txt",
        .data = "hello",
    });

    const root_path = try testingTmpPath(allocator, tmp.dir);
    defer allocator.free(root_path);
    const dep_path = try std.fs.path.join(allocator, &.{ root_path, "dep" });
    defer allocator.free(dep_path);

    const source: Manifest.Source = .{
        .url = dep_path,
        .ref = null,
    };

    const fetched = try source.fetch(process, allocator, root_path, "dep", &progress);
    defer allocator.free(fetched.destination);
    defer allocator.free(fetched.resolved_source.hash);

    var lock = ManifestLock{};
    defer lock.dependencies.deinit(allocator);
    defer lock.dev_dependencies.deinit(allocator);
    try lock.dependencies.put(allocator, "dep", fetched.resolved_source);

    const lock_path = try std.fs.path.join(allocator, &.{ root_path, MANIFEST_LOCK });
    defer allocator.free(lock_path);
    try lock.write(std.testing.io, allocator, lock_path);

    const loaded_lock = try loadManifestLock(process, process_arena.allocator(), lock_path);
    const locked_dep = loaded_lock.dependencies.get("dep").?;
    try std.testing.expectEqualStrings(dep_path, locked_dep.url);
    try std.testing.expect(locked_dep.ref == null);
    try std.testing.expectEqual(@as(usize, 64), locked_dep.hash.len);

    const vendor_dep_path = try std.fs.path.join(allocator, &.{ root_path, VENDORS, "dep" });
    defer allocator.free(vendor_dep_path);
    const vendor_hash = try hashPackageTree(std.testing.io, allocator, vendor_dep_path);
    defer allocator.free(vendor_hash);
    try std.testing.expectEqualStrings(vendor_hash, locked_dep.hash);

    const locked_fetch = try locked_dep.fetch(process, allocator, root_path, "dep", &progress);
    defer allocator.free(locked_fetch.destination);
    try std.testing.expect(!locked_fetch.downloaded);

    try source.validateLock(locked_dep);
    try std.testing.expectError(error.ManifestLockDrift, (Manifest.Source{
        .url = "/somewhere/else",
        .ref = null,
    }).validateLock(locked_dep));
}
