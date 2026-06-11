//! Stage 1 of the AOT story (see issue #302): a "bundler".
//!
//! Instead of emitting native code, `buzz compile` produces a standalone
//! executable by copying the buzz runtime binary and appending the script
//! to it. At startup the runtime checks whether it has a payload glued to
//! its tail; if so it runs that instead of acting as the normal CLI.
//!
//! On-disk layout of a bundled executable:
//!
//!     [ buzz runtime binary ][ source ][ name ][ root_dir ][ Footer ]
//!
//! The Footer is fixed size and ends the file, so a bundled binary can locate
//! its own payload by reading its last `footer_size` bytes. This reuses the
//! existing parser/codegen/VM (no new codegen) and keeps buzz state, stack and
//! GC exactly as they are when running a script normally — the executable is
//! "runtime + program", which is what the GC needs anyway.
//!
//! Known limitations (intentional for a stage 1 prototype):
//!   - The script source is embedded, not its bytecode, so the bundle still
//!     parses/compiles at startup. Serializing compiled functions is a later
//!     step.
//!   - `import` of the std lib still resolves from disk (via BUZZ_PATH / the
//!     embedded root_dir), so bundles are not yet fully self-contained.

const std = @import("std");

/// Identifies a buzz bundle. Bumped if the footer layout changes.
pub const magic = "buzzbndl".*;
pub const format_version: u32 = 1;

/// magic(8) + version(4) + 6 * u64 offsets/lengths.
pub const footer_size = 8 + 4 + 8 * 6;

const Footer = struct {
    source_offset: u64,
    source_len: u64,
    name_offset: u64,
    name_len: u64,
    root_dir_offset: u64,
    root_dir_len: u64,

    fn read(buf: *const [footer_size]u8) ?Footer {
        if (!std.mem.eql(u8, buf[0..8], &magic)) return null;
        if (std.mem.readInt(u32, buf[8..12], .little) != format_version) return null;

        return .{
            .source_offset = std.mem.readInt(u64, buf[12..20], .little),
            .source_len = std.mem.readInt(u64, buf[20..28], .little),
            .name_offset = std.mem.readInt(u64, buf[28..36], .little),
            .name_len = std.mem.readInt(u64, buf[36..44], .little),
            .root_dir_offset = std.mem.readInt(u64, buf[44..52], .little),
            .root_dir_len = std.mem.readInt(u64, buf[52..60], .little),
        };
    }

    fn write(self: Footer, buf: *[footer_size]u8) void {
        @memcpy(buf[0..8], &magic);
        std.mem.writeInt(u32, buf[8..12], format_version, .little);
        std.mem.writeInt(u64, buf[12..20], self.source_offset, .little);
        std.mem.writeInt(u64, buf[20..28], self.source_len, .little);
        std.mem.writeInt(u64, buf[28..36], self.name_offset, .little);
        std.mem.writeInt(u64, buf[36..44], self.name_len, .little);
        std.mem.writeInt(u64, buf[44..52], self.root_dir_offset, .little);
        std.mem.writeInt(u64, buf[52..60], self.root_dir_len, .little);
    }
};

pub const Payload = struct {
    source: []u8,
    name: []u8,
    root_dir: []u8,

    pub fn deinit(self: Payload, allocator: std.mem.Allocator) void {
        allocator.free(self.source);
        allocator.free(self.name);
        allocator.free(self.root_dir);
    }
};

/// Absolute path of the running executable, the same primitive buzz uses to
/// locate its std lib prefix (see `Parser.buzzPrefix`).
fn selfExePath(allocator: std.mem.Allocator, io: std.Io) ![]u8 {
    var buf: [std.Io.Dir.max_path_bytes]u8 = undefined;
    const len = try std.process.executablePath(io, &buf);
    return allocator.dupe(u8, buf[0..len]);
}

fn readAllAlloc(io: std.Io, file: anytype, allocator: std.mem.Allocator) ![]u8 {
    const size = (try file.stat(io)).size;
    const bytes = try allocator.alloc(u8, size);
    errdefer allocator.free(bytes);
    _ = try file.readPositionalAll(io, bytes, 0);
    return bytes;
}

/// If the running executable has a bundle glued to its tail, return its payload.
/// Returns null (never errors) when not a bundle, so a normal `buzz` invocation
/// falls through to the regular CLI.
pub fn read(allocator: std.mem.Allocator, io: std.Io) ?Payload {
    return readImpl(allocator, io) catch null;
}

fn readImpl(allocator: std.mem.Allocator, io: std.Io) !?Payload {
    const exe_path = try selfExePath(allocator, io);
    defer allocator.free(exe_path);

    var file = try std.Io.Dir.openFileAbsolute(io, exe_path, .{});
    defer file.close(io);

    const size = (try file.stat(io)).size;
    if (size < footer_size) return null;

    var footer_buf: [footer_size]u8 = undefined;
    _ = try file.readPositionalAll(io, &footer_buf, size - footer_size);

    const footer = Footer.read(&footer_buf) orelse return null;

    const source = try allocator.alloc(u8, footer.source_len);
    errdefer allocator.free(source);
    _ = try file.readPositionalAll(io, source, footer.source_offset);

    const name = try allocator.alloc(u8, footer.name_len);
    errdefer allocator.free(name);
    _ = try file.readPositionalAll(io, name, footer.name_offset);

    const root_dir = try allocator.alloc(u8, footer.root_dir_len);
    errdefer allocator.free(root_dir);
    _ = try file.readPositionalAll(io, root_dir, footer.root_dir_offset);

    return .{ .source = source, .name = name, .root_dir = root_dir };
}

/// Produce `out_path`: a copy of the running buzz binary with `script_path`
/// (and the root dir it should resolve imports against) appended.
pub fn write(
    allocator: std.mem.Allocator,
    io: std.Io,
    script_path: []const u8,
    out_path: []const u8,
) !void {
    var script_file = if (std.fs.path.isAbsolute(script_path))
        try std.Io.Dir.openFileAbsolute(io, script_path, .{})
    else
        try std.Io.Dir.cwd().openFile(io, script_path, .{});
    defer script_file.close(io);

    const source = try readAllAlloc(io, script_file, allocator);
    defer allocator.free(source);

    // Resolve the script's absolute path so imports keep working the same way
    // they would when running the script directly.
    var abs_buf: [std.Io.Dir.max_path_bytes]u8 = undefined;
    const abs_len = try script_file.realPath(io, &abs_buf);
    const name = abs_buf[0..abs_len];
    const root_dir = std.fs.path.dirname(name) orelse ".";

    const exe_path = try selfExePath(allocator, io);
    defer allocator.free(exe_path);

    var exe_file = try std.Io.Dir.openFileAbsolute(io, exe_path, .{});
    const runtime = try readAllAlloc(io, exe_file, allocator);
    exe_file.close(io);
    defer allocator.free(runtime);

    const source_offset = runtime.len;
    const name_offset = source_offset + source.len;
    const root_dir_offset = name_offset + name.len;

    var footer_buf: [footer_size]u8 = undefined;
    (Footer{
        .source_offset = source_offset,
        .source_len = source.len,
        .name_offset = name_offset,
        .name_len = name.len,
        .root_dir_offset = root_dir_offset,
        .root_dir_len = root_dir.len,
    }).write(&footer_buf);

    var out_file = try std.Io.Dir.cwd().createFile(io, out_path, .{ .truncate = true });
    defer out_file.close(io);

    // Make the result runnable (no-op on platforms without a unix exec bit).
    try out_file.setPermissions(io, .executable_file);

    var out_buffer: [64 * 1024]u8 = undefined;
    var out_writer = out_file.writer(io, &out_buffer);
    const w = &out_writer.interface;

    try w.writeAll(runtime);
    try w.writeAll(source);
    try w.writeAll(name);
    try w.writeAll(root_dir);
    try w.writeAll(&footer_buf);
    try w.flush();
}
