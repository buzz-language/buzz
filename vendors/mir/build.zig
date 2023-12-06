const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const upstream = b.dependency("mir", .{});

    const lib = b.addStaticLibrary(.{
        .name = "mir",
        .target = target,
        .optimize = optimize,
    });
    lib.linkLibC();
    lib.addIncludePath(upstream.path(""));
    lib.installHeadersDirectoryOptions(.{
        .source_dir = upstream.path(""),
        .install_dir = .header,
        .install_subdir = "",
        .include_extensions = &.{".h"},
    });

    var flags = std.ArrayList([]const u8).init(b.allocator);
    defer flags.deinit();

    const files = [_]std.build.LazyPath{
        upstream.path("mir.c"),
        upstream.path("mir-gen.c"),
    };
    for (files) |file| {
        lib.addCSourceFile(.{
            .file = file,
            .flags = &.{},
        });
    }

    b.installArtifact(lib);
}
