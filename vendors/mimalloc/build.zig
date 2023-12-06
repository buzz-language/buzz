const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const upstream = b.dependency("mimalloc", .{});

    const lib = b.addStaticLibrary(.{
        .name = "mimalloc",
        .target = target,
        .optimize = optimize,
    });
    lib.linkLibC();
    lib.addIncludePath(upstream.path("include"));
    lib.installHeadersDirectoryOptions(.{
        .source_dir = upstream.path("include"),
        .install_dir = .header,
        .install_subdir = "",
        .include_extensions = &.{".h"},
    });

    const files = [_]std.Build.LazyPath{
        upstream.path("src/alloc-aligned.c"),
        upstream.path("src/alloc.c"),
        upstream.path("src/arena.c"),
        upstream.path("src/bitmap.c"),
        upstream.path("src/heap.c"),
        upstream.path("src/init.c"),
        upstream.path("src/options.c"),
        upstream.path("src/os.c"),
        upstream.path("src/page.c"),
        upstream.path("src/random.c"),
        upstream.path("src/segment-map.c"),
        upstream.path("src/segment.c"),
        upstream.path("src/stats.c"),
        upstream.path("src/prim/prim.c"),
    };

    for (files) |file| {
        lib.addCSourceFile(
            .{
                .file = file,
                .flags = if (lib.optimize != .Debug)
                    &.{
                        "-DNDEBUG=1",
                        "-DMI_SECURE=0",
                        "-DMI_STAT=0",
                    }
                else
                    &.{},
            },
        );
    }

    b.installArtifact(lib);
}
