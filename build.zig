const std = @import("std");
const Builder = std.build.Builder;

pub fn build(b: *Builder) void {
    const build_mode = b.standardReleaseOptions();
    const target = b.standardTargetOptions(.{});

    var exe = b.addExecutable("buzz", "src/main.zig");
    exe.setTarget(target);
    exe.install();
    exe.setBuildMode(build_mode);
    exe.setMainPkgPath(".");

    var lib = b.addSharedLibrary("buzz", "src/buzz_api.zig", .{ .unversioned = {} });
    lib.setTarget(target);
    lib.install();
    lib.setMainPkgPath(".");
    lib.setBuildMode(build_mode);

    var std_lib = b.addSharedLibrary("std", "lib/buzz_std.zig", .{ .unversioned = {} });
    std_lib.setTarget(target);
    std_lib.install();
    std_lib.setMainPkgPath(".");
    std_lib.setBuildMode(build_mode);
    std_lib.linkLibrary(lib);

    var io_lib = b.addSharedLibrary("io", "lib/buzz_io.zig", .{ .unversioned = {} });
    io_lib.setTarget(target);
    io_lib.install();
    io_lib.setMainPkgPath(".");
    io_lib.setBuildMode(build_mode);
    io_lib.linkLibrary(lib);

    var gc_lib = b.addSharedLibrary("gc", "lib/buzz_gc.zig", .{ .unversioned = {} });
    gc_lib.setTarget(target);
    gc_lib.install();
    gc_lib.setMainPkgPath(".");
    gc_lib.setBuildMode(build_mode);
    gc_lib.linkLibrary(lib);

    var os_lib = b.addSharedLibrary("os", "lib/buzz_os.zig", .{ .unversioned = {} });
    os_lib.setTarget(target);
    os_lib.install();
    os_lib.setMainPkgPath(".");
    os_lib.setBuildMode(build_mode);
    os_lib.linkLibrary(lib);

    var fs_lib = b.addSharedLibrary("fs", "lib/buzz_fs.zig", .{ .unversioned = {} });
    fs_lib.setTarget(target);
    fs_lib.install();
    fs_lib.setMainPkgPath(".");
    fs_lib.setBuildMode(build_mode);
    fs_lib.linkLibrary(lib);

    var math_lib = b.addSharedLibrary("math", "lib/buzz_math.zig", .{ .unversioned = {} });
    math_lib.setTarget(target);
    math_lib.install();
    math_lib.setMainPkgPath(".");
    math_lib.setBuildMode(build_mode);
    math_lib.linkLibrary(lib);

    b.default_step.dependOn(&exe.step);
    b.default_step.dependOn(&lib.step);
    b.default_step.dependOn(&std_lib.step);
    b.default_step.dependOn(&io_lib.step);
    b.default_step.dependOn(&gc_lib.step);
    b.default_step.dependOn(&os_lib.step);
    b.default_step.dependOn(&fs_lib.step);
    b.default_step.dependOn(&math_lib.step);

    const play = b.step("run", "Run buzz");
    const run = exe.run();
    run.step.dependOn(b.getInstallStep());
    play.dependOn(&run.step);
}
