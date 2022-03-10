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

    b.default_step.dependOn(&exe.step);
    b.default_step.dependOn(&lib.step);
    b.default_step.dependOn(&std_lib.step);
    b.default_step.dependOn(&io_lib.step);

    const play = b.step("run", "Run buzz");
    const run = exe.run();
    run.step.dependOn(b.getInstallStep());
    play.dependOn(&run.step);
}
