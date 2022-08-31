const std = @import("std");
const builtin = @import("builtin");
const Builder = std.build.Builder;

pub fn build(b: *Builder) void {
    const build_mode = b.standardReleaseOptions();
    const target = b.standardTargetOptions(.{});

    var exe = b.addExecutable("buzz", "src/main.zig");
    exe.use_stage1 = true;
    exe.setTarget(target);
    exe.install();
    exe.addIncludePath("/usr/local/include");
    exe.addIncludePath("/usr/include");
    exe.linkSystemLibrary("pcre");
    if (builtin.os.tag == .linux) {
        exe.linkLibC();
    }
    exe.setBuildMode(build_mode);
    exe.setMainPkgPath(".");

    var lib = b.addSharedLibrary("buzz", "src/buzz_api.zig", .{ .unversioned = {} });
    lib.use_stage1 = true;
    lib.setTarget(target);
    lib.install();
    lib.addIncludePath("/usr/local/include");
    lib.addIncludePath("/usr/include");
    lib.linkSystemLibrary("pcre");
    if (builtin.os.tag == .linux) {
        lib.linkLibC();
    }
    lib.setMainPkgPath(".");
    lib.setBuildMode(build_mode);

    b.default_step.dependOn(&exe.step);
    b.default_step.dependOn(&lib.step);

    const lib_paths = [_][]const u8{
        "lib/buzz_std.zig",
        "lib/buzz_io.zig",
        "lib/buzz_gc.zig",
        "lib/buzz_os.zig",
        "lib/buzz_fs.zig",
        "lib/buzz_math.zig",
        "lib/buzz_debug.zig",
        "lib/buzz_buffer.zig",
    };
    const lib_names = [_][]const u8{
        "std",
        "io",
        "gc",
        "os",
        "fs",
        "math",
        "debug",
        "buffer",
    };

    for (lib_paths) |lib_path, index| {
        var std_lib = b.addSharedLibrary(lib_names[index], lib_path, .{ .unversioned = {} });
        std_lib.use_stage1 = true;
        std_lib.setTarget(target);
        std_lib.install();
        std_lib.addIncludePath("/usr/local/include");
        std_lib.addIncludePath("/usr/include");
        std_lib.linkSystemLibrary("pcre");
        if (builtin.os.tag == .linux) {
            std_lib.linkLibC();
        }
        std_lib.setMainPkgPath(".");
        std_lib.setBuildMode(build_mode);
        std_lib.linkLibrary(lib);
        b.default_step.dependOn(&std_lib.step);
    }

    const test_step = b.step("test", "Run all the tests");
    test_step.dependOn(b.getInstallStep());

    var unit_tests = b.addTest("src/main.zig");
    unit_tests.addIncludePath("/usr/local/include");
    unit_tests.addIncludePath("/usr/include");
    unit_tests.linkSystemLibrary("pcre");
    if (builtin.os.tag == .linux) {
        unit_tests.linkLibC();
    }
    unit_tests.use_stage1 = true;
    unit_tests.setBuildMode(.Debug);
    unit_tests.setTarget(target);
    test_step.dependOn(&unit_tests.step);
}
