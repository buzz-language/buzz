const std = @import("std");
const builtin = @import("builtin");
const Build = std.build;

const BuzzDebugOptions = struct {
    debug: bool,
    stack: bool,
    current_instruction: bool,
    perf: bool,
    stop_on_report: bool,
    placeholders: bool,

    pub fn step(self: BuzzDebugOptions, options: *std.build.OptionsStep) void {
        options.addOption(@TypeOf(self.debug), "debug", self.debug);
        options.addOption(@TypeOf(self.stack), "debug_stack", self.stack);
        options.addOption(@TypeOf(self.current_instruction), "debug_current_instruction", self.current_instruction);
        options.addOption(@TypeOf(self.perf), "show_perf", self.perf);
        options.addOption(@TypeOf(self.stop_on_report), "stop_on_report", self.stop_on_report);
        options.addOption(@TypeOf(self.placeholders), "debug_placeholders", self.placeholders);
    }
};

const BuzzJITOptions = struct {
    on: bool,
    always_on: bool,
    debug: bool,
    prof_threshold: f128 = 0.05,

    pub fn step(self: BuzzJITOptions, options: *std.build.OptionsStep) void {
        options.addOption(@TypeOf(self.debug), "jit_debug", self.debug);
        options.addOption(@TypeOf(self.always_on), "jit_always_on", self.always_on);
        options.addOption(@TypeOf(self.on), "jit", self.on);
        options.addOption(@TypeOf(self.prof_threshold), "jit_prof_threshold", self.prof_threshold);
    }
};

const BuzzGCOptions = struct {
    debug: bool,
    debug_light: bool,
    debug_access: bool,
    on: bool,
    initial_gc: usize,
    next_gc_ratio: usize,
    next_full_gc_ratio: usize,

    pub fn step(self: BuzzGCOptions, options: *std.build.OptionsStep) void {
        options.addOption(@TypeOf(self.debug), "gc_debug", self.debug);
        options.addOption(@TypeOf(self.debug_light), "gc_debug_light", self.debug_light);
        options.addOption(@TypeOf(self.debug_access), "gc_debug_access", self.debug_access);
        options.addOption(@TypeOf(self.on), "gc", self.on);
        options.addOption(@TypeOf(self.initial_gc), "initial_gc", self.initial_gc);
        options.addOption(@TypeOf(self.next_gc_ratio), "next_gc_ratio", self.next_gc_ratio);
        options.addOption(@TypeOf(self.next_full_gc_ratio), "next_full_gc_ratio", self.next_full_gc_ratio);
    }
};

const BuzzBuildOptions = struct {
    version: []const u8,
    sha: []const u8,
    mimalloc: bool,
    debug: BuzzDebugOptions,
    gc: BuzzGCOptions,
    jit: BuzzJITOptions,
    target: std.zig.CrossTarget,

    pub fn step(self: @This(), b: *Build) *std.build.OptionsStep {
        var options = b.addOptions();
        options.addOption(@TypeOf(self.version), "version", self.version);
        options.addOption(@TypeOf(self.sha), "sha", self.sha);
        options.addOption(@TypeOf(self.mimalloc), "mimalloc", self.mimalloc);

        self.debug.step(options);
        self.gc.step(options);
        self.jit.step(options);

        return options;
    }

    pub fn needLibC(self: @This()) bool {
        // TODO: remove libc if possible
        // mir can be built with musl libc
        // mimalloc can be built with musl libc
        // longjmp/setjmp need to be removed
        return self.target.isLinux() or self.mimalloc;
    }
};

fn get_buzz_prefix(b: *Build) []const u8 {
    return std.os.getenv("BUZZ_PATH") orelse std.fs.path.dirname(b.exe_dir).?;
}

pub fn build(b: *Build) !void {
    // Check minimum zig version
    const current_zig = builtin.zig_version;
    const min_zig = std.SemanticVersion.parse("0.12.0-dev.888+130227491") catch return;
    if (current_zig.order(min_zig).compare(.lt)) {
        @panic(b.fmt("Your Zig version v{} does not meet the minimum build requirement of v{}", .{ current_zig, min_zig }));
    }

    const build_mode = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const install_step = b.getInstallStep();

    var build_options = BuzzBuildOptions{
        .target = target,
        // Version is latest tag or empty string
        .version = std.mem.trim(
            u8,
            (std.ChildProcess.exec(.{
                .allocator = b.allocator,
                .argv = &.{
                    "git",
                    "describe",
                    "--tags",
                    "--abbrev=0",
                },
                .cwd = b.pathFromRoot("."),
                .expand_arg0 = .expand,
            }) catch {
                std.debug.print("Warning: failed to get git HEAD", .{});
                unreachable;
            }).stdout,
            "\n \t",
        ),
        // Current commit sha
        .sha = std.os.getenv("GIT_SHA") orelse
            std.os.getenv("GITHUB_SHA") orelse std.mem.trim(
            u8,
            (std.ChildProcess.exec(.{
                .allocator = b.allocator,
                .argv = &.{
                    "git",
                    "rev-parse",
                    "--short",
                    "HEAD",
                },
                .cwd = b.pathFromRoot("."),
                .expand_arg0 = .expand,
            }) catch {
                std.debug.print("Warning: failed to get git HEAD", .{});
                unreachable;
            }).stdout,
            "\n \t",
        ),
        .mimalloc = b.option(
            bool,
            "mimalloc",
            "Use mimalloc allocator",
        ) orelse true,
        .debug = .{
            .debug = b.option(
                bool,
                "debug",
                "Show debug information (AST, generated bytecode and more)",
            ) orelse false,
            .stack = b.option(
                bool,
                "debug_stack",
                "Dump stack after each bytecode",
            ) orelse false,
            .current_instruction = b.option(
                bool,
                "debug_current_instruction",
                "Dump stack after each bytecode",
            ) orelse false,
            .perf = b.option(
                bool,
                "show_perf",
                "Show performance information",
            ) orelse false,
            .stop_on_report = b.option(
                bool,
                "stop_on_report",
                "Stop compilation whenever an error is encountered",
            ) orelse false,
            .placeholders = b.option(
                bool,
                "debug_placeholders",
                "Stop compilation whenever an error is encountered",
            ) orelse false,
        },
        .gc = .{
            .debug = b.option(
                bool,
                "gc_debug",
                "Show debug information for the garbage collector",
            ) orelse false,
            .debug_light = b.option(
                bool,
                "gc_debug_light",
                "Show lighter debug information for the garbage collector",
            ) orelse false,
            .debug_access = b.option(
                bool,
                "gc_debug_access",
                "Track objects access",
            ) orelse false,
            .on = b.option(
                bool,
                "gc",
                "Turn on garbage collector",
            ) orelse true,
            .initial_gc = b.option(
                usize,
                "initial_gc",
                "In Kb, threshold at which the first garbage collector pass will occur",
            ) orelse if (builtin.mode == .Debug) 1 else 8,
            .next_gc_ratio = b.option(
                usize,
                "next_gc_ratio",
                "Ratio applied to get the next GC threshold",
            ) orelse 2,
            .next_full_gc_ratio = b.option(
                usize,
                "next_full_gc_ratio",
                "Ratio applied to get the next full GC threshold",
            ) orelse 4,
        },
        .jit = .{
            .debug = b.option(
                bool,
                "jit_debug",
                "Show debug information for the JIT engine",
            ) orelse false,
            .always_on = b.option(
                bool,
                "jit_always_on",
                "JIT engine will compile any function encountered",
            ) orelse false,
            .on = b.option(
                bool,
                "jit",
                "Turn on JIT engine",
            ) orelse true,
            .prof_threshold = b.option(
                f128,
                "jit_prof_threshold",
                "Threshold to determine if a function is hot. If the numbers of calls to it makes this percentage of all calls, it's considered hot and will be JIT compiled.",
            ) orelse 0.05,
        },
    };

    var sys_libs = std.ArrayList([]const u8).init(b.allocator);
    defer sys_libs.deinit();
    var includes = std.ArrayList([]const u8).init(b.allocator);
    defer includes.deinit();
    var llibs = std.ArrayList([]const u8).init(b.allocator);
    defer llibs.deinit();

    sys_libs.appendSlice(
        &[_][]const u8{
            "mir",
        },
    ) catch unreachable;

    includes.appendSlice(&[_][]const u8{
        "/usr/local/include",
        "/usr/include",
        "./vendors/mir",
        "./vendors/mimalloc/include",
    }) catch unreachable;

    llibs.appendSlice(&[_][]const u8{
        "/usr/local/lib",
        "/usr/lib",
        "./vendors/mir",
    }) catch unreachable;

    const lib_pcre2 = try buildPcre2(b, target, build_mode);
    const lib_mimalloc = if (build_options.mimalloc) try buildMimalloc(b, target, build_mode) else null;

    // If macOS, add homebrew paths
    if (builtin.os.tag == .macos) {
        const result: ?std.ChildProcess.ExecResult = std.ChildProcess.exec(
            .{
                .allocator = b.allocator,
                .argv = &[_][]const u8{ "brew", "--prefix" },
            },
        ) catch null;

        const prefix = if (result) |r|
            std.mem.trim(u8, r.stdout, "\n")
        else
            std.os.getenv("HOMEBREW_PREFIX") orelse "/opt/homebrew";

        var include = std.ArrayList(u8).init(b.allocator);
        include.writer().print("{s}{s}include", .{ prefix, std.fs.path.sep_str }) catch unreachable;

        var lib = std.ArrayList(u8).init(b.allocator);
        lib.writer().print("{s}{s}lib", .{ prefix, std.fs.path.sep_str }) catch unreachable;

        includes.append(include.items) catch unreachable;
        llibs.append(lib.items) catch unreachable;
    }

    var exe = b.addExecutable(.{
        .name = "buzz",
        .root_source_file = Build.FileSource.relative("src/main.zig"),
        .target = target,
        .optimize = build_mode,
    });
    b.installArtifact(exe);

    const run_exe = b.addRunArtifact(exe);
    run_exe.step.dependOn(install_step);
    if (b.args) |args| {
        run_exe.addArgs(args);
    }
    b.step("run", "run buzz").dependOn(&run_exe.step);

    for (includes.items) |include| {
        exe.addIncludePath(.{ .path = include });
    }
    for (llibs.items) |lib| {
        exe.addLibraryPath(.{ .path = lib });
    }
    for (sys_libs.items) |slib| {
        // FIXME: if mir is linked as static library (libmir.a), here also need to link libc
        // it's better to built it with Zig's build system
        exe.linkSystemLibrary(slib);
    }
    if (build_options.needLibC()) {
        exe.linkLibC();
    }
    exe.main_mod_path = .{ .path = "." };

    exe.addOptions("build_options", build_options.step(b));

    var lib = b.addSharedLibrary(.{
        .name = "buzz",
        .root_source_file = Build.FileSource.relative("src/buzz_api.zig"),
        .target = target,
        .optimize = build_mode,
    });
    b.installArtifact(lib);
    for (includes.items) |include| {
        lib.addIncludePath(.{ .path = include });
    }
    for (llibs.items) |llib| {
        lib.addLibraryPath(.{ .path = llib });
    }
    for (sys_libs.items) |slib| {
        lib.linkSystemLibrary(slib);
    }
    if (build_options.needLibC()) {
        lib.linkLibC();
    }
    lib.main_mod_path = .{ .path = "src" };

    lib.addOptions("build_options", build_options.step(b));

    lib.linkLibrary(lib_pcre2);
    if (lib_mimalloc) |mimalloc| {
        lib.linkLibrary(mimalloc);
        if (lib.target.getOsTag() == .windows) {
            lib.linkSystemLibrary("bcrypt");
        }
    }
    // So that JIT compiled function can reference buzz_api
    exe.linkLibrary(lib);

    b.default_step.dependOn(&exe.step);
    b.default_step.dependOn(&lib.step);

    const lib_paths = [_][]const u8{
        "src/lib/buzz_std.zig",
        "src/lib/buzz_io.zig",
        "src/lib/buzz_gc.zig",
        "src/lib/buzz_os.zig",
        "src/lib/buzz_fs.zig",
        "src/lib/buzz_math.zig",
        "src/lib/buzz_debug.zig",
        "src/lib/buzz_buffer.zig",
        "src/lib/buzz_crypto.zig",
        "src/lib/buzz_http.zig",
        "src/lib/buzz_ffi.zig",
        "src/lib/buzz_serialize.zig",
    };
    // Zig only libs
    const lib_names = [_][]const u8{
        "std",
        "io",
        "gc",
        "os",
        "fs",
        "math",
        "debug",
        "buffer",
        "crypto",
        "http",
        "ffi",
        "serialize",
    };
    const all_lib_names = [_][]const u8{
        "std",
        "io",
        "gc",
        "os",
        "fs",
        "math",
        "debug",
        "buffer",
        "crypto",
        "http",
        "errors",
        "ffi",
        "serialize",
        "test",
    };

    // TODO: this section is slow. Modifying Buzz parser shouldn't trigger recompile of all buzz dynamic libraries

    var libs = [_]*std.build.LibExeObjStep{undefined} ** lib_names.len;
    for (lib_paths, 0..) |lib_path, index| {
        var std_lib = b.addSharedLibrary(.{
            .name = lib_names[index],
            .root_source_file = Build.FileSource.relative(lib_path),
            .target = target,
            .optimize = build_mode,
        });
        const artifact = b.addInstallArtifact(std_lib, .{});
        install_step.dependOn(&artifact.step);
        artifact.dest_dir = .{ .custom = "lib/buzz" };

        for (includes.items) |include| {
            std_lib.addIncludePath(.{ .path = include });
        }
        for (llibs.items) |llib| {
            std_lib.addLibraryPath(.{ .path = llib });
        }
        for (sys_libs.items) |slib| {
            std_lib.linkSystemLibrary(slib);
        }
        if (build_options.needLibC()) {
            std_lib.linkLibC();
        }
        std_lib.main_mod_path = .{ .path = "src" };
        std_lib.linkLibrary(lib_pcre2);
        if (lib_mimalloc) |mimalloc| {
            std_lib.linkLibrary(mimalloc);
            if (std_lib.target.getOsTag() == .windows) {
                std_lib.linkSystemLibrary("bcrypt");
            }
        }
        std_lib.linkLibrary(lib);
        std_lib.addOptions("build_options", build_options.step(b));

        // Adds `$BUZZ_PATH/lib` and `/usr/local/lib/buzz` as search path for other shared lib referenced by this one (libbuzz.dylib most of the time)
        std_lib.addRPath(
            .{
                .path = b.fmt(
                    "{s}" ++ std.fs.path.sep_str ++ "lib/buzz",
                    .{get_buzz_prefix(b)},
                ),
            },
        );
        std_lib.addRPath(.{ .path = "/usr/local/lib/buzz" });

        b.default_step.dependOn(&std_lib.step);

        libs[index] = std_lib;
    }

    for (all_lib_names) |name| {
        const step = b.addInstallLibFile(
            std.build.FileSource.relative(b.fmt("src/lib/{s}.buzz", .{name})),
            b.fmt("buzz/{s}.buzz", .{name}),
        );
        install_step.dependOn(&step.step);
    }

    const tests = b.addTest(.{
        .root_source_file = Build.FileSource.relative("src/main.zig"),
        .target = target,
        .optimize = build_mode,
    });
    for (includes.items) |include| {
        tests.addIncludePath(.{ .path = include });
    }
    for (llibs.items) |llib| {
        tests.addLibraryPath(.{ .path = llib });
    }
    for (sys_libs.items) |slib| {
        tests.linkSystemLibrary(slib);
    }
    if (build_options.needLibC()) {
        tests.linkLibC();
    }
    tests.linkLibrary(lib_pcre2);
    if (lib_mimalloc) |mimalloc| {
        tests.linkLibrary(mimalloc);
        if (tests.target.getOsTag() == .windows) {
            tests.linkSystemLibrary("bcrypt");
        }
    }
    tests.addOptions("build_options", build_options.step(b));

    const test_step = b.step("test", "Run all the tests");
    const run_tests = b.addRunArtifact(tests);
    run_tests.cwd = Build.LazyPath{ .path = "." };
    run_tests.setEnvironmentVariable("BUZZ_PATH", get_buzz_prefix(b));
    run_tests.step.dependOn(install_step); // wait for libraries to be installed
    test_step.dependOn(&run_tests.step);
}

pub fn buildPcre2(b: *Build, target: std.zig.CrossTarget, optimize: std.builtin.OptimizeMode) !*Build.Step.Compile {
    const copyFiles = b.addWriteFiles();
    copyFiles.addCopyFileToSource(
        .{ .path = "vendors/pcre2/src/config.h.generic" },
        "vendors/pcre2/src/config.h",
    );
    copyFiles.addCopyFileToSource(
        .{ .path = "vendors/pcre2/src/pcre2.h.generic" },
        "vendors/pcre2/src/pcre2.h",
    );
    copyFiles.addCopyFileToSource(
        .{ .path = "vendors/pcre2/src/pcre2_chartables.c.dist" },
        "vendors/pcre2/src/pcre2_chartables.c",
    );

    const lib = b.addStaticLibrary(.{
        .name = "pcre2",
        .target = target,
        .optimize = optimize,
    });
    lib.addIncludePath(.{ .path = "src" });
    lib.addCSourceFiles(
        .{
            .files = &.{
                "vendors/pcre2/src/pcre2_auto_possess.c",
                "vendors/pcre2/src/pcre2_compile.c",
                "vendors/pcre2/src/pcre2_config.c",
                "vendors/pcre2/src/pcre2_context.c",
                "vendors/pcre2/src/pcre2_convert.c",
                "vendors/pcre2/src/pcre2_dfa_match.c",
                "vendors/pcre2/src/pcre2_error.c",
                "vendors/pcre2/src/pcre2_extuni.c",
                "vendors/pcre2/src/pcre2_find_bracket.c",
                "vendors/pcre2/src/pcre2_maketables.c",
                "vendors/pcre2/src/pcre2_match.c",
                "vendors/pcre2/src/pcre2_match_data.c",
                "vendors/pcre2/src/pcre2_newline.c",
                "vendors/pcre2/src/pcre2_ord2utf.c",
                "vendors/pcre2/src/pcre2_pattern_info.c",
                "vendors/pcre2/src/pcre2_script_run.c",
                "vendors/pcre2/src/pcre2_serialize.c",
                "vendors/pcre2/src/pcre2_string_utils.c",
                "vendors/pcre2/src/pcre2_study.c",
                "vendors/pcre2/src/pcre2_substitute.c",
                "vendors/pcre2/src/pcre2_substring.c",
                "vendors/pcre2/src/pcre2_tables.c",
                "vendors/pcre2/src/pcre2_ucd.c",
                "vendors/pcre2/src/pcre2_valid_utf.c",
                "vendors/pcre2/src/pcre2_xclass.c",
                "vendors/pcre2/src/pcre2_chartables.c",
            },
            .flags = &.{
                "-std=c99",
                "-DHAVE_CONFIG_H",
                "-DPCRE2_CODE_UNIT_WIDTH=8",
                "-DPCRE2_STATIC",
            },
        },
    );
    lib.step.dependOn(&copyFiles.step);
    lib.installHeader("vendors/pcre2/src/pcre2.h.generic", "pcre2.h");
    lib.linkLibC();
    b.installArtifact(lib);

    return lib;
}

pub fn buildMimalloc(b: *Build, target: std.zig.CrossTarget, optimize: std.builtin.OptimizeMode) !*Build.Step.Compile {
    const lib = b.addStaticLibrary(
        .{
            .name = "mimalloc",
            .target = target,
            .optimize = optimize,
        },
    );

    lib.addIncludePath(.{ .path = "./vendors/mimalloc/include" });
    lib.linkLibC();

    if (lib.target.getOsTag() == .macos) {
        var macOS_sdk_path = std.ArrayList(u8).init(b.allocator);
        try macOS_sdk_path.writer().print(
            "{s}/usr/include",
            .{
                (std.ChildProcess.exec(.{
                    .allocator = b.allocator,
                    .argv = &.{
                        "xcrun",
                        "--show-sdk-path",
                    },
                    .cwd = b.pathFromRoot("."),
                    .expand_arg0 = .expand,
                }) catch {
                    std.debug.print("Warning: failed to get MacOSX sdk path", .{});
                    unreachable;
                }).stdout,
            },
        );

        lib.addSystemIncludePath(.{ .path = macOS_sdk_path.items });
        // Github macos-12 runner (https://github.com/actions/runner-images/blob/main/images/macos/macos-12-Readme.md).
        lib.addSystemIncludePath(.{ .path = "/Applications/Xcode_14.0.1.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include" });
        lib.addSystemIncludePath(.{ .path = "/Library/Developer/CommandLineTools/SDKs/MacOSX14.0.sdk/usr/include" });
        lib.addSystemIncludePath(.{ .path = "/Library/Developer/CommandLineTools/SDKs/MacOSX13.3.sdk/usr/include" });
        lib.addSystemIncludePath(.{ .path = "/Library/Developer/CommandLineTools/SDKs/MacOSX12.3.sdk/usr/include" });
        lib.addSystemIncludePath(.{ .path = "/Library/Developer/CommandLineTools/SDKs/MacOSX12.1.sdk/usr/include" });
    }

    lib.addCSourceFiles(
        .{
            .files = &.{
                "./vendors/mimalloc/src/alloc-aligned.c",
                "./vendors/mimalloc/src/alloc.c",
                "./vendors/mimalloc/src/arena.c",
                "./vendors/mimalloc/src/bitmap.c",
                "./vendors/mimalloc/src/heap.c",
                "./vendors/mimalloc/src/init.c",
                "./vendors/mimalloc/src/options.c",
                "./vendors/mimalloc/src/os.c",
                "./vendors/mimalloc/src/page.c",
                "./vendors/mimalloc/src/random.c",
                "./vendors/mimalloc/src/segment-map.c",
                "./vendors/mimalloc/src/segment.c",
                "./vendors/mimalloc/src/stats.c",
                "./vendors/mimalloc/src/prim/prim.c",
            },
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

    return lib;
}
