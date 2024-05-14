const std = @import("std");
const builtin = @import("builtin");
const Build = std.Build;

const BuzzDebugOptions = struct {
    debug: bool,
    stack: bool,
    current_instruction: bool,
    perf: bool,
    stop_on_report: bool,
    placeholders: bool,

    pub fn step(self: BuzzDebugOptions, options: *Build.Step.Options) void {
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
    hotspot_always_on: bool,
    debug: bool,
    prof_threshold: f128 = 0.05,

    pub fn step(self: BuzzJITOptions, options: *Build.Step.Options) void {
        options.addOption(@TypeOf(self.debug), "jit_debug", self.debug);
        options.addOption(@TypeOf(self.always_on), "jit_always_on", self.always_on);
        options.addOption(@TypeOf(self.hotspot_always_on), "jit_hotspot_always_on", self.hotspot_always_on);
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
    memory_limit: ?usize,

    pub fn step(self: BuzzGCOptions, options: *Build.Step.Options) void {
        options.addOption(@TypeOf(self.debug), "gc_debug", self.debug);
        options.addOption(@TypeOf(self.debug_light), "gc_debug_light", self.debug_light);
        options.addOption(@TypeOf(self.debug_access), "gc_debug_access", self.debug_access);
        options.addOption(@TypeOf(self.on), "gc", self.on);
        options.addOption(@TypeOf(self.initial_gc), "initial_gc", self.initial_gc);
        options.addOption(@TypeOf(self.next_gc_ratio), "next_gc_ratio", self.next_gc_ratio);
        options.addOption(@TypeOf(self.next_full_gc_ratio), "next_full_gc_ratio", self.next_full_gc_ratio);
        options.addOption(@TypeOf(self.memory_limit), "memory_limit", self.memory_limit);
    }
};

const BuzzBuildOptions = struct {
    version: std.SemanticVersion,
    sha: []const u8,
    mimalloc: bool,
    debug: BuzzDebugOptions,
    gc: BuzzGCOptions,
    jit: BuzzJITOptions,
    target: Build.ResolvedTarget,
    cycle_limit: ?u128,
    recursive_call_limit: ?u32,
    stack_size: usize = 100_000,

    pub fn step(self: @This(), b: *Build) *Build.Module {
        var options = b.addOptions();
        options.addOption(@TypeOf(self.version), "version", self.version);
        options.addOption(@TypeOf(self.sha), "sha", self.sha);
        options.addOption(@TypeOf(self.mimalloc), "mimalloc", self.mimalloc);
        options.addOption(@TypeOf(self.cycle_limit), "cycle_limit", self.cycle_limit);
        options.addOption(@TypeOf(self.recursive_call_limit), "recursive_call_limit", self.recursive_call_limit);
        options.addOption(@TypeOf(self.stack_size), "stack_size", self.stack_size);

        self.debug.step(options);
        self.gc.step(options);
        self.jit.step(options);

        return options.createModule();
    }

    pub fn needLibC(self: @This()) bool {
        return !self.target.result.cpu.arch.isWasm();
    }
};

fn getBuzzPrefix(b: *Build) ![]const u8 {
    return std.posix.getenv("BUZZ_PATH") orelse std.fs.path.dirname(b.exe_dir).?;
}

pub fn build(b: *Build) !void {
    // Check minimum zig version
    const current_zig = builtin.zig_version;
    const min_zig = std.SemanticVersion.parse("0.13.0-dev.73+db890dbae") catch return;
    if (current_zig.order(min_zig).compare(.lt)) {
        @panic(b.fmt("Your Zig version v{} does not meet the minimum build requirement of v{}", .{ current_zig, min_zig }));
    }

    const build_mode = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const is_wasm = target.result.cpu.arch.isWasm();
    const install_step = b.getInstallStep();

    var build_options = BuzzBuildOptions{
        .target = target,
        // Version is latest tag or empty string
        .version = std.SemanticVersion{ .major = 0, .minor = 4, .patch = 0 },
        // Current commit sha
        .sha = std.posix.getenv("GIT_SHA") orelse
            std.posix.getenv("GITHUB_SHA") orelse std.mem.trim(
            u8,
            (std.ChildProcess.run(.{
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
        .cycle_limit = b.option(
            usize,
            "cycle_limit",
            "Amount of bytecode (x 1000) the script is allowed to run (WARNING: this disables JIT compilation)",
        ) orelse null,
        .recursive_call_limit = b.option(
            u32,
            "recursive_call_limit",
            "Maximum depth for recursive calls",
        ),
        .stack_size = b.option(
            usize,
            "stack_size",
            "Stack maximum size",
        ) orelse 100_000,
        .mimalloc = !is_wasm and b.option(
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
            .perf = !is_wasm and b.option(
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
            .memory_limit = b.option(
                usize,
                "memory_limit",
                "Memory limit in bytes",
            ) orelse null,
        },
        .jit = .{
            .debug = b.option(
                bool,
                "jit_debug",
                "Show debug information for the JIT engine",
            ) orelse false,
            .always_on = !is_wasm and b.option(
                bool,
                "jit_always_on",
                "JIT engine will compile any function encountered",
            ) orelse false,
            .hotspot_always_on = !is_wasm and b.option(
                bool,
                "jit_hotspot_always_on",
                "JIT engine will compile any hotspot encountered",
            ) orelse false,
            .on = !is_wasm and b.option(
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

    const build_option_module = build_options.step(b);

    var sys_libs = std.ArrayList([]const u8).init(b.allocator);
    defer sys_libs.deinit();
    var includes = std.ArrayList([]const u8).init(b.allocator);
    defer includes.deinit();
    var llibs = std.ArrayList([]const u8).init(b.allocator);
    defer llibs.deinit();

    if (!is_wasm) {
        sys_libs.appendSlice(
            &[_][]const u8{
                "mir",
            },
        ) catch unreachable;
    }

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

    const lib_pcre2 = if (!is_wasm)
        try buildPcre2(b, target, build_mode)
    else
        null;
    const lib_mimalloc = if (build_options.mimalloc)
        try buildMimalloc(b, target, build_mode)
    else
        null;
    const lib_linenoise = if (!is_wasm)
        try buildLinenoise(b, target, build_mode)
    else
        null;

    // If macOS, add homebrew paths
    if (builtin.os.tag == .macos) {
        const result = std.ChildProcess.run(
            .{
                .allocator = b.allocator,
                .argv = &[_][]const u8{ "brew", "--prefix" },
            },
        ) catch null;

        const prefix = if (result) |r|
            std.mem.trim(u8, r.stdout, "\n")
        else
            std.posix.getenv("HOMEBREW_PREFIX") orelse "/opt/homebrew";

        var include = std.ArrayList(u8).init(b.allocator);
        include.writer().print("{s}{s}include", .{ prefix, std.fs.path.sep_str }) catch unreachable;

        var lib = std.ArrayList(u8).init(b.allocator);
        lib.writer().print("{s}{s}lib", .{ prefix, std.fs.path.sep_str }) catch unreachable;

        includes.append(include.items) catch unreachable;
        llibs.append(lib.items) catch unreachable;
    }

    var exe = b.addExecutable(.{
        .name = "buzz",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = build_mode,
    });
    b.installArtifact(exe);

    if (is_wasm) {
        exe.global_base = 6560;
        exe.entry = .disabled;
        exe.rdynamic = true;
        exe.import_memory = true;
        exe.stack_size = std.wasm.page_size;

        exe.initial_memory = std.wasm.page_size * 100;
        exe.max_memory = std.wasm.page_size * 1000;
    }

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

    exe.root_module.addImport("build_options", build_option_module);

    if (!is_wasm) {
        // Building buzz api library
        var lib = b.addSharedLibrary(.{
            .name = "buzz",
            .root_source_file = .{ .path = "src/buzz_api.zig" },
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

        lib.root_module.addImport(
            "build_options",
            build_option_module,
        );

        if (lib_pcre2) |pcre| {
            lib.linkLibrary(pcre);
        }
        if (lib_mimalloc) |mimalloc| {
            lib.linkLibrary(mimalloc);
            if (lib.root_module.resolved_target.?.result.os.tag == .windows) {
                lib.linkSystemLibrary("bcrypt");
            }
        }
        // So that JIT compiled function can reference buzz_api
        exe.linkLibrary(lib);
        if (lib_linenoise) |ln| {
            exe.linkLibrary(ln);
        }

        b.default_step.dependOn(&exe.step);
        b.default_step.dependOn(&lib.step);

        // Building std libraries
        const Lib = struct {
            path: ?[]const u8,
            name: []const u8,
            wasm_compatible: bool = true,
        };

        const libraries = [_]Lib{
            .{ .name = "std", .path = "src/lib/buzz_std.zig" },
            .{ .name = "io", .path = "src/lib/buzz_io.zig", .wasm_compatible = false },
            .{ .name = "gc", .path = "src/lib/buzz_gc.zig" },
            .{ .name = "os", .path = "src/lib/buzz_os.zig", .wasm_compatible = false },
            .{ .name = "fs", .path = "src/lib/buzz_fs.zig", .wasm_compatible = false },
            .{ .name = "math", .path = "src/lib/buzz_math.zig" },
            .{ .name = "debug", .path = "src/lib/buzz_debug.zig" },
            .{ .name = "buffer", .path = "src/lib/buzz_buffer.zig" },
            .{ .name = "crypto", .path = "src/lib/buzz_crypto.zig" },
            .{ .name = "http", .path = "src/lib/buzz_http.zig", .wasm_compatible = false },
            .{ .name = "ffi", .path = "src/lib/buzz_ffi.zig", .wasm_compatible = false },
            .{ .name = "serialize", .path = "src/lib/buzz_serialize.zig" },
            .{ .name = "testing", .path = null },
            .{ .name = "errors", .path = null },
        };

        var library_steps = std.ArrayList(*std.Build.Step.Compile).init(b.allocator);
        for (libraries) |library| {
            // Copy buzz definitions
            const step = b.addInstallLibFile(
                .{ .path = b.fmt("src/lib/{s}.buzz", .{library.name}) },
                b.fmt("buzz/{s}.buzz", .{library.name}),
            );
            install_step.dependOn(&step.step);

            if (library.path == null or (!library.wasm_compatible and is_wasm)) {
                continue;
            }

            var std_lib = b.addSharedLibrary(.{
                .name = library.name,
                .root_source_file = .{ .path = library.path.? },
                .target = target,
                .optimize = build_mode,
            });

            const artifact = b.addInstallArtifact(std_lib, .{});
            install_step.dependOn(&artifact.step);
            artifact.dest_dir = .{ .custom = "lib/buzz" };

            // No need to link anything when building for wasm since everything is static
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

            if (lib_pcre2) |pcre| {
                std_lib.linkLibrary(pcre);
            }

            if (lib_mimalloc) |mimalloc| {
                std_lib.linkLibrary(mimalloc);
                if (std_lib.root_module.resolved_target.?.result.os.tag == .windows) {
                    std_lib.linkSystemLibrary("bcrypt");
                }
            }
            std_lib.linkLibrary(lib);
            std_lib.root_module.addImport("build_options", build_option_module);

            // Adds `$BUZZ_PATH/lib` and `/usr/local/lib/buzz` as search path for other shared lib referenced by this one (libbuzz.dylib most of the time)
            std_lib.addRPath(
                .{
                    .path = b.fmt(
                        "{s}" ++ std.fs.path.sep_str ++ "lib/buzz",
                        .{try getBuzzPrefix(b)},
                    ),
                },
            );
            std_lib.addRPath(.{ .path = "/usr/local/lib/buzz" });

            b.default_step.dependOn(&std_lib.step);

            library_steps.append(std_lib) catch unreachable;
        }
    }

    const tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
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
    if (lib_pcre2) |pcre| {
        tests.linkLibrary(pcre);
    }
    if (lib_mimalloc) |mimalloc| {
        tests.linkLibrary(mimalloc);
        if (tests.root_module.resolved_target.?.result.os.tag == .windows) {
            tests.linkSystemLibrary("bcrypt");
        }
    }
    tests.root_module.addImport("build_options", build_option_module);

    const test_step = b.step("test", "Run all the tests");
    const run_tests = b.addRunArtifact(tests);
    run_tests.cwd = Build.LazyPath{ .path = "." };
    run_tests.setEnvironmentVariable("BUZZ_PATH", try getBuzzPrefix(b));
    run_tests.step.dependOn(install_step); // wait for libraries to be installed
    test_step.dependOn(&run_tests.step);

    if (is_wasm) {
        buildWasmReplDemo(b, exe);
    }
}

pub fn buildPcre2(b: *Build, target: Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) !*Build.Step.Compile {
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
    lib.linkLibC();
    b.installArtifact(lib);

    return lib;
}

pub fn buildMimalloc(b: *Build, target: Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) !*Build.Step.Compile {
    const lib = b.addStaticLibrary(
        .{
            .name = "mimalloc",
            .target = target,
            .optimize = optimize,
        },
    );

    lib.addIncludePath(.{ .path = "./vendors/mimalloc/include" });
    lib.linkLibC();

    if (lib.root_module.resolved_target.?.result.os.tag == .macos) {
        var macOS_sdk_path = std.ArrayList(u8).init(b.allocator);
        try macOS_sdk_path.writer().print(
            "{s}/usr/include",
            .{
                (std.ChildProcess.run(.{
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
            .flags = if (lib.root_module.optimize != .Debug)
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

pub fn buildLinenoise(b: *Build, target: Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) !*Build.Step.Compile {
    const lib = b.addStaticLibrary(.{
        .name = "linenoise",
        .target = target,
        .optimize = optimize,
    });

    lib.addIncludePath(.{ .path = "vendors/linenoise" });
    lib.addCSourceFiles(
        .{
            .files = &.{
                "vendors/linenoise/linenoise.c",
            },
            .flags = &.{
                "-Os",
            },
        },
    );
    lib.linkLibC();
    b.installArtifact(lib);

    return lib;
}

pub fn buildWasmReplDemo(b: *Build, exe: *Build.Step.Compile) void {
    const npm_install = b.addSystemCommand(
        &.{ "npm", "i" },
    );

    const esbuild = b.addSystemCommand(
        &.{
            b.pathFromRoot("node_modules/.bin/esbuild"),
            b.pathFromRoot("src/wasm.ts"),
            "--bundle",
            "--format=esm",
            // This replaces all occurrences of '__WASM_ARTIFACT_FILENAME' in TypeScript source files
            // with the filename of the compiled WebAssembly artifact.
            b.fmt(
                "--define:__WASM_ARTIFACT_FILENAME=\"{s}\"",
                .{exe.out_filename},
            ),
            // Output compiled/bundled files into zig-out/bin
            b.fmt(
                "--outdir={s}",
                .{
                    b.getInstallPath(.bin, ""),
                },
            ),
        },
    );

    b.getInstallStep().dependOn(&npm_install.step);
    b.getInstallStep().dependOn(&esbuild.step);

    const copyRepl = b.addInstallBinFile(
        .{ .path = "src/repl.html" },
        "repl.html",
    );
    b.getInstallStep().dependOn(&copyRepl.step);
}
