const std = @import("std");
const builtin = @import("builtin");
const Build = std.Build;

const BuildOptions = struct {
    version: std.SemanticVersion,
    sha: []const u8,
    mimalloc: bool,
    debug: DebugOptions,
    gc: GCOptions,
    jit: JITOptions,
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

    const DebugOptions = struct {
        debug: bool,
        stack: bool,
        current_instruction: bool,
        perf: bool,
        stop_on_report: bool,
        placeholders: bool,
        type_registry: bool,

        pub fn step(self: DebugOptions, options: *Build.Step.Options) void {
            options.addOption(@TypeOf(self.debug), "debug", self.debug);
            options.addOption(@TypeOf(self.stack), "debug_stack", self.stack);
            options.addOption(@TypeOf(self.current_instruction), "debug_current_instruction", self.current_instruction);
            options.addOption(@TypeOf(self.perf), "show_perf", self.perf);
            options.addOption(@TypeOf(self.stop_on_report), "stop_on_report", self.stop_on_report);
            options.addOption(@TypeOf(self.placeholders), "debug_placeholders", self.placeholders);
            options.addOption(@TypeOf(self.type_registry), "debug_type_registry", self.type_registry);
        }
    };

    const JITOptions = struct {
        on: bool,
        always_on: bool,
        hotspot_always_on: bool,
        hotspot_on: bool,
        debug: bool,
        prof_threshold: f128 = 0.05,

        pub fn step(self: JITOptions, options: *Build.Step.Options) void {
            options.addOption(@TypeOf(self.debug), "jit_debug", self.debug);
            options.addOption(@TypeOf(self.always_on), "jit_always_on", self.always_on);
            options.addOption(@TypeOf(self.hotspot_always_on), "jit_hotspot_always_on", self.hotspot_always_on);
            options.addOption(@TypeOf(self.on), "jit", self.on);
            options.addOption(@TypeOf(self.prof_threshold), "jit_prof_threshold", self.prof_threshold);
            options.addOption(@TypeOf(self.hotspot_on), "jit_hotspot_on", self.hotspot_on);
        }
    };

    const GCOptions = struct {
        debug: bool,
        debug_light: bool,
        debug_access: bool,
        on: bool,
        initial_gc_size: usize,
        initial_gc: usize,
        next_gc_ratio: usize,
        next_full_gc_ratio: usize,
        shrink_gc_ratio: usize,
        memory_limit: ?usize,

        pub fn step(self: GCOptions, options: *Build.Step.Options) void {
            options.addOption(@TypeOf(self.debug), "gc_debug", self.debug);
            options.addOption(@TypeOf(self.debug_light), "gc_debug_light", self.debug_light);
            options.addOption(@TypeOf(self.debug_access), "gc_debug_access", self.debug_access);
            options.addOption(@TypeOf(self.on), "gc", self.on);
            options.addOption(@TypeOf(self.initial_gc_size), "initial_gc_size", self.initial_gc_size);
            options.addOption(@TypeOf(self.initial_gc), "initial_gc", self.initial_gc);
            options.addOption(@TypeOf(self.next_gc_ratio), "next_gc_ratio", self.next_gc_ratio);
            options.addOption(@TypeOf(self.next_full_gc_ratio), "next_full_gc_ratio", self.next_full_gc_ratio);
            options.addOption(@TypeOf(self.shrink_gc_ratio), "shrink_gc_ratio", self.shrink_gc_ratio);
            options.addOption(@TypeOf(self.memory_limit), "memory_limit", self.memory_limit);
        }
    };
};

pub fn build(b: *Build) !void {
    var envMap = try std.process.getEnvMap(b.allocator);
    defer envMap.deinit();

    // Check minimum zig version
    const current_zig = builtin.zig_version;
    const min_zig = std.SemanticVersion.parse("0.15.1") catch return;
    if (current_zig.order(min_zig).compare(.lt)) {
        @panic(b.fmt("Your Zig version v{f} does not meet the minimum build requirement of v{f}", .{ current_zig, min_zig }));
    }

    const build_mode = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const is_wasm = target.result.cpu.arch.isWasm();
    const install_step = b.getInstallStep();

    var build_options = BuildOptions{
        .target = target,
        .version = std.SemanticVersion{ .major = 0, .minor = 6, .patch = 0 },
        // Current commit sha
        .sha = envMap.get("GIT_SHA") orelse
            envMap.get("GITHUB_SHA") orelse std.mem.trim(
            u8,
            b.run(
                &.{
                    "git",
                    "rev-parse",
                    "--short",
                    "HEAD",
                },
            ),
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
                "Debug placeholders resolution",
            ) orelse false,
            .type_registry = b.option(
                bool,
                "debug_type_registry",
                "Debug type_registry",
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

            .initial_gc_size = b.option(
                usize,
                "initial_gc_size",
                "In Kb, how much memory is initially allocated for a buzz script",
            ) orelse 2048,
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
            .shrink_gc_ratio = b.option(
                usize,
                "shrink_gc_ratio",
                "Percentage under which the used memory will be shrinked",
            ) orelse 20,
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
            .hotspot_on = !is_wasm and b.option(
                bool,
                "jit_hotspot_on",
                "JIT engine will compile hotspot when threshold reached",
            ) orelse true,
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

    const lib_pcre2 = if (!is_wasm)
        try buildPcre2(b, target, build_mode)
    else
        null;
    const lib_mimalloc = if (build_options.mimalloc)
        try buildMimalloc(b, target, build_mode)
    else
        null;
    const lib_linenoise = if (!is_wasm and target.result.os.tag != .windows)
        try buildLinenoise(b, target, build_mode)
    else
        null;
    const lib_mir = if (!is_wasm)
        try buildMir(b, target, build_mode)
    else
        null;

    var exe = b.addExecutable(
        .{
            .name = "buzz",
            .use_llvm = true,
            .root_module = b.createModule(
                .{
                    .target = target,
                    .optimize = build_mode,
                    .root_source_file = b.path("src/main.zig"),
                },
            ),
        },
    );
    b.installArtifact(exe);

    const behavior_exe = if (!is_wasm) b.addExecutable(
        .{
            .name = "buzz_behavior",
            .use_llvm = true,
            .root_module = b.createModule(
                .{
                    .root_source_file = b.path("src/behavior.zig"),
                    .target = target,
                    .optimize = build_mode,
                },
            ),
        },
    ) else null;
    if (behavior_exe) |bexe| b.installArtifact(bexe);

    if (behavior_exe) |bexe| {
        const run_behavior = b.addRunArtifact(bexe);
        run_behavior.step.dependOn(install_step);
        b.step("test-behavior", "Test behavior").dependOn(&run_behavior.step);
    }

    const clap = b.dependency(
        "clap",
        .{
            .target = target,
            .optimize = build_mode,
        },
    );

    exe.root_module.addImport(
        "clap",
        clap.module("clap"),
    );

    var lsp_exe = if (!is_wasm)
        b.addExecutable(
            .{
                .name = "buzz_lsp",
                .use_llvm = true,
                .root_module = b.createModule(
                    .{
                        .root_source_file = b.path("src/lsp.zig"),
                        .target = target,
                        .optimize = build_mode,
                    },
                ),
            },
        )
    else
        null;

    if (!is_wasm) {
        b.installArtifact(lsp_exe.?);

        lsp_exe.?.root_module.addImport(
            "clap",
            clap.module("clap"),
        );

        const lsp = b.dependency(
            "lsp_kit",
            .{
                .target = target,
                .optimize = build_mode,
            },
        );

        lsp_exe.?.root_module.addImport(
            "lsp",
            lsp.module("lsp"),
        );
    }

    var exe_check = b.addExecutable(
        .{
            .name = "buzz",
            .use_llvm = true,
            .root_module = b.createModule(
                .{
                    .root_source_file = b.path("src/main.zig"),
                    .target = target,
                    .optimize = build_mode,
                },
            ),
        },
    );

    exe.root_module.sanitize_c = .off;
    if (behavior_exe) |bexe| bexe.root_module.sanitize_c = .off;
    if (!is_wasm) lsp_exe.?.root_module.sanitize_c = .off;

    const check = b.step("check", "Check if buzz compiles");
    check.dependOn(&exe_check.step);

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

    if (!is_wasm) {
        const run_lsp_exe = b.addRunArtifact(lsp_exe.?);
        run_lsp_exe.step.dependOn(install_step);
        if (b.args) |args| {
            run_lsp_exe.addArgs(args);
        }
        b.step("lsp", "run buzz lsp").dependOn(&run_lsp_exe.step);
    }

    if (build_options.needLibC()) {
        exe.linkLibC();
        exe_check.linkLibC();
        if (behavior_exe) |bexe| bexe.linkLibC();
        if (!is_wasm) lsp_exe.?.linkLibC();
    }

    exe.root_module.addImport("build_options", build_option_module);
    exe_check.root_module.addImport("build_options", build_option_module);
    if (behavior_exe) |bexe| bexe.root_module.addImport("build_options", build_option_module);
    if (!is_wasm) lsp_exe.?.root_module.addImport("build_options", build_option_module);

    if (!is_wasm) {
        // Building buzz api library
        var lib = b.addLibrary(
            .{
                .name = "buzz",
                .linkage = .dynamic,
                .use_llvm = true,
                .root_module = b.createModule(
                    .{
                        .root_source_file = b.path("src/buzz_api.zig"),
                        .target = target,
                        .optimize = build_mode,
                    },
                ),
            },
        );

        b.installArtifact(lib);

        if (build_options.needLibC()) {
            lib.linkLibC();
        }

        lib.root_module.addImport(
            "build_options",
            build_option_module,
        );

        if (lib_pcre2) |pcre| {
            lib.linkLibrary(pcre);
            exe.linkLibrary(pcre);
            if (behavior_exe) |bexe| bexe.linkLibrary(pcre);
            if (!is_wasm) lsp_exe.?.linkLibrary(pcre);
        }

        if (lib_mimalloc) |mimalloc| {
            lib.linkLibrary(mimalloc);
            exe.linkLibrary(mimalloc);
            if (behavior_exe) |bexe| bexe.linkLibrary(mimalloc);
            if (!is_wasm) lsp_exe.?.linkLibrary(mimalloc);
            if (lib.root_module.resolved_target.?.result.os.tag == .windows) {
                lib.linkSystemLibrary("bcrypt");
                exe.linkSystemLibrary("bcrypt");
                if (behavior_exe) |bexe| bexe.linkSystemLibrary("bcrypt");
                if (!is_wasm) lsp_exe.?.linkSystemLibrary("bcrypt");
            }
        }

        if (lib_mir) |mir| {
            lib.linkLibrary(mir);
            exe.linkLibrary(mir);
            if (behavior_exe) |bexe| bexe.linkLibrary(mir);
            if (!is_wasm) lsp_exe.?.linkLibrary(mir);
        }

        // So that JIT compiled function can reference buzz_api
        exe.linkLibrary(lib);
        if (behavior_exe) |bexe| bexe.linkLibrary(lib);
        if (!is_wasm) lsp_exe.?.linkLibrary(lib);
        exe_check.linkLibrary(lib);
        if (lib_linenoise) |ln| {
            exe.linkLibrary(ln);
            if (behavior_exe) |bexe| bexe.linkLibrary(ln);
            if (!is_wasm) lsp_exe.?.linkLibrary(ln);
            exe_check.linkLibrary(ln);
        }

        b.default_step.dependOn(&exe.step);
        if (!is_wasm) b.default_step.dependOn(&lsp_exe.?.step);
        b.default_step.dependOn(&lib.step);

        // Building std libraries
        const Lib = struct {
            path: ?[]const u8 = null,
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
            // FIXME: API has changed
            // .{ .name = "http", .path = "src/lib/buzz_http.zig", .wasm_compatible = false },
            .{ .name = "ffi", .path = "src/lib/buzz_ffi.zig", .wasm_compatible = false },
            .{ .name = "serialize", .path = "src/lib/buzz_serialize.zig" },
            .{ .name = "testing" },
            .{ .name = "errors" },
        };

        var library_steps = std.ArrayList(*std.Build.Step.Compile){};
        for (libraries) |library| {
            // Copy buzz definitions
            const step = b.addInstallLibFile(
                b.path(
                    b.fmt(
                        "src/lib/{s}.buzz",
                        .{library.name},
                    ),
                ),
                b.fmt(
                    "buzz/{s}.buzz",
                    .{library.name},
                ),
            );
            install_step.dependOn(&step.step);

            if (library.path == null or (!library.wasm_compatible and is_wasm)) {
                continue;
            }

            var std_lib = b.addLibrary(
                .{
                    .name = library.name,
                    .linkage = .dynamic,
                    .use_llvm = true,
                    .root_module = b.createModule(
                        .{
                            .root_source_file = b.path(library.path.?),
                            .target = target,
                            .optimize = build_mode,
                        },
                    ),
                },
            );

            const artifact = b.addInstallArtifact(std_lib, .{});
            install_step.dependOn(&artifact.step);
            artifact.dest_dir = .{ .custom = "lib/buzz" };

            // No need to link anything when building for wasm since everything is static
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

            if (lib_mir) |mir| {
                std_lib.linkLibrary(mir);
            }

            std_lib.linkLibrary(lib);
            std_lib.root_module.addImport("build_options", build_option_module);

            b.default_step.dependOn(&std_lib.step);

            library_steps.append(b.allocator, std_lib) catch unreachable;
        }
    }

    const tests = b.addTest(
        .{
            .use_llvm = true,
            .root_module = b.createModule(
                .{
                    .root_source_file = b.path("src/main.zig"),
                    .target = target,
                    .optimize = build_mode,
                },
            ),
        },
    );
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
    if (lib_mir) |mir| {
        tests.linkLibrary(mir);
    }
    tests.root_module.addImport("build_options", build_option_module);

    const test_step = b.step("test", "Run all the tests");
    const run_tests = b.addRunArtifact(tests);
    run_tests.cwd = b.path(".");
    run_tests.setEnvironmentVariable("BUZZ_PATH", envMap.get("BUZZ_PATH") orelse std.fs.path.dirname(b.exe_dir).?);
    run_tests.step.dependOn(install_step); // wait for libraries to be installed
    test_step.dependOn(&run_tests.step);
}

pub fn buildPcre2(b: *Build, target: Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) !*Build.Step.Compile {
    const copyFiles = b.addWriteFiles();
    _ = copyFiles.addCopyFile(
        b.path("vendors/pcre2/src/config.h.generic"),
        "vendors/pcre2/src/config.h",
    );
    _ = copyFiles.addCopyFile(
        b.path("vendors/pcre2/src/pcre2.h.generic"),
        "vendors/pcre2/src/pcre2.h",
    );
    _ = copyFiles.addCopyFile(
        b.path("vendors/pcre2/src/pcre2_chartables.c.dist"),
        "vendors/pcre2/src/pcre2_chartables.c",
    );

    const flags: []const []const u8 = &.{
        "-std=c99",
        "-DHAVE_CONFIG_H",
        "-DPCRE2_CODE_UNIT_WIDTH=8",
        "-DPCRE2_STATIC",
    };
    const lib = b.addLibrary(
        .{
            .linkage = .static,
            .name = "pcre2",
            .root_module = b.createModule(
                .{
                    .target = target,
                    .optimize = optimize,
                },
            ),
        },
    );
    lib.addIncludePath(b.path("vendors/pcre2/src"));
    lib.addIncludePath(copyFiles.getDirectory().path(b, "vendors/pcre2/src"));
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
            },
            .flags = flags,
        },
    );
    lib.addCSourceFile(
        .{
            .file = copyFiles.getDirectory().path(b, "vendors/pcre2/src/pcre2_chartables.c"),
            .flags = flags,
        },
    );
    lib.step.dependOn(&copyFiles.step);
    lib.linkLibC();
    b.installArtifact(lib);

    return lib;
}

pub fn buildMimalloc(b: *Build, target: Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) !*Build.Step.Compile {
    const lib = b.addLibrary(
        .{
            .name = "mimalloc",
            .linkage = .static,
            .use_llvm = true,
            .root_module = b.createModule(
                .{
                    .target = target,
                    .optimize = optimize,
                },
            ),
        },
    );

    lib.addIncludePath(b.path("./vendors/mimalloc/include"));
    lib.linkLibC();

    lib.addCSourceFiles(
        .{
            .files = &.{
                "./vendors/mimalloc/src/alloc.c",
                "./vendors/mimalloc/src/alloc-aligned.c",
                "./vendors/mimalloc/src/alloc-posix.c",
                "./vendors/mimalloc/src/arena.c",
                "./vendors/mimalloc/src/arena-meta.c",
                "./vendors/mimalloc/src/bitmap.c",
                "./vendors/mimalloc/src/heap.c",
                "./vendors/mimalloc/src/init.c",
                "./vendors/mimalloc/src/libc.c",
                "./vendors/mimalloc/src/options.c",
                "./vendors/mimalloc/src/os.c",
                "./vendors/mimalloc/src/page.c",
                "./vendors/mimalloc/src/page-map.c",
                "./vendors/mimalloc/src/random.c",
                "./vendors/mimalloc/src/stats.c",
                "./vendors/mimalloc/src/prim/prim.c",
            },
            .flags = if (lib.root_module.optimize != .Debug)
                &.{
                    "-DNDEBUG=1",
                    "-DMI_SECURE=0",
                    "-DMI_STAT=0",
                    "-DMI_SHOW_ERRORS=1",
                    "-fno-sanitize=undefined",
                    "-Wno-date-time",
                    // We should not need this, but io\File.readAll makes the exit collect fail
                    "-D=MI_SKIP_COLLECT_ON_EXIT=1",
                }
            else
                &.{
                    "-fno-sanitize=undefined",
                    "-Wno-date-time",
                    // We should not need this, but io\File.readAll makes the exit collect fail
                    "-D=MI_SKIP_COLLECT_ON_EXIT=1",
                },
        },
    );

    return lib;
}

pub fn buildLinenoise(b: *Build, target: Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) !*Build.Step.Compile {
    const lib = b.addLibrary(
        .{
            .name = "linenoise",
            .linkage = .static,
            .use_llvm = true,
            .root_module = b.createModule(
                .{
                    .target = target,
                    .optimize = optimize,
                },
            ),
        },
    );

    lib.addIncludePath(b.path("vendors/linenoise"));
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
        b.path("src/repl.html"),
        "repl.html",
    );
    b.getInstallStep().dependOn(&copyRepl.step);
}

pub fn buildMir(b: *Build, target: Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) !*Build.Step.Compile {
    const lib = b.addLibrary(
        .{
            .name = "mir",
            .linkage = .static,
            .use_llvm = true,
            .root_module = b.createModule(
                .{
                    .target = target,
                    .optimize = optimize,
                },
            ),
        },
    );

    lib.addIncludePath(b.path("./vendors/mir"));

    lib.linkLibC();

    lib.addCSourceFiles(
        .{
            .files = &.{
                "./vendors/mir/mir.c",
                "./vendors/mir/mir-gen.c",
                "./vendors/mir/c2mir/c2mir.c",
            },
            .flags = &.{
                "-fsigned-char",
                "-O3",
                "-DNDEBUG=1",
                "-DMIR_PARALLEL_GEN=1",
                "-fno-sanitize=undefined", // MIR has some undefined behaviour somewhere so we need this
            },
        },
    );

    if (target.result.os.tag == .windows) {
        lib.linkSystemLibrary("kernel32");
        lib.linkSystemLibrary("psapi");
    }

    return lib;
}
