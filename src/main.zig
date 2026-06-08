const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const _vm = @import("vm.zig");
const VM = _vm.VM;
const Init = _vm.Init;
const RunFlavor = _vm.RunFlavor;
const ImportRegistry = _vm.ImportRegistry;
const Parser = @import("Parser.zig");
const CodeGen = @import("Codegen.zig");
const _obj = @import("obj.zig");
const ObjString = _obj.ObjString;
const ObjTypeDef = _obj.ObjTypeDef;
const TypeRegistry = @import("TypeRegistry.zig");
const Ast = @import("Ast.zig");
const BuildOptions = @import("build_options");
const clap = @import("clap");
const GC = @import("GC.zig");
const is_wasm = builtin.cpu.arch.isWasm();
const _repl = if (!is_wasm) @import("repl.zig") else void;
const repl = if (!is_wasm) _repl.repl else void;
const wasm_repl = @import("wasm_repl.zig");
const Renderer = @import("renderer.zig").Renderer;
const io = @import("io.zig");
const Runner = @import("Runner.zig");
const Perf = @import("Perf.zig");
const Package = @import("Package.zig");

pub export const initRepl_export = wasm_repl.initRepl;
pub export const runLine_export = wasm_repl.runLine;

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

const SubCommand = enum {
    @"test",
    check,
    fetch,
    format,
    help,
    init,
    run,
    @"run-script",
    version,
};

/// Top-level command descriptions shown by `buzz help`.
const command_summaries = .{
    .check = .{ .name = "check", .description = "Parse and type-check a buzz script without running it." },
    .fetch = .{ .name = "fetch", .description = "Prepare package vendor links from a manifest." },
    .format = .{ .name = "format", .description = "Format a buzz script." },
    .help = .{ .name = "help", .description = "Show global or command-specific help." },
    .init = .{ .name = "init", .description = "Create a minimal buzz package in the current directory." },
    .run = .{ .name = "run", .description = "Run src/main.buzz from the current package." },
    .@"run-script" = .{ .name = "run-script", .description = "Run a standalone buzz script by path." },
    .@"test" = .{ .name = "test", .description = "Run tests from a buzz script." },
    .version = .{ .name = "version", .description = "Print buzz version information." },
};

const main_params = clap.parseParamsComptime(
    \\<command>
);

const test_params = clap.parseParamsComptime(
    \\-L, --library <str>... Add search path for external libraries
    \\-r, --root-dir <str>   Root dir for package resolution
    \\<str>                  Script to test
);

const check_params = clap.parseParamsComptime(
    \\-L, --library <str>... Add search path for external libraries
    \\-r, --root-dir <str>   Root dir for package resolution 
    \\<str>                  Script to check
);

const format_params = clap.parseParamsComptime(
    \\--line-width <u8>       Formatter line width (defaults to 80)
    \\-L, --library <str>...  Add search path for external libraries
    \\-r, --root-dir <str>    Root dir for package resolution
    \\<str>                   Script to format
);

const run_params = clap.parseParamsComptime(
    \\<str>...               Arguments to pass to src/main.buzz
);

const run_script_params = clap.parseParamsComptime(
    \\-L, --library <str>... Add search path for external libraries
    \\-r, --root-dir <str>   Root dir for package resolution
    \\<str>                  Script to run
    \\<str>...               Arguments to pass to the script
);

const help_params = clap.parseParamsComptime(
    \\<str>  Command for which you want help
);

const fetch_params = clap.parseParamsComptime(
    \\-m, --manifest <str>  Path to manifest file (defaults to `./manifest.buzz`)
);

const main_parsers = .{
    .command = clap.parsers.enumeration(SubCommand),
};

pub fn main(provided_init: Init) u8 {
    if (is_wasm) unreachable;

    if (BuildOptions.jit and BuildOptions.jit_always_on and BuildOptions.jit_asynchronous) {
        @compileError("jit_always_on and jit_asynchronous can't used together");
    }

    var init = provided_init;
    const allocator = if (BuildOptions.mimalloc)
        @import("mimalloc.zig").mim_allocator
    else
        init.gpa;
    // FIXME: Use process.allocator everywhere?
    init.gpa = allocator;

    var stderr = io.stderrWriter(init.io);
    var stdout = io.stdoutWriter(init.io);

    var arg_iter = try init.minimal.args.iterateAllocator(init.gpa);
    defer arg_iter.deinit();

    _ = arg_iter.next();

    var diag = clap.Diagnostic{};
    var res = clap.parseEx(
        clap.Help,
        &main_params,
        main_parsers,
        &arg_iter,
        .{
            .allocator = allocator,
            .diagnostic = &diag,
            // Stop parsing after we read the subcommand
            .terminating_positional = 0,
        },
    ) catch |err| {
        // Report useful error and exit
        diag.report(&stderr.interface, err) catch {};
        return 1;
    };
    defer res.deinit();

    // No arguments, we run the REPL
    if (res.positionals[0]) |command| {
        return switch (command) {
            .@"test" => run(
                init,
                allocator,
                command,
                clap.parseEx(
                    clap.Help,
                    &test_params,
                    clap.parsers.default,
                    &arg_iter,
                    .{
                        .allocator = allocator,
                        .diagnostic = &diag,
                    },
                ) catch |err| {
                    // Report useful error and exit
                    diag.report(&stderr.interface, err) catch {};
                    return 1;
                },
                .{},
            ),
            .check => run(
                init,
                allocator,
                command,
                clap.parseEx(
                    clap.Help,
                    &check_params,
                    clap.parsers.default,
                    &arg_iter,
                    .{
                        .allocator = allocator,
                        .diagnostic = &diag,
                    },
                ) catch |err| {
                    // Report useful error and exit
                    diag.report(&stderr.interface, err) catch {};
                    return 1;
                },
                .{},
            ),
            .format => {
                const sub_res = clap.parseEx(
                    clap.Help,
                    &format_params,
                    clap.parsers.default,
                    &arg_iter,
                    .{
                        .allocator = allocator,
                        .diagnostic = &diag,
                    },
                ) catch |err| {
                    // Report useful error and exit
                    diag.report(&stderr.interface, err) catch {};
                    return 1;
                };

                return run(
                    init,
                    allocator,
                    command,
                    sub_res,
                    renderer_options: {
                        if (sub_res.args.@"line-width") |line_width| {
                            if (line_width < Renderer.min_line_width) {
                                stderr.interface.print(
                                    "--line-width must be at least {}\n",
                                    .{Renderer.min_line_width},
                                ) catch {};
                                return 1;
                            }

                            break :renderer_options .{ .line_width = line_width };
                        } else break :renderer_options .{};
                    },
                );
            },
            .run => {
                const sub_res = clap.parseEx(
                    clap.Help,
                    &run_params,
                    clap.parsers.default,
                    &arg_iter,
                    .{
                        .allocator = allocator,
                        .diagnostic = &diag,
                    },
                ) catch |err| {
                    // Report useful error and exit
                    diag.report(&stderr.interface, err) catch {};
                    return 1;
                };

                std.Io.Dir.cwd().access(init.io, Package.MANIFEST, .{ .read = true }) catch |err| {
                    stderr.interface.print(
                        "Could not find `{s}` in current directory: {s}\n",
                        .{
                            Package.MANIFEST,
                            @errorName(err),
                        },
                    ) catch @panic("Could not check package manifest");
                    return 1;
                };

                var perf: ?Perf = if (BuildOptions.show_perf) Perf.init(init.io) else null;
                defer if (perf) |*p| p.report();

                var runner: Runner = undefined;
                runner.init(
                    init,
                    allocator,
                    .Run,
                    null,
                    if (perf) |*p| p else null,
                ) catch {
                    return 1;
                };
                defer runner.deinit();

                return runner.runFile(
                    ".",
                    "src/main.buzz",
                    sub_res.positionals[0],
                ) catch {
                    return 1;
                };
            },
            .@"run-script" => run(
                init,
                allocator,
                command,
                clap.parseEx(
                    clap.Help,
                    &run_script_params,
                    clap.parsers.default,
                    &arg_iter,
                    .{
                        .allocator = allocator,
                        .diagnostic = &diag,
                    },
                ) catch |err| {
                    // Report useful error and exit
                    diag.report(&stderr.interface, err) catch {};
                    return 1;
                },
                .{},
            ),
            .fetch => {
                const sub_res = clap.parseEx(
                    clap.Help,
                    &fetch_params,
                    clap.parsers.default,
                    &arg_iter,
                    .{
                        .allocator = allocator,
                        .diagnostic = &diag,
                    },
                ) catch |err| {
                    // Report useful error and exit
                    diag.report(&stderr.interface, err) catch {};
                    return 1;
                };

                const manifest_path = sub_res.args.manifest orelse ("./" ++ Package.MANIFEST);

                const manifest = Package.loadManifest(
                    init,
                    allocator,
                    manifest_path,
                ) catch |err| {
                    stderr.interface.print(
                        "Could not load manifest at `{s}`: {s}\n",
                        .{
                            manifest_path,
                            @errorName(err),
                        },
                    ) catch @panic("Could not load manifest");
                    return 1;
                };

                const manifest_real_path = std.Io.Dir.cwd().realPathFileAlloc(
                    init.io,
                    manifest_path,
                    allocator,
                ) catch |err| {
                    stderr.interface.print(
                        "Could not resolve manifest root at `{s}`: {s}\n",
                        .{
                            manifest_path,
                            @errorName(err),
                        },
                    ) catch @panic("Could not resolve manifest root");
                    return 1;
                };
                defer allocator.free(manifest_real_path);

                const manifest_root = std.fs.path.dirname(manifest_real_path) orelse ".";

                Package.ensureSelfVendorSymlink(
                    init,
                    manifest_root,
                    manifest.name,
                ) catch |err| {
                    stderr.interface.print(
                        "Could not create self vendor link for `{s}`: {s}\n",
                        .{
                            manifest.name,
                            @errorName(err),
                        },
                    ) catch @panic("Could not create self vendor link");
                    return 1;
                };

                if (manifest.fetch(init, manifest_root) catch |err| {
                    stderr.interface.print(
                        "Could fetch dependencies for `{s}`: {s}\n",
                        .{
                            manifest.name,
                            @errorName(err),
                        },
                    ) catch @panic("Could not create self vendor link");
                    return 1;
                }) {
                    return 0;
                }

                return 1;
            },
            .help => {
                const sub_res = clap.parseEx(
                    clap.Help,
                    &help_params,
                    clap.parsers.default,
                    &arg_iter,
                    .{
                        .allocator = allocator,
                        .diagnostic = &diag,
                    },
                ) catch |err| {
                    // Report useful error and exit
                    diag.report(&stderr.interface, err) catch {};
                    return 1;
                };

                return help(
                    init,
                    &stderr.interface,
                    sub_res.positionals[0],
                );
            },
            .init => return initPackage(init),
            .version => {
                _repl.printBanner(&stdout.interface, true);

                return 0;
            },
        };
    } else {
        repl(init, allocator) catch {
            return 1;
        };
    }

    return 0;
}

fn initPackage(init: Init) u8 {
    var stderr = io.stderrWriter(init.io);

    Package.init(init) catch |err| {
        switch (err) {
            error.ManifestAlreadyCreated => stderr.interface.print("A `manifest.buzz` file already exists\n", .{}) catch
                @panic("Could not init buzz package"),
            else => stderr.interface.print("Could not initialize buzz package: {s}\n", .{@errorName(err)}) catch
                @panic("Could not init buzz package"),
        }

        return 1;
    };

    return 0;
}

fn run(
    init: Init,
    allocator: std.mem.Allocator,
    command: SubCommand,
    sub_res: anytype,
    renderer_options: Renderer.Options,
) u8 {
    if (command == .@"run-script" and sub_res.positionals[0] == null) {
        var stderr = io.stderrWriter(init.io);
        stderr.interface.writeAll("Missing script to run\n") catch {};
        return 1;
    }

    var perf: ?Perf = if (BuildOptions.show_perf) Perf.init(init.io) else null;
    defer if (perf) |*p| p.report();

    var runner: Runner = undefined;
    runner.init(
        init,
        allocator,
        switch (command) {
            .@"test" => .Test,
            .check => .Check,
            .format => .Fmt,
            .run, .@"run-script" => .Run,
            else => unreachable,
        },
        null,
        if (perf) |*p| p else null,
    ) catch {
        return 1;
    };
    defer runner.deinit();
    runner.renderer_options = renderer_options;

    if (sub_res.args.library.len > 0) {
        var list = std.ArrayList([]const u8).empty;

        for (sub_res.args.library) |path| {
            list.append(allocator, path) catch return 1;
        }

        Parser.user_library_paths = list.toOwnedSlice(allocator) catch return 1;
    }

    return runner.runFile(
        sub_res.args.@"root-dir",
        sub_res.positionals[0] orelse &.{},
        if (sub_res.positionals.len > 1) sub_res.positionals[1] else &.{},
    ) catch {
        return 1;
    };
}

fn help(init: Init, stderr: *std.Io.Writer, subcommand_opt: ?[]const u8) u8 {
    io.print(init.io, "👨‍🚀 buzz A small/lightweight typed scripting language\n\nUsage: buzz ", .{});

    if (subcommand_opt) |subcommand| {
        io.print(init.io, "{s} ", .{subcommand});

        if (std.mem.eql(u8, subcommand, "test")) {
            clap.usage(
                stderr,
                clap.Help,
                &test_params,
            ) catch return 1;

            io.print(init.io, "\n\n", .{});

            clap.help(
                stderr,
                clap.Help,
                &test_params,
                .{
                    .description_on_new_line = false,
                    .description_indent = 4,
                    .spacing_between_parameters = 0,
                },
            ) catch return 1;
        } else if (std.mem.eql(u8, subcommand, "check")) {
            clap.usage(
                stderr,
                clap.Help,
                &check_params,
            ) catch return 1;

            io.print(init.io, "\n\n", .{});

            clap.help(
                stderr,
                clap.Help,
                &check_params,
                .{
                    .description_on_new_line = false,
                    .description_indent = 4,
                    .spacing_between_parameters = 0,
                },
            ) catch return 1;
        } else if (std.mem.eql(u8, subcommand, "format")) {
            clap.usage(
                stderr,
                clap.Help,
                &format_params,
            ) catch return 1;

            io.print(init.io, "\n\n", .{});

            clap.help(
                stderr,
                clap.Help,
                &format_params,
                .{
                    .description_on_new_line = false,
                    .description_indent = 4,
                    .spacing_between_parameters = 0,
                },
            ) catch return 1;
        } else if (std.mem.eql(u8, subcommand, "run")) {
            clap.usage(
                stderr,
                clap.Help,
                &run_params,
            ) catch return 1;

            io.print(init.io, "\n\n", .{});

            clap.help(
                stderr,
                clap.Help,
                &run_params,
                .{
                    .description_on_new_line = false,
                    .description_indent = 4,
                    .spacing_between_parameters = 0,
                },
            ) catch return 1;
        } else if (std.mem.eql(u8, subcommand, "run-script")) {
            clap.usage(
                stderr,
                clap.Help,
                &run_script_params,
            ) catch return 1;

            io.print(init.io, "\n\n", .{});

            clap.help(
                stderr,
                clap.Help,
                &run_script_params,
                .{
                    .description_on_new_line = false,
                    .description_indent = 4,
                    .spacing_between_parameters = 0,
                },
            ) catch return 1;
        } else if (std.mem.eql(u8, subcommand, "fetch")) {
            clap.usage(
                stderr,
                clap.Help,
                &fetch_params,
            ) catch return 1;

            io.print(init.io, "\n\n", .{});

            clap.help(
                stderr,
                clap.Help,
                &fetch_params,
                .{
                    .description_on_new_line = false,
                    .description_indent = 4,
                    .spacing_between_parameters = 0,
                },
            ) catch return 1;
        } else if (std.mem.eql(u8, subcommand, "help")) {
            clap.usage(
                stderr,
                clap.Help,
                &help_params,
            ) catch return 1;

            io.print(init.io, "\n\n", .{});

            clap.help(
                stderr,
                clap.Help,
                &help_params,
                .{
                    .description_on_new_line = false,
                    .description_indent = 4,
                    .spacing_between_parameters = 0,
                },
            ) catch return 1;
        } else if (std.mem.eql(u8, subcommand, "init")) {
            io.print(init.io, "\n{s}\n", .{command_summaries.init.description});
        } else if (std.mem.eql(u8, subcommand, "version")) {
            io.print(init.io, "\n{s}\n", .{command_summaries.version.description});
        }
    } else {
        clap.usage(
            stderr,
            clap.Help,
            &main_params,
        ) catch return 1;

        io.print(init.io, "\n\n", .{});

        io.print(init.io, "Commands:\n", .{});
        inline for (std.meta.fields(@TypeOf(command_summaries))) |field| {
            const command = @field(command_summaries, field.name);
            io.print(init.io, "  {s:<11} {s}\n", .{
                command.name,
                command.description,
            });
        }
        io.print(init.io, "\nUse `buzz help <command>` for command-specific help.\n", .{});
    }

    return 0;
}

test {
    _ = if (builtin.os.tag != .windows) @import("Scanner.zig") else {};
    _ = if (builtin.os.tag != .windows) @import("tests/fmt.zig") else {};
}
