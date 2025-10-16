const std = @import("std");
const builtin = @import("builtin");
const BuildOptions = @import("build_options");
const dap = @import("dap");
const clap = @import("clap");
const io = @import("io.zig");
const v = @import("vm.zig");
const printBanner = @import("repl.zig").printBanner;
const VM = v.VM;

const Debugger = @This();

allocator: std.mem.Allocator,
/// Queues to/from debugger/debuggee
transport: dap.Server.Transport,
/// Server runs in its thread and reads requests over TCP connection to the debugger
server_thread: std.Thread,
/// Adapter reads incoming requests from the
adapter: dap.Adapter(Debugger),

pub fn start(allocator: std.mem.Allocator, address: std.net.Address) (std.Thread.SpawnError || error{OutOfMemory})!*Debugger {
    var self = try allocator.create(Debugger);
    self.* = Debugger{
        .allocator = allocator,
        .transport = .{
            .from = try .initCapacity(allocator, 256),
            .to = try .initCapacity(allocator, 256),
        },
        .server_thread = undefined,
        .adapter = undefined,
    };

    self.adapter = .{
        .handler = self,
        .transport = &self.transport,
    };

    self.server_thread = try dap.Server.spawn(
        allocator,
        address,
        &self.transport,
    );

    return self;
}

pub fn initialize(_: *Debugger, _: dap.ProtocolMessage.InitializeArguments) !dap.ProtocolMessage.InitializeResponseBody {
    std.log.debug("Handling `initialize request...`", .{});

    return .{
        .capabilities = .{},
    };
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = builtin.mode == .Debug }){};
    const allocator: std.mem.Allocator = if (builtin.mode == .Debug)
        gpa.allocator()
    else if (BuildOptions.mimalloc)
        @import("mimalloc.zig").mim_allocator
    else
        std.heap.c_allocator;

    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Show help and exit
        \\-p, --port <u16>       On which port the debugger should be listening
        \\-v, --version          Print version and exit
        \\-L, --library <str>... Add search path for external libraries
        \\
    );

    var diag = clap.Diagnostic{};
    var res = clap.parse(
        clap.Help,
        &params,
        clap.parsers.default,
        .{
            .allocator = allocator,
            .diagnostic = &diag,
        },
    ) catch |err| {
        // Report useful error and exit
        diag.report(io.stderrWriter, err) catch {};
        return 1;
    };
    defer res.deinit();

    if (res.args.version == 1) {
        printBanner(io.stdoutWriter, true);

        return 0;
    }

    if (res.args.help == 1) {
        io.print("👨‍🚀 Debugger for the buzz programming language\n\nUsage: buzz_debugger ", .{});

        clap.usage(
            io.stderrWriter,
            clap.Help,
            &params,
        ) catch return 1;

        io.print("\n\n", .{});

        clap.help(
            io.stderrWriter,
            clap.Help,
            &params,
            .{
                .description_on_new_line = false,
                .description_indent = 4,
                .spacing_between_parameters = 0,
            },
        ) catch return 1;

        return 0;
    }

    // Start the debugger
    const debugger = try start(
        allocator,
        .initIp4(
            .{ 127, 0, 0, 1 },
            res.args.port orelse 9000,
        ),
    );

    // Read requests
    while (true) {
        if (debugger.adapter.handleRequest()) |handled_request| {
            switch (handled_request) {
                // We send the initalized event right after responsding to the request
                .initialize => debugger.adapter.emitEvent(
                    .{
                        .event = .initialized,
                        .body = .{
                            .initialized = {},
                        },
                    },
                ),
                else => {},
            }
        }

        std.Thread.sleep(10_000_000);
    }

    // Await for the server to end
    debugger.server_thread.join();
}
