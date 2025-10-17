const std = @import("std");
const builtin = @import("builtin");
const BuildOptions = @import("build_options");
const dap = @import("dap");
const clap = @import("clap");
const io = @import("io.zig");
const v = @import("vm.zig");
const printBanner = @import("repl.zig").printBanner;
const VM = v.VM;
const Runner = @import("Runner.zig");

const Debugger = @This();

const Error = error{
    InvalidArguments,
    LaunchFailed,
    SessionAlreadyStarted,
    SessionNotStarted,
    OutOfMemory,
};

const DebugSession = struct {
    runner: Runner,
    breakpoints: std.ArrayList(dap.ProtocolMessage.Breakpoint) = .empty,
};

allocator: std.mem.Allocator,
/// Queues to/from debugger/debuggee
transport: dap.Server.Transport,
/// Server runs in its thread and reads requests over TCP connection to the debugger
server_thread: std.Thread,
/// Adapter reads incoming requests from the
adapter: dap.Adapter(Debugger),
/// Debug session
session: ?DebugSession = null,

pub fn start(self: *Debugger, allocator: std.mem.Allocator, address: std.net.Address) (std.Thread.SpawnError || error{OutOfMemory})!void {
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
}

//
// Adapter callbacks
// FIXME: should write fn ... (self: *Debugger, args: @...) Error!@...

pub fn initialize(_: *Debugger, _: dap.ProtocolMessage.InitializeArguments) Error!dap.ProtocolMessage.Capabilities {
    std.log.debug("Handling `initialize request...`", .{});

    return .{
        .supportsConfigurationDoneRequest = true,
    };
}

// FIXME: we might need to copy any data we're getting from incoming messages?

pub fn launch(self: *Debugger, arguments: dap.ProtocolMessage.LaunchArguments) Error!void {
    // Launch arguments are implementation specific, we assume there's what we need
    const launch_data = arguments.launch_data orelse return error.InvalidArguments;
    const program = (launch_data.object.get("program") orelse return error.InvalidArguments).string;

    if (self.session != null) {
        return error.SessionAlreadyStarted;
    }

    self.session = .{
        .runner = undefined,
    };

    // While debugger is active, the program won't start right away
    self.session.?.runner.runFile(
        self.allocator,
        program,
        &.{}, // TODO
        .Run, // TODO: we should be able to debug tests too
        self,
    ) catch return error.LaunchFailed;
}

pub fn setBreakpoints(self: *Debugger, arguments: dap.ProtocolMessage.SetBreakpointsArguments) Error!dap.ProtocolMessage.SetBreakpointsResponseBody {
    if (self.session) |*session| {
        const current = if (session.breakpoints.items.len > 0) session.breakpoints.items.len - 1 else 0;
        for (arguments.breakpoints orelse &.{}) |point| {
            try session.breakpoints.append(
                self.allocator,
                .{
                    .id = session.breakpoints.items.len,
                    .verified = false,
                    .source = arguments.source,
                    .line = point.line,
                },
            );
        }

        return .{
            .breakpoints = session.breakpoints.items[current..],
        };
    }

    return error.SessionNotStarted;
}

pub fn setExceptionBreakpoints(_: *Debugger, _: dap.ProtocolMessage.SetExceptionBreakpointsArguments) Error!dap.ProtocolMessage.SetExceptionBreakpointsResponseBody {
    return .{};
}

pub fn threads(self: *Debugger, _: void) Error!dap.ProtocolMessage.ThreadsResponseBody {
    if (self.session) |*session| {
        var thds = std.ArrayList(dap.ProtocolMessage.Thread).empty;

        var fiber: ?*v.Fiber = session.runner.vm.current_fiber;
        while (fiber) |fb| : (fiber = fb.parent_fiber) {
            try thds.append(
                self.allocator,
                .{
                    .id = @intFromPtr(fb),
                    .name = if (fb.frames.items.len > 0)
                        fb.frames.items[fb.frames.items.len - 1].closure.function.type_def.resolved_type.?.Function.name.string
                    else
                        "thread",
                },
            );
        }

        return .{
            // FIXME: free this once sent
            .threads = try thds.toOwnedSlice(self.allocator),
        };
    }

    return error.SessionNotStarted;
}

pub fn configurationDone(self: *Debugger, _: void) Error!void {
    if (self.session == null) return error.SessionNotStarted;
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
    var debugger: Debugger = undefined;
    try debugger.start(
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
                .configurationDone => {
                    // Now the configuration phase is done, we start the program and it's up to the VM to call handlRequest at safepoints
                    debugger.session.?.runner.vm.run();
                    break;
                },
                else => {},
            }
        }

        std.Thread.sleep(10_000_000);
    }

    // Await for the server to end
    debugger.server_thread.join();

    return 0;
}
