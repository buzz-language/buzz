const std = @import("std");
const builtin = @import("builtin");
const BuildOptions = @import("build_options");
const dap = @import("dap");
const ProtocolMessage = dap.ProtocolMessage;
const Arguments = dap.Arguments;
const Response = dap.Response;
const Server = dap.Server;
const Adapter = dap.Adapter;
const clap = @import("clap");
const io = @import("io.zig");
const v = @import("vm.zig");
const printBanner = @import("repl.zig").printBanner;
const VM = v.VM;
const Runner = @import("Runner.zig");
const o = @import("obj.zig");
const Value = @import("value.zig").Value;
const assert = std.debug.assert;

const Debugger = @This();

const OutputStream = enum { stdout, stderr };

var log_file: std.fs.File = undefined;
var log_mutex = std.Thread.Mutex{};

pub const std_options: std.Options = .{
    .log_level = if (builtin.mode == .Debug) .debug else .info,
    .logFn = logFn,
};

/// We log to file to avoid polluting the program output forwarded to the client since
pub fn logFn(
    comptime level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    log_mutex.lock();
    defer log_mutex.unlock();

    // Ensure we append to the file
    log_file.seekFromEnd(0) catch {};

    var writer = log_file.writerStreaming(&.{});
    writer.interface.print(
        "[{s}] {s} " ++ format ++ "\n",
        .{
            @tagName(scope),
            @tagName(level),
        } ++ args,
    ) catch {};
}

allocator: std.mem.Allocator,
/// Queues to/from debugger/debuggee
transport: Server.Transport,
/// Server runs in its thread and reads requests over TCP connection to the debugger
server_thread: std.Thread,
/// Adapter reads incoming requests from the
adapter: Adapter(Debugger),
/// Debug session
session: ?DebugSession = null,
/// Stdout pipe so we can relay the output to the client
stdout_pipe: [2]std.posix.fd_t,
/// Stderr pipe so we can relay the output to the client
stderr_pipe: [2]std.posix.fd_t,
/// Output poller
out_poller: std.Io.Poller(OutputStream),

const Error = error{
    InvalidArguments,
    LaunchFailed,
    SessionAlreadyStarted,
    SessionNotStarted,
    OutOfMemory,
    FiberNotFound,
    CantCaptureOutput,
    BadState,
    CantEvaluate,
};

const DebugSession = struct {
    runner: Runner,
    /// If true, program must terminate
    run_state: RunState = .resumed,
    /// List of breakpoints to enforce
    breakpoints: std.ArrayHashMapUnmanaged(
        BreakpointKey,
        ProtocolMessage.Breakpoint,
        BreakpointKey.AutoContext,
        true,
    ) = .empty,
    /// Variable cache
    variables: std.ArrayList(Variable) = .empty,

    pub fn resetVariables(self: *DebugSession, allocator: std.mem.Allocator) void {
        self.variables.deinit(allocator);
    }

    pub const Step = struct {
        line: u32,
        frame: *v.CallFrame,
        frame_count: usize,
    };

    pub const RunState = union(enum) {
        paused: void,
        /// Should stop at the next line after this value
        step_over: Step,
        step_in: Step,
        /// Index of the frame that was paused, we pause when that frame_count is inferior to it
        step_out: usize,
        terminated: void,
        resumed: void,
    };

    pub fn setState(self: *DebugSession, new_state: RunState) void {
        self.run_state = new_state;

        // Reset variable cache (but keep globals)
        if (self.variables.items.len > 1) {
            self.variables.shrinkRetainingCapacity(1);
        }
    }

    pub fn deinit(self: *DebugSession) void {
        self.breakpoints.deinit(self.runner.gc.allocator);
        self.runner.deinit();
    }
};

const VariableValue = union(enum) {
    value: Value,
    map_entry: struct {
        map_type: *o.ObjTypeDef,
        key: Value,
        value: Value,
    },
    bound_entry: struct {
        receiver: Value,
        method: Value,
    },
    scope: union(enum) {
        global: void,
        frame: struct {
            fiber: *v.Fiber,
            frame: *v.CallFrame,
        },
    },
};

const Variable = struct {
    value: VariableValue,
    variable: union(enum) {
        variable: ProtocolMessage.Variable,
        scope: ProtocolMessage.Scope,
    },
    children: ?[]const Variable = null,
};

const BreakpointKey = struct {
    script: []const u8,
    line: u32,

    pub const AutoContext = struct {
        pub fn hash(_: AutoContext, bk: BreakpointKey) u32 {
            var hasher = std.hash.Wyhash.init(0);

            hasher.update(bk.script);
            hasher.update(std.mem.asBytes(&bk.line));

            return @truncate(hasher.final());
        }

        pub fn eql(_: AutoContext, bk_a: BreakpointKey, bk_b: BreakpointKey, _: usize) bool {
            return bk_a.line == bk_b.line and std.mem.eql(u8, bk_a.script, bk_b.script);
        }
    };
};

pub fn start(self: *Debugger, allocator: std.mem.Allocator, address: std.net.Address) (std.Thread.SpawnError || error{OutOfMemory} || Error)!void {
    self.* = Debugger{
        .allocator = allocator,
        .transport = .{
            .from = try .initCapacity(allocator, 256),
            .to = try .initCapacity(allocator, 256),
        },
        .server_thread = undefined,
        .adapter = undefined,
        .stdout_pipe = std.posix.pipe() catch return error.CantCaptureOutput,
        .stderr_pipe = std.posix.pipe() catch return error.CantCaptureOutput,
        .out_poller = undefined,
    };

    // FIXME: do this for windows with CreatePipe (https://github.com/ziglang/zig/blob/master/lib/std/os/windows.zig#L187)

    // Duplicate output pipe's ends to stdout/stderr
    std.posix.dup2(self.stdout_pipe[1], std.posix.STDOUT_FILENO) catch return error.CantCaptureOutput;
    std.posix.dup2(self.stderr_pipe[1], std.posix.STDERR_FILENO) catch return error.CantCaptureOutput;

    // Create poller on the read ends of the pipes
    self.out_poller = std.Io.poll(
        self.allocator,
        OutputStream,
        .{
            .stdout = std.fs.File{
                .handle = self.stdout_pipe[0],
            },
            .stderr = std.fs.File{
                .handle = self.stderr_pipe[0],
            },
        },
    );

    self.adapter = .{
        .handler = self,
        .transport = &self.transport,
    };

    self.server_thread = try Server.spawn(
        allocator,
        address,
        &self.transport,
    );
}

fn currentLine(self: *Debugger, current_frame: *v.CallFrame) u32 {
    const location = current_frame.closure.function.chunk.locations.items[current_frame.ip];
    return @intCast(self.session.?.runner.vm.current_ast.tokens.items(.line)[location] + 1);
}

/// Returns true if program has been terminated
pub fn onDispatch(self: *Debugger) Error!bool {
    _ = self.adapter.handleRequest();

    switch (self.session.?.run_state) {
        .paused => {},
        .terminated => return true,
        .step_over => |step| {
            const current_frame = self.session.?.runner.vm.currentFrame();

            // We pause on the next line in the same frame
            if (current_frame != null and
                (current_frame == step.frame or self.session.?.runner.vm.current_fiber.frame_count < step.frame_count) and
                current_frame.?.ip < current_frame.?.closure.function.chunk.locations.items.len and
                self.currentLine(current_frame.?) != step.line)
            {
                self.session.?.setState(.paused);

                // Notifiy we stopped
                self.adapter.emitEvent(
                    .{
                        .event = .stopped,
                        .body = .{
                            .stopped = .{
                                .reason = .step,
                                .threadId = @intFromPtr(self.session.?.runner.vm.current_fiber),
                            },
                        },
                    },
                );
            }
        },
        .step_out => |frame_count| {
            const current_frame = self.session.?.runner.vm.currentFrame();

            // We pause when we got out of the frame
            if (current_frame != null and
                self.session.?.runner.vm.current_fiber.frame_count < frame_count)
            {
                self.session.?.setState(.paused);

                // Notifiy we stopped
                self.adapter.emitEvent(
                    .{
                        .event = .stopped,
                        .body = .{
                            .stopped = .{
                                .reason = .step,
                                .threadId = @intFromPtr(self.session.?.runner.vm.current_fiber),
                            },
                        },
                    },
                );
            }
        },
        .step_in => |step| {
            const current_frame = self.session.?.runner.vm.currentFrame();

            // We step when the frame or the line changes
            if (current_frame != null and
                (current_frame != step.frame or
                    (current_frame.?.ip < current_frame.?.closure.function.chunk.locations.items.len and
                        self.currentLine(current_frame.?) != step.line)))
            {
                self.session.?.setState(.paused);

                // Notifiy we stopped
                self.adapter.emitEvent(
                    .{
                        .event = .stopped,
                        .body = .{
                            .stopped = .{
                                .reason = .step,
                                .threadId = @intFromPtr(self.session.?.runner.vm.current_fiber),
                            },
                        },
                    },
                );
            }
        },
        .resumed => {
            // Did we reach a breakpoint?
            const current_frame = self.session.?.runner.vm.currentFrame();

            if (current_frame != null and
                current_frame.?.ip < current_frame.?.closure.function.chunk.locations.items.len)
            {
                const location = current_frame.?.closure.function.chunk.locations.items[current_frame.?.ip];
                const line = self.session.?.runner.vm.current_ast.tokens.items(.line)[location] + 1;
                const script = self.session.?.runner.vm.current_ast.tokens.items(.script_name)[location];

                if (self.session.?.breakpoints.getPtr(
                    .{
                        .line = @intCast(line),
                        .script = script,
                    },
                )) |breakpoint| {
                    self.session.?.setState(.paused);

                    // Notifiy we stopped
                    self.adapter.emitEvent(
                        .{
                            .event = .stopped,
                            .body = .{
                                .stopped = .{
                                    .reason = .breakpoint,
                                    .threadId = @intFromPtr(self.session.?.runner.vm.current_fiber),
                                    .hitBreakpointIds = if (breakpoint.id) |bid| &.{bid} else null,
                                },
                            },
                        },
                    );

                    // If breakpoint breviously unverified, notify that too
                    if (!breakpoint.verified) {
                        breakpoint.verified = true;

                        self.adapter.emitEvent(
                            .{
                                .event = .breakpoint,
                                .body = .{
                                    .breakpoint = .{
                                        .breakpoint = breakpoint.*,
                                        .reason = .changed,
                                    },
                                },
                            },
                        );
                    }
                }
            }
        },
    }

    // Do we have output to relay to the client
    if (self.out_poller.pollTimeout(500_000) catch false) {
        const stdout = try self.out_poller.toOwnedSlice(.stdout);
        if (stdout.len > 0) {
            self.adapter.emitEvent(
                .{
                    .event = .output,
                    .body = .{
                        .output = .{
                            .category = .stdout,
                            .output = stdout,
                        },
                    },
                },
            );
        }

        // FIXME: stderr doesn't seem to be relayed here?
        const stderr = try self.out_poller.toOwnedSlice(.stderr);
        if (stderr.len > 0) {
            self.adapter.emitEvent(
                .{
                    .event = .output,
                    .body = .{
                        .output = .{
                            .category = .stderr,
                            .output = stderr,
                        },
                    },
                },
            );
        }
    }

    return false;
}

//
// Adapter callbacks
//

pub fn initialize(_: *Debugger, _: Arguments(.initialize)) Error!Response(.initialize) {
    return .{
        .supportsConfigurationDoneRequest = true,
    };
}

// FIXME: we might need to copy any data we're getting from incoming messages?
// => implement deinit on each ResponseBody structs, or use an arena

pub fn launch(self: *Debugger, arguments: Arguments(.launch)) Error!Response(.launch) {
    // Launch arguments are implementation specific, we assume there's what we need
    const launch_data = arguments.launch_data orelse return error.InvalidArguments;
    const program = (launch_data.object.get("program") orelse return error.InvalidArguments).string;

    if (self.session != null) {
        return error.SessionAlreadyStarted;
    }

    self.session = .{
        .runner = undefined,
    };

    self.session.?.runner.init(
        self.allocator,
        .Run,
        self,
    ) catch return error.LaunchFailed;

    try self.session.?.variables.append(
        self.allocator,
        .{
            .variable = .{
                .scope = .{
                    .name = "Globals",
                    .variablesReference = 1,
                    .expensive = false,
                    .namedVariables = 0,
                },
            },
            .value = .{
                .scope = .{
                    .global = {},
                },
            },
        },
    );

    // While debugger is active, the program won't start right away
    // FIXME: needs to report stdout of the program with `output` events
    self.session.?.runner.runFile(
        program,
        &.{}, // TODO
    ) catch return error.LaunchFailed;
}

pub fn setBreakpoints(self: *Debugger, arguments: Arguments(.setBreakpoints)) Error!Response(.setBreakpoints) {
    if (self.session) |*session| {
        session.breakpoints.clearRetainingCapacity();

        for (arguments.breakpoints orelse &.{}) |point| {
            try session.breakpoints.put(
                self.allocator,
                .{
                    .script = arguments.source.path orelse arguments.source.name.?,
                    .line = @intCast(point.line),
                },
                .{
                    .id = session.breakpoints.count(),
                    .verified = false,
                    .source = arguments.source,
                    .line = point.line,
                },
            );
        }

        return .{
            .breakpoints = session.breakpoints.values(),
        };
    }

    return error.SessionNotStarted;
}

pub fn setExceptionBreakpoints(_: *Debugger, _: Arguments(.setExceptionBreakpoints)) Error!Response(.setExceptionBreakpoints) {
    return .{};
}

pub fn next(self: *Debugger, _: Arguments(.next)) Error!Response(.next) {
    // We ignore the threadId for now, until we actually support multi threading
    if (self.session) |*session| {
        if (session.run_state != .paused) {
            return error.BadState;
        }

        session.setState(
            .{
                .step_over = .{
                    .frame_count = session.runner.vm.current_fiber.frame_count,
                    .frame = session.runner.vm.currentFrame().?,
                    .line = self.currentLine(session.runner.vm.currentFrame().?),
                },
            },
        );
    }

    return error.SessionNotStarted;
}

pub fn stepIn(self: *Debugger, _: Arguments(.stepIn)) Error!Response(.stepIn) {
    // We ignore the threadId for now, until we actually support multi threading
    if (self.session) |*session| {
        if (session.run_state != .paused) {
            return error.BadState;
        }

        session.setState(
            .{
                .step_in = .{
                    .frame_count = session.runner.vm.current_fiber.frame_count,
                    .frame = session.runner.vm.currentFrame().?,
                    .line = self.currentLine(session.runner.vm.currentFrame().?),
                },
            },
        );
    }

    return error.SessionNotStarted;
}

pub fn stepOut(self: *Debugger, _: Arguments(.stepOut)) Error!Response(.stepOut) {
    // We ignore the threadId for now, until we actually support multi threading
    if (self.session) |*session| {
        if (session.run_state != .paused) {
            return error.BadState;
        }

        session.setState(
            .{
                .step_out = session.runner.vm.current_fiber.frame_count,
            },
        );
    }

    return error.SessionNotStarted;
}

pub fn @"continue"(self: *Debugger, _: Arguments(.@"continue")) Error!Response(.@"continue") {
    if (self.session) |*session| {
        session.setState(.resumed);

        return .{};
    }

    return error.SessionNotStarted;
}

// FIXME: right now we're saying fibers are threads but really threads should be threads. When the thread std lib will land,
// how will we reconciliate the two with DAP?
pub fn threads(self: *Debugger, _: Arguments(.threads)) Error!Response(.threads) {
    if (self.session) |*session| {
        var thds = std.ArrayList(ProtocolMessage.Thread).empty;

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

pub fn stackTrace(self: *Debugger, arguments: Arguments(.stackTrace)) Error!Response(.stackTrace) {
    if (self.session) |*session| {
        // Retrieve appropriate fiber
        var fiber: ?*v.Fiber = session.runner.vm.current_fiber;
        while (@intFromPtr(fiber) != arguments.threadId and fiber != null) : (fiber = fiber.?.parent_fiber) {}

        if (fiber) |fb| {
            var stack_frames = std.ArrayList(ProtocolMessage.StackFrame).empty;

            var i = if (fb.frame_count > 0) fb.frame_count - 1 else fb.frame_count;
            while (i >= 0) : (i -= 1) {
                const frame = &fb.frames.items[i];
                const location = session.runner.vm.current_ast.tokens
                    .get(frame.closure.function.chunk.locations.items[frame.ip]);

                try stack_frames.append(
                    self.allocator,
                    .{
                        .id = @intFromPtr(frame),
                        .name = frame.closure.function.type_def.resolved_type.?.Function.name.string,
                        .line = location.line + 1,
                        .column = location.column,
                        .source = .{
                            .name = if (std.mem.lastIndexOf(u8, frame.closure.function.type_def.resolved_type.?.Function.script_name.string, "/")) |slash|
                                frame.closure.function.type_def.resolved_type.?.Function.script_name.string[slash + 1 ..]
                            else
                                frame.closure.function.type_def.resolved_type.?.Function.script_name.string,
                            .path = frame.closure.function.type_def.resolved_type.?.Function.script_name.string,
                        },
                    },
                );

                if (i == 0) break;
            }

            const count = @min(stack_frames.items.len, arguments.levels orelse stack_frames.items.len);
            return .{
                .stackFrames = (try stack_frames.toOwnedSlice(self.allocator))[0..count],
                .totalFrames = count,
            };
        }

        return error.FiberNotFound;
    }

    return error.SessionNotStarted;
}

fn findFrame(self: *Debugger, frameId: u64) ?struct { *v.CallFrame, *v.Fiber } {
    if (self.session) |*session| {
        var fiber: ?*v.Fiber = session.runner.vm.current_fiber;
        while (fiber) |fb| : (fiber = fb.parent_fiber) {
            for (fb.frames.items) |*frame| {
                if (@intFromPtr(frame) == frameId) {
                    return .{ frame, fb };
                }
            }
        }
    }

    return null;
}

pub fn scopes(self: *Debugger, args: Arguments(.scopes)) Error!Response(.scopes) {
    if (self.session) |*session| {
        var scps = try std.ArrayList(ProtocolMessage.Scope).initCapacity(
            self.allocator,
            2,
        );

        // Locals of the current frame
        var found = false;
        for (session.variables.items) |vbl| {
            if (vbl.value == .scope and vbl.value.scope == .frame and @intFromPtr(vbl.value.scope.frame.frame) == args.frameId) {
                try scps.append(self.allocator, vbl.variable.scope);
                found = true;
                break;
            }
        }

        // Otherwise search the appropriate frame
        if (!found) {
            if (self.findFrame(args.frameId)) |frame_fiber| {
                const scope = Variable{
                    .value = .{
                        .scope = .{
                            .frame = .{
                                .frame = frame_fiber.@"0",
                                .fiber = frame_fiber.@"1",
                            },
                        },
                    },
                    .variable = .{
                        .scope = .{
                            .name = "Locals",
                            .variablesReference = session.variables.items.len + 1,
                            .expensive = false,
                        },
                    },
                };

                try session.variables.append(self.allocator, scope);
                try scps.append(self.allocator, scope.variable.scope);
            }
        }

        // Globals might be defined out of order, we need to count those that are actually defined
        var global_count: usize = 0;
        for (session.runner.vm.globals_dbg.items) |name| {
            if (!name.isNull()) {
                global_count += 1;
            }
        }

        var global_scope = session.variables.items[0];
        global_scope.variable.scope.namedVariables = global_count;

        try scps.append(self.allocator, global_scope.variable.scope);

        return .{
            .scopes = try scps.toOwnedSlice(self.allocator),
        };
    }

    return error.SessionNotStarted;
}

pub fn variables(self: *Debugger, arguments: Arguments(.variables)) Error!Response(.variables) {
    if (self.session) |*session| {
        var result = std.ArrayList(ProtocolMessage.Variable).empty;
        const ref = arguments.variablesReference - 1;

        if (session.variables.items.len > ref) {
            if (session.variables.items[ref].children) |children| {
                for (children) |child| {
                    try result.append(self.allocator, child.variable.variable);
                }
            } else {
                const previous_len = session.variables.items.len;

                switch (session.variables.items[ref].value) {
                    .scope => |sc| switch (sc) {
                        .global => {
                            for (session.runner.vm.globals_dbg.items, 0..) |global, slot| {
                                const value = session.runner.vm.globals.items[slot];

                                if (!global.isNull()) {
                                    try result.append(
                                        self.allocator,
                                        try self.variable(
                                            value,
                                            global,
                                            null,
                                        ),
                                    );
                                }
                            }
                        },
                        .frame => |ctx| {
                            const frame = ctx.frame;
                            const fiber = ctx.fiber;
                            const stack = @as([*]Value, @ptrCast(fiber.stack));
                            const frame_base_idx = frame.slots - stack;
                            const top = session.runner.frameTop(fiber, frame);
                            const top_idx = top - stack;

                            for (frame_base_idx..@min(top_idx - 1, fiber.locals_dbg.items.len)) |idx| {
                                if (!fiber.locals_dbg.items[idx].isNull()) {
                                    const name = fiber.locals_dbg.items[idx];

                                    // "Hidden" locals start with `$`
                                    if (o.ObjString.cast(name.obj()).?.string[0] != '$') {
                                        try result.append(
                                            self.allocator,
                                            try self.variable(
                                                fiber.stack[idx + 1],
                                                name,
                                                null,
                                            ),
                                        );
                                    }
                                }
                            }
                        },
                    },
                    .value => |vl| {
                        try self.valueChildren(
                            vl,
                            &result,
                        );
                    },
                    .bound_entry => |be| {
                        try result.append(
                            self.allocator,
                            try self.variable(
                                be.receiver,
                                (session.runner.gc.copyString("receiver") catch return error.OutOfMemory).toValue(),
                                null,
                            ),
                        );

                        try result.append(
                            self.allocator,
                            try self.variable(
                                be.method,
                                (session.runner.gc.copyString("method") catch return error.OutOfMemory).toValue(),
                                null,
                            ),
                        );
                    },
                    .map_entry => |me| {
                        try result.append(
                            self.allocator,
                            try self.variable(
                                me.key,
                                (session.runner.gc.copyString("key") catch return error.OutOfMemory).toValue(),
                                me.map_type.resolved_type.?.Map.key_type,
                            ),
                        );

                        try result.append(
                            self.allocator,
                            try self.variable(
                                me.value,
                                (session.runner.gc.copyString("value") catch return error.OutOfMemory).toValue(),
                                me.map_type.resolved_type.?.Map.value_type,
                            ),
                        );
                    },
                }

                // Populate children cache
                session.variables.items[ref].children = session.variables.items[previous_len..];
                assert(session.variables.items[ref].children.?.len == result.items.len);
            }
        }

        return .{
            .variables = try result.toOwnedSlice(self.allocator),
        };
    }

    return error.SessionNotStarted;
}

pub fn evaluate(self: *Debugger, args: Arguments(.evaluate)) Error!Response(.evaluate) {
    if (self.session) |*session| {
        const frame, const fiber = if (args.frameId) |frameId|
            self.findFrame(frameId) orelse .{ null, null }
        else
            .{ null, null };

        const result = session.runner.evaluate(
            fiber orelse session.runner.vm.current_fiber,
            frame orelse session.runner.vm.currentFrame().?,
            args.expression,
        ) catch return error.CantEvaluate;

        if (result.isObj()) {
            _ = try self.variable(
                result,
                (session.runner.gc.copyString("__eval__") catch return error.OutOfMemory).toValue(),
                null,
            );
        }

        return .{
            .result = result.toStringAlloc(self.allocator) catch return error.OutOfMemory,
            .type = (result.typeOf(&session.runner.gc) catch return error.OutOfMemory)
                .toStringAlloc(self.allocator, false) catch return error.OutOfMemory,
            .variablesReference = if (result.isObj())
                session.variables.items.len - 1
            else
                0,
        };
    }

    return error.SessionNotStarted;
}

pub fn configurationDone(self: *Debugger, _: Arguments(.configurationDone)) Error!Response(.configurationDone) {
    if (self.session == null) return error.SessionNotStarted;
}

pub fn terminate(self: *Debugger, _: Arguments(.terminate)) Error!Response(.terminate) {
    if (self.session) |*session| {
        session.setState(.terminated);
    }
}

pub fn disconnect(self: *Debugger, _: Arguments(.disconnect)) Error!Response(.disconnect) {
    if (self.session) |*session| {
        session.setState(.terminated);
    }
}

pub fn pause(self: *Debugger, _: Arguments(.pause)) Error!Response(.pause) {
    if (self.session) |*session| {
        session.setState(.paused);
    }

    return error.SessionNotStarted;
}

fn variable(self: *Debugger, value: Value, name: Value, explicit_type_def: ?*o.ObjTypeDef) Error!ProtocolMessage.Variable {
    const indexed_count = self.valueIndexedChildren(value);
    const named_count = self.valueNamedChildren(value);

    const vbl = Variable{
        .value = val: {
            if (value.isObj()) {
                break :val switch (value.obj().obj_type) {
                    .Bound => .{
                        .bound_entry = .{
                            .receiver = o.ObjBoundMethod.cast(value.obj()).?.receiver,
                            .method = if (o.ObjBoundMethod.cast(value.obj()).?.closure) |cls|
                                cls.toValue()
                            else
                                o.ObjBoundMethod.cast(value.obj()).?.native.?.toValue(),
                        },
                    },
                    else => .{
                        .value = value,
                    },
                };
            }

            break :val .{
                .value = value,
            };
        },
        .variable = .{
            .variable = .{
                .name = name.obj().cast(o.ObjString, .String).?.string,
                .evaluateName = name.obj().cast(o.ObjString, .String).?.string,
                .value = value.toStringAlloc(self.allocator) catch "Could not get value",
                .variablesReference = if (indexed_count + named_count > 0)
                    self.session.?.variables.items.len + 1
                else
                    0,
                .type = if (explicit_type_def) |type_def|
                    type_def.toStringAlloc(
                        self.allocator,
                        false,
                    ) catch null
                else if (value.typeOf(&self.session.?.runner.gc)) |type_def|
                    type_def.toStringAlloc(
                        self.allocator,
                        false,
                    ) catch null
                else |_|
                    null,
                .indexedVariables = indexed_count,
                .namedVariables = named_count,
            },
        },
    };

    try self.session.?.variables.append(self.allocator, vbl);

    return vbl.variable.variable;
}

fn valueIndexedChildren(_: *Debugger, value: Value) u64 {
    if (value.isObj()) {
        return switch (value.obj().obj_type) {
            .List => o.ObjList.cast(value.obj()).?.items.items.len,
            else => 0,
        };
    }

    return 0;
}

fn valueNamedChildren(_: *Debugger, value: Value) u64 {
    if (value.isObj()) {
        return switch (value.obj().obj_type) {
            .Object => obj: {
                const fields = o.ObjObject.cast(value.obj()).?
                    .type_def.resolved_type.?.Object
                    .fields.values();
                var count: u64 = 0;
                for (fields) |field| {
                    if (field.static and !field.method) {
                        count += 1;
                    }
                }

                break :obj count;
            },
            .ObjectInstance => obj: {
                const fields = o.ObjObjectInstance.cast(value.obj()).?
                    .type_def.resolved_type.?.ObjectInstance.of
                    .resolved_type.?.Object
                    .fields.values();
                var count: u64 = 0;
                for (fields) |field| {
                    if (!field.static and !field.method) {
                        count += 1;
                    }
                }

                break :obj count;
            },
            .Enum => o.ObjEnum.cast(value.obj()).?.cases.len,
            .ForeignContainer => o.ObjForeignContainer.cast(value.obj()).?
                .type_def.resolved_type.?.ForeignContainer
                .fields.count(),
            .Map => 2, // key, value
            .Bound => 2, // receiver, method
            else => 0,
        };
    }

    return 0;
}

fn valueChildren(self: *Debugger, value: Value, result: *std.ArrayList(ProtocolMessage.Variable)) Error!void {
    if (value.isObj()) {
        switch (value.obj().obj_type) {
            // No children
            .String,
            .Pattern,
            .Fiber,
            .Type,
            .Closure,
            .Function,
            .Range,
            .EnumInstance,
            .Native,
            .UserData,
            => {},

            .UpValue => try self.valueChildren(
                o.ObjUpValue.cast(value.obj()).?.closed orelse
                    o.ObjUpValue.cast(value.obj()).?.location.*,
                result,
            ),

            // Proxied earlier
            .Bound => unreachable,

            .ForeignContainer => {
                const container = o.ObjForeignContainer.cast(value.obj()).?;
                const def = container.type_def.resolved_type.?.ForeignContainer;

                var it = def.fields.iterator();
                while (it.next()) |kv| {
                    try result.append(
                        self.allocator,
                        try self.variable(
                            container.getField(
                                &self.session.?.runner.vm,
                                def.fields.getIndex(kv.key_ptr.*).?,
                            ),
                            (self.session.?.runner.gc.copyString(kv.key_ptr.*) catch return error.OutOfMemory).toValue(),
                            def.buzz_type.get(kv.key_ptr.*).?,
                        ),
                    );
                }
            },

            .Enum => {
                const enm = o.ObjEnum.cast(value.obj()).?;

                for (enm.type_def.resolved_type.?.Enum.cases, 0..) |case_name, i| {
                    try result.append(
                        self.allocator,
                        try self.variable(
                            enm.cases[i],
                            (self.session.?.runner.gc.copyString(case_name) catch return error.OutOfMemory).toValue(),
                            enm.type_def.resolved_type.?.Enum.enum_type,
                        ),
                    );
                }
            },

            .Object => {
                const object = o.ObjObject.cast(value.obj()).?;

                var it = object.type_def.resolved_type.?.Object.fields.iterator();
                while (it.next()) |kv| {
                    if (!kv.value_ptr.method and kv.value_ptr.static) {
                        try result.append(
                            self.allocator,
                            try self.variable(
                                object.fields[kv.value_ptr.index],
                                (self.session.?.runner.gc.copyString(kv.key_ptr.*) catch return error.OutOfMemory).toValue(),
                                kv.value_ptr.type_def,
                            ),
                        );
                    }
                }
            },

            .ObjectInstance => {
                const instance = o.ObjObjectInstance.cast(value.obj()).?;

                var it = instance.type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object.fields.iterator();
                while (it.next()) |kv| {
                    if (!kv.value_ptr.method and !kv.value_ptr.static) {
                        try result.append(
                            self.allocator,
                            try self.variable(
                                instance.fields[kv.value_ptr.index],
                                (self.session.?.runner.gc.copyString(kv.key_ptr.*) catch return error.OutOfMemory).toValue(),
                                kv.value_ptr.type_def,
                            ),
                        );
                    }
                }
            },

            .List => {
                const list = o.ObjList.cast(value.obj()).?;

                for (list.items.items, 0..) |val, i| {
                    var name = std.Io.Writer.Allocating.init(self.allocator);
                    name.writer.print("#{}", .{i}) catch return error.OutOfMemory;

                    try result.append(
                        self.allocator,
                        try self.variable(
                            val,
                            (self.session.?.runner.gc.copyString(
                                try name.toOwnedSlice(),
                            ) catch return error.OutOfMemory).toValue(),
                            list.type_def.resolved_type.?.List.item_type,
                        ),
                    );
                }
            },

            .Map => {
                const map = o.ObjMap.cast(value.obj()).?;

                var it = map.map.iterator();
                var count: usize = 0;
                while (it.next()) |kv| : (count += 1) {
                    var name = std.Io.Writer.Allocating.init(self.allocator);
                    name.writer.print("MapEntry#{}", .{count}) catch return error.OutOfMemory;

                    const vbl = Variable{
                        .value = .{
                            .map_entry = .{
                                .map_type = map.type_def,
                                .key = kv.key_ptr.*,
                                .value = kv.value_ptr.*,
                            },
                        },
                        .variable = .{
                            .variable = .{
                                .name = name.written(),
                                .evaluateName = name.written(),
                                .value = "{...}",
                                .variablesReference = self.session.?.variables.items.len + 3,
                                .indexedVariables = 0,
                                .namedVariables = 2,
                            },
                        },
                    };

                    try self.session.?.variables.append(self.allocator, vbl);

                    try result.append(self.allocator, vbl.variable.variable);
                }
            },
        }
    }
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
        \\-o, --output <str>     File where any log output will be written (defaults to $PWD/log.txt)
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

    // Open log file
    const output_file = res.args.output orelse "./log.txt";
    log_file = if (std.fs.path.isAbsolute(output_file))
        try std.fs.createFileAbsolute(
            output_file,
            .{
                .truncate = false,
                .read = false,
            },
        )
    else
        try std.fs.cwd().createFile(
            output_file,
            .{
                .truncate = false,
                .read = false,
            },
        );

    // Start the debugger
    var debugger: Debugger = undefined;
    try debugger.start(
        allocator,
        .initIp4(
            .{ 127, 0, 0, 1 },
            res.args.port orelse 9000,
        ),
    );
    errdefer debugger.server_thread.join();

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
                    debugger.session.?.runner.vm.run() catch {
                        // TODO
                    };

                    // Program is over, send terminated and stop event
                    debugger.adapter.emitEvent(
                        .{
                            .event = .terminated,
                            .body = .{
                                .terminated = .{},
                            },
                        },
                    );

                    debugger.adapter.emitEvent(
                        .{
                            .event = .exited,
                            .body = .{
                                .exited = .{
                                    // TODO: grap actual exit code
                                    .exitCode = 0,
                                },
                            },
                        },
                    );

                    debugger.session.?.deinit();
                    debugger.session = null;

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
