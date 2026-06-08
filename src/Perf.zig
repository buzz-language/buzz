const std = @import("std");
const builtin = @import("builtin");
const BuildOptions = @import("build_options");
const bz_io = @import("io.zig");

const Self = @This();

pub const Component = enum {
    file_io,
    scanner,
    parser,
    codegen,
    vm,
    gc,
    jit,
    native,
};

pub const Scope = struct {
    perf: ?*Self,
    component: Component,
    start: std.Io.Clock.Timestamp = undefined,

    pub inline fn end(self: *Scope) void {
        if (!BuildOptions.show_perf or builtin.cpu.arch.isWasm()) {
            return;
        }

        const perf = self.perf orelse return;
        const duration = self.start.untilNow(perf.io).raw.toNanoseconds();
        const component = self.component;

        if (StackState.depth > 0 and StackState.stack[StackState.depth - 1].component == component) {
            StackState.depth -= 1;
        }

        const parent = if (StackState.depth > 0)
            StackState.stack[StackState.depth - 1].component
        else
            null;

        perf.recordDuration(component, parent, @intCast(@max(0, duration)));
        self.perf = null;
    }
};

const component_count = @typeInfo(Component).@"enum".fields.len;
const default_order = defaultOrder();
const max_stack = 64;

const Active = struct {
    component: Component,
};

const StackState = if (builtin.cpu.arch.isWasm()) struct {
    var stack: [max_stack]Active = undefined;
    var depth: usize = 0;
} else struct {
    threadlocal var stack: [max_stack]Active = undefined;
    threadlocal var depth: usize = 0;
};

io: bz_io.Io,
started_at: std.Io.Clock.Timestamp,
mutex: std.Io.Mutex = .init,
totals: [component_count]i128 = [_]i128{0} ** component_count,
children: [component_count][component_count]i128 = [_][component_count]i128{[_]i128{0} ** component_count} ** component_count,

pub fn init(io: bz_io.Io) Self {
    return .{
        .io = io,
        .started_at = std.Io.Clock.Timestamp.now(io, .awake),
    };
}

pub inline fn begin(self: *Self, component: Component) Scope {
    if (!BuildOptions.show_perf or builtin.cpu.arch.isWasm()) {
        return .{ .perf = null, .component = component };
    }

    var i: usize = 0;
    while (i < StackState.depth) : (i += 1) {
        if (StackState.stack[i].component == component) {
            return .{ .perf = null, .component = component };
        }
    }

    if (StackState.depth >= max_stack) {
        return .{ .perf = null, .component = component };
    }

    StackState.stack[StackState.depth] = .{ .component = component };
    StackState.depth += 1;

    return .{
        .perf = self,
        .component = component,
        .start = std.Io.Clock.Timestamp.now(self.io, .awake),
    };
}

pub inline fn start(perf: ?*Self, component: Component) Scope {
    if (!BuildOptions.show_perf or builtin.cpu.arch.isWasm()) {
        return .{ .perf = null, .component = component };
    }

    return if (perf) |p| p.begin(component) else .{ .perf = null, .component = component };
}

pub fn report(self: *Self) void {
    if (!BuildOptions.show_perf or builtin.cpu.arch.isWasm()) {
        return;
    }

    var totals: [component_count]i128 = undefined;
    var children: [component_count][component_count]i128 = undefined;
    self.mutex.lockUncancelable(self.io);
    totals = self.totals;
    children = self.children;
    self.mutex.unlock(self.io);

    const elapsed = @as(i128, @intCast(@max(0, self.started_at.untilNow(self.io).raw.toNanoseconds())));
    if (elapsed == 0) {
        return;
    }

    var order = default_order;
    std.mem.sort(usize, &order, &totals, durationGreaterThan);

    var stderr = bz_io.stderrWriter(self.io);
    const out = &stderr.interface;

    out.print("\n\x1b[36mPerformance\x1b[0m\n", .{}) catch return;
    out.print("Total elapsed: ", .{}) catch return;
    printDuration(out, elapsed) catch return;
    out.print("\n\n", .{}) catch return;

    for (order) |component_index| {
        const duration = totals[component_index];
        if (duration <= 0) {
            continue;
        }

        const component: Component = @enumFromInt(component_index);
        const percent: u128 = @intCast(@divTrunc(duration * 100, elapsed));
        var duration_buffer: [32]u8 = undefined;
        const duration_string = formatDuration(&duration_buffer, duration) catch return;
        out.print("{s: <9} {s: >10} {d: >3}% ", .{
            label(component),
            duration_string,
            percent,
        }) catch return;
        bz_io.printProgressBar(out, @intCast(duration), @intCast(elapsed), 24, color(component)) catch return;
        tryPrintChildren(out, children[component_index]) catch return;
        out.print("\n", .{}) catch return;
    }
}

fn defaultOrder() [component_count]usize {
    var order: [component_count]usize = undefined;
    for (&order, 0..) |*item, index| {
        item.* = index;
    }

    return order;
}

fn durationGreaterThan(totals: *const [component_count]i128, lhs: usize, rhs: usize) bool {
    return totals.*[lhs] > totals.*[rhs];
}

fn recordDuration(self: *Self, component: Component, parent: ?Component, duration: i128) void {
    if (duration <= 0) {
        return;
    }

    const component_index = @intFromEnum(component);

    self.mutex.lockUncancelable(self.io);
    defer self.mutex.unlock(self.io);

    self.totals[component_index] += duration;

    if (parent) |p| {
        self.children[@intFromEnum(p)][component_index] += duration;
    }
}

fn label(component: Component) []const u8 {
    return switch (component) {
        .file_io => "File I/O",
        .scanner => "Scanner",
        .parser => "Parser",
        .codegen => "Codegen",
        .vm => "VM",
        .gc => "GC",
        .jit => "JIT",
        .native => "Native",
    };
}

fn color(component: Component) []const u8 {
    return switch (component) {
        .file_io => "\x1b[34m",
        .scanner => "\x1b[35m",
        .parser => "\x1b[36m",
        .codegen => "\x1b[33m",
        .vm => "\x1b[32m",
        .gc => "\x1b[31m",
        .jit => "\x1b[95m",
        .native => "\x1b[94m",
    };
}

fn printDuration(out: *std.Io.Writer, duration: i128) !void {
    const ns_per_us = std.time.ns_per_us;
    const ns_per_ms = std.time.ns_per_ms;
    const ns_per_s = std.time.ns_per_s;
    const value: u128 = @intCast(@max(0, duration));

    if (value >= ns_per_s) {
        try out.print("{d}.{d:0>3}s", .{
            @divTrunc(value, ns_per_s),
            @divTrunc(@mod(value, ns_per_s), ns_per_ms),
        });
    } else if (value >= ns_per_ms) {
        try out.print("{d}.{d:0>3}ms", .{
            @divTrunc(value, ns_per_ms),
            @divTrunc(@mod(value, ns_per_ms), ns_per_us),
        });
    } else if (value >= ns_per_us) {
        try out.print("{d}us", .{@divTrunc(value, ns_per_us)});
    } else {
        try out.print("{d}ns", .{value});
    }
}

fn formatDuration(buffer: *[32]u8, duration: i128) ![]const u8 {
    var writer: std.Io.Writer = .fixed(buffer);
    try printDuration(&writer, duration);
    return writer.buffered();
}

fn tryPrintChildren(out: *std.Io.Writer, component_children: [component_count]i128) !void {
    var first = true;

    for (component_children, 0..) |duration, index| {
        if (duration <= 0) {
            continue;
        }

        if (first) {
            try out.writeAll("  includes: ");
            first = false;
        } else {
            try out.writeAll(", ");
        }

        try out.print("{s} ", .{label(@enumFromInt(index))});
        try printDuration(out, duration);
    }
}
