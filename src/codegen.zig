const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const _chunk = @import("./chunk.zig");
const _obj = @import("./obj.zig");
const _vm = @import("./vm.zig");
const _value = @import("./value.zig");
const _disassembler = @import("./disassembler.zig");
const _utils = @import("./utils.zig");
const _parser = @import("./parser.zig");
const _node = @import("./node.zig");
const _token = @import("./token.zig");
const GarbageCollector = @import("./memory.zig").GarbageCollector;
const Config = @import("./config.zig").Config;
const ParseNode = _node.ParseNode;
const FunctionNode = _node.FunctionNode;
const ObjFunction = _obj.ObjFunction;
const Global = _parser.Global;
const Parser = _parser.Parser;
const OpCode = _chunk.OpCode;
const Value = _value.Value;
const Chunk = _chunk.Chunk;
const Token = _token.Token;
const ObjTypeDef = _obj.ObjTypeDef;
const PlaceholderDef = _obj.PlaceholderDef;
const TypeRegistry = _obj.TypeRegistry;

pub const Frame = struct {
    enclosing: ?*Frame = null,
    function_node: *FunctionNode,
    function: ?*ObjFunction = null,
    return_counts: bool = false,
    return_emitted: bool = false,
};

pub const CodeGen = struct {
    const Self = @This();

    current: ?*Frame = null,
    gc: *GarbageCollector,
    type_registry: *TypeRegistry,
    testing: bool,
    // Jump to patch at end of current expression with a optional unwrapping in the middle of it
    opt_jumps: ?std.ArrayList(usize) = null,
    had_error: bool = false,
    panic_mode: bool = false,
    // Used to generate error messages
    parser: *Parser,

    pub fn init(
        gc: *GarbageCollector,
        parser: *Parser,
        type_registry: *TypeRegistry,
        testing: bool,
    ) Self {
        return .{
            .gc = gc,
            .parser = parser,
            .type_registry = type_registry,
            .testing = testing,
        };
    }

    pub fn deinit(_: *Self) void {}

    pub inline fn currentCode(self: *Self) usize {
        return self.current.?.function.?.chunk.code.items.len;
    }

    pub fn generate(self: *Self, root: *FunctionNode) anyerror!?*ObjFunction {
        self.had_error = false;
        self.panic_mode = false;

        if (Config.debug) {
            var out = std.ArrayList(u8).init(self.gc.allocator);
            defer out.deinit();

            try root.node.toJson(&root.node, out.writer());

            try std.io.getStdOut().writer().print("\n{s}", .{out.items});
        }

        const function = try root.node.toByteCode(&root.node, self, null);

        return if (self.had_error) null else function;
    }

    pub fn emit(self: *Self, location: Token, code: u32) !void {
        try self.current.?.function.?.chunk.write(code, location.line);
    }

    pub fn emitTwo(self: *Self, location: Token, a: u8, b: u24) !void {
        try self.emit(location, (@intCast(u32, a) << 24) | @intCast(u32, b));
    }

    // OP_ | arg
    pub fn emitCodeArg(self: *Self, location: Token, code: OpCode, arg: u24) !void {
        try self.emit(location, (@intCast(u32, @enumToInt(code)) << 24) | @intCast(u32, arg));
    }

    // OP_ | a | b
    pub fn emitCodeArgs(self: *Self, location: Token, code: OpCode, a: u8, b: u16) !void {
        try self.emit(location, (@intCast(u32, @enumToInt(code)) << 24) | (@intCast(u32, a) << 16) | (@intCast(u32, b)));
    }

    pub fn emitOpCode(self: *Self, location: Token, code: OpCode) !void {
        try self.emit(location, @intCast(u32, @intCast(u32, @enumToInt(code)) << 24));
    }

    pub fn emitLoop(self: *Self, location: Token, loop_start: usize) !void {
        const offset: usize = self.currentCode() - loop_start + 1;
        if (offset > 16777215) {
            try self.reportError("Loop body to large.");
        }

        try self.emitCodeArg(location, .OP_LOOP, @intCast(u24, offset));
    }

    pub fn emitJump(self: *Self, location: Token, instruction: OpCode) !usize {
        try self.emitCodeArg(location, instruction, 0xffffff);

        return self.currentCode() - 1;
    }

    pub fn patchJumpOrLoop(self: *Self, offset: usize, loop_start: ?usize) !void {
        const original: u32 = self.current.?.function.?.chunk.code.items[offset];
        const instruction: u8 = @intCast(u8, original >> 24);
        const code: OpCode = @intToEnum(OpCode, instruction);

        if (code == .OP_LOOP) { // Patching a continue statement
            assert(loop_start != null);
            const loop_offset: usize = offset - loop_start.? + 1;
            if (loop_offset > 16777215) {
                try self.reportError("Loop body to large.");
            }

            self.current.?.function.?.chunk.code.items[offset] =
                (@intCast(u32, instruction) << 24) | @intCast(u32, loop_offset);
        } else { // Patching a break statement
            try self.patchJump(offset);
        }
    }

    pub fn patchJump(self: *Self, offset: usize) !void {
        assert(offset < self.currentCode());

        const jump: usize = self.currentCode() - offset - 1;

        if (jump > 16777215) {
            try self.reportError("Jump to large.");
        }

        const original: u32 = self.current.?.function.?.chunk.code.items[offset];
        const instruction: u8 = @intCast(u8, original >> 24);

        self.current.?.function.?.chunk.code.items[offset] =
            (@intCast(u32, instruction) << 24) | @intCast(u32, jump);
    }

    pub fn emitList(
        self: *Self,
        location: Token,
    ) !usize {
        try self.emitCodeArg(location, .OP_LIST, 0xffffff);

        return self.currentCode() - 1;
    }

    pub fn patchList(self: *Self, offset: usize, constant: u24) !void {
        const original: u32 = self.current.?.function.?.chunk.code.items[offset];
        const instruction: u8 = @intCast(u8, original >> 24);

        self.current.?.function.?.chunk.code.items[offset] =
            (@intCast(u32, instruction) << 24) | @intCast(u32, constant);
    }

    pub fn emitMap(self: *Self, location: Token) !usize {
        try self.emitCodeArg(location, .OP_MAP, 0xffffff);

        return self.currentCode() - 1;
    }

    pub fn patchMap(self: *Self, offset: usize, map_type_constant: u24) !void {
        const original: u32 = self.current.?.function.?.chunk.code.items[offset];
        const instruction: u8 = @intCast(u8, original >> 24);

        self.current.?.function.?.chunk.code.items[offset] =
            (@intCast(u32, instruction) << 24) | @intCast(u32, map_type_constant);
    }

    pub fn emitReturn(self: *Self, location: Token) !void {
        try self.emitOpCode(location, .OP_VOID);
        try self.emitOpCode(location, .OP_RETURN);
    }

    pub fn emitConstant(self: *Self, location: Token, value: Value) !void {
        try self.emitCodeArg(location, .OP_CONSTANT, try self.makeConstant(value));
    }

    pub fn makeConstant(self: *Self, value: Value) !u24 {
        var constant: u24 = try self.current.?.function.?.chunk.addConstant(null, value);
        if (constant > Chunk.max_constants) {
            try self.reportError("Too many constants in one chunk.");
            return 0;
        }

        return constant;
    }

    pub fn identifierConstant(self: *Self, name: []const u8) !u24 {
        return try self.makeConstant(
            Value{
                .Obj = (try self.gc.copyString(name)).toObj(),
            },
        );
    }

    pub fn report(self: *Self, location: Token, message: []const u8) !void {
        const lines: std.ArrayList([]const u8) = try location.getLines(self.gc.allocator, 3);
        defer lines.deinit();
        var report_line = std.ArrayList(u8).init(self.gc.allocator);
        defer report_line.deinit();
        var writer = report_line.writer();

        try writer.print("", .{});
        var l: usize = if (location.line > 0) location.line - 1 else 0;
        for (lines.items) |line| {
            if (l != location.line) {
                try writer.print("\u{001b}[2m", .{});
            }

            var prefix_len: usize = report_line.items.len;
            try writer.print(" {: >5} |", .{l + 1});
            prefix_len = report_line.items.len - prefix_len;
            try writer.print(" {s}\n\u{001b}[0m", .{line});

            if (l == location.line) {
                try writer.writeByteNTimes(' ', location.column + prefix_len);
                try writer.print("\u{001b}[31m^\u{001b}[0m\n", .{});
            }

            l += 1;
        }
        std.debug.print("{s}:{}:{}: \u{001b}[31mCompile error:\u{001b}[0m {s}\n{s}", .{
            location.script_name,
            location.line + 1,
            location.column + 1,
            message,
            report_line.items,
        });

        if (Config.debug_stop_on_report) {
            unreachable;
        }
    }

    // Unlocated error, should not be used
    fn reportError(self: *Self, message: []const u8) !void {
        if (self.panic_mode) {
            return;
        }

        self.panic_mode = true;
        self.had_error = true;

        try self.report(
            Token{
                .token_type = .Error,
                .source = "",
                .script_name = "",
                .lexeme = "",
                .line = 0,
                .column = 0,
            },
            message,
        );
    }

    pub fn reportErrorAt(self: *Self, token: Token, message: []const u8) !void {
        if (self.panic_mode) {
            return;
        }

        self.panic_mode = true;
        self.had_error = true;

        try self.report(token, message);
    }

    pub fn reportErrorFmt(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) !void {
        var message = std.ArrayList(u8).init(self.gc.allocator);
        defer message.deinit();

        var writer = message.writer();
        try writer.print(fmt, args);

        try self.reportErrorAt(token, message.items);
    }

    pub fn reportTypeCheckAt(self: *Self, expected_type: *ObjTypeDef, actual_type: *ObjTypeDef, message: []const u8, at: Token) !void {
        var error_message = std.ArrayList(u8).init(self.gc.allocator);
        var writer = error_message.writer();

        try writer.print("{s}: expected type `", .{message});
        try expected_type.toString(writer);
        try writer.writeAll("`, got `");
        try actual_type.toString(writer);
        try writer.writeAll("`");

        try self.reportErrorAt(at, error_message.items);
    }

    // Got to the root placeholder and report it
    pub fn reportPlaceholder(self: *Self, placeholder: PlaceholderDef) anyerror!void {
        if (placeholder.parent) |parent| {
            try self.reportPlaceholder(parent.resolved_type.?.Placeholder);
        } else {
            // Should be a root placeholder with a name
            assert(placeholder.name != null);
            try self.reportErrorFmt(placeholder.where, "`{s}` is not defined", .{placeholder.name.?.string});
        }
    }
};
