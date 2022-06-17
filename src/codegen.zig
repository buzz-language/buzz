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
const Config = @import("./config.zig").Config;
const copyStringRaw = _obj.copyStringRaw;
const Frame = _parser.Frame;
const ParseNode = _node.ParseNode;
const FunctionNode = _node.FunctionNode;
const ObjString = _obj.ObjString;
const ObjFunction = _obj.ObjFunction;
const Global = _parser.Global;
const OpCode = _chunk.OpCode;
const Value = _value.Value;
const Chunk = _chunk.Chunk;
const Token = _token.Token;
const ObjTypeDef = _obj.ObjTypeDef;

pub const CodeGen = struct {
    const Self = @This();

    current: ?*Frame = null,
    allocator: Allocator,
    strings: *std.StringHashMap(*ObjString),
    globals: std.ArrayList(Global),
    testing: bool,
    // Jump to patch at end of current expression with a optional unwrapping in the middle of it
    opt_jumps: ?std.ArrayList(usize) = null,

    pub fn init(allocator: Allocator, strings: *std.StringHashMap(*ObjString), testing: bool) Self {
        return .{
            .allocator = Allocator,
            .strings = strings,
            .testing = testing,
            .globals = std.ArrayList(Global).init(allocator),
        };
    }

    inline fn currentCode(self: *Self) usize {
        return self.current.?.function.?.chunk.code.items.len;
    }

    pub fn generate(self: *Self, root: *FunctionNode) anyerror!*ObjFunction {
        return try root.generate(root.node, self);
    }

    pub fn emit(self: *Self, code: u32) !void {
        try self.current.?.function.?.chunk.write(code, null);
    }

    pub fn emitTwo(self: *Self, a: u8, b: u24) !void {
        try self.emit((@intCast(u32, a) << 24) | @intCast(u32, b));
    }

    // OP_ | arg
    pub fn emitCodeArg(self: *Self, code: OpCode, arg: u24) !void {
        try self.emit((@intCast(u32, @enumToInt(code)) << 24) | @intCast(u32, arg));
    }

    // OP_ | a | b
    pub fn emitCodeArgs(self: *Self, code: OpCode, a: u8, b: u16) !void {
        try self.emit((@intCast(u32, @enumToInt(code)) << 24) | (@intCast(u32, a) << 16) | (@intCast(u32, b)));
    }

    pub fn emitOpCode(self: *Self, code: OpCode) !void {
        try self.emit(@intCast(u32, @intCast(u32, @enumToInt(code)) << 24));
    }

    pub fn emitLoop(self: *Self, loop_start: usize) !void {
        const offset: usize = self.currentCode() - loop_start + 1;
        if (offset > 16777215) {
            try self.reportError("Loop body to large.");
        }

        try self.emitCodeArg(.OP_LOOP, @intCast(u24, offset));
    }

    pub fn emitJump(self: *Self, instruction: OpCode) !usize {
        try self.emitCodeArg(instruction, 0xffffff);

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
        const jump: usize = self.currentCode() - offset - 1;

        if (jump > 16777215) {
            try self.reportError("Jump to large.");
        }

        const original: u32 = self.current.?.function.?.chunk.code.items[offset];
        const instruction: u8 = @intCast(u8, original >> 24);

        self.current.?.function.?.chunk.code.items[offset] =
            (@intCast(u32, instruction) << 24) | @intCast(u32, jump);
    }

    pub fn emitList(self: *Self) !usize {
        try self.emitCodeArg(.OP_LIST, 0xffffff);

        return self.currentCode() - 1;
    }

    pub fn patchList(self: *Self, offset: usize, constant: u24) !void {
        const original: u32 = self.current.?.function.?.chunk.code.items[offset];
        const instruction: u8 = @intCast(u8, original >> 24);

        self.current.?.function.?.chunk.code.items[offset] =
            (@intCast(u32, instruction) << 24) | @intCast(u32, constant);
    }

    pub fn emitMap(self: *Self) !usize {
        try self.emitCodeArg(.OP_MAP, 0xffffff);

        return self.currentCode() - 1;
    }

    pub fn patchMap(self: *Self, offset: usize, map_type_constant: u24) !void {
        const original: u32 = self.current.?.function.?.chunk.code.items[offset];
        const instruction: u8 = @intCast(u8, original >> 24);

        self.current.?.function.?.chunk.code.items[offset] =
            (@intCast(u32, instruction) << 24) | @intCast(u32, map_type_constant);
    }

    pub fn emitReturn(self: *Self) !void {
        try self.emitOpCode(.OP_VOID);
        try self.emitOpCode(.OP_RETURN);
    }

    pub fn emitConstant(self: *Self, value: Value) !void {
        try self.emitCodeArg(.OP_CONSTANT, try self.makeConstant(value));
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
        return try self.makeConstant(Value{ .Obj = (try copyStringRaw(self.strings, self.allocator, name, false)).toObj() });
    }

    pub fn report(self: *Self, token: Token, message: []const u8) !void {
        const lines: std.ArrayList([]const u8) = try self.scanner.?.getLines(self.allocator, if (token.line > 0) token.line - 1 else 0, 3);
        defer lines.deinit();
        var report_line = std.ArrayList(u8).init(self.allocator);
        defer report_line.deinit();
        var writer = report_line.writer();

        try writer.print("\n", .{});
        var l: usize = if (token.line > 0) token.line - 1 else 0;
        for (lines.items) |line| {
            if (l != token.line) {
                try writer.print("\u{001b}[2m", .{});
            }

            var prefix_len: usize = report_line.items.len;
            try writer.print(" {: >5} |", .{l + 1});
            prefix_len = report_line.items.len - prefix_len;
            try writer.print(" {s}\n\u{001b}[0m", .{line});

            if (l == token.line) {
                try writer.writeByteNTimes(' ', token.column - 1 + prefix_len);
                try writer.print("^\n", .{});
            }

            l += 1;
        }
        std.debug.print("{s}:{}:{}: \u{001b}[31mError:\u{001b}[0m {s}\n", .{ report_line.items, token.line + 1, token.column + 1, message });
    }

    pub fn reportErrorAt(self: *Self, token: Token, message: []const u8) !void {
        if (self.parser.panic_mode) {
            return;
        }

        self.parser.panic_mode = true;
        self.parser.had_error = true;

        try self.report(token, message);
    }

    pub fn reportErrorFmt(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) !void {
        var message = std.ArrayList(u8).init(self.allocator);
        defer message.deinit();

        var writer = message.writer();
        try writer.print(fmt, args);

        try self.reportErrorAt(token, message.items);
    }

    pub fn reportTypeCheckAt(self: *Self, expected_type: *ObjTypeDef, actual_type: *ObjTypeDef, message: []const u8, at: Token) !void {
        var expected_str: []const u8 = try expected_type.toString(self.allocator);
        var actual_str: []const u8 = try actual_type.toString(self.allocator);
        var error_message: []u8 = try self.allocator.alloc(u8, expected_str.len + actual_str.len + 200);
        defer {
            self.allocator.free(error_message);
            self.allocator.free(expected_str);
            self.allocator.free(actual_str);
        }

        error_message = try std.fmt.bufPrint(error_message, "{s}: expected type `{s}`, got `{s}`", .{ message, expected_str, actual_str });

        try self.reportErrorAt(at, error_message);
    }
};
