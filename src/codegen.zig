const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const _chunk = @import("chunk.zig");
const _obj = @import("obj.zig");
const _vm = @import("vm.zig");
const RunFlavor = _vm.RunFlavor;
const _value = @import("value.zig");
const _disassembler = @import("disassembler.zig");
const _parser = @import("parser.zig");
const _node = @import("node.zig");
const _token = @import("token.zig");
const GarbageCollector = @import("memory.zig").GarbageCollector;
const Reporter = @import("reporter.zig");
const BuildOptions = @import("build_options");
const MIRJIT = @import("mirjit.zig");
const ParseNode = _node.ParseNode;
const FunctionNode = _node.FunctionNode;
const ObjFunction = _obj.ObjFunction;
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

    try_should_handle: ?std.AutoHashMap(*ObjTypeDef, Token) = null,
};

pub const CodeGen = struct {
    const Self = @This();

    current: ?*Frame = null,
    gc: *GarbageCollector,
    flavor: RunFlavor,
    // Jump to patch at end of current expression with a optional unwrapping in the middle of it
    opt_jumps: ?std.ArrayList(usize) = null,
    // Used to generate error messages
    parser: *Parser,
    mir_jit: ?*MIRJIT,

    reporter: Reporter,

    pub fn init(
        gc: *GarbageCollector,
        parser: *Parser,
        flavor: RunFlavor,
        mir_jit: ?*MIRJIT,
    ) Self {
        return .{
            .gc = gc,
            .parser = parser,
            .flavor = flavor,
            .reporter = Reporter{
                .allocator = gc.allocator,
                .error_prefix = "Compile",
            },
            .mir_jit = mir_jit,
        };
    }

    pub fn deinit(_: *Self) void {}

    pub inline fn currentCode(self: *Self) usize {
        return self.current.?.function.?.chunk.code.items.len;
    }

    pub fn generate(self: *Self, root: *FunctionNode) anyerror!?*ObjFunction {
        self.reporter.had_error = false;
        self.reporter.panic_mode = false;

        if (BuildOptions.debug) {
            var out = std.ArrayList(u8).init(self.gc.allocator);
            defer out.deinit();

            try root.node.toJson(&root.node, &out.writer());

            try std.io.getStdOut().writer().print("\n{s}", .{out.items});
        }

        const function = try root.node.toByteCode(&root.node, self, null);

        return if (self.reporter.had_error) null else function;
    }

    pub fn emit(self: *Self, location: Token, code: u32) !void {
        try self.current.?.function.?.chunk.write(code, location);
    }

    pub fn emitTwo(self: *Self, location: Token, a: u8, b: u24) !void {
        try self.emit(location, (@as(u32, @intCast(a)) << 24) | @as(u32, @intCast(b)));
    }

    // OP_ | arg
    pub fn emitCodeArg(self: *Self, location: Token, code: OpCode, arg: u24) !void {
        try self.emit(
            location,
            (@as(u32, @intCast(@intFromEnum(code))) << 24) | @as(u32, @intCast(arg)),
        );
    }

    // OP_ | a | b
    pub fn emitCodeArgs(self: *Self, location: Token, code: OpCode, a: u8, b: u16) !void {
        try self.emit(
            location,
            (@as(u32, @intCast(@intFromEnum(code))) << 24) | (@as(u32, @intCast(a)) << 16) | (@as(u32, @intCast(b))),
        );
    }

    pub fn emitOpCode(self: *Self, location: Token, code: OpCode) !void {
        try self.emit(location, @as(u32, @intCast(@intFromEnum(code))) << 24);
    }

    pub fn emitLoop(self: *Self, location: Token, loop_start: usize) !void {
        const offset: usize = self.currentCode() - loop_start + 1;
        if (offset > 16777215) {
            self.reportError(.loop_body_too_large, "Loop body too large.");
        }

        try self.emitCodeArg(location, .OP_LOOP, @as(u24, @intCast(offset)));
    }

    pub fn emitJump(self: *Self, location: Token, instruction: OpCode) !usize {
        try self.emitCodeArg(location, instruction, 0xffffff);

        return self.currentCode() - 1;
    }

    pub fn patchJumpOrLoop(self: *Self, offset: usize, loop_start: ?usize) !void {
        const original: u32 = self.current.?.function.?.chunk.code.items[offset];
        const instruction: u8 = @intCast(original >> 24);
        const code: OpCode = @enumFromInt(instruction);

        if (code == .OP_LOOP) { // Patching a continue statement
            assert(loop_start != null);
            const loop_offset: usize = offset - loop_start.? + 1;
            if (loop_offset > 16777215) {
                self.reportError(.loop_body_too_large, "Loop body too large.");
            }

            self.current.?.function.?.chunk.code.items[offset] =
                (@as(u32, @intCast(instruction)) << 24) | @as(u32, @intCast(loop_offset));
        } else { // Patching a break statement
            self.patchJump(offset);
        }
    }

    pub fn patchJump(self: *Self, offset: usize) void {
        assert(offset < self.currentCode());

        const jump: usize = self.currentCode() - offset - 1;

        if (jump > 16777215) {
            self.reportError(.jump_too_large, "Jump too large.");
        }

        const original: u32 = self.current.?.function.?.chunk.code.items[offset];
        const instruction: u8 = @intCast(original >> 24);

        self.current.?.function.?.chunk.code.items[offset] =
            (@as(u32, @intCast(instruction)) << 24) | @as(u32, @intCast(jump));
    }

    pub fn patchTry(self: *Self, offset: usize) void {
        assert(offset < self.currentCode());

        const jump: usize = self.currentCode();

        if (jump > 16777215) {
            self.reportError(
                .block_too_large,
                "Try block too large.",
            );
        }

        const original: u32 = self.current.?.function.?.chunk.code.items[offset];
        const instruction: u8 = @intCast(original >> 24);

        self.current.?.function.?.chunk.code.items[offset] =
            (@as(u32, @intCast(instruction)) << 24) | @as(u32, @intCast(jump));
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
        const instruction: u8 = @intCast(original >> 24);

        self.current.?.function.?.chunk.code.items[offset] =
            (@as(u32, @intCast(instruction)) << 24) | @as(u32, @intCast(constant));
    }

    pub fn emitMap(self: *Self, location: Token) !usize {
        try self.emitCodeArg(location, .OP_MAP, 0xffffff);

        return self.currentCode() - 1;
    }

    pub fn patchMap(self: *Self, offset: usize, map_type_constant: u24) !void {
        const original: u32 = self.current.?.function.?.chunk.code.items[offset];
        const instruction: u8 = @intCast(original >> 24);

        self.current.?.function.?.chunk.code.items[offset] =
            (@as(u32, @intCast(instruction)) << 24) | @as(u32, @intCast(map_type_constant));
    }

    pub fn emitReturn(self: *Self, location: Token) !void {
        try self.emitOpCode(location, .OP_VOID);
        try self.emitOpCode(location, .OP_RETURN);
    }

    pub fn emitConstant(self: *Self, location: Token, value: Value) !void {
        try self.emitCodeArg(location, .OP_CONSTANT, try self.makeConstant(value));
    }

    pub fn makeConstant(self: *Self, value: Value) !u24 {
        const constant: u24 = try self.current.?.function.?.chunk.addConstant(null, value);
        if (constant > Chunk.max_constants) {
            self.reportError("Too many constants in one chunk.");
            return 0;
        }

        return constant;
    }

    pub fn identifierConstant(self: *Self, name: []const u8) !u24 {
        return try self.makeConstant(
            Value.fromObj((try self.gc.copyString(name)).toObj()),
        );
    }

    // Unlocated error, should not be used
    fn reportError(self: *Self, error_type: Reporter.Error, message: []const u8) void {
        if (self.reporter.panic_mode) {
            return;
        }

        self.reporter.report(
            error_type,
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
};
