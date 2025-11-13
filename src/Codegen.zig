const std = @import("std");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();

const Chunk = @import("Chunk.zig");
const Ast = @import("Ast.zig");
const obj = @import("obj.zig");
const vm = @import("vm.zig");
const RunFlavor = vm.RunFlavor;
const Value = @import("value.zig").Value;
const Parser = @import("Parser.zig");
const Token = @import("Token.zig");
const GC = @import("GC.zig");
const Reporter = @import("Reporter.zig");
const BuildOptions = @import("build_options");
const JIT = if (!is_wasm) @import("Jit.zig") else void;
const disassembler = @import("disassembler.zig");
const io = @import("io.zig");
const TypeChecker = @import("TypeChecker.zig");

const Self = @This();

pub const Error = error{
    CantCompile,
    UnwrappedNull,
    DivisionByZero,
    OutOfBound,
    ReachedMaximumMemoryUsage,
    WriteFailed,
} || std.mem.Allocator.Error || std.fmt.BufPrintError;

pub const Frame = struct {
    enclosing: ?*Frame = null,
    function_node: Ast.Node.Index,
    function: ?*obj.ObjFunction = null,
    return_counts: bool = false,
    return_emitted: bool = false,
    // Keep track of constants to avoid adding the same more than once to the chunk
    constants: std.AutoHashMapUnmanaged(Value, u24),

    try_should_handle: ?std.AutoHashMapUnmanaged(*obj.ObjTypeDef, Ast.TokenIndex) = null,

    pub fn deinit(self: *Frame, allocator: std.mem.Allocator) void {
        self.constants.deinit(allocator);
        if (self.try_should_handle) |*handle| {
            handle.deinit(allocator);
        }
    }
};

const NodeGen = *const fn (
    self: *Self,
    node: Ast.Node.Index,
    breaks: ?*Breaks,
) Error!?*obj.ObjFunction;

const Break = struct {
    ip: usize, // The op code will tell us if this is a continue or a break statement
    label_node: ?Ast.Node.Index = null,
};

const Breaks = std.ArrayList(Break);

current: ?*Frame = null,
ast: Ast.Slice = undefined,
gc: *GC,
flavor: RunFlavor,
/// Jump to patch at end of current expression with a optional unwrapping in the middle of it
opt_jumps: std.ArrayList(std.ArrayList(usize)) = .{},
/// Used to generate error messages
parser: *Parser,
jit: ?*JIT,
/// Wether we are debugging the program
debugging: bool,

reporter: Reporter,

const generators = [_]?NodeGen{
    null, // AnonymousObjectType,
    generateAs, // As,
    generateAsyncCall, // AsyncCall,
    generateBinary, // Binary,
    generateBlock, // Block,
    generateBlockExpression, // BlockExpression,
    generateBoolean, // Boolean,
    generateBreak, // Break,
    generateCall, // Call,
    generateContinue, // Continue,
    generateDot, // Dot,
    generateDoUntil, // DoUntil,
    generateEnum, // Enum,
    generateExport, // Export,
    generateExpression, // Expression,
    null, // FiberType,
    generateFloat, // Double,
    generateFor, // For,
    generateForceUnwrap, // ForceUnwrap,
    generateForEach, // ForEach,
    generateFunction, // Function,
    null, // FunctionType,
    generateFunDeclaration, // FunDeclaration,
    generateGenericResolve, // GenericResolve,
    null, // GenericResolveType,
    null, // GenericType,
    generateGrouping, // Grouping,
    generateIf, // If,
    generateImport, // Import,
    generateInteger, // Integer,
    generateIs, // Is,
    generateList, // List,
    null, // ListType,
    generateMap, // Map,
    null, // MapType,
    null, // Namespace,
    generateNamedVariable, // NamedVariable,
    generateNull, // Null,
    generateObjectDeclaration, // ObjectDeclaration,
    generateObjectInit, // ObjectInit,
    generateOut, // Out,
    generatePattern, // Pattern,
    generateProtocolDeclaration, // ProtocolDeclaration,
    generateRange, // Range,
    generateResolve, // Resolve,
    generateResume, // Resume,
    generateReturn, // Return,
    null, // SimpleType,
    generateString, // String,
    generateStringLiteral, // StringLiteral,
    generateSubscript, // Subscript,
    generateThrow, // Throw,
    generateTry, // Try,
    generateTypeExpression, // TypeExpression,
    generateTypeOfExpression, // TypeOfExpression,
    generateUnary, // Unary,
    generateUnwrap, // Unwrap,
    null, // UserType,
    generateVarDeclaration, // VarDeclaration,
    generateVoid, // Void,
    generateWhile, // While,
    generateYield, // Yield,
    generateZdef, // Zdef,
};

pub fn init(
    gc: *GC,
    parser: *Parser,
    flavor: RunFlavor,
    jit: ?*JIT,
    debugging: bool,
) Self {
    return .{
        .gc = gc,
        .parser = parser,
        .flavor = flavor,
        .reporter = Reporter{
            .allocator = gc.allocator,
            .error_prefix = "Compile",
            .collect = flavor == .Ast,
        },
        .jit = jit,
        .debugging = debugging,
    };
}

pub fn deinit(self: *Self) void {
    self.reporter.deinit();
}

pub inline fn currentCode(self: *Self) usize {
    return self.current.?.function.?.chunk.code.items.len;
}

pub fn generate(self: *Self, ast: Ast.Slice) Error!?*obj.ObjFunction {
    self.ast = ast;
    self.reporter.last_error = null;
    self.reporter.panic_mode = false;

    const function = self.generateNode(self.ast.root.?, null);

    return if (self.reporter.last_error != null) null else function;
}

pub fn emit(self: *Self, location: Ast.TokenIndex, code: u32) !void {
    try self.current.?.function.?.chunk.write(code, location);
}

pub fn emitTwo(self: *Self, location: Ast.TokenIndex, a: u8, b: u24) !void {
    try self.emit(location, (@as(u32, @intCast(a)) << 24) | @as(u32, @intCast(b)));
}

// OP_ | arg
pub fn emitCodeArg(self: *Self, location: Ast.TokenIndex, code: Chunk.OpCode, arg: u24) !void {
    try self.emit(
        location,
        (@as(u32, @intCast(@intFromEnum(code))) << 24) | @as(u32, @intCast(arg)),
    );
}

// OP_ | a | b
pub fn emitCodeArgs(self: *Self, location: Ast.TokenIndex, code: Chunk.OpCode, a: u8, b: u16) !void {
    try self.emit(
        location,
        (@as(u32, @intCast(@intFromEnum(code))) << 24) | (@as(u32, @intCast(a)) << 16) | (@as(u32, @intCast(b))),
    );
}

pub fn emitOpCode(self: *Self, location: Ast.TokenIndex, code: Chunk.OpCode) !void {
    try self.emit(location, @as(u32, @intCast(@intFromEnum(code))) << 24);
}

pub fn emitLoop(self: *Self, location: Ast.TokenIndex, loop_start: usize) !void {
    const offset: usize = self.currentCode() - loop_start + 1;
    if (offset > std.math.maxInt(u24)) {
        self.reportError(.loop_body_too_large, "Loop body too large.");
    }

    try self.emitCodeArg(location, .OP_LOOP, @as(u24, @intCast(offset)));
}

pub fn emitJump(self: *Self, location: Ast.TokenIndex, instruction: Chunk.OpCode) !usize {
    try self.emitCodeArg(location, instruction, 0xffffff);

    return self.currentCode() - 1;
}

pub fn patchJumpOrLoop(self: *Self, offset: usize, loop_start: ?usize) !void {
    const original = self.current.?.function.?.chunk.code.items[offset];
    const instruction: u8 = @intCast(original >> 24);
    const code: Chunk.OpCode = @enumFromInt(instruction);

    if (code == .OP_LOOP) { // Patching a continue statement
        std.debug.assert(loop_start != null);
        const loop_offset: usize = offset - loop_start.? + 1;
        if (loop_offset > std.math.maxInt(u24)) {
            self.reportError(.loop_body_too_large, "Loop body too large.");
        }

        self.current.?.function.?.chunk.code.items[offset] =
            (@as(u32, @intCast(instruction)) << 24) | @as(u32, @intCast(loop_offset));
    } else { // Patching a break statement
        self.patchJump(offset);
    }
}

fn patchBreaks(self: *Self, breaks: *Breaks, previous_breaks: ?*Breaks, loop_node: Ast.Node.Index, loop_start: usize) !void {
    for (breaks.items) |brk| {
        if (brk.label_node == null or brk.label_node.? == loop_node) {
            try self.patchJumpOrLoop(brk.ip, loop_start);
        } else if (previous_breaks) |brks| { // The break/continue if for an upper scope
            try brks.append(self.gc.allocator, brk);
        } else {
            unreachable; // Should not happen: we search for the scope during parsing
        }
    }
}

pub fn patchJump(self: *Self, offset: usize) void {
    std.debug.assert(offset < self.currentCode());

    const jump = self.currentCode() - offset - 1;

    if (jump > std.math.maxInt(u24)) {
        self.reportError(.jump_too_large, "Jump too large.");
    }

    const original = self.current.?.function.?.chunk.code.items[offset];
    const instruction: u8 = @intCast(original >> 24);

    self.current.?.function.?.chunk.code.items[offset] =
        (@as(u32, @intCast(instruction)) << 24) | @as(u32, @intCast(jump));
}

pub fn patchTryOrJit(self: *Self, offset: usize) void {
    std.debug.assert(offset < self.currentCode());

    const jump = self.currentCode();

    if (jump > std.math.maxInt(u24)) {
        self.reportError(
            .block_too_large,
            "Try block too large.",
        );
    }

    const original = self.current.?.function.?.chunk.code.items[offset];
    const instruction: u8 = @intCast(original >> 24);

    self.current.?.function.?.chunk.code.items[offset] =
        (@as(u32, @intCast(instruction)) << 24) | @as(u32, @intCast(jump));
}

pub fn emitReturn(self: *Self, location: Ast.TokenIndex) !void {
    try self.OP_VOID(location);
    try self.OP_RETURN(location);
}

pub fn emitConstant(self: *Self, location: Ast.TokenIndex, value: Value) !void {
    try self.emitCodeArg(location, .OP_CONSTANT, try self.makeConstant(value));
}

pub fn makeConstant(self: *Self, value: Value) !u24 {
    if (self.current.?.constants.get(value)) |idx| {
        return idx;
    }

    const constant = try self.current.?.function.?.chunk.addConstant(null, value);
    try self.current.?.constants.put(self.gc.allocator, value, constant);
    if (constant > Chunk.max_constants) {
        self.reportError("Too many constants in one chunk.");
        return 0;
    }

    return constant;
}

pub fn identifierConstant(self: *Self, name: []const u8) !u24 {
    return try self.makeConstant(
        (try self.gc.copyString(name)).toValue(),
    );
}

// Unlocated error, should not be used
fn reportError(self: *Self, error_type: Reporter.Error, comptime message: []const u8) void {
    @branchHint(.cold);

    if (self.reporter.panic_mode) {
        return;
    }

    self.reporter.report(
        error_type,
        Token{
            .tag = .Error,
            .source = "",
            .script_name = "",
            .lexeme = "",
            .line = 0,
            .column = 0,
        },
        Token{
            .tag = .Error,
            .source = "",
            .script_name = "",
            .lexeme = "",
            .line = 0,
            .column = 0,
        },
        message,
    );
}

fn synchronize(self: *Self, node: Ast.Node.Index) bool {
    if (self.reporter.panic_mode) {
        switch (self.ast.nodes.items(.tag)[node]) {
            .ObjectDeclaration,
            .Enum,
            .FunDeclaration,
            .If,
            .While,
            .DoUntil,
            .For,
            .ForEach,
            .Return,
            .VarDeclaration,
            .Throw,
            .Break,
            .Continue,
            .Export,
            .Import,
            => {
                self.reporter.panic_mode = false;
                return false;
            },
            else => {},
        }

        return true;
    }

    return false;
}

fn patchOptJumps(self: *Self, node: Ast.Node.Index) !void {
    const location = self.ast.nodes.items(.location)[node];

    if (self.ast.nodes.items(.patch_opt_jumps)[node]) {
        std.debug.assert(self.reporter.last_error != null or self.opt_jumps.items.len > 0);

        if (self.opt_jumps.items.len == 0) {
            return;
        }

        // Hope over OP_POP if actual value
        const njump: usize = try self.emitJump(location, .OP_JUMP);

        for (self.opt_jumps.items[self.opt_jumps.items.len - 1].items) |jump| {
            self.patchJump(jump);
        }
        // If aborted by a null optional, will result in null on the stack
        try self.OP_POP(location);

        self.patchJump(njump);

        var popped = self.opt_jumps.pop().?;
        popped.deinit(self.gc.allocator);
    }
}

fn endScope(self: *Self, node: Ast.Node.Index) Error!void {
    const location = self.ast.nodes.items(.location)[node];

    if (self.ast.nodes.items(.ends_scope)[node]) |closing| {
        for (closing) |cls| {
            try self.emitOpCode(location, cls.opcode);
            try self.OP_DBG_LOCAL_EXIT(location, cls.slot);
        }
    }
}

fn generateNode(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    if (self.synchronize(node)) {
        return null;
    }

    _ = try TypeChecker.check(
        self.ast,
        &self.reporter,
        self.gc,
        if (self.current) |current| current.function_node else null,
        node,
    );

    if (Self.generators[@intFromEnum(self.ast.nodes.items(.tag)[node])]) |generator| {
        return generator(self, node, breaks);
    }

    return null;
}

fn nodeValue(self: *Self, node: Ast.Node.Index) Error!?Value {
    const value = &self.ast.nodes.items(.value)[node];

    if (value.* == null and self.ast.isConstant(node)) {
        value.* = try self.ast.typeCheckAndToValue(
            node,
            &self.reporter,
            self.gc,
        );
    }

    return value.*;
}

fn generateAs(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const node_location = locations[node];
    const components = self.ast.nodes.items(.components)[node].As;

    const constant = try self.ast.typeCheckAndToValue(
        components.constant,
        &self.reporter,
        self.gc,
    );

    std.debug.assert(constant.isObj() and constant.obj().obj_type == .Type);

    _ = try self.generateNode(components.left, breaks);

    try self.OP_COPY(locations[components.left]);
    try self.OP_CONSTANT(node_location, constant);
    try self.OP_IS(node_location);
    try self.OP_NOT(node_location);
    const jump = try self.OP_JUMP_IF_FALSE(node_location);
    try self.OP_POP(node_location);
    try self.OP_POP(node_location);
    try self.OP_NULL(node_location);
    const jump_over = try self.OP_JUMP(node_location);
    self.patchJump(jump);
    try self.OP_POP(node_location);
    self.patchJump(jump_over);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateAsyncCall(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const type_defs = self.ast.nodes.items(.type_def);
    const node_location = locations[node];
    const type_def = type_defs[node].?;
    const call_node = self.ast.nodes.items(.components)[node].AsyncCall;

    // Push fiber type as constant (we only need it if the fiber is printed out)
    // Should not interfere with local counts since OP_FIBER will consume it right away
    try self.emitConstant(
        node_location,
        type_def.toValue(),
    );

    _ = try self.generateNode(call_node, breaks);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateBinary(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].Binary;

    const locations = self.ast.nodes.items(.location);
    const type_defs = self.ast.nodes.items(.type_def);
    const left_type = type_defs[components.left].?;

    switch (components.operator) {
        .QuestionQuestion => {
            _ = try self.generateNode(components.left, breaks);

            const end_jump: usize = try self.OP_JUMP_IF_NOT_NULL(locations[node]);
            try self.OP_POP(locations[node]);

            _ = try self.generateNode(components.right, breaks);

            self.patchJump(end_jump);
        },
        .Ampersand => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_BAND(locations[node]);
        },
        .Bor => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_BOR(locations[node]);
        },
        .Xor => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_XOR(locations[node]);
        },
        .ShiftLeft => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_SHL(locations[node]);
        },
        .ShiftRight => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_SHR(locations[node]);
        },
        .Greater => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_GREATER(locations[node]);
        },
        .Less => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_LESS(locations[node]);
        },
        .GreaterEqual => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_LESS(locations[node]);
            try self.OP_NOT(locations[node]);
        },
        .LessEqual => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_GREATER(locations[node]);
            try self.OP_NOT(locations[node]);
        },
        .BangEqual => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_EQUAL(locations[node]);
            try self.OP_NOT(locations[node]);
        },
        .EqualEqual => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_EQUAL(locations[node]);
        },
        .Plus => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.emitOpCode(locations[node], switch (left_type.def_type) {
                .String => .OP_ADD_STRING,
                .List => .OP_ADD_LIST,
                .Map => .OP_ADD_MAP,
                .Integer => .OP_ADD_I,
                .Double => .OP_ADD_F,
                else => other: {
                    std.debug.assert(self.reporter.last_error != null);

                    break :other .OP_ADD_I;
                },
            });
        },
        .Minus => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);

            if (left_type.def_type == .Integer) {
                try self.OP_SUBTRACT_I(locations[node]);
            } else {
                try self.OP_SUBTRACT_F(locations[node]);
            }
        },
        .Star => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);

            if (left_type.def_type == .Integer) {
                try self.OP_MULTIPLY_I(locations[node]);
            } else {
                try self.OP_MULTIPLY_F(locations[node]);
            }
        },
        .Slash => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);

            if (left_type.def_type == .Integer) {
                try self.OP_DIVIDE_I(locations[node]);
            } else {
                try self.OP_DIVIDE_F(locations[node]);
            }
        },
        .Percent => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);

            if (left_type.def_type == .Integer) {
                try self.OP_MOD_I(locations[node]);
            } else {
                try self.OP_MOD_F(locations[node]);
            }
        },
        .And => {
            _ = try self.generateNode(components.left, breaks);

            const end_jump: usize = try self.OP_JUMP_IF_FALSE(locations[node]);
            try self.OP_POP(locations[node]);

            _ = try self.generateNode(components.right, breaks);

            self.patchJump(end_jump);
        },
        .Or => {
            _ = try self.generateNode(components.left, breaks);

            const else_jump: usize = try self.OP_JUMP_IF_FALSE(locations[node]);
            const end_jump: usize = try self.OP_JUMP(locations[node]);

            self.patchJump(else_jump);
            try self.OP_POP(locations[node]);

            _ = try self.generateNode(components.right, breaks);

            self.patchJump(end_jump);
        },
        else => unreachable,
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateBlock(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const tags = self.ast.nodes.items(.tag);

    var seen_return = false;
    for (self.ast.nodes.items(.components)[node].Block) |statement| {
        if (seen_return) {
            self.reporter.warnFmt(
                .code_after_return,
                self.ast.tokens.get(
                    self.ast.nodes.items(.location)[statement],
                ),
                self.ast.tokens.get(
                    self.ast.nodes.items(.end_location)[statement],
                ),
                "Code after return statement will never be reached",
                .{},
            );

            // No need to generate following statements
            break;
        }

        _ = try self.generateNode(statement, breaks);

        seen_return = !seen_return and tags[statement] == .Return;
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateBlockExpression(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    for (self.ast.nodes.items(.components)[node].BlockExpression) |statement| {
        _ = try self.generateNode(statement, breaks);
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateOut(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    _ = try self.generateNode(self.ast.nodes.items(.components)[node].Out, breaks);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateBoolean(self: *Self, node: Ast.Node.Index, _: ?*Breaks) Error!?*obj.ObjFunction {
    try self.emitOpCode(
        self.ast.nodes.items(.location)[node],
        if (self.ast.nodes.items(.components)[node].Boolean) .OP_TRUE else .OP_FALSE,
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateBreak(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    // Close scope(s), then jump
    try self.endScope(node);
    try breaks.?.append(
        self.gc.allocator,
        .{
            .ip = try self.OP_JUMP(self.ast.nodes.items(.location)[node]),
            .label_node = self.ast.nodes.items(.components)[node].Break.destination,
        },
    );

    try self.patchOptJumps(node);

    return null;
}

fn generateContinue(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    // Close scope(s), then jump
    try self.endScope(node);
    try breaks.?.append(
        self.gc.allocator,
        .{
            .ip = try self.emitJump(
                self.ast.nodes.items(.location)[node],
                .OP_LOOP,
            ),
            .label_node = self.ast.nodes.items(.components)[node].Continue.destination,
        },
    );

    try self.patchOptJumps(node);

    return null;
}

fn generateCall(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const type_defs = self.ast.nodes.items(.type_def);
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);
    const node_components = self.ast.nodes.items(.components);
    const lexemes = self.ast.tokens.items(.lexeme);

    const components = node_components[node].Call;

    const callee_type_def = type_defs[components.callee].?;

    // This is not a call but an Enum(value)
    if (callee_type_def.def_type == .Enum) {
        const value = components.arguments[0].value;

        _ = try self.generateNode(components.callee, breaks);
        _ = try self.generateNode(value, breaks);
        try self.OP_GET_ENUM_CASE_FROM_VALUE(locations[value]);

        try self.patchOptJumps(node);
        try self.endScope(node);

        return null;
    }

    // Find out if call is invoke or regular call
    var invoked = false;
    var invoked_on: ?obj.ObjTypeDef.Type = null;

    if (self.ast.nodes.items(.tag)[components.callee] == .Dot) {
        const dot = node_components[components.callee].Dot;
        const field_accessed = type_defs[dot.callee].?;

        if (field_accessed.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, field_accessed.resolved_type.?.Placeholder);
        }

        invoked = field_accessed.def_type != .Object;
        invoked_on = field_accessed.def_type;
    }

    if (!invoked and invoked_on == null) {
        _ = try self.generateNode(components.callee, breaks);
    }

    const callee_type = switch (self.ast.nodes.items(.tag)[components.callee]) {
        .Dot => node_components[components.callee].Dot.member_type_def,
        else => type_defs[components.callee],
    };

    const yield_type = callee_type.?.resolved_type.?.Function.yield_type;

    // Function being called and current function should have matching yield type unless the current function is an entrypoint
    // We do this type checking here because we need access to the current function node
    if (!components.is_async) {
        const current_function_typedef = type_defs[self.current.?.function_node].?.resolved_type.?.Function;
        const current_function_type = current_function_typedef.function_type;
        const current_function_yield_type = current_function_typedef.yield_type;
        switch (current_function_type) {
            // Event though a function can call a yieldable function without wraping it in a fiber, the function itself could be called in a fiber
            .Function, .Method, .Anonymous => {
                if (!current_function_yield_type.eql(yield_type)) {
                    self.reporter.reportTypeCheck(
                        .yield_type,
                        self.ast.tokens.get(locations[self.current.?.function_node]),
                        self.ast.tokens.get(end_locations[self.current.?.function_node]),
                        current_function_yield_type,
                        self.ast.tokens.get(locations[node]),
                        self.ast.tokens.get(end_locations[node]),
                        yield_type,
                        "Bad function yield type",
                    );
                }
            },
            else => {},
        }
    }

    // Arguments
    const args = callee_type.?.resolved_type.?.Function.parameters;
    const defaults = callee_type.?.resolved_type.?.Function.defaults;
    const arg_keys = args.keys();
    const arg_count = arg_keys.len;

    var missing_arguments = std.StringArrayHashMapUnmanaged(usize).empty;
    defer missing_arguments.deinit(self.gc.allocator);
    for (arg_keys, 0..) |arg_name, pindex| {
        try missing_arguments.put(
            self.gc.allocator,
            arg_name.string,
            pindex,
        );
    }

    // First push on the stack arguments has they are parsed
    var needs_reorder = false;
    for (components.arguments, 0..) |argument, index| {
        if (index >= arg_count) {
            break;
        }

        const arg_key = if (argument.name) |arg_name|
            try self.gc.copyString(lexemes[arg_name])
        else
            null;
        const actual_arg_key = if (index == 0 and arg_key == null)
            arg_keys[0]
        else
            arg_key.?;

        const ref_index = args.getIndex(actual_arg_key);
        if (index != ref_index) {
            needs_reorder = true;
        }

        _ = missing_arguments.orderedRemove(actual_arg_key.string);
        _ = try self.generateNode(argument.value, breaks);
    }

    // Argument order reference
    var arguments_order_ref = std.ArrayList([]const u8).empty;
    defer arguments_order_ref.deinit(self.gc.allocator);
    for (components.arguments) |arg| {
        try arguments_order_ref.append(
            self.gc.allocator,
            if (arg.name) |name|
                lexemes[name]
            else
                "$",
        );
    }

    // Push default arguments
    if (missing_arguments.count() > 0) {
        var tmp_missing_arguments = try missing_arguments.clone(self.gc.allocator);
        defer tmp_missing_arguments.deinit(self.gc.allocator);
        const missing_keys = tmp_missing_arguments.keys();
        for (missing_keys) |missing_key| {
            if (defaults.get(try self.gc.copyString(missing_key))) |default| {
                try self.emitConstant(locations[node], default);
                try self.OP_CLONE(locations[node]);

                try arguments_order_ref.append(self.gc.allocator, missing_key);
                _ = missing_arguments.orderedRemove(missing_key);
                needs_reorder = true;
            }
        }
    }

    // Reorder arguments (don't bother is something failed before)
    if (self.reporter.last_error == null and needs_reorder) {
        // Until ordered
        while (true) {
            var ordered = true;
            for (arguments_order_ref.items, 0..) |arg_key, index| {
                const actual_arg_key = if (index == 0 and std.mem.eql(u8, arg_key, "$"))
                    args.keys()[0].string
                else
                    arg_key;
                const correct_index = args.getIndex(try self.gc.copyString(actual_arg_key)) orelse 0;

                if (correct_index != index) {
                    ordered = false;

                    try self.OP_SWAP(
                        locations[node],
                        @intCast(arg_count - index - 1),
                        @intCast(arg_count - correct_index - 1),
                    );

                    // Switch it in the reference
                    const temp = arguments_order_ref.items[index];
                    arguments_order_ref.items[index] = arguments_order_ref.items[correct_index];
                    arguments_order_ref.items[correct_index] = temp;

                    // Stop (so we can take the swap into account) and try again
                    break;
                }
            }

            if (ordered) break;
        }
    }

    // Catch clause
    const error_types = callee_type.?.resolved_type.?.Function.error_types;
    if (components.catch_default) |catch_default| {
        if (error_types != null and error_types.?.len > 0) {
            _ = try self.generateNode(catch_default, breaks);
        }
    } else if (error_types) |errors| {
        if (self.current.?.enclosing != null and self.current.?.function.?.type_def.resolved_type.?.Function.function_type != .Test) {
            var handles_any = false;
            var not_handled = std.ArrayList(*obj.ObjTypeDef).empty;
            defer not_handled.deinit(self.gc.allocator);
            for (errors) |error_type| {
                if (error_type.def_type == .Void) {
                    continue;
                }

                var handled = false;

                if (self.current.?.function.?.type_def.resolved_type.?.Function.error_types) |handled_types| {
                    for (handled_types) |handled_type| {
                        if (error_type.eql(handled_type)) {
                            handled = true;
                            break;
                        }

                        if (handled_type.def_type == .Any) {
                            handles_any = true;
                            break;
                        }
                    }
                }

                if (!handled) {
                    if (self.current.?.try_should_handle != null) {
                        try self.current.?.try_should_handle.?.put(
                            self.gc.allocator,
                            error_type,
                            locations[components.callee],
                        );
                    } else {
                        try not_handled.append(self.gc.allocator, error_type);
                    }
                }

                if (handles_any) {
                    not_handled.clearAndFree(self.gc.allocator);
                    break;
                }
            }

            for (not_handled.items) |error_type| {
                const error_str = try error_type.toStringAlloc(self.gc.allocator, false);
                defer self.gc.allocator.free(error_str);

                self.reporter.reportErrorFmt(
                    .error_not_handled,
                    self.ast.tokens.get(locations[node]),
                    self.ast.tokens.get(end_locations[node]),
                    "Error `{s}` is not handled",
                    .{
                        error_str,
                    },
                );
            }
        }
    }

    // This is an async call, create a fiber
    if (components.is_async) {
        // We emit OP_FIBER, the vm will then read the next instruction to get the info about the call
        // and create the fiber
        try self.OP_FIBER(locations[node]);
    }

    // Normal call/invoke
    if (invoked) {
        const callee_def_type = type_defs[node_components[components.callee].Dot.callee].?.def_type;
        const member_lexeme = lexemes[node_components[components.callee].Dot.identifier];

        if (callee_def_type == .ObjectInstance) {
            const fields = type_defs[node_components[components.callee].Dot.callee].?
                .resolved_type.?.ObjectInstance.of
                .resolved_type.?.Object.fields;

            const field = fields.get(member_lexeme).?;

            try self.emitCodeArg(
                locations[node],
                if (components.tail_call and field.method)
                    .OP_INSTANCE_TAIL_INVOKE
                else if (field.method)
                    .OP_INSTANCE_INVOKE
                else if (components.tail_call)
                    .OP_TAIL_CALL_INSTANCE_PROPERTY
                else
                    .OP_CALL_INSTANCE_PROPERTY,
                @intCast(field.index),
            );
        } else if (callee_def_type == .ProtocolInstance) {
            try self.emitCodeArg(
                locations[node],
                if (components.tail_call)
                    .OP_PROTOCOL_TAIL_INVOKE
                else
                    .OP_PROTOCOL_INVOKE,
                try self.identifierConstant(member_lexeme),
            );
        } else {
            try self.emitCodeArg(
                locations[node],
                switch (callee_def_type) {
                    .String => .OP_STRING_INVOKE,
                    .Pattern => .OP_PATTERN_INVOKE,
                    .Fiber => .OP_FIBER_INVOKE,
                    .List => .OP_LIST_INVOKE,
                    .Map => .OP_MAP_INVOKE,
                    .Range => .OP_RANGE_INVOKE,
                    else => unexpected: {
                        std.debug.assert(self.reporter.last_error != null);
                        break :unexpected .OP_INSTANCE_INVOKE;
                    },
                },
                @intCast(
                    switch (callee_def_type) {
                        .String => obj.ObjString.members_name.get(member_lexeme).?,
                        .Pattern => obj.ObjPattern.members_name.get(member_lexeme).?,
                        .Fiber => obj.ObjFiber.members_name.get(member_lexeme).?,
                        .List => obj.ObjList.members_name.get(member_lexeme).?,
                        .Map => obj.ObjMap.members_name.get(member_lexeme).?,
                        .Range => obj.ObjRange.members_name.get(member_lexeme).?,
                        else => unexpected: {
                            std.debug.assert(self.reporter.last_error != null);
                            break :unexpected 0;
                        },
                    },
                ),
            );
        }
    }

    if (!invoked) {
        try self.emitCodeArgs(
            locations[node],
            if (components.tail_call) .OP_TAIL_CALL else .OP_CALL,
            @intCast(arguments_order_ref.items.len),
            if (components.catch_default != null) 1 else 0,
        );
    } else {
        try self.emitTwo(
            locations[node],
            if (invoked_on != null and invoked_on.? != .ObjectInstance and invoked_on.? != .ProtocolInstance)
                @as(u8, @intCast(arg_count)) + 1
            else
                @as(u8, @intCast(arg_count)),
            if (components.catch_default != null) 1 else 0,
        );
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateDot(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const node_components = self.ast.nodes.items(.components);
    const type_defs = self.ast.nodes.items(.type_def);
    const locations = self.ast.nodes.items(.location);
    const tags = self.ast.tokens.items(.tag);

    const components = node_components[node].Dot;
    const identifier_lexeme = self.ast.tokens.items(.lexeme)[components.identifier];

    _ = try self.generateNode(components.callee, breaks);

    const callee_type = type_defs[components.callee] orelse self.gc.type_registry.any_type;

    const get_code: ?Chunk.OpCode = switch (callee_type.def_type) {
        .Object => .OP_GET_OBJECT_PROPERTY,
        .ObjectInstance => .OP_GET_INSTANCE_PROPERTY,
        .ProtocolInstance => .OP_GET_PROTOCOL_METHOD,
        .ForeignContainer => .OP_GET_FCONTAINER_INSTANCE_PROPERTY,
        .List => .OP_GET_LIST_PROPERTY,
        .Map => .OP_GET_MAP_PROPERTY,
        .String => .OP_GET_STRING_PROPERTY,
        .Pattern => .OP_GET_PATTERN_PROPERTY,
        .Fiber => .OP_GET_FIBER_PROPERTY,
        .Range => .OP_GET_RANGE_PROPERTY,
        else => null,
    };

    switch (callee_type.def_type) {
        .Fiber, .Pattern, .String => {
            if (components.member_kind == .Call) {
                try self.OP_COPY(locations[node]);
                _ = try self.generateNode(components.value_or_call_or_enum.Call, breaks);
            } else { // Expression
                std.debug.assert(components.member_kind != .Value);
                try self.emitCodeArg(
                    locations[node],
                    get_code.?,
                    switch (callee_type.def_type) {
                        .Fiber => @intCast(obj.ObjFiber.members_name.get(identifier_lexeme).?),
                        .Pattern => @intCast(obj.ObjPattern.members_name.get(identifier_lexeme).?),
                        .String => @intCast(obj.ObjString.members_name.get(identifier_lexeme).?),
                        else => unreachable,
                    },
                );
            }
        },
        .ForeignContainer, .ObjectInstance, .Object => {
            const field_name = self.ast.tokens.items(.lexeme)[components.identifier];
            const field = switch (callee_type.def_type) {
                .ObjectInstance => callee_type.resolved_type.?.ObjectInstance.of
                    .resolved_type.?.Object
                    .fields
                    .get(field_name),
                .Object => callee_type.resolved_type.?.Object.fields
                    .get(field_name),
                else => null,
            };
            const field_index = if (field) |f|
                f.index
            else fcontainer: {
                std.debug.assert(callee_type.def_type == .ForeignContainer);
                break :fcontainer callee_type.resolved_type.?.ForeignContainer.fields.getIndex(field_name).?;
            };

            switch (components.member_kind) {
                .Value => {
                    const value = components.value_or_call_or_enum.Value.value;
                    const assign_token = components.value_or_call_or_enum.Value.assign_token;
                    var value_type_def = type_defs[value].?;
                    if (value_type_def.def_type == .Placeholder) {
                        self.reporter.reportPlaceholder(self.ast, value_type_def.resolved_type.?.Placeholder);
                    }

                    switch (tags[assign_token]) {
                        .PlusEqual,
                        .MinusEqual,
                        .StarEqual,
                        .SlashEqual,
                        .ShiftRightEqual,
                        .ShiftLeftEqual,
                        .XorEqual,
                        .BorEqual,
                        .BnotEqual,
                        .AmpersandEqual,
                        .PercentEqual,
                        => {
                            // Copy because OP_GET_INSTANCE_... will consume the subject
                            try self.OP_COPY(locations[node]);

                            try self.emitCodeArg(
                                locations[node],
                                if (callee_type.def_type == .ObjectInstance and field.?.method)
                                    .OP_GET_INSTANCE_METHOD
                                else
                                    get_code.?,
                                @intCast(field_index),
                            );
                        },
                        else => {},
                    }

                    _ = try self.generateNode(value, breaks);

                    switch (tags[assign_token]) {
                        .PlusEqual => switch (type_defs[value].?.def_type) {
                            .Integer => try self.OP_ADD_I(locations[value]),
                            .Double => try self.OP_ADD_F(locations[value]),
                            .List => try self.OP_ADD_LIST(locations[value]),
                            .Map => try self.OP_ADD_MAP(locations[value]),
                            .String => try self.OP_ADD_STRING(locations[value]),
                            else => {},
                        },
                        .MinusEqual => switch (type_defs[value].?.def_type) {
                            .Integer => try self.OP_SUBTRACT_I(locations[value]),
                            .Double => try self.OP_SUBTRACT_F(locations[value]),
                            else => {},
                        },
                        .StarEqual => switch (type_defs[value].?.def_type) {
                            .Integer => try self.OP_MULTIPLY_I(locations[value]),
                            .Double => try self.OP_MULTIPLY_F(locations[value]),
                            else => {},
                        },
                        .SlashEqual => switch (type_defs[value].?.def_type) {
                            .Integer => try self.OP_DIVIDE_I(locations[value]),
                            .Double => try self.OP_DIVIDE_F(locations[value]),
                            else => {},
                        },
                        .ShiftRightEqual => try self.OP_SHR(locations[value]),
                        .ShiftLeftEqual => try self.OP_SHL(locations[value]),
                        .XorEqual => try self.OP_XOR(locations[value]),
                        .BorEqual => try self.OP_BOR(locations[value]),
                        .AmpersandEqual => try self.OP_BAND(locations[value]),
                        .PercentEqual => switch (type_defs[value].?.def_type) {
                            .Integer => try self.OP_MOD_I(locations[value]),
                            .Double => try self.OP_MOD_F(locations[value]),
                            else => {},
                        },
                        else => {},
                    }

                    try self.emitCodeArg(
                        locations[node],
                        switch (callee_type.def_type) {
                            .ObjectInstance => .OP_SET_INSTANCE_PROPERTY,
                            .ForeignContainer => .OP_SET_FCONTAINER_INSTANCE_PROPERTY,
                            .Object => .OP_SET_OBJECT_PROPERTY,
                            else => unreachable,
                        },
                        @intCast(field_index),
                    );
                },
                .Call => {
                    // Static call
                    if (callee_type.def_type == .Object) {
                        try self.emitCodeArg(
                            locations[node],
                            get_code.?,
                            @intCast(field.?.index),
                        );
                    }

                    _ = try self.generateNode(components.value_or_call_or_enum.Call, breaks);
                },
                .Ref => try self.emitCodeArg(
                    locations[node],
                    if (callee_type.def_type == .ObjectInstance and field.?.method)
                        .OP_GET_INSTANCE_METHOD
                    else
                        get_code.?,
                    @intCast(field_index),
                ),
                else => unreachable,
            }
        },
        .ProtocolInstance => {
            if (components.member_kind == .Call) {
                _ = try self.generateNode(components.value_or_call_or_enum.Call, breaks);
            } else {
                std.debug.assert(components.member_kind == .Ref);
                try self.emitCodeArg(
                    locations[node],
                    get_code.?,
                    try self.identifierConstant(identifier_lexeme),
                );
            }
        },
        .Enum => {
            try self.OP_GET_ENUM_CASE(
                locations[node],
                @intCast(components.value_or_call_or_enum.EnumCase),
            );
        },
        .EnumInstance => {
            std.debug.assert(std.mem.eql(u8, identifier_lexeme, "value"));

            try self.OP_GET_ENUM_CASE_VALUE(locations[node]);
        },
        .List, .Map, .Range => {
            if (components.member_kind == .Call) {
                try self.OP_COPY(locations[node]);
                _ = try self.generateNode(components.value_or_call_or_enum.Call, breaks);
            } else {
                std.debug.assert(components.member_kind != .Value);
                try self.emitCodeArg(
                    locations[node],
                    get_code.?,
                    switch (callee_type.def_type) {
                        .List => @intCast(obj.ObjList.members_name.get(identifier_lexeme).?),
                        .Map => @intCast(obj.ObjMap.members_name.get(identifier_lexeme).?),
                        .Range => @intCast(obj.ObjRange.members_name.get(identifier_lexeme).?),
                        else => unreachable,
                    },
                );
            }
        },

        else => std.debug.assert(self.reporter.last_error != null),
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateDoUntil(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const node_components = self.ast.nodes.items(.components);
    const components = node_components[node].DoUntil;

    const loop_start = self.currentCode();

    var lbreaks = Breaks{};
    defer lbreaks.deinit(self.gc.allocator);

    _ = try self.generateNode(components.body, &lbreaks);
    _ = try self.generateNode(components.condition, &lbreaks);

    try self.OP_NOT(locations[node]);
    const exit_jump = try self.OP_JUMP_IF_FALSE(locations[node]);
    try self.OP_POP(locations[node]);

    try self.emitLoop(locations[node], loop_start);
    self.patchJump(exit_jump);

    try self.OP_POP(locations[node]); // Pop condition

    // Patch breaks
    try self.patchBreaks(
        &lbreaks,
        breaks,
        node,
        loop_start,
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateEnum(self: *Self, node: Ast.Node.Index, _: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const node_components = self.ast.nodes.items(.components);
    const components = node_components[node].Enum;

    try self.OP_CONSTANT(
        locations[node],
        try self.ast.typeCheckAndToValue(
            node,
            &self.reporter,
            self.gc,
        ),
    );
    try self.OP_DEFINE_GLOBAL(
        locations[node],
        @intCast(components.slot),
        (try self.gc.copyString(self.ast.tokens.items(.lexeme)[components.name])).toValue(),
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateExport(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].Export;

    if (components.declaration) |decl| {
        _ = try self.generateNode(decl, breaks);
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateExpression(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);
    const components = self.ast.nodes.items(.components);
    const expr = components[node].Expression;
    const expr_node_type = self.ast.nodes.items(.tag)[expr];
    const expr_type_def = self.ast.nodes.items(.type_def)[expr];

    _ = try self.generateNode(expr, breaks);

    try self.OP_POP(locations[node]);

    const lone_expr = (expr_node_type != .NamedVariable or components[expr].NamedVariable.value == null) and
        (expr_node_type != .Subscript or components[expr].Subscript.value == null) and
        (expr_node_type != .Dot or components[expr].Dot.member_kind != .Value) and
        expr_type_def != null and
        expr_type_def.?.def_type != .Void;

    if (self.flavor != .Repl and lone_expr and expr_type_def.?.def_type != .Placeholder) {
        const type_def_str = expr_type_def.?.toStringAlloc(self.gc.allocator, false) catch unreachable;
        defer self.gc.allocator.free(type_def_str);

        self.reporter.warnFmt(
            .discarded_value,
            self.ast.tokens.get(locations[node]),
            self.ast.tokens.get(end_locations[node]),
            "Discarded value of type `{s}`",
            .{
                type_def_str,
            },
        );
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateFloat(self: *Self, node: Ast.Node.Index, _: ?*Breaks) Error!?*obj.ObjFunction {
    try self.emitConstant(
        self.ast.nodes.items(.location)[node],
        try self.ast.typeCheckAndToValue(
            node,
            &self.reporter,
            self.gc,
        ),
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateFor(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);
    const type_defs = self.ast.nodes.items(.type_def);
    const node_components = self.ast.nodes.items(.components);

    const components = node_components[node].For;
    if (try self.ast.isConstant(self.gc.allocator, components.condition) and !(try self.ast.typeCheckAndToValue(
        components.condition,
        &self.reporter,
        self.gc,
    )).boolean()) {
        try self.patchOptJumps(node);

        return null;
    }

    for (components.init_declarations) |decl| {
        _ = try self.generateNode(decl, breaks);
    }

    const loop_start = self.currentCode();
    const jit_jump = if (!is_wasm) try self.OP_HOTSPOT(locations[node]) else {};
    if (!is_wasm) try self.emit(locations[node], node);

    const condition_type_def = type_defs[components.condition].?;
    if (condition_type_def.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, condition_type_def.resolved_type.?.Placeholder);
    }

    if (condition_type_def.def_type != .Bool) {
        self.reporter.reportErrorAt(
            .for_condition_type,
            self.ast.tokens.get(locations[components.condition]),
            self.ast.tokens.get(end_locations[components.condition]),
            "`for` condition must be bool",
        );
    }

    _ = try self.generateNode(components.condition, breaks);

    const exit_jump: usize = try self.OP_JUMP_IF_FALSE(locations[node]);
    try self.OP_POP(locations[node]); // Pop condition

    // Jump over expressions which will be executed at end of loop
    const body_jump = try self.OP_JUMP(locations[node]);

    const expr_loop: usize = self.currentCode();
    for (components.post_loop) |expr| {
        const expr_type_def = type_defs[expr].?;
        if (expr_type_def.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, expr_type_def.resolved_type.?.Placeholder);
        }

        _ = try self.generateNode(expr, breaks);
        try self.OP_POP(locations[expr]);
    }

    try self.emitLoop(locations[node], loop_start);

    self.patchJump(body_jump);

    var lbreaks = Breaks{};
    defer lbreaks.deinit(self.gc.allocator);

    _ = try self.generateNode(components.body, &lbreaks);

    try self.emitLoop(locations[node], expr_loop);

    self.patchJump(exit_jump);

    try self.OP_POP(locations[node]); // Pop condition

    // Patch breaks
    try self.patchBreaks(
        &lbreaks,
        breaks,
        node,
        loop_start,
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    if (!is_wasm) self.patchTryOrJit(jit_jump);

    return null;
}

fn generateForceUnwrap(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const components = self.ast.nodes.items(.components)[node].ForceUnwrap;

    _ = try self.generateNode(components.unwrapped, breaks);

    try self.OP_UNWRAP(locations[node]);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateForEach(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const node_components = self.ast.nodes.items(.components);
    const locations = self.ast.nodes.items(.location);
    const type_defs = self.ast.nodes.items(.type_def);
    const components = node_components[node].ForEach;

    const iterable_type_def = type_defs[components.iterable].?;

    // If iterable constant and empty, skip the node
    if (try self.ast.isConstant(self.gc.allocator, components.iterable)) {
        const iterable_value = (try self.ast.typeCheckAndToValue(
            components.iterable,
            &self.reporter,
            self.gc,
        ));

        const iterable_obj = if (iterable_value.isObj())
            iterable_value.obj()
        else
            null;

        if (iterable_obj) |iterable| {
            if (switch (iterable.obj_type) {
                .List => obj.ObjList.cast(iterable).?.items.items.len == 0,
                .Map => obj.ObjMap.cast(iterable).?.map.count() == 0,
                .String => obj.ObjString.cast(iterable).?.string.len == 0,
                .Enum => obj.ObjEnum.cast(iterable).?.cases.len == 0,
                .Range => obj.ObjRange.cast(iterable).?.high == obj.ObjRange.cast(iterable).?.low,
                else => self.reporter.last_error != null,
            }) {
                try self.patchOptJumps(node);
                return null;
            }
        }
    }

    _ = try self.generateNode(components.key, breaks);
    _ = try self.generateNode(components.value, breaks);
    _ = try self.generateNode(components.iterable, breaks);

    const loop_start: usize = self.currentCode();
    const jit_jump = if (!is_wasm) try self.emitJump(locations[node], .OP_HOTSPOT) else {};
    if (!is_wasm) try self.emit(locations[node], node);

    // Calls `next` and update key and value locals
    try self.emitOpCode(
        locations[node],
        switch (iterable_type_def.def_type) {
            .String => .OP_STRING_FOREACH,
            .List => .OP_LIST_FOREACH,
            .Enum => .OP_ENUM_FOREACH,
            .Map => .OP_MAP_FOREACH,
            .Fiber => .OP_FIBER_FOREACH,
            .Range => .OP_RANGE_FOREACH,
            else => unexpected: {
                std.debug.assert(self.reporter.last_error != null);
                break :unexpected .OP_STRING_FOREACH;
            },
        },
    );

    // If next key is null, exit loop
    try self.OP_GET_LOCAL(
        locations[node],
        @intCast(
            switch (iterable_type_def.def_type) {
                .String, .List, .Map => node_components[components.key].VarDeclaration.slot,
                else => node_components[components.value].VarDeclaration.slot,
            },
        ),
    );
    try self.OP_NULL(locations[node]);
    try self.OP_EQUAL(locations[node]);
    try self.OP_NOT(locations[node]);
    const exit_jump: usize = try self.OP_JUMP_IF_FALSE(locations[node]);
    try self.OP_POP(locations[node]); // Pop condition result

    var lbreaks = Breaks{};
    defer lbreaks.deinit(self.gc.allocator);

    _ = try self.generateNode(components.body, &lbreaks);

    try self.emitLoop(locations[node], loop_start);

    // Patch condition jump
    self.patchJump(exit_jump);

    try self.OP_POP(locations[node]); // Pop condition result

    // Patch breaks
    try self.patchBreaks(
        &lbreaks,
        breaks,
        node,
        loop_start,
    );

    try self.patchOptJumps(node);
    // Should have key, [value,] iterable to pop
    std.debug.assert(
        self.ast.nodes.items(.ends_scope)[node] != null and self.ast.nodes.items(.ends_scope)[node].?.len == 3,
    );
    try self.endScope(node);

    if (!is_wasm) self.patchTryOrJit(jit_jump);

    return null;
}

fn generateFunction(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const node_components = self.ast.nodes.items(.components);
    const type_defs = self.ast.nodes.items(.type_def);
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);
    const components = node_components[node].Function;
    const function_signature = if (components.function_signature) |fs|
        node_components[fs].FunctionType
    else
        null;
    const node_type_def = type_defs[node].?;
    const function_type = node_type_def.resolved_type.?.Function.function_type;

    // If function is a test block and we're not testing/checking/etc. don't waste time generating the node
    if (self.flavor == .Run and function_type == .Test) {
        try self.emitOpCode(locations[node], .OP_NULL);
        return null;
    }

    const enclosing = self.current;
    self.current = try self.gc.allocator.create(Frame);
    self.current.?.* = .{
        .enclosing = enclosing,
        .function_node = node,
        .constants = .{},
    };

    var function = try obj.ObjFunction.init(
        self.gc.allocator,
        self.ast,
        node,
    );

    function.type_def = node_type_def;

    // Check that default arguments are constant values
    switch (function_type) {
        .Function, .Method, .Anonymous, .Extern => {
            for (self.ast.nodes.items(.components)[components.function_signature.?].FunctionType.arguments) |argument| {
                if (argument.default) |default| {
                    if (!try self.ast.isConstant(self.gc.allocator, default)) {
                        self.reporter.reportErrorAt(
                            .constant_default,
                            self.ast.tokens.get(locations[default]),
                            self.ast.tokens.get(end_locations[default]),
                            "Default parameters must be constant values.",
                        );
                    } else {
                        try node_type_def.resolved_type.?.Function.defaults.put(
                            self.gc.allocator,
                            try self.gc.copyString(self.ast.tokens.items(.lexeme)[argument.name]),
                            try self.ast.typeCheckAndToValue(
                                default,
                                &self.reporter,
                                self.gc,
                            ),
                        );
                    }
                }
            }
        },
        else => {},
    }

    // Check for any remaining placeholders in function signature
    if (function.type_def.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, function.type_def.resolved_type.?.Placeholder);
    } else {
        const function_def = function.type_def.resolved_type.?.Function;

        if (function_def.return_type.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, function_def.return_type.resolved_type.?.Placeholder);
        }

        if (function_def.yield_type.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, function_def.yield_type.resolved_type.?.Placeholder);
        }

        var it = function_def.parameters.iterator();
        while (it.next()) |kv| {
            if (kv.value_ptr.*.def_type == .Placeholder) {
                self.reporter.reportPlaceholder(self.ast, kv.value_ptr.*.resolved_type.?.Placeholder);
            }
        }

        if (function_def.error_types) |error_types| {
            for (error_types) |error_type| {
                if (error_type.def_type == .Placeholder) {
                    self.reporter.reportPlaceholder(self.ast, error_type.resolved_type.?.Placeholder);
                }
            }
        }
    }

    // First chunk constant is the empty string
    _ = try function.chunk.addConstant(
        null,
        Value.fromObj((try self.gc.copyString("")).toObj()),
    );

    self.current.?.function = try self.gc.allocateObject(function);

    // Generate function's body
    if (components.body) |body| {
        _ = try self.generateNode(body, breaks);

        if (function_signature != null and function_signature.?.lambda) {
            try self.OP_RETURN(locations[body]);
            self.current.?.return_emitted = true;
        }
    }

    if (function_type != .Extern) {
        // If .Script, search for exported globals and return them in a map
        if (function_type == .Script or function_type == .ScriptEntryPoint) {
            // If top level, search `main` or `test` function(s) and call them
            // Then put any exported globals on the stack
            if (self.flavor != .Test and function_type == .ScriptEntryPoint) {
                if (components.entry.?.main_slot) |main_slot| {
                    try self.OP_GET_GLOBAL(
                        components.entry.?.main_location.?,
                        @intCast(main_slot),
                    );

                    if (components.entry.?.push_cli_args) {
                        try self.OP_GET_LOCAL(
                            components.entry.?.main_location.?,
                            0, // cli args are always local 0
                        );
                    }

                    try self.OP_CALL(
                        components.entry.?.main_location.?,
                        if (components.entry.?.push_cli_args)
                            1
                        else
                            0,
                        false,
                    );
                }
            } else if (self.flavor == .Test) {
                // Create an entry point wich runs all `test`
                for (components.entry.?.test_slots, 0..) |slot, index| {
                    try self.OP_GET_GLOBAL(
                        components.entry.?.test_locations[index],
                        @intCast(slot),
                    );
                    try self.OP_CALL(
                        components.entry.?.test_locations[index],
                        0,
                        false,
                    );
                }
            }

            // If we're being imported, put all globals on the stack
            if (components.import_root) {
                if (components.entry.?.exported_count > std.math.maxInt(u24)) {
                    self.reporter.reportErrorFmt(
                        .export_count,
                        self.ast.tokens.get(locations[node]),
                        self.ast.tokens.get(end_locations[node]),
                        "Can't export more than {} values.",
                        .{std.math.maxInt(u24)},
                    );
                }

                var index: usize = 0;
                while (index < components.entry.?.exported_count) : (index += 1) {
                    try self.OP_GET_GLOBAL(locations[node], @intCast(index));

                    if (self.debugging) {
                        try self.emitConstant(
                            locations[node],
                            Value.fromInteger(@intCast(index)),
                        );
                    }
                }

                try self.OP_EXPORT(locations[node], @intCast(components.entry.?.exported_count));
            } else {
                try self.OP_VOID(locations[node]);
                try self.OP_RETURN(locations[node]);
                self.current.?.return_emitted = true;
            }
        } else if (function_type == .Repl and
            components.body != null and
            self.ast.nodes.items(.tag)[components.body.?] == .Block and
            node_components[components.body.?].Block.len > 0 and
            self.ast.nodes.items(.tag)[node_components[components.body.?].Block[node_components[components.body.?].Block.len - 1]] == .Expression)
        {
            // Repl and last expression is a lone statement, remove OP_POP, add OP_RETURN
            std.debug.assert(vm.VM.getCode(self.current.?.function.?.chunk.code.pop().?) == .OP_POP);
            _ = self.current.?.function.?.chunk.locations.pop();

            try self.emitReturn(locations[node]);
        } else if (self.current.?.function.?.type_def.resolved_type.?.Function.return_type.def_type == .Void and !self.current.?.return_emitted) {
            // TODO: detect if some branches of the function body miss a return statement
            try self.emitReturn(locations[node]);
        } else if (!self.current.?.return_emitted) {
            self.reporter.reportErrorAt(
                .missing_return,
                self.ast.tokens.get(locations[node]),
                self.ast.tokens.get(end_locations[node]),
                "Missing return statement",
            );
        }
    }

    var frame = self.current.?;
    const current_function = frame.function.?;
    current_function.upvalue_count = @intCast(components.upvalue_binding.count());

    if (BuildOptions.debug) {
        disassembler.disassembleChunk(&current_function.chunk, current_function.type_def.resolved_type.?.Function.name.string);
        io.print("\n\n", .{});
    }

    self.current = frame.enclosing;

    // We don't need this frame anymore
    frame.deinit(self.gc.allocator);
    self.gc.allocator.destroy(frame);

    if (function_type != .ScriptEntryPoint and function_type != .Repl) {
        // `extern` functions don't have upvalues
        if (function_type == .Extern) {
            try self.OP_CONSTANT(
                locations[node],
                components.native.?.toValue(),
            );
        } else {
            try self.OP_CLOSURE(
                locations[node],
                current_function.toValue(),
            );

            var it = components.upvalue_binding.iterator();
            while (it.next()) |kv| {
                try self.emit(locations[node], if (kv.value_ptr.*) 1 else 0);
                try self.emit(locations[node], kv.key_ptr.*);
            }
        }
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    node_components[node].Function.function = current_function;

    return current_function;
}

fn generateFunDeclaration(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const node_components = self.ast.nodes.items(.components);
    const components = node_components[node].FunDeclaration;

    _ = try self.generateNode(components.function, breaks);

    if (components.slot_type == .Global) {
        try self.OP_DEFINE_GLOBAL(
            self.ast.nodes.items(.location)[node],
            @intCast(components.slot),
            self.ast.nodes.items(.type_def)[node].?.resolved_type.?.Function.name.toValue(),
        );
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateGenericResolve(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    _ = try self.generateNode(
        self.ast.nodes.items(.components)[node].GenericResolve.expression,
        breaks,
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateGrouping(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components);
    const expr = components[node].Grouping;

    _ = try self.generateNode(expr, breaks);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateIf(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const type_defs = self.ast.nodes.items(.type_def);
    const locations = self.ast.nodes.items(.location);
    const node_components = self.ast.nodes.items(.components);
    const components = node_components[node].If;
    const location = locations[node];

    // If condition is a constant expression, no need to generate branches
    if (try self.ast.isConstant(self.gc.allocator, components.condition) and
        components.unwrapped_identifier == null and
        components.casted_type == null)
    {
        const condition = try self.ast.typeCheckAndToValue(
            components.condition,
            &self.reporter,
            self.gc,
        );

        if (condition.boolean()) {
            _ = try self.generateNode(components.body, breaks);
        } else if (components.else_branch) |else_branch| {
            _ = try self.generateNode(else_branch, breaks);
        }

        try self.patchOptJumps(node);
        try self.endScope(node);

        return null;
    }

    _ = try self.generateNode(components.condition, breaks);
    const condition_location = locations[components.condition];
    if (components.casted_type) |casted_type| {
        try self.OP_COPY(condition_location);
        try self.emitConstant(condition_location, type_defs[casted_type].?.toValue());
        try self.OP_IS(condition_location);
    } else if (components.unwrapped_identifier != null) {
        try self.OP_COPY(condition_location);
        try self.OP_NULL(condition_location);
        try self.OP_EQUAL(condition_location);
        try self.OP_NOT(condition_location);
    }

    const else_jump = try self.OP_JUMP_IF_FALSE(location);
    try self.OP_POP(location);

    _ = try self.generateNode(components.body, breaks);

    const out_jump = try self.emitJump(location, .OP_JUMP);

    self.patchJump(else_jump);
    if (components.unwrapped_identifier != null or components.casted_type != null) {
        // Since we did not enter the if block, we did not pop the unwrapped local
        try self.OP_POP(location);
    }
    try self.OP_POP(location);

    if (components.else_branch) |else_branch| {
        _ = try self.generateNode(else_branch, breaks);
    }

    self.patchJump(out_jump);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateImport(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].Import;
    const location = self.ast.nodes.items(.location)[node];

    if (components.import) |import| {
        try self.emitConstant(location, import.absolute_path.toValue());

        _ = try self.generateNode(import.function, breaks);

        // FIXME: avoid generating the same import function more than once!
        try self.OP_IMPORT(location);
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateInteger(self: *Self, node: Ast.Node.Index, _: ?*Breaks) Error!?*obj.ObjFunction {
    try self.emitConstant(
        self.ast.nodes.items(.location)[node],
        try self.ast.typeCheckAndToValue(
            node,
            &self.reporter,
            self.gc,
        ),
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateIs(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].Is;
    const location = self.ast.nodes.items(.location)[node];
    const constant = try self.ast.typeCheckAndToValue(
        components.constant,
        &self.reporter,
        self.gc,
    );

    std.debug.assert(constant.isObj());
    std.debug.assert(constant.obj().obj_type == .Type);

    if (obj.ObjTypeDef.cast(constant.obj()).?.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, obj.ObjTypeDef.cast(constant.obj()).?.resolved_type.?.Placeholder);
    }

    _ = try self.generateNode(components.left, breaks);

    try self.OP_CONSTANT(location, constant);

    try self.OP_IS(location);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateList(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const components = self.ast.nodes.items(.components)[node].List;
    const type_defs = self.ast.nodes.items(.type_def);

    try self.OP_LIST(
        locations[node],
        type_defs[node].?.toValue(),
    );

    for (components.items) |item| {
        _ = try self.generateNode(item, breaks);
    }

    if (components.items.len > 0) {
        try self.OP_LIST_APPEND(
            locations[node],
            @intCast(components.items.len),
        );
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateMap(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const components = self.ast.nodes.items(.components)[node].Map;
    const type_defs = self.ast.nodes.items(.type_def);

    try self.OP_MAP(
        locations[node],
        type_defs[node].?.toValue(),
    );

    for (components.entries) |entry| {
        _ = try self.generateNode(entry.key, breaks);
        _ = try self.generateNode(entry.value, breaks);
    }

    if (components.entries.len > 0) {
        try self.OP_SET_MAP(
            locations[node],
            @intCast(components.entries.len),
        );
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateNamedVariable(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].NamedVariable;
    const locations = self.ast.nodes.items(.location);
    const type_defs = self.ast.nodes.items(.type_def);
    const tags = self.ast.tokens.items(.tag);

    var get_op: Chunk.OpCode = undefined;
    var set_op: Chunk.OpCode = undefined;

    switch (components.slot_type) {
        .Local => {
            get_op = .OP_GET_LOCAL;
            set_op = .OP_SET_LOCAL;
        },
        .Global => {
            get_op = .OP_GET_GLOBAL;
            set_op = .OP_SET_GLOBAL;
        },
        .UpValue => {
            get_op = .OP_GET_UPVALUE;
            set_op = .OP_SET_UPVALUE;
        },
    }

    if (components.value) |value| {
        switch (tags[components.assign_token.?]) {
            .PlusEqual,
            .MinusEqual,
            .StarEqual,
            .SlashEqual,
            .ShiftRightEqual,
            .ShiftLeftEqual,
            .XorEqual,
            .BorEqual,
            .BnotEqual,
            .AmpersandEqual,
            .PercentEqual,
            => try self.emitCodeArg(
                locations[node],
                get_op,
                @intCast(components.slot),
            ),
            else => {},
        }

        _ = try self.generateNode(value, breaks);

        switch (tags[components.assign_token.?]) {
            .PlusEqual => switch (type_defs[value].?.def_type) {
                .Integer => try self.OP_ADD_I(locations[value]),
                .Double => try self.OP_ADD_F(locations[value]),
                .List => try self.OP_ADD_LIST(locations[value]),
                .Map => try self.OP_ADD_MAP(locations[value]),
                .String => try self.OP_ADD_STRING(locations[value]),
                else => {},
            },
            .MinusEqual => switch (type_defs[value].?.def_type) {
                .Integer => try self.OP_SUBTRACT_I(locations[value]),
                .Double => try self.OP_SUBTRACT_F(locations[value]),
                else => {},
            },
            .StarEqual => switch (type_defs[value].?.def_type) {
                .Integer => try self.OP_MULTIPLY_I(locations[value]),
                .Double => try self.OP_MULTIPLY_F(locations[value]),
                else => {},
            },
            .SlashEqual => switch (type_defs[value].?.def_type) {
                .Integer => try self.OP_DIVIDE_I(locations[value]),
                .Double => try self.OP_DIVIDE_F(locations[value]),
                else => {},
            },
            .ShiftRightEqual => try self.OP_SHR(locations[value]),
            .ShiftLeftEqual => try self.OP_SHL(locations[value]),
            .XorEqual => try self.OP_XOR(locations[value]),
            .BorEqual => try self.OP_BOR(locations[value]),
            .AmpersandEqual => try self.OP_BAND(locations[value]),
            .PercentEqual => switch (type_defs[value].?.def_type) {
                .Integer => try self.OP_MOD_I(locations[value]),
                .Double => try self.OP_MOD_F(locations[value]),
                else => {},
            },
            else => {},
        }

        try self.emitCodeArg(
            locations[node],
            set_op,
            @intCast(components.slot),
        );
    } else {
        try self.emitCodeArg(
            locations[node],
            get_op,
            @intCast(components.slot),
        );
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateNull(self: *Self, node: Ast.Node.Index, _: ?*Breaks) Error!?*obj.ObjFunction {
    try self.OP_NULL(self.ast.nodes.items(.location)[node]);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateObjectDeclaration(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const type_defs = self.ast.nodes.items(.type_def);
    const lexemes = self.ast.tokens.items(.lexeme);
    const components = self.ast.nodes.items(.components)[node].ObjectDeclaration;
    const location = locations[node];

    const object_type = type_defs[node].?;
    const object_def = object_type.resolved_type.?.Object;

    // Put  object on the stack and define global with it
    try self.OP_OBJECT(location, object_type.toValue());
    try self.OP_DEFINE_GLOBAL(
        location,
        @intCast(components.slot),
        (try self.gc.copyString(self.ast.tokens.items(.lexeme)[components.name])).toValue(),
    );

    // Put the object on the stack to set its fields
    try self.OP_GET_GLOBAL(location, @intCast(components.slot));

    for (components.members) |member| {
        const member_name = lexemes[member.name];

        if (member.method) {
            // Method
            const member_field = object_def.fields.get(member_name).?;
            const member_idx = member_field.index;

            _ = try self.generateNode(member.method_or_default_value.?, breaks);
            try self.OP_PROPERTY(location, @intCast(member_idx));
        } else {
            // Property
            const property_field = object_def.fields.get(member_name).?;
            const property_idx = property_field.index;

            // Create property default value
            if (member.method_or_default_value) |default| {
                if (property_field.static) {
                    try self.OP_COPY(location);
                }

                _ = try self.generateNode(default, breaks);

                // Create property default value
                if (property_field.static) {
                    try self.OP_SET_OBJECT_PROPERTY(location, @intCast(property_idx));
                    try self.OP_POP(location);
                } else {
                    try self.OP_OBJECT_DEFAULT(
                        location,
                        @intCast(property_idx),
                    );
                }
            }
        }
    }

    // Pop object
    try self.OP_POP(location);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateObjectInit(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const type_defs = self.ast.nodes.items(.type_def);
    const lexemes = self.ast.tokens.items(.lexeme);
    const components = self.ast.nodes.items(.components)[node].ObjectInit;
    const location = locations[node];
    const node_type_def = type_defs[node].?;

    if (components.object != null and type_defs[components.object.?].?.def_type == .Object) {
        _ = try self.generateNode(components.object.?, breaks);
    } else if (node_type_def.def_type == .ObjectInstance) {
        try self.OP_NULL(location);
    }

    try self.OP_CONSTANT(location, node_type_def.toValue());

    try self.emitOpCode(
        location,
        if (node_type_def.def_type == .ObjectInstance)
            .OP_INSTANCE
        else
            .OP_FCONTAINER_INSTANCE,
    );

    for (components.properties) |property| {
        const property_name = lexemes[property.name];
        const property_idx = if (node_type_def.def_type == .ObjectInstance)
            if (node_type_def.resolved_type.?.ObjectInstance.of
                .resolved_type.?.Object
                .fields.get(property_name)) |field|
                field.index
            else
                null
        else
            node_type_def.resolved_type.?.ForeignContainer
                .fields.getIndex(property_name);

        try self.OP_COPY(location); // Will be popped by OP_SET_PROPERTY

        _ = try self.generateNode(property.value, breaks);

        try self.emitCodeArg(
            location,
            if (node_type_def.def_type == .ObjectInstance)
                .OP_SET_INSTANCE_PROPERTY
            else
                .OP_SET_FCONTAINER_INSTANCE_PROPERTY,
            @intCast(property_idx.?),
        );
        try self.OP_POP(location); // Pop property value

    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generatePattern(self: *Self, node: Ast.Node.Index, _: ?*Breaks) Error!?*obj.ObjFunction {
    try self.emitConstant(
        self.ast.nodes.items(.location)[node],
        try self.ast.typeCheckAndToValue(
            node,
            &self.reporter,
            self.gc,
        ),
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateProtocolDeclaration(self: *Self, node: Ast.Node.Index, _: ?*Breaks) Error!?*obj.ObjFunction {
    const location = self.ast.nodes.items(.location)[node];
    const components = self.ast.nodes.items(.components)[node].ProtocolDeclaration;
    const type_def = self.ast.nodes.items(.type_def)[node].?;

    try self.emitConstant(location, type_def.toValue());
    try self.OP_DEFINE_GLOBAL(
        location,
        @intCast(components.slot),
        (try self.gc.copyString(self.ast.tokens.items(.lexeme)[components.name])).toValue(),
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateRange(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].Range;
    const locations = self.ast.nodes.items(.location);

    _ = try self.generateNode(components.low, breaks);
    _ = try self.generateNode(components.high, breaks);

    try self.OP_RANGE(locations[node]);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateResolve(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const fiber = self.ast.nodes.items(.components)[node].Resolve;
    const locations = self.ast.nodes.items(.location);

    _ = try self.generateNode(fiber, breaks);

    try self.OP_RESOLVE(locations[node]);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateResume(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const fiber = self.ast.nodes.items(.components)[node].Resume;
    const locations = self.ast.nodes.items(.location);

    _ = try self.generateNode(fiber, breaks);

    try self.OP_RESUME(locations[node]);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateReturn(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].Return;
    const locations = self.ast.nodes.items(.location);

    if (components.unconditional) {
        self.current.?.return_emitted = true;
    }

    if (components.value) |value| {
        _ = try self.generateNode(value, breaks);
    } else {
        try self.OP_VOID(locations[node]);
    }

    try self.OP_RETURN(locations[node]);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateString(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const location = self.ast.nodes.items(.location)[node];
    const type_defs = self.ast.nodes.items(.type_def);
    const elements = self.ast.nodes.items(.components)[node].String;

    if (elements.len == 0) {
        // Push the empty string which is always the constant 0
        try self.emitCodeArg(location, .OP_CONSTANT, 0);

        try self.endScope(node);

        return null;
    }

    for (elements, 0..) |element, index| {
        const element_type_def = type_defs[element].?;

        _ = try self.generateNode(element, breaks);
        if (element_type_def.def_type != .String or element_type_def.optional) {
            try self.OP_TO_STRING(location);
        }

        if (index >= 1) {
            try self.OP_ADD_STRING(location);
        }
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateStringLiteral(self: *Self, node: Ast.Node.Index, _: ?*Breaks) Error!?*obj.ObjFunction {
    try self.emitConstant(
        self.ast.nodes.items(.location)[node],
        self.ast.nodes.items(.components)[node].StringLiteral.literal.toValue(),
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateSubscript(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const location = locations[node];
    const type_defs = self.ast.nodes.items(.type_def);
    const components = self.ast.nodes.items(.components)[node].Subscript;
    const tags = self.ast.tokens.items(.tag);

    _ = try self.generateNode(components.subscripted, breaks);

    const subscripted_type_def = type_defs[components.subscripted].?;

    var get_code: Chunk.OpCode = .OP_GET_LIST_SUBSCRIPT;
    var set_code: Chunk.OpCode = .OP_SET_LIST_SUBSCRIPT;
    switch (subscripted_type_def.def_type) {
        .String => {
            get_code = .OP_GET_STRING_SUBSCRIPT;

            std.debug.assert(components.value == null);
        },

        .Map => {
            get_code = .OP_GET_MAP_SUBSCRIPT;
            set_code = .OP_SET_MAP_SUBSCRIPT;
        },
        else => {},
    }

    _ = try self.generateNode(components.index, breaks);

    if (!components.checked and components.value != null) {
        const value = components.value.?;

        if (tags[components.assign_token.?].isAssignShortcut()) {
            try self.emitCodeArg(
                locations[value],
                get_code,
                2, // 2 means, leave subscript and index on the stack
            );
        }

        _ = try self.generateNode(value, breaks);

        switch (tags[components.assign_token.?]) {
            .PlusEqual => switch (type_defs[value].?.def_type) {
                .Integer => try self.OP_ADD_I(locations[value]),
                .Double => try self.OP_ADD_F(locations[value]),
                .List => try self.OP_ADD_LIST(locations[value]),
                .Map => try self.OP_ADD_MAP(locations[value]),
                .String => try self.OP_ADD_STRING(locations[value]),
                else => {},
            },
            .MinusEqual => switch (type_defs[value].?.def_type) {
                .Integer => try self.OP_SUBTRACT_I(locations[value]),
                .Double => try self.OP_SUBTRACT_F(locations[value]),
                else => {},
            },
            .StarEqual => switch (type_defs[value].?.def_type) {
                .Integer => try self.OP_MULTIPLY_I(locations[value]),
                .Double => try self.OP_MULTIPLY_F(locations[value]),
                else => {},
            },
            .SlashEqual => switch (type_defs[value].?.def_type) {
                .Integer => try self.OP_DIVIDE_I(locations[value]),
                .Double => try self.OP_DIVIDE_F(locations[value]),
                else => {},
            },
            .ShiftRightEqual => try self.OP_SHR(locations[value]),
            .ShiftLeftEqual => try self.OP_SHL(locations[value]),
            .XorEqual => try self.OP_XOR(locations[value]),
            .BorEqual => try self.OP_BOR(locations[value]),
            .AmpersandEqual => try self.OP_BAND(locations[value]),
            .PercentEqual => switch (type_defs[value].?.def_type) {
                .Integer => try self.OP_MOD_I(locations[value]),
                .Double => try self.OP_MOD_F(locations[value]),
                else => {},
            },
            else => {},
        }

        try self.emitOpCode(location, set_code);
    } else {
        try self.emitCodeArg(
            location,
            get_code,
            if (components.checked) 1 else 0,
        );
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateTry(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].Try;
    const type_defs = self.ast.nodes.items(.type_def);
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);
    const location = locations[node];

    self.current.?.try_should_handle = .{};
    defer {
        self.current.?.try_should_handle.?.deinit(self.gc.allocator);
        self.current.?.try_should_handle = null;
    }

    // OP_TRY notifies runtime that we're handling error at offset
    const try_jump = try self.OP_TRY(location);

    _ = try self.generateNode(components.body, breaks);

    // Jump reached if no error was raised
    const no_error_jump = try self.OP_JUMP(self.ast.nodes.items(.end_location)[components.body]);

    var exit_jumps = std.ArrayList(usize).empty;
    defer exit_jumps.deinit(self.gc.allocator);

    self.patchTryOrJit(try_jump);
    var has_unconditional = components.unconditional_clause != null;
    for (components.clauses) |clause| {
        const error_type = type_defs[clause.type_def].?;

        if (error_type.def_type == .Any) {
            has_unconditional = true;
        }

        // We assume the error is on top of the stack
        try self.OP_COPY(clause.identifier); // Copy error value since its argument to the catch clause
        try self.emitConstant(clause.identifier, error_type.toValue());
        try self.OP_IS(clause.identifier);
        // If error type does not match, jump to next catch clause
        const next_clause_jump: usize = try self.OP_JUMP_IF_FALSE(location);
        // Pop `is` result
        try self.OP_POP(clause.identifier);

        // Clause block will pop error value since its declared as a local in it
        // We don't catch things is the catch clause
        const previous = self.current.?.try_should_handle;
        self.current.?.try_should_handle = null;
        _ = try self.generateNode(clause.body, breaks);
        self.current.?.try_should_handle = previous;

        // After handling the error, jump over next clauses
        try exit_jumps.append(
            self.gc.allocator,
            try self.emitJump(location, .OP_JUMP),
        );

        self.patchJump(next_clause_jump);
        // Pop `is` result
        try self.OP_POP(clause.identifier);
    }

    if (components.unconditional_clause) |unconditional_clause| {
        // pop error because its not a local of this clause
        try self.OP_POP(locations[unconditional_clause]);
        // We don't catch things is the catch clause
        const previous = self.current.?.try_should_handle;
        self.current.?.try_should_handle = null;
        _ = try self.generateNode(unconditional_clause, breaks);
        self.current.?.try_should_handle = previous;

        try exit_jumps.append(
            self.gc.allocator,
            try self.emitJump(location, .OP_JUMP),
        );
    }

    // Tell runtime we're not in a try block anymore
    try self.OP_TRY_END(location);
    // Uncaught error, throw the error again
    try self.OP_THROW(location);

    // Patch exit jumps
    for (exit_jumps.items) |exit_jump| {
        self.patchJump(exit_jump);
    }

    self.patchJump(no_error_jump);

    // OP_TRY_END notifies runtime that we're not in a try block anymore
    try self.OP_TRY_END(location);

    // Did we handle all errors not specified in current function signature?
    if (!has_unconditional) {
        var it = self.current.?.try_should_handle.?.iterator();
        while (it.next()) |kv| {
            var clause: ?Ast.Try.Clause = null;
            for (components.clauses) |cls| {
                if (type_defs[cls.type_def] == kv.key_ptr.*) {
                    clause = cls;
                    break;
                }
            }

            if (clause == null) {
                const err_str = try kv.key_ptr.*.toStringAlloc(self.gc.allocator, false);
                defer self.gc.allocator.free(err_str);

                self.reporter.reportWithOrigin(
                    .error_not_handled,
                    self.ast.tokens.get(location),
                    self.ast.tokens.get(end_locations[node]),
                    self.ast.tokens.get(kv.value_ptr.*),
                    self.ast.tokens.get(kv.value_ptr.*),
                    "Error type `{s}` not handled",
                    .{err_str},
                    "can occur here",
                );
            }
        }
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateThrow(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].Throw;
    const type_defs = self.ast.nodes.items(.type_def);
    const location = self.ast.nodes.items(.location)[node];
    const end_locations = self.ast.nodes.items(.end_location);

    if (components.unconditional) {
        self.current.?.return_emitted = true;
    }

    const expression_type_def = type_defs[components.expression].?;
    const current_error_types = self.current.?.function.?.type_def.resolved_type.?.Function.error_types;

    var found_match = false;
    if (current_error_types != null) {
        for (current_error_types.?) |error_type| {
            if (error_type.eql(expression_type_def)) {
                found_match = true;
                break;
            }
        }
    }

    if (!found_match) {
        if (self.current.?.try_should_handle != null) {
            // In a try catch remember to check that we handle that error when finishing parsing the try-catch
            try self.current.?.try_should_handle.?.put(
                self.gc.allocator,
                expression_type_def,
                location,
            );
        } else {
            // Not in a try-catch and function signature does not expect this error type
            const error_str = try type_defs[components.expression].?.toStringAlloc(self.gc.allocator, false);
            defer self.gc.allocator.free(error_str);

            self.reporter.reportErrorFmt(
                .unexpected_error_type,
                self.ast.tokens.get(location),
                self.ast.tokens.get(end_locations[node]),
                "Error type `{s}` not expected",
                .{error_str},
            );
        }
    }

    _ = try self.generateNode(components.expression, breaks);

    try self.OP_THROW(location);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateTypeExpression(self: *Self, node: Ast.Node.Index, _: ?*Breaks) Error!?*obj.ObjFunction {
    const node_components = self.ast.nodes.items(.components);
    const type_defs = self.ast.nodes.items(.type_def);

    try self.emitConstant(
        self.ast.nodes.items(.location)[node],
        type_defs[node_components[node].TypeExpression].?.toValue(),
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateTypeOfExpression(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    _ = try self.generateNode(self.ast.nodes.items(.components)[node].TypeOfExpression, breaks);

    try self.OP_TYPEOF(self.ast.nodes.items(.location)[node]);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateUnary(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].Unary;
    const location = self.ast.nodes.items(.location)[node];
    const expression_type_def = self.ast.nodes.items(.type_def)[components.expression].?;

    _ = try self.generateNode(components.expression, breaks);

    switch (components.operator) {
        .Bnot => try self.OP_BNOT(location),
        .Bang => try self.OP_NOT(location),
        .Minus => if (expression_type_def.def_type == .Integer)
            try self.OP_NEGATE_I(location)
        else if (expression_type_def.def_type == .Double)
            try self.OP_NEGATE_F(location),
        else => unreachable,
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateUnwrap(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const location = locations[node];
    const components = self.ast.nodes.items(.components)[node].Unwrap;

    _ = try self.generateNode(components.unwrapped, breaks);

    try self.OP_COPY(location);
    try self.OP_NULL(location);
    try self.OP_EQUAL(location);
    try self.OP_NOT(location);

    const jump = try self.OP_JUMP_IF_FALSE(location);

    if (self.opt_jumps.items.len == 0 or components.start_opt_jumps) {
        try self.opt_jumps.append(self.gc.allocator, .{});
    } else if (self.opt_jumps.items.len == 0) {
        @panic("Unwrap node not marked as starting opt_jumps but not ongoing opt_jumps");
    }

    try self.opt_jumps.items[self.opt_jumps.items.len - 1].append(self.gc.allocator, jump);

    try self.OP_POP(location); // Pop test result

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateVarDeclaration(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].VarDeclaration;
    const locations = self.ast.nodes.items(.location);
    const location = locations[node];

    if (components.value) |value| {
        _ = try self.generateNode(value, breaks);
    } else {
        try self.OP_NULL(location);
    }

    switch (components.slot_type) {
        .Global => try self.OP_DEFINE_GLOBAL(
            location,
            @intCast(components.slot),
            (try self.gc.copyString(self.ast.tokens.items(.lexeme)[components.name])).toValue(),
        ),
        .Local => {
            try self.OP_DBG_LOCAL_ENTER(
                location,
                @intCast(components.slot),
                (try self.gc.copyString(self.ast.tokens.items(.lexeme)[components.name])).toValue(),
            );
        },
        .UpValue => {}, // TODO: ??
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateVoid(self: *Self, node: Ast.Node.Index, _: ?*Breaks) Error!?*obj.ObjFunction {
    try self.OP_VOID(self.ast.nodes.items(.location)[node]);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateWhile(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].While;
    const locations = self.ast.nodes.items(.location);
    const location = locations[node];

    // If condition constant and false, skip the node
    if (try self.ast.isConstant(self.gc.allocator, components.condition) and !(try self.ast.typeCheckAndToValue(
        components.condition,
        &self.reporter,
        self.gc,
    )).boolean()) {
        try self.patchOptJumps(node);
        try self.endScope(node);

        return null;
    }

    const loop_start: usize = self.currentCode();

    const jit_jump = if (!is_wasm) try self.emitJump(locations[node], .OP_HOTSPOT) else {};
    if (!is_wasm) try self.emit(locations[node], node);

    _ = try self.generateNode(components.condition, breaks);

    const exit_jump = try self.OP_JUMP_IF_FALSE(location);
    try self.OP_POP(location);

    var while_breaks = Breaks{};
    defer while_breaks.deinit(self.gc.allocator);

    _ = try self.generateNode(components.body, &while_breaks);

    try self.emitLoop(location, loop_start);
    self.patchJump(exit_jump);

    try self.OP_POP(location); // Pop condition (is not necessary if broke out of the loop)

    // Patch breaks
    try self.patchBreaks(
        &while_breaks,
        breaks,
        node,
        loop_start,
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    if (!is_wasm) self.patchTryOrJit(jit_jump);

    return null;
}

fn generateYield(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const expression = self.ast.nodes.items(.components)[node].Yield;
    const locations = self.ast.nodes.items(.location);
    const location = locations[node];

    _ = try self.generateNode(expression, breaks);

    try self.OP_YIELD(location);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateZdef(self: *Self, node: Ast.Node.Index, _: ?*Breaks) Error!?*obj.ObjFunction {
    if (is_wasm) {
        return null;
    }

    const components = self.ast.nodes.items(.components)[node].Zdef;
    const location = self.ast.nodes.items(.location)[node];

    if (self.flavor.resolveDynLib()) {
        for (components.elements) |*element| {
            // Generate ObjNative wrapper of actual zdef
            switch (element.zdef.type_def.def_type) {
                .Function => {
                    if (element.obj_native == null) {
                        var timer = if (!is_wasm) std.time.Timer.start() catch unreachable else {};

                        element.obj_native = try self.jit.?.compileZdef(self.ast, element.*);

                        if (!is_wasm) {
                            self.jit.?.jit_time += timer.read();
                        }

                        try self.emitConstant(location, element.obj_native.?.toValue());
                    }
                },
                .ForeignContainer => {
                    var timer = if (!is_wasm) std.time.Timer.start() catch unreachable else {};

                    try self.jit.?.compileZdefContainer(self.ast, element.*);

                    if (!is_wasm) {
                        self.jit.?.jit_time += timer.read();
                    }

                    try self.emitConstant(location, element.zdef.type_def.toValue());
                },
                else => {},
            }
            try self.OP_DEFINE_GLOBAL(
                location,
                @intCast(element.slot),
                (try self.gc.copyString(self.ast.tokens.items(.lexeme)[components.lib_name])).toValue(),
            );
        }
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn OP_DBG_LOCAL_ENTER(self: *Self, location: Ast.TokenIndex, slot: u8, name: Value) !void {
    // Don't emit those if we are not debugging
    if (!self.debugging) return;

    try self.emitOpCode(location, .OP_DBG_LOCAL_ENTER);
    try self.emitTwo(location, slot, try self.makeConstant(name));
}

fn OP_DBG_LOCAL_EXIT(self: *Self, location: Ast.TokenIndex, slot: u8) !void {
    // Don't emit those if we are not debugging
    if (!self.debugging) return;

    try self.emitCodeArg(location, .OP_DBG_LOCAL_EXIT, slot);
}

fn OP_DBG_GLOBAL_DEFINE(self: *Self, location: Ast.TokenIndex, slot: u24, name: Value) !void {
    // Don't emit those if we are not debugging
    if (!self.debugging) return;

    try self.emitOpCode(location, .OP_DBG_GLOBAL_DEFINE);
    try self.emit(location, slot);
    try self.emit(location, try self.makeConstant(name));
}

fn OP_SWAP(self: *Self, location: Ast.TokenIndex, slotA: u8, slotB: u8) !void {
    try self.emitCodeArgs(location, .OP_SWAP, slotA, slotB);
}

fn OP_HOTSPOT(self: *Self, location: Ast.TokenIndex) !usize {
    return try self.emitJump(location, .OP_HOTSPOT);
}

fn OP_NULL(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_NULL);
}

fn OP_VOID(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_VOID);
}

fn OP_TRUE(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_TRUE);
}

fn OP_FALSE(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_FALSE);
}

fn OP_POP(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_POP);
}

fn OP_EQUAL(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_EQUAL);
}

fn OP_IS(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_IS);
}

fn OP_GREATER(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_GREATER);
}

fn OP_LESS(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_LESS);
}

fn OP_ADD_F(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_ADD_F);
}

fn OP_ADD_I(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_ADD_I);
}

fn OP_ADD_STRING(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_ADD_STRING);
}

fn OP_ADD_LIST(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_ADD_LIST);
}

fn OP_ADD_MAP(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_ADD_MAP);
}

fn OP_SUBTRACT_I(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_SUBTRACT_I);
}

fn OP_SUBTRACT_F(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_SUBTRACT_F);
}

fn OP_MULTIPLY(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_MULTIPLY);
}

fn OP_DIVIDE(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_DIVIDE);
}

fn OP_MULTIPLY_I(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_MULTIPLY_I);
}

fn OP_MULTIPLY_F(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_MULTIPLY_F);
}

fn OP_DIVIDE_I(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_DIVIDE_I);
}

fn OP_DIVIDE_F(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_DIVIDE_F);
}

fn OP_NOT(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_NOT);
}

fn OP_NEGATE_I(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_NEGATE_I);
}

fn OP_NEGATE_F(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_NEGATE_F);
}

fn OP_BAND(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_BAND);
}

fn OP_BOR(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_BOR);
}

fn OP_XOR(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_XOR);
}

fn OP_BNOT(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_BNOT);
}

fn OP_SHL(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_SHL);
}

fn OP_SHR(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_SHR);
}

fn OP_MOD_I(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_MOD_I);
}

fn OP_MOD_F(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_MOD_F);
}

fn OP_UNWRAP(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_UNWRAP);
}

fn OP_GET_ENUM_CASE_VALUE(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_GET_ENUM_CASE_VALUE);
}

fn OP_SET_LIST_SUBSCRIPT(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_SET_LIST_SUBSCRIPT);
}

fn OP_SET_MAP_SUBSCRIPT(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_SET_MAP_SUBSCRIPT);
}

fn OP_THROW(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_THROW);
}

fn OP_IMPORT(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_IMPORT);
}

fn OP_TO_STRING(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_TO_STRING);
}

fn OP_INSTANCE(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_INSTANCE);
}

fn OP_FCONTAINER_INSTANCE(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_FCONTAINER_INSTANCE);
}

fn OP_STRING_FOREACH(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_STRING_FOREACH);
}

fn OP_LIST_FOREACH(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_LIST_FOREACH);
}

fn OP_ENUM_FOREACH(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_ENUM_FOREACH);
}

fn OP_MAP_FOREACH(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_MAP_FOREACH);
}

fn OP_FIBER_FOREACH(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_FIBER_FOREACH);
}

fn OP_RANGE_FOREACH(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_RANGE_FOREACH);
}

fn OP_RESUME(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_RESUME);
}

fn OP_YIELD(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_YIELD);
}

fn OP_RESOLVE(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_RESOLVE);
}

fn OP_TRY_END(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_TRY_END);
}

fn OP_GET_ENUM_CASE_FROM_VALUE(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_GET_ENUM_CASE_FROM_VALUE);
}

fn OP_TYPEOF(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_TYPEOF);
}

fn OP_HOTSPOT_CALL(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_HOTSPOT_CALL);
}

fn OP_DEFINE_GLOBAL(self: *Self, location: Ast.TokenIndex, slot: u24, name: Value) !void {
    try self.emitCodeArg(
        location,
        .OP_DEFINE_GLOBAL,
        slot,
    );

    try self.OP_DBG_GLOBAL_DEFINE(
        location,
        slot,
        name,
    );
}

fn OP_GET_GLOBAL(self: *Self, location: Ast.TokenIndex, slot: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_GLOBAL,
        slot,
    );
}

fn OP_SET_GLOBAL(self: *Self, location: Ast.TokenIndex, slot: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_SET_GLOBAL,
        slot,
    );
}

fn OP_GET_LOCAL(self: *Self, location: Ast.TokenIndex, slot: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_LOCAL,
        slot,
    );
}

fn OP_SET_LOCAL(self: *Self, location: Ast.TokenIndex, slot: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_SET_LOCAL,
        slot,
    );
}

fn OP_GET_UPVALUE(self: *Self, location: Ast.TokenIndex, slot: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_UPVALUE,
        slot,
    );
}

fn OP_SET_UPVALUE(self: *Self, location: Ast.TokenIndex, slot: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_SET_UPVALUE,
        slot,
    );
}

fn OP_GET_ENUM_CASE(self: *Self, location: Ast.TokenIndex, case: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_ENUM_CASE,
        case,
    );
}

fn OP_EXPORT(self: *Self, location: Ast.TokenIndex, count: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_EXPORT,
        count,
    );
}

fn OP_COPY(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_COPY);
}

fn OP_CLONE(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_CLONE);
}

fn OP_CLOSE_UPVALUE(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_CLOSE_UPVALUE);
}

fn OP_RETURN(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_RETURN);
}

fn OP_LIST_APPEND(self: *Self, location: Ast.TokenIndex, count: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_LIST_APPEND,
        count,
    );
}

fn OP_SET_MAP(self: *Self, location: Ast.TokenIndex, slot: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_SET_MAP,
        slot,
    );
}

fn OP_PROPERTY(self: *Self, location: Ast.TokenIndex, member_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_PROPERTY,
        member_idx,
    );
}

fn OP_OBJECT_DEFAULT(self: *Self, location: Ast.TokenIndex, property_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_OBJECT_DEFAULT,
        property_idx,
    );
}

fn OP_SET_OBJECT_PROPERTY(self: *Self, location: Ast.TokenIndex, property_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_SET_OBJECT_PROPERTY,
        property_idx,
    );
}

fn OP_SET_INSTANCE_PROPERTY(self: *Self, location: Ast.TokenIndex, property_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_SET_INSTANCE_PROPERTY,
        property_idx,
    );
}

fn OP_GET_INSTANCE_PROPERTY(self: *Self, location: Ast.TokenIndex, property_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_INSTANCE_PROPERTY,
        property_idx,
    );
}

fn OP_GET_INSTANCE_METHOD(self: *Self, location: Ast.TokenIndex, property_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_INSTANCE_METHOD,
        property_idx,
    );
}

fn OP_GET_LIST_PROPERTY(self: *Self, location: Ast.TokenIndex, property_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_LIST_PROPERTY,
        property_idx,
    );
}

fn OP_GET_MAP_PROPERTY(self: *Self, location: Ast.TokenIndex, property_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_MAP_PROPERTY,
        property_idx,
    );
}

fn OP_GET_FIBER_PROPERTY(self: *Self, location: Ast.TokenIndex, property_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_FIBER_PROPERTY,
        property_idx,
    );
}

fn OP_GET_RANGE_PROPERTY(self: *Self, location: Ast.TokenIndex, property_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_RANGE_PROPERTY,
        property_idx,
    );
}

fn OP_GET_STRING_PROPERTY(self: *Self, location: Ast.TokenIndex, property_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_STRING_PROPERTY,
        property_idx,
    );
}

fn OP_GET_PATTERN_PROPERTY(self: *Self, location: Ast.TokenIndex, property_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_PATTERN_PROPERTY,
        property_idx,
    );
}

fn OP_GET_OBJECT_PROPERTY(self: *Self, location: Ast.TokenIndex, property_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_OBJECT_PROPERTY,
        property_idx,
    );
}

fn OP_GET_LIST_SUBSCRIPT(self: *Self, location: Ast.TokenIndex, checked: bool) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_LIST_SUBSCRIPT,
        if (checked) 1 else 0,
    );
}

fn OP_GET_STRING_SUBSCRIPT(self: *Self, location: Ast.TokenIndex, checked: bool) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_STRING_SUBSCRIPT,
        if (checked) 1 else 0,
    );
}

fn OP_GET_MAP_SUBSCRIPT(self: *Self, location: Ast.TokenIndex, checked: bool) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_MAP_SUBSCRIPT,
        if (checked) 1 else 0,
    );
}

fn OP_OBJECT(self: *Self, location: Ast.TokenIndex, constant: Value) !void {
    try self.emitCodeArg(
        location,
        .OP_OBJECT,
        try self.makeConstant(constant),
    );
}

fn OP_LIST(self: *Self, location: Ast.TokenIndex, constant: Value) !void {
    try self.emitCodeArg(
        location,
        .OP_LIST,
        try self.makeConstant(constant),
    );
}

fn OP_MAP(self: *Self, location: Ast.TokenIndex, constant: Value) !void {
    try self.emitCodeArg(
        location,
        .OP_MAP,
        try self.makeConstant(constant),
    );
}

fn OP_RANGE(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_RANGE);
}

fn OP_GET_PROTOCOL_METHOD(self: *Self, location: Ast.TokenIndex, identifier: []const u8) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_PROTOCOL_METHOD,
        try self.identifierConstant(identifier),
    );
}

fn OP_GET_FCONTAINER_INSTANCE_PROPERTY(self: *Self, location: Ast.TokenIndex, field_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_GET_FCONTAINER_INSTANCE_PROPERTY,
        field_idx,
    );
}

fn OP_SET_FCONTAINER_INSTANCE_PROPERTY(self: *Self, location: Ast.TokenIndex, field_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_SET_FCONTAINER_INSTANCE_PROPERTY,
        field_idx,
    );
}

fn OP_CONSTANT(self: *Self, location: Ast.TokenIndex, constant: Value) !void {
    try self.emitCodeArg(
        location,
        .OP_CONSTANT,
        try self.makeConstant(constant),
    );
}

fn OP_JUMP(self: *Self, location: Ast.TokenIndex) !usize {
    return try self.emitJump(
        location,
        .OP_JUMP,
    );
}

fn OP_JUMP_IF_FALSE(self: *Self, location: Ast.TokenIndex) !usize {
    return try self.emitJump(
        location,
        .OP_JUMP_IF_FALSE,
    );
}

fn OP_JUMP_IF_NOT_NULL(self: *Self, location: Ast.TokenIndex) !usize {
    return try self.emitJump(
        location,
        .OP_JUMP_IF_NOT_NULL,
    );
}

fn OP_LOOP(self: *Self, location: Ast.TokenIndex, loop_start: usize) !void {
    const offset: usize = self.currentCode() - loop_start + 1;
    if (offset > std.math.maxInt(u24)) {
        self.reportError(.loop_body_too_large, "Loop body too large.");
    }

    try self.emitCodeArg(location, .OP_LOOP, @as(u24, @intCast(offset)));
}

fn OP_TRY(self: *Self, location: Ast.TokenIndex) !usize {
    return try self.emitJump(
        location,
        .OP_TRY,
    );
}

fn OP_CALL_INSTANCE_PROPERTY(self: *Self, location: Ast.TokenIndex, field_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_CALL_INSTANCE_PROPERTY,
        field_idx,
    );
}

fn OP_TAIL_CALL_INSTANCE_PROPERTY(self: *Self, location: Ast.TokenIndex, field_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_TAIL_CALL_INSTANCE_PROPERTY,
        field_idx,
    );
}

fn OP_INSTANCE_INVOKE(self: *Self, location: Ast.TokenIndex, field_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_INSTANCE_INVOKE,
        field_idx,
    );
}

fn OP_INSTANCE_TAIL_INVOKE(self: *Self, location: Ast.TokenIndex, field_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_INSTANCE_TAIL_INVOKE,
        field_idx,
    );
}

fn OP_STRING_INVOKE(self: *Self, location: Ast.TokenIndex, field_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_STRING_INVOKE,
        field_idx,
    );
}

fn OP_PATTERN_INVOKE(self: *Self, location: Ast.TokenIndex, field_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_PATTERN_INVOKE,
        field_idx,
    );
}

fn OP_FIBER_INVOKE(self: *Self, location: Ast.TokenIndex, field_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_FIBER_INVOKE,
        field_idx,
    );
}

fn OP_LIST_INVOKE(self: *Self, location: Ast.TokenIndex, field_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_LIST_INVOKE,
        field_idx,
    );
}

fn OP_MAP_INVOKE(self: *Self, location: Ast.TokenIndex, field_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_MAP_INVOKE,
        field_idx,
    );
}

fn OP_RANGE_INVOKE(self: *Self, location: Ast.TokenIndex, field_idx: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_RANGE_INVOKE,
        field_idx,
    );
}

fn OP_PROTOCOL_INVOKE(self: *Self, location: Ast.TokenIndex, identifier: []const u8) !void {
    try self.emitCodeArg(
        location,
        .OP_PROTOCOL_INVOKE,
        try self.identifierConstant(identifier),
    );
}

fn OP_PROTOCOL_TAIL_INVOKE(self: *Self, location: Ast.TokenIndex, identifier: []const u8) !void {
    try self.emitCodeArg(
        location,
        .OP_PROTOCOL_TAIL_INVOKE,
        try self.identifierConstant(identifier),
    );
}

fn OP_CALL(self: *Self, location: Ast.TokenIndex, arg_count: u8, catch_default: bool) !void {
    try self.emitCodeArgs(
        location,
        .OP_CALL,
        arg_count,
        if (catch_default) 1 else 0,
    );
}

fn OP_TAIL_CALL(self: *Self, location: Ast.TokenIndex, arg_count: u24, catch_default: bool) !void {
    try self.emitCodeArgs(
        location,
        .OP_TAIL_CALL,
        arg_count,
        if (catch_default) 1 else 0,
    );
}

fn OP_FIBER(self: *Self, location: Ast.TokenIndex) !void {
    try self.emitOpCode(location, .OP_FIBER);
}

fn OP_CLOSURE(self: *Self, location: Ast.TokenIndex, closure: Value) !void {
    try self.emitCodeArg(
        location,
        .OP_CLOSURE,
        try self.makeConstant(closure),
    );
}
