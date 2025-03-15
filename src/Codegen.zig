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
const GarbageCollector = @import("memory.zig").GarbageCollector;
const Reporter = @import("Reporter.zig");
const BuildOptions = @import("build_options");
const JIT = if (!is_wasm) @import("Jit.zig") else void;
const disassembler = @import("disassembler.zig");
const io = @import("io.zig");

const Self = @This();

pub const Error = error{
    CantCompile,
    UnwrappedNull,
    OutOfBound,
    ReachedMaximumMemoryUsage,
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

const Breaks = std.ArrayListUnmanaged(Break);

current: ?*Frame = null,
ast: Ast.Slice = undefined,
gc: *GarbageCollector,
flavor: RunFlavor,
/// Jump to patch at end of current expression with a optional unwrapping in the middle of it
opt_jumps: std.ArrayListUnmanaged(std.ArrayListUnmanaged(usize)) = .{},
/// Used to generate error messages
parser: *Parser,
jit: ?*JIT,

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
    gc: *GarbageCollector,
    parser: *Parser,
    flavor: RunFlavor,
    jit: ?*JIT,
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

    // if (BuildOptions.debug) {
    //     var out = std.ArrayList(u8).init(self.gc.allocator);
    //     defer out.deinit();

    //     try root.node.toJson(&root.node, &out.writer());

    //     try io.stdOutWriter.print("\n{s}", .{out.items});
    // }

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
    const original: u32 = self.current.?.function.?.chunk.code.items[offset];
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

    const original: u32 = self.current.?.function.?.chunk.code.items[offset];
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
        std.debug.assert(self.opt_jumps.items.len > 0);

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
        for (closing) |op| {
            try self.emitOpCode(location, op);
        }
    }
}

fn generateNode(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    if (self.synchronize(node)) {
        return null;
    }

    if (Self.generators[@intFromEnum(self.ast.nodes.items(.tag)[node])]) |generator| {
        return generator(self, node, breaks);
    }

    return null;
}

fn nodeValue(self: *Self, node: Ast.Node.Index) Error!?Value {
    const value = &self.ast.nodes.items(.value)[node];

    if (value.* == null) {
        if (self.ast.isConstant(node)) {
            value.* = try self.ast.toValue(node, self.gc);
        }
    }

    return value.*;
}

fn generateAs(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const node_location = locations[node];
    const components = self.ast.nodes.items(.components)[node].As;

    const constant = try self.ast.toValue(components.constant, self.gc);

    std.debug.assert(constant.isObj() and constant.obj().obj_type == .Type);

    if (obj.ObjTypeDef.cast(constant.obj()).?.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(
            self.ast,
            obj.ObjTypeDef.cast(constant.obj()).?.resolved_type.?.Placeholder,
        );
    }

    _ = try self.generateNode(components.left, breaks);

    try self.OP_COPY(locations[components.left], 0);
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
    const end_locations = self.ast.nodes.items(.end_location);
    const type_defs = self.ast.nodes.items(.type_def);
    const left_type = type_defs[components.left].?;
    const right_type = type_defs[components.right].?;

    if (left_type.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, left_type.resolved_type.?.Placeholder);
    }

    if (right_type.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, right_type.resolved_type.?.Placeholder);
    }

    switch (components.operator) {
        .QuestionQuestion,
        .Ampersand,
        .Bor,
        .Xor,
        .ShiftLeft,
        .ShiftRight,
        .Plus,
        .Minus,
        .Star,
        .Slash,
        .Percent,
        .And,
        .Or,
        => {
            if (!left_type.eql(right_type)) {
                self.reporter.reportTypeCheck(
                    .binary_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    left_type,
                    self.ast.tokens.get(locations[components.right]),
                    self.ast.tokens.get(end_locations[components.right]),
                    right_type,
                    "Type mismatch",
                );
            }
        },

        .Greater,
        .Less,
        .GreaterEqual,
        .LessEqual,
        .BangEqual,
        .EqualEqual,
        => {
            // We allow comparison between double and int so raise error if type != and one operand is not a number
            if (!left_type.eql(right_type) and
                !right_type.eql(left_type) and
                ((left_type.def_type != .Integer and left_type.def_type != .Double) or (right_type.def_type != .Integer and right_type.def_type != .Double)))
            {
                self.reporter.reportTypeCheck(
                    .comparison_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    left_type,
                    self.ast.tokens.get(locations[components.right]),
                    self.ast.tokens.get(end_locations[components.right]),
                    right_type,
                    "Type mismatch",
                );
            }
        },

        else => unreachable,
    }

    if (components.operator != .QuestionQuestion and components.operator != .EqualEqual and components.operator != .BangEqual) {
        if (left_type.optional) {
            self.reporter.reportErrorAt(
                .binary_operand_type,
                self.ast.tokens.get(locations[components.left]),
                self.ast.tokens.get(end_locations[components.left]),
                "Binary operand can't be optional",
            );
        }

        if (right_type.optional) {
            self.reporter.reportErrorAt(
                .binary_operand_type,
                self.ast.tokens.get(locations[components.right]),
                self.ast.tokens.get(end_locations[components.right]),
                "Binary operand can't be optional",
            );
        }
    }

    switch (components.operator) {
        .QuestionQuestion => {
            if (!left_type.optional) {
                self.reporter.reportErrorAt(
                    .optional,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Not an optional",
                );
            }

            _ = try self.generateNode(components.left, breaks);

            const end_jump: usize = try self.OP_JUMP_IF_NOT_NULL(locations[node]);
            try self.OP_POP(locations[node]);

            _ = try self.generateNode(components.right, breaks);

            self.patchJump(end_jump);
        },
        .Ampersand => {
            // Checking only left operand since we asserted earlier that both operand have the same type
            if (left_type.def_type != .Integer) {
                self.reporter.reportErrorAt(
                    .bitwise_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected `int`.",
                );
            }

            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_BAND(locations[node]);
        },
        .Bor => {
            // Checking only left operand since we asserted earlier that both operand have the same type
            if (left_type.def_type != .Integer) {
                self.reporter.reportErrorAt(
                    .bitwise_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected `int`.",
                );
            }

            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_BOR(locations[node]);
        },
        .Xor => {
            // Checking only left operand since we asserted earlier that both operand have the same type
            if (left_type.def_type != .Integer) {
                self.reporter.reportErrorAt(
                    .bitwise_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected `int`.",
                );
            }

            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_XOR(locations[node]);
        },
        .ShiftLeft => {
            // Checking only left operand since we asserted earlier that both operand have the same type
            if (left_type.def_type != .Integer) {
                self.reporter.reportErrorAt(
                    .bitwise_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected `int`.",
                );
            }

            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_SHL(locations[node]);
        },
        .ShiftRight => {
            // Checking only left operand since we asserted earlier that both operand have the same type
            if (left_type.def_type != .Integer) {
                self.reporter.reportErrorAt(
                    .bitwise_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected `int`.",
                );
            }

            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_SHR(locations[node]);
        },
        .Greater => {
            // Checking only left operand since we asserted earlier that both operand have the same type
            if (left_type.def_type != .Integer and left_type.def_type != .Double) {
                self.reporter.reportErrorAt(
                    .comparison_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected `int` or `double`.",
                );
            }

            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_GREATER(locations[node]);
        },
        .Less => {
            if (left_type.def_type != .Integer and left_type.def_type != .Double) {
                self.reporter.reportErrorAt(
                    .comparison_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected `int` or `double`.",
                );
            }

            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_LESS(locations[node]);
        },
        .GreaterEqual => {
            if (left_type.def_type != .Integer and left_type.def_type != .Double) {
                self.reporter.reportErrorAt(
                    .comparison_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected `int` or `double`.",
                );
            }

            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);
            try self.OP_LESS(locations[node]);
            try self.OP_NOT(locations[node]);
        },
        .LessEqual => {
            if (left_type.def_type != .Integer and left_type.def_type != .Double) {
                self.reporter.reportErrorAt(
                    .comparison_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected `int` or `double`.",
                );
            }

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
            if (left_type.def_type != .Integer and
                left_type.def_type != .Double and
                left_type.def_type != .String and
                left_type.def_type != .List and
                left_type.def_type != .Map)
            {
                self.reporter.reportErrorAt(
                    .arithmetic_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected a `int`, `double`, `str`, list or map.",
                );
            }

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
            } else if (left_type.def_type == .Double) {
                try self.OP_SUBTRACT_F(locations[node]);
            } else {
                self.reporter.reportErrorAt(
                    .arithmetic_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected `int` or `double`.",
                );
            }
        },
        .Star => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);

            if (left_type.def_type == .Integer) {
                try self.OP_MULTIPLY_I(locations[node]);
            } else if (left_type.def_type == .Double) {
                try self.OP_MULTIPLY_F(locations[node]);
            } else {
                self.reporter.reportErrorAt(
                    .arithmetic_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected `int` or `double`.",
                );
            }
        },
        .Slash => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);

            if (left_type.def_type == .Integer) {
                try self.OP_DIVIDE_I(locations[node]);
            } else if (left_type.def_type == .Double) {
                try self.OP_DIVIDE_F(locations[node]);
            } else {
                self.reporter.reportErrorAt(
                    .arithmetic_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected `int` or `double`.",
                );
            }
        },
        .Percent => {
            _ = try self.generateNode(components.left, breaks);
            _ = try self.generateNode(components.right, breaks);

            if (left_type.def_type == .Integer) {
                try self.OP_MOD_I(locations[node]);
            } else if (left_type.def_type == .Double) {
                try self.OP_MOD_F(locations[node]);
            } else {
                self.reporter.reportErrorAt(
                    .arithmetic_operand_type,
                    self.ast.tokens.get(locations[components.left]),
                    self.ast.tokens.get(end_locations[components.left]),
                    "Expected `int` or `double`.",
                );
            }
        },
        .And => {
            if (left_type.def_type != .Bool) {
                self.reporter.reportErrorAt(
                    .logical_operand_type,
                    self.ast.tokens.get(locations[node]),
                    self.ast.tokens.get(end_locations[node]),
                    "`and` expects operands to be `bool`",
                );
            }

            _ = try self.generateNode(components.left, breaks);

            const end_jump: usize = try self.OP_JUMP_IF_FALSE(locations[node]);
            try self.OP_POP(locations[node]);

            _ = try self.generateNode(components.right, breaks);

            self.patchJump(end_jump);
        },
        .Or => {
            if (left_type.def_type != .Bool) {
                self.reporter.reportErrorAt(
                    .logical_operand_type,
                    self.ast.tokens.get(locations[node]),
                    self.ast.tokens.get(end_locations[node]),
                    "`and` expects operands to be `bool`",
                );
            }

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
            .label_node = self.ast.nodes.items(.components)[node].Break,
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
            .label_node = self.ast.nodes.items(.components)[node].Continue,
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
    if (callee_type_def.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, callee_type_def.resolved_type.?.Placeholder);
    }

    // This is not a call but an Enum(value)
    if (callee_type_def.def_type == .Enum) {
        if (components.is_async) {
            self.reporter.reportErrorAt(
                .fiber_call_not_allowed,
                self.ast.tokens.get(locations[components.callee]),
                self.ast.tokens.get(end_locations[components.callee]),
                "Can't be wrapped in a fiber",
            );
        }

        if (components.catch_default != null) {
            self.reporter.reportErrorAt(
                .no_error,
                self.ast.tokens.get(locations[components.callee]),
                self.ast.tokens.get(end_locations[components.callee]),
                "Doesn't raise any error",
            );
        }

        if (components.arguments.len != 1) {
            self.reporter.reportErrorAt(
                .enum_argument,
                self.ast.tokens.get(locations[components.callee]),
                self.ast.tokens.get(end_locations[components.callee]),
                "Enum instanciation requires only value argument",
            );
        }

        const value = components.arguments[0].value;

        if (type_defs[value].?.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, type_defs[value].?.resolved_type.?.Placeholder);
        }

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

    if (callee_type == null) {
        self.reporter.reportErrorAt(
            .undefined,
            self.ast.tokens.get(locations[components.callee]),
            self.ast.tokens.get(end_locations[components.callee]),
            "Callee is not defined",
        );
    } else if (callee_type.?.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, callee_type.?.resolved_type.?.Placeholder);

        // We know nothing about the function being called, no need to go any further
        return null;
    } else if (callee_type.?.def_type != .Function) {
        self.reporter.reportErrorAt(
            .callable,
            self.ast.tokens.get(locations[node]),
            self.ast.tokens.get(end_locations[node]),
            "Can't be called",
        );

        return null;
    } else if (callee_type.?.optional) {
        self.reporter.reportErrorAt(
            .callable,
            self.ast.tokens.get(locations[node]),
            self.ast.tokens.get(end_locations[node]),
            "Function maybe null and can't be called",
        );
    }

    const yield_type = callee_type.?.resolved_type.?.Function.yield_type;

    // Function being called and current function should have matching yield type unless the current function is an entrypoint
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

    var missing_arguments = std.StringArrayHashMap(usize).init(self.gc.allocator);
    defer missing_arguments.deinit();
    for (arg_keys, 0..) |arg_name, pindex| {
        try missing_arguments.put(arg_name.string, pindex);
    }

    if (components.arguments.len > args.count()) {
        self.reporter.reportErrorAt(
            .call_arguments,
            self.ast.tokens.get(locations[node]),
            self.ast.tokens.get(end_locations[node]),
            "Too many arguments.",
        );
    }

    // First push on the stack arguments has they are parsed
    var needs_reorder = false;
    for (components.arguments, 0..) |argument, index| {
        var argument_type_def = type_defs[argument.value].?;
        const arg_key = if (argument.name) |arg_name|
            try self.gc.copyString(lexemes[arg_name])
        else
            null;
        const actual_arg_key = if (index == 0 and arg_key == null)
            arg_keys[0]
        else
            arg_key.?;
        const def_arg_type = args.get(actual_arg_key);

        const ref_index = args.getIndex(actual_arg_key);
        if (index != ref_index) {
            needs_reorder = true;
        }

        // Type check the argument
        if (def_arg_type) |arg_type| {
            self.populateEmptyCollectionType(argument.value, arg_type);
            argument_type_def = type_defs[argument.value].?;

            if (argument_type_def.def_type == .Placeholder) {
                self.reporter.reportPlaceholder(self.ast, argument_type_def.resolved_type.?.Placeholder);
            } else if (!arg_type.eql(argument_type_def)) {
                self.reporter.reportTypeCheck(
                    .call_argument_type,
                    self.ast.tokens.get(locations[components.callee]),
                    self.ast.tokens.get(end_locations[components.callee]),
                    arg_type,
                    self.ast.tokens.get(locations[argument.value]),
                    self.ast.tokens.get(end_locations[argument.value]),
                    argument_type_def,
                    "Bad argument type",
                );
            }

            _ = missing_arguments.orderedRemove(actual_arg_key.string);
        } else {
            self.reporter.reportErrorFmt(
                .call_arguments,
                self.ast.tokens.get(locations[argument.value]),
                self.ast.tokens.get(end_locations[argument.value]),
                "Argument `{s}` does not exists.",
                .{if (arg_key) |key| key.string else "unknown"},
            );
        }

        _ = try self.generateNode(argument.value, breaks);
    }

    // Argument order reference
    var arguments_order_ref = std.ArrayList([]const u8).init(self.gc.allocator);
    defer arguments_order_ref.deinit();
    for (components.arguments) |arg| {
        try arguments_order_ref.append(
            if (arg.name) |name|
                lexemes[name]
            else
                "$",
        );
    }

    // Push default arguments
    if (missing_arguments.count() > 0) {
        var tmp_missing_arguments = try missing_arguments.clone();
        defer tmp_missing_arguments.deinit();
        const missing_keys = tmp_missing_arguments.keys();
        for (missing_keys) |missing_key| {
            if (defaults.get(try self.gc.copyString(missing_key))) |default| {
                // TODO: like ObjTypeDef, avoid generating constants multiple time for the same value
                try self.emitConstant(locations[node], default);
                try self.OP_CLONE(locations[node]);

                try arguments_order_ref.append(missing_key);
                _ = missing_arguments.orderedRemove(missing_key);
                needs_reorder = true;
            }
        }
    }

    if (missing_arguments.count() > 0) {
        var missing = std.ArrayList(u8).init(self.gc.allocator);
        const missing_writer = missing.writer();
        for (missing_arguments.keys(), 0..) |key, i| {
            try missing_writer.print(
                "{s}{s}",
                .{
                    key,
                    if (i < missing_arguments.keys().len - 1)
                        ", "
                    else
                        "",
                },
            );
        }
        defer missing.deinit();
        self.reporter.reportErrorFmt(
            .call_arguments,
            self.ast.tokens.get(locations[node]),
            self.ast.tokens.get(end_locations[node]),
            "Missing argument{s}: {s}",
            .{
                if (missing_arguments.count() > 1)
                    "s"
                else
                    "",
                missing.items,
            },
        );
    }

    // Reorder arguments
    if (needs_reorder) {
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
        const catch_default_type_def = type_defs[catch_default].?;
        if (error_types == null or error_types.?.len == 0) {
            self.reporter.reportErrorAt(
                .no_error,
                self.ast.tokens.get(locations[node]),
                self.ast.tokens.get(end_locations[node]),
                "Function doesn't raise any error",
            );
        } else if (error_types != null) {
            if (catch_default_type_def.def_type == .Placeholder) {
                self.reporter.reportPlaceholder(self.ast, catch_default_type_def.resolved_type.?.Placeholder);
            } else {
                const node_type_def = type_defs[node].?;
                // Expression
                if (!node_type_def.eql(catch_default_type_def) and !(try node_type_def.cloneOptional(&self.gc.type_registry)).eql(catch_default_type_def)) {
                    self.reporter.reportTypeCheck(
                        .inline_catch_type,
                        self.ast.tokens.get(locations[components.callee]),
                        self.ast.tokens.get(end_locations[components.callee]),
                        node_type_def,
                        self.ast.tokens.get(locations[catch_default]),
                        self.ast.tokens.get(end_locations[catch_default]),
                        catch_default_type_def,
                        "Bad inline catch value type",
                    );
                }
            }

            _ = try self.generateNode(catch_default, breaks);
        }
    } else if (error_types) |errors| {
        if (self.current.?.enclosing != null and self.current.?.function.?.type_def.resolved_type.?.Function.function_type != .Test) {
            var handles_any = false;
            var not_handled = std.ArrayList(*obj.ObjTypeDef).init(self.gc.allocator);
            defer not_handled.deinit();
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
                        try not_handled.append(error_type);
                    }
                }

                if (handles_any) {
                    not_handled.clearAndFree();
                    break;
                }
            }

            for (not_handled.items) |error_type| {
                const error_str = try error_type.toStringAlloc(self.gc.allocator);
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

// FIXME: this is become a unreadable mess
fn generateDot(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const node_components = self.ast.nodes.items(.components);
    const type_defs = self.ast.nodes.items(.type_def);
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);
    const tags = self.ast.tokens.items(.tag);

    const components = node_components[node].Dot;
    const identifier_lexeme = self.ast.tokens.items(.lexeme)[components.identifier];

    _ = try self.generateNode(components.callee, breaks);

    const callee_type = type_defs[components.callee].?;

    if (callee_type.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, callee_type.resolved_type.?.Placeholder);
    }

    switch (callee_type.def_type) {
        .ObjectInstance,
        .Object,
        .ProtocolInstance,
        .Enum,
        .EnumInstance,
        .List,
        .Map,
        .String,
        .Pattern,
        .Fiber,
        .ForeignContainer,
        .Range,
        => {},
        else => self.reporter.reportErrorAt(
            .field_access,
            self.ast.tokens.get(locations[node]),
            self.ast.tokens.get(end_locations[node]),
            "Doesn't have field access",
        ),
    }

    if (callee_type.optional) {
        self.reporter.reportErrorAt(
            .field_access,
            self.ast.tokens.get(locations[node]),
            self.ast.tokens.get(end_locations[node]),
            "Optional doesn't have field access",
        );
    }

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
                try self.OP_COPY(locations[node], 0);
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

                    // Type check value
                    switch (callee_type.def_type) {
                        .ForeignContainer => {
                            const field_type = callee_type.resolved_type.?.ForeignContainer.buzz_type.get(field_name).?;

                            if (!field_type.eql(value_type_def)) {
                                self.reporter.reportTypeCheck(
                                    .assignment_value_type,
                                    callee_type.resolved_type.?.ForeignContainer.location,
                                    callee_type.resolved_type.?.ForeignContainer.location,
                                    field_type,
                                    self.ast.tokens.get(locations[value]),
                                    self.ast.tokens.get(end_locations[value]),
                                    value_type_def,
                                    "Bad property type",
                                );
                            }
                        },
                        .ObjectInstance, .Object => {
                            if (field.?.method or
                                (callee_type.def_type == .ObjectInstance and field.?.static) or
                                (callee_type.def_type == .Object and !field.?.static))
                            {
                                self.reporter.reportErrorFmt(
                                    .assignable,
                                    self.ast.tokens.get(locations[components.callee]),
                                    self.ast.tokens.get(end_locations[components.callee]),
                                    "`{s}` is not assignable",
                                    .{
                                        field_name,
                                    },
                                );
                            } else if (field.?.final) {
                                self.reporter.reportErrorFmt(
                                    .constant_property,
                                    self.ast.tokens.get(locations[components.callee]),
                                    self.ast.tokens.get(end_locations[components.callee]),
                                    "`{s}` is final",
                                    .{
                                        field_name,
                                    },
                                );
                            } else if (callee_type.def_type == .ObjectInstance and !callee_type.resolved_type.?.ObjectInstance.mutable) {
                                self.reporter.reportWithOrigin(
                                    .not_mutable,
                                    self.ast.tokens.get(locations[components.callee]),
                                    self.ast.tokens.get(end_locations[components.callee]),
                                    callee_type.resolved_type.?.ObjectInstance.of
                                        .resolved_type.?.Object.location,
                                    callee_type.resolved_type.?.ObjectInstance.of
                                        .resolved_type.?.Object.location,
                                    "Instance of `{s}` is not mutable",
                                    .{
                                        callee_type.resolved_type.?.ObjectInstance.of
                                            .resolved_type.?.Object.qualified_name.string,
                                    },
                                    "declared here",
                                );
                            }

                            self.populateEmptyCollectionType(value, field.?.type_def);
                            value_type_def = type_defs[value].?;

                            if (!field.?.type_def.eql(value_type_def)) {
                                self.reporter.reportTypeCheck(
                                    .assignment_value_type,
                                    field.?.location,
                                    field.?.location,
                                    field.?.type_def,
                                    self.ast.tokens.get(locations[value]),
                                    self.ast.tokens.get(end_locations[value]),
                                    value_type_def,
                                    "Bad property type",
                                );
                            }
                        },
                        else => unreachable,
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
                            try self.OP_COPY(locations[node], 0);

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

                    // Type check that operator is allowed
                    switch (tags[assign_token]) {
                        .PlusEqual => switch (type_defs[value].?.def_type) {
                            .Integer,
                            .Double,
                            .List,
                            .Map,
                            .String,
                            => {},
                            else => self.reporter.report(
                                .arithmetic_operand_type,
                                self.ast.tokens.get(assign_token),
                                self.ast.tokens.get(assign_token),
                                "Addition is only allowed for types `int`, `double`, list, map and `str`",
                            ),
                        },
                        .MinusEqual,
                        .StarEqual,
                        .SlashEqual,
                        .PercentEqual,
                        => switch (type_defs[value].?.def_type) {
                            .Integer, .Double => {},
                            else => self.reporter.report(
                                .arithmetic_operand_type,
                                self.ast.tokens.get(assign_token),
                                self.ast.tokens.get(assign_token),
                                "Operator is only allowed for types `int`, `double`",
                            ),
                        },
                        .ShiftRightEqual,
                        .ShiftLeftEqual,
                        .XorEqual,
                        .BorEqual,
                        .BnotEqual,
                        .AmpersandEqual,
                        => if (type_defs[value].?.def_type != .Integer) {
                            self.reporter.report(
                                .arithmetic_operand_type,
                                self.ast.tokens.get(assign_token),
                                self.ast.tokens.get(assign_token),
                                "Operator is only allowed for `int`",
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
                    if (callee_type.def_type == .ForeignContainer) {
                        self.reporter.reportErrorAt(
                            .callable,
                            self.ast.tokens.get(locations[components.callee]),
                            self.ast.tokens.get(end_locations[components.callee]),
                            "Not callable",
                        );
                    }

                    // Static call
                    if (callee_type.def_type == .Object) {
                        try self.emitCodeArg(
                            locations[node],
                            get_code.?,
                            @intCast(field.?.index),
                        );
                    } else if (field.?.mutable and !callee_type.resolved_type.?.ObjectInstance.mutable) {
                        self.reporter.report(
                            .not_mutable,
                            self.ast.tokens.get(components.identifier),
                            self.ast.tokens.get(components.identifier),
                            "Method requires mutable instance",
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
                const field_name = self.ast.tokens.items(.lexeme)[components.identifier];
                const field = callee_type.resolved_type.?.ProtocolInstance.of
                    .resolved_type.?.Protocol
                    .methods
                    .get(field_name);

                if (field.?.mutable and !callee_type.resolved_type.?.ProtocolInstance.mutable) {
                    self.reporter.report(
                        .not_mutable,
                        self.ast.tokens.get(components.identifier),
                        self.ast.tokens.get(components.identifier),
                        "Method requires mutable instance",
                    );
                }

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
                const identifier = self.ast.tokens.items(.lexeme)[components.identifier];

                try self.OP_COPY(locations[node], 0);

                switch (callee_type.def_type) {
                    .List => if (callee_type.resolved_type.?.List.methods.get(identifier).?.mutable and
                        !callee_type.resolved_type.?.List.mutable)
                    {
                        self.reporter.reportErrorFmt(
                            .not_mutable,
                            self.ast.tokens.get(components.identifier),
                            self.ast.tokens.get(components.identifier),
                            "Method `{s}` requires mutable list",
                            .{
                                identifier,
                            },
                        );
                    },
                    .Map => if (callee_type.resolved_type.?.Map.methods.get(identifier).?.mutable and
                        !callee_type.resolved_type.?.Map.mutable)
                    {
                        self.reporter.reportErrorFmt(
                            .not_mutable,
                            self.ast.tokens.get(components.identifier),
                            self.ast.tokens.get(components.identifier),
                            "Method `{s}` requires mutable list",
                            .{
                                identifier,
                            },
                        );
                    },
                    else => {},
                }

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
    const end_locations = self.ast.nodes.items(.end_location);
    const type_defs = self.ast.nodes.items(.type_def);
    const node_components = self.ast.nodes.items(.components);
    const components = node_components[node].DoUntil;

    const loop_start = self.currentCode();

    var lbreaks = Breaks{};
    defer lbreaks.deinit(self.gc.allocator);

    _ = try self.generateNode(components.body, &lbreaks);

    const condition_type_def = type_defs[components.condition].?;

    if (condition_type_def.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, condition_type_def.resolved_type.?.Placeholder);
    }

    if (condition_type_def.def_type != .Bool) {
        self.reporter.reportErrorAt(
            .do_condition_type,
            self.ast.tokens.get(locations[components.condition]),
            self.ast.tokens.get(end_locations[components.condition]),
            "`do` condition must be bool",
        );
    }

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
    const end_locations = self.ast.nodes.items(.end_location);
    const type_defs = self.ast.nodes.items(.type_def);
    const node_components = self.ast.nodes.items(.components);
    const components = node_components[node].Enum;

    const enum_type = type_defs[node].?.resolved_type.?.Enum.enum_type;
    if (enum_type.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, enum_type.resolved_type.?.Placeholder);

        return null;
    }

    switch (enum_type.def_type) {
        .String,
        .Integer,
        .Double,
        .Pattern,
        .UserData,
        .Void,
        .Range,
        => {},
        else => {
            self.reporter.reportErrorAt(
                .syntax,
                self.ast.tokens.get(locations[node]),
                self.ast.tokens.get(end_locations[node]),
                "Type not allowed as enum value",
            );
            return null;
        },
    }

    for (components.cases) |case| {
        const case_type_def = if (case.value) |value|
            type_defs[value].?
        else
            null;

        if (case_type_def) |case_type| {
            if (case_type.def_type == .Placeholder) {
                self.reporter.reportPlaceholder(self.ast, case_type.resolved_type.?.Placeholder);
            } else if (!((try enum_type.toInstance(&self.gc.type_registry, false))).eql(case_type)) {
                self.reporter.reportTypeCheck(
                    .enum_case_type,
                    self.ast.tokens.get(locations[node]),
                    self.ast.tokens.get(end_locations[node]),
                    (try enum_type.toInstance(
                        &self.gc.type_registry,
                        false,
                    )),
                    self.ast.tokens.get(locations[case.value.?]),
                    self.ast.tokens.get(end_locations[case.value.?]),
                    case_type,
                    "Bad enum case type",
                );
            }
        }
    }

    try self.OP_CONSTANT(
        locations[node],
        try self.ast.toValue(node, self.gc),
    );
    try self.OP_DEFINE_GLOBAL(
        locations[node],
        @intCast(components.slot),
        self.parser.ast.tokens.items(.lexeme)[components.identifier.?],
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
        const type_def_str = expr_type_def.?.toStringAlloc(self.gc.allocator) catch unreachable;
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
        try self.ast.toValue(node, self.gc),
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
    if (try self.ast.isConstant(self.gc.allocator, components.condition) and !(try self.ast.toValue(components.condition, self.gc)).boolean()) {
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
    const end_locations = self.ast.nodes.items(.end_location);
    const components = self.ast.nodes.items(.components)[node].ForceUnwrap;

    if (components.original_type.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, components.original_type.resolved_type.?.Placeholder);

        return null;
    }

    if (!components.original_type.optional) {
        self.reporter.reportErrorAt(
            .optional,
            self.ast.tokens.get(locations[components.unwrapped]),
            self.ast.tokens.get(end_locations[components.unwrapped]),
            "Not an optional",
        );
    }

    _ = try self.generateNode(components.unwrapped, breaks);

    try self.OP_UNWRAP(locations[node]);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateForEach(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const node_components = self.ast.nodes.items(.components);
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);
    const type_defs = self.ast.nodes.items(.type_def);
    const components = node_components[node].ForEach;

    // Type checking
    const iterable_type_def = type_defs[components.iterable].?;
    var key_type_def = type_defs[components.key].?;
    const value_type_def = type_defs[components.value].?;
    if (iterable_type_def.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, iterable_type_def.resolved_type.?.Placeholder);
    } else {
        if (!components.key_omitted) {
            if (key_type_def.def_type == .Placeholder) {
                self.reporter.reportPlaceholder(self.ast, key_type_def.resolved_type.?.Placeholder);
            }

            switch (iterable_type_def.def_type) {
                .String, .List => {
                    if (key_type_def.def_type != .Integer) {
                        self.reporter.reportErrorAt(
                            .foreach_key_type,
                            self.ast.tokens.get(locations[components.key]),
                            self.ast.tokens.get(end_locations[components.key]),
                            "Expected `int`.",
                        );
                    }
                },
                .Map => {
                    if (!iterable_type_def.resolved_type.?.Map.key_type.strictEql(key_type_def)) {
                        self.reporter.reportTypeCheck(
                            .foreach_key_type,
                            self.ast.tokens.get(locations[components.iterable]),
                            self.ast.tokens.get(end_locations[components.iterable]),
                            iterable_type_def.resolved_type.?.Map.key_type,
                            self.ast.tokens.get(locations[components.key]),
                            self.ast.tokens.get(end_locations[components.key]),
                            key_type_def,
                            "Bad key type",
                        );
                    }
                },
                .Enum => self.reporter.reportErrorAt(
                    .foreach_key_type,
                    self.ast.tokens.get(locations[components.key]),
                    self.ast.tokens.get(end_locations[components.key]),
                    "No key available when iterating over enum.",
                ),
                .Range => self.reporter.reportErrorAt(
                    .foreach_key_type,
                    self.ast.tokens.get(locations[components.key]),
                    self.ast.tokens.get(end_locations[components.key]),
                    "No key available when iterating over range.",
                ),
                else => self.reporter.reportErrorAt(
                    .foreach_iterable,
                    self.ast.tokens.get(locations[components.iterable]),
                    self.ast.tokens.get(end_locations[components.iterable]),
                    "Not iterable.",
                ),
            }
        } else {
            // Key was omitted, put the correct type in the key var declation to avoid raising errors
            switch (iterable_type_def.def_type) {
                .Map => key_type_def = iterable_type_def.resolved_type.?.Map.key_type,
                .String, .List => key_type_def = self.gc.type_registry.int_type,
                else => {},
            }
        }

        if (value_type_def.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, value_type_def.resolved_type.?.Placeholder);
        }

        switch (iterable_type_def.def_type) {
            .Map => {
                if (!iterable_type_def.resolved_type.?.Map.value_type.strictEql(value_type_def)) {
                    self.reporter.reportTypeCheck(
                        .foreach_value_type,
                        self.ast.tokens.get(locations[components.iterable]),
                        self.ast.tokens.get(end_locations[components.iterable]),
                        iterable_type_def.resolved_type.?.Map.value_type,
                        self.ast.tokens.get(locations[components.value]),
                        self.ast.tokens.get(end_locations[components.value]),
                        value_type_def,
                        "Bad value type",
                    );
                }
            },
            .List => {
                if (!iterable_type_def.resolved_type.?.List.item_type.strictEql(value_type_def)) {
                    self.reporter.reportTypeCheck(
                        .foreach_value_type,
                        self.ast.tokens.get(locations[components.iterable]),
                        self.ast.tokens.get(end_locations[components.iterable]),
                        iterable_type_def.resolved_type.?.List.item_type,
                        self.ast.tokens.get(locations[components.value]),
                        self.ast.tokens.get(end_locations[components.value]),
                        value_type_def,
                        "Bad value type",
                    );
                }
            },
            .Range => {
                if (value_type_def.def_type != .Integer or value_type_def.optional) {
                    self.reporter.reportTypeCheck(
                        .foreach_value_type,
                        self.ast.tokens.get(locations[components.iterable]),
                        self.ast.tokens.get(end_locations[components.iterable]),
                        self.gc.type_registry.int_type,
                        self.ast.tokens.get(locations[components.value]),
                        self.ast.tokens.get(end_locations[components.value]),
                        value_type_def,
                        "Bad value type",
                    );
                }
            },
            .String => {
                if (value_type_def.def_type != .String) {
                    self.reporter.reportTypeCheck(
                        .foreach_value_type,
                        self.ast.tokens.get(locations[components.iterable]),
                        self.ast.tokens.get(end_locations[components.iterable]),
                        self.gc.type_registry.str_type,
                        self.ast.tokens.get(locations[components.value]),
                        self.ast.tokens.get(end_locations[components.value]),
                        value_type_def,
                        "Bad value type",
                    );
                }
            },
            .Enum => {
                const iterable_type = try iterable_type_def.toInstance(
                    &self.gc.type_registry,
                    false,
                );
                if (!iterable_type.strictEql(value_type_def)) {
                    self.reporter.reportTypeCheck(
                        .foreach_value_type,
                        self.ast.tokens.get(locations[components.iterable]),
                        self.ast.tokens.get(end_locations[components.iterable]),
                        iterable_type,
                        self.ast.tokens.get(locations[components.value]),
                        self.ast.tokens.get(end_locations[components.value]),
                        value_type_def,
                        "Bad value type",
                    );
                }
            },
            .Fiber => {
                const iterable_type = try iterable_type_def.resolved_type.?.Fiber.yield_type.toInstance(
                    &self.gc.type_registry,
                    false,
                );
                if (!iterable_type.strictEql(value_type_def)) {
                    self.reporter.reportTypeCheck(
                        .foreach_value_type,
                        self.ast.tokens.get(locations[components.iterable]),
                        self.ast.tokens.get(end_locations[components.iterable]),
                        iterable_type,
                        self.ast.tokens.get(locations[components.value]),
                        self.ast.tokens.get(end_locations[components.value]),
                        value_type_def,
                        "Bad value type",
                    );
                }
            },
            else => self.reporter.reportErrorAt(
                .foreach_iterable,
                self.ast.tokens.get(locations[components.iterable]),
                self.ast.tokens.get(end_locations[components.iterable]),
                "Not iterable.",
            ),
        }
    }

    // If iterable constant and empty, skip the node
    if (try self.ast.isConstant(self.gc.allocator, components.iterable)) {
        const iterable = (try self.ast.toValue(components.iterable, self.gc)).obj();

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
                            try self.ast.toValue(default, self.gc),
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

    self.current.?.function = try self.gc.allocateObject(obj.ObjFunction, function);

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
            _ = self.current.?.function.?.chunk.lines.pop();

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
    const function_component = node_components[components.function].Function;

    _ = try self.generateNode(components.function, breaks);

    if (components.slot_type == .Global) {
        try self.OP_DEFINE_GLOBAL(
            self.ast.nodes.items(.location)[node],
            @intCast(components.slot),
            self.parser.ast.tokens.items(.lexeme)[function_component.identifier.?],
        );
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateGenericResolve(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const type_def = self.ast.nodes.items(.type_def)[node].?;
    const expr = self.ast.nodes.items(.components)[node].GenericResolve;
    const node_location = self.ast.nodes.items(.location)[node];
    const node_end_location = self.ast.nodes.items(.end_location)[node];

    switch (type_def.def_type) {
        .Function => {
            const function_type = type_def.resolved_type.?.Function;

            if (function_type.generic_types.count() > 0 and
                (function_type.resolved_generics == null or function_type.resolved_generics.?.len < function_type.generic_types.count()))
            {
                self.reporter.reportErrorFmt(
                    .generic_type,
                    self.ast.tokens.get(node_location),
                    self.ast.tokens.get(node_end_location),
                    "Missing generic types. Expected {} got {}.",
                    .{
                        function_type.generic_types.count(),
                        if (function_type.resolved_generics == null)
                            0
                        else
                            function_type.resolved_generics.?.len,
                    },
                );
            } else if (function_type.resolved_generics != null and function_type.resolved_generics.?.len > function_type.generic_types.count()) {
                self.reporter.reportErrorFmt(
                    .generic_type,
                    self.ast.tokens.get(node_location),
                    self.ast.tokens.get(node_end_location),
                    "Too many generic types. Expected {} got {}.",
                    .{
                        function_type.generic_types.count(),
                        if (function_type.resolved_generics == null)
                            0
                        else
                            function_type.resolved_generics.?.len,
                    },
                );
            }
        },
        .Object => {
            const object_type = type_def.resolved_type.?.Object;

            if (object_type.generic_types.count() > 0 and
                (object_type.resolved_generics == null or object_type.resolved_generics.?.len < object_type.generic_types.count()))
            {
                self.reporter.reportErrorFmt(
                    .generic_type,
                    self.ast.tokens.get(node_location),
                    self.ast.tokens.get(node_end_location),
                    "Missing generic types. Expected {} got {}.",
                    .{
                        object_type.generic_types.count(),
                        if (object_type.resolved_generics == null)
                            0
                        else
                            object_type.resolved_generics.?.len,
                    },
                );
            } else if (object_type.resolved_generics != null and object_type.resolved_generics.?.len > object_type.generic_types.count()) {
                self.reporter.reportErrorFmt(
                    .generic_type,
                    self.ast.tokens.get(node_location),
                    self.ast.tokens.get(node_end_location),
                    "Too many generic types. Expected {} got {}.",
                    .{
                        object_type.generic_types.count(),
                        if (object_type.resolved_generics == null)
                            0
                        else
                            object_type.resolved_generics.?.len,
                    },
                );
            }
        },
        else => {
            const type_def_str = type_def.toStringAlloc(self.gc.allocator) catch unreachable;
            defer self.gc.allocator.free(type_def_str);

            self.reporter.reportErrorFmt(
                .generic_type,
                self.ast.tokens.get(node_location),
                self.ast.tokens.get(node_end_location),
                "Type `{s}` does not support generic types",
                .{
                    type_def_str,
                },
            );
        },
    }

    _ = try self.generateNode(expr, breaks);

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
    const end_locations = self.ast.nodes.items(.end_location);
    const node_components = self.ast.nodes.items(.components);
    const components = node_components[node].If;
    const location = locations[node];

    // Type checking
    if (type_defs[components.condition].?.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, type_defs[components.condition].?.resolved_type.?.Placeholder);
    }

    if (!components.is_statement) {
        if (type_defs[components.body].?.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, type_defs[components.body].?.resolved_type.?.Placeholder);
        }

        if (type_defs[components.else_branch.?].?.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, type_defs[components.else_branch.?].?.resolved_type.?.Placeholder);
        }

        // Both should have same type
        if (!type_defs[node].?.eql(type_defs[components.body].?)) {
            self.reporter.reportTypeCheck(
                .inline_if_body_type,
                self.ast.tokens.get(location),
                self.ast.tokens.get(end_locations[node]),
                type_defs[node].?,
                self.ast.tokens.get(locations[components.body]),
                self.ast.tokens.get(end_locations[components.body]),
                type_defs[components.body].?,
                "Inline if body type not matching",
            );
        }

        if (!type_defs[node].?.eql(type_defs[components.else_branch.?].?)) {
            self.reporter.reportTypeCheck(
                .inline_if_else_type,
                self.ast.tokens.get(location),
                self.ast.tokens.get(end_locations[node]),
                type_defs[node].?,
                self.ast.tokens.get(locations[components.else_branch.?]),
                self.ast.tokens.get(end_locations[components.else_branch.?]),
                type_defs[components.else_branch.?].?,
                "Inline if else type not matching",
            );
        }
    }

    if (components.unwrapped_identifier != null) {
        if (!type_defs[components.condition].?.optional) {
            self.reporter.reportErrorAt(
                .optional,
                self.ast.tokens.get(locations[components.condition]),
                self.ast.tokens.get(end_locations[components.condition]),
                "Expected optional",
            );
        }
    } else if (components.casted_type == null) {
        if (type_defs[components.condition].?.def_type != .Bool) {
            self.reporter.reportErrorAt(
                .if_condition_type,
                self.ast.tokens.get(locations[components.condition]),
                self.ast.tokens.get(end_locations[components.condition]),
                "`if` condition must be bool",
            );
        }
    }

    // If condition is a constant expression, no need to generate branches
    if (try self.ast.isConstant(self.gc.allocator, components.condition) and components.unwrapped_identifier == null and components.casted_type == null) {
        const condition = try self.ast.toValue(components.condition, self.gc);

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
    if (components.unwrapped_identifier != null) {
        try self.OP_COPY(condition_location, 0);
        try self.OP_NULL(condition_location);
        try self.OP_EQUAL(condition_location);
        try self.OP_NOT(condition_location);
    } else if (components.casted_type) |casted_type| {
        try self.OP_COPY(condition_location, 0);
        try self.emitConstant(condition_location, type_defs[casted_type].?.toValue());
        try self.OP_IS(condition_location);
    }

    const else_jump: usize = try self.OP_JUMP_IF_FALSE(location);
    try self.OP_POP(location);

    _ = try self.generateNode(components.body, breaks);

    const out_jump: usize = try self.emitJump(location, .OP_JUMP);

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
        try self.ast.toValue(node, self.gc),
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateIs(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].Is;
    const location = self.ast.nodes.items(.location)[node];
    const constant = try self.ast.toValue(components.constant, self.gc);

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
    const end_locations = self.ast.nodes.items(.end_location);
    const components = self.ast.nodes.items(.components)[node].List;
    const type_defs = self.ast.nodes.items(.type_def);

    const item_type = type_defs[node].?.resolved_type.?.List.item_type;

    try self.OP_LIST(
        locations[node],
        type_defs[node].?.toValue(),
    );

    for (components.items) |item| {
        if (item_type.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, type_defs[item].?.resolved_type.?.Placeholder);
        } else if (!item_type.eql(type_defs[item].?)) {
            self.reporter.reportTypeCheck(
                .list_item_type,
                self.ast.tokens.get(locations[node]),
                self.ast.tokens.get(end_locations[node]),
                item_type,
                self.ast.tokens.get(locations[item]),
                self.ast.tokens.get(end_locations[item]),
                type_defs[item].?,
                "Bad list type",
            );
        } else {
            _ = try self.generateNode(item, breaks);
        }
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
    const end_locations = self.ast.nodes.items(.end_location);
    const components = self.ast.nodes.items(.components)[node].Map;
    const type_defs = self.ast.nodes.items(.type_def);

    const key_type = if (components.explicit_key_type) |kt|
        type_defs[kt]
    else
        null;

    const value_type = if (components.explicit_value_type) |vt|
        type_defs[vt]
    else
        null;

    try self.OP_MAP(
        locations[node],
        type_defs[node].?.toValue(),
    );

    for (components.entries) |entry| {
        _ = try self.generateNode(entry.key, breaks);
        _ = try self.generateNode(entry.value, breaks);

        if (type_defs[entry.key].?.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, type_defs[entry.key].?.resolved_type.?.Placeholder);
        }

        if (type_defs[entry.value].?.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, type_defs[entry.value].?.resolved_type.?.Placeholder);
        }

        if (key_type != null and !key_type.?.eql(type_defs[entry.key].?)) {
            self.reporter.reportTypeCheck(
                .map_key_type,
                self.ast.tokens.get(locations[node]),
                self.ast.tokens.get(end_locations[node]),
                key_type.?,
                self.ast.tokens.get(locations[entry.key]),
                self.ast.tokens.get(end_locations[entry.key]),
                type_defs[entry.key].?,
                "Bad key type",
            );
        }

        if (value_type != null and !value_type.?.eql(type_defs[entry.value].?)) {
            self.reporter.reportTypeCheck(
                .map_value_type,
                self.ast.tokens.get(locations[node]),
                self.ast.tokens.get(end_locations[node]),
                value_type.?,
                self.ast.tokens.get(locations[entry.value]),
                self.ast.tokens.get(end_locations[entry.value]),
                type_defs[entry.value].?,
                "Bad value type",
            );
        }
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
    const end_locations = self.ast.nodes.items(.end_location);
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
        // Type checking
        if (type_defs[node].?.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, type_defs[node].?.resolved_type.?.Placeholder);
        }

        if (!type_defs[node].?.eql(type_defs[value].?)) {
            self.reporter.reportTypeCheck(
                .assignment_value_type,
                self.ast.tokens.get(locations[node]),
                self.ast.tokens.get(end_locations[node]),
                type_defs[node].?,
                self.ast.tokens.get(locations[value]),
                self.ast.tokens.get(end_locations[value]),
                type_defs[value].?,
                "Bad value type",
            );
        }

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

        // Type check that operator is allowed
        switch (tags[components.assign_token.?]) {
            .PlusEqual => switch (type_defs[node].?.def_type) {
                .Integer,
                .Double,
                .List,
                .Map,
                .String,
                => {},
                else => self.reporter.report(
                    .arithmetic_operand_type,
                    self.ast.tokens.get(components.assign_token.?),
                    self.ast.tokens.get(components.assign_token.?),
                    "Addition is only allowed for types `int`, `double`, list, map and `str`",
                ),
            },
            .MinusEqual,
            .StarEqual,
            .SlashEqual,
            .PercentEqual,
            => switch (type_defs[node].?.def_type) {
                .Integer, .Double => {},
                else => self.reporter.report(
                    .arithmetic_operand_type,
                    self.ast.tokens.get(components.assign_token.?),
                    self.ast.tokens.get(components.assign_token.?),
                    "Operator is only allowed for types `int`, `double`",
                ),
            },
            .ShiftRightEqual,
            .ShiftLeftEqual,
            .XorEqual,
            .BorEqual,
            .BnotEqual,
            .AmpersandEqual,
            => if (type_defs[node].?.def_type != .Integer) {
                self.reporter.report(
                    .arithmetic_operand_type,
                    self.ast.tokens.get(components.assign_token.?),
                    self.ast.tokens.get(components.assign_token.?),
                    "Operator is only allowed for `int`",
                );
            },
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
    const end_locations = self.ast.nodes.items(.end_location);
    const type_defs = self.ast.nodes.items(.type_def);
    const lexemes = self.ast.tokens.items(.lexeme);
    const components = self.ast.nodes.items(.components)[node].ObjectDeclaration;
    const location = locations[node];

    const object_type = type_defs[node].?;
    const object_def = object_type.resolved_type.?.Object;

    // Check object conforms to declared protocols
    var protocol_it = object_def.conforms_to.iterator();
    while (protocol_it.next()) |kv| {
        const protocol_type_def = kv.key_ptr.*;

        if (protocol_type_def.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, protocol_type_def.resolved_type.?.Placeholder);
        } else {
            const protocol_def = protocol_type_def.resolved_type.?.Protocol;

            var method_it = protocol_def.methods.iterator();
            while (method_it.next()) |mkv| {
                var found = false;
                for (components.members) |member| {
                    if (member.method and std.mem.eql(u8, self.ast.tokens.items(.lexeme)[member.name], mkv.key_ptr.*)) {
                        found = true;
                        if (type_defs[member.method_or_default_value.?].?.def_type == .Placeholder) {
                            self.reporter.reportPlaceholder(
                                self.ast,
                                type_defs[member.method_or_default_value.?].?.resolved_type.?.Placeholder,
                            );
                        } else if (!mkv.value_ptr.*.type_def.eql(type_defs[member.method_or_default_value.?].?) or
                            mkv.value_ptr.*.mutable != object_def.fields.get(mkv.key_ptr.*).?.mutable)
                        {
                            self.reporter.reportTypeCheck(
                                .protocol_conforming,
                                protocol_def.location,
                                protocol_def.location,
                                mkv.value_ptr.*.type_def,
                                self.ast.tokens.get(locations[member.method_or_default_value.?]),
                                self.ast.tokens.get(end_locations[member.method_or_default_value.?]),
                                type_defs[member.method_or_default_value.?].?,
                                "Method not conforming to protocol",
                            );
                        }
                        break;
                    }
                }

                if (!found) {
                    self.reporter.reportWithOrigin(
                        .protocol_conforming,
                        self.ast.tokens.get(location),
                        self.ast.tokens.get(end_locations[node]),
                        protocol_def.methods_locations.get(mkv.value_ptr.*.type_def.resolved_type.?.Function.name.string).?,
                        protocol_def.methods_locations.get(mkv.value_ptr.*.type_def.resolved_type.?.Function.name.string).?,
                        "Object declared as conforming to protocol `{s}` but doesn't implement method `{s}`",
                        .{
                            protocol_def.name.string,
                            mkv.value_ptr.*.type_def.resolved_type.?.Function.name.string,
                        },
                        null,
                    );
                }
            }
        }
    }

    // Put  object on the stack and define global with it
    try self.OP_OBJECT(location, object_type.toValue());
    try self.OP_DEFINE_GLOBAL(
        location,
        @intCast(components.slot),
        self.parser.ast.tokens.items(.lexeme)[components.identifier.?],
    );

    // Put the object on the stack to set its fields
    try self.OP_GET_GLOBAL(location, @intCast(components.slot));

    for (components.members) |member| {
        const member_name = lexemes[member.name];

        if (member.method) {
            // Method
            const member_field = object_def.fields.get(member_name).?;
            const member_type_def = member_field.type_def;
            const member_idx = member_field.index;

            if (member_type_def.def_type == .Placeholder) {
                self.reporter.reportPlaceholder(self.ast, member_type_def.resolved_type.?.Placeholder);
            }

            // Enforce "collect" method signature
            if (std.mem.eql(u8, member_name, "collect")) {
                const collect_def = member_type_def.resolved_type.?.Function;

                if (collect_def.parameters.count() > 0 or
                    collect_def.return_type.def_type != .Void or
                    collect_def.yield_type.def_type != .Void or
                    collect_def.error_types != null)
                {
                    const collect_def_str = member_type_def.toStringAlloc(self.gc.allocator) catch @panic("Out of memory");
                    defer self.gc.allocator.free(collect_def_str);
                    self.reporter.reportErrorFmt(
                        .collect_signature,
                        self.ast.tokens.get(locations[member.method_or_default_value.?]),
                        self.ast.tokens.get(end_locations[member.method_or_default_value.?]),
                        "Expected `collect` method to be `fun collect() > void` got {s}",
                        .{
                            collect_def_str,
                        },
                    );
                }
            } else if (std.mem.eql(u8, member_name, "toString")) { // Enforce "toString" method signature
                const tostring_def = member_type_def.resolved_type.?.Function;

                if (tostring_def.parameters.count() > 0 or
                    tostring_def.return_type.def_type != .String or
                    tostring_def.yield_type.def_type != .Void or
                    tostring_def.error_types != null or
                    tostring_def.generic_types.count() > 0)
                {
                    const tostring_def_str = member_type_def.toStringAlloc(self.gc.allocator) catch @panic("Out of memory");
                    defer self.gc.allocator.free(tostring_def_str);
                    self.reporter.reportErrorFmt(
                        .tostring_signature,
                        self.ast.tokens.get(locations[member.method_or_default_value.?]),
                        self.ast.tokens.get(end_locations[member.method_or_default_value.?]),
                        "Expected `toString` method to be `fun toString() > str` got {s}",
                        .{
                            tostring_def_str,
                        },
                    );
                }
            }

            _ = try self.generateNode(member.method_or_default_value.?, breaks);
            try self.OP_PROPERTY(location, @intCast(member_idx));
        } else {
            // Property
            const property_field = object_def.fields.get(member_name).?;
            const property_type = property_field.type_def;
            const property_idx = property_field.index;

            // Create property default value
            if (member.method_or_default_value) |default| {
                self.populateEmptyCollectionType(default, property_type);
                const default_type_def = type_defs[default].?;

                if (default_type_def.def_type == .Placeholder) {
                    self.reporter.reportPlaceholder(self.ast, default_type_def.resolved_type.?.Placeholder);
                } else if (!property_type.eql(default_type_def)) {
                    self.reporter.reportTypeCheck(
                        .property_default_value,
                        object_def.location,
                        object_def.location,
                        property_type,
                        self.ast.tokens.get(locations[default]),
                        self.ast.tokens.get(end_locations[default]),
                        default_type_def,
                        "Wrong property default value type",
                    );
                }

                if (property_field.static) {
                    try self.OP_COPY(location, 0);
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
    const end_locations = self.ast.nodes.items(.end_location);
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

    if (node_type_def.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, node_type_def.resolved_type.?.Placeholder);

        return null;
    } else if (node_type_def.def_type != .ObjectInstance and node_type_def.def_type != .ForeignContainer) {
        self.reporter.reportErrorAt(
            .expected_object,
            self.ast.tokens.get(location),
            self.ast.tokens.get(end_locations[node]),
            "Expected object or foreign struct.",
        );

        return null;
    }

    try self.emitOpCode(
        location,
        if (node_type_def.def_type == .ObjectInstance)
            .OP_INSTANCE
        else
            .OP_FCONTAINER_INSTANCE,
    );

    var fields = if (node_type_def.def_type == .ObjectInstance) inst: {
        const fields = node_type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object.fields;
        var fields_type_defs = std.StringArrayHashMap(*obj.ObjTypeDef).init(self.gc.allocator);
        var it = fields.iterator();
        while (it.next()) |kv| {
            try fields_type_defs.put(
                kv.value_ptr.*.name,
                kv.value_ptr.*.type_def,
            );
        }
        break :inst fields_type_defs;
    } else node_type_def.resolved_type.?.ForeignContainer.buzz_type;

    defer if (node_type_def.def_type == .ObjectInstance) {
        fields.deinit();
    };

    const object_location = if (node_type_def.def_type == .ObjectInstance)
        node_type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object.location
    else
        node_type_def.resolved_type.?.ForeignContainer.location;

    // Keep track of what's been initialized or not by this statement
    var init_properties = std.StringHashMap(void).init(self.gc.allocator);
    defer init_properties.deinit();

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

        if (fields.get(property_name)) |prop| {
            try self.OP_COPY(location, 0); // Will be popped by OP_SET_PROPERTY

            self.populateEmptyCollectionType(property.value, prop);
            const value_type_def = type_defs[property.value].?;

            if (value_type_def.def_type == .Placeholder) {
                self.reporter.reportPlaceholder(self.ast, value_type_def.resolved_type.?.Placeholder);
            } else if (!prop.eql(value_type_def)) {
                if (BuildOptions.debug_placeholders) {
                    io.print(
                        "prop {}({}), value {}({})\n",
                        .{
                            @intFromPtr(prop.resolved_type.?.ObjectInstance.of),
                            prop.optional,
                            @intFromPtr(value_type_def.resolved_type.?.ObjectInstance.of),
                            value_type_def.optional,
                        },
                    );
                }

                const err_location = if (node_type_def.def_type == .ObjectInstance)
                    node_type_def.resolved_type.?.ObjectInstance.of
                        .resolved_type.?.Object.fields.get(property_name).?.location
                else
                    object_location;
                self.reporter.reportTypeCheck(
                    .property_type,
                    err_location,
                    err_location,
                    prop,
                    self.ast.tokens.get(locations[property.value]),
                    self.ast.tokens.get(end_locations[property.value]),
                    value_type_def,
                    "Wrong property type",
                );
            }

            _ = try self.generateNode(property.value, breaks);

            try init_properties.put(property_name, {});

            try self.emitCodeArg(
                location,
                if (node_type_def.def_type == .ObjectInstance)
                    .OP_SET_INSTANCE_PROPERTY
                else
                    .OP_SET_FCONTAINER_INSTANCE_PROPERTY,
                @intCast(property_idx.?),
            );
            try self.OP_POP(location); // Pop property value
        } else {
            self.reporter.reportWithOrigin(
                .property_does_not_exists,
                self.ast.tokens.get(location),
                self.ast.tokens.get(end_locations[node]),
                object_location,
                object_location,
                "Property `{s}` does not exists",
                .{property_name},
                null,
            );
        }
    }

    // Did we initialized all properties without a default value?
    // If union we're statisfied with only on field initialized
    if (node_type_def.def_type != .ForeignContainer or node_type_def.resolved_type.?.ForeignContainer.zig_type != .Union or init_properties.count() == 0) {
        const field_defs = if (node_type_def.def_type == .ObjectInstance)
            node_type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object.fields
        else
            null;

        var it = fields.iterator();
        while (it.next()) |kv| {
            const field = if (field_defs) |fd| fd.get(kv.key_ptr.*) else null;
            // If ommitted in initialization and doesn't have default value
            if (init_properties.get(kv.key_ptr.*) == null and
                (field == null or (!field.?.has_default and !field.?.method and !field.?.static)))
            {
                self.reporter.reportErrorFmt(
                    .property_not_initialized,
                    self.ast.tokens.get(location),
                    self.ast.tokens.get(end_locations[node]),
                    "Property `{s}` was not initialized and has no default value",
                    .{kv.key_ptr.*},
                );
            }
        }
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generatePattern(self: *Self, node: Ast.Node.Index, _: ?*Breaks) Error!?*obj.ObjFunction {
    try self.emitConstant(
        self.ast.nodes.items(.location)[node],
        try self.ast.toValue(node, self.gc),
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
        self.parser.ast.tokens.items(.lexeme)[components.identifier.?],
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateRange(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const type_defs = self.ast.nodes.items(.type_def);
    const components = self.ast.nodes.items(.components)[node].Range;
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);

    // Type checking
    if (type_defs[components.low].?.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, type_defs[components.low].?.resolved_type.?.Placeholder);
    }

    if (type_defs[components.high].?.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, type_defs[components.high].?.resolved_type.?.Placeholder);
    }

    if (type_defs[components.low].?.def_type != .Integer) {
        self.reporter.reportTypeCheck(
            .range_type,
            null,
            null,
            self.gc.type_registry.int_type,
            self.ast.tokens.get(locations[components.low]),
            self.ast.tokens.get(end_locations[components.low]),
            type_defs[components.low].?,
            "Bad low range limit type",
        );
    }

    if (type_defs[components.high].?.def_type != .Integer) {
        self.reporter.reportTypeCheck(
            .range_type,
            null,
            null,
            self.gc.type_registry.int_type,
            self.ast.tokens.get(locations[components.high]),
            self.ast.tokens.get(end_locations[components.high]),
            type_defs[components.high].?,
            "Bad high range limit type",
        );
    }

    _ = try self.generateNode(components.low, breaks);
    _ = try self.generateNode(components.high, breaks);

    try self.OP_RANGE(locations[node]);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateResolve(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const fiber = self.ast.nodes.items(.components)[node].Resolve;
    const fiber_type_def = self.ast.nodes.items(.type_def)[fiber].?;
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);

    if (fiber_type_def.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, fiber_type_def.resolved_type.?.Placeholder);

        return null;
    }

    if (fiber_type_def.def_type != .Fiber) {
        self.reporter.reportErrorAt(
            .fiber,
            self.ast.tokens.get(locations[fiber]),
            self.ast.tokens.get(end_locations[fiber]),
            "Not a fiber",
        );
    }

    _ = try self.generateNode(fiber, breaks);

    try self.OP_RESOLVE(locations[node]);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateResume(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const fiber = self.ast.nodes.items(.components)[node].Resume;
    const fiber_type_def = self.ast.nodes.items(.type_def)[fiber].?;
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);

    if (fiber_type_def.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, fiber_type_def.resolved_type.?.Placeholder);

        return null;
    }

    if (fiber_type_def.def_type != .Fiber) {
        self.reporter.reportErrorAt(
            .fiber,
            self.ast.tokens.get(locations[fiber]),
            self.ast.tokens.get(end_locations[fiber]),
            "Not a fiber",
        );
    }

    _ = try self.generateNode(fiber, breaks);

    try self.OP_RESUME(locations[node]);

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateReturn(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const components = self.ast.nodes.items(.components)[node].Return;
    const type_defs = self.ast.nodes.items(.type_def);
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);

    if (components.unconditional) {
        self.current.?.return_emitted = true;
    }

    if (components.value) |value| {
        const value_type_def = type_defs[value];
        if (value_type_def == null) {
            self.reporter.reportErrorAt(
                .undefined,
                self.ast.tokens.get(locations[value]),
                self.ast.tokens.get(end_locations[value]),
                "Unknown type.",
            );
        } else if (value_type_def.?.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, value_type_def.?.resolved_type.?.Placeholder);
        } else if (!self.current.?.function.?.type_def.resolved_type.?.Function.return_type.eql(value_type_def.?)) {
            self.reporter.reportTypeCheck(
                .return_type,
                self.ast.tokens.get(locations[self.current.?.function_node]),
                self.ast.tokens.get(end_locations[self.current.?.function_node]),
                self.current.?.function.?.type_def.resolved_type.?.Function.return_type,
                self.ast.tokens.get(locations[value]),
                self.ast.tokens.get(end_locations[value]),
                value_type_def.?,
                "Return value",
            );
        }

        _ = try self.generateNode(value, breaks);
    } else {
        if (self.current.?.function.?.type_def.resolved_type.?.Function.return_type.def_type != .Void) {
            self.reporter.reportTypeCheck(
                .return_type,
                self.ast.tokens.get(locations[self.current.?.function_node]),
                self.ast.tokens.get(end_locations[self.current.?.function_node]),
                self.current.?.function.?.type_def.resolved_type.?.Function.return_type,
                self.ast.tokens.get(locations[node]),
                self.ast.tokens.get(end_locations[node]),
                self.gc.type_registry.void_type,
                "Return value",
            );
        }

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

        if (element_type_def.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, element_type_def.resolved_type.?.Placeholder);

            continue;
        }

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
        self.ast.nodes.items(.components)[node].StringLiteral.toValue(),
    );

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateSubscript(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);
    const location = locations[node];
    const type_defs = self.ast.nodes.items(.type_def);
    const components = self.ast.nodes.items(.components)[node].Subscript;

    _ = try self.generateNode(components.subscripted, breaks);

    const subscripted_type_def = type_defs[components.subscripted].?;
    const index_type_def = type_defs[components.index].?;
    const value_type_def = if (components.value) |value| type_defs[value] else null;

    if (subscripted_type_def.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, subscripted_type_def.resolved_type.?.Placeholder);
    }

    if (index_type_def.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, index_type_def.resolved_type.?.Placeholder);
    }

    if (components.value != null and value_type_def.?.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, value_type_def.?.resolved_type.?.Placeholder);
    }

    var get_code: Chunk.OpCode = .OP_GET_LIST_SUBSCRIPT;
    var set_code: Chunk.OpCode = .OP_SET_LIST_SUBSCRIPT;
    switch (subscripted_type_def.def_type) {
        .String => {
            if (index_type_def.def_type != .Integer) {
                self.reporter.reportErrorAt(
                    .subscript_key_type,
                    self.ast.tokens.get(locations[components.index]),
                    self.ast.tokens.get(end_locations[components.index]),
                    "Expected `int` index.",
                );
            }

            get_code = .OP_GET_STRING_SUBSCRIPT;

            std.debug.assert(components.value == null);
        },
        .List => {
            if (index_type_def.def_type != .Integer) {
                self.reporter.reportErrorAt(
                    .subscript_key_type,
                    self.ast.tokens.get(locations[components.index]),
                    self.ast.tokens.get(end_locations[components.index]),
                    "Expected `int` index.",
                );
            }

            if (components.value) |value| {
                if (!type_defs[components.subscripted].?.isMutable()) {
                    const callee_type_str = type_defs[components.subscripted].?.toStringAlloc(self.gc.allocator) catch unreachable;
                    defer self.gc.allocator.free(callee_type_str);
                    self.reporter.reportErrorFmt(
                        .not_mutable,
                        self.ast.tokens.get(locations[components.subscripted]),
                        self.ast.tokens.get(end_locations[components.subscripted]),
                        "`{s}` not mutable",
                        .{
                            callee_type_str,
                        },
                    );
                }

                if (!subscripted_type_def.resolved_type.?.List.item_type.eql(value_type_def.?)) {
                    self.reporter.reportTypeCheck(
                        .subscript_value_type,
                        self.ast.tokens.get(locations[components.subscripted]),
                        self.ast.tokens.get(end_locations[components.subscripted]),
                        subscripted_type_def.resolved_type.?.List.item_type,
                        self.ast.tokens.get(locations[value]),
                        self.ast.tokens.get(end_locations[value]),
                        value_type_def.?,
                        "Bad value type",
                    );
                }
            }
        },
        .Map => {
            if (!subscripted_type_def.resolved_type.?.Map.key_type.eql(index_type_def)) {
                self.reporter.reportTypeCheck(
                    .subscript_key_type,
                    self.ast.tokens.get(locations[components.subscripted]),
                    self.ast.tokens.get(end_locations[components.subscripted]),
                    subscripted_type_def.resolved_type.?.Map.key_type,
                    self.ast.tokens.get(locations[components.index]),
                    self.ast.tokens.get(end_locations[components.index]),
                    index_type_def,
                    "Bad key type",
                );
            }

            if (components.value) |value| {
                if (!type_defs[components.subscripted].?.isMutable()) {
                    const callee_type_str = type_defs[components.subscripted].?.toStringAlloc(self.gc.allocator) catch unreachable;
                    defer self.gc.allocator.free(callee_type_str);
                    self.reporter.reportErrorFmt(
                        .not_mutable,
                        self.ast.tokens.get(locations[components.subscripted]),
                        self.ast.tokens.get(end_locations[components.subscripted]),
                        "`{s}` not mutable",
                        .{
                            callee_type_str,
                        },
                    );
                }

                if (!subscripted_type_def.resolved_type.?.Map.value_type.eql(value_type_def.?)) {
                    self.reporter.reportTypeCheck(
                        .subscript_value_type,
                        self.ast.tokens.get(locations[components.subscripted]),
                        self.ast.tokens.get(end_locations[components.subscripted]),
                        subscripted_type_def.resolved_type.?.Map.value_type,
                        self.ast.tokens.get(locations[value]),
                        self.ast.tokens.get(end_locations[value]),
                        value_type_def.?,
                        "Bad value type",
                    );
                }
            }

            get_code = .OP_GET_MAP_SUBSCRIPT;
            set_code = .OP_SET_MAP_SUBSCRIPT;
        },
        else => self.reporter.reportErrorAt(
            .subscriptable,
            self.ast.tokens.get(location),
            self.ast.tokens.get(end_locations[node]),
            "Not subscriptable.",
        ),
    }

    _ = try self.generateNode(components.index, breaks);

    if (components.value) |value| {
        _ = try self.generateNode(value, breaks);

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

    var exit_jumps = std.ArrayList(usize).init(self.gc.allocator);
    defer exit_jumps.deinit();

    self.patchTryOrJit(try_jump);
    var has_unconditional = components.unconditional_clause != null;
    for (components.clauses) |clause| {
        const error_type = type_defs[clause.type_def].?;

        if (error_type.eql(self.gc.type_registry.any_type)) {
            has_unconditional = true;
        }

        // We assume the error is on top of the stack
        try self.OP_COPY(clause.identifier, 0); // Copy error value since its argument to the catch clause
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
        try exit_jumps.append(try self.emitJump(location, .OP_JUMP));

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

        try exit_jumps.append(try self.emitJump(location, .OP_JUMP));
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
                const err_str = try kv.key_ptr.*.toStringAlloc(self.gc.allocator);
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
    if (expression_type_def.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, expression_type_def.resolved_type.?.Placeholder);
    } else {
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
                const error_str = try type_defs[components.expression].?.toStringAlloc(self.gc.allocator);
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
    const end_locations = self.ast.nodes.items(.end_location);
    const expression_location = self.ast.nodes.items(.location)[components.expression];
    const expression_type_def = self.ast.nodes.items(.type_def)[components.expression].?;

    if (expression_type_def.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, expression_type_def.resolved_type.?.Placeholder);

        return null;
    }

    _ = try self.generateNode(components.expression, breaks);

    switch (components.operator) {
        .Bnot => {
            if (expression_type_def.def_type != .Integer) {
                self.reporter.reportErrorFmt(
                    .bitwise_operand_type,
                    self.ast.tokens.get(expression_location),
                    self.ast.tokens.get(end_locations[components.expression]),
                    "Expected type `int`, got `{s}`",
                    .{
                        try expression_type_def.toStringAlloc(self.gc.allocator),
                    },
                );
            }

            try self.OP_BNOT(location);
        },
        .Bang => {
            if (expression_type_def.def_type != .Bool) {
                self.reporter.reportErrorFmt(
                    .bitwise_operand_type,
                    self.ast.tokens.get(expression_location),
                    self.ast.tokens.get(end_locations[components.expression]),
                    "Expected type `bool`, got `{s}`",
                    .{
                        try expression_type_def.toStringAlloc(self.gc.allocator),
                    },
                );
            }

            try self.OP_NOT(location);
        },
        .Minus => {
            if (expression_type_def.def_type == .Integer) {
                try self.OP_NEGATE_I(location);
            } else if (expression_type_def.def_type == .Double) {
                try self.OP_NEGATE_F(location);
            } else {
                self.reporter.reportErrorFmt(
                    .arithmetic_operand_type,
                    self.ast.tokens.get(expression_location),
                    self.ast.tokens.get(end_locations[components.expression]),
                    "Expected type `int` or `double`, got `{s}`",
                    .{
                        try expression_type_def.toStringAlloc(self.gc.allocator),
                    },
                );
            }
        },
        else => unreachable,
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

fn generateUnwrap(self: *Self, node: Ast.Node.Index, breaks: ?*Breaks) Error!?*obj.ObjFunction {
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);
    const location = locations[node];
    const components = self.ast.nodes.items(.components)[node].Unwrap;

    if (components.original_type.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, components.original_type.resolved_type.?.Placeholder);

        return null;
    }

    if (!components.original_type.optional) {
        self.reporter.reportErrorAt(
            .optional,
            self.ast.tokens.get(locations[components.unwrapped]),
            self.ast.tokens.get(end_locations[components.unwrapped]),
            "Not an optional",
        );
    }

    _ = try self.generateNode(components.unwrapped, breaks);

    try self.OP_COPY(location, 0);
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
    const type_defs = self.ast.nodes.items(.type_def);
    const type_def = type_defs[node].?;
    const value_type_def = if (components.value) |value|
        self.ast.nodes.items(.type_def)[value]
    else
        null;
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);
    const location = locations[node];

    if (components.value) |value| {
        _ = try self.generateNode(value, breaks);

        if (value_type_def.?.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, value_type_def.?.resolved_type.?.Placeholder);
        } else if (type_def.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, type_def.resolved_type.?.Placeholder);
        } else if (!(try type_def.toInstance(&self.gc.type_registry, type_def.isMutable())).eql(value_type_def.?) and
            !(try (try type_def.toInstance(&self.gc.type_registry, type_def.isMutable())).cloneNonOptional(&self.gc.type_registry)).eql(value_type_def.?))
        {
            self.reporter.reportTypeCheck(
                .assignment_value_type,
                self.ast.tokens.get(location),
                self.ast.tokens.get(end_locations[node]),
                try type_def.toInstance(&self.gc.type_registry, type_def.isMutable()),
                self.ast.tokens.get(locations[value]),
                self.ast.tokens.get(end_locations[value]),
                value_type_def.?,
                "Wrong variable type",
            );
        }
    } else {
        try self.OP_NULL(location);
    }

    if (components.slot_type == .Global) {
        try self.OP_DEFINE_GLOBAL(
            location,
            @intCast(components.slot),
            self.parser.ast.tokens.items(.lexeme)[components.identifier.?],
        );
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
    const type_defs = self.ast.nodes.items(.type_def);
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);
    const location = locations[node];
    const condition_type_def = type_defs[components.condition].?;

    // If condition constant and false, skip the node
    if (try self.ast.isConstant(self.gc.allocator, components.condition) and !(try self.ast.toValue(components.condition, self.gc)).boolean()) {
        try self.patchOptJumps(node);
        try self.endScope(node);

        return null;
    }

    const loop_start: usize = self.currentCode();

    const jit_jump = if (!is_wasm) try self.emitJump(locations[node], .OP_HOTSPOT) else {};
    if (!is_wasm) try self.emit(locations[node], node);

    if (condition_type_def.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, condition_type_def.resolved_type.?.Placeholder);
    }

    if (condition_type_def.def_type != .Bool) {
        self.reporter.reportErrorAt(
            .while_condition_type,
            self.ast.tokens.get(locations[components.condition]),
            self.ast.tokens.get(end_locations[components.condition]),
            "`while` condition must be bool",
        );
    }

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
    const type_defs = self.ast.nodes.items(.type_def);
    const type_def = type_defs[node];
    const locations = self.ast.nodes.items(.location);
    const end_locations = self.ast.nodes.items(.end_location);
    const location = locations[node];

    const current_function_typedef = type_defs[self.current.?.function_node].?.resolved_type.?.Function;
    const current_function_type = current_function_typedef.function_type;
    switch (current_function_type) {
        .Script,
        .ScriptEntryPoint,
        .Repl,
        .EntryPoint,
        .Test,
        .Extern,
        => self.reporter.reportErrorAt(
            .yield_not_allowed,
            self.ast.tokens.get(location),
            self.ast.tokens.get(end_locations[node]),
            "Can't yield here",
        ),
        else => {},
    }

    if (type_def == null) {
        self.reporter.reportErrorAt(
            .unknown,
            self.ast.tokens.get(location),
            self.ast.tokens.get(end_locations[node]),
            "Unknown type.",
        );
    } else if (type_def.?.def_type == .Placeholder) {
        self.reporter.reportPlaceholder(self.ast, type_def.?.resolved_type.?.Placeholder);
    } else if (!self.current.?.function.?.type_def.resolved_type.?.Function.yield_type.eql(type_def.?)) {
        self.reporter.reportTypeCheck(
            .yield_type,
            self.ast.tokens.get(locations[self.current.?.function_node]),
            self.ast.tokens.get(end_locations[self.current.?.function_node]),
            self.current.?.function.?.type_def.resolved_type.?.Function.yield_type,
            self.ast.tokens.get(location),
            self.ast.tokens.get(end_locations[node]),
            type_def.?,
            "Bad yield value",
        );
    }

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
            else => unreachable,
        }
        try self.OP_DEFINE_GLOBAL(
            location,
            @intCast(element.slot),
            self.parser.ast.tokens.items(.lexeme)[element.identifier.?],
        );
    }

    try self.patchOptJumps(node);
    try self.endScope(node);

    return null;
}

pub fn populateEmptyCollectionType(self: *Self, value: Ast.Node.Index, target_type: *obj.ObjTypeDef) void {
    const tags = self.ast.nodes.items(.tag);
    const components = self.ast.nodes.items(.components);

    // variable: [T] = [<any>] -> variable: [T] = [<T>];
    if (target_type.def_type == .List and
        tags[value] == .List and
        components[value].List.explicit_item_type == null and
        components[value].List.items.len == 0)
    {
        self.ast.nodes.items(.type_def)[value] = target_type;
    }

    // variable: {K: V} = {<any: any>} -> variable: {K: V} = [<K: V>];
    if (target_type.def_type == .Map and
        tags[value] == .Map and
        components[value].Map.explicit_key_type == null and
        components[value].Map.explicit_value_type == null and
        components[value].Map.entries.len == 0)
    {
        self.ast.nodes.items(.type_def)[value] = target_type;
    }
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

fn OP_DEFINE_GLOBAL(self: *Self, location: Ast.TokenIndex, slot: u24, name: []const u8) !void {
    try self.emitCodeArg(
        location,
        .OP_CONSTANT,
        try self.identifierConstant(name),
    );
    try self.emitCodeArg(
        location,
        .OP_DEFINE_GLOBAL,
        slot,
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

fn OP_COPY(self: *Self, location: Ast.TokenIndex, dist: u24) !void {
    try self.emitCodeArg(
        location,
        .OP_COPY,
        dist,
    );
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
