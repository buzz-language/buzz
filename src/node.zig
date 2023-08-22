const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const _obj = @import("./obj.zig");
const _token = @import("./token.zig");
const _value = @import("./value.zig");
const _codegen = @import("./codegen.zig");
const _parser = @import("./parser.zig");
const _chunk = @import("./chunk.zig");
const disassembler = @import("./disassembler.zig");
const BuildOptions = @import("build_options");
const VM = @import("./vm.zig").VM;
const GarbageCollector = @import("./memory.zig").GarbageCollector;
const global_allocator = @import("./buzz_api.zig").allocator;
const ZigType = @import("./zigtypes.zig").Type;
const FFI = @import("./ffi.zig");

const disassembleChunk = disassembler.disassembleChunk;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjString = _obj.ObjString;
const ObjNative = _obj.ObjNative;
const ObjFunction = _obj.ObjFunction;
const ObjObject = _obj.ObjObject;
const ObjList = _obj.ObjList;
const ObjEnum = _obj.ObjEnum;
const ObjPattern = _obj.ObjPattern;
const ObjMap = _obj.ObjMap;
const ObjBoundMethod = _obj.ObjBoundMethod;
const FunctionType = ObjFunction.FunctionType;
const copyObj = _obj.copyObj;
const Value = _value.Value;
const valueToString = _value.valueToString;
const floatToInteger = _value.floatToInteger;
const Token = _token.Token;
const TokenType = _token.TokenType;
const CodeGen = _codegen.CodeGen;
const Parser = _parser.Parser;
const Frame = _codegen.Frame;
const Local = _parser.Local;
const Global = _parser.Global;
const UpValue = _parser.UpValue;
const OpCode = _chunk.OpCode;

pub const GenError = error{NotConstant};

pub const ParsedArg = struct {
    name: ?Token,
    arg: *ParseNode,
};

pub const ParseNodeType = enum(u8) {
    Function,
    Enum,
    VarDeclaration,
    FunDeclaration,
    ObjectDeclaration,
    ProtocolDeclaration,
    Binary,
    Unary,
    Subscript,
    Unwrap,
    ForceUnwrap,
    Is,
    As,
    Expression,
    Grouping,
    NamedVariable,
    Integer,
    Float,
    String,
    StringLiteral,
    Pattern,
    Boolean,
    Null,
    Void,
    List,
    Map,
    Dot,
    ObjectInit,
    Throw,
    Break,
    Continue,
    Call,
    AsyncCall,
    Resume,
    Resolve,
    Yield,
    If,
    InlineIf,
    Block,
    Return,
    For,
    ForEach,
    DoUntil,
    While,
    Export,
    Import,
    Try,
    Range,
    Zdef,
    TypeExpression,
    TypeOfExpression,
};

pub const RenderError = Allocator.Error || std.fmt.BufPrintError;

pub const ParseNode = struct {
    const Self = @This();

    node_type: ParseNodeType,
    // If null, either its a statement or its a reference to something unknown that should ultimately raise a compile error
    type_def: ?*ObjTypeDef = null,
    location: Token = undefined,
    end_location: Token = undefined,
    // Wether optional jumps must be patch before generate this node bytecode
    patch_opt_jumps: bool = false,
    docblock: ?Token = null,

    // Does this node closes a scope
    ends_scope: ?std.ArrayList(OpCode) = null,

    // Because of https://github.com/ziglang/zig/issues/12325 we can't reference ParseNode in any of those signatures
    toJson: *const fn (*anyopaque, *const std.ArrayList(u8).Writer) RenderError!void = stringify,
    toByteCode: *const fn (*anyopaque, *anyopaque, ?*std.ArrayList(usize)) anyerror!?*ObjFunction,
    toValue: *const fn (*anyopaque, *GarbageCollector) anyerror!Value,
    render: *const fn (*anyopaque, *const std.ArrayList(u8).Writer, usize) RenderError!void,
    isConstant: *const fn (*anyopaque) bool,

    // If returns true, node must be skipped
    pub fn synchronize(self: *Self, codegen: *CodeGen) bool {
        if (codegen.reporter.panic_mode) {
            switch (self.node_type) {
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
                    codegen.reporter.panic_mode = false;
                    return false;
                },
                else => {},
            }

            return true;
        }

        return false;
    }

    fn patchOptJumps(self: *Self, codegen: *CodeGen) !void {
        if (self.patch_opt_jumps) {
            assert(codegen.opt_jumps != null);

            // Hope over OP_POP if actual value
            const njump: usize = try codegen.emitJump(self.location, .OP_JUMP);

            for (codegen.opt_jumps.?.items) |jump| {
                codegen.patchJump(jump);
            }
            // If aborted by a null optional, will result in null on the stack
            try codegen.emitOpCode(self.location, .OP_POP);

            codegen.patchJump(njump);

            codegen.opt_jumps.?.deinit();
            codegen.opt_jumps = null;
        }
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = @as(*Self, @ptrCast(node));

        try out.writeAll("\"type_def\": \"");
        if (self.type_def) |type_def| {
            try type_def.toString(out);
        } else {
            try out.writeAll("N/A");
        }

        // TODO: assumes a token is on one line, right now it's completely true except for a .Docblock token
        const end_col: usize = if (self.location.eql(self.end_location)) self.location.column + self.location.lexeme.len else self.end_location.column;

        try out.print(
            "\", \"location\": {{ \"start_line\": {}, \"start_column\": {}, \"end_line\": {}, \"end_column\": {}, \"script\": \"{s}\"}}",
            .{
                self.location.line,
                self.location.column,
                self.end_location.line,
                end_col,
                self.location.script_name,
            },
        );

        if (self.docblock != null) {
            var escaped = try escape(global_allocator, self.docblock.?.literal_string.?);
            defer escaped.deinit();
            try out.print(", \"docblock\": \"{s}\"", .{escaped.items});
        }
    }

    fn endScope(self: *Self, codegen: *CodeGen) anyerror!void {
        if (self.ends_scope) |closing| {
            for (closing.items) |op| {
                try codegen.emitOpCode(self.location, op);
            }
        }
    }

    pub fn deinit(self: *Self) void {
        if (self.ends_scope) {
            self.ends_scope.?.deinit();
        }
    }
};

pub const ExpressionNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Expression,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    expression: *ParseNode,

    fn constant(nodePtr: *anyopaque) bool {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        return self.expression.isConstant(self.expression);
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.isConstant(node)) {
            const self = Self.cast(node).?;

            return self.expression.toValue(self.expression, gc);
        }

        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        const self = Self.cast(node).?;

        _ = try self.expression.toByteCode(self.expression, codegen, breaks);

        try codegen.emitOpCode(node.location, .OP_POP);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.print("{{\"node\": \"Expression\", ", .{});

        try ParseNode.stringify(node, out);

        try out.writeAll(",\"expression\": ");

        try self.expression.toJson(self.expression, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        // Its a statement, should be on its own line
        try out.writeByteNTimes(' ', depth * 4);
        try self.expression.render(self.expression, out, depth);
        try out.writeAll(";\n");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Expression) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const GroupingNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Grouping,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    expression: *ParseNode,

    fn constant(nodePtr: *anyopaque) bool {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        return self.expression.isConstant(self.expression);
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.isConstant(node)) {
            const self = Self.cast(node).?;

            return self.expression.toValue(self.expression, gc);
        }

        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        const self = Self.cast(node).?;

        _ = try self.expression.toByteCode(self.expression, codegen, breaks);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.print("{{\"node\": \"Grouping\", ", .{});

        try ParseNode.stringify(node, out);

        try out.writeAll(",\"expression\": ");

        try self.expression.toJson(self.expression, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        // Its a statement, should be on its own line
        try out.writeAll("(");
        try self.expression.render(self.expression, out, depth);
        try out.writeAll(")");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Grouping) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const SlotType = enum(u8) {
    Local,
    UpValue,
    Global,
};

pub const NamedVariableNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .NamedVariable,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    identifier: Token,
    value: ?*ParseNode = null,
    slot: usize,
    slot_type: SlotType,
    slot_constant: bool,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        var get_op: OpCode = undefined;
        var set_op: OpCode = undefined;

        switch (self.slot_type) {
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

        if (self.value) |value| {
            // Type checking
            if (node.type_def.?.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(node.type_def.?.resolved_type.?.Placeholder);
            }

            if (!node.type_def.?.eql(value.type_def.?)) {
                codegen.reporter.reportTypeCheck(
                    .assignment_value_type,
                    node.location,
                    node.type_def.?,
                    value.location,
                    value.type_def.?,
                    "Bad value type",
                );
            }

            _ = try value.toByteCode(value, codegen, breaks);

            try codegen.emitCodeArg(self.node.location, set_op, @intCast(self.slot));
        } else {
            try codegen.emitCodeArg(self.node.location, get_op, @intCast(self.slot));
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print(
            "{{\"node\": \"NamedVariable\", \"identifier\": \"{s}\", \"slot\": \"{}\", \"slot_type\": \"{}\",",
            .{
                if (self.identifier.literal_string) |literal| literal else "unknown",
                self.slot,
                self.slot_type,
            },
        );

        try ParseNode.stringify(node, out);

        try out.writeAll(",\"value\": ");

        if (self.value) |value| {
            try value.toJson(value, out);
        } else {
            try out.writeAll("null");
        }

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeAll(self.identifier.lexeme);
        if (self.value) |value| {
            try out.writeAll(" = ");
            try value.render(value, out, depth);
        }
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .NamedVariable) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const IntegerNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Integer,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = cnst,
        .render = render,
    },

    integer_constant: i32,

    fn cnst(_: *anyopaque) bool {
        return true;
    }

    fn val(nodePtr: *anyopaque, _: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        return Value.fromInteger(self.integer_constant);
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        try codegen.emitConstant(self.node.location, Value.fromInteger(self.integer_constant));

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Integer\", \"constant\": ", .{});

        try out.print("{d}, ", .{self.integer_constant});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, _: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.print("{d}", .{self.integer_constant});
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Integer) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const RangeNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Range,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = cnst,
        .render = render,
    },

    low: *ParseNode,
    hi: *ParseNode,

    fn cnst(_: *anyopaque) bool {
        // const node: *ParseNode = @ptrCast( @alignCast( nodePtr));
        // const self = Self.cast(node).?;

        // return self.low.isConstant(self.low) and self.hi.isConstant(self.hi);

        // If true, we'd still need to clone the value
        return false;
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        var list = try gc.allocateObject(
            ObjList,
            _obj.ObjList.init(
                gc.allocator,
                try gc.type_registry.getTypeDef(.{
                    .def_type = .Integer,
                }),
            ),
        );

        const low = (try self.low.toValue(self.low, gc)).integer();
        const hi = (try self.hi.toValue(self.hi, gc)).integer();
        const delta: i32 = if (low < hi) 1 else -1;

        var i = low;
        while ((low < hi and i < hi) or (low >= hi and i > hi)) : (i += delta) {
            try list.items.append(Value.fromInteger(i));
        }

        return list.toValue();
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        const codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        if (node.synchronize(codegen)) {
            return null;
        }

        // Type checking
        if (self.low.type_def == null or self.low.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.low.type_def.?.resolved_type.?.Placeholder);
        }

        if (self.hi.type_def == null or self.hi.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.hi.type_def.?.resolved_type.?.Placeholder);
        }

        if (self.low.type_def.?.def_type != .Integer) {
            codegen.reporter.reportTypeCheck(
                .range_type,
                null,
                try codegen.gc.type_registry.getTypeDef(.{
                    .def_type = .Integer,
                }),
                self.low.location,
                self.low.type_def.?,
                "Bad low range limit type",
            );
        }

        if (self.hi.type_def.?.def_type != .Integer) {
            codegen.reporter.reportTypeCheck(
                .range_type,
                null,
                try codegen.gc.type_registry.getTypeDef(.{
                    .def_type = .Integer,
                }),
                self.hi.location,
                self.hi.type_def.?,
                "Bad high range limit type",
            );
        }

        _ = try self.low.toByteCode(self.low, codegen, breaks);
        _ = try self.hi.toByteCode(self.hi, codegen, breaks);

        try codegen.emitOpCode(node.location, .OP_RANGE);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Range\", \"low\": ", .{});
        try self.low.toJson(self.low, out);
        try out.print(", \"high\": ", .{});
        try self.hi.toJson(self.hi, out);
        try out.print(", ", .{});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, inc: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try self.low.render(self.low, out, inc);
        try out.print("..", .{});
        try self.hi.render(self.hi, out, inc);
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Range) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const FloatNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Float,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = cnst,
        .render = render,
    },

    float_constant: f64,

    fn cnst(_: *anyopaque) bool {
        return true;
    }

    fn val(nodePtr: *anyopaque, _: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        return Value.fromFloat(self.float_constant);
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        try codegen.emitConstant(self.node.location, Value.fromFloat(self.float_constant));

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Float\", \"constant\": ", .{});

        try out.print("{d}, ", .{self.float_constant});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, _: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.print("{d}", .{self.float_constant});
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Float) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const BooleanNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Boolean,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = cnts,
        .render = render,
    },

    constant: bool,

    fn cnts(_: *anyopaque) bool {
        return true;
    }

    fn val(nodePtr: *anyopaque, _: *GarbageCollector) anyerror!Value {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        return Value.fromBoolean(Self.cast(node).?.constant);
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        try codegen.emitOpCode(self.node.location, if (self.constant) .OP_TRUE else .OP_FALSE);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Boolean\", \"constant\": \"{}\", ", .{self.constant});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, _: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        if (self.constant) {
            try out.writeAll("true");
        } else {
            try out.writeAll("false");
        }
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Boolean) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const StringLiteralNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .StringLiteral,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = cnst,
        .render = render,
    },

    constant: *ObjString,

    fn cnst(_: *anyopaque) bool {
        return true;
    }

    fn val(nodePtr: *anyopaque, _: *GarbageCollector) anyerror!Value {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        return Self.cast(node).?.constant.toValue();
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        try codegen.emitConstant(self.node.location, self.constant.toValue());

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        var escaped = try escape(global_allocator, self.constant.string);
        defer escaped.deinit();
        try out.print("{{\"node\": \"StringLiteral\", \"constant\": \"{s}\", ", .{escaped.items});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, _: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        const multiline = std.mem.indexOf(u8, self.constant.string, "\n") != null;

        if (multiline) {
            try out.print("`{s}`", .{self.constant.string});
        } else {
            try out.print("\"{s}\"", .{self.constant.string});
        }
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .StringLiteral) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const PatternNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Pattern,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = cnst,
        .render = render,
    },

    constant: *ObjPattern,

    fn cnst(_: *anyopaque) bool {
        return true;
    }

    fn val(nodePtr: *anyopaque, _: *GarbageCollector) anyerror!Value {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        return Self.cast(node).?.constant.toValue();
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        try codegen.emitConstant(self.node.location, self.constant.toValue());

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        var escaped = try escape(global_allocator, self.constant.source);
        defer escaped.deinit();
        try out.print("{{\"node\": \"Pattern\", \"constant\": \"{s}\", ", .{escaped.items});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, _: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.print("_{s}_", .{self.constant.source});
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Pattern) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const StringNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .String,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    // List of nodes that will eventually be converted to strings concatened together
    elements: []*ParseNode,

    fn constant(nodePtr: *anyopaque) bool {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        for (self.elements) |element| {
            if (!element.isConstant(element)) {
                return false;
            }
        }

        return true;
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.isConstant(node)) {
            const self = Self.cast(node).?;

            var list = std.ArrayList(*ObjString).init(gc.allocator);
            defer list.deinit();

            var str_value = std.ArrayList(u8).init(gc.allocator);
            var writer = &str_value.writer();
            for (self.elements) |element| {
                assert(element.isConstant(element));

                try valueToString(writer, try element.toValue(element, gc));
            }

            return (try gc.copyString(str_value.items)).toValue();
        }

        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        if (self.elements.len == 0) {
            // Push the empty string which is always the constant 0
            try codegen.emitCodeArg(self.node.location, .OP_CONSTANT, 0);

            try node.endScope(codegen);

            return null;
        }

        for (self.elements, 0..) |element, index| {
            if (element.type_def == null or element.type_def.?.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(element.type_def.?.resolved_type.?.Placeholder);

                continue;
            }

            _ = try element.toByteCode(element, codegen, breaks);
            if (element.type_def.?.def_type != .String or element.type_def.?.optional) {
                try codegen.emitOpCode(self.node.location, .OP_TO_STRING);
            }

            if (index >= 1) {
                try codegen.emitOpCode(self.node.location, .OP_ADD_STRING);
            }
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"String\", \"elements\": [");

        for (self.elements, 0..) |element, i| {
            try element.toJson(element, out);

            if (i < self.elements.len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        // A different token would be more efficient but it would complexify everything just for this case
        var multiline = false;
        for (self.elements) |element| {
            if (element.node_type == .StringLiteral and std.mem.indexOf(u8, StringLiteralNode.cast(element).?.constant.string, "\n") != null) {
                multiline = true;
                break;
            }
        }

        try out.writeAll(if (multiline) "`" else "\"");

        for (self.elements) |element| {
            if (element.node_type == .StringLiteral) {
                try out.writeAll(StringLiteralNode.cast(element).?.constant.string);
            } else {
                // Interpolation
                try out.writeAll("{");
                try element.render(element, out, depth);
                try out.writeAll("}");
            }
        }

        try out.writeAll(if (multiline) "`" else "\"");
    }

    pub fn init(allocator: Allocator) Self {
        return .{
            .elements = std.ArrayList(*ParseNode).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.elements.deinit();
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .String) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const NullNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Null,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    fn constant(_: *anyopaque) bool {
        return true;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return Value.Null;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        try codegen.emitOpCode(node.location, .OP_NULL);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        try out.writeAll("{\"node\": \"Null\", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(_: *anyopaque, out: *const std.ArrayList(u8).Writer, _: usize) RenderError!void {
        try out.writeAll("null");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Null) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const VoidNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Void,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    fn constant(_: *anyopaque) bool {
        return true;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return Value.Void;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        try codegen.emitOpCode(node.location, .OP_VOID);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        try out.writeAll("{\"node\": \"Void\", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(_: *anyopaque, out: *const std.ArrayList(u8).Writer, _: usize) RenderError!void {
        try out.writeAll("void");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Void) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ListNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .List,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    items: []*ParseNode,
    trailing_comma: bool,

    fn constant(nodePtr: *anyopaque) bool {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        for (self.items) |item| {
            // A constant list can't contain other list/maps since they won't be cloned
            if (item.node_type == .List or item.node_type == .Map or !item.isConstant(item)) {
                return false;
            }
        }

        return true;
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.isConstant(node)) {
            const self = Self.cast(node).?;

            assert(node.type_def != null and node.type_def.?.def_type != .Placeholder);

            var list = try gc.allocateObject(ObjList, _obj.ObjList.init(gc.allocator, node.type_def.?));

            for (self.items) |item| {
                try list.items.append(try item.toValue(item, gc));
            }

            return list.toValue();
        }

        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        const item_type = self.node.type_def.?.resolved_type.?.List.item_type;
        const list_offset: usize = try codegen.emitList(self.node.location);

        for (self.items) |item| {
            if (item.type_def.?.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(item.type_def.?.resolved_type.?.Placeholder);
            } else if (!item_type.eql(item.type_def.?)) {
                codegen.reporter.reportTypeCheck(
                    .list_item_type,
                    node.location,
                    item_type,
                    item.location,
                    item.type_def.?,
                    "Bad list type",
                );
            } else {
                _ = try item.toByteCode(item, codegen, breaks);

                try codegen.emitOpCode(item.location, .OP_LIST_APPEND);
            }
        }

        const list_type_constant: u24 = try codegen.makeConstant(Value.fromObj(node.type_def.?.toObj()));
        try codegen.patchList(list_offset, list_type_constant);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"List\", \"items\": [");

        for (self.items, 0..) |item, i| {
            try item.toJson(item, out);

            if (i < self.items.len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeAll("[");
        if (!self.trailing_comma) {
            try out.writeAll(" ");
        }

        if (self.items.len == 0) {
            try out.writeAll("<");
            try self.node.type_def.?.resolved_type.?.List.item_type.toStringUnqualified(out);
            try out.writeAll(">");
        }

        for (self.items, 0..) |item, i| {
            // If trailing comma, means we want all element on its own line
            if (self.trailing_comma) {
                try out.writeAll("\n");
                try out.writeByteNTimes(' ', (depth + 1) * 4);
            }

            try item.render(item, out, depth + 1);

            if (i != self.items.len - 1 or self.trailing_comma) {
                try out.writeAll(",");
                if (!self.trailing_comma) {
                    try out.writeAll(" ");
                }
            }
        }

        if (self.trailing_comma) {
            try out.writeAll("\n");
        } else {
            try out.writeAll(" ");
        }
        try out.writeAll("]");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .List) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const MapNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Map,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    keys: []*ParseNode,
    values: []*ParseNode,
    trailing_comma: bool,

    fn constant(nodePtr: *anyopaque) bool {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        for (self.keys) |key| {
            if (key.node_type == .List or key.node_type == .Map or !key.isConstant(key)) {
                return false;
            }
        }

        for (self.values) |value| {
            if (value.node_type == .List or value.node_type == .Map or !value.isConstant(value)) {
                return false;
            }
        }

        return true;
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.isConstant(node)) {
            const self = Self.cast(node).?;

            assert(node.type_def != null and node.type_def.?.def_type != .Placeholder);

            var map = try gc.allocateObject(ObjMap, _obj.ObjMap.init(gc.allocator, node.type_def.?));

            assert(self.keys.len == self.values.len);

            for (self.keys, 0..) |key, index| {
                const value = self.values[index];
                try map.map.put(
                    try key.toValue(key, gc),
                    try value.toValue(value, gc),
                );
            }

            return map.toValue();
        }

        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        const key_type = self.node.type_def.?.resolved_type.?.Map.key_type;
        const value_type = self.node.type_def.?.resolved_type.?.Map.value_type;

        const map_offset: usize = try codegen.emitMap(self.node.location);

        assert(self.keys.len == self.values.len);

        for (self.keys, 0..) |key, i| {
            const value = self.values[i];

            _ = try key.toByteCode(key, codegen, breaks);
            _ = try value.toByteCode(value, codegen, breaks);

            try codegen.emitOpCode(self.node.location, .OP_SET_MAP);

            if (key.type_def.?.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(key.type_def.?.resolved_type.?.Placeholder);
            }

            if (value.type_def.?.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(value.type_def.?.resolved_type.?.Placeholder);
            }

            if (!key_type.eql(key.type_def.?)) {
                codegen.reporter.reportTypeCheck(
                    .map_key_type,
                    node.location,
                    key_type,
                    key.location,
                    key.type_def.?,
                    "Bad key type",
                );
            }

            if (!value_type.eql(value.type_def.?)) {
                codegen.reporter.reportTypeCheck(
                    .map_value_type,
                    node.location,
                    value_type,
                    value.location,
                    value.type_def.?,
                    "Bad value type",
                );
            }
        }

        const map_type_constant: u24 = try codegen.makeConstant(Value.fromObj(node.type_def.?.toObj()));
        try codegen.patchMap(map_offset, map_type_constant);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Map\", \"items\": [");

        for (self.keys, 0..) |key, i| {
            try out.writeAll("{\"key\":");

            try key.toJson(key, out);

            try out.writeAll(", \"value\": ");

            try self.values[i].toJson(self.values[i], out);

            try out.writeAll("}");

            if (i < self.keys.len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeAll("{");
        if (!self.trailing_comma) {
            try out.writeAll(" ");
        }

        if (self.keys.len == 0) {
            try out.writeAll("<");
            try self.node.type_def.?.resolved_type.?.Map.key_type.toStringUnqualified(out);
            try out.writeAll(", ");
            try self.node.type_def.?.resolved_type.?.Map.value_type.toStringUnqualified(out);
            try out.writeAll(">");
        }

        for (self.keys, 0..) |key, i| {
            // If trailing comma, means we want all element on its own line
            if (self.trailing_comma) {
                try out.writeAll("\n");
                try out.writeByteNTimes(' ', (depth + 1) * 4);
            }

            try key.render(key, out, depth + 1);
            try out.writeAll(": ");
            try self.values[i].render(self.values[i], out, depth + 1);

            if (i != self.keys.len - 1 or self.trailing_comma) {
                try out.writeAll(",");
                if (!self.trailing_comma) {
                    try out.writeAll(" ");
                }
            }
        }

        if (self.trailing_comma) {
            try out.writeAll("\n");
        } else {
            try out.writeAll(" ");
        }
        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Map) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const UnwrapNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Unwrap,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    unwrapped: *ParseNode,
    original_type: ?*ObjTypeDef,

    fn constant(nodePtr: *anyopaque) bool {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        return self.unwrapped.isConstant(self.unwrapped);
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.isConstant(node)) {
            const self = Self.cast(node).?;

            return try self.unwrapped.toValue(self.unwrapped, gc);
        }

        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        const self = Self.cast(node).?;

        if (self.original_type == null or self.original_type.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.original_type.?.resolved_type.?.Placeholder);
        }

        if (!self.original_type.?.optional) {
            codegen.reporter.reportErrorAt(
                .optional,
                self.unwrapped.location,
                "Not an optional.",
            );
        }

        _ = try self.unwrapped.toByteCode(self.unwrapped, codegen, breaks);

        try codegen.emitOpCode(self.node.location, .OP_COPY);
        try codegen.emitOpCode(self.node.location, .OP_NULL);
        try codegen.emitOpCode(self.node.location, .OP_EQUAL);
        try codegen.emitOpCode(self.node.location, .OP_NOT);

        const jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);

        if (codegen.opt_jumps == null) {
            codegen.opt_jumps = std.ArrayList(usize).init(codegen.gc.allocator);
        }
        try codegen.opt_jumps.?.append(jump);

        try codegen.emitOpCode(self.node.location, .OP_POP); // Pop test result

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Unwrap\", \"unwrapped\": ");

        try self.unwrapped.toJson(self.unwrapped, out);
        try out.writeAll(",");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try self.unwrapped.render(self.unwrapped, out, depth);
        try out.writeAll("?");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Unwrap) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ForceUnwrapNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .ForceUnwrap,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    unwrapped: *ParseNode,
    original_type: ?*ObjTypeDef,

    fn constant(nodePtr: *anyopaque) bool {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        return self.unwrapped.isConstant(self.unwrapped);
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.isConstant(node)) {
            const self = Self.cast(node).?;

            const value = try self.unwrapped.toValue(self.unwrapped, gc);

            if (value.isNull()) {
                return VM.Error.UnwrappedNull;
            }

            return value;
        }
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        const self = Self.cast(node).?;

        if (self.original_type == null or self.original_type.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.original_type.?.resolved_type.?.Placeholder);

            return null;
        }

        if (!self.original_type.?.optional) {
            codegen.reporter.reportErrorAt(
                .optional,
                self.unwrapped.location,
                "Not an optional.",
            );
        }

        _ = try self.unwrapped.toByteCode(self.unwrapped, codegen, breaks);

        try codegen.emitOpCode(self.node.location, .OP_UNWRAP);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"ForceUnwrap\", \"unwrapped\": ");

        try self.unwrapped.toJson(self.unwrapped, out);
        try out.writeAll(",");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try self.unwrapped.render(self.unwrapped, out, depth);
        try out.writeAll("!");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .ForceUnwrap) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const IsNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Is,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = cnts,
        .render = render,
    },

    left: *ParseNode,
    constant: Value,

    fn cnts(nodePtr: *anyopaque) bool {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        return self.left.isConstant(self.left);
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.isConstant(node)) {
            const self = Self.cast(node).?;
            const left = try self.left.toValue(self.left, gc);

            return Value.fromBoolean(_value.valueIs(self.constant, left));
        }
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        assert(self.constant.isObj());
        assert(self.constant.obj().obj_type == .Type);

        if (ObjTypeDef.cast(self.constant.obj()).?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(ObjTypeDef.cast(self.constant.obj()).?.resolved_type.?.Placeholder);
        }

        _ = try self.left.toByteCode(self.left, codegen, breaks);

        try codegen.emitCodeArg(self.node.location, .OP_CONSTANT, try codegen.makeConstant(self.constant));

        try codegen.emitOpCode(self.node.location, .OP_IS);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Is\", \"left\": ");

        try self.left.toJson(self.left, out);

        try out.writeAll(", \"constant\": \"");
        try valueToString(out, self.constant);
        try out.writeAll("\", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try self.left.render(self.left, out, depth);
        try out.writeAll(" is ");
        try ObjTypeDef.cast(self.constant.obj()).?.toStringUnqualified(out);
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Is) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const AsNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .As,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = cnts,
        .render = render,
    },

    left: *ParseNode,
    constant: Value,

    fn cnts(nodePtr: *anyopaque) bool {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        return self.left.isConstant(self.left);
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.isConstant(node)) {
            const self = Self.cast(node).?;
            const left = try self.left.toValue(self.left, gc);

            if (_value.valueIs(self.constant, left)) {
                return left;
            }

            return _value.Value.Null;
        }

        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        assert(self.constant.isObj());
        assert(self.constant.obj().obj_type == .Type);

        if (ObjTypeDef.cast(self.constant.obj()).?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(ObjTypeDef.cast(self.constant.obj()).?.resolved_type.?.Placeholder);
        }

        _ = try self.left.toByteCode(self.left, codegen, breaks);

        try codegen.emitOpCode(self.left.location, .OP_COPY);
        try codegen.emitCodeArg(self.node.location, .OP_CONSTANT, try codegen.makeConstant(self.constant));
        try codegen.emitOpCode(self.node.location, .OP_IS);
        try codegen.emitOpCode(self.node.location, .OP_NOT);
        const jump = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
        try codegen.emitOpCode(self.node.location, .OP_POP);
        try codegen.emitOpCode(self.node.location, .OP_POP);
        try codegen.emitOpCode(self.node.location, .OP_NULL);
        const jump_over = try codegen.emitJump(self.node.location, .OP_JUMP);
        codegen.patchJump(jump);
        try codegen.emitOpCode(self.node.location, .OP_POP);
        codegen.patchJump(jump_over);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"AsOpt\", \"left\": ");

        try self.left.toJson(self.left, out);

        try out.writeAll(", \"constant\": \"");
        try valueToString(out, self.constant);
        try out.writeAll("\", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try self.left.render(self.left, out, depth);
        try out.writeAll(" as? ");
        try ObjTypeDef.cast(self.constant.obj()).?.toStringUnqualified(out);
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .As) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const UnaryNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Unary,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    left: *ParseNode,
    operator: TokenType,

    fn constant(nodePtr: *anyopaque) bool {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        return self.left.isConstant(self.left);
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.isConstant(node)) {
            const self = Self.cast(node).?;

            const value = try self.left.toValue(self.left, gc);

            return switch (self.operator) {
                .Bnot => Value.fromInteger(~(if (value.isInteger()) value.integer() else @as(i32, @intFromFloat(value.float())))),
                .Bang => Value.fromBoolean(!value.boolean()),
                .Minus => number: {
                    if (value.isInteger()) {
                        break :number Value.fromInteger(-value.integer());
                    } else {
                        break :number Value.fromFloat(-value.float());
                    }
                },
                else => unreachable,
            };
        }

        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        if (self.left.type_def == null or self.left.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.left.type_def.?.resolved_type.?.Placeholder);

            return null;
        }

        _ = try self.left.toByteCode(self.left, codegen, breaks);

        const left_type = self.left.type_def.?;
        switch (self.operator) {
            .Bnot => {
                if (left_type.def_type != .Integer) {
                    codegen.reporter.reportErrorFmt(
                        .bitwise_operand_type,
                        self.left.location,
                        "Expected type `int`, got `{s}`",
                        .{(try left_type.toStringAlloc(codegen.gc.allocator)).items},
                    );
                }

                try codegen.emitOpCode(self.node.location, .OP_BNOT);
            },
            .Bang => {
                if (left_type.def_type != .Bool) {
                    codegen.reporter.reportErrorFmt(
                        .bitwise_operand_type,
                        self.left.location,
                        "Expected type `bool`, got `{s}`",
                        .{(try left_type.toStringAlloc(codegen.gc.allocator)).items},
                    );
                }

                try codegen.emitOpCode(self.node.location, .OP_NOT);
            },
            .Minus => {
                if (left_type.def_type != .Integer and left_type.def_type != .Float) {
                    codegen.reporter.reportErrorFmt(
                        .arithmetic_operand_type,
                        self.left.location,
                        "Expected type `int` or `float`, got `{s}`",
                        .{(try left_type.toStringAlloc(codegen.gc.allocator)).items},
                    );
                }

                try codegen.emitOpCode(self.node.location, .OP_NEGATE);
            },
            else => unreachable,
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Unary\", \"left\": ");

        try self.left.toJson(self.left, out);
        try out.print(", \"operator\": \"{}\", ", .{self.operator});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByte(switch (self.operator) {
            .Minus => '-',
            .Bang => '!',
            .Bnot => '~',
            else => unreachable,
        });
        try self.left.render(self.left, out, depth);
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Unary) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const BinaryNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Binary,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    left: *ParseNode,
    right: *ParseNode,
    operator: TokenType,

    fn constant(nodePtr: *anyopaque) bool {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        return self.left.isConstant(self.left) and self.right.isConstant(self.right);
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.isConstant(node)) {
            const self = Self.cast(node).?;

            var left = floatToInteger(try self.left.toValue(self.left, gc));
            var right = floatToInteger(try self.right.toValue(self.right, gc));
            var left_f: ?f64 = if (left.isFloat()) left.float() else null;
            var right_f: ?f64 = if (right.isFloat()) right.float() else null;
            var left_i: ?i32 = if (left.isInteger()) left.integer() else null;
            var right_i: ?i32 = if (right.isInteger()) right.integer() else null;

            switch (self.operator) {
                .Ampersand => {
                    return Value.fromInteger((left_i orelse @as(i32, @intFromFloat(left_f.?))) & (right_i orelse @as(i32, @intFromFloat(right_f.?))));
                },
                .Bor => {
                    return Value.fromInteger((left_i orelse @as(i32, @intFromFloat(left_f.?))) | (right_i orelse @as(i32, @intFromFloat(right_f.?))));
                },
                .Xor => {
                    return Value.fromInteger((left_i orelse @as(i32, @intFromFloat(left_f.?))) ^ (right_i orelse @as(i32, @intFromFloat(right_f.?))));
                },
                .ShiftLeft => {
                    const b: i32 = right_i orelse @intFromFloat(right_f.?);

                    if (b < 0) {
                        if (b * -1 > std.math.maxInt(u6)) {
                            return Value.fromInteger(0);
                        }

                        return Value.fromInteger(
                            (left_i orelse @as(i32, @intFromFloat(left_f.?))) >> @as(u5, @truncate(@as(u64, @intCast(b * -1)))),
                        );
                    } else {
                        if (b > std.math.maxInt(u6)) {
                            return Value.fromInteger(0);
                        }

                        return Value.fromInteger(
                            (left_i orelse @as(i32, @intFromFloat(left_f.?))) << @as(u5, @truncate(@as(u64, @intCast(b)))),
                        );
                    }
                },
                .ShiftRight => {
                    const b: i32 = right_i orelse @intFromFloat(right_f.?);

                    if (b < 0) {
                        if (b * -1 > std.math.maxInt(u6)) {
                            return Value.fromInteger(0);
                        }

                        return Value.fromInteger((left_i orelse @as(i32, @intFromFloat(left_f.?))) << @as(u5, @truncate(@as(u64, @intCast(b * -1)))));
                    } else {
                        if (b > std.math.maxInt(u6)) {
                            return Value.fromInteger(0);
                        }

                        return Value.fromInteger((left_i orelse @as(i32, @intFromFloat(left_f.?))) >> @as(u5, @truncate(@as(u64, @intCast(b)))));
                    }
                },
                .QuestionQuestion => {
                    if (left.isNull()) {
                        return right;
                    }

                    return left;
                },
                .Greater => {
                    if (left_f) |lf| {
                        if (right_f) |rf| {
                            return Value.fromBoolean(lf > rf);
                        } else {
                            return Value.fromBoolean(lf > @as(f64, @floatFromInt(right_i.?)));
                        }
                    } else {
                        if (right_f) |rf| {
                            return Value.fromBoolean(@as(f64, @floatFromInt(left_i.?)) > rf);
                        } else {
                            return Value.fromBoolean(left_i.? > right_i.?);
                        }
                    }
                    return Value.fromBoolean((left_f orelse left_i.?) > (right_f orelse right_i.?));
                },
                .Less => {
                    if (left_f) |lf| {
                        if (right_f) |rf| {
                            return Value.fromBoolean(lf < rf);
                        } else {
                            return Value.fromBoolean(lf < @as(f64, @floatFromInt(right_i.?)));
                        }
                    } else {
                        if (right_f) |rf| {
                            return Value.fromBoolean(@as(f64, @floatFromInt(left_i.?)) < rf);
                        } else {
                            return Value.fromBoolean(left_i.? < right_i.?);
                        }
                    }
                    return Value.fromBoolean((left_f orelse left_i.?) < (right_f orelse right_i.?));
                },
                .GreaterEqual => {
                    if (left_f) |lf| {
                        if (right_f) |rf| {
                            return Value.fromBoolean(lf >= rf);
                        } else {
                            return Value.fromBoolean(lf >= @as(f64, @floatFromInt(right_i.?)));
                        }
                    } else {
                        if (right_f) |rf| {
                            return Value.fromBoolean(@as(f64, @floatFromInt(left_i.?)) >= rf);
                        } else {
                            return Value.fromBoolean(left_i.? >= right_i.?);
                        }
                    }
                    return Value.fromBoolean((left_f orelse left_i.?) >= (right_f orelse right_i.?));
                },
                .LessEqual => {
                    if (left_f) |lf| {
                        if (right_f) |rf| {
                            return Value.fromBoolean(lf <= rf);
                        } else {
                            return Value.fromBoolean(lf <= @as(f64, @floatFromInt(right_i.?)));
                        }
                    } else {
                        if (right_f) |rf| {
                            return Value.fromBoolean(@as(f64, @floatFromInt(left_i.?)) <= rf);
                        } else {
                            return Value.fromBoolean(left_i.? <= right_i.?);
                        }
                    }
                    return Value.fromBoolean((left_f orelse left_i.?) <= (right_f orelse right_i.?));
                },
                .BangEqual => {
                    return Value.fromBoolean(!_value.valueEql(left, right));
                },
                .EqualEqual => {
                    return Value.fromBoolean(_value.valueEql(left, right));
                },
                .Plus => {
                    const right_s: ?*ObjString = if (right.isObj()) ObjString.cast(right.obj()) else null;
                    const left_s: ?*ObjString = if (left.isObj()) ObjString.cast(left.obj()) else null;

                    const right_l: ?*ObjList = if (right.isObj()) ObjList.cast(right.obj()) else null;
                    const left_l: ?*ObjList = if (left.isObj()) ObjList.cast(left.obj()) else null;

                    const right_m: ?*ObjMap = if (right.isObj()) ObjMap.cast(right.obj()) else null;
                    const left_m: ?*ObjMap = if (left.isObj()) ObjMap.cast(left.obj()) else null;

                    if (right_s != null) {
                        var new_string: std.ArrayList(u8) = std.ArrayList(u8).init(gc.allocator);
                        try new_string.appendSlice(left_s.?.string);
                        try new_string.appendSlice(right_s.?.string);

                        return (try gc.copyString(new_string.items)).toValue();
                    } else if (right_f != null or left_f != null) {
                        return Value.fromFloat((right_f orelse @as(f64, @floatFromInt(right_i.?))) + (left_f orelse @as(f64, @floatFromInt(left_i.?))));
                    } else if (right_i != null or left_i != null) {
                        return Value.fromInteger(right_i.? + left_i.?);
                    } else if (right_l != null) {
                        var new_list = std.ArrayList(Value).init(gc.allocator);
                        try new_list.appendSlice(left_l.?.items.items);
                        try new_list.appendSlice(right_l.?.items.items);

                        var list = try gc.allocateObject(
                            ObjList,
                            ObjList{
                                .type_def = left_l.?.type_def,
                                .methods = left_l.?.methods,
                                .items = new_list,
                            },
                        );

                        return list.toValue();
                    }

                    // map
                    var new_map = try left_m.?.map.clone();
                    var it = right_m.?.map.iterator();
                    while (it.next()) |entry| {
                        try new_map.put(entry.key_ptr.*, entry.value_ptr.*);
                    }

                    var map = try gc.allocateObject(
                        ObjMap,
                        ObjMap{
                            .type_def = left_m.?.type_def,
                            .methods = left_m.?.methods,
                            .map = new_map,
                        },
                    );

                    return map.toValue();
                },
                .Minus => {
                    if (right_f != null or left_f != null) {
                        return Value.fromFloat((right_f orelse @as(f64, @floatFromInt(right_i.?))) - (left_f orelse @as(f64, @floatFromInt(left_i.?))));
                    }

                    return Value.fromInteger(right_i.? - left_i.?);
                },
                .Star => {
                    if (right_f != null or left_f != null) {
                        return Value.fromFloat((right_f orelse @as(f64, @floatFromInt(right_i.?))) * (left_f orelse @as(f64, @floatFromInt(left_i.?))));
                    }

                    return Value.fromInteger(right_i.? * left_i.?);
                },
                .Slash => {
                    if (right_f != null or left_f != null) {
                        return Value.fromFloat((right_f orelse @as(f64, @floatFromInt(right_i.?))) / (left_f orelse @as(f64, @floatFromInt(left_i.?))));
                    }

                    return Value.fromFloat(@as(f64, @floatFromInt(right_i.?)) / @as(f64, @floatFromInt(left_i.?)));
                },
                .Percent => {
                    if (right_f != null or left_f != null) {
                        return Value.fromFloat(@mod((right_f orelse @as(f64, @floatFromInt(right_i.?))), (left_f orelse @as(f64, @floatFromInt(left_i.?)))));
                    }

                    return Value.fromInteger(@mod(right_i.?, left_i.?));
                },
                .And => {
                    return Value.fromBoolean(left.boolean() and right.boolean());
                },
                .Or => {
                    return Value.fromBoolean(left.boolean() or right.boolean());
                },
                else => unreachable,
            }
        }

        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        const left_type = self.left.type_def.?;
        const right_type = self.right.type_def.?;

        if (self.left.type_def == null or self.left.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.left.type_def.?.resolved_type.?.Placeholder);
        }

        if (self.right.type_def == null or self.right.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.right.type_def.?.resolved_type.?.Placeholder);
        }

        switch (self.operator) {
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
                    codegen.reporter.reportTypeCheck(
                        .binary_operand_type,
                        self.left.location,
                        left_type,
                        self.right.location,
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
                // We allow comparison between float and int so raise error if type != and one operand is not a number
                if (!left_type.eql(right_type) and ((left_type.def_type != .Integer and left_type.def_type != .Float) or (right_type.def_type != .Integer and right_type.def_type != .Float))) {
                    codegen.reporter.reportTypeCheck(
                        .comparison_operand_type,
                        self.left.location,
                        left_type,
                        self.right.location,
                        right_type,
                        "Type mismatch",
                    );
                }
            },

            else => unreachable,
        }

        switch (self.operator) {
            .QuestionQuestion => {
                if (!left_type.optional) {
                    codegen.reporter.reportErrorAt(
                        .optional,
                        node.location,
                        "Not an optional",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);

                const end_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_NOT_NULL);
                try codegen.emitOpCode(self.node.location, .OP_POP);

                _ = try self.right.toByteCode(self.right, codegen, breaks);

                codegen.patchJump(end_jump);
            },
            .Ampersand => {
                // Checking only left operand since we asserted earlier that both operand have the same type
                if (left_type.def_type != .Integer) {
                    codegen.reporter.reportErrorAt(
                        .bitwise_operand_type,
                        self.left.location,
                        "Expected `int`.",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_BAND);
            },
            .Bor => {
                // Checking only left operand since we asserted earlier that both operand have the same type
                if (left_type.def_type != .Integer) {
                    codegen.reporter.reportErrorAt(
                        .bitwise_operand_type,
                        self.left.location,
                        "Expected `int`.",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_BOR);
            },
            .Xor => {
                // Checking only left operand since we asserted earlier that both operand have the same type
                if (left_type.def_type != .Integer) {
                    codegen.reporter.reportErrorAt(
                        .bitwise_operand_type,
                        self.left.location,
                        "Expected `int`.",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_XOR);
            },
            .ShiftLeft => {
                // Checking only left operand since we asserted earlier that both operand have the same type
                if (left_type.def_type != .Integer) {
                    codegen.reporter.reportErrorAt(
                        .bitwise_operand_type,
                        self.left.location,
                        "Expected `int`.",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_SHL);
            },
            .ShiftRight => {
                // Checking only left operand since we asserted earlier that both operand have the same type
                if (left_type.def_type != .Integer) {
                    codegen.reporter.reportErrorAt(
                        .bitwise_operand_type,
                        self.left.location,
                        "Expected `int`.",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_SHR);
            },
            .Greater => {
                // Checking only left operand since we asserted earlier that both operand have the same type
                if (left_type.def_type != .Integer and left_type.def_type != .Float) {
                    codegen.reporter.reportErrorAt(
                        .comparison_operand_type,
                        self.left.location,
                        "Expected `int` or `float`.",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_GREATER);
            },
            .Less => {
                if (left_type.def_type != .Integer and left_type.def_type != .Float) {
                    codegen.reporter.reportErrorAt(
                        .comparison_operand_type,
                        self.left.location,
                        "Expected `int` or `float`.",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_LESS);
            },
            .GreaterEqual => {
                if (left_type.def_type != .Integer and left_type.def_type != .Float) {
                    codegen.reporter.reportErrorAt(
                        .comparison_operand_type,
                        self.left.location,
                        "Expected `int` or `float`.",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_LESS);
                try codegen.emitOpCode(self.node.location, .OP_NOT);
            },
            .LessEqual => {
                if (left_type.def_type != .Integer and left_type.def_type != .Float) {
                    codegen.reporter.reportErrorAt(
                        .comparison_operand_type,
                        self.left.location,
                        "Expected `int` or `float`.",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_GREATER);
                try codegen.emitOpCode(self.node.location, .OP_NOT);
            },
            .BangEqual => {
                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_EQUAL);
                try codegen.emitOpCode(self.node.location, .OP_NOT);
            },
            .EqualEqual => {
                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_EQUAL);
            },
            .Plus => {
                // zig fmt: off
                if (left_type.def_type != .Integer
                    and left_type.def_type != .Float
                    and left_type.def_type != .String
                    and left_type.def_type != .List
                    and left_type.def_type != .Map) {
                    codegen.reporter.reportErrorAt(.arithmetic_operand_type,self.left.location, "Expected a `int`, `float`, `str`, list or map.",);
                }
                // zig fmt: on

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, switch (left_type.def_type) {
                    .String => .OP_ADD_STRING,
                    .List => .OP_ADD_LIST,
                    .Map => .OP_ADD_MAP,
                    else => .OP_ADD,
                });
            },
            .Minus => {
                if (left_type.def_type != .Integer and left_type.def_type != .Float) {
                    codegen.reporter.reportErrorAt(
                        .arithmetic_operand_type,
                        self.left.location,
                        "Expected `int` or `float`.",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_SUBTRACT);
            },
            .Star => {
                if (left_type.def_type != .Integer and left_type.def_type != .Float) {
                    codegen.reporter.reportErrorAt(
                        .arithmetic_operand_type,
                        self.left.location,
                        "Expected `int` or `float`.",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_MULTIPLY);
            },
            .Slash => {
                if (left_type.def_type != .Integer and left_type.def_type != .Float) {
                    codegen.reporter.reportErrorAt(
                        .arithmetic_operand_type,
                        self.left.location,
                        "Expected `int` or `float`.",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_DIVIDE);
            },
            .Percent => {
                if (left_type.def_type != .Integer and left_type.def_type != .Float) {
                    codegen.reporter.reportErrorAt(
                        .arithmetic_operand_type,
                        self.left.location,
                        "Expected `int` or `float`.",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_MOD);
            },
            .And => {
                if (left_type.def_type != .Bool) {
                    codegen.reporter.reportErrorAt(
                        .logical_operand_type,
                        node.location,
                        "`and` expects operands to be `bool`",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);

                const end_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
                try codegen.emitOpCode(self.node.location, .OP_POP);

                _ = try self.right.toByteCode(self.right, codegen, breaks);

                codegen.patchJump(end_jump);
            },
            .Or => {
                if (left_type.def_type != .Bool) {
                    codegen.reporter.reportErrorAt(
                        .logical_operand_type,
                        node.location,
                        "`and` expects operands to be `bool`",
                    );
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);

                const else_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
                const end_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP);

                codegen.patchJump(else_jump);
                try codegen.emitOpCode(self.node.location, .OP_POP);

                _ = try self.right.toByteCode(self.right, codegen, breaks);

                codegen.patchJump(end_jump);
            },
            else => unreachable,
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Binary\", \"left\": ");

        try self.left.toJson(self.left, out);
        try out.print(", \"operator\": \"{}\", \"right\": ", .{self.operator});
        try self.right.toJson(self.right, out);
        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try self.left.render(self.left, out, depth);
        try out.writeAll(switch (self.operator) {
            .QuestionQuestion => " ?? ",
            .Ampersand => " & ",
            .Bor => " \\ ",
            .Xor => " ^ ",
            .ShiftLeft => " << ",
            .ShiftRight => " >> ",
            .Greater => " > ",
            .Less => " < ",
            .GreaterEqual => " >= ",
            .LessEqual => " <= ",
            .BangEqual => " != ",
            .EqualEqual => " == ",
            .Plus => " + ",
            .Minus => " - ",
            .Star => " * ",
            .Slash => " / ",
            .Percent => " % ",
            .And => " and ",
            .Or => " or ",
            else => unreachable,
        });
        try self.right.render(self.right, out, depth);
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Binary) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const SubscriptNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Subscript,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    subscripted: *ParseNode,
    index: *ParseNode,
    value: ?*ParseNode,

    fn constant(nodePtr: *anyopaque) bool {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        return self.subscripted.isConstant(self.subscripted) and self.index.isConstant(self.index) and self.value == null;
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.isConstant(node)) {
            const self = Self.cast(node).?;

            const subscriptable = (try self.subscripted.toValue(self.subscripted, gc)).obj();
            const index = floatToInteger(try self.index.toValue(self.index, gc));

            switch (subscriptable.obj_type) {
                .List => {
                    const list: *ObjList = ObjList.cast(subscriptable).?;

                    const list_index_i: ?i32 = if (index.isInteger()) index.integer() else null;

                    if (list_index_i == null or list_index_i.? < 0) {
                        return VM.Error.OutOfBound;
                    }

                    const list_index: usize = @intCast(list_index_i.?);

                    if (list_index < list.items.items.len) {
                        return list.items.items[list_index];
                    } else {
                        return VM.Error.OutOfBound;
                    }
                },
                .Map => {
                    const map: *ObjMap = ObjMap.cast(subscriptable).?;

                    if (map.map.get(index)) |value| {
                        return value;
                    } else {
                        return Value.Null;
                    }
                },
                .String => {
                    const str: *ObjString = ObjString.cast(subscriptable).?;

                    const str_index_i: ?i32 = if (index.isInteger()) index.integer() else null;

                    if (str_index_i == null or str_index_i.? < 0) {
                        return VM.Error.OutOfBound;
                    }

                    const str_index: usize = @intCast(str_index_i.?);

                    if (str_index < str.string.len) {
                        return (try gc.copyString(&([_]u8{str.string[str_index]}))).toValue();
                    } else {
                        return VM.Error.OutOfBound;
                    }
                },
                else => unreachable,
            }
        }

        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        _ = try self.subscripted.toByteCode(self.subscripted, codegen, breaks);

        if (self.subscripted.type_def == null or self.subscripted.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.subscripted.type_def.?.resolved_type.?.Placeholder);
        }

        if (self.index.type_def == null or self.index.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.index.type_def.?.resolved_type.?.Placeholder);
        }

        if (self.value != null and (self.value.?.type_def == null or self.value.?.type_def.?.def_type == .Placeholder)) {
            codegen.reporter.reportPlaceholder(self.value.?.type_def.?.resolved_type.?.Placeholder);
        }

        var get_code: OpCode = .OP_GET_LIST_SUBSCRIPT;
        var set_code: OpCode = .OP_SET_LIST_SUBSCRIPT;
        switch (self.subscripted.type_def.?.def_type) {
            .String => {
                if (self.index.type_def.?.def_type != .Integer) {
                    codegen.reporter.reportErrorAt(
                        .subscript_key_type,
                        self.index.location,
                        "Expected `int` index.",
                    );
                }

                get_code = .OP_GET_STRING_SUBSCRIPT;

                assert(self.value == null);
            },
            .List => {
                if (self.index.type_def.?.def_type != .Integer) {
                    codegen.reporter.reportErrorAt(
                        .subscript_key_type,
                        self.index.location,
                        "Expected `int` index.",
                    );
                }

                if (self.value) |value| {
                    if (!self.subscripted.type_def.?.resolved_type.?.List.item_type.eql(value.type_def.?)) {
                        codegen.reporter.reportTypeCheck(
                            .subscript_value_type,
                            self.subscripted.location,
                            self.subscripted.type_def.?.resolved_type.?.List.item_type,
                            value.location,
                            value.type_def.?,
                            "Bad value type",
                        );
                    }
                }
            },
            .Map => {
                if (!self.subscripted.type_def.?.resolved_type.?.Map.key_type.eql(self.index.type_def.?)) {
                    codegen.reporter.reportTypeCheck(
                        .subscript_key_type,
                        self.subscripted.location,
                        self.subscripted.type_def.?.resolved_type.?.Map.key_type,
                        self.index.location,
                        self.index.type_def.?,
                        "Bad key type",
                    );
                }

                if (self.value) |value| {
                    if (!self.subscripted.type_def.?.resolved_type.?.Map.value_type.eql(value.type_def.?)) {
                        codegen.reporter.reportTypeCheck(
                            .subscript_value_type,
                            self.subscripted.location,
                            self.subscripted.type_def.?.resolved_type.?.Map.value_type,
                            value.location,
                            value.type_def.?,
                            "Bad value type",
                        );
                    }
                }

                get_code = .OP_GET_MAP_SUBSCRIPT;
                set_code = .OP_SET_MAP_SUBSCRIPT;
            },
            else => codegen.reporter.reportErrorAt(
                .subscriptable,
                node.location,
                "Not subscriptable.",
            ),
        }

        _ = try self.index.toByteCode(self.index, codegen, breaks);

        if (self.value) |value| {
            _ = try value.toByteCode(value, codegen, breaks);

            try codegen.emitOpCode(self.node.location, set_code);
        } else {
            try codegen.emitOpCode(self.node.location, get_code);
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Subscript\", \"subscripted\": ");

        try self.subscripted.toJson(self.subscripted, out);

        try out.writeAll(", \"index\": ");

        try self.index.toJson(self.index, out);

        try out.writeAll(", ");

        if (self.value) |value| {
            try out.writeAll("\"value\": ");
            try value.toJson(value, out);
            try out.writeAll(", ");
        }

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try self.subscripted.render(self.subscripted, out, depth);
        try out.writeAll("[");
        try self.index.render(self.index, out, depth);
        try out.writeAll("]");
        if (self.value) |value| {
            try out.writeAll(" = ");
            try value.render(value, out, depth);
        }
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Subscript) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const TryNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Try,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    body: *ParseNode,
    clauses: std.AutoArrayHashMap(*ObjTypeDef, *ParseNode),
    clause_identifiers: [][]const u8,
    unconditional_clause: ?*ParseNode,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        const self = Self.cast(node).?;

        codegen.current.?.try_should_handle = std.AutoHashMap(*ObjTypeDef, Token).init(codegen.gc.allocator);
        defer {
            codegen.current.?.try_should_handle.?.deinit();
            codegen.current.?.try_should_handle = null;
        }

        // OP_TRY notifies runtime that we're handling error at offset
        const try_jump = try codegen.emitJump(node.location, .OP_TRY);

        _ = try self.body.toByteCode(self.body, codegen, breaks);

        // Jump reached if no error was raised
        const no_error_jump = try codegen.emitJump(self.body.end_location, .OP_JUMP);

        var exit_jumps = std.ArrayList(usize).init(codegen.gc.allocator);
        defer exit_jumps.deinit();

        codegen.patchTry(try_jump);
        for (self.clauses.keys()) |error_type| {
            const clause = self.clauses.get(error_type).?;

            // We assume the error is on top of the stack
            try codegen.emitOpCode(clause.location, .OP_COPY); // Copy error value since its argument to the catch clause
            try codegen.emitConstant(clause.location, error_type.toValue());
            try codegen.emitOpCode(clause.location, .OP_IS);
            // If error type does not match, jump to next catch clause
            const next_clause_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
            // Pop `is` result
            try codegen.emitOpCode(clause.location, .OP_POP);

            // Clause block will pop error value since its declared as a local in it
            // We don't catch things is the catch clause
            const previous = codegen.current.?.try_should_handle;
            codegen.current.?.try_should_handle = null;
            _ = try clause.toByteCode(clause, codegen, breaks);
            codegen.current.?.try_should_handle = previous;

            // After handling the error, jump over next clauses
            try exit_jumps.append(try codegen.emitJump(self.node.location, .OP_JUMP));

            codegen.patchJump(next_clause_jump);
            // Pop `is` result
            try codegen.emitOpCode(clause.location, .OP_POP);
        }

        if (self.unconditional_clause) |unconditional_clause| {
            // pop error because its not a local of this clause
            try codegen.emitOpCode(unconditional_clause.location, .OP_POP);
            // We don't catch things is the catch clause
            const previous = codegen.current.?.try_should_handle;
            codegen.current.?.try_should_handle = null;
            _ = try unconditional_clause.toByteCode(unconditional_clause, codegen, breaks);
            codegen.current.?.try_should_handle = previous;

            try exit_jumps.append(try codegen.emitJump(self.node.location, .OP_JUMP));
        }

        // Tell runtime we're not in a try block anymore
        try codegen.emitOpCode(node.location, .OP_TRY_END);
        // Uncaught error, throw the error again
        try codegen.emitOpCode(node.location, .OP_THROW);

        // Patch exit jumps
        for (exit_jumps.items) |exit_jump| {
            codegen.patchJump(exit_jump);
        }

        codegen.patchJump(no_error_jump);

        // OP_TRY_END notifies runtime that we're not in a try block anymore
        try codegen.emitOpCode(node.location, .OP_TRY_END);

        // Did we handle all errors not specified in current function signature?
        var it = codegen.current.?.try_should_handle.?.iterator();
        while (it.next()) |kv| {
            if (self.unconditional_clause == null and self.clauses.get(try kv.key_ptr.*.toParentType(codegen.gc.allocator, &codegen.gc.type_registry)) == null) {
                const err_str = try kv.key_ptr.*.toStringAlloc(codegen.gc.allocator);
                defer err_str.deinit();

                codegen.reporter.reportWithOrigin(
                    .error_not_handled,
                    node.location,
                    kv.value_ptr.*,
                    "Error type `{s}` not handled",
                    .{err_str.items},
                    "can occur here",
                );
            }
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Try\", ", .{});

        try ParseNode.stringify(node, out);

        try out.writeAll(",\"body\": ");

        try self.body.toJson(self.body, out);

        try out.writeAll(",\"unconditional_clause\": ");

        if (self.unconditional_clause) |clause| {
            try clause.toJson(clause, out);
        } else {
            try out.writeAll("null");
        }

        try out.writeAll(",\"clauses\": {");
        // TODO
        try out.writeAll("}");

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);
        try out.writeAll("try {\n");
        try self.body.render(self.body, out, depth + 1);
        try out.writeAll("}");

        var it = self.clauses.iterator();
        var i: usize = 0;
        while (it.next()) |kv| : (i += 1) {
            try out.writeAll(" catch (");
            try kv.key_ptr.*.toStringUnqualified(out);
            try out.print("{s}) {{\n", .{self.clause_identifiers[i]});
            try kv.value_ptr.*.render(kv.value_ptr.*, out, depth + 1);
            try out.writeAll("}");
        }

        if (self.unconditional_clause) |unconditional_clause| {
            try out.writeAll(" catch {\n");
            try unconditional_clause.render(unconditional_clause, out, depth + 1);
            try out.writeAll("}");
        }

        try out.writeAll("\n");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Try) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const FunctionNode = struct {
    const Self = @This();

    var next_id: usize = 0;

    node: ParseNode = .{
        .node_type = .Function,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    id: usize,
    static: bool = false,
    body: ?*BlockNode = null,
    arrow_expr: ?*ParseNode = null,
    native: ?*ObjNative = null,
    test_message: ?[]const u8 = null,
    // If true this is the root of a script being imported
    import_root: bool = false,
    upvalue_binding: std.AutoArrayHashMap(u8, bool),

    // Useful when generating root script bootstrap code
    main_slot: ?usize = null,
    main_location: ?Token = null,
    test_slots: ?[]usize = null,
    test_locations: ?[]Token = null,
    exported_count: ?usize = null,

    // Set when the function is first generated
    // The JIT compiler can then reference it when creating its closure
    function: ?*ObjFunction = null,

    pub fn nextId() usize {
        Self.next_id += 1;

        return Self.next_id;
    }

    fn constant(_: *anyopaque) bool {
        // TODO: should be true but requires to codegen the node
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        const codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        const function_type = node.type_def.?.resolved_type.?.Function.function_type;

        if (node.synchronize(codegen)) {
            return null;
        }

        // If function is a test block and we're not testing/checking/etc. don't waste time generating the node
        if (codegen.flavor == .Run and function_type == .Test) {
            std.debug.print("skipped a test at {s}:{}:{}\n", .{ node.location.script_name, node.location.line, node.location.column });
            return null;
        }

        var self = Self.cast(node).?;

        var enclosing = codegen.current;
        codegen.current = try codegen.gc.allocator.create(Frame);
        codegen.current.?.* = Frame{
            .enclosing = enclosing,
            .function_node = self,
        };

        var function = try ObjFunction.init(
            codegen.gc.allocator,
            self,
            node.type_def.?.resolved_type.?.Function.name,
        );

        function.type_def = node.type_def.?;

        // Check for any remaining placeholders in function signature
        if (function.type_def.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(function.type_def.resolved_type.?.Placeholder);
        } else {
            const function_def = function.type_def.resolved_type.?.Function;

            if (function_def.return_type.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(function_def.return_type.resolved_type.?.Placeholder);
            }

            if (function_def.yield_type.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(function_def.yield_type.resolved_type.?.Placeholder);
            }

            var it = function_def.parameters.iterator();
            while (it.next()) |kv| {
                if (kv.value_ptr.*.def_type == .Placeholder) {
                    codegen.reporter.reportPlaceholder(kv.value_ptr.*.resolved_type.?.Placeholder);
                }
            }

            if (function_def.error_types) |error_types| {
                for (error_types) |error_type| {
                    if (error_type.def_type == .Placeholder) {
                        codegen.reporter.reportPlaceholder(error_type.resolved_type.?.Placeholder);
                    }
                }
            }
        }

        // First chunk constant is the empty string
        _ = try function.chunk.addConstant(
            null,
            Value.fromObj((try codegen.gc.copyString("")).toObj()),
        );

        codegen.current.?.function = try codegen.gc.allocateObject(ObjFunction, function);

        // Can't have both arrow expression and body
        assert((self.arrow_expr != null and self.body == null) or (self.arrow_expr == null and self.body != null));

        // Generate function's body bytecode
        if (self.arrow_expr) |arrow_expr| {
            _ = try arrow_expr.toByteCode(arrow_expr, codegen, breaks);
            try codegen.emitOpCode(arrow_expr.location, .OP_RETURN);
            codegen.current.?.return_emitted = true;
        } else {
            _ = try self.body.?.node.toByteCode(&self.body.?.node, codegen, breaks);
        }

        if (function_type != .Extern) {
            // If .Script, search for exported globals and return them in a map
            if (function_type == .Script or function_type == .ScriptEntryPoint) {
                // If top level, search `main` or `test` function(s) and call them
                // Then put any exported globals on the stack
                if (codegen.flavor != .Test and function_type == .ScriptEntryPoint) {
                    if (self.main_slot) |main_slot| {
                        try codegen.emitCodeArg(self.main_location.?, .OP_GET_GLOBAL, @intCast(main_slot));
                        try codegen.emitCodeArg(self.main_location.?, .OP_GET_LOCAL, 0); // cli args are always local 0
                        try codegen.emitCodeArgs(self.main_location.?, .OP_CALL, 1, 0);
                    }
                } else if (codegen.flavor == .Test and self.test_slots != null) {
                    // Create an entry point wich runs all `test`
                    for (self.test_slots.?, 0..) |slot, index| {
                        try codegen.emitCodeArg(self.test_locations.?[index], .OP_GET_GLOBAL, @intCast(slot));
                        try codegen.emitCodeArgs(self.test_locations.?[index], .OP_CALL, 0, 0);
                    }
                }

                // If we're being imported, put all globals on the stack
                if (self.import_root) {
                    if (self.exported_count orelse 0 > 16777215) {
                        codegen.reporter.reportErrorAt(
                            .export_count,
                            node.location,
                            "Can't export more than 16777215 values.",
                        );
                    }

                    var index: usize = 0;
                    while (index < self.exported_count orelse 0) : (index += 1) {
                        try codegen.emitCodeArg(node.location, .OP_GET_GLOBAL, @intCast(index));
                    }

                    try codegen.emitCodeArg(node.location, .OP_EXPORT, @intCast(self.exported_count orelse 0));
                } else {
                    try codegen.emitOpCode(node.location, .OP_VOID);
                    try codegen.emitOpCode(node.location, .OP_RETURN);
                    codegen.current.?.return_emitted = true;
                }
            } else if (codegen.current.?.function.?.type_def.resolved_type.?.Function.return_type.def_type == .Void and !codegen.current.?.return_emitted) {
                // TODO: detect if some branches of the function body miss a return statement
                try codegen.emitReturn(node.location);
            } else if (!codegen.current.?.return_emitted) {
                codegen.reporter.reportErrorAt(
                    .missing_return,
                    node.location,
                    "Missing return statement",
                );
            }
        }

        var frame = codegen.current.?;
        var current_function: *ObjFunction = frame.function.?;
        current_function.upvalue_count = @intCast(self.upvalue_binding.count());

        if (BuildOptions.debug) {
            try disassembleChunk(&current_function.chunk, current_function.name.string);
            std.debug.print("\n\n", .{});
        }

        codegen.current = frame.enclosing;

        if (function_type != .ScriptEntryPoint) {
            // `extern` functions don't have upvalues
            if (function_type == .Extern) {
                try codegen.emitCodeArg(node.location, .OP_CONSTANT, try codegen.makeConstant(self.native.?.toValue()));
            } else {
                try codegen.emitCodeArg(node.location, .OP_CLOSURE, try codegen.makeConstant(current_function.toValue()));

                var it = self.upvalue_binding.iterator();
                while (it.next()) |kv| {
                    try codegen.emit(node.location, if (kv.value_ptr.*) 1 else 0);
                    try codegen.emit(node.location, kv.key_ptr.*);
                }
            }
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        self.function = current_function;

        return current_function;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print(
            "{{\"node\": \"Function\", \"type\": \"{}\", \"name\": \"{s}\", ",
            .{
                self.node.type_def.?.resolved_type.?.Function.function_type,
                self.node.type_def.?.resolved_type.?.Function.name.string,
            },
        );

        if (self.body) |body| {
            try out.writeAll("\"body\": ");

            try body.toNode().toJson(body.toNode(), out);
        } else if (self.arrow_expr) |expr| {
            try out.writeAll("\"arrow_expr\": ");

            try expr.toJson(expr, out);
        }

        try out.writeAll(", ");

        if (self.native) |native| {
            try out.writeAll("\"native\": \"");

            try valueToString(out, native.toValue());

            try out.writeAll("\",");
        }

        if (self.test_message) |test_message| {
            try out.print("\"test_message\": \"{s}\", ", .{test_message});
        }

        const function_def = node.type_def.?.resolved_type.?.Function;
        if (function_def.error_types != null and function_def.error_types.?.len > 0) {
            try out.writeAll("\"error_types\":[");
            for (function_def.error_types.?, 0..) |error_type, i| {
                try out.writeAll("\"");
                try error_type.toString(out);
                try out.writeAll("\"");
                if (i < function_def.error_types.?.len - 1) {
                    try out.writeAll(",");
                }
            }
            try out.writeAll("],");
        }

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn init(parser: *Parser, function_type: FunctionType, script_name: []const u8, name: ?[]const u8) !Self {
        var self = Self{
            .id = Self.nextId(),
            .body = try parser.gc.allocator.create(BlockNode),
            .upvalue_binding = std.AutoArrayHashMap(u8, bool).init(parser.gc.allocator),
        };

        self.body.?.* = BlockNode.init(parser.gc.allocator);

        const function_name: []const u8 = switch (function_type) {
            .EntryPoint => "main",
            .ScriptEntryPoint, .Script => name orelse script_name,
            else => name orelse "???",
        };

        const function_def = ObjFunction.FunctionDef{
            .id = ObjFunction.FunctionDef.nextId(),
            .name = try parser.gc.copyString(function_name),
            .script_name = try parser.gc.copyString(script_name),
            .return_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
            .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
            .parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
            .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
            .function_type = function_type,
            .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
        };

        const type_def = ObjTypeDef.TypeUnion{ .Function = function_def };

        self.node.type_def = try parser.gc.type_registry.getTypeDef(
            .{
                .def_type = .Function,
                .resolved_type = type_def,
            },
        );

        return self;
    }

    pub fn deinit(self: Self) void {
        self.body.deinit();
        self.default_arguments.deinit();
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;
        const function_type = node.type_def.?.resolved_type.?.Function.function_type;

        if (self.test_message) |test_message| {
            try out.print("\ntest \"{s}\"", .{test_message});
        } else if (self.arrow_expr == null and function_type != .ScriptEntryPoint) {
            // No need to print return type for arrow function
            try out.writeAll("\n");
            try out.writeByteNTimes(' ', depth * 4);
            if (self.static) {
                try out.writeAll("static ");
            }
            try node.type_def.?.toStringUnqualified(out);
        }

        if (self.arrow_expr) |arrow_expr| {
            try out.writeAll(" -> ");
            try arrow_expr.render(arrow_expr, out, depth);
        } else if (self.body) |body| {
            if (function_type != .ScriptEntryPoint) {
                try out.writeAll(" {\n");
                try body.node.render(&body.node, out, depth + 1);
                try out.writeByteNTimes(' ', depth * 4);
                try out.writeAll("}");
            } else {
                try body.node.render(&body.node, out, depth);
            }
        }
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Function) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const YieldNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Yield,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    expression: *ParseNode,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?; // self

        const current_function_typedef = codegen.current.?.function_node.node.type_def.?.resolved_type.?.Function;
        const current_function_type = current_function_typedef.function_type;
        switch (current_function_type) {
            .Script,
            .ScriptEntryPoint,
            .EntryPoint,
            .Test,
            .Extern,
            => codegen.reporter.reportErrorAt(.yield_not_allowed, node.location, "Can't yield here"),
            else => {},
        }

        if (node.type_def == null) {
            codegen.reporter.reportErrorAt(.unknown, node.location, "Unknown type.");
        } else if (node.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(node.type_def.?.resolved_type.?.Placeholder);
        } else if (!codegen.current.?.function.?.type_def.resolved_type.?.Function.yield_type.eql(node.type_def.?)) {
            codegen.reporter.reportTypeCheck(
                .yield_type,
                codegen.current.?.function_node.node.location,
                codegen.current.?.function.?.type_def.resolved_type.?.Function.yield_type,
                node.location,
                node.type_def.?,
                "Bad yield value",
            );
        }

        _ = try self.expression.toByteCode(self.expression, codegen, breaks);

        try codegen.emitOpCode(node.location, .OP_YIELD);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?; // self

        try out.writeAll("{\"node\": \"Yield\", \"expression\": ");

        try self.expression.toJson(self.expression, out);

        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeAll("yield ");
        try self.expression.render(self.expression, out, depth);
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Yield) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ResolveNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Resolve,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    fiber: *ParseNode,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?; // self

        if (self.fiber.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.fiber.type_def.?.resolved_type.?.Placeholder);

            return null;
        }

        if (self.fiber.type_def.?.def_type != .Fiber) {
            codegen.reporter.reportErrorAt(.fiber, self.fiber.location, "Not a fiber");
        }

        _ = try self.fiber.toByteCode(self.fiber, codegen, breaks);

        try codegen.emitOpCode(node.location, .OP_RESOLVE);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?; // self

        try out.writeAll("{\"node\": \"Resolve\", \"fiber\": ");

        try self.fiber.toJson(self.fiber, out);

        try out.writeAll(",");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeAll("resolve ");
        try self.fiber.render(self.fiber, out, depth);
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Resolve) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ResumeNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Resume,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    fiber: *ParseNode,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?; // self

        if (self.fiber.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.fiber.type_def.?.resolved_type.?.Placeholder);

            return null;
        }

        if (self.fiber.type_def.?.def_type != .Fiber) {
            codegen.reporter.reportErrorAt(.fiber, self.fiber.location, "Not a fiber");
        }

        _ = try self.fiber.toByteCode(self.fiber, codegen, breaks);

        try codegen.emitOpCode(node.location, .OP_RESUME);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?; // self

        try out.writeAll("{\"node\": \"Resume\", \"fiber\": ");

        try self.fiber.toJson(self.fiber, out);

        try out.writeAll(",");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeAll("resume ");
        try self.fiber.render(self.fiber, out, depth);
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Resume) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const AsyncCallNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .AsyncCall,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    call: *ParseNode,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?; // self

        // Push fiber type as constant (we only need it if the fiber is printed out)
        // Should not interfere with local counts since OP_ROUTINE will consume it right away
        try codegen.emitConstant(
            node.location,
            node.type_def.?.toValue(),
        );

        _ = try self.call.toByteCode(self.call, codegen, breaks);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?; // self

        try out.writeAll("{\"node\": \"AsyncCall\", \"call\": ");

        try self.call.toJson(self.call, out);

        try out.writeAll(",");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeAll("&");
        try self.call.render(self.call, out, depth);
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .AsyncCall) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const CallNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Call,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    async_call: bool = false,
    callee: *ParseNode,
    callable_type: ?*ObjTypeDef,
    arguments: std.AutoArrayHashMap(*ObjString, *ParseNode),
    catch_default: ?*ParseNode = null,
    trailing_comma: bool,

    resolved_generics: []*ObjTypeDef,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        if (self.callee.type_def == null or self.callee.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.callee.type_def.?.resolved_type.?.Placeholder);
        }

        // This is not a call but an Enum(value)
        if (self.callee.type_def.?.def_type == .Enum) {
            if (self.async_call) {
                codegen.reporter.reportErrorAt(
                    .fiber_call_not_allowed,
                    self.callee.end_location,
                    "Can't be wrapped in a fiber",
                );
            }

            if (self.catch_default != null) {
                codegen.reporter.reportErrorAt(
                    .no_error,
                    self.callee.end_location,
                    "Doesn't raise any error",
                );
            }

            if (self.arguments.count() > 1) {
                codegen.reporter.reportErrorAt(
                    .enum_argument,
                    self.callee.end_location,
                    "Enum instanciation expect only value",
                );
            } else if (self.arguments.count() == 0) {
                codegen.reporter.reportErrorAt(
                    .enum_argument,
                    self.callee.end_location,
                    "Enum instanciation expect value",
                );

                return null;
            }

            const value = self.arguments.get(self.arguments.keys()[0]).?;

            if (value.type_def.?.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(value.type_def.?.resolved_type.?.Placeholder);
            }

            _ = try self.callee.toByteCode(self.callee, codegen, breaks);
            _ = try value.toByteCode(value, codegen, breaks);
            try codegen.emitOpCode(value.location, .OP_GET_ENUM_CASE_FROM_VALUE);

            return null;
        }

        // Find out if call is invoke or regular call
        var invoked = false;
        var invoked_on: ?ObjTypeDef.Type = null;

        if (self.callee.node_type == .Dot) {
            const dot = DotNode.cast(self.callee).?;
            const field_accessed = dot.callee.type_def;

            if (field_accessed == null or field_accessed.?.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(field_accessed.?.resolved_type.?.Placeholder);
            }

            invoked = field_accessed.?.def_type != .Object;
            invoked_on = field_accessed.?.def_type;
        }

        if (!invoked and invoked_on == null) {
            _ = try self.callee.toByteCode(self.callee, codegen, breaks);
        }

        const callee_type = switch (self.callee.node_type) {
            .Dot => DotNode.cast(self.callee).?.member_type_def,
            else => self.callee.type_def,
        };

        if (callee_type == null or callee_type.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(callee_type.?.resolved_type.?.Placeholder);

            // We know nothing about the function being called, no need to go any further
            return null;
        } else if (callee_type.?.def_type != .Function) {
            codegen.reporter.reportErrorAt(
                .callable,
                self.node.location,
                "Can't be called",
            );

            return null;
        } else if (callee_type.?.optional) {
            codegen.reporter.reportErrorAt(
                .callable,
                self.node.location,
                "Function maybe null and can't be called",
            );
        }

        const function_type = try callee_type.?.populateGenerics(
            callee_type.?.resolved_type.?.Function.id,
            self.resolved_generics,
            &codegen.gc.type_registry,
            null,
        );

        if (function_type.resolved_type.?.Function.generic_types.count() > self.resolved_generics.len) {
            codegen.reporter.reportErrorAt(
                .generic_type,
                node.location,
                "Missing generic types",
            );
        } else if (function_type.resolved_type.?.Function.generic_types.count() < self.resolved_generics.len) {
            codegen.reporter.reportErrorAt(
                .generic_type,
                node.location,
                "Too many generic types",
            );
        }

        const yield_type = function_type.resolved_type.?.Function.yield_type;

        // Function being called and current function should have matching yield type unless the current function is an entrypoint
        const current_function_typedef = codegen.current.?.function_node.node.type_def.?.resolved_type.?.Function;
        const current_function_type = current_function_typedef.function_type;
        const current_function_yield_type = current_function_typedef.yield_type;
        switch (current_function_type) {
            // Event though a function can call a yieldable function without wraping it in a fiber, the function itself could be called in a fiber
            .Function, .Method, .Anonymous => {
                if (!current_function_yield_type.eql(yield_type)) {
                    codegen.reporter.reportTypeCheck(
                        .yield_type,
                        codegen.current.?.function_node.node.location,
                        current_function_yield_type,
                        node.location,
                        yield_type,
                        "Bad function yield type",
                    );
                }
            },
            else => {},
        }

        // Arguments
        const args: std.AutoArrayHashMap(*ObjString, *ObjTypeDef) = function_type.resolved_type.?.Function.parameters;
        const defaults = function_type.resolved_type.?.Function.defaults;
        const arg_keys = args.keys();
        const arg_count = arg_keys.len;

        var missing_arguments = std.AutoArrayHashMap(*ObjString, usize).init(codegen.gc.allocator);
        defer missing_arguments.deinit();
        for (arg_keys, 0..) |arg_name, pindex| {
            try missing_arguments.put(arg_name, pindex);
        }

        if (self.arguments.count() > args.count()) {
            codegen.reporter.reportErrorAt(
                .call_arguments,
                node.location,
                "Too many arguments.",
            );
        }

        // First push on the stack arguments has they are parsed
        var needs_reorder = false;
        for (self.arguments.keys(), 0..) |arg_key, index| {
            const argument = self.arguments.get(arg_key).?;
            const actual_arg_key = if (index == 0 and std.mem.eql(u8, arg_key.string, "$")) arg_keys[0] else arg_key;
            const def_arg_type = args.get(actual_arg_key);

            const ref_index = args.getIndex(actual_arg_key);
            if (index != ref_index) {
                needs_reorder = true;
            }

            // Type check the argument
            if (def_arg_type) |arg_type| {
                if (argument.type_def == null or argument.type_def.?.def_type == .Placeholder) {
                    codegen.reporter.reportPlaceholder(argument.type_def.?.resolved_type.?.Placeholder);
                } else if (!arg_type.eql(argument.type_def.?)) {
                    codegen.reporter.reportTypeCheck(
                        .call_argument_type,
                        self.callee.location,
                        arg_type,
                        argument.location,
                        argument.type_def.?,
                        "Bad argument type",
                    );
                }

                _ = missing_arguments.orderedRemove(actual_arg_key);
            } else {
                codegen.reporter.reportErrorFmt(
                    .call_arguments,
                    argument.location,
                    "Argument `{s}` does not exists.",
                    .{arg_key.string},
                );
            }

            _ = try argument.toByteCode(argument, codegen, breaks);
        }

        // Argument order reference
        var arguments_order_ref = std.ArrayList(*ObjString).init(codegen.gc.allocator);
        defer arguments_order_ref.deinit();
        try arguments_order_ref.appendSlice(self.arguments.keys());

        // Push default arguments
        if (missing_arguments.count() > 0) {
            var tmp_missing_arguments = try missing_arguments.clone();
            defer tmp_missing_arguments.deinit();
            const missing_keys = tmp_missing_arguments.keys();
            for (missing_keys) |missing_key| {
                if (defaults.get(missing_key)) |default| {
                    // TODO: like ObjTypeDef, avoid generating constants multiple time for the same value
                    try codegen.emitConstant(node.location, default);
                    try codegen.emitOpCode(node.location, .OP_CLONE);

                    try arguments_order_ref.append(missing_key);
                    _ = missing_arguments.orderedRemove(missing_key);
                    needs_reorder = true;
                }
            }
        }

        if (missing_arguments.count() > 0) {
            var missing = std.ArrayList(u8).init(codegen.gc.allocator);
            const missing_writer = missing.writer();
            for (missing_arguments.keys()) |key| {
                try missing_writer.print("{s}, ", .{key.string});
            }
            defer missing.deinit();
            codegen.reporter.reportErrorFmt(
                .call_arguments,
                node.location,
                "Missing argument(s): {s}",
                .{missing.items},
            );
        }

        // Reorder arguments
        if (needs_reorder) {
            // Until ordered
            while (true) {
                var ordered = true;

                for (arguments_order_ref.items, 0..) |arg_key, index| {
                    const actual_arg_key = if (index == 0 and std.mem.eql(u8, arg_key.string, "$")) args.keys()[0] else arg_key;
                    const correct_index = args.getIndex(actual_arg_key).?;

                    if (correct_index != index) {
                        ordered = false;

                        // TODO: both OP_SWAP args could fit in a 32 bit instruction
                        try codegen.emitCodeArg(node.location, .OP_SWAP, @intCast(arg_count - index - 1));
                        // to where it should be
                        try codegen.emit(node.location, @intCast(arg_count - correct_index - 1));

                        // Switch it in the reference
                        var temp = arguments_order_ref.items[index];
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
        const error_types = function_type.resolved_type.?.Function.error_types;
        if (self.catch_default) |catch_default| {
            if (error_types == null or error_types.?.len == 0) {
                codegen.reporter.reportErrorAt(
                    .no_error,
                    node.location,
                    "Function doesn't raise any error",
                );
            } else if (error_types != null) {
                if (catch_default.type_def == null or catch_default.type_def.?.def_type == .Placeholder) {
                    codegen.reporter.reportPlaceholder(catch_default.type_def.?.resolved_type.?.Placeholder);
                } else {
                    // Expression
                    if (!node.type_def.?.eql(catch_default.type_def.?) and !(try node.type_def.?.cloneOptional(&codegen.gc.type_registry)).eql(catch_default.type_def.?)) {
                        codegen.reporter.reportTypeCheck(
                            .inline_catch_type,
                            self.callee.location,
                            node.type_def.?,
                            catch_default.location,
                            catch_default.type_def.?,
                            "Bad inline catch value type",
                        );
                    }
                }

                _ = try catch_default.toByteCode(catch_default, codegen, breaks);
            }
        } else if (error_types) |errors| {
            if (codegen.current.?.enclosing != null and codegen.current.?.function.?.type_def.resolved_type.?.Function.function_type != .Test) {
                var handles_any = false;
                var not_handled = std.ArrayList(*ObjTypeDef).init(codegen.gc.allocator);
                defer not_handled.deinit();
                for (errors) |error_type| {
                    if (error_type.def_type == .Void) {
                        continue;
                    }

                    var handled = false;

                    if (codegen.current.?.function.?.type_def.resolved_type.?.Function.error_types) |handled_types| {
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
                        if (codegen.current.?.try_should_handle != null) {
                            try codegen.current.?.try_should_handle.?.put(error_type, self.callee.location);
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
                    const error_str = try error_type.toStringAlloc(codegen.gc.allocator);
                    defer error_str.deinit();

                    codegen.reporter.reportErrorFmt(
                        .error_not_handled,
                        node.location,
                        "Error `{s}` is not handled",
                        .{error_str.items},
                    );
                }
            }
        }

        // This is an async call, create a fiber
        if (self.async_call) {
            if (!invoked) {
                // zig fmt: off
                const call_arg_count: u8 = if (!invoked) @as(u8, @intCast( arg_count))
                    else
                        if (invoked_on != null and invoked_on.? != .ObjectInstance and invoked_on.? != .ProtocolInstance) @as(u8, @intCast( arg_count)) + 1 
                        else @as(u8, @intCast( arg_count));
                // zig fmt: on

                try codegen.emitCodeArgs(
                    self.node.location,
                    .OP_ROUTINE,
                    call_arg_count,
                    if (self.catch_default != null) 1 else 0,
                );

                try node.patchOptJumps(codegen);
                try node.endScope(codegen);

                return null;
            } else {
                if (invoked) {
                    try codegen.emitCodeArg(
                        self.node.location,
                        .OP_INVOKE_ROUTINE,
                        try codegen.identifierConstant(DotNode.cast(self.callee).?.identifier.lexeme),
                    );
                }

                try codegen.emitTwo(
                    self.node.location,
                    if (invoked_on != null and invoked_on.? != .ObjectInstance and invoked_on.? != .ProtocolInstance) @as(u8, @intCast(arg_count)) + 1 else @as(u8, @intCast(self.arguments.count())),
                    if (self.catch_default != null) 1 else 0,
                );

                try node.patchOptJumps(codegen);
                try node.endScope(codegen);

                return null;
            }
        }

        // Normal call/invoke
        if (invoked) {
            // TODO: can it be invoked without callee being a DotNode?
            try codegen.emitCodeArg(
                self.node.location,
                switch (DotNode.cast(self.callee).?.callee.type_def.?.def_type) {
                    .ObjectInstance, .ProtocolInstance => .OP_INSTANCE_INVOKE,
                    .String => .OP_STRING_INVOKE,
                    .Pattern => .OP_PATTERN_INVOKE,
                    .Fiber => .OP_FIBER_INVOKE,
                    .List => .OP_LIST_INVOKE,
                    .Map => .OP_MAP_INVOKE,
                    else => unreachable,
                },
                try codegen.identifierConstant(DotNode.cast(self.callee).?.identifier.lexeme),
            );
        }

        if (!invoked) {
            try codegen.emitCodeArgs(
                self.node.location,
                .OP_CALL,
                @intCast(arguments_order_ref.items.len),
                if (self.catch_default != null) 1 else 0,
            );
        } else {
            try codegen.emitTwo(
                self.node.location,
                if (invoked_on != null and invoked_on.? != .ObjectInstance and invoked_on.? != .ProtocolInstance)
                    @as(u8, @intCast(arg_count)) + 1
                else
                    @as(u8, @intCast(arg_count)),
                if (self.catch_default != null) 1 else 0,
            );
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Call\"");

        var invoked = false;
        var invoked_on = false;
        if (self.callee.node_type == .Dot) {
            const dot = DotNode.cast(self.callee).?;
            const field_accessed = dot.callee.type_def;

            invoked = field_accessed.?.def_type != .Object;
            invoked_on = true;
        }

        if (!invoked and !invoked_on) {
            try out.writeAll(", \"callee\": ");
            try self.callee.toJson(self.callee, out);
        }

        try out.writeAll(", \"resolved_generics\": [");
        for (self.resolved_generics, 0..) |generic, i| {
            try out.writeAll("\"");
            try generic.toString(out);
            try out.writeAll("\"");

            if (i < self.resolved_generics.len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], \"arguments\": [");

        for (self.arguments.keys(), 0..) |key, i| {
            const argument = self.arguments.get(key).?;

            try out.print("{{\"name\": \"{s}\", \"value\": ", .{key.string});

            try argument.toJson(argument, out);

            try out.writeAll("}");

            if (i < self.arguments.keys().len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], ");

        if (self.catch_default) |default| {
            try out.writeAll("\"catch_default\": ");

            try default.toJson(default, out);

            try out.writeAll(",");
        }

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        // Find out if call is invoke or regular call
        var invoked = false;
        var invoked_on: ?ObjTypeDef.Type = null;

        if (self.callee.node_type == .Dot) {
            const dot = DotNode.cast(self.callee).?;
            const field_accessed = dot.callee.type_def;

            invoked = field_accessed.?.def_type != .Object;
            invoked_on = field_accessed.?.def_type;
        }

        if (!invoked and invoked_on == null) {
            try self.callee.render(self.callee, out, depth);
        }

        try out.writeAll("(");
        var it = self.arguments.iterator();
        var i: usize = 0;
        while (it.next()) |kv| : (i += 1) {
            // If trailing comma, means we want all element on its own line
            if (self.trailing_comma) {
                try out.writeAll("\n");
                try out.writeByteNTimes(' ', (depth + 1) * 4);
            }

            if (i > 0) {
                try out.print("{s}: ", .{kv.key_ptr.*.string});
            }

            try kv.value_ptr.*.render(kv.value_ptr.*, out, depth + 1);

            if (i < self.arguments.count() - 1 or self.trailing_comma) {
                try out.writeAll(",");
                if (!self.trailing_comma) {
                    try out.writeAll(" ");
                }
            }
        }
        if (self.trailing_comma) {
            try out.writeAll("\n");
            try out.writeByteNTimes(' ', depth * 4);
        }
        try out.writeAll(")");

        if (self.catch_default) |catch_default| {
            try out.writeAll(" catch ");
            try catch_default.render(catch_default, out, depth);
        }
    }

    pub fn init(allocator: Allocator, callee: *ParseNode) Self {
        return Self{
            .callee = callee,
            .arguments = std.ArrayList(ParsedArg).init(allocator),
        };
    }

    pub fn deinit(self: Self) void {
        self.callee.deinit();
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Call) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const FunDeclarationNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .FunDeclaration,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    function: *FunctionNode,
    slot: usize,
    slot_type: SlotType,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        _ = try self.function.node.toByteCode(&self.function.node, codegen, breaks);

        if (self.slot_type == .Global) {
            try codegen.emitCodeArg(self.node.location, .OP_DEFINE_GLOBAL, @intCast(self.slot));
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"FunDeclaration\",\"slot_type\": \"{}\",\"function\": ", .{self.slot_type});

        try self.function.node.toJson(&self.function.node, out);

        try out.writeAll(",");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);
        try self.function.node.render(&self.function.node, out, depth);
        if (self.function.native != null or self.function.arrow_expr != null) {
            try out.writeAll(";");
        }
        try out.writeAll("\n");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .FunDeclaration) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const VarDeclarationNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .VarDeclaration,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    name: Token,
    value: ?*ParseNode = null,
    type_def: *ObjTypeDef,
    constant: bool,
    slot: usize,
    slot_type: SlotType,
    expression: bool,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        if (self.value) |value| {
            if (value.type_def == null or value.type_def.?.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(value.type_def.?.resolved_type.?.Placeholder);
            } else if (self.type_def.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(self.type_def.resolved_type.?.Placeholder);
            } else if (!(try self.type_def.toInstance(codegen.gc.allocator, &codegen.gc.type_registry)).eql(value.type_def.?) and !(try (try self.type_def.toInstance(codegen.gc.allocator, &codegen.gc.type_registry)).cloneNonOptional(&codegen.gc.type_registry)).eql(value.type_def.?)) {
                codegen.reporter.reportTypeCheck(
                    .assignment_value_type,
                    node.location,
                    try self.type_def.toInstance(codegen.gc.allocator, &codegen.gc.type_registry),
                    value.location,
                    value.type_def.?,
                    "Wrong variable type",
                );
            }

            _ = try value.toByteCode(value, codegen, breaks);
        } else {
            try codegen.emitOpCode(self.node.location, .OP_NULL);
        }

        if (self.slot_type == .Global) {
            try codegen.emitCodeArg(self.node.location, .OP_DEFINE_GLOBAL, @intCast(self.slot));
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print(
            "{{\"node\": \"VarDeclaration\", \"name\": \"{s}\", \"constant\": {}, \"var_type\": \"",
            .{
                self.name.lexeme,
                self.constant,
            },
        );

        try self.type_def.toString(out);

        try out.print(" @{}\", ", .{@intFromPtr(self.type_def)});

        if (self.value) |value| {
            try out.writeAll("\"value\": ");

            try value.toJson(value, out);

            try out.writeAll(", ");
        }

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        // $test#X is a test block, only render its value
        if (std.mem.startsWith(u8, self.name.lexeme, "$test")) {
            std.debug.assert(self.value != null);

            try self.value.?.render(self.value.?, out, depth);

            return;
        }

        if (!self.expression) {
            try out.writeByteNTimes(' ', depth * 4);
        }

        if (self.constant) {
            try out.writeAll("const ");
        }

        try self.type_def.toStringUnqualified(out);
        try out.print(" {s}", .{self.name.lexeme});

        if (self.value) |value| {
            try out.writeAll(" = ");
            try value.render(value, out, depth);
        }

        if (!self.expression) {
            try out.writeAll(";\n");
        }
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .VarDeclaration) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const EnumNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Enum,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    slot: usize,
    cases: std.ArrayList(*ParseNode),
    picked: std.ArrayList(bool),
    case_type_picked: bool,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        const enum_type = node.type_def.?.resolved_type.?.Enum.enum_type;

        if (enum_type.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(enum_type.resolved_type.?.Placeholder);

            return null;
        }

        switch (enum_type.def_type) {
            .String, .Integer, .Float => {},
            else => {
                codegen.reporter.reportErrorAt(.syntax, node.location, "Type not allowed as enum value");
                return null;
            },
        }

        try codegen.emitCodeArg(self.node.location, .OP_ENUM, try codegen.makeConstant(node.type_def.?.toValue()));
        try codegen.emitCodeArg(self.node.location, .OP_DEFINE_GLOBAL, @intCast(self.slot));

        try codegen.emitCodeArg(self.node.location, .OP_GET_GLOBAL, @intCast(self.slot));

        for (self.cases.items) |case| {
            if (case.type_def == null or case.type_def.?.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(case.type_def.?.resolved_type.?.Placeholder);
            } else if (!((try enum_type.toInstance(codegen.gc.allocator, &codegen.gc.type_registry))).eql(case.type_def.?)) {
                codegen.reporter.reportTypeCheck(
                    .enum_case_type,
                    node.location,
                    (try enum_type.toInstance(codegen.gc.allocator, &codegen.gc.type_registry)),
                    case.location,
                    case.type_def.?,
                    "Bad enum case type",
                );
            }

            _ = try case.toByteCode(case, codegen, breaks);

            try codegen.emitOpCode(self.node.location, .OP_ENUM_CASE);
        }

        try codegen.emitOpCode(self.node.location, .OP_POP);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Enum\", \"cases\": [");

        for (self.cases.items, 0..) |case, i| {
            try case.toJson(case, out);
            if (i < self.cases.items.len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;
        const enum_def = node.type_def.?.resolved_type.?.Enum;

        try out.writeByteNTimes(' ', depth * 4);

        try out.writeAll("enum");
        if (self.case_type_picked) {
            try out.writeAll("(");
            try enum_def.enum_type.toStringUnqualified(out);
            try out.writeAll(")");
        }
        try out.print(" {s} {{\n", .{enum_def.name.string});
        for (enum_def.cases.items, 0..) |case, i| {
            try out.writeByteNTimes(' ', (depth + 1) * 4);
            try out.writeAll(case);

            if (self.picked.items[i]) {
                try out.writeAll(" = ");
                try self.cases.items[i].render(self.cases.items[i], out, depth + 1);
            }

            try out.writeAll(",\n");
        }

        try out.writeByteNTimes(' ', depth * 4);
        try out.writeAll("}\n");
    }

    pub fn init(allocator: Allocator) Self {
        return Self{
            .cases = std.ArrayList(*ParseNode).init(allocator),
            .picked = std.ArrayList(bool).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.cases.deinit();
        self.picked.deinit();
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Enum) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ThrowNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Throw,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    error_value: *ParseNode,
    unconditional: bool,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        if (self.unconditional) {
            codegen.current.?.return_emitted = true;
        }

        assert(self.error_value.type_def != null);
        if (self.error_value.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.error_value.type_def.?.resolved_type.?.Placeholder);
        } else {
            const current_error_types = codegen.current.?.function.?.type_def.resolved_type.?.Function.error_types;
            if (current_error_types != null) {
                var found_match = false;
                for (current_error_types.?) |error_type| {
                    if (error_type.eql(self.error_value.type_def.?)) {
                        found_match = true;
                        break;
                    }
                }

                if (!found_match) {
                    if (codegen.current.?.try_should_handle != null) {
                        // In a try catch remember to check that we handle that error when finishing parsing the try-catch
                        try codegen.current.?.try_should_handle.?.put(self.error_value.type_def.?, node.location);
                    } else {
                        // Not in a try-catch and function signature does not expect this error type
                        const error_str = try self.error_value.type_def.?.toStringAlloc(codegen.gc.allocator);
                        defer error_str.deinit();

                        codegen.reporter.reportErrorFmt(
                            .unexpected_error_type,
                            node.location,
                            "Error type `{s}` not expected",
                            .{error_str.items},
                        );
                    }
                }
            }
        }

        _ = try self.error_value.toByteCode(self.error_value, codegen, breaks);

        try codegen.emitOpCode(self.node.location, .OP_THROW);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Throw\", \"error_value\": ");

        try self.error_value.toJson(self.error_value, out);

        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);
        try out.writeAll("throw ");
        try self.error_value.render(self.error_value, out, depth);
        try out.writeAll(";\n");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Throw) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const BreakNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Break,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        assert(breaks != null);

        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        // Close scope(s), then jump
        try node.endScope(codegen);
        try breaks.?.append(try codegen.emitJump(node.location, .OP_JUMP));

        // TODO: not sur if this makes sense here
        try node.patchOptJumps(codegen);

        return null;
    }

    fn stringify(_: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        try out.writeAll("{\"node\": \"Break\" }");
    }

    fn render(_: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        try out.writeByteNTimes(' ', depth * 4);
        try out.writeAll("break;\n");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Break) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ContinueNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Continue,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        assert(breaks != null);
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        // Close scope(s), then jump
        try node.endScope(codegen);
        try breaks.?.append(try codegen.emitJump(node.location, .OP_LOOP));

        // TODO: not sur if this makes sense here
        try node.patchOptJumps(codegen);

        return null;
    }

    fn stringify(_: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        try out.writeAll("{\"node\": \"Continue\" }");
    }

    fn render(_: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        try out.writeByteNTimes(' ', depth * 4);
        try out.writeAll("continue;\n");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Continue) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const InlineIfNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .InlineIf,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    condition: *ParseNode,
    body: *ParseNode,
    else_branch: *ParseNode,

    fn constant(nodePtr: *anyopaque) bool {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        return self.condition.isConstant(self.condition) and self.body.isConstant(self.body) and self.else_branch.isConstant(self.else_branch);
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.isConstant(node)) {
            const self = Self.cast(node).?;

            if ((try self.condition.toValue(self.condition, gc)).boolean()) {
                return self.body.toValue(self.body, gc);
            } else {
                return self.else_branch.toValue(self.else_branch, gc);
            }
        }

        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        // Type checking
        if (self.condition.type_def == null or self.condition.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.condition.type_def.?.resolved_type.?.Placeholder);
        }

        if (self.body.type_def == null or self.body.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.body.type_def.?.resolved_type.?.Placeholder);
        }

        if (self.else_branch.type_def == null or self.else_branch.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.else_branch.type_def.?.resolved_type.?.Placeholder);
        }

        // Both should have same type
        if (!node.type_def.?.eql(self.body.type_def.?)) {
            codegen.reporter.reportTypeCheck(
                .inline_if_body_type,
                node.location,
                node.type_def.?,
                self.body.location,
                self.body.type_def.?,
                "Inline if body type not matching",
            );
        }

        if (!node.type_def.?.eql(self.else_branch.type_def.?)) {
            codegen.reporter.reportTypeCheck(
                .inline_if_else_type,
                node.location,
                node.type_def.?,
                self.else_branch.location,
                self.else_branch.type_def.?,
                "Inline if else type not matching",
            );
        }

        // If condition is constant only generate appropriate branch
        if (self.condition.isConstant(self.condition)) {
            const condition = try self.condition.toValue(self.condition, codegen.gc);

            if (condition.boolean()) {
                _ = try self.body.toByteCode(self.body, codegen, breaks);
            } else {
                _ = try self.else_branch.toByteCode(self.else_branch, codegen, breaks);
            }

            try node.patchOptJumps(codegen);
            try node.endScope(codegen);

            return null;
        }

        // Generate the if/else
        _ = try self.condition.toByteCode(self.condition, codegen, breaks);

        const then_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
        try codegen.emitOpCode(self.node.location, .OP_POP);

        _ = try self.body.toByteCode(self.body, codegen, breaks);

        const else_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP);

        codegen.patchJump(then_jump);
        try codegen.emitOpCode(self.node.location, .OP_POP);

        _ = try self.else_branch.toByteCode(self.else_branch, codegen, breaks);
        codegen.patchJump(else_jump);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"InlineIf\", \"condition\": ");

        try self.condition.toJson(self.condition, out);

        try out.writeAll(", \"body\": ");

        try self.body.toJson(self.body, out);

        try out.writeAll(", \"else\": ");
        try self.else_branch.toJson(self.else_branch, out);

        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeAll("if (");
        try self.condition.render(self.condition, out, depth);
        try out.writeAll(") ");
        try self.body.render(self.body, out, depth + 1);
        try out.writeAll(" else ");
        try self.else_branch.render(self.else_branch, out, depth + 1);
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .InlineIf) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const IfNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .If,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    condition: *ParseNode,
    unwrapped_identifier: ?Token,
    casted_type: ?*ObjTypeDef,
    body: *ParseNode,
    else_branch: ?*ParseNode = null,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        if (self.condition.type_def == null or self.condition.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.condition.type_def.?.resolved_type.?.Placeholder);
        }

        if (self.unwrapped_identifier != null) {
            if (!self.condition.type_def.?.optional) {
                codegen.reporter.reportErrorAt(
                    .optional,
                    self.condition.location,
                    "Expected optional",
                );
            }
        } else if (self.casted_type == null) {
            if (self.condition.type_def.?.def_type != .Bool) {
                codegen.reporter.reportErrorAt(
                    .if_condition_type,
                    self.condition.location,
                    "`if` condition must be bool",
                );
            }
        }

        // If condition is a constant expression, no need to generate branches
        if (self.condition.isConstant(self.condition) and self.unwrapped_identifier == null and self.casted_type == null) {
            const condition = try self.condition.toValue(self.condition, codegen.gc);

            if (condition.boolean()) {
                _ = try self.body.toByteCode(self.body, codegen, breaks);
            } else if (self.else_branch) |else_branch| {
                _ = try else_branch.toByteCode(else_branch, codegen, breaks);
            }

            try node.patchOptJumps(codegen);
            try node.endScope(codegen);

            return null;
        }

        _ = try self.condition.toByteCode(self.condition, codegen, breaks);
        if (self.unwrapped_identifier != null) {
            try codegen.emitOpCode(self.condition.location, .OP_COPY);
            try codegen.emitOpCode(self.condition.location, .OP_NULL);
            try codegen.emitOpCode(self.condition.location, .OP_EQUAL);
            try codegen.emitOpCode(self.condition.location, .OP_NOT);
        } else if (self.casted_type) |casted_type| {
            try codegen.emitOpCode(self.condition.location, .OP_COPY);
            try codegen.emitConstant(self.condition.location, casted_type.toValue());
            try codegen.emitOpCode(self.condition.location, .OP_IS);
        }

        const then_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
        try codegen.emitOpCode(self.node.location, .OP_POP);

        _ = try self.body.toByteCode(self.body, codegen, breaks);

        const else_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP);

        codegen.patchJump(then_jump);
        if (self.unwrapped_identifier != null or self.casted_type != null) {
            // Since we did not enter the if block, we did not pop the unwrapped local
            try codegen.emitOpCode(self.node.location, .OP_POP);
        }
        try codegen.emitOpCode(self.node.location, .OP_POP);

        if (self.else_branch) |else_branch| {
            _ = try else_branch.toByteCode(else_branch, codegen, breaks);
        }

        codegen.patchJump(else_jump);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"If\", \"condition\": ");

        try self.condition.toJson(self.condition, out);

        try out.writeAll(", \"body\": ");

        try self.body.toJson(self.body, out);

        if (self.else_branch) |else_branch| {
            try out.writeAll(", \"else\": ");
            try else_branch.toJson(else_branch, out);
        }

        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);

        try out.writeAll("if (");
        try self.condition.render(self.condition, out, depth);
        if (self.unwrapped_identifier) |unwrapped_identifier| {
            try out.print(" -> {s}", .{unwrapped_identifier.lexeme});
        } else if (self.casted_type) |casted_type| {
            try out.writeAll(" -> ");
            try casted_type.toStringUnqualified(out);
        }

        try out.writeAll(") {\n");
        try self.body.render(self.body, out, depth + 1);
        try out.writeByteNTimes(' ', depth * 4);
        try out.writeAll("}");

        if (self.else_branch) |else_branch| {
            try out.writeAll(" else ");

            if (else_branch.node_type != .If) {
                try out.writeAll("{\n");
                try else_branch.render(else_branch, out, depth + 1);
                try out.writeByteNTimes(' ', depth * 4);
                try out.writeAll("}");
            } else {
                try else_branch.render(else_branch, out, depth);
            }
        }

        try out.writeAll("\n\n");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .If) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ReturnNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Return,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    value: ?*ParseNode,
    unconditional: bool,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        if (self.unconditional) {
            codegen.current.?.return_emitted = true;
        }

        if (self.value) |value| {
            if (value.type_def == null) {
                codegen.reporter.reportErrorAt(
                    .undefined,
                    value.location,
                    "Unknown type.",
                );
            } else if (value.type_def.?.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(value.type_def.?.resolved_type.?.Placeholder);
            } else if (!codegen.current.?.function.?.type_def.resolved_type.?.Function.return_type.eql(value.type_def.?)) {
                codegen.reporter.reportTypeCheck(
                    .return_type,
                    codegen.current.?.function_node.node.location,
                    codegen.current.?.function.?.type_def.resolved_type.?.Function.return_type,
                    value.location,
                    value.type_def.?,
                    "Return value",
                );
            }

            _ = try value.toByteCode(value, codegen, breaks);
        } else {
            try codegen.emitOpCode(self.node.location, .OP_VOID);
        }

        try codegen.emitOpCode(self.node.location, .OP_RETURN);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Return\", ");

        if (self.value) |value| {
            try out.writeAll("\"value\": ");
            try value.toJson(value, out);
            try out.writeAll(", ");
        }

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);

        try out.writeAll("return");
        if (self.value) |value| {
            try out.writeAll(" ");
            try value.render(value, out, depth);
        }

        try out.writeAll(";\n");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Return) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ForNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .For,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    init_declarations: std.ArrayList(*VarDeclarationNode),
    condition: *ParseNode,
    post_loop: std.ArrayList(*ParseNode),
    body: *ParseNode,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        if (self.condition.isConstant(self.condition) and !(try self.condition.toValue(self.condition, codegen.gc)).boolean()) {
            try node.patchOptJumps(codegen);

            return null;
        }

        for (self.init_declarations.items) |var_declaration| {
            _ = try var_declaration.node.toByteCode(&var_declaration.node, codegen, _breaks);
        }

        const loop_start: usize = codegen.currentCode();

        if (self.condition.type_def == null or self.condition.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.condition.type_def.?.resolved_type.?.Placeholder);
        }

        if (self.condition.type_def.?.def_type != .Bool) {
            codegen.reporter.reportErrorAt(
                .for_condition_type,
                self.condition.location,
                "`for` condition must be bool",
            );
        }

        _ = try self.condition.toByteCode(self.condition, codegen, _breaks);

        const exit_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
        try codegen.emitOpCode(self.node.location, .OP_POP); // Pop condition

        // Jump over expressions which will be executed at end of loop
        // TODO: since we don't generate as we parse, we can get rid of this jump and just generate the post_loop later
        var body_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP);

        const expr_loop: usize = codegen.currentCode();
        for (self.post_loop.items) |expr| {
            if (expr.type_def == null or expr.type_def.?.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(expr.type_def.?.resolved_type.?.Placeholder);
            }

            _ = try expr.toByteCode(expr, codegen, _breaks);
            try codegen.emitOpCode(expr.location, .OP_POP);
        }

        try codegen.emitLoop(self.node.location, loop_start);

        codegen.patchJump(body_jump);

        var breaks: std.ArrayList(usize) = std.ArrayList(usize).init(codegen.gc.allocator);
        defer breaks.deinit();

        _ = try self.body.toByteCode(self.body, codegen, &breaks);

        try codegen.emitLoop(self.node.location, expr_loop);

        codegen.patchJump(exit_jump);

        try codegen.emitOpCode(self.node.location, .OP_POP); // Pop condition

        // Patch breaks
        for (breaks.items) |jump| {
            try codegen.patchJumpOrLoop(jump, loop_start);
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"For\", \"init_declarations\": [");

        for (self.init_declarations.items, 0..) |var_declaration, i| {
            try var_declaration.node.toJson(&var_declaration.node, out);

            if (i < self.init_declarations.items.len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], \"condition\": ");

        try self.condition.toJson(self.condition, out);

        try out.writeAll(", \"post_loop\": [");

        for (self.post_loop.items, 0..) |expression, i| {
            try expression.toJson(expression, out);

            if (i < self.post_loop.items.len - 1) {
                try out.writeAll(", ");
            }
        }

        try out.writeAll("], \"body\": ");

        try self.body.toJson(self.body, out);

        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);

        try out.writeAll("for (");
        for (self.init_declarations.items, 0..) |decl, i| {
            try decl.node.render(&decl.node, out, depth);
            if (i < self.init_declarations.items.len - 1) {
                try out.writeAll(", ");
            }
        }
        try out.writeAll("; ");

        try self.condition.render(self.condition, out, depth);
        try out.writeAll("; ");

        for (self.post_loop.items, 0..) |expr, i| {
            try expr.render(expr, out, depth);

            if (i < self.post_loop.items.len - 1) {
                try out.writeAll(", ");
            }
        }

        try out.writeAll(") {\n");

        try self.body.render(self.body, out, depth + 1);

        try out.writeByteNTimes(' ', depth * 4);
        try out.writeAll("}\n");
    }

    pub fn init(allocator: Allocator) Self {
        return Self{
            .init_declarations = std.ArrayList(*VarDeclarationNode).init(allocator),
            .post_loop = std.ArrayList(*ParseNode).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.init_declarations.deinit();
        self.post_loop.deinit();
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .For) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ForEachNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .ForEach,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    key_omitted: bool,
    key: *VarDeclarationNode,
    value: *VarDeclarationNode,
    iterable: *ParseNode,
    block: *ParseNode,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        // Type checking
        if (self.iterable.type_def == null or self.iterable.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.iterable.type_def.?.resolved_type.?.Placeholder);
        } else {
            if (!self.key_omitted) {
                if (self.key.type_def.def_type == .Placeholder) {
                    codegen.reporter.reportPlaceholder(self.key.type_def.resolved_type.?.Placeholder);
                }

                switch (self.iterable.type_def.?.def_type) {
                    .String, .List => {
                        if (self.key.type_def.def_type != .Integer) {
                            codegen.reporter.reportErrorAt(
                                .foreach_key_type,
                                self.key.node.location,
                                "Expected `int`.",
                            );
                        }
                    },
                    .Map => {
                        if (!self.iterable.type_def.?.resolved_type.?.Map.key_type.strictEql(self.key.type_def)) {
                            codegen.reporter.reportTypeCheck(
                                .foreach_key_type,
                                self.iterable.location,
                                self.iterable.type_def.?.resolved_type.?.Map.key_type,
                                self.key.node.location,
                                self.key.type_def,
                                "Bad key type",
                            );
                        }
                    },
                    .Enum => codegen.reporter.reportErrorAt(
                        .foreach_key_type,
                        self.key.node.location,
                        "No key available when iterating over enum.",
                    ),
                    else => codegen.reporter.reportErrorAt(
                        .foreach_iterable,
                        self.iterable.location,
                        "Not iterable.",
                    ),
                }
            } else {
                // Key was omitted, put the correct type in the key var declation to avoid raising errors
                switch (self.iterable.type_def.?.def_type) {
                    .Map => self.key.type_def = self.iterable.type_def.?.resolved_type.?.Map.key_type,
                    .String, .List => self.key.type_def = try codegen.gc.type_registry.getTypeDef(.{ .def_type = .Integer }),
                    else => {},
                }
            }

            if (self.value.type_def.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(self.value.type_def.resolved_type.?.Placeholder);
            }

            switch (self.iterable.type_def.?.def_type) {
                .Map => {
                    if (!self.iterable.type_def.?.resolved_type.?.Map.value_type.strictEql(self.value.type_def)) {
                        codegen.reporter.reportTypeCheck(
                            .foreach_value_type,
                            self.iterable.location,
                            self.iterable.type_def.?.resolved_type.?.Map.value_type,
                            self.value.node.location,
                            self.value.type_def,
                            "Bad value type",
                        );
                    }
                },
                .List => {
                    if (!self.iterable.type_def.?.resolved_type.?.List.item_type.strictEql(self.value.type_def)) {
                        codegen.reporter.reportTypeCheck(
                            .foreach_value_type,
                            self.iterable.location,
                            self.iterable.type_def.?.resolved_type.?.List.item_type,
                            self.value.node.location,
                            self.value.type_def,
                            "Bad value type",
                        );
                    }
                },
                .String => {
                    if (self.value.type_def.def_type != .String) {
                        codegen.reporter.reportErrorAt(
                            .foreach_value_type,
                            self.value.node.location,
                            "Expected `str`.",
                        );
                    }
                },
                .Enum => {
                    const iterable_type = try self.iterable.type_def.?.toInstance(codegen.gc.allocator, &codegen.gc.type_registry);
                    if (!iterable_type.strictEql(self.value.type_def)) {
                        codegen.reporter.reportTypeCheck(
                            .foreach_value_type,
                            self.iterable.location,
                            iterable_type,
                            self.value.node.location,
                            self.value.type_def,
                            "Bad value type",
                        );
                    }
                },
                .Fiber => {
                    const iterable_type = try self.iterable.type_def.?.resolved_type.?.Fiber.yield_type.toInstance(
                        codegen.gc.allocator,
                        &codegen.gc.type_registry,
                    );
                    if (!iterable_type.strictEql(self.value.type_def)) {
                        codegen.reporter.reportTypeCheck(
                            .foreach_value_type,
                            self.iterable.location,
                            iterable_type,
                            self.value.node.location,
                            self.value.type_def,
                            "Bad value type",
                        );
                    }
                },
                else => codegen.reporter.reportErrorAt(
                    .foreach_iterable,
                    self.iterable.location,
                    "Not iterable.",
                ),
            }
        }

        // If iterable constant and empty, skip the node
        if (self.iterable.isConstant(self.iterable)) {
            const iterable = (try self.iterable.toValue(self.iterable, codegen.gc)).obj();

            if (switch (iterable.obj_type) {
                .List => ObjList.cast(iterable).?.items.items.len == 0,
                .Map => ObjMap.cast(iterable).?.map.count() == 0,
                .String => ObjString.cast(iterable).?.string.len == 0,
                .Enum => ObjEnum.cast(iterable).?.cases.items.len == 0,
                else => unreachable,
            }) {
                try node.patchOptJumps(codegen);
                return null;
            }
        }

        _ = try self.key.node.toByteCode(&self.key.node, codegen, _breaks);
        _ = try self.value.node.toByteCode(&self.value.node, codegen, _breaks);

        _ = try self.iterable.toByteCode(self.iterable, codegen, _breaks);

        const loop_start: usize = codegen.currentCode();

        // Calls `next` and update key and value locals
        try codegen.emitOpCode(
            self.node.location,
            switch (self.iterable.type_def.?.def_type) {
                .String => .OP_STRING_FOREACH,
                .List => .OP_LIST_FOREACH,
                .Enum => .OP_ENUM_FOREACH,
                .Map => .OP_MAP_FOREACH,
                .Fiber => .OP_FIBER_FOREACH,
                else => unreachable,
            },
        );

        // If next key is null, exit loop
        try codegen.emitCodeArg(
            self.node.location,
            .OP_GET_LOCAL,
            @as(
                u24,
                @intCast(
                    switch (self.iterable.type_def.?.def_type) {
                        .String, .List, .Map => self.key.slot,
                        else => self.value.slot,
                    },
                ),
            ),
        );
        try codegen.emitOpCode(self.node.location, .OP_NULL);
        try codegen.emitOpCode(self.node.location, .OP_EQUAL);
        try codegen.emitOpCode(self.node.location, .OP_NOT);
        const exit_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
        try codegen.emitOpCode(self.node.location, .OP_POP); // Pop condition result

        var breaks: std.ArrayList(usize) = std.ArrayList(usize).init(codegen.gc.allocator);
        defer breaks.deinit();

        _ = try self.block.toByteCode(self.block, codegen, &breaks);

        try codegen.emitLoop(self.node.location, loop_start);

        // Patch condition jump
        codegen.patchJump(exit_jump);

        try codegen.emitOpCode(self.node.location, .OP_POP); // Pop condition result

        // Patch breaks
        for (breaks.items) |jump| {
            try codegen.patchJumpOrLoop(jump, loop_start);
        }

        try node.patchOptJumps(codegen);
        // Should have key, [value,] iterable to pop
        assert(node.ends_scope != null and node.ends_scope.?.items.len == 3);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"ForEach\", ");

        if (!self.key_omitted) {
            try out.writeAll("\"key\": ");
            try self.key.node.toJson(&self.key.node, out);
            try out.writeAll(", ");
        }

        try out.writeAll("\"value\": ");

        try self.value.node.toJson(&self.value.node, out);

        try out.writeAll(", \"iterable\": ");

        try self.iterable.toJson(self.iterable, out);

        try out.writeAll(", \"block\": ");

        try self.block.toJson(self.block, out);

        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);

        try out.writeAll("foreach (");
        if (!self.key_omitted) {
            try self.key.node.render(&self.key.node, out, depth);
            try out.writeAll(", ");
        }
        try self.value.node.render(&self.value.node, out, depth);
        try out.writeAll(" in ");
        try self.iterable.render(self.iterable, out, depth);
        try out.writeAll(") {\n");
        try self.block.render(self.block, out, depth + 1);
        try out.writeByteNTimes(' ', depth * 4);
        try out.writeAll("}\n");
    }

    pub fn init(allocator: Allocator) Self {
        return Self{
            .init_expression = std.ArrayList(*ParseNode).init(allocator),
            .post_loop = std.ArrayList(*ParseNode).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.init_expressions.deinit();
        self.post_loop.deinit();
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .ForEach) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const WhileNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .While,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    condition: *ParseNode,
    block: *ParseNode,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        // If condition constant and false, skip the node
        if (self.condition.isConstant(self.condition) and !(try self.condition.toValue(self.condition, codegen.gc)).boolean()) {
            try node.patchOptJumps(codegen);
            try node.endScope(codegen);

            return null;
        }

        const loop_start: usize = codegen.currentCode();

        if (self.condition.type_def == null or self.condition.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.condition.type_def.?.resolved_type.?.Placeholder);
        }

        if (self.condition.type_def.?.def_type != .Bool) {
            codegen.reporter.reportErrorAt(
                .while_condition_type,
                self.condition.location,
                "`while` condition must be bool",
            );
        }

        _ = try self.condition.toByteCode(self.condition, codegen, _breaks);

        const exit_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
        try codegen.emitOpCode(self.node.location, .OP_POP);

        var breaks: std.ArrayList(usize) = std.ArrayList(usize).init(codegen.gc.allocator);
        defer breaks.deinit();

        _ = try self.block.toByteCode(self.block, codegen, &breaks);

        try codegen.emitLoop(self.node.location, loop_start);
        codegen.patchJump(exit_jump);

        try codegen.emitOpCode(self.node.location, .OP_POP); // Pop condition (is not necessary if broke out of the loop)

        // Patch breaks
        for (breaks.items) |jump| {
            try codegen.patchJumpOrLoop(jump, loop_start);
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"While\", \"condition\": ");

        try self.condition.toJson(self.condition, out);

        try out.writeAll(", \"block\": ");

        try self.block.toJson(self.block, out);

        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);

        try out.writeAll("while (");
        try self.condition.render(self.condition, out, depth);
        try out.writeAll(") {\n");
        try self.block.render(self.block, out, depth + 1);
        try out.writeByteNTimes(' ', depth * 4);
        try out.writeAll("}\n");
    }

    pub fn init(allocator: Allocator) Self {
        return Self{
            .init_expression = std.ArrayList(*ParseNode).init(allocator),
            .post_loop = std.ArrayList(*ParseNode).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.init_expressions.deinit();
        self.post_loop.deinit();
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .While) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const DoUntilNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .DoUntil,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    condition: *ParseNode,
    block: *ParseNode,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        const loop_start: usize = codegen.currentCode();

        var breaks: std.ArrayList(usize) = std.ArrayList(usize).init(codegen.gc.allocator);
        defer breaks.deinit();

        _ = try self.block.toByteCode(self.block, codegen, &breaks);

        if (self.condition.type_def == null or self.condition.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(self.condition.type_def.?.resolved_type.?.Placeholder);
        }

        if (self.condition.type_def.?.def_type != .Bool) {
            codegen.reporter.reportErrorAt(
                .do_condition_type,
                self.condition.location,
                "`do` condition must be bool",
            );
        }

        _ = try self.condition.toByteCode(self.condition, codegen, &breaks);

        try codegen.emitOpCode(self.node.location, .OP_NOT);
        const exit_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
        try codegen.emitOpCode(self.node.location, .OP_POP);

        try codegen.emitLoop(self.node.location, loop_start);
        codegen.patchJump(exit_jump);

        try codegen.emitOpCode(self.node.location, .OP_POP); // Pop condition

        // Patch breaks
        for (breaks.items) |jump| {
            try codegen.patchJumpOrLoop(jump, loop_start);
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"DoUntil\", \"condition\": ");

        try self.condition.toJson(self.condition, out);

        try out.writeAll(", \"block\": ");

        try self.block.toJson(self.block, out);

        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);

        try out.writeAll("do {\n");
        try self.block.render(self.block, out, depth + 1);
        try out.writeAll("} until (");
        try self.condition.render(self.condition, out, depth);
        try out.writeAll(")\n");
    }

    pub fn init(allocator: Allocator) Self {
        return Self{
            .init_expression = std.ArrayList(*ParseNode).init(allocator),
            .post_loop = std.ArrayList(*ParseNode).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.init_expressions.deinit();
        self.post_loop.deinit();
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .DoUntil) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const BlockNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Block,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    statements: std.ArrayList(*ParseNode),

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        for (self.statements.items) |statement| {
            _ = try statement.toByteCode(statement, codegen, breaks);
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Block\", \"statements\": [");

        for (self.statements.items, 0..) |statement, i| {
            try statement.toJson(statement, out);

            if (i < self.statements.items.len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        for (self.statements.items) |statement| {
            try statement.render(statement, out, depth);
        }
    }

    pub fn init(allocator: Allocator) Self {
        return Self{
            .statements = std.ArrayList(*ParseNode).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.statements.deinit();
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Block) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const DotNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Dot,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    callee: *ParseNode,
    identifier: Token,
    member_type_def: ?*ObjTypeDef = null,
    value: ?*ParseNode = null,
    call: ?*CallNode = null,
    enum_index: ?usize = null,

    fn constant(_: *anyopaque) bool {
        // TODO: should be true, but we have to evaluate a constant call
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        _ = try self.callee.toByteCode(self.callee, codegen, breaks);

        const callee_type = self.callee.type_def.?;

        if (callee_type.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(callee_type.resolved_type.?.Placeholder);
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
            .ForeignStruct,
            => {},
            else => codegen.reporter.reportErrorAt(
                .field_access,
                node.location,
                "Doesn't have field access",
            ),
        }

        if (callee_type.optional) {
            codegen.reporter.reportErrorAt(
                .field_access,
                node.location,
                "Optional doesn't have field access",
            );
        }

        var get_code: ?OpCode = switch (callee_type.def_type) {
            .Object => .OP_GET_OBJECT_PROPERTY,
            .ObjectInstance, .ProtocolInstance => .OP_GET_INSTANCE_PROPERTY,
            .ForeignStruct => .OP_GET_FSTRUCT_INSTANCE_PROPERTY,
            .List => .OP_GET_LIST_PROPERTY,
            .Map => .OP_GET_MAP_PROPERTY,
            .String => .OP_GET_STRING_PROPERTY,
            .Pattern => .OP_GET_PATTERN_PROPERTY,
            .Fiber => .OP_GET_FIBER_PROPERTY,
            else => null,
        };

        switch (callee_type.def_type) {
            .Fiber, .Pattern, .String => {
                if (self.call) |call_node| { // Call
                    try codegen.emitOpCode(self.node.location, .OP_COPY);
                    _ = try call_node.node.toByteCode(&call_node.node, codegen, breaks);
                } else { // Expression
                    assert(self.value == null);
                    try codegen.emitCodeArg(
                        self.node.location,
                        get_code.?,
                        try codegen.identifierConstant(self.identifier.lexeme),
                    );
                }
            },
            .ForeignStruct, .ObjectInstance, .Object => {
                if (self.value) |value| {
                    if (value.type_def == null or value.type_def.?.def_type == .Placeholder) {
                        codegen.reporter.reportPlaceholder(value.type_def.?.resolved_type.?.Placeholder);
                    }

                    _ = try value.toByteCode(value, codegen, breaks);

                    try codegen.emitCodeArg(
                        self.node.location,
                        switch (callee_type.def_type) {
                            .ObjectInstance => .OP_SET_INSTANCE_PROPERTY,
                            .ForeignStruct => .OP_SET_FSTRUCT_INSTANCE_PROPERTY,
                            else => .OP_SET_OBJECT_PROPERTY,
                        },
                        try codegen.identifierConstant(self.identifier.lexeme),
                    );
                } else if (self.call) |call| {
                    if (callee_type.def_type == .ForeignStruct) {
                        codegen.reporter.reportErrorAt(
                            .callable,
                            self.callee.location,
                            "Not callable",
                        );
                    }

                    // Static call
                    if (callee_type.def_type == .Object) {
                        try codegen.emitCodeArg(
                            node.location,
                            get_code.?,
                            try codegen.identifierConstant(self.identifier.lexeme),
                        );
                    }

                    _ = try call.node.toByteCode(&call.node, codegen, breaks);
                } else {
                    try codegen.emitCodeArg(
                        self.node.location,
                        get_code.?,
                        try codegen.identifierConstant(self.identifier.lexeme),
                    );
                }
            },
            .ProtocolInstance => {
                if (self.call) |call| {
                    _ = try call.node.toByteCode(&call.node, codegen, breaks);
                } else {
                    assert(self.value == null);
                    try codegen.emitCodeArg(
                        self.node.location,
                        get_code.?,
                        try codegen.identifierConstant(self.identifier.lexeme),
                    );
                }
            },
            .Enum => {
                try codegen.emitCodeArg(self.node.location, .OP_GET_ENUM_CASE, @intCast(self.enum_index.?));
            },
            .EnumInstance => {
                assert(std.mem.eql(u8, self.identifier.lexeme, "value"));

                try codegen.emitOpCode(self.node.location, .OP_GET_ENUM_CASE_VALUE);
            },
            .List, .Map => {
                if (self.call) |call| {
                    try codegen.emitOpCode(self.node.location, .OP_COPY);

                    _ = try call.node.toByteCode(&call.node, codegen, breaks);
                } else {
                    assert(self.value == null);
                    try codegen.emitCodeArg(
                        self.node.location,
                        get_code.?,
                        try codegen.identifierConstant(self.identifier.lexeme),
                    );
                }
            },
            else => unreachable,
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Dot\", \"callee\": ");

        try self.callee.toJson(self.callee, out);

        try out.print(", \"identifier\": \"{s}\", ", .{self.identifier.lexeme});

        if (self.value) |value| {
            try out.writeAll("\"value\": ");
            try value.toJson(value, out);
            try out.writeAll(", ");
        }

        if (self.call) |call| {
            try out.writeAll("\"call\": ");
            try call.toNode().toJson(call.toNode(), out);
            try out.writeAll(", ");
        }

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try self.callee.render(self.callee, out, depth);
        try out.print(".{s}", .{self.identifier.lexeme});

        if (self.value) |value| {
            try out.writeAll(" = ");
            try value.render(value, out, depth);
        } else if (self.call) |call| {
            try call.node.render(&call.node, out, depth);
        }
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .Dot) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ObjectInitNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .ObjectInit,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    object: ?*ParseNode, // Should mostly be a NamedVariableNode
    properties: std.StringArrayHashMap(*ParseNode),

    fn checkOmittedProperty(
        self: *Self,
        codegenPtr: *anyopaque,
        fields: std.StringArrayHashMap(*ObjTypeDef),
        fields_defaults: ?std.StringArrayHashMap(void),
        init_properties: std.StringHashMap(void),
    ) anyerror!void {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var it = fields.iterator();
        while (it.next()) |kv| {
            // If ommitted in initialization and doesn't have default value
            if (init_properties.get(kv.key_ptr.*) == null and (fields_defaults == null or fields_defaults.?.get(kv.key_ptr.*) == null)) {
                codegen.reporter.reportErrorFmt(
                    .property_not_initialized,
                    self.node.location,
                    "Property `{s}` was not initialized and has no default value",
                    .{kv.key_ptr.*},
                );
            }
        }
    }

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        if (self.object != null and self.object.?.type_def.?.def_type == .Object) {
            _ = try self.object.?.toByteCode(self.object.?, codegen, breaks);
        } else {
            // Anonymous object or struct, we push its type
            try codegen.emitCodeArg(
                node.location,
                .OP_CONSTANT,
                try codegen.makeConstant(node.type_def.?.toValue()),
            );
        }

        if (node.type_def == null or node.type_def.?.def_type == .Placeholder) {
            codegen.reporter.reportPlaceholder(node.type_def.?.resolved_type.?.Placeholder);
        } else if (node.type_def.?.def_type != .ObjectInstance and node.type_def.?.def_type != .ForeignStruct) {
            codegen.reporter.reportErrorAt(
                .expected_object,
                node.location,
                "Expected object or foreign struct.",
            );
        }

        try codegen.emitOpCode(
            self.node.location,
            if (node.type_def.?.def_type == .ObjectInstance)
                .OP_INSTANCE
            else
                .OP_FSTRUCT_INSTANCE,
        );

        const fields = if (node.type_def.?.def_type == .ObjectInstance)
            node.type_def.?.resolved_type.?.ObjectInstance.resolved_type.?.Object.fields
        else
            node.type_def.?.resolved_type.?.ForeignStruct.buzz_type;

        const location = if (node.type_def.?.def_type == .ObjectInstance)
            node.type_def.?.resolved_type.?.ObjectInstance.resolved_type.?.Object.location
        else
            node.type_def.?.resolved_type.?.ForeignStruct.location;

        const fields_location = if (node.type_def.?.def_type == .ObjectInstance)
            node.type_def.?.resolved_type.?.ObjectInstance.resolved_type.?.Object.fields_locations
        else
            null;

        // To keep track of what's been initialized or not by this statement
        var init_properties = std.StringHashMap(void).init(codegen.gc.allocator);
        defer init_properties.deinit();

        for (self.properties.keys()) |property_name| {
            const property_name_constant: u24 = try codegen.identifierConstant(property_name);
            const value = self.properties.get(property_name).?;

            if (fields.get(property_name)) |prop| {
                try codegen.emitCodeArg(self.node.location, .OP_COPY, 0); // Will be popped by OP_SET_PROPERTY

                if (value.type_def == null or value.type_def.?.def_type == .Placeholder) {
                    codegen.reporter.reportPlaceholder(value.type_def.?.resolved_type.?.Placeholder);
                } else if (!prop.eql(value.type_def.?)) {
                    if (BuildOptions.debug_placeholders) {
                        std.debug.print(
                            "prop {}({}), value {}({})\n",
                            .{
                                @intFromPtr(prop.resolved_type.?.ObjectInstance),
                                prop.optional,
                                @intFromPtr(value.type_def.?.resolved_type.?.ObjectInstance),
                                value.type_def.?.optional,
                            },
                        );
                    }
                    codegen.reporter.reportTypeCheck(
                        .property_type,
                        if (fields_location) |floc|
                            floc.get(property_name)
                        else
                            location,
                        prop,
                        value.location,
                        value.type_def.?,
                        "Wrong property type",
                    );
                }

                _ = try value.toByteCode(value, codegen, breaks);

                try init_properties.put(property_name, {});

                try codegen.emitCodeArg(
                    self.node.location,
                    if (node.type_def.?.def_type == .ObjectInstance)
                        .OP_SET_INSTANCE_PROPERTY
                    else
                        .OP_SET_FSTRUCT_INSTANCE_PROPERTY,
                    property_name_constant,
                );
                try codegen.emitOpCode(self.node.location, .OP_POP); // Pop property value
            } else {
                codegen.reporter.reportWithOrigin(
                    .property_does_not_exists,
                    node.location,
                    location,
                    "Property `{s}` does not exists",
                    .{property_name},
                    null,
                );
            }
        }

        // Did we initialized all properties without a default value?
        try self.checkOmittedProperty(
            codegen,
            fields,
            if (node.type_def.?.def_type == .ObjectInstance)
                node.type_def.?.resolved_type.?.ObjectInstance.resolved_type.?.Object.fields_defaults
            else
                null,
            init_properties,
        );

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"ObjectInit\", \"properties\": {");

        var it = self.properties.iterator();
        var first = true;
        while (it.next()) |entry| {
            if (!first) {
                try out.writeAll(",");
            }

            first = false;

            try out.print("\"{s}\": ", .{entry.key_ptr.*});

            try entry.value_ptr.*.toJson(entry.value_ptr.*, out);
        }

        try out.writeAll("}, \"object\": ");

        if (self.object) |object| {
            try object.toJson(object, out);
        } else {
            try out.writeAll("null");
        }
        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        if (self.object) |object| {
            try object.render(object, out, depth);
        } else {
            try out.writeAll(".");
        }
        try out.writeAll("{");

        if (self.properties.count() > 0) {
            try out.writeAll("\n");
        }

        var it = self.properties.iterator();
        while (it.next()) |kv| {
            try out.writeByteNTimes(' ', (depth + 1) * 4);
            try out.print("{s} = ", .{kv.key_ptr.*});
            try kv.value_ptr.*.render(kv.value_ptr.*, out, depth + 1);
            try out.writeAll(",\n");
        }

        if (self.properties.count() > 0) {
            try out.writeByteNTimes(' ', depth * 4);
        }
        try out.writeAll("}");
    }

    pub fn init(allocator: Allocator, object: ?*ParseNode) Self {
        return Self{
            .object = object,
            .properties = std.StringArrayHashMap(*ParseNode).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.properties.deinit();
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.node_type != .ObjectInit) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ObjectDeclarationNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .ObjectDeclaration,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    slot: usize,
    // All properties and methods with preserved order for buzz --fmt
    fields: [][]const u8,
    methods: std.StringHashMap(*ParseNode),
    properties: std.StringHashMap(?*ParseNode),
    properties_type: std.StringHashMap(*ObjTypeDef),
    docblocks: std.StringHashMap(?Token),

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        const object_type = node.type_def.?;
        const object_def = object_type.resolved_type.?.Object;

        // Check object conforms to declared protocols
        var protocol_it = object_def.conforms_to.iterator();
        while (protocol_it.next()) |kv| {
            const protocol_type_def = kv.key_ptr.*;

            if (protocol_type_def.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(protocol_type_def.resolved_type.?.Placeholder);
            } else {
                const protocol_def = protocol_type_def.resolved_type.?.Protocol;

                var method_it = protocol_def.methods.iterator();
                while (method_it.next()) |mkv| {
                    if (self.methods.get(mkv.key_ptr.*)) |method| {
                        if (method.type_def.?.def_type == .Placeholder) {
                            codegen.reporter.reportPlaceholder(method.type_def.?.resolved_type.?.Placeholder);
                        } else if (!mkv.value_ptr.*.eql(method.type_def.?)) {
                            codegen.reporter.reportTypeCheck(
                                .protocol_conforming,
                                protocol_def.location,
                                mkv.value_ptr.*,
                                method.location,
                                method.type_def.?,
                                "Method not conforming to protocol",
                            );
                        }
                    } else {
                        codegen.reporter.reportWithOrigin(
                            .protocol_conforming,
                            self.node.location,
                            protocol_def.methods_locations.get(mkv.value_ptr.*.resolved_type.?.Function.name.string).?,
                            "Object declared as conforming to protocol `{s}` but doesn't implement method `{s}`",
                            .{
                                protocol_def.name.string,
                                mkv.value_ptr.*.resolved_type.?.Function.name.string,
                            },
                            null,
                        );
                    }
                }
            }
        }

        const name_constant = try codegen.makeConstant(object_def.name.toValue());
        const object_type_constant = try codegen.makeConstant(object_type.toValue());

        // Put  object on the stack and define global with it
        try codegen.emitCodeArg(self.node.location, .OP_OBJECT, name_constant);
        try codegen.emit(self.node.location, @intCast(object_type_constant));
        try codegen.emitCodeArg(self.node.location, .OP_DEFINE_GLOBAL, @intCast(self.slot));

        // Put the object on the stack to set its fields
        try codegen.emitCodeArg(self.node.location, .OP_GET_GLOBAL, @intCast(self.slot));

        // Methods
        var it = self.methods.iterator();
        while (it.next()) |kv| {
            const member_name = kv.key_ptr.*;
            const member = kv.value_ptr.*;
            const member_name_constant: u24 = try codegen.identifierConstant(member_name);

            if (member.type_def == null or member.type_def.?.def_type == .Placeholder) {
                codegen.reporter.reportPlaceholder(member.type_def.?.resolved_type.?.Placeholder);
            }

            // Enforce "collect" method signature
            if (std.mem.eql(u8, member_name, "collect")) {
                const collect_def = member.type_def.?.resolved_type.?.Function;

                // zig fmt: off
                if (collect_def.parameters.count() > 0
                    or collect_def.return_type.def_type != .Void
                    or collect_def.yield_type.def_type != .Void
                    or collect_def.error_types != null) {
                    // zig fmt: on
                    const collect_def_str = member.type_def.?.toStringAlloc(codegen.gc.allocator) catch @panic("Out of memory");
                    defer collect_def_str.deinit();
                    codegen.reporter.reportErrorFmt(
                        .collect_signature,
                        member.location,
                        "Expected `collect` method to be `fun collect() > void` got {s}",
                        .{
                            collect_def_str.items,
                        },
                    );
                }
            }

            // Enforce "toString" method signature
            if (std.mem.eql(u8, member_name, "toString")) {
                const tostring_def = member.type_def.?.resolved_type.?.Function;

                // zig fmt: off
                if (tostring_def.parameters.count() > 0
                    or tostring_def.return_type.def_type != .String
                    or tostring_def.yield_type.def_type != .Void
                    or tostring_def.error_types != null
                    or tostring_def.generic_types.count() > 0) {
                    // zig fmt: on
                    const tostring_def_str = member.type_def.?.toStringAlloc(codegen.gc.allocator) catch @panic("Out of memory");
                    defer tostring_def_str.deinit();
                    codegen.reporter.reportErrorFmt(
                        .tostring_signature,
                        member.location,
                        "Expected `toString` method to be `fun toString() > str` got {s}",
                        .{
                            tostring_def_str.items,
                        },
                    );
                }
            }

            const is_static = object_def.static_fields.get(member_name) != null;

            _ = try member.toByteCode(member, codegen, breaks);
            try codegen.emitCodeArg(self.node.location, if (is_static) .OP_PROPERTY else .OP_METHOD, member_name_constant);
        }

        // Properties
        var it2 = self.properties.iterator();
        while (it2.next()) |kv| {
            const member_name = kv.key_ptr.*;
            const member = kv.value_ptr.*;
            const member_name_constant: u24 = try codegen.identifierConstant(member_name);
            const is_static = object_def.static_fields.get(member_name) != null;
            const property_type = object_def.fields.get(member_name) orelse object_def.static_fields.get(member_name);

            assert(property_type != null);

            // Create property default value
            if (member) |default| {
                if (default.type_def == null or default.type_def.?.def_type == .Placeholder) {
                    codegen.reporter.reportPlaceholder(default.type_def.?.resolved_type.?.Placeholder);
                } else if (!property_type.?.eql(default.type_def.?)) {
                    codegen.reporter.reportTypeCheck(
                        .property_default_value,
                        object_def.location,
                        property_type.?,
                        default.location,
                        default.type_def.?,
                        "Wrong property default value type",
                    );
                }

                if (is_static) {
                    try codegen.emitOpCode(self.node.location, .OP_COPY);
                }

                _ = try default.toByteCode(default, codegen, breaks);

                // Create property default value
                if (is_static) {
                    try codegen.emitCodeArg(self.node.location, .OP_SET_OBJECT_PROPERTY, member_name_constant);
                    try codegen.emitOpCode(self.node.location, .OP_POP);
                } else {
                    try codegen.emitCodeArg(self.node.location, .OP_PROPERTY, member_name_constant);
                }
            }
        }

        // Pop object
        try codegen.emitOpCode(self.node.location, .OP_POP);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"ObjectDeclaration\", \"methods\": {");

        var it = self.methods.iterator();
        var i: usize = 0;
        while (it.next()) |kv| {
            const member = kv.value_ptr.*;

            try out.print("\"{s}\": ", .{kv.key_ptr.*});

            try member.toJson(member, out);

            if (i < self.methods.count() - 1) {
                try out.writeAll(",");
            }

            i += 1;
        }

        try out.writeAll("}, \"members\": {");

        var it2 = self.properties_type.iterator();
        i = 0;
        while (it2.next()) |kv| {
            try out.print(
                "\"{s}\": {{\"type_def\": \"",
                .{
                    kv.key_ptr.*,
                },
            );

            try kv.value_ptr.*.toString(out);

            try out.print("\"", .{});

            if (self.docblocks.get(kv.key_ptr.*).?) |docblock| {
                var escaped = try escape(global_allocator, docblock.literal_string orelse "");
                defer escaped.deinit();
                try out.print(", \"docblock\": \"{s}\"}}", .{escaped.items});
            } else {
                try out.print("}}", .{});
            }

            if (i < self.properties_type.count() - 1) {
                try out.writeAll(",");
            }

            i += 1;
        }

        try out.writeAll("}, ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;
        const obj_def = node.type_def.?.resolved_type.?.Object;

        try out.writeByteNTimes(' ', depth * 4);

        try out.writeAll("\nobject");
        if (obj_def.conforms_to.count() > 0) {
            try out.writeAll("(");
            var it = obj_def.conforms_to.iterator();
            var i: usize = 0;
            while (it.next()) |kv| : (i += 1) {
                try kv.key_ptr.*.toStringUnqualified(out);

                if (i < obj_def.conforms_to.count() - 1) {
                    try out.writeAll(", ");
                }
            }
            try out.writeAll(") ");
        }

        try out.print(" {s} {{", .{obj_def.name.string});

        if (self.fields.len > 0) {
            try out.writeAll("\n");
        }

        for (self.fields) |field_name| {
            const method = self.methods.get(field_name);
            const property = obj_def.fields.get(field_name);

            try out.writeByteNTimes(' ', (depth + 1) * 4);

            if (method == null) {
                try property.?.toStringUnqualified(out);
                try out.print(" {s}", .{field_name});

                if (self.properties.get(field_name)) |default_expr| {
                    if (default_expr) |expr| {
                        try out.writeAll(" = ");
                        try expr.render(expr, out, depth + 1);
                    }
                }

                try out.writeAll(",");
            } else {
                try method.?.render(method.?, out, depth + 1);
            }

            try out.writeAll("\n");
        }

        try out.writeByteNTimes(' ', depth * 4);
        try out.writeAll("}\n");
    }

    pub fn init(allocator: Allocator) Self {
        return Self{
            .properties = std.StringHashMap(*ParseNode).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.properties.deinit();
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.node_type != .ObjectDeclaration) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ProtocolDeclarationNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .ProtocolDeclaration,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    slot: usize,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        const self = Self.cast(node).?;

        try codegen.emitConstant(node.location, node.type_def.?.toValue());
        try codegen.emitCodeArg(node.location, .OP_DEFINE_GLOBAL, @intCast(self.slot));

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        try out.writeAll("{\"node\": \"ProtocolDeclaration\", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        const protocol_def = node.type_def.?.resolved_type.?.Protocol;

        try out.writeByteNTimes(' ', depth * 4);

        try out.print("protocol {s} {{\n", .{protocol_def.name.string});
        var it = protocol_def.methods.iterator();
        while (it.next()) |kv| {
            try out.writeByteNTimes(' ', (depth + 1) * 4);
            try kv.value_ptr.*.toStringUnqualified(out);
            try out.writeAll(";\n");
        }
        try out.writeByteNTimes(' ', depth * 4);
        try out.writeAll("}\n");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.node_type != .ProtocolDeclaration) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ExportNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Export,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    identifier: ?Token = null,
    alias: ?Token = null,

    declaration: ?*ParseNode = null,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        const codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        if (self.declaration) |decl| {
            _ = try decl.toByteCode(decl, codegen, breaks);
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Export\", ", .{});

        if (self.identifier) |identifier| {
            try out.print("\"identifier\": \"{s}\", ", .{identifier.lexeme});
        }

        if (self.alias) |alias| {
            try out.print("\"alias\": \"{s}\", ", .{alias.lexeme});
        }

        if (self.declaration) |decl| {
            try out.print("\"declaration\": ", .{});

            try decl.toJson(@ptrCast(decl), out);

            try out.print(", ", .{});
        }

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);

        if (self.identifier) |identifier| {
            try out.print("export {s}", .{identifier.lexeme});
            if (self.alias) |alias| {
                try out.print(" as {s}", .{alias.lexeme});
            }
            try out.writeAll(";\n");
        } else {
            try out.print("export ", .{});
            try self.declaration.?.render(@ptrCast(self.declaration), out, depth);
        }
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.node_type != .Export) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const TypeExpressionNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .TypeExpression,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    value: Value,

    fn constant(_: *anyopaque) bool {
        return true;
    }

    fn val(nodePtr: *anyopaque, _: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        return Self.cast(node).?.value;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        const codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        const self = Self.cast(node).?;

        try codegen.emitConstant(self.node.location, self.value);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"TypeExpression\", \"value\": \"", .{});

        try ObjTypeDef.cast(self.value.obj()).?.toString(out);

        try out.print("\", ", .{});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);

        try out.print("<", .{});

        try ObjTypeDef.cast(self.value.obj()).?.toString(out);

        try out.writeAll(">");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.node_type != .TypeExpression) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const TypeOfExpressionNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .TypeOfExpression,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    expression: *ParseNode,

    fn constant(nodePtr: *anyopaque) bool {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        return self.expression.isConstant(self.expression);
    }

    fn val(nodePtr: *anyopaque, gc: *GarbageCollector) anyerror!Value {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        if (node.isConstant(node)) {
            const self = Self.cast(node).?;

            const expr = try self.expression.toValue(self.expression, gc);

            return (try Value.typeOf(expr, gc)).toValue();
        }

        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        const codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        const self = Self.cast(node).?;

        _ = try self.expression.toByteCode(self.expression, codegen, breaks);

        try codegen.emitOpCode(node.location, .OP_TYPEOF);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"TypeOfExpression\", \"expression\": \"", .{});

        try self.expression.toJson(self.expression, out);

        try out.print(", ", .{});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);

        try out.print("<", .{});

        try self.expression.render(self.expression, out, depth);

        try out.writeAll(">");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.node_type != .TypeOfExpression) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ZdefNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Zdef,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    lib_name: Token,
    symbol: []const u8,
    source: Token,
    fn_ptr: ?*anyopaque = null,
    obj_native: ?*ObjNative = null,
    // TODO: On the stack, do we free it at some point?
    zdef: *FFI.Zdef,
    slot: usize,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, _: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        const self = Self.cast(node).?;

        // Generate ObjNative wrapper of actual zdef
        switch (node.type_def.?.def_type) {
            .Function => {
                if (self.obj_native == null) {
                    self.obj_native = try codegen.mir_jit.?.compileZdef(self);

                    try codegen.emitConstant(node.location, self.obj_native.?.toValue());
                }
            },
            .ForeignStruct => {
                try codegen.mir_jit.?.compileZdefStruct(self);

                try codegen.emitConstant(node.location, node.type_def.?.toValue());
            },
            else => unreachable,
        }
        try codegen.emitCodeArg(node.location, .OP_DEFINE_GLOBAL, @intCast(self.slot));

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Zdef\", \"lib_name\": {s}, ", .{self.lib_name.lexeme});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);

        try out.print(
            "zdef({s}) {s}",
            .{
                self.lib_name.lexeme,
                self.source.lexeme,
            },
        );

        try out.writeAll(";\n");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.node_type != .Zdef) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const ImportNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Import,
        .toJson = stringify,
        .toByteCode = generate,
        .toValue = val,
        .isConstant = constant,
        .render = render,
    },

    imported_symbols: ?std.StringHashMap(void) = null,
    prefix: ?Token = null,
    path: Token,
    import: ?Parser.ScriptImport,

    fn constant(_: *anyopaque) bool {
        return false;
    }

    fn val(_: *anyopaque, _: *GarbageCollector) anyerror!Value {
        return GenError.NotConstant;
    }

    fn generate(nodePtr: *anyopaque, codegenPtr: *anyopaque, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        var codegen: *CodeGen = @ptrCast(@alignCast(codegenPtr));
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.synchronize(codegen)) {
            return null;
        }

        var self = Self.cast(node).?;

        if (self.import) |import| {
            try codegen.emitConstant(
                node.location,
                import.absolute_path.toValue(),
            );
            _ = try import.function.toByteCode(import.function, codegen, breaks);
            // FIXME: avoid generating the same import function more than once!
            try codegen.emitOpCode(self.node.location, .OP_IMPORT);
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer) RenderError!void {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Import\", \"path\": \"{s}\"", .{self.path.literal_string.?});

        if (self.prefix) |prefix| {
            try out.print(",\"prefix\": \"{s}\"", .{prefix.lexeme});
        }

        try out.writeAll(",\"imported_symbols\": [");
        if (self.imported_symbols) |imported_symbols| {
            var key_it = imported_symbols.keyIterator();
            var total = imported_symbols.count();
            var count: usize = 0;
            while (key_it.next()) |symbol| {
                try out.print("\"{s}\"", .{symbol});

                if (count < total - 1) {
                    try out.writeAll(",");
                }

                count += 1;
            }
        }
        try out.writeAll("]");

        if (self.import) |import| {
            try out.writeAll(",\"import\": ");
            try import.function.toJson(import.function, out);
        }

        try out.writeAll(",");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    fn render(nodePtr: *anyopaque, out: *const std.ArrayList(u8).Writer, depth: usize) RenderError!void {
        const node: *ParseNode = @ptrCast(@alignCast(nodePtr));
        const self = Self.cast(node).?;

        try out.writeByteNTimes(' ', depth * 4);

        try out.writeAll("import ");
        if (self.imported_symbols) |imported_symbols| {
            var it = imported_symbols.iterator();
            var i: usize = 0;
            while (it.next()) |kv| : (i += 1) {
                try out.writeAll(kv.key_ptr.*);

                if (i < imported_symbols.count() - 1) {
                    try out.writeAll(", ");
                }
            }

            try out.writeAll(" from ");
        }
        try out.print("{s}", .{self.path.lexeme});
        if (self.prefix) |prefix| {
            try out.print(" as {s}", .{prefix.lexeme});
        }

        try out.writeAll(";\n");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(nodePtr: *anyopaque) ?*Self {
        var node: *ParseNode = @ptrCast(@alignCast(nodePtr));

        if (node.node_type != .Import) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

fn escape(allocator: Allocator, from: []const u8) !std.ArrayList(u8) {
    // Escape \
    var backslash = try std.ArrayList(u8).initCapacity(
        allocator,
        std.mem.replacementSize(
            u8,
            from,
            "\\",
            "\\\\",
        ),
    );
    defer backslash.deinit();
    backslash.expandToCapacity();

    _ = std.mem.replace(
        u8,
        from,
        "\\",
        "\\\\",
        backslash.items,
    );

    // Escape new lines
    var newlines = try std.ArrayList(u8).initCapacity(
        allocator,
        std.mem.replacementSize(
            u8,
            backslash.items,
            "\n",
            "\\n",
        ),
    );
    defer newlines.deinit();
    newlines.expandToCapacity();

    _ = std.mem.replace(
        u8,
        backslash.items,
        "\n",
        "\\n",
        newlines.items,
    );

    // Escape "
    var quotes = try std.ArrayList(u8).initCapacity(
        allocator,
        std.mem.replacementSize(
            u8,
            newlines.items,
            "\"",
            "\\\"",
        ),
    );
    defer quotes.deinit();
    quotes.expandToCapacity();

    _ = std.mem.replace(
        u8,
        newlines.items,
        "\"",
        "\\\"",
        quotes.items,
    );

    // Escape \r
    var r = try std.ArrayList(u8).initCapacity(
        allocator,
        std.mem.replacementSize(
            u8,
            quotes.items,
            "\r",
            "\\r",
        ),
    );
    defer r.deinit();
    r.expandToCapacity();

    _ = std.mem.replace(
        u8,
        quotes.items,
        "\r",
        "\\r",
        r.items,
    );

    // Escape \t
    var t = try std.ArrayList(u8).initCapacity(
        allocator,
        std.mem.replacementSize(
            u8,
            r.items,
            "\t",
            "\\t",
        ),
    );
    t.expandToCapacity();

    _ = std.mem.replace(
        u8,
        r.items,
        "\t",
        "\\t",
        t.items,
    );

    return t;
}
