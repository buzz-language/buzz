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
const Config = @import("./config.zig").Config;

const disassembleChunk = disassembler.disassembleChunk;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjString = _obj.ObjString;
const ObjNative = _obj.ObjNative;
const ObjFunction = _obj.ObjFunction;
const ObjObject = _obj.ObjObject;
const FunctionType = ObjFunction.FunctionType;
const copyStringRaw = _obj.copyStringRaw;
const Value = _value.Value;
const valueToString = _value.valueToString;
const Token = _token.Token;
const TokenType = _token.TokenType;
const CodeGen = _codegen.CodeGen;
const Parser = _parser.Parser;
const Frame = _codegen.Frame;
const Local = _parser.Local;
const Global = _parser.Global;
const UpValue = _parser.UpValue;
const OpCode = _chunk.OpCode;

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

    Binary,
    Unary,
    Subscript,
    Unwrap,
    ForceUnwrap,
    Is,

    And,
    Or,

    Expression,
    NamedVariable,

    Number,
    String,
    StringLiteral,
    Boolean,
    Null,

    List,
    Map,

    Super,
    Dot,
    ObjectInit,

    Throw,
    Break,
    Continue,
    Call,
    If,
    Block, // For semantic purposes only
    Return,
    For,
    ForEach,
    DoUntil,
    While,
    Export,
    Import,
    Catch,
};

pub const ParseNode = struct {
    const Self = @This();

    node_type: ParseNodeType,
    // If null, either its a statement or its a reference to something unkown that should ultimately raise a compile error
    type_def: ?*ObjTypeDef = null,
    location: Token = undefined,
    // Wether optional jumps must be patch before generate this node bytecode
    patch_opt_jumps: bool = false,
    docblock: ?Token = null,

    // Does this node closes a scope
    ends_scope: ?std.ArrayList(OpCode) = null,

    toJson: fn (*Self, std.ArrayList(u8).Writer) anyerror!void = stringify,
    toByteCode: fn (*Self, *CodeGen, ?*std.ArrayList(usize)) anyerror!?*ObjFunction = generate,

    // TODO: constant expressions (https://github.com/giann/buzz/issues/46)
    pub fn isConstant(self: *Self) bool {
        // zig fmt: off
        return self.node_type == .Number
            or self.node_type == .StringLiteral
            or self.node_type == .Boolean
            or self.node_type == .Null
            or (
                self.node_type == .String
                    and StringNode.cast(self).?.elements.len == 1
                    and StringNode.cast(self).?.elements[0].isConstant()
                );
        // zig fmt: on
    }

    pub fn toValue(self: *Self, allocator: Allocator, strings: *std.StringHashMap(*ObjString)) anyerror!?Value {
        return switch (self.node_type) {
            .Number => Value{ .Number = NumberNode.cast(self).?.constant },
            .String => string: {
                const string_node = StringNode.cast(self).?;
                const element = string_node.elements[0];
                const element_value = (try element.toValue(allocator, strings)).?;

                break :string (try copyStringRaw(strings, allocator, try valueToString(allocator, element_value), true)).toValue();
            },
            .StringLiteral => StringLiteralNode.cast(self).?.constant.toValue(),
            .Boolean => Value{ .Boolean = BooleanNode.cast(self).?.constant },
            .Null => Value{ .Null = null },
            else => null,
        };
    }

    fn generate(_: *Self, _: *CodeGen, _: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        return null;
    }

    fn patchOptJumps(self: *Self, codegen: *CodeGen) !void {
        if (self.patch_opt_jumps) {
            assert(codegen.opt_jumps != null);

            // Hope over OP_POP if actual value
            const njump: usize = try codegen.emitJump(self.location, .OP_JUMP);

            for (codegen.opt_jumps.?.items) |jump| {
                try codegen.patchJump(jump);
            }
            // If aborted by a null optional, will result in null on the stack
            try codegen.emitOpCode(self.location, .OP_POP);

            try codegen.patchJump(njump);

            codegen.opt_jumps.?.deinit();
            codegen.opt_jumps = null;
        }
    }

    fn stringify(self: *Self, out: std.ArrayList(u8).Writer) anyerror!void {
        try out.print(
            "\"type_def\": \"{s}\"",
            .{
                if (self.type_def) |type_def| try type_def.toString(std.heap.c_allocator) else "N/A",
            },
        );

        if (self.docblock != null) {
            try out.print(", \"docblock\": \"{s}\"", .{self.docblock.?.literal_string});
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
    },

    expression: *ParseNode,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        _ = try self.expression.toByteCode(self.expression, codegen, breaks);

        try codegen.emitOpCode(node.location, .OP_POP);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Expression\", ", .{});

        try ParseNode.stringify(node, out);

        try out.writeAll(",\"expression\": ");

        try self.expression.toJson(self.expression, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .Expression) {
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
    },

    identifier: Token,
    value: ?*ParseNode = null,
    slot: usize,
    slot_type: SlotType,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

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
            _ = try value.toByteCode(value, codegen, breaks);

            try codegen.emitCodeArg(self.node.location, set_op, @intCast(u24, self.slot));
        } else {
            try codegen.emitCodeArg(self.node.location, get_op, @intCast(u24, self.slot));
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"NamedVariable\", \"identifier\": \"{s}\", \"slot\": \"{}\", \"slot_type\": \"{}\",", .{ self.identifier.literal_string, self.slot, self.slot_type });

        try ParseNode.stringify(node, out);

        try out.writeAll(",\"value\": ");

        if (self.value) |value| {
            try value.toJson(value, out);
        } else {
            try out.writeAll("null");
        }

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .NamedVariable) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const NumberNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Number,
        .toJson = stringify,
        .toByteCode = generate,
    },

    constant: f64,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        try codegen.emitConstant(self.node.location, Value{ .Number = self.constant });

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Number\", \"constant\": \"{}\", ", .{self.constant});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .Number) {
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
    },

    constant: bool,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        try codegen.emitOpCode(self.node.location, if (self.constant) .OP_TRUE else .OP_FALSE);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Boolean\", \"constant\": \"{}\", ", .{self.constant});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    constant: *ObjString,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        try codegen.emitConstant(self.node.location, self.constant.toValue());

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        // var self = Self.cast(node).?;

        try out.print("{{\"node\": \"StringLiteral\", \"constant\": \"__TODO_ESCAPE_QUOTES__\", ", .{}); //.{self.constant.string});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .StringLiteral) {
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
    },

    // List of nodes that will eventually be converted to strings concatened together
    elements: []*ParseNode,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        if (self.elements.len == 0) {
            // Push the empty string which is always the constant 0
            try codegen.emitCodeArg(self.node.location, .OP_CONSTANT, 0);

            try node.endScope(codegen);

            return null;
        }

        for (self.elements) |element, index| {
            if (element.type_def == null or element.type_def.?.def_type == .Placeholder) {
                try codegen.reportErrorAt(element.location, "Unknown type");

                continue;
            }

            _ = try element.toByteCode(element, codegen, breaks);
            if (element.type_def.?.def_type != .String or element.type_def.?.optional) {
                try codegen.emitOpCode(self.node.location, .OP_TO_STRING);
            }

            if (index >= 1) {
                try codegen.emitOpCode(self.node.location, .OP_ADD);
            }
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"String\", \"elements\": [");

        for (self.elements) |element, i| {
            try element.toJson(element, out);

            if (i < self.elements.len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
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

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        try codegen.emitOpCode(node.location, .OP_NULL);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        try out.writeAll("{\"node\": \"Null\", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .Null) {
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
    },

    items: []*ParseNode,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        const item_type = self.node.type_def.?.resolved_type.?.List.item_type;
        const list_offset: usize = try codegen.emitList(self.node.location);

        for (self.items) |item| {
            if (item.type_def.?.def_type == .Placeholder) {
                try codegen.reportErrorAt(item.location, "Unknown type");
            } else if (!item_type.eql(item.type_def.?)) {
                try codegen.reportTypeCheckAt(item_type, item.type_def.?, "Bad list type", item.location);
            } else {
                _ = try item.toByteCode(item, codegen, breaks);

                try codegen.emitOpCode(item.location, .OP_LIST_APPEND);
            }
        }

        const list_type_constant: u24 = try codegen.makeConstant(Value{ .Obj = node.type_def.?.toObj() });
        try codegen.patchList(list_offset, list_type_constant);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"List\", \"items\": [");

        for (self.items) |item, i| {
            try item.toJson(item, out);

            if (i < self.items.len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    keys: []*ParseNode,
    values: []*ParseNode,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        const key_type = self.node.type_def.?.resolved_type.?.Map.key_type;
        const value_type = self.node.type_def.?.resolved_type.?.Map.value_type;

        const map_offset: usize = try codegen.emitMap(self.node.location);

        assert(self.keys.len == self.values.len);

        for (self.keys) |key, i| {
            const value = self.values[i];

            _ = try key.toByteCode(key, codegen, breaks);
            _ = try value.toByteCode(value, codegen, breaks);

            try codegen.emitOpCode(self.node.location, .OP_SET_MAP);

            if (key.type_def.?.def_type == .Placeholder) {
                try codegen.reportErrorAt(key.location, "Unknown type");
            }

            if (value.type_def.?.def_type == .Placeholder) {
                try codegen.reportErrorAt(value.location, "Unknown type");
            }

            if (!key_type.eql(key.type_def.?)) {
                try codegen.reportTypeCheckAt(key_type, key.type_def.?, "Bad key type", key.location);
            }

            if (!value_type.eql(value.type_def.?)) {
                try codegen.reportTypeCheckAt(value_type, value.type_def.?, "Bad value type", value.location);
            }
        }

        const map_type_constant: u24 = try codegen.makeConstant(Value{ .Obj = node.type_def.?.toObj() });
        try codegen.patchMap(map_offset, map_type_constant);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Map\", \"items\": [");

        for (self.keys) |key, i| {
            try out.writeAll("{\"key\": ");

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

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    unwrapped: *ParseNode,
    original_type: ?*ObjTypeDef,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        if (self.original_type == null or self.original_type.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(self.unwrapped.location, "Unknown type");
        }

        if (!self.original_type.?.optional) {
            try codegen.reportErrorAt(self.unwrapped.location, "Not an optional.");
        }

        _ = try self.unwrapped.toByteCode(self.unwrapped, codegen, breaks);

        try codegen.emitOpCode(self.node.location, .OP_COPY);
        try codegen.emitOpCode(self.node.location, .OP_NULL);
        try codegen.emitOpCode(self.node.location, .OP_EQUAL);
        try codegen.emitOpCode(self.node.location, .OP_NOT);

        const jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);

        if (codegen.opt_jumps == null) {
            codegen.opt_jumps = std.ArrayList(usize).init(codegen.allocator);
        }
        try codegen.opt_jumps.?.append(jump);

        try codegen.emitOpCode(self.node.location, .OP_POP); // Pop test result

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Unwrap\", \"unwrapped\": ");

        try self.unwrapped.toJson(self.unwrapped, out);
        try out.writeAll(",");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    unwrapped: *ParseNode,
    original_type: ?*ObjTypeDef,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        if (self.original_type == null or self.original_type.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(self.unwrapped.location, "Unknown type");

            return null;
        }

        if (!self.original_type.?.optional) {
            try codegen.reportErrorAt(self.unwrapped.location, "Not an optional.");
        }

        _ = try self.unwrapped.toByteCode(self.unwrapped, codegen, breaks);

        try codegen.emitOpCode(self.node.location, .OP_UNWRAP);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"ForceUnwrap\", \"unwrapped\": ");

        try self.unwrapped.toJson(self.unwrapped, out);
        try out.writeAll(",");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    left: *ParseNode,
    constant: Value,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        _ = try self.left.toByteCode(self.left, codegen, breaks);

        try codegen.emitCodeArg(self.node.location, .OP_CONSTANT, try codegen.makeConstant(self.constant));

        try codegen.emitOpCode(self.node.location, .OP_IS);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Is\", \"left\": ");

        try self.left.toJson(self.left, out);
        try out.print(", \"constant\": \"{s}\", ", .{try valueToString(std.heap.c_allocator, self.constant)});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .Is) {
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
    },

    left: *ParseNode,
    operator: TokenType,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        if (self.left.type_def == null or self.left.type_def.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(node.location, "Unknown type");

            return null;
        }

        _ = try self.left.toByteCode(self.left, codegen, breaks);

        const left_type = self.left.type_def.?;
        switch (self.operator) {
            .Bang => {
                if (left_type.def_type != .Bool) {
                    try codegen.reportErrorFmt(
                        self.left.location,
                        "Expected type `bool`, got `{s}`",
                        .{try left_type.toString(codegen.allocator)},
                    );
                }

                try codegen.emitOpCode(self.node.location, .OP_NOT);
            },
            .Minus => {
                if (left_type.def_type != .Number) {
                    try codegen.reportErrorFmt(
                        self.left.location,
                        "Expected type `num`, got `{s}`",
                        .{try left_type.toString(codegen.allocator)},
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

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Unary\", \"left\": ");

        try self.left.toJson(self.left, out);
        try out.print(", \"operator\": \"{}\", ", .{self.operator});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    left: *ParseNode,
    right: *ParseNode,
    operator: TokenType,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        const left_type = self.left.type_def.?;
        const right_type = self.right.type_def.?;

        if (self.left.type_def == null or self.left.type_def.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(self.left.location, "Unknown type");

            return null;
        }

        if (self.right.type_def == null or self.right.type_def.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(self.right.location, "Unknown type");

            return null;
        }

        if (!left_type.eql(right_type)) {
            try codegen.reportTypeCheckAt(left_type, right_type, "Type mismatch", node.location);
        }

        switch (self.operator) {
            .QuestionQuestion => {
                if (!left_type.optional) {
                    try codegen.reportErrorAt(node.location, "Not an optional");
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_NULL_OR);
            },
            .Greater => {
                // Checking only left operand since we asserted earlier that both operand have the same type
                if (left_type.def_type != .Number) {
                    try codegen.reportErrorAt(self.left.location, "Expected `num`.");
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_GREATER);
            },
            .Less => {
                if (left_type.def_type != .Number) {
                    try codegen.reportErrorAt(self.left.location, "Expected `num`.");
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_LESS);
            },
            .GreaterEqual => {
                if (left_type.def_type != .Number) {
                    try codegen.reportErrorAt(self.left.location, "Expected `num`.");
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_LESS);
                try codegen.emitOpCode(self.node.location, .OP_NOT);
            },
            .LessEqual => {
                if (left_type.def_type != .Number) {
                    try codegen.reportErrorAt(self.left.location, "Expected `num`.");
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
                if (left_type.def_type != .Number
                    and left_type.def_type != .String
                    and left_type.def_type != .List
                    and left_type.def_type != .Map) {
                    try codegen.reportErrorAt(self.left.location, "Expected a `num`, `str`, list or map.");
                }
                // zig fmt: on

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_ADD);
            },
            .Minus => {
                if (left_type.def_type != .Number) {
                    try codegen.reportErrorAt(node.location, "Expected `num`.");
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_SUBTRACT);
            },
            .Star => {
                if (left_type.def_type != .Number) {
                    try codegen.reportErrorAt(node.location, "Expected `num`.");
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_MULTIPLY);
            },
            .Slash => {
                if (left_type.def_type != .Number) {
                    try codegen.reportErrorAt(node.location, "Expected `num`.");
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_DIVIDE);
            },
            .Percent => {
                if (left_type.def_type != .Number) {
                    try codegen.reportErrorAt(node.location, "Expected `num`.");
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);
                _ = try self.right.toByteCode(self.right, codegen, breaks);
                try codegen.emitOpCode(self.node.location, .OP_MOD);
            },
            .And => {
                if (left_type.def_type != .Bool) {
                    try codegen.reportErrorAt(node.location, "`and` expects operands to be `bool`");
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);

                const end_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
                try codegen.emitOpCode(self.node.location, .OP_POP);

                _ = try self.right.toByteCode(self.right, codegen, breaks);

                try codegen.patchJump(end_jump);
            },
            .Or => {
                if (left_type.def_type != .Bool) {
                    try codegen.reportErrorAt(node.location, "`and` expects operands to be `bool`");
                }

                _ = try self.left.toByteCode(self.left, codegen, breaks);

                const else_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
                const end_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP);

                try codegen.patchJump(else_jump);
                try codegen.emitOpCode(self.node.location, .OP_POP);

                _ = try self.right.toByteCode(self.right, codegen, breaks);

                try codegen.patchJump(end_jump);
            },
            else => unreachable,
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Binary\", \"left\": ");

        try self.left.toJson(self.left, out);
        try out.print(", \"operator\": \"{}\", \"right\": ", .{self.operator});
        try self.right.toJson(self.right, out);
        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    subscripted: *ParseNode,
    index: *ParseNode,
    value: ?*ParseNode,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        _ = try self.subscripted.toByteCode(self.subscripted, codegen, breaks);

        if (self.subscripted.type_def == null or self.subscripted.type_def.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(self.subscripted.location, "Unknown type.");
        }

        if (self.index.type_def == null or self.index.type_def.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(self.index.location, "Unknown type.");
        }

        if (self.value != null and (self.value.?.type_def == null or self.value.?.type_def.?.def_type == .Placeholder)) {
            try codegen.reportErrorAt(self.value.?.location, "Unknown type.");
        }

        switch (self.subscripted.type_def.?.def_type) {
            .String => {
                if (self.index.type_def.?.def_type != .Number) {
                    try codegen.reportErrorAt(self.index.location, "Expected `num` index.");
                }

                assert(self.value == null);
            },
            .List => {
                if (self.index.type_def.?.def_type != .Number) {
                    try codegen.reportErrorAt(self.index.location, "Expected `num` index.");
                }

                if (self.value) |value| {
                    if (!self.subscripted.type_def.?.resolved_type.?.List.item_type.eql(value.type_def.?)) {
                        try codegen.reportTypeCheckAt(self.subscripted.type_def.?.resolved_type.?.List.item_type, value.type_def.?, "Bad value type", value.location);
                    }
                }
            },
            .Map => {
                if (!self.subscripted.type_def.?.resolved_type.?.Map.key_type.eql(self.index.type_def.?)) {
                    try codegen.reportTypeCheckAt(self.subscripted.type_def.?.resolved_type.?.Map.key_type, self.index.type_def.?, "Bad key type", self.index.location);
                }

                if (self.value) |value| {
                    if (!self.subscripted.type_def.?.resolved_type.?.Map.value_type.eql(value.type_def.?)) {
                        try codegen.reportTypeCheckAt(self.subscripted.type_def.?.resolved_type.?.Map.value_type, value.type_def.?, "Bad value type", value.location);
                    }
                }
            },
            else => try codegen.reportErrorAt(node.location, "Not subscriptable."),
        }

        _ = try self.index.toByteCode(self.index, codegen, breaks);

        if (self.value) |value| {
            _ = try value.toByteCode(value, codegen, breaks);

            try codegen.emitOpCode(self.node.location, .OP_SET_SUBSCRIPT);
        } else {
            try codegen.emitOpCode(self.node.location, .OP_GET_SUBSCRIPT);
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
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

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .Subscript) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const FunctionNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Function,
        .toJson = stringify,
        .toByteCode = generate,
    },

    body: ?*BlockNode = null,
    arrow_expr: ?*ParseNode = null,
    native: ?*ObjNative = null,
    test_message: ?*ParseNode = null,
    // If true this is the root of a script being imported
    import_root: bool = false,
    upvalue_binding: std.AutoArrayHashMap(u8, bool),

    // Useful when generating root script bootstrap code
    main_slot: ?usize = null,
    test_slots: ?[]usize = null,
    exported_count: ?usize = null,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        var enclosing = codegen.current;
        codegen.current = try codegen.allocator.create(Frame);
        codegen.current.?.* = Frame{
            .enclosing = enclosing,
            .function_node = self,
        };

        var function = try ObjFunction.init(
            codegen.allocator,
            node.type_def.?.resolved_type.?.Function.name,
        );

        function.type_def = node.type_def.?;

        // First chunk constant is the empty string
        _ = try function.chunk.addConstant(null, Value{
            .Obj = (try copyStringRaw(codegen.strings, codegen.allocator, "", true // The substring we built is now owned by codegen
            )).toObj(),
        });

        codegen.current.?.function = try codegen.allocator.create(ObjFunction);
        codegen.current.?.function.?.* = function;

        const function_type = node.type_def.?.resolved_type.?.Function.function_type;

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
                if (!codegen.testing and function_type == .ScriptEntryPoint) {
                    if (self.main_slot) |main_slot| {
                        try codegen.emitCodeArg(node.location, .OP_GET_GLOBAL, @intCast(u24, main_slot));
                        try codegen.emitCodeArg(node.location, .OP_GET_LOCAL, 0); // cli args are always local 0
                        try codegen.emitCodeArgs(node.location, .OP_CALL, 1, 0);
                    }
                } else if (codegen.testing and self.test_slots != null) {
                    // Create an entry point wich runs all `test`
                    for (self.test_slots.?) |slot| {
                        try codegen.emitCodeArg(node.location, .OP_GET_GLOBAL, @intCast(u24, slot));
                        try codegen.emitCodeArgs(node.location, .OP_CALL, 0, 0);
                    }
                }

                // If we're being imported, put all globals on the stack
                if (self.import_root) {
                    if (self.exported_count orelse 0 > 16777215) {
                        try codegen.reportErrorAt(node.location, "Can't export more than 16777215 values.");
                    }

                    var index: usize = 0;
                    while (index < self.exported_count orelse 0) : (index += 1) {
                        try codegen.emitCodeArg(node.location, .OP_GET_GLOBAL, @intCast(u24, index));
                    }

                    try codegen.emitCodeArg(node.location, .OP_EXPORT, @intCast(u24, self.exported_count orelse 0));
                } else {
                    try codegen.emitOpCode(node.location, .OP_VOID);
                    try codegen.emitOpCode(node.location, .OP_RETURN);
                }
            } else if (codegen.current.?.function.?.type_def.resolved_type.?.Function.return_type.def_type == .Void and !codegen.current.?.return_emitted) {
                // TODO: detect if some branches of the function body miss a return statement
                try codegen.emitReturn(node.location);
            }
        }

        var frame = codegen.current.?;
        var current_function: *ObjFunction = frame.function.?;
        current_function.upvalue_count = @intCast(u8, self.upvalue_binding.count());

        if (Config.debug) {
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

        return current_function;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Function\", \"type\": \"{}\", ", .{self.node.type_def.?.resolved_type.?.Function.function_type});

        if (self.body) |body| {
            try out.writeAll("\"body\": ");

            try body.toNode().toJson(body.toNode(), out);
        } else if (self.arrow_expr) |expr| {
            try out.writeAll("\"arrow_expr\": ");

            try expr.toJson(expr, out);
        }

        try out.writeAll(", ");

        if (self.native) |native| {
            try out.print("\"native\": \"{s}\",", .{try valueToString(std.heap.c_allocator, native.toValue())});
        }

        if (self.test_message) |test_message| {
            try out.writeAll("\"test_message\": ");
            try test_message.toJson(test_message, out);

            try out.writeAll(", ");
        }

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn init(parser: *Parser, function_type: FunctionType, file_name_or_name: ?[]const u8) !Self {
        var self = Self{
            .body = try parser.allocator.create(BlockNode),
            .upvalue_binding = std.AutoArrayHashMap(u8, bool).init(parser.allocator),
        };

        self.body.?.* = BlockNode.init(parser.allocator);

        const function_name: []const u8 = switch (function_type) {
            .EntryPoint => "main",
            .ScriptEntryPoint, .Script => file_name_or_name orelse "<script>",
            .Catch => "<catch>",
            else => file_name_or_name orelse "???",
        };

        const function_def = ObjFunction.FunctionDef{
            .name = try copyStringRaw(parser.strings, parser.allocator, function_name, false),
            .return_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
            .parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator),
            .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
            .function_type = function_type,
        };

        const type_def = ObjTypeDef.TypeUnion{ .Function = function_def };

        self.node.type_def = try parser.type_registry.getTypeDef(
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

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .Function) {
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
    },

    callee: *ParseNode,
    arguments: std.StringArrayHashMap(*ParseNode),
    catches: ?[]*ParseNode = null,
    super: ?*NamedVariableNode = null,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        if (self.callee.type_def == null or self.callee.type_def.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(node.location, "Called element is of unkown type");
        }

        var invoked = false;
        var invoked_on: ?ObjTypeDef.Type = null;

        if (self.callee.node_type == .Dot) {
            const dot = DotNode.cast(self.callee).?;
            const field_accessed = dot.callee.type_def;

            if (field_accessed == null or field_accessed.?.def_type == .Placeholder) {
                try codegen.reportErrorAt(dot.node.location, "Unknown field type");
            }

            invoked = field_accessed.?.def_type != .Object;
            invoked_on = field_accessed.?.def_type;
        }

        if (!invoked and self.super == null and invoked_on == null) {
            _ = try self.callee.toByteCode(self.callee, codegen, breaks);
        }

        const callee_type = switch (self.callee.node_type) {
            .Dot => DotNode.cast(self.callee).?.member_type_def,
            .Super => SuperNode.cast(self.callee).?.member_type_def,
            else => self.callee.type_def,
        };

        if (callee_type == null or callee_type.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(self.callee.location, "Unknown type");
        }

        const args: std.StringArrayHashMap(*ObjTypeDef) = if (callee_type.?.def_type == .Function)
            callee_type.?.resolved_type.?.Function.parameters
        else
            callee_type.?.resolved_type.?.Native.parameters;

        const defaults = if (callee_type.?.def_type == .Function)
            callee_type.?.resolved_type.?.Function.defaults
        else
            callee_type.?.resolved_type.?.Native.defaults;
        const arg_keys = args.keys();
        const arg_count = arg_keys.len;

        var missing_arguments = std.StringArrayHashMap(usize).init(codegen.allocator);
        defer missing_arguments.deinit();
        for (arg_keys) |arg_name, pindex| {
            try missing_arguments.put(arg_name, pindex);
        }

        if (self.arguments.count() > args.count()) {
            try codegen.reportErrorAt(node.location, "Too many arguments.");
        }

        // First push on the stack arguments has they are parsed
        var needs_reorder = false;
        for (self.arguments.keys()) |arg_key, index| {
            const argument = self.arguments.get(arg_key).?;
            const actual_arg_key = if (index == 0 and std.mem.eql(u8, arg_key, "$")) args.keys()[0] else arg_key;
            const def_arg_type = args.get(actual_arg_key);

            const ref_index = args.getIndex(actual_arg_key);
            if (index != ref_index) {
                needs_reorder = true;
            }

            // Type check the argument
            if (def_arg_type) |arg_type| {
                if (argument.type_def == null or argument.type_def.?.def_type == .Placeholder) {
                    try codegen.reportErrorAt(argument.location, "Unknown type");
                } else if (!arg_type.eql(argument.type_def.?)) {
                    try codegen.reportTypeCheckAt(
                        arg_type,
                        argument.type_def.?,
                        "Bad argument type",
                        argument.location,
                    );
                }

                _ = missing_arguments.orderedRemove(actual_arg_key);
            } else {
                try codegen.reportErrorFmt(argument.location, "Argument `{s}` does not exists.", .{arg_key});
            }

            _ = try argument.toByteCode(argument, codegen, breaks);
        }

        // Argument order reference
        var arguments_order_ref = std.ArrayList([]const u8).init(codegen.allocator);
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

                    try arguments_order_ref.append(missing_key);
                    _ = missing_arguments.orderedRemove(missing_key);
                    needs_reorder = true;
                }
            }
        }

        if (missing_arguments.count() > 0) {
            const missing = try std.mem.join(codegen.allocator, ", ", missing_arguments.keys());
            defer codegen.allocator.free(missing);
            try codegen.reportErrorFmt(node.location, "Missing argument(s): {s}", .{missing});
        }

        // Reorder arguments
        if (needs_reorder) {
            // Until ordered
            while (true) {
                var ordered = true;

                for (arguments_order_ref.items) |arg_key, index| {
                    const actual_arg_key = if (index == 0 and std.mem.eql(u8, arg_key, "$")) args.keys()[0] else arg_key;
                    const correct_index = args.getIndex(actual_arg_key).?;

                    if (correct_index != index) {
                        ordered = false;

                        // TODO: both OP_SWAP args could fit in a 32 bit instruction
                        try codegen.emitCodeArg(node.location, .OP_SWAP, @intCast(u24, arg_count - index - 1));
                        // to where it should be
                        try codegen.emit(node.location, @intCast(u32, arg_count - correct_index - 1));

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

        // If super call, push super as a new local
        if (self.super) |super| {
            _ = try super.node.toByteCode(&super.node, codegen, breaks);
        }

        if (invoked) {
            // TODO: can it be invoked without callee being a DotNode?
            try codegen.emitCodeArg(
                self.node.location,
                .OP_INVOKE,
                try codegen.identifierConstant(DotNode.cast(self.callee).?.identifier.lexeme),
            );
        } else if (self.super != null) {
            try codegen.emitCodeArg(
                self.node.location,
                .OP_SUPER_INVOKE,
                try codegen.identifierConstant(SuperNode.cast(self.callee).?.identifier.lexeme),
            );
        }

        // Catch clauses
        if (self.catches) |catches| {
            for (catches) |catch_clause| {
                if (catch_clause.type_def == null or catch_clause.type_def.?.def_type == .Placeholder) {
                    try codegen.reportErrorAt(catch_clause.location, "Unknown type.");
                } else {
                    switch (catch_clause.type_def.?.def_type) {
                        .Function => {
                            if (!node.type_def.?.eql(catch_clause.type_def.?.resolved_type.?.Function.return_type)) {
                                try codegen.reportTypeCheckAt(
                                    node.type_def.?,
                                    catch_clause.type_def.?.resolved_type.?.Function.return_type,
                                    "Wrong catch clause return type",
                                    catch_clause.location,
                                );
                            }
                        },
                        else => {
                            assert(self.catches.?.len == 1);

                            // Expression
                            if (!node.type_def.?.eql(catch_clause.type_def.?)) {
                                try codegen.reportTypeCheckAt(
                                    node.type_def.?,
                                    catch_clause.type_def.?.resolved_type.?.Function.return_type,
                                    "Wrong catch clause return type",
                                    catch_clause.location,
                                );
                            }
                        },
                    }
                }

                _ = try catch_clause.toByteCode(catch_clause, codegen, breaks);
            }
        }

        if (!invoked and self.super == null) {
            try codegen.emitCodeArgs(
                self.node.location,
                .OP_CALL,
                @intCast(u8, arguments_order_ref.items.len),
                if (self.catches) |catches| @intCast(u16, catches.len) else 0,
            );
        } else {
            try codegen.emitTwo(
                self.node.location,
                if (self.super == null and (invoked_on != null and invoked_on.? != .ObjectInstance)) @intCast(u8, self.arguments.count()) + 1 else @intCast(u8, self.arguments.count()),
                if (self.catches) |catches| @intCast(u16, catches.len) else 0,
            );
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Call\"");

        // if (!invoked and invoked_on == null) {
        //     try out.writeAll(", \"callee\": ");
        //     try self.callee.toJson(self.callee, out);
        // }

        try out.writeAll(", \"arguments\": [");

        for (self.arguments.keys()) |key, i| {
            const argument = self.arguments.get(key).?;

            try out.print("{{\"name\": \"{s}\", \"value\": ", .{key});

            try argument.toJson(argument, out);

            try out.writeAll("}");

            if (i < self.arguments.keys().len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], ");

        if (self.catches) |catches| {
            try out.writeAll("\"catches\": [");

            for (catches) |clause, i| {
                try clause.toJson(clause, out);

                if (i < catches.len - 1) {
                    try out.writeAll(",");
                }
            }

            try out.writeAll("],");
        }

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
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

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    function: *FunctionNode,
    slot: usize,
    slot_type: SlotType,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        _ = try self.function.node.toByteCode(&self.function.node, codegen, breaks);

        if (self.slot_type == .Global) {
            try codegen.emitCodeArg(self.node.location, .OP_DEFINE_GLOBAL, @intCast(u24, self.slot));
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"FunDeclaration\",\"slot_type\": \"{}\",\"function\": ", .{self.slot_type});

        try self.function.node.toJson(&self.function.node, out);

        try out.writeAll(",");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    name: Token,
    value: ?*ParseNode = null,
    type_def: ?*ObjTypeDef = null,
    type_name: ?Token = null,
    constant: bool,
    slot: usize,
    slot_type: SlotType,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        if (self.value) |value| {
            if (value.type_def == null or value.type_def.?.def_type == .Placeholder) {
                try codegen.reportErrorAt(value.location, "Unknown type.");
            } else if (self.type_def == null or self.type_def.?.def_type == .Placeholder) {
                try codegen.reportErrorAt(node.location, "Unknown type.");
            } else if (!(try self.type_def.?.toInstance(codegen.allocator, codegen.type_registry)).eql(value.type_def.?) and !(try (try self.type_def.?.toInstance(codegen.allocator, codegen.type_registry)).cloneNonOptional(codegen.type_registry)).eql(value.type_def.?)) {
                try codegen.reportTypeCheckAt(try self.type_def.?.toInstance(codegen.allocator, codegen.type_registry), value.type_def.?, "Wrong variable type", value.location);
            }

            _ = try value.toByteCode(value, codegen, breaks);
        } else {
            try codegen.emitOpCode(self.node.location, .OP_NULL);
        }

        if (self.slot_type == .Global) {
            try codegen.emitCodeArg(self.node.location, .OP_DEFINE_GLOBAL, @intCast(u24, self.slot));
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        const var_type = if (self.type_def) |type_def| try type_def.toString(std.heap.c_allocator) else "";

        try out.print(
            "{{\"node\": \"VarDeclaration\", \"name\": \"{s}\", \"constant\": {}, \"var_type\": \"{s} @{}\", ",
            .{
                self.name.lexeme,
                self.constant,
                var_type,
                if (self.type_def) |type_def| @ptrToInt(type_def) else 0,
            },
        );

        if (self.value) |value| {
            try out.writeAll("\"value\": ");

            try value.toJson(value, out);

            try out.writeAll(", ");
        }

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    slot: usize,
    cases: std.ArrayList(*ParseNode),

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        if (node.type_def.?.resolved_type.?.Enum.enum_type.def_type == .Placeholder) {
            try codegen.reportErrorAt(node.location, "Unknwon enum type.");
        }

        try codegen.emitCodeArg(self.node.location, .OP_ENUM, try codegen.makeConstant(node.type_def.?.toValue()));
        try codegen.emitCodeArg(self.node.location, .OP_DEFINE_GLOBAL, @intCast(u24, self.slot));

        try codegen.emitCodeArg(self.node.location, .OP_GET_GLOBAL, @intCast(u24, self.slot));

        for (self.cases.items) |case| {
            if (case.type_def == null or case.type_def.?.def_type == .Placeholder) {
                try codegen.reportErrorAt(case.location, "Unknown type.");
            } else if (!((try node.type_def.?.resolved_type.?.Enum.enum_type.toInstance(codegen.allocator, codegen.type_registry))).eql(case.type_def.?)) {
                try codegen.reportTypeCheckAt((try node.type_def.?.resolved_type.?.Enum.enum_type.toInstance(codegen.allocator, codegen.type_registry)), case.type_def.?, "Bad enum case type", case.location);
            }

            _ = try case.toByteCode(case, codegen, breaks);

            try codegen.emitOpCode(self.node.location, .OP_ENUM_CASE);
        }

        try codegen.emitOpCode(self.node.location, .OP_POP);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Enum\", \"cases\": [");

        for (self.cases.items) |case, i| {
            try case.toJson(case, out);
            if (i < self.cases.items.len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn init(allocator: Allocator) Self {
        return Self{
            .cases = std.ArrayList(*ParseNode).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.cases.deinit();
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    error_value: *ParseNode,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        _ = try self.error_value.toByteCode(self.error_value, codegen, breaks);

        try codegen.emitOpCode(self.node.location, .OP_THROW);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Throw\", \"error_value\": ");

        try self.error_value.toJson(self.error_value, out);

        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        assert(breaks != null);

        try breaks.?.append(try codegen.emitJump(node.location, .OP_JUMP));

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(_: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        try out.writeAll("{\"node\": \"Break\" }");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        assert(breaks != null);

        try breaks.?.append(try codegen.emitJump(node.location, .OP_LOOP));

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(_: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        try out.writeAll("{\"node\": \"Continue\" }");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .Continue) {
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
    },

    condition: *ParseNode,
    body: *ParseNode,
    else_branch: ?*ParseNode = null,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        if (self.condition.type_def == null or self.condition.type_def.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(self.condition.location, "Unknown type.");
        }

        if (self.condition.type_def.?.def_type != .Bool) {
            try codegen.reportErrorAt(self.condition.location, "`if` condition must be bool");
        }

        _ = try self.condition.toByteCode(self.condition, codegen, breaks);

        const then_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
        try codegen.emitOpCode(self.node.location, .OP_POP);

        _ = try self.body.toByteCode(self.body, codegen, breaks);

        const else_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP);

        try codegen.patchJump(then_jump);
        try codegen.emitOpCode(self.node.location, .OP_POP);

        if (self.else_branch) |else_branch| {
            _ = try else_branch.toByteCode(else_branch, codegen, breaks);
        }

        try codegen.patchJump(else_jump);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
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

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    value: ?*ParseNode,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        if (self.value) |value| {
            if (value.type_def == null) {
                try codegen.reportErrorAt(value.location, "Unknown type.");
            } else if (value.type_def.?.def_type == .Placeholder) {
                try codegen.reportErrorFmt(
                    value.type_def.?.resolved_type.?.Placeholder.where,
                    "Unresolved placeholder @{} ({s})",
                    .{
                        @ptrToInt(value.type_def.?),
                        if (value.type_def.?.resolved_type.?.Placeholder.name) |name| name.string else "unknown",
                    },
                );
            } else if (!codegen.current.?.function.?.type_def.resolved_type.?.Function.return_type.eql(value.type_def.?)) {
                try codegen.reportTypeCheckAt(
                    codegen.current.?.function.?.type_def.resolved_type.?.Function.return_type,
                    value.type_def.?,
                    "Return value",
                    value.location,
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

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
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

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    init_declarations: std.ArrayList(*VarDeclarationNode),
    condition: *ParseNode,
    post_loop: std.ArrayList(*ParseNode),
    body: *ParseNode,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, _breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, _breaks);

        var self = Self.cast(node).?;

        for (self.init_declarations.items) |var_declaration| {
            _ = try var_declaration.node.toByteCode(&var_declaration.node, codegen, _breaks);
        }

        const loop_start: usize = codegen.currentCode();

        if (self.condition.type_def == null or self.condition.type_def.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(self.condition.location, "Unknown type.");
        }

        if (self.condition.type_def.?.def_type != .Bool) {
            try codegen.reportErrorAt(self.condition.location, "`for` condition must be bool");
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
                try codegen.reportErrorAt(expr.location, "Unknown type");
            }

            _ = try expr.toByteCode(expr, codegen, _breaks);
            try codegen.emitOpCode(expr.location, .OP_POP);
        }

        try codegen.emitLoop(self.node.location, loop_start);

        try codegen.patchJump(body_jump);

        var breaks: std.ArrayList(usize) = std.ArrayList(usize).init(codegen.allocator);
        defer breaks.deinit();

        _ = try self.body.toByteCode(self.body, codegen, &breaks);

        try codegen.emitLoop(self.node.location, expr_loop);

        try codegen.patchJump(exit_jump);

        try codegen.emitOpCode(self.node.location, .OP_POP); // Pop condition

        // Patch breaks
        for (breaks.items) |jump| {
            try codegen.patchJumpOrLoop(jump, loop_start);
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"For\", \"init_declarations\": [");

        for (self.init_declarations.items) |var_declaration, i| {
            try var_declaration.node.toJson(&var_declaration.node, out);

            if (i < self.init_declarations.items.len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], \"condition\": ");

        try self.condition.toJson(self.condition, out);

        try out.writeAll(", \"post_loop\": [");

        for (self.post_loop.items) |expression| {
            try expression.toJson(expression, out);
            try out.writeAll(", ");
        }

        try out.writeAll("], \"body\": ");

        try self.body.toJson(self.body, out);

        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
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

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    key: ?*VarDeclarationNode = null,
    value: *VarDeclarationNode,
    iterable: *ParseNode,
    block: *ParseNode,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, _breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, _breaks);

        var self = Self.cast(node).?;

        // Type checking
        if (self.iterable.type_def == null or self.iterable.type_def.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(self.iterable.location, "Unknown type.");
        } else {
            if (self.key) |key| {
                if (key.type_def == null or key.type_def.?.def_type == .Placeholder) {
                    try codegen.reportErrorAt(key.node.location, "Unknown type.");
                }

                switch (self.iterable.type_def.?.def_type) {
                    .String, .List => {
                        if (key.type_def.?.def_type != .Number) {
                            try codegen.reportErrorAt(key.node.location, "Expected `num`.");
                        }
                    },
                    .Map => {
                        if (!self.iterable.type_def.?.resolved_type.?.Map.key_type.eql(key.type_def.?)) {
                            try codegen.reportTypeCheckAt(self.iterable.type_def.?.resolved_type.?.Map.key_type, key.type_def.?, "Bad key type", key.node.location);
                        }
                    },
                    .Enum => try codegen.reportErrorAt(key.node.location, "No key available when iterating over enum."),
                    else => try codegen.reportErrorAt(self.iterable.location, "Not iterable."),
                }
            }

            if (self.value.type_def == null or self.value.type_def.?.def_type == .Placeholder) {
                try codegen.reportErrorAt(self.value.node.location, "Unknown type.");
            }

            switch (self.iterable.type_def.?.def_type) {
                .Map => {
                    if (!self.iterable.type_def.?.resolved_type.?.Map.value_type.eql(self.value.type_def.?)) {
                        try codegen.reportTypeCheckAt(
                            self.iterable.type_def.?.resolved_type.?.Map.value_type,
                            self.value.type_def.?,
                            "Bad value type",
                            self.value.node.location,
                        );
                    }
                },
                .List => {
                    if (!self.iterable.type_def.?.resolved_type.?.List.item_type.eql(self.value.type_def.?)) {
                        try codegen.reportTypeCheckAt(
                            self.iterable.type_def.?.resolved_type.?.List.item_type,
                            self.value.type_def.?,
                            "Bad value type",
                            self.value.node.location,
                        );
                    }
                },
                .String => {
                    if (self.value.type_def.?.def_type != .String) {
                        try codegen.reportErrorAt(self.value.node.location, "Expected `str`.");
                    }
                },
                .Enum => {
                    const iterable_type = try self.iterable.type_def.?.toInstance(codegen.allocator, codegen.type_registry);
                    if (!iterable_type.eql(self.value.type_def.?)) {
                        try codegen.reportTypeCheckAt(
                            iterable_type,
                            self.value.type_def.?,
                            "Bad value type",
                            self.value.node.location,
                        );
                    }
                },
                else => try codegen.reportErrorAt(self.iterable.location, "Not iterable."),
            }
        }

        if (self.key) |key| {
            _ = try key.node.toByteCode(&key.node, codegen, _breaks);
        }
        _ = try self.value.node.toByteCode(&self.value.node, codegen, _breaks);
        _ = try self.iterable.toByteCode(self.iterable, codegen, _breaks);

        const loop_start: usize = codegen.currentCode();

        // Calls `next` and update key and value locals
        try codegen.emitOpCode(self.node.location, .OP_FOREACH);

        // If next key is null, exit loop
        try codegen.emitCodeArg(self.node.location, .OP_GET_LOCAL, @intCast(u24, (self.key orelse self.value).slot));
        try codegen.emitOpCode(self.node.location, .OP_NULL);
        try codegen.emitOpCode(self.node.location, .OP_EQUAL);
        try codegen.emitOpCode(self.node.location, .OP_NOT);
        const exit_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
        try codegen.emitOpCode(self.node.location, .OP_POP); // Pop condition result

        var breaks: std.ArrayList(usize) = std.ArrayList(usize).init(codegen.allocator);
        defer breaks.deinit();

        _ = try self.block.toByteCode(self.block, codegen, &breaks);

        try codegen.emitLoop(self.node.location, loop_start);

        // Patch condition jump
        try codegen.patchJump(exit_jump);

        // Patch breaks
        for (breaks.items) |jump| {
            try codegen.patchJumpOrLoop(jump, loop_start);
        }

        try codegen.emitOpCode(self.node.location, .OP_POP); // Pop condition result

        try node.patchOptJumps(codegen);
        // Should have key, [value,] iterable to pop
        assert(node.ends_scope != null and node.ends_scope.?.items.len >= 2);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"ForEach\", ");

        if (self.key) |key| {
            try out.writeAll("\"key\": ");
            try key.node.toJson(&key.node, out);
        }

        try out.writeAll(", \"value\": ");

        try self.value.node.toJson(&self.value.node, out);

        try out.writeAll(", \"iterable\": ");

        try self.iterable.toJson(self.iterable, out);

        try out.writeAll(", \"block\": ");

        try self.block.toJson(self.block, out);

        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
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

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    condition: *ParseNode,
    block: *ParseNode,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, _breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, _breaks);

        var self = Self.cast(node).?;

        const loop_start: usize = codegen.currentCode();

        if (self.condition.type_def == null or self.condition.type_def.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(self.condition.location, "Unknown type.");
        }

        if (self.condition.type_def.?.def_type != .Bool) {
            try codegen.reportErrorAt(self.condition.location, "`while` condition must be bool");
        }

        _ = try self.condition.toByteCode(self.condition, codegen, _breaks);

        const exit_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
        try codegen.emitOpCode(self.node.location, .OP_POP);

        var breaks: std.ArrayList(usize) = std.ArrayList(usize).init(codegen.allocator);
        defer breaks.deinit();

        _ = try self.block.toByteCode(self.block, codegen, &breaks);

        try codegen.emitLoop(self.node.location, loop_start);
        try codegen.patchJump(exit_jump);

        try codegen.emitOpCode(self.node.location, .OP_POP); // Pop condition (is not necessary if broke out of the loop)

        // Patch breaks
        for (breaks.items) |jump| {
            try codegen.patchJumpOrLoop(jump, loop_start);
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"While\", \"condition\": ");

        try self.condition.toJson(self.condition, out);

        try out.writeAll(", \"block\": ");

        try self.block.toJson(self.block, out);

        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
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

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    condition: *ParseNode,
    block: *ParseNode,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, _breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, _breaks);

        var self = Self.cast(node).?;

        const loop_start: usize = codegen.currentCode();

        var breaks: std.ArrayList(usize) = std.ArrayList(usize).init(codegen.allocator);
        defer breaks.deinit();

        _ = try self.block.toByteCode(self.block, codegen, &breaks);

        if (self.condition.type_def == null or self.condition.type_def.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(self.condition.location, "Unknown type.");
        }

        if (self.condition.type_def.?.def_type != .Bool) {
            try codegen.reportErrorAt(self.condition.location, "`do` condition must be bool");
        }

        _ = try self.condition.toByteCode(self.condition, codegen, &breaks);

        try codegen.emitOpCode(self.node.location, .OP_NOT);
        const exit_jump: usize = try codegen.emitJump(self.node.location, .OP_JUMP_IF_FALSE);
        try codegen.emitOpCode(self.node.location, .OP_POP);

        try codegen.emitLoop(self.node.location, loop_start);
        try codegen.patchJump(exit_jump);

        try codegen.emitOpCode(self.node.location, .OP_POP); // Pop condition

        // Patch breaks
        for (breaks.items) |jump| {
            try codegen.patchJumpOrLoop(jump, loop_start);
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"DoUntil\", \"condition\": ");

        try self.condition.toJson(self.condition, out);

        try out.writeAll(", \"block\": ");

        try self.block.toJson(self.block, out);

        try out.writeAll(", ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
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

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    statements: std.ArrayList(*ParseNode),

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        for (self.statements.items) |statement| {
            _ = try statement.toByteCode(statement, codegen, breaks);
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.writeAll("{\"node\": \"Block\", \"statements\": [");

        for (self.statements.items) |statement, i| {
            try statement.toJson(statement, out);

            if (i < self.statements.items.len - 1) {
                try out.writeAll(",");
            }
        }

        try out.writeAll("], ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
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

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .Block) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};

pub const SuperNode = struct {
    const Self = @This();

    node: ParseNode = .{
        .node_type = .Super,
        .toJson = stringify,
        .toByteCode = generate,
    },

    identifier: Token,
    member_type_def: ?*ObjTypeDef = null,
    // if call, CallNode will fetch super
    super: ?*NamedVariableNode = null,
    this: *NamedVariableNode,
    call: ?*CallNode = null,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        _ = try self.this.node.toByteCode(&self.this.node, codegen, breaks);

        if (self.call) |call| {
            _ = try call.node.toByteCode(&call.node, codegen, breaks);
        } else {
            assert(self.super != null);

            _ = try self.super.?.node.toByteCode(&self.super.?.node, codegen, breaks);

            try codegen.emitCodeArg(self.node.location, .OP_GET_SUPER, try codegen.identifierConstant(self.identifier.lexeme));
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Super\", \"member_name\": \"{s}\", ", .{self.identifier.lexeme});

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .Super) {
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
    },

    callee: *ParseNode,
    identifier: Token,
    member_type_def: ?*ObjTypeDef = null,
    value: ?*ParseNode = null,
    call: ?*CallNode = null,
    enum_index: ?usize = null,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        _ = try self.callee.toByteCode(self.callee, codegen, breaks);

        const callee_type = self.callee.type_def.?;

        if (callee_type.def_type == .Placeholder) {
            try codegen.reportErrorAt(node.location, "Unknown type");
        }

        // zig fmt: off
        if (callee_type.def_type != .ObjectInstance
            and callee_type.def_type != .Object
            and callee_type.def_type != .Enum
            and callee_type.def_type != .EnumInstance
            and callee_type.def_type != .List
            and callee_type.def_type != .Map
            and callee_type.def_type != .String) {
            try codegen.reportErrorAt(node.location, "Doesn't have field access.");
        }
        // zig fmt: on

        switch (callee_type.def_type) {
            .String => {
                if (self.call) |call_node| { // Call
                    try codegen.emitOpCode(self.node.location, .OP_COPY);
                    _ = try call_node.node.toByteCode(&call_node.node, codegen, breaks);
                } else { // Expression
                    try codegen.emitCodeArg(self.node.location, .OP_GET_PROPERTY, try codegen.identifierConstant(self.identifier.lexeme));
                }
            },
            .ObjectInstance, .Object => {
                if (self.value) |value| {
                    if (value.type_def == null or value.type_def.?.def_type == .Placeholder) {
                        try codegen.reportErrorAt(value.location, "Unknown type");
                    }

                    _ = try value.toByteCode(value, codegen, breaks);

                    try codegen.emitCodeArg(self.node.location, .OP_SET_PROPERTY, try codegen.identifierConstant(self.identifier.lexeme));
                } else if (self.call) |call| {
                    // Static call
                    if (callee_type.def_type == .Object) {
                        try codegen.emitCodeArg(node.location, .OP_GET_PROPERTY, try codegen.identifierConstant(self.identifier.lexeme));
                    }

                    _ = try call.node.toByteCode(&call.node, codegen, breaks);
                } else {
                    try codegen.emitCodeArg(self.node.location, .OP_GET_PROPERTY, try codegen.identifierConstant(self.identifier.lexeme));
                }
            },
            .Enum => {
                try codegen.emitCodeArg(self.node.location, .OP_GET_ENUM_CASE, @intCast(u24, self.enum_index.?));
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
                    try codegen.emitCodeArg(self.node.location, .OP_GET_PROPERTY, try codegen.identifierConstant(self.identifier.lexeme));
                }
            },
            else => unreachable,
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
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
            try out.writeAll("\"value\": ");
            try call.toNode().toJson(call.toNode(), out);
            try out.writeAll(", ");
        }

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    object: *ParseNode, // Should mostly be a NamedVariableNode
    properties: std.StringHashMap(*ParseNode),

    fn getSuperField(self: *Self, object: *ObjTypeDef, name: []const u8) ?*ObjTypeDef {
        const obj_def: ObjObject.ObjectDef = object.resolved_type.?.Object;
        if (obj_def.fields.get(name)) |obj_field| {
            return obj_field;
        } else if (obj_def.super) |obj_super| {
            return self.getSuperField(obj_super, name);
        }

        return null;
    }

    fn checkOmittedProperty(self: *Self, codegen: *CodeGen, obj_def: ObjObject.ObjectDef, init_properties: std.StringHashMap(void)) anyerror!void {
        var it = obj_def.fields.iterator();
        while (it.next()) |kv| {
            // If ommitted in initialization and doesn't have default value
            if (init_properties.get(kv.key_ptr.*) == null and obj_def.fields_defaults.get(kv.key_ptr.*) == null) {
                try codegen.reportErrorFmt(self.node.location, "Property `{s}` was not initialized and has no default value", .{kv.key_ptr.*});
            }
        }

        if (obj_def.super) |super_def| {
            try self.checkOmittedProperty(codegen, super_def.resolved_type.?.Object, init_properties);
        }
    }

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        _ = try self.object.toByteCode(self.object, codegen, breaks);

        try codegen.emitOpCode(self.node.location, .OP_INSTANCE);

        if (node.type_def == null or node.type_def.?.def_type == .Placeholder) {
            try codegen.reportErrorAt(node.location, "Unknown type.");
        } else if (node.type_def.?.def_type != .ObjectInstance) {
            try codegen.reportErrorAt(node.location, "Expected an object or a class.");
        }

        const object_type = node.type_def.?.resolved_type.?.ObjectInstance;
        const obj_def = object_type.resolved_type.?.Object;

        // To keep track of what's been initialized or not by this statement
        var init_properties = std.StringHashMap(void).init(codegen.allocator);
        defer init_properties.deinit();

        var it = self.properties.iterator();
        while (it.next()) |kv| {
            const property_name = kv.key_ptr.*;
            const property_name_constant: u24 = try codegen.identifierConstant(property_name);
            const value = kv.value_ptr.*;

            if (obj_def.fields.get(property_name) orelse self.getSuperField(object_type, property_name)) |prop| {
                try codegen.emitCodeArg(self.node.location, .OP_COPY, 0); // Will be popped by OP_SET_PROPERTY

                if (value.type_def == null or value.type_def.?.def_type == .Placeholder) {
                    try codegen.reportErrorAt(value.location, "Unknown type.");
                } else if (!prop.eql(value.type_def.?)) {
                    try codegen.reportTypeCheckAt(prop, value.type_def.?, "Wrong property type", value.location);
                }

                _ = try value.toByteCode(value, codegen, breaks);

                try init_properties.put(property_name, {});

                try codegen.emitCodeArg(self.node.location, .OP_SET_PROPERTY, property_name_constant);
                try codegen.emitOpCode(self.node.location, .OP_POP); // Pop property value
            } else {
                try codegen.reportErrorFmt(node.location, "Property `{s}` does not exists", .{property_name});
            }
        }

        // Did we initialized all properties without a default value?
        try self.checkOmittedProperty(codegen, obj_def, init_properties);

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
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

        try out.writeAll("}, ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn init(allocator: Allocator, object: *ParseNode) Self {
        return Self{
            .object = object,
            .properties = std.StringHashMap(*ParseNode).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.properties.deinit();
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
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
    },

    parent_slot: ?usize = null,
    slot: usize,
    methods: std.StringHashMap(*ParseNode),
    properties: std.StringHashMap(?*ParseNode),
    properties_type: std.StringHashMap(*ObjTypeDef),
    docblocks: std.StringHashMap(?Token),

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        const object_type = node.type_def.?;
        const name_constant = try codegen.makeConstant(object_type.resolved_type.?.Object.name.toValue());
        const object_type_constant = try codegen.makeConstant(object_type.toValue());

        // Put  object on the stack and define global with it
        try codegen.emitCodeArg(self.node.location, .OP_OBJECT, name_constant);
        try codegen.emit(self.node.location, @intCast(u32, object_type_constant));
        try codegen.emitCodeArg(self.node.location, .OP_DEFINE_GLOBAL, @intCast(u24, self.slot));

        // Does it inherits from another object/class
        if (self.parent_slot) |parent_slot| {
            // Put parent on the stack as the `super` local
            try codegen.emitCodeArg(self.node.location, .OP_GET_GLOBAL, @intCast(u24, parent_slot));

            // Actually do the inheritance
            try codegen.emitCodeArg(self.node.location, .OP_GET_GLOBAL, @intCast(u24, self.slot));
            try codegen.emitCodeArg(self.node.location, .OP_INHERIT, @intCast(u24, parent_slot));
        }

        // Put the object on the stack to set its fields
        try codegen.emitCodeArg(self.node.location, .OP_GET_GLOBAL, @intCast(u24, self.slot));

        // Methods
        var it = self.methods.iterator();
        while (it.next()) |kv| {
            const member_name = kv.key_ptr.*;
            const member = kv.value_ptr.*;
            const member_name_constant: u24 = try codegen.identifierConstant(member_name);

            if (member.type_def == null or member.type_def.?.def_type == .Placeholder) {
                try codegen.reportErrorAt(member.location, "Unknown type");
            }

            const is_static = object_type.resolved_type.?.Object.static_fields.get(member_name) != null;

            _ = try member.toByteCode(member, codegen, breaks);
            try codegen.emitCodeArg(self.node.location, if (is_static) .OP_PROPERTY else .OP_METHOD, member_name_constant);
        }

        // Properties
        var it2 = self.properties.iterator();
        while (it2.next()) |kv| {
            const member_name = kv.key_ptr.*;
            const member = kv.value_ptr.*;
            const member_name_constant: u24 = try codegen.identifierConstant(member_name);
            const is_static = object_type.resolved_type.?.Object.static_fields.get(member_name) != null;
            const property_type = object_type.resolved_type.?.Object.fields.get(member_name) orelse object_type.resolved_type.?.Object.static_fields.get(member_name);

            assert(property_type != null);

            // Create property default value
            if (member) |default| {
                if (default.type_def == null or default.type_def.?.def_type == .Placeholder) {
                    try codegen.reportErrorAt(default.location, "Unknown type");
                } else if (!property_type.?.eql(default.type_def.?)) {
                    try codegen.reportTypeCheckAt(property_type.?, default.type_def.?, "Wrong property default value type", default.location);
                }

                if (is_static) {
                    try codegen.emitOpCode(self.node.location, .OP_COPY);
                }

                _ = try default.toByteCode(default, codegen, breaks);

                // Create property default value
                if (is_static) {
                    try codegen.emitCodeArg(self.node.location, .OP_SET_PROPERTY, member_name_constant);
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

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
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
                "\"{s}\": {{\"type_def\": \"{s}\", \"docblock\": \"{s}\"}}",
                .{
                    kv.key_ptr.*,
                    kv.value_ptr.*.toString(std.heap.c_allocator),
                    if (self.docblocks.get(kv.key_ptr.*).?) |docblock| docblock.literal_string else "",
                },
            );

            if (i < self.properties_type.count() - 1) {
                try out.writeAll(",");
            }

            i += 1;
        }

        try out.writeAll("}, ");

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
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

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .ObjectDeclaration) {
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
    },

    identifier: Token,
    alias: ?Token = null,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, _: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Export\", \"identifier\": \"{s}\", ", .{self.identifier.lexeme});

        if (self.alias) |alias| {
            try out.print("\"alias\": \"{s}\", ", .{alias.lexeme});
        }

        try ParseNode.stringify(node, out);

        try out.writeAll("}");
    }

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .Export) {
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
    },

    imported_symbols: ?std.StringHashMap(void) = null,
    prefix: ?Token = null,
    path: Token,
    import: ?Parser.ScriptImport,

    pub fn generate(node: *ParseNode, codegen: *CodeGen, breaks: ?*std.ArrayList(usize)) anyerror!?*ObjFunction {
        _ = try node.generate(codegen, breaks);

        var self = Self.cast(node).?;

        if (self.import) |import| {
            _ = try import.function.toByteCode(import.function, codegen, breaks);
            try codegen.emitOpCode(self.node.location, .OP_IMPORT);
        }

        try node.patchOptJumps(codegen);
        try node.endScope(codegen);

        return null;
    }

    fn stringify(node: *ParseNode, out: std.ArrayList(u8).Writer) anyerror!void {
        var self = Self.cast(node).?;

        try out.print("{{\"node\": \"Import\", \"path\": \"{s}\"", .{self.path.literal_string});

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

    pub fn toNode(self: *Self) *ParseNode {
        return &self.node;
    }

    pub fn cast(node: *ParseNode) ?*Self {
        if (node.node_type != .Import) {
            return null;
        }

        return @fieldParentPtr(Self, "node", node);
    }
};
