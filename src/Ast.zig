const std = @import("std");
const obj = @import("obj.zig");
const Token = @import("Token.zig");
const Chunk = @import("Chunk.zig");
const v = @import("value.zig");
const Value = v.Value;
const FFI = @import("FFI.zig");
const Parser = @import("Parser.zig");
const GarbageCollector = @import("memory.zig").GarbageCollector;
// TODO: cleanup Error sets!
const Error = @import("Codegen.zig").Error;

const Self = @This();

// Since the AST must live for the whole program lifetime (can be used by the JIT)
// there's no deinit functions. Everything should be allocated with an ArenaAllocator

pub const TokenIndex = u32;
pub const TokenList = std.MultiArrayList(Token);
pub const NodeList = std.MultiArrayList(Node);

allocator: std.mem.Allocator,
tokens: TokenList,
nodes: NodeList,
root: ?Node.Index = null,

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .tokens = TokenList{},
        .nodes = NodeList{},
    };
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    self.tokens.deinit(allocator);
    self.nodes.deinit(allocator);
}

pub inline fn appendNode(self: *Self, node: Node) !Node.Index {
    try self.nodes.append(self.allocator, node);

    return @intCast(self.nodes.len - 1);
}

pub inline fn appendToken(self: *Self, token: Token) !TokenIndex {
    try self.tokens.append(self.allocator, token);

    return @intCast(self.tokens.len - 1);
}

pub fn jsonStringify(self: *const Self, jw: anytype) !void {
    try self.nodes.get(self.root.?).jsonStringify(self, jw);
}

pub const Node = struct {
    tag: Tag,
    /// First token of this node
    location: TokenIndex,
    /// Last token of this node
    end_location: TokenIndex,
    /// Docblock if any
    docblock: ?TokenIndex = null,

    /// If null, either its a statement or its a reference to something unknown that should ultimately raise a compile error
    type_def: ?*obj.ObjTypeDef = null,
    /// Wether optional jumps must be patch before generate this node bytecode
    patch_opt_jumps: bool = false,
    /// Does this node closes a scope
    ends_scope: ?[]const Chunk.OpCode = null,

    /// Data related to this node
    components: Components,

    /// To avoid generating a node const value multiple times
    value: ?Value = null,

    /// How many time it was visited at runtime (used to decide wether its a hotspot that needs to be compiled)
    count: usize = 0,

    /// Wether its blacklisted or already compiled
    compilable: bool = true,

    pub const Index = u32;

    pub fn jsonStringify(self: *const Node, ast: *const Self, jw: anytype) !void {
        try std.json.stringify(
            .{
                .tag = @tagName(self.tag),
                .location = ast.tokens.get(self.location),
                .end_location = ast.tokens.get(self.end_location),
                .dockblock = if (self.docblock) |docblock|
                    ast.tokens.get(docblock)
                else
                    null,
                .type_def = self.type_def,
                .patch_opt_jumps = self.patch_opt_jumps,
                .ends_scope = self.ends_scope,
                .components = try self.components.jsonStringify(ast, jw),
            },
            jw.options,
            jw.stream,
        );
    }

    pub const Tag = enum(u8) {
        AnonymousObjectType,
        As,
        AsyncCall,
        Binary,
        Block,
        BlockExpression,
        Boolean,
        Break,
        Call,
        Continue,
        Dot,
        DoUntil,
        Enum,
        Export,
        Expression,
        FiberType,
        Float,
        For,
        ForceUnwrap,
        ForEach,
        Function,
        FunctionType,
        FunDeclaration,
        GenericResolve,
        GenericResolveType,
        GenericType,
        Grouping,
        If,
        Import,
        Integer,
        Is,
        List,
        ListType,
        Map,
        MapType,
        Namespace,
        NamedVariable,
        Null,
        ObjectDeclaration,
        ObjectInit,
        Out,
        Pattern,
        ProtocolDeclaration,
        Range,
        Resolve,
        Resume,
        Return,
        SimpleType,
        String,
        StringLiteral,
        Subscript,
        Throw,
        Try,
        TypeExpression,
        TypeOfExpression,
        Unary,
        Unwrap,
        UserType,
        VarDeclaration,
        Void,
        While,
        Yield,
        Zdef,

        pub fn isHotspot(self: Tag) bool {
            return switch (self) {
                .While,
                .For,
                .ForEach,
                => true,
                else => false,
            };
        }
    };

    pub const Components = union(Tag) {
        AnonymousObjectType: AnonymousObjectType,
        As: IsAs,
        AsyncCall: Node.Index,
        Binary: Binary,
        Block: []const Node.Index,
        BlockExpression: []const Node.Index,
        Boolean: bool,
        Break: ?Node.Index,
        Call: Call,
        Continue: ?Node.Index,
        Dot: Dot,
        DoUntil: WhileDoUntil,
        Enum: Enum,
        Export: Export,
        Expression: Node.Index,
        FiberType: FiberType,
        Float: v.Float,
        For: For,
        ForceUnwrap: Unwrap,
        ForEach: ForEach,
        Function: Function,
        FunctionType: FunctionType,
        FunDeclaration: FunDeclaration,
        GenericResolve: Node.Index,
        GenericResolveType: GenericResolveType,
        GenericType: void,
        Grouping: Node.Index,
        If: If,
        Import: Import,
        Integer: v.Integer,
        Is: IsAs,
        List: List,
        ListType: ListType,
        Map: Map,
        MapType: MapType,
        Namespace: TokenIndex,
        NamedVariable: NamedVariable,
        Null: void,
        ObjectDeclaration: ObjectDeclaration,
        ObjectInit: ObjectInit,
        Out: Node.Index,
        Pattern: *obj.ObjPattern,
        ProtocolDeclaration: ProtocolDeclaration,
        Range: Range,
        Resolve: Node.Index,
        Resume: Node.Index,
        Return: Return,
        // The type should be taken from the type_def field and not the token
        SimpleType: void,
        String: []const Node.Index,
        StringLiteral: *obj.ObjString,
        Subscript: Subscript,
        Throw: Throw,
        Try: Try,
        TypeExpression: Node.Index,
        TypeOfExpression: Node.Index,
        Unary: Unary,
        Unwrap: Unwrap,
        UserType: UserType,
        VarDeclaration: VarDeclaration,
        Void: void,
        While: WhileDoUntil,
        Yield: Node.Index,
        Zdef: Zdef,

        pub fn jsonStringify(self: Components, ast: *const Self, jw: anytype) !void {
            switch (self) {
                .Boolean => try jw.write(self.Boolean),
                .Float => try jw.write(self.Float),
                .Integer => try jw.write(self.Integer),
                .Pattern => try jw.write(self.Pattern.source),
                .StringLiteral => try jw.write(self.StringLiteral.string),
                .AnonymousObjectType => try self.AnonymousObjectType.jsonStringify(ast, jw),
                .As => try self.As.jsonStringify(ast, jw),
                .Is => try self.Is.jsonStringify(ast, jw),
                .Binary => try self.Binary.jsonStringify(ast, jw),
                .Call => try self.Call.jsonStringify(ast, jw),
                .Dot => try self.Dot.jsonStringify(ast, jw),
                .Enum => try self.Enum.jsonStringify(ast, jw),
                .Export => try self.Export.jsonStringify(ast, jw),
                .FiberType => try self.FiberType.jsonStringify(ast, jw),
                .For => try self.For.jsonStringify(ast, jw),
                .ForEach => try self.ForEach.jsonStringify(ast, jw),
                .Function => try self.Functionh.jsonStringify(ast, jw),
                .FunctionType => try self.FunctionTypeh.jsonStringify(ast, jw),
                .FunDeclaration => try self.FunDeclaration.jsonStringify(ast, jw),
                .GenericResolveType => try self.GenericResolveType.jsonStringify(ast, jw),
                .If => try self.If.jsonStringify(ast, jw),
                .Import => try self.Import.jsonStringify(ast, jw),
                .List => try self.List.jsonStringify(ast, jw),
                .ListType => try self.ListType.jsonStringify(ast, jw),
                .Map => try self.Map.jsonStringify(ast, jw),
                .MapType => try self.MapType.jsonStringify(ast, jw),
                .NamedVariable => try self.NamedVariable.jsonStringify(ast, jw),
                .ObjectDeclaration => try self.ObjectDeclaration.jsonStringify(ast, jw),
                .ObjectInit => try self.ObjectInit.jsonStringify(ast, jw),
                .ProtocolDeclaration => try self.ProtocolDeclaration.jsonStringify(ast, jw),
                .Range => try self.Range.jsonStringify(ast, jw),
                .Return => try self.Return.jsonStringify(ast, jw),
                .Subscript => try self.Subscript.jsonStringify(ast, jw),
                .Throw => try self.Throw.jsonStringify(ast, jw),
                .Try => try self.Try.jsonStringify(ast, jw),
                .Unary => try self.Unary.jsonStringify(ast, jw),
                .UserType => try self.UserType.jsonStringify(ast, jw),
                .VarDeclaration => try self.VarDeclaration.jsonStringify(ast, jw),
                .While => try self.While.jsonStringify(ast, jw),
                .DoUntil => try self.DoUntil.jsonStringify(ast, jw),
                .Zdef => try self.Zdef.jsonStringify(ast, jw),
                .AsyncCall => try ast.nodes.get(self.AsyncCall).jsonStringify(ast, jw),
                .Expression => try ast.nodes.get(self.Expression).jsonStringify(ast, jw),
                .GenericResolve => try ast.nodes.get(self.GenericResolve).jsonStringify(ast, jw),
                .Grouping => try ast.nodes.get(self.Grouping).jsonStringify(ast, jw),
                .Out => try ast.nodes.get(self.Out).jsonStringify(ast, jw),
                .Resolve => try ast.nodes.get(self.Resolve).jsonStringify(ast, jw),
                .Resume => try ast.nodes.get(self.Resume).jsonStringify(ast, jw),
                .TypeExpression => try ast.nodes.get(self.TypeExpression).jsonStringify(ast, jw),
                .TypeOfExpression => try ast.nodes.get(self.TypeOfExpression).jsonStringify(ast, jw),
                .Yield => try ast.nodes.get(self.Yield).jsonStringify(ast, jw),
                .Break => try ast.nodes.get(self.Break).jsonStringify(ast, jw),
                .Continue => try ast.nodes.get(self.Continue).jsonStringify(ast, jw),
                .Unwrap => try ast.nodes.get(self.Unwrap).jsonStringify(ast, jw),
                .ForceUnwrap => try ast.nodes.get(self.ForceUnwrap).jsonStringify(ast, jw),
                .Namespace => try jw.write(ast.tokens.get(self.Namespace)),
                .GenericType,
                .Null,
                .SimpleType,
                .Void,
                => try jw.write(.{}),
                .Block => {
                    try jw.beginArray();
                    for (self.Block) |node| try ast.nodes.get(node).jsonStringify(ast, jw);
                    try jw.endArray();
                },
                .BlockExpression => {
                    try jw.beginArray();
                    for (self.BlockExpression) |node| try ast.nodes.get(node).jsonStringify(ast, jw);
                    try jw.endArray();
                },
                .String => {
                    try jw.beginArray();
                    for (self.String) |node| try ast.nodes.get(node).jsonStringify(ast, jw);
                    try jw.endArray();
                },
            }
        }
    };
};

pub fn usesFiber(self: Self, node: Node.Index, seen: *std.AutoHashMap(Node.Index, void)) !bool {
    if (seen.get(node) != null) {
        return false;
    }

    try seen.put(node, {});

    const components = self.nodes.items(.components)[node];
    return switch (self.nodes.items(.tag)[node]) {
        .As => try self.usesFiber(components.As.left, seen),
        .AsyncCall,
        .Resolve,
        .Resume,
        .Yield,
        => true,
        .Binary => try self.usesFiber(components.Binary.left, seen) or try self.usesFiber(components.Binary.right, seen),
        .Block => blk: {
            for (components.Block) |stmt| {
                if (try self.usesFiber(stmt, seen)) {
                    break :blk true;
                }
            }

            break :blk false;
        },
        .BlockExpression => blk: {
            for (components.BlockExpression) |stmt| {
                if (try self.usesFiber(stmt, seen)) {
                    break :blk true;
                }
            }

            break :blk false;
        },
        .String => blk: {
            for (components.String) |stmt| {
                if (try self.usesFiber(stmt, seen)) {
                    break :blk true;
                }
            }

            break :blk false;
        },
        .Call => call: {
            if (try self.usesFiber(components.Call.callee, seen) or (components.Call.catch_default != null and try self.usesFiber(components.Call.catch_default.?, seen))) {
                break :call true;
            }

            for (components.Call.arguments) |argument| {
                if (try self.usesFiber(argument.value, seen)) {
                    break :call true;
                }
            }

            break :call false;
        },
        .Dot => dot: {
            if (try self.usesFiber(components.Dot.callee, seen)) {
                break :dot true;
            }

            switch (components.Dot.member_kind) {
                .Value => {
                    if (try self.usesFiber(components.Dot.value_or_call_or_enum.Value, seen)) {
                        break :dot true;
                    }
                },
                .Call => {
                    if (try self.usesFiber(components.Dot.value_or_call_or_enum.Call, seen)) {
                        break :dot true;
                    }
                },
                else => {},
            }

            break :dot false;
        },
        .DoUntil => try self.usesFiber(components.DoUntil.condition, seen) or try self.usesFiber(components.DoUntil.body, seen),
        .Expression => try self.usesFiber(components.Expression, seen),
        .For => for_loop: {
            if (try self.usesFiber(components.For.condition, seen) or try self.usesFiber(components.For.body, seen)) {
                break :for_loop true;
            }

            for (components.For.init_declarations) |decl| {
                if (try self.usesFiber(decl, seen)) {
                    break :for_loop true;
                }
            }

            for (components.For.post_loop) |decl| {
                if (try self.usesFiber(decl, seen)) {
                    break :for_loop true;
                }
            }

            break :for_loop false;
        },
        .ForceUnwrap => try self.usesFiber(components.ForceUnwrap.unwrapped, seen),
        .ForEach => try self.usesFiber(components.ForEach.iterable, seen) or try self.usesFiber(components.ForEach.key, seen) or try self.usesFiber(components.ForEach.value, seen) or try self.usesFiber(components.ForEach.body, seen),
        .Function => components.Function.body != null and try self.usesFiber(components.Function.body.?, seen),
        .Grouping => try self.usesFiber(components.Grouping, seen),
        .If => try self.usesFiber(components.If.condition, seen) or try self.usesFiber(components.If.body, seen) or (components.If.else_branch != null and try self.usesFiber(components.If.else_branch.?, seen)),
        .Is => try self.usesFiber(components.Is.left, seen),
        .List => list: {
            for (components.List.items) |item| {
                if (try self.usesFiber(item, seen)) {
                    break :list true;
                }
            }

            break :list false;
        },
        .Map => map: {
            for (components.Map.entries) |entry| {
                if (try self.usesFiber(entry.key, seen) or try self.usesFiber(entry.value, seen)) {
                    break :map true;
                }
            }

            break :map false;
        },
        .NamedVariable => components.NamedVariable.value != null and try self.usesFiber(components.NamedVariable.value.?, seen),
        .ObjectInit => obj_init: {
            for (components.ObjectInit.properties) |property| {
                if (try self.usesFiber(property.value, seen)) {
                    break :obj_init true;
                }
            }

            break :obj_init false;
        },
        .Out => try self.usesFiber(components.Out, seen),
        .Range => try self.usesFiber(components.Range.low, seen) or try self.usesFiber(components.Range.high, seen),
        .Return => components.Return.value != null and try self.usesFiber(components.Return.value.?, seen),
        .Subscript => try self.usesFiber(components.Subscript.subscripted, seen) or try self.usesFiber(components.Subscript.index, seen) or (components.Subscript.value != null and try self.usesFiber(components.Subscript.value.?, seen)),
        .Throw => try self.usesFiber(components.Throw.expression, seen),
        .Try => @"try": {
            if (try self.usesFiber(components.Try.body, seen) or (components.Try.unconditional_clause != null and try self.usesFiber(components.Try.unconditional_clause.?, seen))) {
                break :@"try" true;
            }

            for (components.Try.clauses) |clause| {
                if (try self.usesFiber(clause.body, seen)) {
                    break :@"try" true;
                }
            }

            break :@"try" false;
        },
        .TypeOfExpression => try self.usesFiber(components.TypeOfExpression, seen),
        .Unary => try self.usesFiber(components.Unary.expression, seen),
        .Unwrap => try self.usesFiber(components.Unwrap.unwrapped, seen),
        .VarDeclaration => components.VarDeclaration.value != null and try self.usesFiber(components.VarDeclaration.value.?, seen),
        .While => try self.usesFiber(components.While.condition, seen) or try self.usesFiber(components.While.body, seen),
        else => false,
    };
}

pub const AnonymousObjectType = struct {
    fields: []const Field,

    pub const Field = struct {
        name: TokenIndex,
        type: Node.Index,
    };

    pub fn jsonStringify(self: AnonymousObjectType, ast: *const Self, jw: anytype) !void {
        try jw.beginArray();
        for (self.fields) |field| {
            try jw.beginObject();
            try jw.objectField("name");
            try std.json.stringify(
                ast.tokens.get(field.name),
                jw.options,
                jw.stream,
            );
            try jw.objectField("type");
            try ast.nodes.get(field.type).jsonStringify(ast, jw);
            try jw.endObject();
        }
        try jw.endArray();
    }
};

pub const Binary = struct {
    left: Node.Index,
    right: Node.Index,
    operator: Token.Type,

    pub fn jsonStringify(self: Binary, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("left");
        try ast.nodes.get(self.left).jsonStringify(ast, jw);
        try jw.objectField("right");
        try ast.nodes.get(self.right).jsonStringify(ast, jw);
        try jw.objectField("operator");
        try std.json.stringify(
            ast.tokens.get(self.operator),
            jw.options,
            jw.stream,
        );
        try jw.endObject();
    }
};

pub const Call = struct {
    is_async: bool,
    callee: Node.Index,
    // We need this because in a dot.call, callee is dot and its type will be == to call return type
    callee_type_def: *obj.ObjTypeDef,
    arguments: []const Argument,
    catch_default: ?Node.Index,
    tail_call: bool = false,

    pub const Argument = struct {
        name: ?TokenIndex,
        value: Node.Index,
    };

    pub fn jsonStringify(self: Call, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("is_async");
        try jw.valueStart();
        try jw.stream.writeAll(if (self.is_async) "true" else "false");
        try jw.valueDone();
        if (ast.nodes.items(.tag)[self.callee] != .Dot) {
            try jw.objectField("callee");
            try ast.nodes.get(self.callee).jsonStringify(ast, jw);
        }
        try jw.objectField("callee_type_def");
        try std.json.stringify(
            self.callee_type_def,
            jw.options,
            jw.stream,
        );
        try jw.objectField("arguments");
        try jw.beginArray();
        for (self.arguments) |arg| {
            try jw.beginObject();
            try jw.objectField("name");
            if (arg.name) |name| {
                try jw.stringValue(name);
            } else {
                try self.valueStart();
                try self.stream.writeAll("null");
                self.valueDone();
            }
            try jw.objectField("value");
            try ast.nodes.get(arg.value).jsonStringify(ast, jw);
            try jw.endObject();
        }
        try jw.endArray();
        try jw.objectField("catch_default");
        if (self.catch_default) |catch_default| {
            try ast.nodes.get(catch_default).jsonStringify(ast, jw);
        } else {
            try self.valueStart();
            try self.stream.writeAll("null");
            self.valueDone();
        }
        try jw.objectField("tail_call");
        try jw.valueStart();
        try jw.stream.writeAll(if (self.tail_call) "true" else "false");
        try jw.valueDone();
        try jw.endObject();
    }
};

pub const Dot = struct {
    pub const MemberKind = enum {
        Ref,
        Value,
        Call,
        EnumCase,
    };

    pub const Member = union(MemberKind) {
        Ref: void,
        Value: Node.Index,
        Call: Node.Index,
        EnumCase: u32,
    };

    callee: Node.Index,
    identifier: TokenIndex,
    member_kind: MemberKind,
    value_or_call_or_enum: Member,
    generic_resolve: ?Node.Index,
    member_type_def: *obj.ObjTypeDef,

    pub fn jsonStringify(self: Dot, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("callee");
        try ast.nodes.get(self.callee).jsonStringify(ast, jw);
        try jw.objectField("identifier");
        try std.json.stringify(
            ast.tokens.get(self.identifier),
            jw.options,
            jw.stream,
        );
        switch (self.member_kind) {
            .Ref => {},
            .Value => {
                try jw.objectField("value");
                try ast.nodes.get(self.value_or_call_or_enum.Value).jsonStringify(ast, jw);
            },
            .Call => {
                try jw.objectField("call");
                try ast.nodes.get(self.value_or_call_or_enum.Call).jsonStringify(ast, jw);
            },
            .EnumCase => {
                try jw.objectField("enum_case");
                try jw.valueStart();
                try jw.print("{}", .{self.value_or_call_or_enum.EnumCase});
                try jw.valueDone();
            },
        }
        try jw.objectField("generic_resolve");
        if (self.generic_resolve) |generic_resolve| {
            try ast.nodes.get(generic_resolve).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("member_type_def");
        try std.json.stringify(
            self.member_type_def,
            jw.options,
            jw.stream,
        );
        try jw.endObject();
    }
};

pub const Enum = struct {
    name: TokenIndex,
    case_type: ?Node.Index,
    slot: Slot,
    cases: []const Case,

    pub const Case = struct {
        name: TokenIndex,
        docblock: ?TokenIndex,
        value: ?Node.Index,
    };

    pub fn jsonStringify(self: Enum, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("name");
        try std.json.stringify(
            ast.tokens.get(self.name),
            jw.options,
            jw.stream,
        );
        try jw.objectField("case_type");
        if (self.case_type) |case_type| {
            try ast.nodes.get(case_type).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("slot");
        try jw.valueStart();
        try jw.stream.print("{}", .{self.slot});
        try jw.valueEnd();
        try jw.objectField("cases");
        try jw.beginArray();
        for (self.cases) |case| {
            try jw.beginObject();
            try jw.objectField("name");
            try std.json.stringify(
                ast.tokens.get(case.name),
                jw.options,
                jw.stream,
            );
            try jw.objectField("docblock");
            if (case.docblock) |db| {
                try std.json.stringify(
                    ast.tokens.get(db),
                    jw.options,
                    jw.stream,
                );
            } else {
                try jw.valueStart();
                try jw.stream.writeAll("null");
                try jw.valueDone();
            }
            try jw.objectField("value");
            if (case.value) |value| {
                try ast.nodes.get(value).jsonStringify(ast, jw);
            } else {
                try jw.valueStart();
                try jw.stream.writeAll("null");
                try jw.valueDone();
            }
            try jw.endObject();
        }
        try jw.endArray();
        try jw.endObject();
    }
};

pub const Export = struct {
    identifier: ?TokenIndex,
    alias: ?TokenIndex,
    declaration: ?Node.Index,

    pub fn jsonStringify(self: Export, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("identifier");
        if (self.identifier) |identifier| {
            try std.json.stringify(
                ast.tokens.get(identifier),
                jw.options,
                jw.stream,
            );
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("alias");
        if (self.alias) |alias| {
            try std.json.stringify(
                ast.tokens.get(alias),
                jw.options,
                jw.stream,
            );
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("declaration");
        if (self.declaration) |declaration| {
            try ast.nodes.get(declaration).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.endObject();
    }
};

pub const FiberType = struct {
    return_type: Node.Index,
    yield_type: Node.Index,

    pub fn jsonStringify(self: FiberType, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("return_type");
        try ast.nodes.get(self.return_type).jsonStringify(ast, jw);
        try jw.objectField("yield_type");
        try ast.nodes.get(self.yield_type).jsonStringify(ast, jw);
        try jw.endObject();
    }
};

pub const For = struct {
    init_declarations: []const Node.Index,
    condition: Node.Index,
    post_loop: []const Node.Index,
    body: Node.Index,
    label: ?TokenIndex,

    pub fn jsonStringify(self: For, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("init_declarations");
        try jw.beginArray();
        for (self.init_declarations) |decl| {
            try ast.nodes.get(decl).jsonStringify(ast, jw);
        }
        try jw.endArray();
        try jw.objectField("condition");
        try ast.nodes.get(self.condition).jsonStringify(ast, jw);
        try jw.objectField("post_loop");
        try jw.beginArray();
        for (self.post_loop) |stmt| {
            try ast.nodes.get(stmt).jsonStringify(ast, jw);
        }
        try jw.endArray();
        try jw.objectField("body");
        try ast.nodes.get(self.body).jsonStringify(ast, jw);
        try jw.objectField("label");
        if (self.label) |label| {
            try std.json.stringify(
                ast.tokens.get(label),
                jw.options,
                jw.stream,
            );
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.endObject();
    }
};

pub const ForEach = struct {
    iterable: Node.Index,
    key: Node.Index,
    value: Node.Index,
    body: Node.Index,
    key_omitted: bool,
    label: ?TokenIndex,

    pub fn jsonStringify(self: ForEach, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("iterable");
        try ast.nodes.get(self.iterable).jsonStringify(ast, jw);
        try jw.objectField("key");
        try ast.nodes.get(self.key).jsonStringify(ast, jw);
        try jw.objectField("value");
        try ast.nodes.get(self.value).jsonStringify(ast, jw);
        try jw.objectField("body");
        try ast.nodes.get(self.body).jsonStringify(ast, jw);
        try jw.objectField("key_omitted");
        try jw.valueStart();
        try jw.stream.writeAll(if (self.key_omitted) "true" else "false");
        try jw.valueDone();
        try jw.objectField("label");
        if (self.label) |label| {
            try std.json.stringify(
                ast.tokens.get(label),
                jw.options,
                jw.stream,
            );
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.endObject();
    }
};

pub const Function = struct {
    var next_id: usize = 0;

    pub fn nextId() usize {
        Function.next_id += 1;

        return Function.next_id;
    }

    id: usize,
    body: ?Node.Index = null,
    docblock: ?TokenIndex = null,
    // TODO: Could be in FunctionType
    test_message: ?TokenIndex = null,
    // Should be .FunctionType
    // Only function without a function_signature is a script
    function_signature: ?Node.Index,

    upvalue_binding: std.AutoArrayHashMap(u8, bool),

    // If the function is a ScritEntryPoint
    entry: ?Entry = null,

    // Set when the function is first generated
    // The JIT compiler can then reference it when creating its closure
    native: ?*obj.ObjNative = null,
    function: ?*obj.ObjFunction = null,

    import_root: bool = false,

    pub const Entry = struct {
        main_slot: ?usize = null,
        main_location: ?TokenIndex = null,
        test_slots: []const usize,
        test_locations: []const TokenIndex,
        exported_count: usize = 0,
    };

    pub fn jsonStringify(self: Function, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("id");
        try jw.valueStart();
        try jw.stream.print("{}", .{self.id});
        try jw.valueDone();
        try jw.objectField("body");
        if (self.body) |body| {
            try ast.nodes.get(body).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("docblock");
        if (self.docblock) |db| {
            try std.json.stringify(
                ast.tokens.get(db),
                jw.options,
                jw.stream,
            );
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("test_message");
        if (self.test_message) |tm| {
            try std.json.stringify(
                ast.tokens.get(tm),
                jw.options,
                jw.stream,
            );
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("function_signature");
        if (self.function_signature) |fs| {
            try ast.nodes.get(fs).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("entry");
        if (self.entry) |entry| {
            try jw.beginObject();
            try jw.objectField("main_slot");
            try jw.valueStart();
            if (entry.main_slot) |ms| {
                try jw.stream.print("{}", .{ms});
            } else {
                try jw.stream.writeAll("null");
            }
            try jw.valueDone();
            try jw.objectField("main_location");
            if (entry.main_location) |ml| {
                try std.json.stringify(
                    ast.tokens.get(ml),
                    jw.options,
                    jw.stream,
                );
            } else {
                try jw.valueStart();
                try jw.stream.writeAll("null");
                try jw.valueDone();
            }
            try jw.objectField("test_slots");
            try jw.beginArray();
            for (entry.test_slots) |ts| {
                try jw.valueStart();
                try jw.stream.print("{}", .{ts});
                try jw.valueDone();
            }
            try jw.endArray();
            try jw.objectField("test_locations");
            try jw.beginArray();
            for (entry.test_locations) |ts| {
                try std.json.stringify(
                    ast.tokens.get(ts),
                    jw.options,
                    jw.stream,
                );
            }
            try jw.endArray();
            try jw.objectField("exported_count");
            try jw.valueStart();
            try jw.stream.print("{}", .{entry.exported_count});
            try jw.valueDone();
            try jw.endObject();
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("import_root");
        try jw.valueStart();
        try jw.stream.writeAll(if (self.import_root) "true" else "false");
        try jw.valueDone();
        try jw.endObject();
    }
};

pub const FunctionType = struct {
    name: ?TokenIndex,
    return_type: ?Node.Index,
    yield_type: ?Node.Index,
    error_types: []const Node.Index,
    arguments: []const Argument,
    generic_types: []const TokenIndex,
    lambda: bool,

    pub const Argument = struct {
        name: TokenIndex,
        type: Node.Index,
        default: ?Node.Index,
    };

    pub fn jsonStringify(self: FunctionType, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("name");
        if (self.name) |name| {
            try std.json.stringify(
                ast.tokens.get(name),
                jw.options,
                jw.stream,
            );
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("return_type");
        if (self.return_type) |rt| {
            try ast.nodes.get(rt).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("yield_type");
        if (self.yield_type) |yt| {
            try ast.nodes.get(yt).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("error_types");
        try jw.beginArray();
        for (self.error_types) |error_type| {
            try ast.nodes.get(error_type).jsonStringify(ast, jw);
        }
        try jw.endArray();
        try jw.objectField("arguments");
        try jw.beginArray();
        for (self.arguments) |arg| {
            try jw.beginObject();
            try jw.objectField("name");
            try std.json.stringify(
                ast.tokens.get(arg.name),
                jw.options,
                jw.stream,
            );
            try jw.objectField("type");
            try ast.nodes.get(arg.type).jsonStringify(ast, jw);
            try jw.objectField("default");
            if (arg.default) |d| {
                try ast.nodes.get(d).jsonStringify(ast, jw);
            } else {
                try jw.valueStart();
                try jw.stream.writeAll("null");
                try jw.valueDone();
            }
            try jw.endObject();
        }
        try jw.endArray();
        try jw.objectField("generic_types");
        try jw.beginArray();
        for (self.generic_types) |generic_type| {
            try ast.nodes.get(generic_type).jsonStringify(ast, jw);
        }
        try jw.endArray();
        try jw.objectField("lambda");
        try jw.valueStart();
        try jw.stream.writeAll(if (self.lambda) "true" else "false");
        try jw.valueDone();
        try jw.endObject();
    }
};

pub const FunDeclaration = struct {
    function: Node.Index,

    slot: Slot,
    slot_type: SlotType,

    pub fn jsonStringify(self: FunDeclaration, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("function");
        try ast.nodes.get(self.function).jsonStringify(ast, jw);
        try jw.objectField("slot");
        try jw.valueStart();
        try jw.stream.print("{}", .{self.slot});
        try jw.valueDone();
        try jw.objectField("slot_type");
        try jw.stringValue(@tagName(self.slot_type));
        try jw.endObject();
    }
};

pub const GenericResolveType = struct {
    resolved_types: []const Node.Index,

    pub fn jsonStringify(self: GenericResolveType, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("resolved_types");
        try jw.beginArray();
        for (self.resolved_types) |rt| {
            try ast.nodes.get(rt).jsonStringify(ast, jw);
        }
        try jw.endArray();
        try jw.endObject();
    }
};

pub const If = struct {
    condition: Node.Index,
    unwrapped_identifier: ?TokenIndex,
    casted_type: ?Node.Index,
    body: Node.Index,
    else_branch: ?Node.Index,
    is_statement: bool,

    pub fn jsonStringify(self: If, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("condition");
        try ast.nodes.get(self.condition).jsonStringify(ast, jw);
        try jw.objectField("unwrapped_identifier");
        if (self.unwrapped_identifier) |ui| {
            try std.json.stringify(
                ast.tokens.get(ui),
                jw.options,
                jw.stream,
            );
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("casted_type");
        if (self.casted_type) |ct| {
            try ast.nodes.get(ct).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("body");
        try ast.nodes.get(self.body).jsonStringify(ast, jw);
        try jw.objectField("else_branch");
        if (self.else_branch) |eb| {
            try ast.nodes.get(eb).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("is_statement");
        try jw.valueStart();
        try jw.stream.writeAll(if (self.is_statement) "true" else "false");
        try jw.valueDone();
        try jw.endObject();
    }
};

pub const Import = struct {
    imported_symbols: []const TokenIndex,
    prefix: ?TokenIndex,
    path: TokenIndex,
    import: ?Parser.ScriptImport,

    pub fn jsonStringify(self: Import, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("imported_symbols");
        try jw.beginArray();
        for (self.imported_symbols) |is| {
            try std.json.stringify(
                ast.tokens.get(is),
                jw.options,
                jw.stream,
            );
        }
        try jw.endArray();
        try jw.objectField("prefix");
        if (self.prefix) |prefix| {
            try std.json.stringify(
                ast.tokens.get(prefix),
                jw.options,
                jw.stream,
            );
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("path");
        try std.json.stringify(
            ast.tokens.get(self.path),
            jw.options,
            jw.stream,
        );
        try jw.objectField("import");
        try jw.endObject();
    }
};

pub const IsAs = struct {
    left: Node.Index,
    constant: Node.Index,

    pub fn jsonStringify(self: IsAs, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("left");
        try ast.nodes.get(self.left).jsonStringify(ast, jw);
        try jw.objectField("constant");
        try ast.nodes.get(self.constant).jsonStringify(ast, jw);
        try jw.endObject();
    }
};

pub const List = struct {
    explicit_item_type: ?TokenIndex,
    items: []const Node.Index,

    pub fn jsonStringify(self: List, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("explicit_item_type");
        if (self.explicit_item_type) |explicit_item_type| {
            try std.json.stringify(
                ast.tokens.get(explicit_item_type),
                jw.options,
                jw.stream,
            );
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("items");
        try jw.beginArray();
        for (self.items) |item| {
            try ast.nodes.get(item).jsonStringify(ast, jw);
        }
        try jw.endArray();
        try jw.endObject();
    }
};

pub const ListType = struct {
    item_type: Node.Index,

    pub fn jsonStringify(self: ListType, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("item_type");
        try ast.nodes.get(self.item_type).jsonStringify(ast, jw);
        try jw.endObject();
    }
};

pub const Map = struct {
    explicit_key_type: ?Node.Index,
    explicit_value_type: ?Node.Index,

    entries: []const Entry,

    pub const Entry = struct {
        key: Node.Index,
        value: Node.Index,
    };

    pub fn jsonStringify(self: Map, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("explicit_key_type");
        if (self.explicit_key_type) |kt| {
            try ast.nodes.get(kt).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("explicit_value_type");
        if (self.explicit_value_type) |vt| {
            try ast.nodes.get(vt).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("entries");
        try jw.beginArray();
        for (self.entries) |entry| {
            try jw.beginObject();
            try jw.objectField("key");
            try ast.nodes.get(entry.key).jsonStringify(ast, jw);
            try jw.objectField("value");
            try ast.nodes.get(entry.value).jsonStringify(ast, jw);
            try jw.endObject();
        }
        try jw.endArray();
        try jw.endObject();
    }
};

pub const MapType = struct {
    key_type: Node.Index,
    value_type: Node.Index,

    pub fn jsonStringify(self: MapType, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("key_type");
        try ast.nodes.get(self.key_type).jsonStringify(ast, jw);
        try jw.objectField("value_type");
        try ast.nodes.get(self.value_type).jsonStringify(ast, jw);
        try jw.endObject();
    }
};

pub const SlotType = enum(u8) {
    Local,
    UpValue,
    Global,
};

pub const Slot = u32;

pub const NamedVariable = struct {
    identifier: TokenIndex,
    value: ?Node.Index,
    slot: Slot,
    slot_type: SlotType,
    slot_constant: bool,

    pub fn jsonStringify(self: NamedVariable, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("identifier");
        try std.json.stringify(
            ast.tokens.get(self.identifier),
            jw.options,
            jw.stream,
        );
        try jw.objectField("value");
        if (self.value) |value| {
            try ast.nodes.get(value).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.valueDone();
        }
        try jw.objectField("slot");
        try jw.valueStart();
        try jw.stream.print("{}", .{self.slot});
        try jw.valueDone();
        try jw.objectField("slot_type");
        try jw.stringValue(@tagName(self.slot_type));
        try jw.objectField("slot_constant");
        try jw.valueStart();
        try jw.stream.writeAll(if (self.slot_constant) "true" else "false");
        try jw.valueDone();
        try jw.endObject();
    }
};

pub const ObjectDeclaration = struct {
    name: TokenIndex,
    slot: Slot,
    protocols: []const Node.Index,
    generics: []const TokenIndex,
    // List of either Function (methods) or VarDeclaration (properties)
    members: []const Member,

    pub const Member = struct {
        name: TokenIndex,
        docblock: ?TokenIndex,
        method: bool,
        method_or_default_value: ?Node.Index,
    };

    pub fn jsonStringify(self: ObjectDeclaration, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("name");
        try std.json.stringify(
            ast.tokens.get(self.name),
            jw.options,
            jw.stream,
        );
        try jw.objectField("slot");
        try jw.valueStart();
        try jw.stream.print("{}", .{self.slot});
        try jw.valueDone();
        try jw.objectField("protocols");
        try jw.beginArray();
        for (self.protocols) |protocol| {
            try ast.nodes.get(protocol).jsonStringify(ast, jw);
        }
        try jw.endArray();
        try jw.objectField("generics");
        try jw.beginArray();
        for (self.generics) |generic| {
            try std.json.stringify(
                ast.tokens.get(generic),
                jw.options,
                jw.stream,
            );
        }
        try jw.endArray();
        try jw.objectField("members");
        try jw.beginArray();
        for (self.members) |member| {
            try jw.objectField("name");
            try std.json.stringify(
                ast.tokens.get(member.name),
                jw.options,
                jw.stream,
            );
            try jw.objectField("docblock");
            if (member.docblock) |db| {
                try std.json.stringify(
                    ast.tokens.get(db),
                    jw.options,
                    jw.stream,
                );
            } else {
                try jw.valueStart();
                try jw.stream.writeAll("null");
                try jw.value.Done();
            }
            if (member.method) {
                try jw.objectField("method");
            } else {
                try jw.objectField("default_value");
            }
            if (member.method_or_default_value) |value| {
                try ast.nodes.get(value).jsonStringify(ast, jw);
            } else {
                try jw.valueStart();
                try jw.stream.writeAll("null");
                try jw.value.Done();
            }
        }
        try jw.endArray();
        try jw.endObject();
    }
};

pub const ObjectInit = struct {
    object: ?Node.Index, // Should be a NamedVariableNode or GenericResolve
    properties: []const Property,

    pub const Property = struct {
        name: TokenIndex,
        value: Node.Index,
    };

    pub fn jsonStringify(self: ObjectInit, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("object");
        if (self.object) |object| {
            try ast.nodes.get(object).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.value.Done();
        }
        try jw.objectField("properties");
        try jw.beginArray();
        for (self.properties) |prop| {
            try jw.objectField("name");
            try std.json.stringify(
                ast.tokens.get(prop.name),
                jw.options,
                jw.stream,
            );
            try jw.objectField("value");
            try ast.nodes.get(prop.value).jsonStringify(ast, jw);
        }
        try jw.endArray();
        try jw.endObject();
    }
};

pub const ProtocolDeclaration = struct {
    name: TokenIndex,
    slot: Slot,
    methods: []const Method,

    pub const Method = struct {
        docblock: ?TokenIndex,
        method: Node.Index,
    };

    pub fn jsonStringify(self: ProtocolDeclaration, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("name");
        try std.json.stringify(
            ast.tokens.get(self.name),
            jw.options,
            jw.stream,
        );
        try jw.objectField("slot");
        try jw.valueStart();
        try jw.stream.print("{}", .{self.slot});
        try jw.valueDone();
        try jw.objectField("methods");
        try jw.beginArray();
        for (self.methods) |method| {
            try jw.beginObject();
            try jw.objectField("docblock");
            if (method.docblock) |db| {
                try std.json.stringify(
                    ast.tokens.get(db),
                    jw.options,
                    jw.stream,
                );
            } else {
                try jw.valueStart();
                try jw.stream.writeAll("null");
                try jw.value.Done();
            }
            try jw.objectField("method");
            try ast.nodes.get(method.method).jsonStringify(ast, jw);
            try jw.endObject();
        }
        try jw.endArray();
        try jw.endObject();
    }
};

pub const Range = struct {
    low: Node.Index,
    high: Node.Index,

    pub fn jsonStringify(self: Range, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("low");
        try ast.nodes.get(self.low).jsonStringify(ast, jw);
        try jw.objectField("high");
        try ast.nodes.get(self.high).jsonStringify(ast, jw);
        try jw.endObject();
    }
};

pub const Return = struct {
    value: ?Node.Index,
    unconditional: bool,

    pub fn jsonStringify(self: Return, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("value");
        if (self.value) |value| {
            try ast.nodes.get(value).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.value.Done();
        }
        try jw.objectField("unconditional");
        try jw.valueStart();
        try jw.stream.writeAll(if (self.slot_constant) "true" else "false");
        try jw.valueDone();
        try jw.endObject();
    }
};

pub const Subscript = struct {
    subscripted: Node.Index,
    index: Node.Index,
    value: ?Node.Index,
    checked: bool,

    pub fn jsonStringify(self: Subscript, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("subscripted");
        try ast.nodes.get(self.subscripted).jsonStringify(ast, jw);
        try jw.objectField("index");
        try ast.nodes.get(self.index).jsonStringify(ast, jw);
        try jw.objectField("value");
        if (self.value) |value| {
            try ast.nodes.get(value).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.value.Done();
        }
        try jw.objectField("checked");
        try jw.valueStart();
        try jw.stream.writeAll(if (self.checked) "true" else "false");
        try jw.valueDone();
        try jw.endObject();
    }
};

pub const Throw = struct {
    expression: Node.Index,
    unconditional: bool,

    pub fn jsonStringify(self: Throw, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("expression");
        try ast.nodes.get(self.expression).jsonStringify(ast, jw);
        try jw.objectField("unconditional");
        try jw.valueStart();
        try jw.stream.writeAll(if (self.unconditional) "true" else "false");
        try jw.valueDone();
        try jw.endObject();
    }
};

pub const Try = struct {
    body: Node.Index,
    clauses: []const Clause,
    unconditional_clause: ?Node.Index,

    pub const Clause = struct {
        identifier: TokenIndex,

        type_def: Node.Index,
        body: Node.Index,
    };

    pub fn jsonStringify(self: Try, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("body");
        try ast.nodes.get(self.body).jsonStringify(ast, jw);
        try jw.objectField("clauses");
        try jw.beginArray();
        for (self.clauses) |clause| {
            try jw.beginObject();
            try jw.objectField("identifier");
            try std.json.stringify(
                ast.tokens.get(clause.identifier),
                jw.options,
                jw.stream,
            );
            try jw.objectField("type_def");
            try ast.nodes.get(clause.type_def).jsonStringify(ast, jw);
            try jw.objectField("body");
            try ast.nodes.get(clause.body).jsonStringify(ast, jw);
            try jw.endObject();
        }
        try jw.endArray();
        try jw.objectField("unconditional_clause");
        if (self.unconditional_clause) |clause| {
            try ast.nodes.get(clause).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.value.Done();
        }
        try jw.endObject();
    }
};

pub const Unary = struct {
    operator: Token.Type,
    expression: Node.Index,

    pub fn jsonStringify(self: Unary, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("operator");
        try std.json.stringify(
            ast.tokens.get(self.operator),
            jw.options,
            jw.stream,
        );
        try jw.objectField("expression");
        try ast.nodes.get(self.expression).jsonStringify(ast, jw);
        try jw.endObject();
    }
};

pub const Unwrap = struct {
    unwrapped: Node.Index,
    original_type: *obj.ObjTypeDef,

    pub fn jsonStringify(self: Unwrap, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("unwrapped");
        try ast.nodes.get(self.unwrapped).jsonStringify(ast, jw);
        try jw.objectField("original_type");
        try std.json.stringify(
            self.original_type,
            jw.options,
            jw.stream,
        );
        try jw.endObject();
    }
};

pub const UserType = struct {
    identifier: TokenIndex,
    generic_resolve: ?Node.Index,

    pub fn jsonStringify(self: UserType, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("identifier");
        try std.json.stringify(
            self.identifier,
            jw.options,
            jw.stream,
        );
        try jw.objectField("generic_resolve");
        if (self.generic_resolve) |gr| {
            try ast.nodes.get(gr).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.value.Done();
        }
        try jw.endObject();
    }
};

pub const VarDeclaration = struct {
    name: TokenIndex,
    value: ?Node.Index,
    type: ?Node.Index,
    constant: bool,
    slot: Slot,
    slot_type: SlotType,

    pub fn jsonStringify(self: VarDeclaration, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("name");
        try std.json.stringify(
            self.name,
            jw.options,
            jw.stream,
        );
        try jw.objectField("value");
        if (self.value) |value| {
            try ast.nodes.get(value).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.value.Done();
        }
        try jw.objectField("type");
        if (self.type) |t| {
            try ast.nodes.get(t).jsonStringify(ast, jw);
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.value.Done();
        }
        try jw.objectField("constant");
        try jw.valueStart();
        try jw.stream.writeAll(if (self.constant) "true" else "false");
        try jw.valueDone();
        try jw.objectField("slot");
        try jw.valueStart();
        try jw.stream.print("{}", .{self.slot});
        try jw.valueDone();
        try jw.objectField("slot_type");
        try jw.stringValue(@tagName(self.slot_type));
        try jw.endObject();
    }
};

pub const WhileDoUntil = struct {
    condition: Node.Index,
    body: Node.Index,
    label: ?TokenIndex,

    pub fn jsonStringify(self: WhileDoUntil, ast: *const Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("condition");
        try ast.nodes.get(self.condition).jsonStringify(ast, jw);
        try jw.objectField("body");
        try ast.nodes.get(self.body).jsonStringify(ast, jw);
        try jw.objectField("label");
        if (self.label) |label| {
            try std.json.stringify(
                label,
                jw.options,
                jw.stream,
            );
        } else {
            try jw.valueStart();
            try jw.stream.writeAll("null");
            try jw.value.Done();
        }
        try jw.endObject();
    }
};

pub const Zdef = struct {
    lib_name: TokenIndex,
    source: TokenIndex,
    elements: []ZdefElement,

    pub const ZdefElement = struct {
        fn_ptr: ?*anyopaque = null,
        obj_native: ?*obj.ObjNative = null,
        // TODO: On the stack, do we free it at some point?
        zdef: *const FFI.Zdef,
        slot: Slot,
        // TODO: add TokenIndex which should wrap portion of the zdef string relative to this element
    };

    pub fn jsonStringify(self: Zdef, _: *Self, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("lib_name");
        try std.json.stringify(
            self.lib_name,
            jw.options,
            jw.stream,
        );
        try jw.objectField("source");
        try std.json.stringify(
            self.source,
            jw.options,
            jw.stream,
        );
        try jw.objectField("elements");
        try jw.beginArray();
        for (self.elements) |element| {
            try jw.beginObject();
            try jw.objectField("slot");
            try jw.valueStart();
            try jw.stream.print("{}", .{element.slot});
            try jw.valueDone();
            try jw.objectField("name");
            try jw.stringValue(element.zdef.name);
            try jw.objectField("type_def");
            try std.json.stringify(
                element.zdef.type_def,
                jw.options,
                jw.stream,
            );
            try jw.objectField("zig_type");
            try std.json.stringify(
                element.zdef.zig_type,
                jw.options,
                jw.stream,
            );
            try jw.endObject();
        }
        try jw.endArray();
        try jw.endObject();
    }
};

pub fn isConstant(self: Self, node: Node.Index) bool {
    return switch (self.nodes.items(.tag)[node]) {
        .AnonymousObjectType,
        .FiberType,
        .FunctionType,
        .GenericResolveType,
        .GenericType,
        .ListType,
        .MapType,
        .SimpleType,
        .UserType,
        .Boolean,
        .Integer,
        .Float,
        .Pattern,
        .Null,
        .StringLiteral,
        .TypeExpression,
        .Void,
        .Namespace,
        => true,

        .AsyncCall,
        .Block,
        .Break,
        .Continue,
        .Call,
        .DoUntil,
        .Enum,
        .Export,
        .For,
        .ForEach,
        .Function,
        .FunDeclaration,
        .Import,
        .NamedVariable,
        .ObjectDeclaration,
        .ObjectInit,
        .ProtocolDeclaration,
        .Resolve,
        .Resume,
        .Return,
        .Try,
        .Throw,
        .VarDeclaration,
        .While,
        .Yield,
        .Zdef,
        .BlockExpression,
        .Out,
        => false,

        .As => self.isConstant(self.nodes.items(.components)[node].As.left),
        .Is => self.isConstant(self.nodes.items(.components)[node].Is.left),
        .Range => self.isConstant(self.nodes.items(.components)[node].Range.low) and self.isConstant(self.nodes.items(.components)[node].Range.high),
        .Binary => {
            const components = self.nodes.items(.components)[node].Binary;

            return self.isConstant(components.left) and self.isConstant(components.right);
        },
        .Dot => {
            const type_def = self.nodes.items(.type_def)[self.nodes.items(.components)[node].Dot.callee].?;

            return type_def.def_type == .Enum and type_def.resolved_type.?.Enum.value != null;
        },
        .Expression => self.isConstant(self.nodes.items(.components)[node].Expression),
        .Grouping => self.isConstant(self.nodes.items(.components)[node].Grouping),
        .ForceUnwrap => self.isConstant(self.nodes.items(.components)[node].ForceUnwrap.unwrapped),
        .GenericResolve => self.isConstant(self.nodes.items(.components)[node].GenericResolve),
        .If => {
            const components = self.nodes.items(.components)[node].If;

            return !components.is_statement and self.isConstant(components.condition) and self.isConstant(components.body) and self.isConstant(components.else_branch.?);
        },
        .List => {
            const components = self.nodes.items(.components)[node].List;
            const node_types = self.nodes.items(.tag);

            for (components.items) |item| {
                if (node_types[item] == .List or node_types[item] == .Map or !self.isConstant(item)) {
                    return false;
                }
            }

            return true;
        },
        .Map => {
            const components = self.nodes.items(.components)[node].Map;
            const node_types = self.nodes.items(.tag);

            for (components.entries) |entry| {
                if (node_types[entry.key] == .List or node_types[entry.value] == .List or node_types[entry.key] == .Map or node_types[entry.key] == .Map or !self.isConstant(entry.key) or !self.isConstant(entry.value)) {
                    return false;
                }
            }

            return true;
        },
        .String => {
            const elements = self.nodes.items(.components)[node].String;

            for (elements) |element| {
                if (!self.isConstant(element)) {
                    return false;
                }
            }

            return true;
        },
        .Subscript => {
            const components = self.nodes.items(.components)[node].Subscript;

            return self.isConstant(components.subscripted) and self.isConstant(components.index) and components.value == null;
        },
        .TypeOfExpression => self.isConstant(self.nodes.items(.components)[node].TypeOfExpression),
        .Unary => self.isConstant(self.nodes.items(.components)[node].Unary.expression),
        .Unwrap => self.isConstant(self.nodes.items(.components)[node].Unwrap.unwrapped),
    };
}

fn binaryValue(self: Self, node: Node.Index, gc: *GarbageCollector) !?Value {
    const components = self.nodes.items(.components)[node].Binary;

    const left = try self.toValue(components.left, gc);
    const left_integer = if (left.isInteger()) left.integer() else null;
    const left_float = if (left.isFloat()) left.float() else null;
    const right = try self.toValue(components.right, gc);
    const right_integer = if (right.isInteger()) right.integer() else null;
    const right_float = if (right.isFloat()) right.float() else null;

    switch (components.operator) {
        .Ampersand => return Value.fromInteger(left_integer.? & right_integer.?),
        .Bor => return Value.fromInteger(left_integer.? | right_integer.?),
        .Xor => return Value.fromInteger(left_integer.? ^ right_integer.?),
        .ShiftLeft => {
            const b = right_integer.?;

            if (b < 0) {
                if (b * -1 > std.math.maxInt(u6)) {
                    return Value.fromInteger(0);
                }

                return Value.fromInteger(
                    left_integer.? >> @as(u5, @truncate(@as(u64, @intCast(b * -1)))),
                );
            } else {
                if (b > std.math.maxInt(u6)) {
                    return Value.fromInteger(0);
                }

                return Value.fromInteger(
                    left_integer.? << @as(u5, @truncate(@as(u64, @intCast(b)))),
                );
            }
        },
        .ShiftRight => {
            const b = right_integer.?;

            if (b < 0) {
                if (b * -1 > std.math.maxInt(u6)) {
                    return Value.fromInteger(0);
                }

                return Value.fromInteger(
                    left_integer.? << @as(u5, @truncate(@as(u64, @intCast(b * -1)))),
                );
            } else {
                if (b > std.math.maxInt(u6)) {
                    return Value.fromInteger(0);
                }

                return Value.fromInteger(
                    left_integer.? >> @as(u5, @truncate(@as(u64, @intCast(b)))),
                );
            }
        },
        .QuestionQuestion => return if (left.isNull())
            right
        else
            left,
        .Greater => {
            if (left_float) |lf| {
                if (right_float) |rf| {
                    return Value.fromBoolean(lf > rf);
                } else {
                    return Value.fromBoolean(lf > @as(f64, @floatFromInt(right_integer.?)));
                }
            } else {
                if (right_float) |rf| {
                    return Value.fromBoolean(@as(f64, @floatFromInt(left_integer.?)) > rf);
                } else {
                    return Value.fromBoolean(left_integer.? > right_integer.?);
                }
            }

            return Value.fromBoolean(left_float orelse left_integer.? > right_float orelse right_integer.?);
        },
        .Less => {
            if (left_float) |lf| {
                if (right_float) |rf| {
                    return Value.fromBoolean(lf < rf);
                } else {
                    return Value.fromBoolean(lf < @as(f64, @floatFromInt(right_integer.?)));
                }
            } else {
                if (right_float) |rf| {
                    return Value.fromBoolean(@as(f64, @floatFromInt(left_integer.?)) < rf);
                } else {
                    return Value.fromBoolean(left_integer.? < right_integer.?);
                }
            }

            return Value.fromBoolean(left_float orelse left_integer.? < right_float orelse right_integer.?);
        },
        .GreaterEqual => {
            if (left_float) |lf| {
                if (right_float) |rf| {
                    return Value.fromBoolean(lf >= rf);
                } else {
                    return Value.fromBoolean(lf >= @as(f64, @floatFromInt(right_integer.?)));
                }
            } else {
                if (right_float) |rf| {
                    return Value.fromBoolean(@as(f64, @floatFromInt(left_integer.?)) >= rf);
                } else {
                    return Value.fromBoolean(left_integer.? >= right_integer.?);
                }
            }

            return Value.fromBoolean(left_float orelse left_integer.? >= right_float orelse right_integer.?);
        },
        .LessEqual => {
            if (left_float) |lf| {
                if (right_float) |rf| {
                    return Value.fromBoolean(lf <= rf);
                } else {
                    return Value.fromBoolean(lf <= @as(f64, @floatFromInt(right_integer.?)));
                }
            } else {
                if (right_float) |rf| {
                    return Value.fromBoolean(@as(f64, @floatFromInt(left_integer.?)) <= rf);
                } else {
                    return Value.fromBoolean(left_integer.? <= right_integer.?);
                }
            }

            return Value.fromBoolean(left_float orelse left_integer.? <= right_float orelse right_integer.?);
        },
        .BangEqual => return Value.fromBoolean(!left.eql(right)),
        .EqualEqual => return Value.fromBoolean(left.eql(right)),
        .Plus => {
            const right_string = if (right.isObj()) obj.ObjString.cast(right.obj()) else null;
            const left_string = if (left.isObj()) obj.ObjString.cast(left.obj()) else null;

            const right_list = if (right.isObj()) obj.ObjList.cast(right.obj()) else null;
            const left_list = if (left.isObj()) obj.ObjList.cast(left.obj()) else null;

            const right_map = if (right.isObj()) obj.ObjMap.cast(right.obj()) else null;
            const left_map = if (left.isObj()) obj.ObjMap.cast(left.obj()) else null;

            if (right_string) |rs| {
                var new_string = std.ArrayList(u8).init(gc.allocator);
                try new_string.appendSlice(left_string.?.string);
                try new_string.appendSlice(rs.string);
                new_string.shrinkAndFree(new_string.items.len);

                return (try gc.copyString(new_string.items)).toValue();
            } else if (right_float) |rf| {
                return Value.fromFloat(rf + left_float.?);
            } else if (right_integer) |ri| {
                return Value.fromInteger(ri +% left_integer.?);
            } else if (right_list) |rl| {
                var new_list = std.ArrayListUnmanaged(Value){};
                try new_list.appendSlice(gc.allocator, left_list.?.items.items);
                try new_list.appendSlice(gc.allocator, rl.items.items);

                return (try gc.allocateObject(
                    obj.ObjList,
                    .{
                        .type_def = left_list.?.type_def,
                        .methods = left_list.?.methods,
                        .items = new_list,
                    },
                )).toValue();
            } else {
                var new_map = try left_map.?.map.clone(gc.allocator);
                var it = right_map.?.map.iterator();
                while (it.next()) |entry| {
                    try new_map.put(
                        gc.allocator,
                        entry.key_ptr.*,
                        entry.value_ptr.*,
                    );
                }

                return (try gc.allocateObject(
                    obj.ObjMap,
                    .{
                        .type_def = left_map.?.type_def,
                        .methods = left_map.?.methods,
                        .map = new_map,
                    },
                )).toValue();
            }
        },
        .Minus => {
            if (right_float) |rf| {
                return Value.fromFloat(rf - left_float.?);
            }

            return Value.fromInteger(right_integer.? -% left_integer.?);
        },
        .Star => {
            if (right_float) |rf| {
                return Value.fromFloat(rf * left_float.?);
            }

            return Value.fromInteger(right_integer.? *% left_integer.?);
        },
        .Slash => {
            if (right_float) |rf| {
                return Value.fromFloat(left_float.? / rf);
            }

            return Value.fromInteger(@divTrunc(left_integer.?, right_integer.?));
        },
        .Percent => {
            if (right_float) |rf| {
                return Value.fromFloat(@mod(left_float.?, rf));
            }

            return Value.fromInteger(@mod(left_integer.?, right_integer.?));
        },
        .And => return Value.fromBoolean(left.boolean() and right.boolean()),
        .Or => return Value.fromBoolean(left.boolean() or right.boolean()),
        else => unreachable,
    }
}

pub fn toValue(self: Self, node: Node.Index, gc: *GarbageCollector) Error!Value {
    const value = &self.nodes.items(.value)[node];

    if (value.* == null and self.isConstant(node)) {
        value.* = switch (self.nodes.items(.tag)[node]) {
            .AnonymousObjectType,
            .FiberType,
            .FunctionType,
            .GenericResolveType,
            .GenericType,
            .ListType,
            .MapType,
            .SimpleType,
            .UserType,
            => self.nodes.items(.type_def)[node].?.toValue(),
            .StringLiteral => self.nodes.items(.components)[node].StringLiteral.toValue(),
            .TypeOfExpression => (try (try self.toValue(
                self.nodes.items(.components)[node].TypeOfExpression,
                gc,
            )).typeOf(gc)).toValue(),
            .TypeExpression => self.nodes.items(.type_def)[self.nodes.items(.components)[node].TypeExpression].?.toValue(),
            .Pattern => self.nodes.items(.components)[node].Pattern.toValue(),
            .Void => Value.Void,
            .Null => Value.Null,
            .Float => Value.fromFloat(self.nodes.items(.components)[node].Float),
            .Integer => Value.fromInteger(self.nodes.items(.components)[node].Integer),
            .Boolean => Value.fromBoolean(self.nodes.items(.components)[node].Boolean),
            .As => try self.toValue(self.nodes.items(.components)[node].As.left, gc),
            .Is => is: {
                const components = self.nodes.items(.components)[node].Is;
                break :is Value.fromBoolean(
                    (try self.toValue(components.constant, gc))
                        .is(try self.toValue(components.left, gc)),
                );
            },
            .Binary => try self.binaryValue(node, gc),
            .Dot => dot: {
                // Only Enum.case can be constant
                const components = self.nodes.items(.components)[node].Dot;
                const type_def = self.nodes.items(.type_def)[components.callee].?;

                break :dot (try gc.allocateObject(
                    obj.ObjEnumInstance,
                    .{
                        .enum_ref = type_def.resolved_type.?.Enum.value.?,
                        .case = @intCast(components.value_or_call_or_enum.EnumCase),
                    },
                )).toValue();
            },
            .Expression => try self.toValue(self.nodes.items(.components)[node].Expression, gc),
            .Grouping => try self.toValue(self.nodes.items(.components)[node].Grouping, gc),
            .ForceUnwrap => fc: {
                const unwrapped = try self.toValue(self.nodes.items(.components)[node].ForceUnwrap.unwrapped, gc);

                if (unwrapped.isNull()) {
                    return Error.UnwrappedNull;
                }

                break :fc unwrapped;
            },
            .GenericResolve => try self.toValue(self.nodes.items(.components)[node].GenericResolve, gc),
            .If => @"if": {
                const components = self.nodes.items(.components)[node].If;
                break :@"if" if ((try self.toValue(components.condition, gc)).boolean())
                    try self.toValue(components.body, gc)
                else
                    try self.toValue(components.else_branch.?, gc);
            },
            .Range => range: {
                const components = self.nodes.items(.components)[node].Range;

                break :range (try gc.allocateObject(
                    obj.ObjRange,
                    .{
                        .low = (try self.toValue(components.low, gc)).integer(),
                        .high = (try self.toValue(components.high, gc)).integer(),
                    },
                )).toValue();
            },
            .List => list: {
                const components = self.nodes.items(.components)[node].List;
                const type_def = self.nodes.items(.type_def)[node];

                std.debug.assert(type_def != null and type_def.?.def_type != .Placeholder);

                var list = try gc.allocateObject(
                    obj.ObjList,
                    try obj.ObjList.init(gc.allocator, type_def.?),
                );

                for (components.items) |item| {
                    try list.items.append(
                        gc.allocator,
                        try self.toValue(item, gc),
                    );
                }

                break :list list.toValue();
            },
            .Map => map: {
                const components = self.nodes.items(.components)[node].Map;
                const type_def = self.nodes.items(.type_def)[node];

                std.debug.assert(type_def != null and type_def.?.def_type != .Placeholder);

                var map = try gc.allocateObject(
                    obj.ObjMap,
                    try obj.ObjMap.init(gc.allocator, type_def.?),
                );

                for (components.entries) |entry| {
                    try map.map.put(
                        gc.allocator,
                        try self.toValue(entry.key, gc),
                        try self.toValue(entry.value, gc),
                    );
                }

                break :map map.toValue();
            },
            .String => string: {
                const elements = self.nodes.items(.components)[node].String;

                var string = std.ArrayList(u8).init(gc.allocator);
                const writer = &string.writer();
                for (elements) |element| {
                    try (try self.toValue(element, gc)).toString(writer);
                }

                string.shrinkAndFree(string.items.len);
                break :string (try gc.copyString(string.items)).toValue();
            },
            .Subscript => subscript: {
                const components = self.nodes.items(.components)[node].Subscript;

                const subscriptable = (try self.toValue(components.subscripted, gc)).obj();
                const key = try self.toValue(components.index, gc);

                switch (subscriptable.obj_type) {
                    .List => {
                        const list = obj.ObjList.cast(subscriptable).?;
                        const index: usize = @intCast(key.integer());

                        if (index < 0 or index >= list.items.items.len) {
                            if (components.checked) {
                                break :subscript Value.Null;
                            }

                            return Error.OutOfBound;
                        }

                        break :subscript list.items.items[index];
                    },
                    .Map => {
                        const map = obj.ObjMap.cast(subscriptable).?;

                        break :subscript map.map.get(key) orelse Value.Null;
                    },
                    .String => {
                        const str = obj.ObjString.cast(subscriptable).?;
                        const index: usize = @intCast(key.integer());

                        if (index < 0 or index >= str.string.len) {
                            if (components.checked) {
                                break :subscript Value.Null;
                            }

                            return Error.OutOfBound;
                        }

                        break :subscript (try gc.copyString(
                            &([_]u8{str.string[index]}),
                        )).toValue();
                    },
                    else => unreachable,
                }

                break :subscript self.nodes.items(.components)[node].Subscript.toValue();
            },
            .Unary => unary: {
                const components = self.nodes.items(.components)[node].Unary;
                const val = try self.toValue(components.expression, gc);

                break :unary switch (components.operator) {
                    .Bnot => Value.fromInteger(~val.integer()),
                    .Bang => Value.fromBoolean(!val.boolean()),
                    .Minus => if (val.isInteger())
                        Value.fromInteger(-%val.integer())
                    else
                        Value.fromFloat(-val.float()),
                    else => unreachable,
                };
            },
            .Unwrap => try self.toValue(self.nodes.items(.components)[node].Unwrap.unwrapped, gc),
            else => null,
        };
    }

    return value.*.?;
}
