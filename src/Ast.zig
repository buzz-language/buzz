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

    pub const Index = u32;

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
        Block: []Node.Index,
        BlockExpression: []Node.Index,
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
        String: []Node.Index,
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
    fields: []Field,

    pub const Field = struct {
        name: TokenIndex,
        type: Node.Index,
    };
};

pub const Binary = struct {
    left: Node.Index,
    right: Node.Index,
    operator: Token.Type,
};

pub const Call = struct {
    is_async: bool,
    callee: Node.Index,
    // We need this because in a dot.call, callee is dot and its type will be == to call return type
    callee_type_def: *obj.ObjTypeDef,
    arguments: []Argument,
    catch_default: ?Node.Index,
    tail_call: bool = false,

    pub const Argument = struct {
        name: ?TokenIndex,
        value: Node.Index,
    };
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
};

pub const Enum = struct {
    name: TokenIndex,
    case_type: ?Node.Index,
    slot: Slot,
    cases: []Case,

    pub const Case = struct {
        name: TokenIndex,
        docblock: ?TokenIndex,
        value: ?Node.Index,
    };
};

pub const Export = struct {
    identifier: ?TokenIndex,
    alias: ?TokenIndex,
    declaration: ?Node.Index,
};

pub const FiberType = struct {
    return_type: Node.Index,
    yield_type: Node.Index,
};

pub const For = struct {
    init_declarations: []Node.Index,
    condition: Node.Index,
    post_loop: []Node.Index,
    body: Node.Index,
    label: ?TokenIndex,
};

pub const ForEach = struct {
    iterable: Node.Index,
    key: Node.Index,
    value: Node.Index,
    body: Node.Index,
    key_omitted: bool,
    label: ?TokenIndex,
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
        test_slots: []usize,
        test_locations: []TokenIndex,
        exported_count: usize = 0,
    };
};

pub const FunctionType = struct {
    name: ?TokenIndex,
    return_type: ?Node.Index,
    yield_type: ?Node.Index,
    error_types: []Node.Index,
    arguments: []Argument,
    generic_types: []TokenIndex,
    lambda: bool,

    pub const Argument = struct {
        name: TokenIndex,
        type: Node.Index,
        default: ?Node.Index,
    };
};

pub const FunDeclaration = struct {
    function: Node.Index,

    slot: Slot,
    slot_type: SlotType,
};

pub const GenericResolveType = struct {
    resolved_types: []Node.Index,
};

pub const If = struct {
    condition: Node.Index,
    unwrapped_identifier: ?TokenIndex,
    casted_type: ?Node.Index,
    body: Node.Index,
    else_branch: ?Node.Index,
    is_statement: bool,
};

pub const Import = struct {
    imported_symbols: []TokenIndex,
    prefix: ?TokenIndex,
    path: TokenIndex,
    import: ?Parser.ScriptImport,
};

pub const IsAs = struct {
    left: Node.Index,
    constant: Node.Index,
};

pub const List = struct {
    explicit_item_type: ?TokenIndex,
    items: []Node.Index,
};

pub const ListType = struct {
    item_type: Node.Index,
};

pub const Map = struct {
    explicit_key_type: ?Node.Index,
    explicit_value_type: ?Node.Index,

    entries: []Entry,

    pub const Entry = struct {
        key: Node.Index,
        value: Node.Index,
    };
};

pub const MapType = struct {
    key_type: Node.Index,
    value_type: Node.Index,
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
};

pub const ObjectDeclaration = struct {
    name: TokenIndex,
    slot: Slot,
    protocols: []Node.Index,
    generics: []TokenIndex,
    // List of either Function (methods) or VarDeclaration (properties)
    members: []Member,

    pub const Member = struct {
        name: TokenIndex,
        docblock: ?TokenIndex,
        method: bool,
        method_or_default_value: ?Node.Index,
    };
};

pub const ObjectInit = struct {
    object: ?Node.Index, // Should be a NamedVariableNode or GenericResolve
    properties: []Property,

    pub const Property = struct {
        name: TokenIndex,
        value: Node.Index,
    };
};

pub const ProtocolDeclaration = struct {
    name: TokenIndex,
    slot: Slot,
    methods: []Method,

    pub const Method = struct {
        docblock: ?TokenIndex,
        method: Node.Index,
    };
};

pub const Range = struct {
    low: Node.Index,
    high: Node.Index,
};

pub const Resume = struct {};

pub const Return = struct {
    value: ?Node.Index,
    unconditional: bool,
};

pub const Subscript = struct {
    subscripted: Node.Index,
    index: Node.Index,
    value: ?Node.Index,
};

pub const Throw = struct {
    expression: Node.Index,
    unconditional: bool,
};

pub const Try = struct {
    body: Node.Index,
    clauses: []Clause,
    unconditional_clause: ?Node.Index,

    pub const Clause = struct {
        identifier: TokenIndex,

        type_def: Node.Index,
        body: Node.Index,
    };
};

pub const Unary = struct {
    operator: Token.Type,
    expression: Node.Index,
};

pub const Unwrap = struct {
    unwrapped: Node.Index,
    original_type: *obj.ObjTypeDef,
};

pub const UserType = struct {
    identifier: TokenIndex,
    generic_resolve: ?Node.Index,
};

pub const VarDeclaration = struct {
    name: TokenIndex,
    value: ?Node.Index,
    type: ?Node.Index,
    constant: bool,
    slot: Slot,
    slot_type: SlotType,
};

pub const WhileDoUntil = struct {
    condition: Node.Index,
    body: Node.Index,
    label: ?TokenIndex,
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
