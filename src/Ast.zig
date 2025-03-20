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

pub const Slice = struct {
    tokens: TokenList.Slice,
    nodes: NodeList.Slice,
    root: ?Node.Index,

    /// Do a breadth first walk of the AST, calling a callback for each node that can stop the walking from going deeper by returning true
    /// ctx should have:
    /// - `fn processNode(ctx: @TypeOf(ctx), allocator: std.mem.Allocator, ast: Ast.Slice, node: Node.Index) error{OutOfMemory}!bool: returns true to stop walking deeper
    // FIXME: the context should have a error type we can reuse in processNode signature
    pub fn walk(self: Slice, allocator: std.mem.Allocator, ctx: anytype, root: Node.Index) !void {
        const tags = self.nodes.items(.tag);
        const components = self.nodes.items(.components);

        // Hold previous node's leaves
        var node_queue = std.ArrayListUnmanaged(Node.Index){};
        try node_queue.append(allocator, root);
        defer node_queue.deinit(allocator);

        while (node_queue.items.len > 0) {
            const node = node_queue.orderedRemove(0);

            // Stop if requested and if there's no neighbors to process
            if (try ctx.processNode(allocator, self, node) and node_queue.items.len == 0) {
                return;
            }

            // Otherwise continue walking the tree
            const comp = components[node];
            switch (tags[node]) {
                .AnonymousObjectType => {
                    for (comp.AnonymousObjectType.fields) |field| {
                        try node_queue.append(allocator, field.type);
                    }
                },
                .Is => {
                    try node_queue.appendSlice(
                        allocator,
                        &.{
                            comp.Is.left,
                            comp.Is.constant,
                        },
                    );
                },
                .As => {
                    try node_queue.appendSlice(
                        allocator,
                        &.{
                            comp.As.left,
                            comp.As.constant,
                        },
                    );
                },
                .AsyncCall => try node_queue.append(allocator, comp.AsyncCall),
                .Binary => {
                    try node_queue.append(allocator, comp.Binary.left);
                    try node_queue.append(allocator, comp.Binary.right);
                },
                .Block => try node_queue.appendSlice(allocator, comp.Block),
                .BlockExpression => try node_queue.appendSlice(allocator, comp.BlockExpression),
                .Call => {
                    // Avoid loop between Call and Dot nodes
                    if (tags[comp.Call.callee] != .Dot or
                        components[comp.Call.callee].Dot.member_kind != .Call or
                        components[comp.Call.callee].Dot.value_or_call_or_enum.Call != node)
                    {
                        try node_queue.append(allocator, comp.Call.callee);
                    }
                    if (comp.Call.catch_default) |default| {
                        try node_queue.append(allocator, default);
                    }
                    for (comp.Call.arguments) |arg| {
                        try node_queue.append(allocator, arg.value);
                    }
                },
                .Dot => {
                    try node_queue.append(allocator, comp.Dot.callee);
                    if (comp.Dot.generic_resolve) |generic_resolve| {
                        try node_queue.append(allocator, generic_resolve);
                    }
                    switch (comp.Dot.member_kind) {
                        .Value => try node_queue.append(allocator, comp.Dot.value_or_call_or_enum.Value.value),
                        .Call => {
                            // We avoid the actual Call node, we're only interested in the Call's parts
                            const call = components[comp.Dot.value_or_call_or_enum.Call].Call;
                            if (call.catch_default) |default| {
                                try node_queue.append(allocator, default);
                            }
                            for (call.arguments) |arg| {
                                try node_queue.append(allocator, arg.value);
                            }
                        },
                        .Ref, .EnumCase => {},
                    }
                },
                .DoUntil => {
                    try node_queue.append(allocator, comp.DoUntil.condition);
                    try node_queue.append(allocator, comp.DoUntil.body);
                },
                .Enum => {
                    if (comp.Enum.case_type) |case_type| {
                        try node_queue.append(allocator, case_type);
                    }
                    for (comp.Enum.cases) |case| {
                        if (case.value) |value| {
                            try node_queue.append(allocator, value);
                        }
                    }
                },
                .Export => {
                    if (comp.Export.declaration) |decl| {
                        try node_queue.append(allocator, decl);
                    }
                },
                .Expression => try node_queue.append(allocator, comp.Expression),
                .FiberType => {
                    try node_queue.append(allocator, comp.FiberType.return_type);
                    try node_queue.append(allocator, comp.FiberType.yield_type);
                },
                .For => {
                    try node_queue.append(allocator, comp.For.condition);
                    try node_queue.append(allocator, comp.For.body);
                    try node_queue.appendSlice(allocator, comp.For.init_declarations);
                    try node_queue.appendSlice(allocator, comp.For.post_loop);
                },
                .ForceUnwrap => try node_queue.append(allocator, comp.ForceUnwrap.unwrapped),
                .ForEach => {
                    try node_queue.append(allocator, comp.ForEach.iterable);
                    try node_queue.append(allocator, comp.ForEach.body);
                    try node_queue.append(allocator, comp.ForEach.key);
                    try node_queue.append(allocator, comp.ForEach.value);
                },
                .Function => {
                    if (comp.Function.body) |body| {
                        try node_queue.append(allocator, body);
                    }

                    if (comp.Function.function_signature) |signature| {
                        try node_queue.append(allocator, signature);
                    }
                },
                .FunctionType => {
                    if (comp.FunctionType.return_type) |return_type| {
                        try node_queue.append(allocator, return_type);
                    }

                    if (comp.FunctionType.yield_type) |yield_type| {
                        try node_queue.append(allocator, yield_type);
                    }

                    try node_queue.appendSlice(allocator, comp.FunctionType.error_types);

                    for (comp.FunctionType.arguments) |arg| {
                        try node_queue.append(allocator, arg.type);

                        if (arg.default) |default| {
                            try node_queue.append(allocator, default);
                        }
                    }
                },
                .FunDeclaration => try node_queue.append(allocator, comp.FunDeclaration.function),
                .GenericResolve => {
                    try node_queue.append(allocator, comp.GenericResolve.expression);
                    try node_queue.appendSlice(allocator, comp.GenericResolve.resolved_types);
                },
                .GenericResolveType => try node_queue.appendSlice(allocator, comp.GenericResolveType),
                .Grouping => try node_queue.append(allocator, comp.Grouping),
                .If => {
                    try node_queue.append(allocator, comp.If.condition);
                    try node_queue.append(allocator, comp.If.body);
                    if (comp.If.casted_type) |casted_type| {
                        try node_queue.append(allocator, casted_type);
                    }
                    if (comp.If.else_branch) |else_branch| {
                        try node_queue.append(allocator, else_branch);
                    }
                },
                .List => {
                    try node_queue.appendSlice(allocator, comp.List.items);
                    if (comp.List.explicit_item_type) |item_type| {
                        try node_queue.append(allocator, item_type);
                    }
                },
                .ListType => try node_queue.append(allocator, comp.ListType),
                .Map => {
                    if (comp.Map.explicit_key_type) |key_type| {
                        try node_queue.append(allocator, key_type);
                    }

                    if (comp.Map.explicit_value_type) |value_type| {
                        try node_queue.append(allocator, value_type);
                    }

                    for (comp.Map.entries) |entry| {
                        try node_queue.append(allocator, entry.key);
                        try node_queue.append(allocator, entry.value);
                    }
                },
                .MapType => {
                    try node_queue.append(allocator, comp.MapType.key_type);
                    try node_queue.append(allocator, comp.MapType.value_type);
                },
                .NamedVariable => if (comp.NamedVariable.value) |value|
                    try node_queue.append(allocator, value),
                .ObjectDeclaration => {
                    try node_queue.appendSlice(allocator, comp.ObjectDeclaration.protocols);
                    for (comp.ObjectDeclaration.members) |member| {
                        if (member.property_type) |property_type| {
                            try node_queue.append(allocator, property_type);
                        }
                        if (member.method_or_default_value) |value| {
                            try node_queue.append(allocator, value);
                        }
                    }
                },
                .ObjectInit => {
                    if (comp.ObjectInit.object) |object| {
                        try node_queue.append(allocator, object);
                    }
                    for (comp.ObjectInit.properties) |property| {
                        try node_queue.append(allocator, property.value);
                    }
                },
                .Out => try node_queue.append(allocator, comp.Out),
                .ProtocolDeclaration => for (comp.ProtocolDeclaration.methods) |method| {
                    try node_queue.append(allocator, method.method);
                },
                .Range => {
                    try node_queue.append(allocator, comp.Range.low);
                    try node_queue.append(allocator, comp.Range.high);
                },
                .Resolve => try node_queue.append(allocator, comp.Resolve),
                .Resume => try node_queue.append(allocator, comp.Resume),
                .Return => if (comp.Return.value) |value|
                    try node_queue.append(allocator, value),
                .String => for (comp.String) |el|
                    try node_queue.append(allocator, el),
                .Subscript => {
                    try node_queue.append(allocator, comp.Subscript.subscripted);
                    try node_queue.append(allocator, comp.Subscript.index);
                    if (comp.Subscript.value) |value| {
                        try node_queue.append(allocator, value);
                    }
                },
                .Throw => try node_queue.append(allocator, comp.Throw.expression),
                .Try => {
                    try node_queue.append(allocator, comp.Try.body);
                    if (comp.Try.unconditional_clause) |clause| {
                        try node_queue.append(allocator, clause);
                    }
                    for (comp.Try.clauses) |clause| {
                        try node_queue.append(allocator, clause.type_def);
                        try node_queue.append(allocator, clause.body);
                    }
                },
                .TypeExpression => try node_queue.append(allocator, comp.TypeExpression),
                .TypeOfExpression => try node_queue.append(allocator, comp.TypeOfExpression),
                .Unary => try node_queue.append(allocator, comp.Unary.expression),
                .Unwrap => try node_queue.append(allocator, comp.Unwrap.unwrapped),
                .UserType => if (comp.UserType.generic_resolve) |generic_resolve|
                    try node_queue.append(allocator, generic_resolve),
                .VarDeclaration => {
                    if (comp.VarDeclaration.value) |value| {
                        try node_queue.append(allocator, value);
                    }
                    if (comp.VarDeclaration.type) |type_def| {
                        try node_queue.append(allocator, type_def);
                    }
                },
                .While => {
                    try node_queue.append(allocator, comp.While.condition);
                    try node_queue.append(allocator, comp.While.body);
                },
                .Yield => try node_queue.append(allocator, comp.Yield),
                .Boolean,
                .Break,
                .Continue,
                .Import,
                .Integer,
                .Double,
                .Null,
                .GenericType,
                .Namespace,
                .Pattern,
                .SimpleType,
                .StringLiteral,
                .Void,
                .Zdef,
                => {},
            }
        }
    }

    const UsesFiberContext = struct {
        result: bool = false,

        pub fn processNode(self: *UsesFiberContext, _: std.mem.Allocator, ast: Self.Slice, node: Self.Node.Index) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
            switch (ast.nodes.items(.tag)[node]) {
                .AsyncCall,
                .Resolve,
                .Resume,
                .Yield,
                => {
                    self.result = true;
                    return true;
                },
                else => return false,
            }
        }
    };

    pub fn usesFiber(self: Self.Slice, allocator: std.mem.Allocator, node: Node.Index) !bool {
        var ctx = UsesFiberContext{};

        try self.walk(allocator, &ctx, node);

        return ctx.result;
    }

    const IsConstantContext = struct {
        result: ?bool = null,

        pub fn processNode(self: *IsConstantContext, _: std.mem.Allocator, ast: Self.Slice, node: Self.Node.Index) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
            switch (ast.nodes.items(.tag)[node]) {
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
                .Double,
                .Pattern,
                .Null,
                .StringLiteral,
                .TypeExpression,
                .Void,
                .Namespace,
                => self.result = self.result == null or self.result.?,

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
                => {
                    self.result = false;
                    return true;
                },

                .Dot => {
                    const type_def = ast.nodes.items(.type_def)[ast.nodes.items(.components)[node].Dot.callee].?;

                    self.result = (self.result == null or self.result.?) and type_def.def_type == .Enum;

                    return true;
                },
                .If => {
                    const components = ast.nodes.items(.components)[node].If;

                    if (components.is_statement or components.casted_type != null) {
                        self.result = false;
                        return true;
                    }
                },
                .List => {
                    const components = ast.nodes.items(.components)[node].List;
                    const node_types = ast.nodes.items(.tag);

                    if (components.items.len == 0) {
                        self.result = self.result == null or self.result.?;
                    }

                    for (components.items) |item| {
                        if (node_types[item] == .List or node_types[item] == .Map) {
                            self.result = false;
                            return true;
                        }
                    }
                },
                .Map => {
                    const components = ast.nodes.items(.components)[node].Map;
                    const node_types = ast.nodes.items(.tag);

                    if (components.entries.len == 0) {
                        self.result = self.result == null or self.result.?;
                    }

                    for (components.entries) |entry| {
                        if (node_types[entry.key] == .List or node_types[entry.value] == .List or node_types[entry.key] == .Map or node_types[entry.key] == .Map) {
                            self.result = false;
                            return true;
                        }
                    }
                },
                .Subscript => {
                    const components = ast.nodes.items(.components)[node].Subscript;

                    if (components.value != null) {
                        self.result = false;
                        return true;
                    }
                },
                .String => {
                    if (ast.nodes.items(.components)[node].String.len == 0) {
                        self.result = self.result == null or self.result.?;
                    }
                },
                .As,
                .Binary,
                .Expression,
                .ForceUnwrap,
                .GenericResolve,
                .Grouping,
                .Is,
                .Range,
                .TypeOfExpression,
                .Unary,
                .Unwrap,
                => {},
            }

            return false;
        }
    };

    pub fn isConstant(self: Self.Slice, allocator: std.mem.Allocator, node: Node.Index) !bool {
        var ctx = IsConstantContext{};

        try self.walk(allocator, &ctx, node);

        return ctx.result orelse false;
    }

    fn binaryValue(self: Self.Slice, node: Node.Index, gc: *GarbageCollector) !?Value {
        const components = self.nodes.items(.components)[node].Binary;

        const left = try self.toValue(components.left, gc);
        const left_integer = if (left.isInteger()) left.integer() else null;
        const left_float = if (left.isDouble()) left.double() else null;
        const right = try self.toValue(components.right, gc);
        const right_integer = if (right.isInteger()) right.integer() else null;
        const right_float = if (right.isDouble()) right.double() else null;

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
                        return Value.fromBoolean(lf > @as(v.Double, @floatFromInt(right_integer.?)));
                    }
                } else {
                    if (right_float) |rf| {
                        return Value.fromBoolean(@as(v.Double, @floatFromInt(left_integer.?)) > rf);
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
                        return Value.fromBoolean(lf < @as(v.Double, @floatFromInt(right_integer.?)));
                    }
                } else {
                    if (right_float) |rf| {
                        return Value.fromBoolean(@as(v.Double, @floatFromInt(left_integer.?)) < rf);
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
                        return Value.fromBoolean(lf >= @as(v.Double, @floatFromInt(right_integer.?)));
                    }
                } else {
                    if (right_float) |rf| {
                        return Value.fromBoolean(@as(v.Double, @floatFromInt(left_integer.?)) >= rf);
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
                        return Value.fromBoolean(lf <= @as(v.Double, @floatFromInt(right_integer.?)));
                    }
                } else {
                    if (right_float) |rf| {
                        return Value.fromBoolean(@as(v.Double, @floatFromInt(left_integer.?)) <= rf);
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

                    return (try gc.copyString(try new_string.toOwnedSlice())).toValue();
                } else if (right_float) |rf| {
                    return Value.fromDouble(rf + left_float.?);
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
                    return Value.fromDouble(rf - left_float.?);
                }

                return Value.fromInteger(right_integer.? -% left_integer.?);
            },
            .Star => {
                if (right_float) |rf| {
                    return Value.fromDouble(rf * left_float.?);
                }

                return Value.fromInteger(right_integer.? *% left_integer.?);
            },
            .Slash => {
                if (right_float) |rf| {
                    return Value.fromDouble(left_float.? / rf);
                }

                return Value.fromInteger(@divTrunc(left_integer.?, right_integer.?));
            },
            .Percent => {
                if (right_float) |rf| {
                    return Value.fromDouble(@mod(left_float.?, rf));
                }

                return Value.fromInteger(@mod(left_integer.?, right_integer.?));
            },
            .And => return Value.fromBoolean(left.boolean() and right.boolean()),
            .Or => return Value.fromBoolean(left.boolean() or right.boolean()),
            else => unreachable,
        }
    }

    pub fn toValue(self: Self.Slice, node: Node.Index, gc: *GarbageCollector) Error!Value {
        const value = &self.nodes.items(.value)[node];

        if (value.* == null and try self.isConstant(gc.allocator, node)) {
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
                .StringLiteral => self.nodes.items(.components)[node].StringLiteral.literal.toValue(),
                .TypeOfExpression => (try (try self.toValue(
                    self.nodes.items(.components)[node].TypeOfExpression,
                    gc,
                )).typeOf(gc)).toValue(),
                .TypeExpression => self.nodes.items(.type_def)[self.nodes.items(.components)[node].TypeExpression].?.toValue(),
                .Pattern => self.nodes.items(.components)[node].Pattern.toValue(),
                .Void => Value.Void,
                .Null => Value.Null,
                .Double => Value.fromDouble(self.nodes.items(.components)[node].Double),
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
                .GenericResolve => try self.toValue(self.nodes.items(.components)[node].GenericResolve.expression, gc),
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

                    break :string (try gc.copyString(try string.toOwnedSlice())).toValue();
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
                            Value.fromDouble(-val.double()),
                        else => unreachable,
                    };
                },
                .Unwrap => try self.toValue(self.nodes.items(.components)[node].Unwrap.unwrapped, gc),
                else => null,
            };
        }

        return value.*.?;
    }
};

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

pub fn slice(self: Self) Slice {
    return .{
        .tokens = self.tokens.slice(),
        .nodes = self.nodes.slice(),
        .root = self.root,
    };
}

pub inline fn appendNode(self: *Self, node: Node) !Node.Index {
    try self.nodes.append(self.allocator, node);

    return @intCast(self.nodes.len - 1);
}

pub inline fn appendToken(self: *Self, token: Token) !TokenIndex {
    // if (token.tag == .Semicolon and self.tokens.items(.tag)[self.tokens.len - 1] == .Semicolon) {
    //     unreachable;
    // }

    try self.tokens.append(self.allocator, token);

    return @intCast(self.tokens.len - 1);
}

pub fn swapNodes(self: *Self, from: Node.Index, to: Node.Index) void {
    const from_node = self.nodes.get(from);
    const to_node = self.nodes.get(to);

    self.nodes.set(from, to_node);
    self.nodes.set(to, from_node);
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

    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        if (self.ends_scope) |ends_scope| {
            allocator.free(ends_scope);
        }

        self.components.deinit(allocator);
    }

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
        Double,
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
        Break: BreakContinue,
        Call: Call,
        Continue: BreakContinue,
        Dot: Dot,
        DoUntil: WhileDoUntil,
        Enum: Enum,
        Export: Export,
        Expression: Node.Index,
        FiberType: FiberType,
        Double: v.Double,
        For: For,
        ForceUnwrap: Unwrap,
        ForEach: ForEach,
        Function: Function,
        FunctionType: FunctionType,
        FunDeclaration: FunDeclaration,
        GenericResolve: GenericResolve,
        GenericResolveType: []const Node.Index,
        GenericType: void,
        Grouping: Node.Index,
        If: If,
        Import: Import,
        Integer: v.Integer,
        Is: IsAs,
        List: List,
        ListType: Node.Index,
        Map: Map,
        MapType: MapType,
        Namespace: []const TokenIndex,
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
        StringLiteral: StringLiteral,
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

        pub fn deinit(self: Components, allocator: std.mem.Allocator) void {
            switch (self) {
                .AnonymousObjectType => allocator.free(self.AnonymousObjectType.fields),
                .Block => allocator.free(self.Block),
                .BlockExpression => allocator.free(self.BlockExpression),
                .Call => allocator.free(self.Call.arguments),
                .Enum => allocator.free(self.Enum.cases),
                .Export => if (self.Export.name) |name| allocator.free(name) else void,
                .For => {
                    allocator.free(self.For.init_declarations);
                    allocator.free(self.For.post_loop);
                },
                .Function => {
                    self.Function.upvalue_binding.deinit(allocator);
                },
                .FunctionType => {
                    allocator.free(self.FunctionType.error_types);
                    allocator.free(self.FunctionType.arguments);
                    allocator.free(self.FunctionType.generic_types);
                },
                .GenericResolveType => allocator.free(self.GenericResolveType.resolved_types),
                .Import => {
                    allocator.free(self.Import.imported_symbols);
                    if (self.Import.prefix) |prefix| {
                        allocator.free(prefix);
                    }
                },
                .List => allocator.free(self.List.items),
                .Map => allocator.free(self.Map.entries),
                .Namespace => allocator.free(self.Namespace),
                .NamedVariable => allocator.free(self.NamedVariable.name),
                .ObjectDeclaration => {
                    allocator.free(self.ObjectDeclaration.protocols);
                    allocator.free(self.ObjectDeclaration.generics);
                    allocator.free(self.ObjectDeclaration.members);
                },
                .ObjectInit => allocator.free(self.ObjectInit.properties),
                .ProtocolDeclaration => allocator.free(self.ProtocolDeclaration.methods),
                .String => allocator.free(self.String),
                .Try => allocator.free(self.Try.clauses),
                .UserType => allocator.free(self.UserType.name),
                .Zdef => allocator.free(self.Zdef.elements),
                .AsyncCall,
                .As,
                .Binary,
                .Boolean,
                .Break,
                .Continue,
                .Dot,
                .DoUntil,
                .Expression,
                .FiberType,
                .Double,
                .ForceUnwrap,
                .ForEach,
                .FunDeclaration,
                .GenericResolve,
                .GenericType,
                .Grouping,
                .If,
                .Integer,
                .Is,
                .ListType,
                .MapType,
                .Null,
                .Out,
                .Pattern,
                .Range,
                .Resolve,
                .Resume,
                .Return,
                .SimpleType,
                .StringLiteral,
                .Subscript,
                .Throw,
                .TypeExpression,
                .TypeOfExpression,
                .Unary,
                .Unwrap,
                .VarDeclaration,
                .Void,
                .While,
                .Yield,
                => {},
            }
        }
    };
};

pub const AnonymousObjectType = struct {
    fields: []const Field,

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

pub const BreakContinue = struct {
    label: ?TokenIndex,
    destination: ?Node.Index,
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
        Value: struct {
            value: Node.Index,
            assign_token: TokenIndex,
        },
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
    cases: []const Case,
    values_omitted: bool,

    pub const Case = struct {
        name: TokenIndex,
        docblock: ?TokenIndex,
        value: ?Node.Index,
    };
};

pub const Export = struct {
    name: ?[]const TokenIndex,
    alias: ?TokenIndex,
    declaration: ?Node.Index,
};

pub const FiberType = struct {
    return_type: Node.Index,
    yield_type: Node.Index,
};

pub const For = struct {
    init_declarations: []const Node.Index,
    condition: Node.Index,
    post_loop: []const Node.Index,
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

    upvalue_binding: std.AutoArrayHashMapUnmanaged(u8, bool),

    // If the function is a ScritEntryPoint
    entry: ?Entry = null,

    // Set when the function is first generated
    // The JIT compiler can then reference it when creating its closure
    native: ?*obj.ObjNative = null,
    function: ?*obj.ObjFunction = null,

    import_root: bool = false,

    pub const Entry = struct {
        main_slot: ?usize = null,
        push_cli_args: bool = false,
        main_location: ?TokenIndex = null,
        test_slots: []const usize,
        test_locations: []const TokenIndex,
        exported_count: usize = 0,
    };
};

pub const FunctionType = struct {
    is_signature: bool,
    name: ?TokenIndex,
    return_type: ?Node.Index,
    yield_type: ?Node.Index,
    error_types: []const Node.Index,
    arguments: []const Argument,
    /// If the struct is use as the function signature, this is a list of nodes otherwise its a list of tokens
    /// Since `TokenIndex` and `Node.Index` are actually the same type, we don't make this any more complicated
    generic_types: []const TokenIndex,
    lambda: bool,

    pub const Argument = struct {
        name: TokenIndex,
        type: Node.Index,
        default: ?Node.Index,
    };
};

pub const GenericResolve = struct {
    expression: Node.Index,
    resolved_types: []const Node.Index,
};

pub const FunDeclaration = struct {
    function: Node.Index,

    slot: Slot,
    slot_type: SlotType,
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
    imported_symbols: []const TokenIndex,
    prefix: ?[]const TokenIndex,
    path: TokenIndex,
    import: ?Parser.ScriptImport,
};

pub const IsAs = struct {
    left: Node.Index,
    constant: Node.Index,
};

pub const List = struct {
    explicit_item_type: ?Node.Index,
    items: []const Node.Index,
};

pub const Map = struct {
    explicit_key_type: ?Node.Index,
    explicit_value_type: ?Node.Index,

    entries: []const Entry,

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
    name: []const TokenIndex,
    definition: Node.Index,
    value: ?Node.Index,
    assign_token: ?TokenIndex,
    slot: Slot,
    slot_type: SlotType,
    slot_final: bool,
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
        property_type: ?Node.Index,
    };
};

pub const ObjectInit = struct {
    object: ?Node.Index, // Should be a NamedVariableNode or GenericResolve
    properties: []const Property,

    pub const Property = struct {
        name: TokenIndex,
        value: Node.Index,
    };
};

pub const ProtocolDeclaration = struct {
    name: TokenIndex,
    slot: Slot,
    methods: []const Method,

    pub const Method = struct {
        docblock: ?TokenIndex,
        method: Node.Index,
    };
};

pub const Range = struct {
    low: Node.Index,
    high: Node.Index,
};

pub const Return = struct {
    value: ?Node.Index,
    unconditional: bool,
};

pub const StringLiteral = struct {
    delimiter: u8,
    literal: *obj.ObjString,
};

pub const Subscript = struct {
    subscripted: Node.Index,
    index: Node.Index,
    value: ?Node.Index,
    checked: bool,
};

pub const Throw = struct {
    expression: Node.Index,
    unconditional: bool,
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
};

pub const Unary = struct {
    operator: Token.Type,
    expression: Node.Index,
};

pub const Unwrap = struct {
    unwrapped: Node.Index,
    original_type: *obj.ObjTypeDef,
    start_opt_jumps: bool,
};

pub const UserType = struct {
    name: []const TokenIndex,
    generic_resolve: ?Node.Index,
};

pub const VarDeclaration = struct {
    name: TokenIndex,
    value: ?Node.Index,
    type: ?Node.Index,
    final: bool,
    omits_qualifier: bool,
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
