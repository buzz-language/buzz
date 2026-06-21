const std = @import("std");
const obj = @import("obj.zig");
const Token = @import("Token.zig");
const Chunk = @import("Chunk.zig");
const v = @import("value.zig");
const Value = v.Value;
const FFI = @import("FFI.zig");
const Parser = @import("Parser.zig");
const GC = @import("GC.zig");
// TODO: cleanup Error sets!
const Error = @import("Codegen.zig").Error;
const Reporter = @import("Reporter.zig");
const TypeChecker = @import("TypeChecker.zig");

const Self = @This();

// Since the AST must live for the whole program lifetime (can be used by the JIT)
// there's no deinit functions. Everything should be allocated with an ArenaAllocator

pub const TokenIndex = u32;
pub const TokenList = std.MultiArrayList(Token);
pub const NodeList = std.MultiArrayList(Node);

pub const WalkStrategy = enum {
    breadthFirst,
    depthFirst,
};

allocator: std.mem.Allocator,
tokens: TokenList,
nodes: NodeList,
root: ?Node.Index = null,

pub const Slice = struct {
    tokens: TokenList.Slice,
    nodes: NodeList.Slice,
    root: ?Node.Index,

    /// Walk the AST, calling a callback for each node that can stop the walking from going deeper by returning true.
    /// ctx should have:
    /// - `fn processNode(ctx: @TypeOf(ctx), allocator: std.mem.Allocator, ast: Ast.Slice, node: Node.Index) error{OutOfMemory}!bool: returns true to stop walking deeper
    /// ctx can optionally have, only used by depth-first walks:
    /// - `fn enterNode(ctx: @TypeOf(ctx), allocator: std.mem.Allocator, ast: Ast.Slice, node: Node.Index) !void`
    /// - `fn exitNode(ctx: @TypeOf(ctx), allocator: std.mem.Allocator, ast: Ast.Slice, node: Node.Index) !void`
    // FIXME: the context should have a error type we can reuse in processNode signature
    pub fn walk(self: Slice, allocator: std.mem.Allocator, ctx: anytype, root: Node.Index, comptime strategy: WalkStrategy) !void {
        switch (strategy) {
            .breadthFirst => try self.walkBreadthFirst(allocator, ctx, root),
            .depthFirst => try self.walkDepthFirst(allocator, ctx, root),
        }
    }

    fn walkBreadthFirst(self: Slice, allocator: std.mem.Allocator, ctx: anytype, root: Node.Index) !void {
        var node_queue = std.ArrayList(Node.Index).empty;
        try node_queue.append(allocator, root);
        defer node_queue.deinit(allocator);

        while (node_queue.items.len > 0) {
            const node = node_queue.orderedRemove(0);

            if (try ctx.processNode(allocator, self, node)) {
                continue;
            }

            try self.appendWalkChildren(allocator, &node_queue, node, .breadthFirst);
        }
    }

    fn walkDepthFirst(self: Slice, allocator: std.mem.Allocator, ctx: anytype, node: Node.Index) !void {
        try self.enterWalkNode(allocator, ctx, node);

        const skip_children = try ctx.processNode(allocator, self, node);
        if (!skip_children) {
            var children = std.ArrayList(Node.Index).empty;
            defer children.deinit(allocator);

            try self.appendWalkChildren(allocator, &children, node, .depthFirst);

            for (children.items) |child| {
                try self.walkDepthFirst(allocator, ctx, child);
            }
        }

        try self.exitWalkNode(allocator, ctx, node);
    }

    fn contextType(comptime Context: type) type {
        return switch (@typeInfo(Context)) {
            .pointer => |pointer| pointer.child,
            else => Context,
        };
    }

    fn enterWalkNode(self: Slice, allocator: std.mem.Allocator, ctx: anytype, node: Node.Index) !void {
        const Context = contextType(@TypeOf(ctx));

        if (@hasDecl(Context, "enterNode")) {
            try ctx.enterNode(allocator, self, node);
        }
    }

    fn exitWalkNode(self: Slice, allocator: std.mem.Allocator, ctx: anytype, node: Node.Index) !void {
        const Context = contextType(@TypeOf(ctx));

        if (@hasDecl(Context, "exitNode")) {
            try ctx.exitNode(allocator, self, node);
        }
    }

    fn appendWalkChildren(
        self: Slice,
        allocator: std.mem.Allocator,
        node_queue: *std.ArrayList(Node.Index),
        node: Node.Index,
        comptime strategy: WalkStrategy,
    ) !void {
        const tags = self.nodes.items(.tag);
        const components = self.nodes.items(.components);
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
            .Match => {
                try node_queue.append(allocator, comp.Match.value);

                for (comp.Match.branches) |branch| {
                    try node_queue.appendSlice(allocator, branch.conditions);
                    try node_queue.append(allocator, branch.expression);
                }

                if (comp.Match.else_branch) |else_branch| {
                    try node_queue.append(allocator, else_branch);
                }
            },
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
            .DoUntil => switch (strategy) {
                .breadthFirst => {
                    try node_queue.append(allocator, comp.DoUntil.condition);
                    try node_queue.append(allocator, comp.DoUntil.body);
                },
                .depthFirst => {
                    try node_queue.append(allocator, comp.DoUntil.body);
                    try node_queue.append(allocator, comp.DoUntil.condition);
                },
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
            .For => switch (strategy) {
                .breadthFirst => {
                    try node_queue.append(allocator, comp.For.condition);
                    try node_queue.append(allocator, comp.For.body);
                    try node_queue.appendSlice(allocator, comp.For.init_declarations);
                    try node_queue.appendSlice(allocator, comp.For.post_loop);
                },
                .depthFirst => {
                    try node_queue.appendSlice(allocator, comp.For.init_declarations);
                    try node_queue.append(allocator, comp.For.condition);
                    try node_queue.appendSlice(allocator, comp.For.post_loop);
                    try node_queue.append(allocator, comp.For.body);
                },
            },
            .ForceUnwrap => try node_queue.append(allocator, comp.ForceUnwrap.unwrapped),
            .ForEach => switch (strategy) {
                .breadthFirst => {
                    try node_queue.append(allocator, comp.ForEach.iterable);
                    try node_queue.append(allocator, comp.ForEach.body);
                    try node_queue.append(allocator, comp.ForEach.key);
                    try node_queue.append(allocator, comp.ForEach.value);
                },
                .depthFirst => {
                    try node_queue.append(allocator, comp.ForEach.key);
                    try node_queue.append(allocator, comp.ForEach.value);
                    try node_queue.append(allocator, comp.ForEach.iterable);
                    try node_queue.append(allocator, comp.ForEach.body);
                },
            },
            .Function => switch (strategy) {
                .breadthFirst => {
                    if (comp.Function.body) |body| {
                        try node_queue.append(allocator, body);
                    }

                    if (comp.Function.function_signature) |signature| {
                        try node_queue.append(allocator, signature);
                    }
                },
                .depthFirst => {
                    if (comp.Function.function_signature) |signature| {
                        try node_queue.append(allocator, signature);
                    }

                    if (comp.Function.body) |body| {
                        try node_queue.append(allocator, body);
                    }
                },
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
            .If => switch (strategy) {
                .breadthFirst => {
                    try node_queue.append(allocator, comp.If.condition);
                    try node_queue.append(allocator, comp.If.body);
                    if (comp.If.casted_type) |casted_type| {
                        try node_queue.append(allocator, casted_type);
                    }
                    if (comp.If.else_branch) |else_branch| {
                        try node_queue.append(allocator, else_branch);
                    }
                },
                .depthFirst => {
                    try node_queue.append(allocator, comp.If.condition);
                    if (comp.If.casted_type) |casted_type| {
                        try node_queue.append(allocator, casted_type);
                    }
                    try node_queue.append(allocator, comp.If.body);
                    if (comp.If.else_branch) |else_branch| {
                        try node_queue.append(allocator, else_branch);
                    }
                },
            },
            .List => switch (strategy) {
                .breadthFirst => {
                    try node_queue.appendSlice(allocator, comp.List.items);
                    if (comp.List.explicit_item_type) |item_type| {
                        try node_queue.append(allocator, item_type);
                    }
                },
                .depthFirst => {
                    if (comp.List.explicit_item_type) |item_type| {
                        try node_queue.append(allocator, item_type);
                    }
                    try node_queue.appendSlice(allocator, comp.List.items);
                },
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
            .VarDeclaration => switch (strategy) {
                .breadthFirst => {
                    if (comp.VarDeclaration.value) |value| {
                        try node_queue.append(allocator, value);
                    }
                    if (comp.VarDeclaration.type) |type_def| {
                        try node_queue.append(allocator, type_def);
                    }
                },
                .depthFirst => {
                    if (comp.VarDeclaration.type) |type_def| {
                        try node_queue.append(allocator, type_def);
                    }
                    if (comp.VarDeclaration.value) |value| {
                        try node_queue.append(allocator, value);
                    }
                },
            },
            .While => {
                try node_queue.append(allocator, comp.While.condition);
                try node_queue.append(allocator, comp.While.body);
            },
            .Yield => try node_queue.append(allocator, comp.Yield),
            .AnonymousEnumCase,
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

    /// Restores the visible declaration list when the depth-first walk exits a lexical scope.
    const ScopeCheckpoint = struct {
        /// Node whose exit ends this scope.
        end_node: Node.Index,
        /// Visible declaration count before entering this scope.
        visible_len: usize,
    };

    /// Depth-first walk context that snapshots declarations visible at a source offset.
    const ScopeAwareContext = struct {
        /// Root node of the visibility query.
        root: Node.Index,
        /// Source byte offset whose lexical environment is being queried.
        offset: usize,
        /// Declarations currently visible at the walk position, in declaration order.
        visible: std.ArrayList(Node.Index) = .empty,
        /// Stack of lexical scopes and the visible list size each scope must restore.
        scope_checkpoints: std.ArrayList(ScopeCheckpoint) = .empty,
        /// Captured visible declaration snapshot once the requested offset is reached.
        result: ?[]const Node.Index = null,

        /// Frees scratch state owned by the context; `result` is returned to the caller.
        fn deinit(self: *ScopeAwareContext, allocator: std.mem.Allocator) void {
            self.visible.deinit(allocator);
            self.scope_checkpoints.deinit(allocator);
        }

        /// Copies the currently visible declarations once a lookup reaches its target.
        fn captureResult(self: *ScopeAwareContext, allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
            if (self.result == null) {
                self.result = try allocator.dupe(Node.Index, self.visible.items);
            }
        }

        /// Marks a lexical scope boundary and remembers how many declarations were visible on entry.
        fn pushScope(self: *ScopeAwareContext, allocator: std.mem.Allocator, end_node: Node.Index) std.mem.Allocator.Error!void {
            try self.scope_checkpoints.append(
                allocator,
                .{
                    .end_node = end_node,
                    .visible_len = self.visible.items.len,
                },
            );
        }

        /// Enters scopes before processing children; some parser scopes end at a child body node.
        pub fn enterNode(
            self: *ScopeAwareContext,
            allocator: std.mem.Allocator,
            ast: Self.Slice,
            node: Self.Node.Index,
        ) std.mem.Allocator.Error!void {
            if (self.result != null) {
                return;
            }

            const components = ast.nodes.items(.components);
            switch (ast.nodes.items(.tag)[node]) {
                .Function, .BlockExpression => try self.pushScope(allocator, node),
                .If => try self.pushScope(allocator, components[node].If.body),
                .While => try self.pushScope(allocator, components[node].While.body),
                .DoUntil => try self.pushScope(allocator, components[node].DoUntil.body),
                .For, .ForEach => try self.pushScope(allocator, node),
                else => if (ast.nodes.items(.ends_scope)[node] != null and
                    (self.scope_checkpoints.items.len == 0 or
                        self.scope_checkpoints.items[self.scope_checkpoints.items.len - 1].end_node != node))
                {
                    try self.pushScope(allocator, node);
                },
            }
        }

        /// Exits scopes whose lifetime ends at this node and drops declarations from those scopes.
        pub fn exitNode(
            self: *ScopeAwareContext,
            allocator: std.mem.Allocator,
            ast: Self.Slice,
            node: Self.Node.Index,
        ) std.mem.Allocator.Error!void {
            const node_ends_current_scope = self.scope_checkpoints.items.len > 0 and
                self.scope_checkpoints.items[self.scope_checkpoints.items.len - 1].end_node == node;

            if (self.result == null and (node_ends_current_scope or node == self.root)) {
                const token_offsets = ast.tokens.items(.offset);
                const lexemes = ast.tokens.items(.lexeme);
                const locations = ast.nodes.items(.location);
                const end_locations = ast.nodes.items(.end_location);
                const node_start = token_offsets[locations[node]];
                const node_end = token_offsets[end_locations[node]] + lexemes[end_locations[node]].len;

                // Only lexical scopes can be the fallback capture point. Other nodes, such as
                // imports, can have ranges from another source and must not stop the walk early.
                if ((node_start <= self.offset and self.offset <= node_end) or node == self.root) {
                    try self.captureResult(allocator);
                }
            }

            while (self.scope_checkpoints.items.len > 0 and
                self.scope_checkpoints.items[self.scope_checkpoints.items.len - 1].end_node == node)
            {
                const checkpoint = self.scope_checkpoints.pop().?;
                self.visible.shrinkRetainingCapacity(checkpoint.visible_len);
            }
        }

        /// Captures visible declarations at the source offset and records declarations as they become visible.
        pub fn processNode(
            self: *ScopeAwareContext,
            allocator: std.mem.Allocator,
            ast: Self.Slice,
            node: Self.Node.Index,
        ) std.mem.Allocator.Error!bool {
            if (self.result != null) {
                return true;
            }

            const token_offsets = ast.tokens.items(.offset);
            const lexemes = ast.tokens.items(.lexeme);
            const locations = ast.nodes.items(.location);
            const end_locations = ast.nodes.items(.end_location);
            const node_start = token_offsets[locations[node]];

            if (node_start >= self.offset) {
                try self.captureResult(allocator);
                return true;
            }

            if (ast.nodes.items(.tag)[node] == .VarDeclaration) {
                const declaration = ast.nodes.items(.components)[node].VarDeclaration;
                const node_end = token_offsets[end_locations[node]] + lexemes[end_locations[node]].len;

                const declaration_is_before_offset = node_end <= self.offset;
                const offset_inside_declaration = node_start <= self.offset and self.offset < node_end;

                if (!declaration.implicit and declaration_is_before_offset and !offset_inside_declaration) {
                    try self.visible.append(allocator, node);
                }
            }

            return false;
        }
    };

    /// Returns declarations visible in the lexical scope at `offset` in the source.
    pub fn visibleSymbolsAtOffset(
        self: Self.Slice,
        allocator: std.mem.Allocator,
        root: Node.Index,
        offset: usize,
    ) ![]const Node.Index {
        var ctx = ScopeAwareContext{
            .root = root,
            .offset = offset,
        };
        defer ctx.deinit(allocator);

        try self.walk(allocator, &ctx, root, .depthFirst);

        return ctx.result orelse &.{};
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

        try self.walk(allocator, &ctx, node, .breadthFirst);

        return ctx.result;
    }

    const IsConstantContext = struct {
        result: ?bool = null,

        pub fn processNode(
            self: *IsConstantContext,
            _: std.mem.Allocator,
            ast: Self.Slice,
            node: Self.Node.Index,
        ) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
            switch (ast.nodes.items(.tag)[node]) {
                .AnonymousObjectType,
                .AnonymousEnumCase,
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
                .Match => {
                    const components = ast.nodes.items(.components)[node].Match;
                    const type_defs = ast.nodes.items(.type_def);
                    const value_type_def = type_defs[components.value].?;

                    if (components.is_statement) {
                        self.result = false;
                        return true;
                    }

                    for (components.branches) |branch| {
                        for (branch.conditions) |condition| {
                            const condition_type_def = type_defs[condition].?;

                            if ((!condition_type_def.optional and condition_type_def.def_type == .Pattern and
                                value_type_def.def_type == .String and !value_type_def.optional) or
                                (!condition_type_def.optional and condition_type_def.def_type == .String and
                                    value_type_def.def_type == .Pattern and !value_type_def.optional))
                            {
                                self.result = false;
                                return true;
                            }
                        }
                    }
                },
                .List => {
                    const components = ast.nodes.items(.components)[node].List;
                    const node_types = ast.nodes.items(.tag);

                    if (ast.nodes.items(.type_def)[node].?.resolved_type.?.List.mutable) {
                        self.result = false;
                        return true;
                    }

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

                    if (ast.nodes.items(.type_def)[node].?.resolved_type.?.Map.mutable) {
                        self.result = false;
                        return true;
                    }

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

        try self.walk(allocator, &ctx, node, .breadthFirst);

        return ctx.result orelse false;
    }

    /// Mirrors Chunk.score (even though Chunk.score and Node.score won't be comparable)
    /// Is used to compute complexity of a hotspot node (which doesn't have a Chunk available to evaluate)
    const ComplexityContext = struct {
        score: usize = 0,

        pub fn processNode(
            ctx: *ComplexityContext,
            _: std.mem.Allocator,
            ast: Self.Slice,
            node: Self.Node.Index,
        ) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
            if (ast.nodes.items(.complexity_score)[node]) |sc| {
                ctx.score += sc;
                return true; // Don't go deeper we already computed this node score
            }

            ctx.score += switch (ast.nodes.items(.tag)[node]) {
                .AsyncCall,
                .Resolve,
                .Resume,
                => { // Blacklist because of fiber use
                    ctx.score = 0;
                    return true;
                },
                .Call,
                .DoUntil,
                .For,
                .ForEach,
                .Throw,
                .Try,
                .While,
                => @as(usize, @intCast(1)),
                else => @as(usize, @intCast(0)),
            } + 1; // At least 1 per node

            return false;
        }
    };

    pub fn score(self: Self.Slice, allocator: std.mem.Allocator, node: Node.Index) !usize {
        const complexity_score = &self.nodes.items(.complexity_score)[node];
        if (complexity_score.* == null) {
            var ctx = ComplexityContext{};

            try self.walk(allocator, &ctx, node, .breadthFirst);

            complexity_score.* = ctx.score;
        }

        return complexity_score.* orelse 0;
    }

    const NamespaceContext = struct {
        namespace: ?[]const TokenIndex = null,

        pub fn processNode(
            ctx: *NamespaceContext,
            _: std.mem.Allocator,
            ast: Self.Slice,
            node: Self.Node.Index,
        ) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
            if (ast.nodes.items(.tag)[node] == .Namespace) {
                ctx.namespace = ast.nodes.items(.components)[node].Namespace;
                return true;
            }

            return false;
        }
    };

    pub fn namespace(self: Self.Slice, allocator: std.mem.Allocator, node: Node.Index) !?[]const TokenIndex {
        var ctx = NamespaceContext{};

        try self.walk(allocator, &ctx, node, .breadthFirst);

        return ctx.namespace;
    }

    fn binaryValue(self: Self.Slice, node: Node.Index, gc: *GC) !?Value {
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
                    var new_string = std.ArrayList(u8).empty;
                    try new_string.appendSlice(gc.allocator, left_string.?.string);
                    try new_string.appendSlice(gc.allocator, rs.string);

                    return (try gc.copyString(try new_string.toOwnedSlice(gc.allocator))).toValue();
                } else if (right_float) |rf| {
                    return Value.fromDouble(rf + left_float.?);
                } else if (right_integer) |ri| {
                    return Value.fromInteger(ri +% left_integer.?);
                } else if (right_list) |rl| {
                    var new_list = std.ArrayList(Value).empty;
                    try new_list.appendSlice(gc.allocator, left_list.?.items.items);
                    try new_list.appendSlice(gc.allocator, rl.items.items);

                    return (try gc.allocateObject(
                        obj.ObjList{
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
                        obj.ObjMap{
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
                if (right_float == 0 or right_integer == 0) {
                    return error.DivisionByZero;
                }

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

    fn matchConditionValue(
        self: Self.Slice,
        gc: *GC,
        value_type_def: *obj.ObjTypeDef,
        match_value: Value,
        condition: Node.Index,
    ) !bool {
        const condition_type_def = self.nodes.items(.type_def)[condition].?;
        const condition_value = try self.toValue(condition, gc);

        if (!condition_type_def.optional and condition_type_def.def_type == .Range and
            (value_type_def.def_type == .Integer or value_type_def.def_type == .Double) and
            !value_type_def.optional)
        {
            const range = obj.ObjRange.cast(condition_value.obj()).?;
            const number = if (match_value.isInteger())
                @as(v.Double, @floatFromInt(match_value.integer()))
            else
                match_value.double();
            const low: v.Double = @floatFromInt(range.low);
            const high: v.Double = @floatFromInt(range.high);

            return (high >= low and number >= low and number < high) or
                (low >= high and number >= high and number < low);
        } else if (!condition_type_def.optional and condition_type_def.def_type == .Type and
            (value_type_def.optional or value_type_def.def_type != .Type))
        {
            return condition_value.is(match_value);
        } else if (!value_type_def.optional and value_type_def.def_type == .Type and
            (condition_type_def.optional or condition_type_def.def_type != .Type))
        {
            return match_value.is(condition_value);
        } else {
            return match_value.eql(condition_value);
        }
    }

    pub fn typeCheckAndToValue(
        self: Self.Slice,
        node: Node.Index,
        reporter: *Reporter,
        gc: *GC,
    ) Error!Value {
        if (try TypeChecker.check(
            self,
            reporter,
            gc,
            null,
            node,
        )) {
            return Value.Void;
        }

        return self.toValue(node, gc);
    }

    pub fn toValue(
        self: Self.Slice,
        node: Node.Index,
        gc: *GC,
    ) Error!Value {
        const value = &self.nodes.items(.value)[node];
        // const type_defs = self.nodes.items(.type_def);
        const components = self.nodes.items(.components);

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
                .StringLiteral => components[node].StringLiteral.literal.toValue(),
                .TypeOfExpression => (try (try self.toValue(components[node].TypeOfExpression, gc)).typeOf(gc)).toValue(),
                .TypeExpression => self.nodes.items(.type_def)[components[node].TypeExpression].?.toValue(),
                .Pattern => components[node].Pattern.toValue(),
                .Void => Value.Void,
                .Null => Value.Null,
                .Double => Value.fromDouble(components[node].Double),
                .Integer => Value.fromInteger(components[node].Integer),
                .Boolean => Value.fromBoolean(components[node].Boolean),
                .AnonymousEnumCase => enum_case: {
                    const components_ptr = &components[node].AnonymousEnumCase;
                    const type_def = self.nodes.items(.type_def)[node].?;
                    const case_name = self.tokens.items(.lexeme)[components_ptr.case_name];
                    const enum_type_def = type_def.resolved_type.?.EnumInstance
                        .of
                        .resolved_type.?.Enum;

                    for (enum_type_def.cases, 0..) |case, idx| {
                        if (std.mem.eql(u8, case, case_name)) {
                            components_ptr.resolved_case = @intCast(idx);

                            break :enum_case (try gc.allocateObject(
                                obj.ObjEnumInstance{
                                    .enum_ref = enum_type_def.value.?,
                                    .case = @intCast(idx),
                                },
                            )).toValue();
                        }
                    }

                    @panic("Could not constant fold anonymous enum case");
                },
                .As => try self.toValue(components[node].As.left, gc),
                .Is => is: {
                    const is_components = components[node].Is;
                    break :is Value.fromBoolean(
                        (try self.toValue(is_components.constant, gc))
                            .is(try self.toValue(is_components.left, gc)),
                    );
                },
                .Binary => try self.binaryValue(node, gc),
                .Dot => dot: {
                    // Only Enum.case can be constant
                    const dot_components = components[node].Dot;
                    const type_def = self.nodes.items(.type_def)[dot_components.callee].?;

                    break :dot (try gc.allocateObject(
                        obj.ObjEnumInstance{
                            .enum_ref = type_def.resolved_type.?.Enum.value.?,
                            .case = @intCast(dot_components.value_or_call_or_enum.EnumCase),
                        },
                    )).toValue();
                },
                .Expression => try self.toValue(components[node].Expression, gc),
                .Grouping => try self.toValue(components[node].Grouping, gc),
                .ForceUnwrap => fc: {
                    const unwrapped = try self.toValue(components[node].ForceUnwrap.unwrapped, gc);

                    if (unwrapped.isNull()) {
                        return Error.UnwrappedNull;
                    }

                    break :fc unwrapped;
                },
                .GenericResolve => try self.toValue(components[node].GenericResolve.expression, gc),
                .If => @"if": {
                    const if_components = components[node].If;
                    break :@"if" if ((try self.toValue(if_components.condition, gc)).boolean())
                        try self.toValue(if_components.body, gc)
                    else
                        try self.toValue(if_components.else_branch.?, gc);
                },
                .Match => match: {
                    const match_components = components[node].Match;
                    const value_type_def = self.nodes.items(.type_def)[match_components.value].?;
                    const match_value = try self.toValue(match_components.value, gc);

                    for (match_components.branches) |branch| {
                        for (branch.conditions) |condition| {
                            if (try self.matchConditionValue(
                                gc,
                                value_type_def,
                                match_value,
                                condition,
                            )) {
                                break :match try self.toValue(branch.expression, gc);
                            }
                        }
                    }

                    break :match if (match_components.else_branch) |else_branch|
                        try self.toValue(else_branch, gc)
                    else
                        Value.Void;
                },
                .Range => range: {
                    const rg_components = components[node].Range;

                    break :range (try gc.allocateObject(
                        obj.ObjRange{
                            .low = (try self.toValue(rg_components.low, gc)).integer(),
                            .high = (try self.toValue(rg_components.high, gc)).integer(),
                        },
                    )).toValue();
                },
                .List => list: {
                    const list_components = components[node].List;
                    const type_def = self.nodes.items(.type_def)[node];

                    std.debug.assert(type_def != null and type_def.?.def_type != .Placeholder);

                    var list = try gc.allocateObject(
                        try obj.ObjList.init(gc.allocator, type_def.?),
                    );

                    for (list_components.items) |item| {
                        try list.items.append(
                            gc.allocator,
                            try self.toValue(item, gc),
                        );
                    }

                    break :list list.toValue();
                },
                .Map => map: {
                    const map_components = components[node].Map;
                    const type_def = self.nodes.items(.type_def)[node];

                    std.debug.assert(type_def != null and type_def.?.def_type != .Placeholder);

                    var map = try gc.allocateObject(
                        try obj.ObjMap.init(gc.allocator, type_def.?),
                    );

                    for (map_components.entries) |entry| {
                        try map.map.put(
                            gc.allocator,
                            try self.toValue(entry.key, gc),
                            try self.toValue(entry.value, gc),
                        );
                    }

                    break :map map.toValue();
                },
                .String => string: {
                    const elements = components[node].String;

                    var string = std.Io.Writer.Allocating.init(gc.allocator);
                    defer string.deinit();

                    for (elements) |element| {
                        try (try self.toValue(element, gc)).toString(&string.writer);
                    }

                    break :string (try gc.copyString(string.written())).toValue();
                },
                .Subscript => subscript: {
                    const subscript_components = components[node].Subscript;

                    const subscriptable = (try self.toValue(subscript_components.subscripted, gc)).obj();
                    const key = try self.toValue(subscript_components.index, gc);

                    switch (subscriptable.obj_type) {
                        .List => {
                            const list = obj.ObjList.cast(subscriptable).?;
                            const index: usize = @intCast(key.integer());

                            if (index < 0 or index >= list.items.items.len) {
                                if (subscript_components.checked) {
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
                                if (subscript_components.checked) {
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

                    break :subscript components[node].Subscript.toValue();
                },
                .Unary => unary: {
                    const unary_components = components[node].Unary;
                    const val = try self.toValue(unary_components.expression, gc);

                    break :unary switch (unary_components.operator) {
                        .Bnot => Value.fromInteger(~val.integer()),
                        .Bang => Value.fromBoolean(!val.boolean()),
                        .Minus => if (val.isInteger())
                            Value.fromInteger(-%val.integer())
                        else
                            Value.fromDouble(-val.double()),
                        else => unreachable,
                    };
                },
                .Unwrap => try self.toValue(components[node].Unwrap.unwrapped, gc),
                else => null,
            };
        }

        return value.* orelse Value.Void;
    }

    /// Context used to interpret terminal statements while analyzing a node.
    pub const TerminalFlowContext = struct {
        /// Loop whose reachable breaks and continues should be reported.
        target_loop: ?Node.Index = null,
        /// Nearest loop that receives unlabeled break and continue statements.
        unlabeled_loop: ?Node.Index = null,
        /// Whether an `out` statement exits the currently analyzed block expression.
        out_exits: bool = false,

        /// Returns whether a break or continue destination reaches `target_loop`.
        fn targetsLoop(self: TerminalFlowContext, destination: ?Node.Index) bool {
            const target_loop = self.target_loop orelse return false;

            if (destination) |dest| {
                return dest == target_loop;
            }

            return self.unlabeled_loop != null and self.unlabeled_loop.? == target_loop;
        }
    };

    /// Conservative summary of whether and how execution can leave an AST node.
    pub const TerminalFlow = struct {
        /// At least one reachable path continues after the analyzed node.
        falls_through: bool = true,
        /// At least one reachable path exits through a return statement.
        returns: bool = false,
        /// At least one reachable path exits through a throw statement.
        throws: bool = false,
        /// At least one reachable path exits the active block expression with `out`.
        outs: bool = false,
        /// At least one reachable path breaks to the active analysis target.
        breaks: bool = false,
        /// At least one reachable path continues to the active analysis target.
        continues: bool = false,

        /// Returns whether the analyzed node cannot continue to the following statement.
        pub fn terminal(self: TerminalFlow) bool {
            return !self.falls_through;
        }

        /// Merges terminal outcomes from another summary without changing fallthrough.
        pub fn merge(self: *TerminalFlow, other: TerminalFlow) void {
            self.returns = self.returns or other.returns;
            self.throws = self.throws or other.throws;
            self.outs = self.outs or other.outs;
            self.breaks = self.breaks or other.breaks;
            self.continues = self.continues or other.continues;
        }

        /// Converts a block-expression-internal summary to the flow seen by its parent expression.
        pub fn asExpression(self: TerminalFlow) TerminalFlow {
            return .{
                .falls_through = self.falls_through or self.outs,
                .returns = self.returns,
                .throws = self.throws,
                .breaks = self.breaks,
                .continues = self.continues,
            };
        }
    };

    /// Terminal flow plus the merged type of reachable block-expression `out` values.
    pub const BlockExpressionFlow = struct {
        terminal: TerminalFlow = .{},
        out_type: ?*obj.ObjTypeDef = null,

        /// Merges an `out` expression type into this summary.
        fn mergeOutType(self: *BlockExpressionFlow, gc: *GC, type_def: ?*obj.ObjTypeDef) Error!void {
            const incoming = type_def orelse return;

            if (self.out_type) |current| {
                if (!current.eql(incoming)) {
                    self.out_type = gc.type_registry.any_type;
                }
            } else {
                self.out_type = incoming;
            }
        }

        /// Merges flow outcomes from another summary without changing fallthrough.
        fn merge(self: *BlockExpressionFlow, gc: *GC, other: BlockExpressionFlow) Error!void {
            self.terminal.merge(other.terminal);
            try self.mergeOutType(gc, other.out_type);
        }

        /// Propagates labeled loop jumps that escape through a nested loop to an outer target loop.
        fn mergeOuterLoopJumps(
            self: *BlockExpressionFlow,
            ast: Self.Slice,
            allocator: std.mem.Allocator,
            gc: *GC,
            body: Node.Index,
            loop_node: Node.Index,
            ctx: TerminalFlowContext,
        ) Error!void {
            if (ctx.target_loop == null or ctx.target_loop.? == loop_node) {
                return;
            }

            const outer_flow = try ast.blockExpressionFlow(
                allocator,
                gc,
                body,
                .{
                    .target_loop = ctx.target_loop,
                    .unlabeled_loop = loop_node,
                    .out_exits = ctx.out_exits,
                },
            );
            self.terminal.breaks = self.terminal.breaks or outer_flow.terminal.breaks;
            self.terminal.continues = self.terminal.continues or outer_flow.terminal.continues;
        }
    };

    /// Computes terminal flow and reachable `out` value type for a single AST node.
    fn blockExpressionFlow(
        self: Self.Slice,
        allocator: std.mem.Allocator,
        gc: *GC,
        node: Node.Index,
        ctx: TerminalFlowContext,
    ) Error!BlockExpressionFlow {
        const tags = self.nodes.items(.tag);
        const components = self.nodes.items(.components);

        return switch (tags[node]) {
            .Return => .{
                .terminal = .{
                    .falls_through = false,
                    .returns = true,
                },
            },
            .Throw => .{
                .terminal = .{
                    .falls_through = false,
                    .throws = true,
                },
            },
            .Out => if (ctx.out_exits) .{
                .terminal = .{
                    .falls_through = false,
                    .outs = true,
                },
                .out_type = self.nodes.items(.type_def)[node],
            } else .{},
            .Break => brk: {
                break :brk .{
                    .terminal = .{
                        .falls_through = false,
                        .breaks = ctx.targetsLoop(components[node].Break.destination),
                    },
                };
            },
            .Continue => .{
                .terminal = .{
                    .falls_through = false,
                    .continues = ctx.targetsLoop(components[node].Continue.destination),
                },
            },
            .Block => try self.blockExpressionFlowStatements(
                allocator,
                gc,
                components[node].Block,
                ctx,
            ),
            .BlockExpression => expr: {
                const inner = try self.blockExpressionFlowStatements(
                    allocator,
                    gc,
                    components[node].BlockExpression,
                    .{
                        .target_loop = ctx.target_loop,
                        .unlabeled_loop = ctx.unlabeled_loop,
                        .out_exits = true,
                    },
                );

                break :expr .{
                    .terminal = inner.terminal.asExpression(),
                };
            },
            .Expression => try self.blockExpressionFlow(
                allocator,
                gc,
                components[node].Expression,
                ctx,
            ),
            .If => try self.ifTerminalFlow(allocator, gc, node, ctx),
            .Match => try self.matchTerminalFlow(allocator, gc, node, ctx),
            .While => try self.whileTerminalFlow(allocator, gc, node, ctx),
            .For => try self.forTerminalFlow(allocator, gc, node, ctx),
            .DoUntil => try self.doUntilTerminalFlow(allocator, gc, node, ctx),
            .Try => .{},
            else => .{},
        };
    }

    /// Computes terminal flow and reachable `out` value type for an ordered statement list.
    pub fn blockExpressionFlowStatements(
        self: Self.Slice,
        allocator: std.mem.Allocator,
        gc: *GC,
        statements: []const Node.Index,
        ctx: TerminalFlowContext,
    ) Error!BlockExpressionFlow {
        var result = BlockExpressionFlow{};
        var falls_through = true;

        for (statements) |statement| {
            if (!falls_through) {
                break;
            }

            const statement_flow = try self.blockExpressionFlow(
                allocator,
                gc,
                statement,
                ctx,
            );
            try result.merge(gc, statement_flow);

            falls_through = statement_flow.terminal.falls_through;
        }

        result.terminal.falls_through = falls_through;

        return result;
    }

    /// Returns a boolean value when a node is a compile-time boolean constant.
    fn constantBoolean(
        self: Self.Slice,
        allocator: std.mem.Allocator,
        gc: *GC,
        node: Node.Index,
    ) Error!?bool {
        const type_def = self.nodes.items(.type_def)[node] orelse return null;
        if (type_def.optional or type_def.def_type != .Boolean) {
            return null;
        }

        if (!try self.isConstant(allocator, node)) {
            return null;
        }

        return (try self.toValue(node, gc)).boolean();
    }

    /// Computes flow for an if node, folding constant conditions when possible.
    fn ifTerminalFlow(
        self: Self.Slice,
        allocator: std.mem.Allocator,
        gc: *GC,
        node: Node.Index,
        ctx: TerminalFlowContext,
    ) Error!BlockExpressionFlow {
        const components = self.nodes.items(.components)[node].If;

        if (components.unwrapped_identifier == null and components.casted_type == null) {
            if (try self.constantBoolean(allocator, gc, components.condition)) |condition| {
                if (condition) {
                    return try self.blockExpressionFlow(allocator, gc, components.body, ctx);
                }

                if (components.else_branch) |else_branch| {
                    return try self.blockExpressionFlow(allocator, gc, else_branch, ctx);
                }

                return .{};
            }
        }

        var result = try self.blockExpressionFlow(allocator, gc, components.body, ctx);

        if (components.else_branch) |else_branch| {
            const else_flow = try self.blockExpressionFlow(allocator, gc, else_branch, ctx);
            result.terminal.falls_through = result.terminal.falls_through or else_flow.terminal.falls_through;
            try result.merge(gc, else_flow);
        } else {
            result.terminal.falls_through = true;
        }

        return result;
    }

    /// Computes flow for a match node when all explicit branches can be inspected.
    fn matchTerminalFlow(
        self: Self.Slice,
        allocator: std.mem.Allocator,
        gc: *GC,
        node: Node.Index,
        ctx: TerminalFlowContext,
    ) Error!BlockExpressionFlow {
        const components = self.nodes.items(.components)[node].Match;
        var result = BlockExpressionFlow{
            .terminal = .{
                .falls_through = components.else_branch == null,
            },
        };

        for (components.branches) |branch| {
            const branch_flow = try self.blockExpressionFlow(
                allocator,
                gc,
                branch.expression,
                ctx,
            );
            result.terminal.falls_through = result.terminal.falls_through or branch_flow.terminal.falls_through;
            try result.merge(gc, branch_flow);
        }

        if (components.else_branch) |else_branch| {
            const else_flow = try self.blockExpressionFlow(allocator, gc, else_branch, ctx);
            result.terminal.falls_through = result.terminal.falls_through or else_flow.terminal.falls_through;
            try result.merge(gc, else_flow);
        }

        return result;
    }

    /// Computes flow for a while loop, including proven infinite loops without reachable breaks.
    fn whileTerminalFlow(
        self: Self.Slice,
        allocator: std.mem.Allocator,
        gc: *GC,
        node: Node.Index,
        ctx: TerminalFlowContext,
    ) Error!BlockExpressionFlow {
        const components = self.nodes.items(.components)[node].While;

        if (try self.constantBoolean(allocator, gc, components.condition)) |condition| {
            if (!condition) {
                return .{};
            }
        } else {
            return .{};
        }

        const body_flow = try self.blockExpressionFlow(
            allocator,
            gc,
            components.body,
            .{
                .target_loop = node,
                .unlabeled_loop = node,
                .out_exits = ctx.out_exits,
            },
        );

        var result = BlockExpressionFlow{
            .terminal = .{
                .falls_through = body_flow.terminal.breaks,
                .returns = body_flow.terminal.returns,
                .throws = body_flow.terminal.throws,
                .outs = body_flow.terminal.outs,
            },
            .out_type = body_flow.out_type,
        };

        try result.mergeOuterLoopJumps(self, allocator, gc, components.body, node, ctx);

        return result;
    }

    /// Computes flow for a for loop, including constant-true loops without reachable breaks.
    fn forTerminalFlow(
        self: Self.Slice,
        allocator: std.mem.Allocator,
        gc: *GC,
        node: Node.Index,
        ctx: TerminalFlowContext,
    ) Error!BlockExpressionFlow {
        const components = self.nodes.items(.components)[node].For;

        if (try self.constantBoolean(allocator, gc, components.condition)) |condition| {
            if (!condition) {
                return .{};
            }
        } else {
            return .{};
        }

        const body_flow = try self.blockExpressionFlow(
            allocator,
            gc,
            components.body,
            .{
                .target_loop = node,
                .unlabeled_loop = node,
                .out_exits = ctx.out_exits,
            },
        );

        var result = BlockExpressionFlow{
            .terminal = .{
                .falls_through = body_flow.terminal.breaks,
                .returns = body_flow.terminal.returns,
                .throws = body_flow.terminal.throws,
                .outs = body_flow.terminal.outs,
            },
            .out_type = body_flow.out_type,
        };

        try result.mergeOuterLoopJumps(self, allocator, gc, components.body, node, ctx);

        return result;
    }

    /// Computes flow for a do-until loop, accounting for the body running before the condition.
    fn doUntilTerminalFlow(
        self: Self.Slice,
        allocator: std.mem.Allocator,
        gc: *GC,
        node: Node.Index,
        ctx: TerminalFlowContext,
    ) Error!BlockExpressionFlow {
        const components = self.nodes.items(.components)[node].DoUntil;
        const body_flow = try self.blockExpressionFlow(
            allocator,
            gc,
            components.body,
            .{
                .target_loop = node,
                .unlabeled_loop = node,
                .out_exits = ctx.out_exits,
            },
        );
        const condition = try self.constantBoolean(allocator, gc, components.condition);
        const condition_allows_exit = condition == null or condition.?;

        var result = BlockExpressionFlow{
            .terminal = .{
                .falls_through = body_flow.terminal.breaks or
                    (body_flow.terminal.falls_through and condition_allows_exit),
                .returns = body_flow.terminal.returns,
                .throws = body_flow.terminal.throws,
                .outs = body_flow.terminal.outs,
            },
            .out_type = body_flow.out_type,
        };

        try result.mergeOuterLoopJumps(self, allocator, gc, components.body, node, ctx);

        return result;
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

pub fn appendNode(self: *Self, node: Node) !Node.Index {
    try self.nodes.append(self.allocator, node);

    return @intCast(self.nodes.len - 1);
}

pub fn appendToken(self: *Self, token: Token) !TokenIndex {
    try self.tokens.append(self.allocator, token);

    return @intCast(self.tokens.len - 1);
}

pub fn swapNodes(self: *Self, from: Node.Index, to: Node.Index) void {
    const from_node = self.nodes.get(from);
    const to_node = self.nodes.get(to);

    self.nodes.set(from, to_node);
    self.nodes.set(to, from_node);
}

pub const Close = struct {
    opcode: Chunk.OpCode,
    slot: u8,
};

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
    ends_scope: ?[]const Close = null,
    /// Data related to this node
    components: Components,
    /// To avoid generating a node const value multiple times
    value: ?Value = null,

    // JIT related metdata

    /// How many time it was visited at runtime (used to decide wether its a hotspot that needs to be compiled)
    count: usize = 0,
    /// Complexity score computed once to help evaluate if the node is worth JIT compiling
    complexity_score: ?usize = null,
    /// Node status: blacklisted, queued/generated/compiled by the JIT, compilable
    jit_status: JitStatus = .compilable,
    /// Once compiled
    compiled: ?*anyopaque = null,

    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        if (self.ends_scope) |ends_scope| {
            allocator.free(ends_scope);
        }

        self.components.deinit(allocator);
    }

    pub const JitStatus = enum {
        /// Node can be jit compiled
        compilable,
        /// Node is already queued in the jit compiler
        queued,
        /// Node has generated native code waiting to be published by the VM thread
        generated,
        /// Node can't be compiled (contains use of fiber)
        blacklisted,
        /// Already compiled
        compiled,
    };

    pub const Index = u32;

    pub const Tag = enum(u8) {
        AnonymousObjectType,
        AnonymousEnumCase,
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
        Match,
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
        AnonymousEnumCase: AnonymousEnumCase,
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
        Match: Match,
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

pub const QualifiedName = struct {
    name: TokenIndex,
    namespace: []const TokenIndex = &.{},

    pub fn print(self: QualifiedName, ast: *Self) void {
        const lexemes = ast.tokens.items(.lexeme);

        for (self.namespace) |part| {
            std.debug.print("{s}\\", .{lexemes[part]});
        }

        std.debug.print("{s}", .{lexemes[self.name]});
    }

    pub fn firstToken(self: QualifiedName) TokenIndex {
        return if (self.namespace.len > 0) self.namespace[0] else self.name;
    }

    pub const Context = struct {
        ast: *const Self,

        pub fn hash(ctx: Context, key: QualifiedName) u64 {
            var h = std.hash.Wyhash.init(0);
            const lexemes = ctx.ast.tokens.items(.lexeme);

            for (key.namespace) |part| {
                h.update(lexemes[part]);
                h.update("\\");
            }

            h.update(lexemes[key.name]);

            return h.final();
        }

        pub fn eql(ctx: @This(), a: QualifiedName, b: QualifiedName) bool {
            const lexemes = ctx.ast.tokens.items(.lexeme);

            if (a.namespace.len != b.namespace.len) return false;
            for (a.namespace, b.namespace) |ap, bp| {
                if (!std.mem.eql(u8, lexemes[ap], lexemes[bp])) return false;
            }

            return std.mem.eql(u8, lexemes[a.name], lexemes[b.name]);
        }
    };

    pub fn HashMap(V: type) type {
        return std.HashMapUnmanaged(
            QualifiedName,
            V,
            Context,
            std.hash_map.default_max_load_percentage,
        );
    }
};

pub const AnonymousObjectType = struct {
    fields: []const Field,

    pub const Field = struct {
        name: TokenIndex,
        type: Node.Index,
    };
};

pub const AnonymousEnumCase = struct {
    case_name: TokenIndex,
    resolved_case: ?u32 = null,
};

pub const Binary = struct {
    left: Node.Index,
    right: Node.Index,
    operator: Token.Tag,
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
    qualified_name: ?QualifiedName,
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

pub const Match = struct {
    is_statement: bool,
    value: Node.Index,
    branches: []const Branch,
    else_branch: ?Node.Index,

    pub const Branch = struct {
        conditions: []const Node.Index,
        expression: Node.Index,
    };
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
    qualified_name: QualifiedName,
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
    assign_token: ?TokenIndex,
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
    operator: Token.Tag,
    expression: Node.Index,
};

pub const Unwrap = struct {
    unwrapped: Node.Index,
    original_type: *obj.ObjTypeDef,
    start_opt_jumps: bool,
};

pub const UserType = struct {
    qualified_name: QualifiedName,
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
    implicit: bool,
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
