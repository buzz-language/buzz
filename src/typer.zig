const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const nd = @import("./parser.zig");
const obj = @import("./obj.zig");

pub const Global = struct {
    prefix: ?[]const u8 = null,
    name: *ObjString,
    // null when globals are parsed out of order, if still null at end of parsing, it's a compile error
    type_def: ?*ObjTypeDef,
    initialized: bool = false,
    exported: bool = false,
    export_alias: ?[]const u8 = null,
    hidden: bool = false,
    // if global parsed out of order, we may don't know yet
    constant: ?bool,
    // When resolving a placeholder, the start of the resolution is the global
    // If `constant` is true, we can search for any `.Assignment` link and fail then.
};

pub const Frame = struct {
    enclosing: ?*Frame = null,
    locals: [255]nd.Local,
    local_count: u8 = 0,
    upvalues: [255]nd.UpValue,
    upvalue_count: u8 = 0,
    scope_depth: u32 = 0,
    // Set at true in a `else` statement of scope_depth 2
    return_counts: bool = false,
    // If false, `return` was omitted or within a conditionned block (if, loop, etc.)
    // We only count `return` emitted within the scope_depth 0 of the current function or unconditionned else statement
    return_emitted: bool = false,
    function_node: *nd.FunctionNode,
};

pub const Typer = struct {
    const Self = @This();

    allocator: Allocator,

    current_frame: ?*Frame = null,
    type_defs: std.StringHashMap(*obj.ObjTypeDef),
    // Interned strings, will be copied over to the vm
    strings: *std.StringHashMap(*obj.ObjString),
    globals: std.ArrayList(Global),

    pub fn init(allocator: Allocator, type_defs: std.StringHashMap(*obj.ObjTypeDef), strings: *std.StringHashMap(*obj.ObjString)) Self {
        return Self{
            .allocator = allocator,
            .type_defs = type_defs,
            .globals = std.ArrayList(Global).init(allocator),
            .strings = strings,
        };
    }

    pub fn getTypeDef(self: *Self, type_def: obj.ObjTypeDef) !*obj.ObjTypeDef {
        // Don't intern placeholders
        if (type_def.def_type == .Placeholder) {
            var type_def_ptr: *obj.ObjTypeDef = try self.allocator.create(obj.ObjTypeDef);
            type_def_ptr.* = type_def;
            return type_def_ptr;
        }

        var type_def_str: []const u8 = try type_def.toString(self.allocator);

        if (self.type_defs.get(type_def_str)) |type_def_ptr| {
            self.allocator.free(type_def_str); // If already in map, we don't need this string anymore
            return type_def_ptr;
        }

        var type_def_ptr: *obj.ObjTypeDef = try self.allocator.create(obj.ObjTypeDef);
        type_def_ptr.* = type_def;

        _ = try self.type_defs.put(type_def_str, type_def_ptr);

        return type_def_ptr;
    }

    pub inline fn getTypeDefByName(self: *Self, name: []const u8) ?*obj.ObjTypeDef {
        return self.type_defs.get(name);
    }

    pub fn visit(self: *Self, node: *nd.ParseNode) !void {
        switch (node.node_type) {
            .Function => self.visitFunction(node),
            .Enum => self.visitEnum(node),
            .VarDeclaration => self.visitVarDeclaration(node),
            .FunDeclaration => self.visitFunDeclaration(node),
            .ListDeclaration => self.visitListDeclaration(node),
            .MapDeclaration => self.visitMapDeclaration(node),
            .ObjectDeclaration => self.visitObjectDeclaration(node),
            .Binary => self.visitBinary(node),
            .Unary => self.visitUnary(node),
            .Subscript => self.visitSubscript(node),
            .Unwrap => self.visitUnwrap(node),
            .ForceUnwrap => self.visitForceUnwrap(node),
            .Is => self.visitIs(node),
            .And => self.visitAnd(node),
            .Or => self.visitOr(node),
            .NamedVariable => self.visitNamedVariable(node),
            .Number => self.visitNumber(node),
            .String => self.visitString(node),
            .StringConstant => self.visitStringConstant(node),
            .Boolean => self.visitBoolean(node),
            .Null => self.visitNull(node),
            .List => self.visitList(node),
            .Map => self.visitMap(node),
            .Super => self.visitSuper(node),
            .Dot => self.visitDot(node),
            .ObjectInit => self.visitObjectInit(node),
            .Throw => self.visitThrow(node),
            .Break => self.visitBreak(node),
            .Continue => self.visitContinue(node),
            .Call => self.visitCall(node),
            .SuperCall => self.visitSuperCall(node),
            .If => self.visitIf(node),
            .Block => self.visitBlock(node),
            .Return => self.visitReturn(node),
            .For => self.visitFor(node),
            .ForEach => self.visitForEach(node),
            .DoUntil => self.visitDoUntil(node),
            .While => self.visitWhile(node),
            .Export => self.visitExport(node),
            .Import => self.visitImport(node),
            .Catch => self.visitCatch(node),
        }
    }

    inline fn beginScope(self: *Self) void {
        self.current_frame.?.scope_depth += 1;
    }

    inline fn endScope(self: *Self) !void {
        self.current_frame.?.scope_depth -= 1;
    }

    inline fn markInitialized(self: *Self) void {
        if (self.current_frame.?.scope_depth == 0) {
            self.globals.items[self.globals.items.len - 1].initialized = true;
        } else {
            self.current_frame.?.locals[self.current_frame.?.local_count - 1].depth = @intCast(i32, self.current_frame.?.scope_depth);
        }
    }

    fn addLocal(self: *Self, name: Token, local_type: *ObjTypeDef, constant: bool) !usize {
        if (self.current_frame.?.local_count == 255) {
            try self.reportError("Too many local variables in scope.");
            return 0;
        }

        self.current_frame.?.locals[self.current_frame.?.local_count] = Local{
            .name = try copyStringRaw(self.strings, self.allocator, name.lexeme, false),
            .depth = -1,
            .is_captured = false,
            .type_def = local_type,
            .constant = constant,
        };

        self.current_frame.?.local_count += 1;

        return self.current_frame.?.local_count - 1;
    }

    fn addGlobal(self: *Self, name: Token, global_type: *ObjTypeDef, constant: bool) !usize {
        // Search for an existing placeholder global with the same name
        for (self.globals.items) |global, index| {
            if (global.type_def.def_type == .Placeholder and global.type_def.resolved_type.?.Placeholder.name != null and mem.eql(u8, name.lexeme, global.name.string)) {
                try self.resolvePlaceholder(global.type_def, global_type, constant);

                return index;
            }
        }

        if (self.globals.items.len == 255) {
            try self.reportError("Too many global variables.");
            return 0;
        }

        try self.globals.append(
            Global{
                .name = try copyStringRaw(self.strings, self.allocator, name.lexeme, false),
                .type_def = global_type,
                .constant = constant,
            },
        );

        return self.globals.items.len - 1;
    }

    fn resolveLocal(self: *Self, name: Token) !?usize {
        if (self.local_count == 0) {
            return null;
        }

        var i: usize = self.local_count - 1;
        while (i >= 0) : (i -= 1) {
            var local: *Local = &self.locals[i];
            if (mem.eql(u8, name.lexeme, local.name.string)) {
                if (local.depth == -1) {
                    try self.reportError("Can't read local variable in its own initializer.");
                }

                return i;
            }

            if (i == 0) break;
        }

        return null;
    }

    // Will consume tokens if find a prefixed identifier
    fn resolveGlobal(self: *Self, prefix: ?[]const u8, name: Token) anyerror!?usize {
        if (self.globals.items.len == 0) {
            return null;
        }

        var i: usize = self.globals.items.len - 1;
        while (i >= 0) : (i -= 1) {
            var global: *Global = &self.globals.items[i];
            if (((prefix == null and global.prefix == null) or (prefix != null and global.prefix != null and mem.eql(u8, prefix.?, global.prefix.?))) and mem.eql(u8, name.lexeme, global.name.string) and !global.hidden) {
                if (!global.initialized) {
                    try self.reportError("Can't read global variable in its own initializer.");
                }

                return i;
                // Is it an import prefix?
            } else if (global.prefix != null and mem.eql(u8, name.lexeme, global.prefix.?)) {
                // try self.consume(.Dot, "Expected `.` after import prefix.");
                // try self.consume(.Identifier, "Expected identifier after import prefix.");
                // return try self.resolveGlobal(global.prefix.?, self.parser.previous_token.?);
                // FIXME
                unreachable;
            }

            if (i == 0) break;
        }

        return null;
    }

    fn addUpvalue(self: *Self, index: usize, is_local: bool) !usize {
        var i: usize = 0;
        while (i < self.upvalue_count) : (i += 1) {
            var upvalue: *UpValue = &compiler.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return i;
            }
        }

        if (self.upvalue_count == 255) {
            try self.reportError("Too many closure variables in function.");
            return 0;
        }

        compiler.upvalues[self.upvalue_count].is_local = is_local;
        compiler.upvalues[self.upvalue_count].index = @intCast(u8, index);
        self.upvalue_count += 1;

        return self.upvalue_count - 1;
    }

    fn resolveUpvalue(self: *Self, name: Token) anyerror!?usize {
        if (compiler.enclosing == null) {
            return null;
        }

        var local: ?usize = try self.enclosing.?.resolveLocal(name);
        if (local) |resolved| {
            compiler.enclosing.?.locals[resolved].is_captured = true;
            return try self.addUpvalue(compiler, resolved, true);
        }

        var upvalue: ?usize = try self.resolveUpvalue(compiler.enclosing.?, name);
        if (upvalue) |resolved| {
            return try self.addUpvalue(compiler, resolved, false);
        }

        return null;
    }

    fn declareVariable(self: *Self, variable_type: *ObjTypeDef, name_token: Token, constant: bool) !usize {
        var name: Token = name_token;

        if (self.current_frame.?.scope_depth > 0) {
            // Check a local with the same name doesn't exists
            var i: usize = self.current_frame.?.locals.len - 1;
            while (i >= 0) : (i -= 1) {
                var local: *Local = &self.current_frame.?.locals[i];

                if (local.depth != -1 and local.depth < self.current_frame.?.scope_depth) {
                    break;
                }

                if (mem.eql(u8, name.lexeme, local.name.string)) {
                    try self.reportError("A variable with the same name already exists in this scope.");
                }

                if (i == 0) break;
            }

            return try self.addLocal(name, variable_type, constant);
        } else {
            // Check a global with the same name doesn't exists
            for (self.globals.items) |global, index| {
                if (mem.eql(u8, name.lexeme, global.name.string) and !global.hidden) {
                    // If we found a placeholder with that name, try to resolve it with `variable_type`
                    if (global.type_def.def_type == .Placeholder and global.type_def.resolved_type.?.Placeholder.name != null and mem.eql(u8, name.lexeme, global.type_def.resolved_type.?.Placeholder.name.?.string)) {

                        // A function declares a global with an incomplete typedef so that it can handle recursion
                        // The placeholder resolution occurs after we parsed the functions body in `funDeclaration`
                        if (variable_type.resolved_type != null or @enumToInt(variable_type.def_type) < @enumToInt(ObjTypeDef.Type.ObjectInstance)) {
                            try self.resolvePlaceholder(global.type_def, variable_type, constant);
                        }

                        return index;
                    } else {
                        try self.reportError("A global with the same name already exists.");
                    }
                }
            }

            return try self.addGlobal(name, variable_type, constant);
        }
    }

    fn visitFunction(self: *Self, node: *nd.ParseNode) !void {
        var function_node = nd.FunctionNode.cast(node).?;
        var function_def = function_node.node.type_def.?.resolved_type.?.Function;

        var frame = try self.allocator.create(Frame);
        frame.* = Frame{
            .locals = [_]Local{undefined} ** 255,
            .upvalues = [_]UpValue{undefined} ** 255,
            .enclosing = compiler.current,
            .function_node = function_node,
        };

        self.current_frame = frame;

        self.beginScope();

        // First local is reserved for an eventual `this` or cli arguments
        var local: *Local = &self.current_frame.?.locals[self.local_count];
        self.current_frame.?.local_count += 1;
        local.depth = 0;
        local.is_captured = false;

        switch (function_node.node.type_def.?.resolved_type.?.Function.function_type) {
            .Method => {
                // `this`
                var type_def: obj.ObjTypeDef.TypeUnion = obj.ObjTypeDef.TypeUnion{
                    .ObjectInstance = function_node.this.?,
                };

                local.type_def = try compiler.getTypeDef(
                    obj.ObjTypeDef{
                        .def_type = .ObjectInstance,
                        .resolved_type = type_def,
                    },
                );
            },
            .EntryPoint, .ScriptEntryPoint => {
                // `args` is [str]
                var list_def: ObjList.ListDef = ObjList.ListDef.init(
                    compiler.allocator,
                    try compiler.getTypeDef(.{ .def_type = .String }),
                );

                var list_union: obj.ObjTypeDef.TypeUnion = .{ .List = list_def };

                local.type_def = try compiler.getTypeDef(obj.ObjTypeDef{
                    .def_type = .List,
                    .resolved_type = list_union,
                });
            },
            else => {
                // TODO: do we actually need to reserve that space since we statically know if we need it?
                // nothing
                local.type_def = try compiler.getTypeDef(obj.ObjTypeDef{
                    .def_type = .Void,
                });
            },
        }

        const name: []const u8 = switch (function_type) {
            .Method => "this",
            .EntryPoint => "$args",
            .ScriptEntryPoint => "args",
            else => "_",
        };

        local.name = try copyStringRaw(compiler.strings, compiler.allocator, name, false);

        // Function arguments are locals
        var it = function_def.parameters.iterator();
        while (it.next()) |entry| {}
    }

    fn endFrame(self: *Self, frame: Frame) !void {
        self.current_frame = if (self.current_frame) |current_frame| current_frame.enclosing else null;
    }

    fn visitEnum(self: *Self, node: *nd.ParseNode) !void {}

    fn visitVarDeclaration(self: *Self, node: *nd.ParseNode) !void {}

    fn visitFunDeclaration(self: *Self, node: *nd.ParseNode) !void {}

    fn visitListDeclaration(self: *Self, node: *nd.ParseNode) !void {}

    fn visitMapDeclaration(self: *Self, node: *nd.ParseNode) !void {}

    fn visitObjectDeclaration(self: *Self, node: *nd.ParseNode) !void {}

    fn visitBinary(self: *Self, node: *nd.ParseNode) !void {}

    fn visitUnary(self: *Self, node: *nd.ParseNode) !void {}

    fn visitSubscript(self: *Self, node: *nd.ParseNode) !void {}

    fn visitUnwrap(self: *Self, node: *nd.ParseNode) !void {}

    fn visitForceUnwrap(self: *Self, node: *nd.ParseNode) !void {}

    fn visitIs(self: *Self, node: *nd.ParseNode) !void {}

    fn visitAnd(self: *Self, node: *nd.ParseNode) !void {}

    fn visitOr(self: *Self, node: *nd.ParseNode) !void {}

    fn visitNamedVariable(self: *Self, node: *nd.ParseNode) !void {}

    fn visitNumber(self: *Self, node: *nd.ParseNode) !void {}

    fn visitString(self: *Self, node: *nd.ParseNode) !void {}

    fn visitStringConstant(self: *Self, node: *nd.ParseNode) !void {}

    fn visitBoolean(self: *Self, node: *nd.ParseNode) !void {}

    fn visitNull(self: *Self, node: *nd.ParseNode) !void {}

    fn visitList(self: *Self, node: *nd.ParseNode) !void {}

    fn visitMap(self: *Self, node: *nd.ParseNode) !void {}

    fn visitSuper(self: *Self, node: *nd.ParseNode) !void {}

    fn visitDot(self: *Self, node: *nd.ParseNode) !void {}

    fn visitObjectInit(self: *Self, node: *nd.ParseNode) !void {}

    fn visitThrow(self: *Self, node: *nd.ParseNode) !void {}

    fn visitBreak(self: *Self, node: *nd.ParseNode) !void {}

    fn visitContinue(self: *Self, node: *nd.ParseNode) !void {}

    fn visitCall(self: *Self, node: *nd.ParseNode) !void {}

    fn visitSuperCall(self: *Self, node: *nd.ParseNode) !void {}

    fn visitIf(self: *Self, node: *nd.ParseNode) !void {}

    fn visitBlock(self: *Self, node: *nd.ParseNode) !void {}

    fn visitReturn(self: *Self, node: *nd.ParseNode) !void {}

    fn visitFor(self: *Self, node: *nd.ParseNode) !void {}

    fn visitForEach(self: *Self, node: *nd.ParseNode) !void {}

    fn visitDoUntil(self: *Self, node: *nd.ParseNode) !void {}

    fn visitWhile(self: *Self, node: *nd.ParseNode) !void {}

    fn visitExport(self: *Self, node: *nd.ParseNode) !void {}

    fn visitImport(self: *Self, node: *nd.ParseNode) !void {}

    fn visitCatch(self: *Self, node: *nd.ParseNode) !void {}
};
