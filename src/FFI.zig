const std = @import("std");
const Ast = std.zig.Ast;
const aro = @import("aro");

const BuzzAst = @import("Ast.zig");
const o = @import("obj.zig");
const v = @import("value.zig");
const Parser = @import("Parser.zig");
const ZigType = @import("zigtypes.zig").Type;
const Reporter = @import("Reporter.zig");
const GC = @import("GC.zig");
const Init = @import("vm.zig").Init;
const CTree = aro.Tree;
const CQualType = aro.QualType;
const CType = aro.Type;

const Self = @This();

pub const Error = error{
    CantCompile,
    NoSpaceLeft,
    OutOfMemory,
    WriteFailed,
};

const basic_types = std.StaticStringMap(o.ObjTypeDef).initComptime(
    .{
        .{ "u8", o.ObjTypeDef{ .def_type = .Integer } },
        .{ "i8", o.ObjTypeDef{ .def_type = .Integer } },
        .{ "u16", o.ObjTypeDef{ .def_type = .Integer } },
        .{ "i16", o.ObjTypeDef{ .def_type = .Integer } },
        .{ "i32", o.ObjTypeDef{ .def_type = .Integer } },

        // Could it be > 32bits one some systems?
        .{ "c_int", o.ObjTypeDef{ .def_type = .Integer } },

        .{ "c_uint", o.ObjTypeDef{ .def_type = .Double } },
        .{ "u32", o.ObjTypeDef{ .def_type = .Double } },
        .{ "i64", o.ObjTypeDef{ .def_type = .Double } },
        .{ "f32", o.ObjTypeDef{ .def_type = .Double } },
        .{ "f64", o.ObjTypeDef{ .def_type = .Double } },

        .{ "u64", o.ObjTypeDef{ .def_type = .UserData } },
        .{ "usize", o.ObjTypeDef{ .def_type = .UserData } },

        .{ "bool", o.ObjTypeDef{ .def_type = .Boolean } },

        .{ "void", o.ObjTypeDef{ .def_type = .Void } },
        .{ "anyopaque", o.ObjTypeDef{ .def_type = .Void } },
    },
);

const zig_basic_types = std.StaticStringMap(ZigType).initComptime(
    .{
        .{
            "anyopaque",
            ZigType{
                .Opaque = .{
                    .decls = &[_]ZigType.Declaration{},
                },
            },
        },
        .{
            "c_int",
            ZigType{
                .Int = .{
                    .signedness = .signed,
                    .bits = 32,
                },
            },
        },
        .{
            "c_uint",
            ZigType{
                .Int = .{
                    .signedness = .unsigned,
                    .bits = 32,
                },
            },
        },
        .{
            "u8",
            ZigType{
                .Int = .{
                    .signedness = .unsigned,
                    .bits = 8,
                },
            },
        },
        .{
            "i8",
            ZigType{
                .Int = .{
                    .signedness = .signed,
                    .bits = 8,
                },
            },
        },
        .{
            "u16",
            ZigType{
                .Int = .{
                    .signedness = .unsigned,
                    .bits = 16,
                },
            },
        },
        .{
            "i16",
            ZigType{
                .Int = .{
                    .signedness = .signed,
                    .bits = 16,
                },
            },
        },
        .{
            "u32",
            ZigType{
                .Int = .{
                    .signedness = .unsigned,
                    .bits = 32,
                },
            },
        },
        .{
            "i32",
            ZigType{
                .Int = .{
                    .signedness = .signed,
                    .bits = 32,
                },
            },
        },
        .{
            "u64",
            ZigType{
                .Int = .{
                    .signedness = .unsigned,
                    .bits = 64,
                },
            },
        },
        .{
            "i64",
            ZigType{
                .Int = .{
                    .signedness = .signed,
                    .bits = 64,
                },
            },
        },
        .{
            "usize",
            ZigType{
                .Int = .{
                    .signedness = .signed,
                    .bits = @bitSizeOf(usize),
                },
            },
        },

        .{
            "f32",
            ZigType{
                .Double = .{ .bits = 32 },
            },
        },
        .{
            "f64",
            ZigType{
                .Double = .{ .bits = 64 },
            },
        },

        .{
            "bool",
            ZigType{ .Bool = {} },
        },
        .{
            "void",
            ZigType{ .Void = {} },
        },
    },
);

pub const ForeignDef = struct {
    name: []const u8,
    type_def: *o.ObjTypeDef,
    zig_type: ZigType,
};

pub const State = struct {
    script: []const u8,
    source: Ast.TokenIndex,
    ast: Ast,
    buzz_ast: ?BuzzAst.Slice = null,
    parser: ?*Parser,
    type_expr: ?[]const u8 = null,
    structs: std.StringHashMapUnmanaged(*ForeignDef) = .empty,
};

const CContext = struct {
    script: []const u8,
    source: Ast.TokenIndex,
    buzz_ast: BuzzAst.Slice,
    parser: *Parser,
    user_source_id: aro.Source.Id,
    comp: *aro.Compilation,
    tree: *const CTree,
    structs: std.StringHashMapUnmanaged(*ForeignDef) = .empty,

    fn deinit(self: *CContext, allocator: std.mem.Allocator) void {
        self.structs.deinit(allocator);
    }
};

const CRecordKind = enum {
    @"struct",
    @"union",
};

gc: *GC,
reporter: Reporter,
state: ?State = null,
type_expr_cache: std.StringHashMapUnmanaged(?*ForeignDef) = .empty,

pub fn init(gc: *GC, process: Init) Self {
    return .{
        .gc = gc,
        .reporter = .{
            .process = process,
            .allocator = gc.allocator,
            .error_prefix = "FFI",
        },
    };
}

pub fn deinit(self: *Self) void {
    self.type_expr_cache.deinit(self.gc.allocator);
}

pub fn parseTypeExpr(self: *Self, ztype: []const u8) !?*ForeignDef {
    if (self.type_expr_cache.get(ztype)) |foreign_def| {
        return foreign_def;
    }

    var full = std.Io.Writer.Allocating.init(self.gc.allocator);
    defer full.deinit();

    full.writer.print("const zig_type: {s};", .{ztype}) catch @panic("Out of memory");

    const foreign_defs = try self.parse(
        null,
        0,
        full.written(),
    );

    std.debug.assert(foreign_defs == null or foreign_defs.?.len == 1);

    self.type_expr_cache.put(
        self.gc.allocator,
        ztype,
        if (foreign_defs) |defs| defs[0] else null,
    ) catch @panic("Out of memory");

    return if (foreign_defs) |defs| defs[0] else null;
}

pub fn parse(self: *Self, parser: ?*Parser, source: Ast.TokenIndex, type_expr: ?[]const u8) !?[]*ForeignDef {
    // TODO: maybe an Arena allocator for those kinds of things that can live for the whole process lifetime
    const duped = self.gc.allocator.dupeZ(
        u8,
        type_expr orelse parser.?.ast.tokens.items(.literal)[source].String,
    ) catch @panic("Out of memory");
    // defer self.gc.allocator.free(duped);

    self.state = .{
        .script = if (parser) |uparser|
            try std.mem.replaceOwned(u8, self.gc.allocator, uparser.script_name, "/", ".")
        else
            "zdef",
        .type_expr = type_expr,
        .source = source,
        .parser = parser,
        .buzz_ast = if (parser) |p|
            p.ast.slice()
        else
            null,
        .ast = Ast.parse(
            self.gc.allocator,
            duped,
            .zig,
        ) catch @panic("Could not parse zdef"),
    };
    defer {
        self.state.?.structs.deinit(self.gc.allocator);
        self.state = null;
    }

    for (self.state.?.ast.errors) |err| {
        if (!err.is_note) {
            self.reportZigError(err);
        }
    }

    if (self.state.?.ast.errors.len > 0) {
        return null;
    }

    const root_decls = self.state.?.ast.rootDecls();

    if (root_decls.len == 0) {
        if (self.state.?.buzz_ast == null) {
            return null;
        }

        const location = self.state.?.buzz_ast.?.tokens.get(self.state.?.source - 4);

        self.reporter.report(
            .zdef,
            location,
            location,
            "At least one declaration is required in zdef",
        );

        return null;
    }

    var foreign_defs = std.ArrayList(*ForeignDef).empty;

    for (root_decls) |decl| {
        if (try self.getZdef(decl)) |foreign_def| {
            try foreign_defs.append(self.gc.allocator, foreign_def);
        }
    }

    if (self.reporter.last_error != null) {
        foreign_defs.deinit(self.gc.allocator);
        return Error.CantCompile;
    }

    return try foreign_defs.toOwnedSlice(self.gc.allocator);
}

pub fn parseC(self: *Self, parser: *Parser, source: Ast.TokenIndex) !?[]*ForeignDef {
    const diagnostics_arena = std.heap.ArenaAllocator.init(self.gc.allocator);
    var diagnostics: aro.Diagnostics = .{
        .output = .{ .to_list = .{ .arena = diagnostics_arena } },
        .details = false,
        .color = false,
    };
    defer diagnostics.deinit();

    var comp = try aro.Compilation.init(.{
        .gpa = self.gc.allocator,
        .arena = self.gc.allocator,
        .io = self.reporter.process.io,
        .diagnostics = &diagnostics,
        .environ_map = parser.process.environ_map,
    });
    defer comp.deinit();

    const user_source = try comp.addSourceFromBuffer(
        "cdef",
        parser.ast.tokens.items(.literal)[source].String,
    );
    const builtin = try comp.generateBuiltinMacros(.no_system_defines);

    var pp = aro.Preprocessor.init(
        &comp,
        .{
            .base_file = user_source.id,
            .add_builtin_macros = true,
        },
    ) catch |err| switch (err) {
        error.FatalError => {
            self.reportCDiagnostics(&diagnostics, parser.ast.slice(), source);
            return null;
        },
        else => return err,
    };
    defer pp.deinit();

    _ = pp.preprocess(builtin) catch |err| switch (err) {
        error.FatalError => {
            self.reportCDiagnostics(&diagnostics, parser.ast.slice(), source);
            return null;
        },
        else => return err,
    };

    const eof = pp.preprocess(user_source) catch |err| switch (err) {
        error.FatalError => {
            self.reportCDiagnostics(&diagnostics, parser.ast.slice(), source);
            return null;
        },
        else => return err,
    };
    try pp.addToken(eof);

    var tree = aro.Parser.parse(&pp) catch |err| switch (err) {
        error.FatalError => {
            self.reportCDiagnostics(&diagnostics, parser.ast.slice(), source);
            return null;
        },
        else => return err,
    };
    defer tree.deinit();

    if (diagnostics.errors > 0) {
        self.reportCDiagnostics(&diagnostics, parser.ast.slice(), source);
        return null;
    }

    if (tree.root_decls.items.len == 0) {
        const location = parser.ast.slice().tokens.get(source - 4);
        self.reporter.report(
            .cdef,
            location,
            location,
            "At least one declaration is required in cdef",
        );
        return null;
    }

    const script = try std.mem.replaceOwned(
        u8,
        self.gc.allocator,
        parser.script_name,
        "/",
        ".",
    );
    defer self.gc.allocator.free(script);

    var ctx = CContext{
        .script = script,
        .source = source,
        .buzz_ast = parser.ast.slice(),
        .parser = parser,
        .user_source_id = user_source.id,
        .comp = &comp,
        .tree = &tree,
    };
    defer ctx.deinit(self.gc.allocator);

    var foreign_defs = std.ArrayList(*ForeignDef).empty;
    var seen = std.StringHashMapUnmanaged(usize).empty;
    defer seen.deinit(self.gc.allocator);

    for (tree.root_decls.items) |decl| {
        if (!isUserCDecl(&ctx, decl)) continue;

        if (try self.getCForeignDef(&ctx, decl)) |foreign_def| {
            if (seen.get(foreign_def.name)) |idx| {
                if (shouldReplaceForeignDef(foreign_defs.items[idx], foreign_def)) {
                    foreign_defs.items[idx] = foreign_def;
                }
            } else {
                try foreign_defs.append(self.gc.allocator, foreign_def);
                try seen.put(
                    self.gc.allocator,
                    foreign_def.name,
                    foreign_defs.items.len - 1,
                );
            }
        }
    }

    if (self.reporter.last_error != null) {
        foreign_defs.deinit(self.gc.allocator);
        return Error.CantCompile;
    }

    return try foreign_defs.toOwnedSlice(self.gc.allocator);
}

fn isUserCDecl(ctx: *const CContext, decl_index: CTree.Node.Index) bool {
    return decl_index.loc(ctx.tree).id == ctx.user_source_id;
}

fn reportCDiagnostics(
    self: *Self,
    diagnostics: *const aro.Diagnostics,
    buzz_ast: BuzzAst.Slice,
    source: Ast.TokenIndex,
) void {
    const location = buzz_ast.tokens.get(source - 4);

    switch (diagnostics.output) {
        .to_list => |list| {
            if (list.messages.items.len == 0) {
                self.reporter.report(
                    .cdef,
                    location,
                    location,
                    "cdef could not be parsed",
                );
                return;
            }

            for (list.messages.items) |message| {
                if (message.effective_kind == .note) continue;

                var full = std.Io.Writer.Allocating.init(self.gc.allocator);
                defer full.deinit();

                if (message.location) |expanded| {
                    full.writer.print(
                        "{s}:{d}:{d}: {s}: {s}",
                        .{
                            expanded.path,
                            expanded.line_no,
                            expanded.col,
                            @tagName(message.effective_kind),
                            message.text,
                        },
                    ) catch unreachable;
                } else {
                    full.writer.print(
                        "{s}: {s}",
                        .{
                            @tagName(message.effective_kind),
                            message.text,
                        },
                    ) catch unreachable;
                }

                self.reporter.report(
                    .cdef,
                    location,
                    location,
                    full.written(),
                );
            }
        },
        else => {},
    }
}

fn reportCdef(self: *Self, ctx: *const CContext, message: []const u8) void {
    const location = ctx.buzz_ast.tokens.get(ctx.source - 4);
    self.reporter.report(.cdef, location, location, message);
}

fn reportCdefFmt(
    self: *Self,
    ctx: *const CContext,
    comptime fmt: []const u8,
    args: anytype,
) void {
    const location = ctx.buzz_ast.tokens.get(ctx.source - 4);
    self.reporter.reportErrorFmt(
        .cdef,
        location,
        location,
        fmt,
        args,
    );
}

fn foreignFieldCount(foreign_def: *const ForeignDef) usize {
    if (foreign_def.type_def.def_type != .ForeignContainer) {
        return 0;
    }

    return switch (foreign_def.zig_type) {
        .Struct => foreign_def.zig_type.Struct.fields.len,
        .Union => foreign_def.zig_type.Union.fields.len,
        else => 0,
    };
}

fn shouldReplaceForeignDef(existing: *const ForeignDef, candidate: *const ForeignDef) bool {
    return foreignFieldCount(existing) == 0 and foreignFieldCount(candidate) > 0;
}

fn allocateNamedForeignDef(
    self: *Self,
    name: []const u8,
    type_def: *o.ObjTypeDef,
    zig_type: ZigType,
) Error!*ForeignDef {
    const foreign_def = try self.gc.allocator.create(ForeignDef);
    foreign_def.* = .{
        .name = try self.gc.allocator.dupe(u8, name),
        .type_def = type_def,
        .zig_type = zig_type,
    };
    return foreign_def;
}

fn cloneNamedForeignDef(self: *Self, foreign_def: *const ForeignDef, name: []const u8) Error!*ForeignDef {
    return try self.allocateNamedForeignDef(name, foreign_def.type_def, foreign_def.zig_type);
}

fn cResolveNamedType(self: *Self, ctx: *CContext, name: []const u8) Error!?*ForeignDef {
    if (ctx.structs.get(name)) |foreign_def| {
        return foreign_def;
    }

    for (ctx.parser.globals.items) |global| {
        if (global.type_def.def_type != .ForeignContainer) continue;

        if (std.mem.eql(
            u8,
            name,
            ctx.parser.ast.tokens.items(.lexeme)[global.qualified_name.name],
        )) {
            const foreign_def = try self.allocateNamedForeignDef(
                name,
                global.type_def,
                global.type_def.resolved_type.?.ForeignContainer.zig_type,
            );
            try ctx.structs.put(self.gc.allocator, foreign_def.name, foreign_def);
            return foreign_def;
        }
    }

    return null;
}

fn getCForeignDef(self: *Self, ctx: *CContext, decl_index: CTree.Node.Index) Error!?*ForeignDef {
    return switch (decl_index.get(ctx.tree)) {
        .empty_decl => null,
        .function => |function| try self.cFunction(ctx, function),
        .struct_decl => |decl| try self.cRecordTypeToForeignDef(
            ctx,
            .@"struct",
            decl.container_qt.getRecord(ctx.comp).?,
            null,
        ),
        .union_decl => |decl| try self.cRecordTypeToForeignDef(
            ctx,
            .@"union",
            decl.container_qt.getRecord(ctx.comp).?,
            null,
        ),
        .struct_forward_decl => |decl| try self.cRecordTypeToForeignDef(
            ctx,
            .@"struct",
            decl.container_qt.getRecord(ctx.comp).?,
            null,
        ),
        .union_forward_decl => |decl| try self.cRecordTypeToForeignDef(
            ctx,
            .@"union",
            decl.container_qt.getRecord(ctx.comp).?,
            null,
        ),
        .typedef => |typedef| try self.cTypedef(ctx, typedef),
        .enum_decl,
        .enum_forward_decl,
        .variable,
        .static_assert,
        .global_asm,
        => unsupported: {
            self.reportCdef(
                ctx,
                "Unsupported C declaration: only function declarations, structs, unions, and typedefs to supported foreign containers are supported",
            );
            break :unsupported null;
        },
        else => unsupported: {
            self.reportCdef(
                ctx,
                "Unsupported C declaration: only function declarations, structs, unions, and typedefs to supported foreign containers are supported",
            );
            break :unsupported null;
        },
    };
}

fn cTypedef(self: *Self, ctx: *CContext, typedef: CTree.Node.Typedef) Error!?*ForeignDef {
    const name = ctx.tree.tokSlice(typedef.name_tok);
    const foreign_def = (try self.cTypeToForeignDef(ctx, typedef.qt, name)) orelse return null;

    if (foreign_def.type_def.def_type != .ForeignContainer) {
        self.reportCdef(
            ctx,
            "Only typedefs to supported foreign containers are exposed by cdef",
        );
        return null;
    }

    try ctx.structs.put(self.gc.allocator, foreign_def.name, foreign_def);
    return foreign_def;
}

fn cFunction(self: *Self, ctx: *CContext, function: CTree.Node.Function) Error!?*ForeignDef {
    if (function.body != null or function.static or function.@"inline") {
        self.reportCdef(
            ctx,
            "Only external C function declarations are supported in cdef",
        );
        return null;
    }

    const fn_type = function.qt.get(ctx.comp, .func) orelse {
        self.reportCdef(ctx, "Unsupported C function declaration");
        return null;
    };

    if (fn_type.kind != .normal) {
        self.reportCdef(
            ctx,
            "Variadic and old-style C function declarations are not supported in cdef",
        );
        return null;
    }

    const return_type_foreign_def = (try self.cTypeToForeignDef(ctx, fn_type.return_type, null)) orelse return null;
    if (return_type_foreign_def.zig_type == .Struct or return_type_foreign_def.zig_type == .Union) {
        self.reportCdef(
            ctx,
            "Passing C structs or unions by value is not supported in cdef",
        );
        return null;
    }

    var function_def = o.ObjFunction.FunctionDef{
        .id = o.ObjFunction.FunctionDef.nextId(),
        .name = try self.gc.copyString(ctx.tree.tokSlice(function.name_tok)),
        .script_name = try self.gc.copyString(
            ctx.buzz_ast.tokens.items(.script_name)[ctx.source],
        ),
        .return_type = return_type_foreign_def.type_def,
        .yield_type = self.gc.type_registry.void_type,
        .function_type = .Extern,
    };

    var parameters_zig_types = std.ArrayList(ZigType.FnType.Param).empty;
    var zig_fn_type = ZigType.FnType{
        .calling_convention = .c,
        .alignment = 4,
        .is_generic = false,
        .is_var_args = false,
        .return_type = &return_type_foreign_def.zig_type,
        .params = undefined,
    };

    for (fn_type.params) |param| {
        const param_name = if (param.name != .empty)
            param.name.lookup(ctx.comp)
        else
            null;

        if (param_name == null) {
            self.reportCdef(
                ctx,
                "Please provide names to C function arguments",
            );
            return null;
        }

        const param_foreign_def = (try self.cTypeToForeignDef(ctx, param.qt, null)) orelse return null;
        if (param_foreign_def.zig_type == .Struct or param_foreign_def.zig_type == .Union) {
            self.reportCdef(
                ctx,
                "Passing C structs or unions by value is not supported in cdef",
            );
            return null;
        }

        try function_def.parameters.put(
            self.gc.allocator,
            try self.gc.copyString(param_name.?),
            param_foreign_def.type_def,
        );

        try parameters_zig_types.append(
            self.gc.allocator,
            .{
                .is_generic = false,
                .is_noalias = false,
                .type = &param_foreign_def.zig_type,
            },
        );
    }

    zig_fn_type.params = try parameters_zig_types.toOwnedSlice(self.gc.allocator);

    const type_def = try self.gc.type_registry.getTypeDef(
        .{
            .def_type = .Function,
            .resolved_type = .{
                .Function = function_def,
            },
        },
    );

    return try self.allocateNamedForeignDef(
        ctx.tree.tokSlice(function.name_tok),
        type_def,
        .{ .Fn = zig_fn_type },
    );
}

fn cRecordTypeToForeignDef(
    self: *Self,
    ctx: *CContext,
    kind: CRecordKind,
    record: CType.Record,
    preferred_name: ?[]const u8,
) Error!?*ForeignDef {
    if (preferred_name) |name| {
        if (try self.cResolveNamedType(ctx, name)) |existing| {
            return existing;
        }
    }

    const tag_name = if (!record.isAnonymous(ctx.comp))
        record.name.lookup(ctx.comp)
    else
        null;

    if (tag_name) |name| {
        if (try self.cResolveNamedType(ctx, name)) |existing| {
            if (preferred_name) |preferred| {
                if (!std.mem.eql(u8, preferred, existing.name)) {
                    const alias = try self.cloneNamedForeignDef(existing, preferred);
                    try ctx.structs.put(self.gc.allocator, alias.name, alias);
                    return alias;
                }
            }

            return existing;
        }
    }

    const exposed_name = preferred_name orelse tag_name orelse {
        self.reportCdef(
            ctx,
            "Anonymous C structs and unions are not supported in cdef",
        );
        return null;
    };

    const foreign_def = try self.cBuildRecordForeignDef(
        ctx,
        kind,
        record,
        exposed_name,
    );

    try ctx.structs.put(self.gc.allocator, foreign_def.name, foreign_def);
    if (tag_name) |name| {
        if (!std.mem.eql(u8, name, foreign_def.name)) {
            try ctx.structs.put(self.gc.allocator, name, foreign_def);
        }
    }

    return foreign_def;
}

fn cBuildRecordForeignDef(
    self: *Self,
    ctx: *CContext,
    kind: CRecordKind,
    record: CType.Record,
    exposed_name: []const u8,
) Error!*ForeignDef {
    var struct_fields = std.ArrayList(ZigType.StructField).empty;
    var union_fields = std.ArrayList(ZigType.UnionField).empty;
    var get_set_fields = std.StringArrayHashMapUnmanaged(o.ObjForeignContainer.ContainerDef.Field).empty;
    var buzz_fields = std.StringArrayHashMapUnmanaged(*o.ObjTypeDef).empty;
    var decls = std.ArrayList(ZigType.Declaration).empty;

    if (record.layout != null) {
        for (record.fields) |field| {
            if (field.name == .empty or field.name_tok == 0) {
                self.reportCdef(
                    ctx,
                    "Anonymous C struct and union fields are not supported in cdef",
                );
                return error.CantCompile;
            }

            if (field.bit_width.unpack() != null) {
                self.reportCdef(
                    ctx,
                    "C bitfields are not supported in cdef",
                );
                return error.CantCompile;
            }

            const member_foreign_def = (try self.cTypeToForeignDef(ctx, field.qt, null)) orelse return error.CantCompile;
            const field_name = try self.gc.allocator.dupe(
                u8,
                field.name.lookup(ctx.comp),
            );
            const field_align: u16 = @intCast(field.qt.alignof(ctx.comp));
            const field_offset: usize = @intCast(field.layout.offset_bits / 8);

            switch (kind) {
                .@"struct" => try struct_fields.append(
                    self.gc.allocator,
                    .{
                        .name = field_name,
                        .type = &member_foreign_def.zig_type,
                        .default_value = null,
                        .is_comptime = false,
                        .alignment = field_align,
                    },
                ),
                .@"union" => try union_fields.append(
                    self.gc.allocator,
                    .{
                        .name = field_name,
                        .type = &member_foreign_def.zig_type,
                        .alignment = field_align,
                    },
                ),
            }

            try decls.append(
                self.gc.allocator,
                .{
                    .name = field_name,
                },
            );

            try buzz_fields.put(
                self.gc.allocator,
                field_name,
                member_foreign_def.type_def,
            );

            try get_set_fields.put(
                self.gc.allocator,
                field_name,
                .{
                    .offset = field_offset,
                    .getter = undefined,
                    .setter = undefined,
                },
            );
        }
    }

    const zig_type = switch (kind) {
        .@"struct" => ZigType{
            .Struct = .{
                .layout = .@"extern",
                .fields = struct_fields.items,
                .decls = decls.items,
                .is_tuple = false,
            },
        },
        .@"union" => ZigType{
            .Union = .{
                .layout = .@"extern",
                .fields = union_fields.items,
                .decls = decls.items,
                .tag_type = null,
            },
        },
    };

    const foreign_def = o.ObjForeignContainer.ContainerDef{
        .location = ctx.source,
        .name = try self.gc.copyString(exposed_name),
        .qualified_name = try self.gc.copyString(exposed_name),
        .zig_type = zig_type,
        .buzz_type = buzz_fields,
        .fields = get_set_fields,
    };

    const type_def = try self.gc.type_registry.getTypeDef(
        .{
            .def_type = .ForeignContainer,
            .resolved_type = .{
                .ForeignContainer = foreign_def,
            },
        },
    );

    return try self.allocateNamedForeignDef(exposed_name, type_def, zig_type);
}

fn cTypeToForeignDef(
    self: *Self,
    ctx: *CContext,
    qt: CQualType,
    preferred_name: ?[]const u8,
) Error!?*ForeignDef {
    const base = qt.base(ctx.comp);

    return switch (base.type) {
        .void => try self.allocateNamedForeignDef(
            preferred_name orelse "void",
            self.gc.type_registry.void_type,
            .{ .Void = {} },
        ),
        .bool => try self.allocateNamedForeignDef(
            preferred_name orelse "bool",
            self.gc.type_registry.bool_type,
            .{ .Bool = {} },
        ),
        .int => |int_type| int: {
            const bits = int_type.bits(ctx.comp);
            const signedness = base.qt.signedness(ctx.comp);
            const type_def = switch (bits) {
                8, 16 => o.ObjTypeDef{ .def_type = .Integer },
                32 => if (signedness == .signed)
                    o.ObjTypeDef{ .def_type = .Integer }
                else
                    o.ObjTypeDef{ .def_type = .Double },
                64 => if (signedness == .signed)
                    o.ObjTypeDef{ .def_type = .Double }
                else
                    o.ObjTypeDef{ .def_type = .UserData },
                else => {
                    self.reportCdef(
                        ctx,
                        "Unsupported C integer type in cdef",
                    );
                    break :int null;
                },
            };

            break :int try self.allocateNamedForeignDef(
                preferred_name orelse "int",
                try self.gc.type_registry.getTypeDef(type_def),
                .{
                    .Int = .{
                        .signedness = signedness,
                        .bits = bits,
                    },
                },
            );
        },
        .float => |float_type| float: {
            const bits = float_type.bits(ctx.comp);
            if (bits != 32 and bits != 64) {
                self.reportCdef(
                    ctx,
                    "Unsupported C floating-point type in cdef",
                );
                break :float null;
            }

            break :float try self.allocateNamedForeignDef(
                preferred_name orelse "float",
                self.gc.type_registry.double_type,
                .{
                    .Double = .{
                        .bits = bits,
                    },
                },
            );
        },
        .pointer => |pointer| ptr: {
            const child_foreign_def = (try self.cTypeToForeignDef(ctx, pointer.child, null)) orelse break :ptr null;

            if (child_foreign_def.zig_type == .Fn) {
                self.reportCdef(
                    ctx,
                    "C function pointers are not supported in cdef",
                );
                break :ptr null;
            }

            const type_def = if (child_foreign_def.zig_type == .Int and
                child_foreign_def.zig_type.Int.bits == 8)
                self.gc.type_registry.str_type
            else if (child_foreign_def.type_def.def_type == .ForeignContainer)
                child_foreign_def.type_def
            else
                self.gc.type_registry.ud_type;

            break :ptr try self.allocateNamedForeignDef(
                preferred_name orelse "ptr",
                type_def,
                .{
                    .Pointer = .{
                        .size = .c,
                        .is_const = pointer.child.@"const",
                        .is_volatile = pointer.child.@"volatile",
                        .alignment = undefined,
                        .address_space = undefined,
                        .child = &child_foreign_def.zig_type,
                        .is_allowzero = undefined,
                        .sentinel = undefined,
                    },
                },
            );
        },
        .@"struct" => |record| try self.cRecordTypeToForeignDef(
            ctx,
            .@"struct",
            record,
            preferred_name,
        ),
        .@"union" => |record| try self.cRecordTypeToForeignDef(
            ctx,
            .@"union",
            record,
            preferred_name,
        ),
        .array,
        .vector,
        .complex,
        .bit_int,
        .atomic,
        .func,
        .@"enum",
        .nullptr_t,
        => unsupported: {
            self.reportCdef(
                ctx,
                "Unsupported C type in cdef",
            );
            break :unsupported null;
        },
        .typeof,
        .typedef,
        .attributed,
        => unreachable,
    };
}

fn getZdef(self: *Self, decl_index: Ast.Node.Index) !?*ForeignDef {
    const decl_tag = self.state.?.ast.nodeTag(decl_index);
    const decl_data = self.state.?.ast.nodeData(decl_index);
    const ast = self.state.?.ast;

    return switch (decl_tag) {
        .fn_proto_simple,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto,
        => try self.fnProto(decl_tag, decl_index),

        .identifier => try self.identifier(decl_index),

        .ptr_type_aligned,
        .ptr_type_sentinel,
        .ptr_type,
        => try self.ptrType(decl_tag, decl_index),

        .simple_var_decl => var_decl: {
            // Allow simple type if we're parsing type expr, or struct type
            if (self.state.?.type_expr != null) {
                break :var_decl try self.getZdef(ast.simpleVarDecl(decl_index).ast.type_node.unwrap().?);
            }

            switch (ast.nodeTag(ast.simpleVarDecl(decl_index).ast.init_node.unwrap().?)) {
                .container_decl,
                .container_decl_trailing,
                .container_decl_two,
                .container_decl_two_trailing,
                .container_decl_arg,
                .container_decl_arg_trailing,
                => {
                    const name = ast.tokenSlice(
                        ast.simpleVarDecl(decl_index).ast.mut_token + 1,
                    );
                    const zdef = try self.containerDecl(
                        ast.tokenSlice(
                            ast.simpleVarDecl(decl_index).ast.mut_token + 1,
                        ),
                        ast.simpleVarDecl(decl_index).ast.init_node.unwrap().?,
                    );

                    try self.state.?.structs.put(
                        self.gc.allocator,
                        name,
                        zdef,
                    );

                    break :var_decl zdef;
                },
                else => {},
            }

            const location = self.state.?.buzz_ast.?.tokens.get(self.state.?.source - 4);
            self.reporter.reportErrorFmt(
                .zdef,
                location,
                location,
                "Unsupported zig node `{}`: only C ABI compatible function signatures, structs and variables are supported",
                .{
                    decl_tag,
                },
            );
            break :var_decl null;
        },

        // TODO: do we support container_field and container_field_align?
        .container_field_init => try self.containerField(decl_index),

        .optional_type => opt: {
            const zdef = try self.getZdef(decl_data.node);

            if (zdef) |uzdef| {
                const opt_zdef = try self.gc.allocator.create(ForeignDef);
                opt_zdef.* = ForeignDef{
                    .zig_type = .{
                        .Optional = .{
                            .child = &uzdef.zig_type,
                        },
                    },
                    .type_def = try uzdef.type_def.cloneOptional(&self.gc.type_registry),
                    .name = uzdef.name,
                };

                if (uzdef.zig_type != .Pointer) {
                    const location = self.state.?.buzz_ast.?.tokens.get(self.state.?.source - 4);

                    self.reporter.reportErrorAt(
                        .zdef,
                        location,
                        location,
                        "Optionals only allowed on pointers",
                    );
                }

                break :opt opt_zdef;
            } else {
                break :opt null;
            }
        },

        else => fail: {
            const location = self.state.?.buzz_ast.?.tokens.get(self.state.?.source - 4);
            self.reporter.reportErrorFmt(
                .zdef,
                location,
                location,
                "Unsupported zig node `{}`: only C ABI compatible function signatures, structs and variables are supported",
                .{
                    decl_tag,
                },
            );
            break :fail null;
        },
    };
}

fn containerDecl(self: *Self, name: []const u8, decl_index: Ast.Node.Index) Error!*ForeignDef {
    const container_node_tag = self.state.?.ast.nodeTag(decl_index);

    var buf: [2]Ast.Node.Index = undefined;
    const container = switch (container_node_tag) {
        .container_decl => self.state.?.ast.containerDecl(decl_index),
        .container_decl_trailing => self.state.?.ast.containerDecl(decl_index),
        .container_decl_two => self.state.?.ast.containerDeclTwo(&buf, decl_index),
        .container_decl_two_trailing => self.state.?.ast.containerDeclTwo(&buf, decl_index),
        .container_decl_arg => self.state.?.ast.containerDeclArg(decl_index),
        .container_decl_arg_trailing => self.state.?.ast.containerDecl(decl_index),
        else => unreachable,
    };

    const main_token = self.state.?.ast.tokens.get(container.ast.main_token).tag;
    if ((container.layout_token == null or self.state.?.ast.tokens.get(container.layout_token.?).tag != .keyword_extern) and main_token != .keyword_opaque) {
        const location = self.state.?.buzz_ast.?.tokens.get(self.state.?.source - 4);
        self.reporter.reportErrorAt(
            .zdef,
            location,
            location,
            "Only `extern` structs are supported",
        );
    }

    return switch (main_token) {
        .keyword_opaque, .keyword_struct => self.structContainer(name, container),
        .keyword_union => self.unionContainer(name, container),

        else => unsupported: {
            const location = self.state.?.buzz_ast.?.tokens.get(self.state.?.source - 4);
            self.reporter.reportErrorFmt(
                .zdef,
                location,
                location,
                "Unsupported container {s}",
                .{self.state.?.ast.tokenSlice(container.ast.main_token)},
            );

            const zdef = try self.gc.allocator.create(ForeignDef);
            zdef.* = .{
                .type_def = self.gc.type_registry.void_type,
                .zig_type = ZigType{ .Void = {} },
                .name = name,
            };

            break :unsupported zdef;
        },
    };
}

fn unionContainer(self: *Self, name: []const u8, container: Ast.full.ContainerDecl) Error!*ForeignDef {
    var fields = std.ArrayList(ZigType.UnionField).empty;
    var get_set_fields = std.StringArrayHashMapUnmanaged(o.ObjForeignContainer.ContainerDef.Field).empty;
    var buzz_fields = std.StringArrayHashMapUnmanaged(*o.ObjTypeDef).empty;
    var decls = std.ArrayList(ZigType.Declaration).empty;
    var next_field: ?*ForeignDef = null;
    for (container.ast.members, 0..) |member, idx| {
        if (next_field orelse try self.getZdef(member)) |member_zdef| {
            next_field = if (idx < container.ast.members.len - 1)
                try self.getZdef(container.ast.members[idx + 1])
            else
                null;

            try fields.append(
                self.gc.allocator,
                ZigType.UnionField{
                    .name = member_zdef.name,
                    .type = &member_zdef.zig_type,
                    .alignment = member_zdef.zig_type.alignment(),
                },
            );

            try decls.append(
                self.gc.allocator,
                ZigType.Declaration{
                    .name = member_zdef.name,
                },
            );

            try buzz_fields.put(
                self.gc.allocator,
                member_zdef.name,
                member_zdef.type_def,
            );

            try get_set_fields.put(
                self.gc.allocator,
                member_zdef.name,
                .{
                    // Always 0 since this is an enum
                    .offset = 0,
                    .getter = undefined,
                    .setter = undefined,
                },
            );
        }
    }

    const zig_type = ZigType{
        .Union = .{
            .layout = .@"extern",
            .fields = fields.items,
            .decls = decls.items,
            .tag_type = null,
        },
    };

    var qualified_name = std.Io.Writer.Allocating.init(self.gc.allocator);
    defer qualified_name.deinit();

    try qualified_name.writer.print(
        "{s}.{s}",
        .{
            self.state.?.script,
            name,
        },
    );

    const zdef = try self.gc.allocator.create(ForeignDef);
    zdef.* = .{
        .type_def = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .ForeignContainer,
                .resolved_type = .{
                    .ForeignContainer = .{
                        .location = self.state.?.source,
                        .name = try self.gc.copyString(name),
                        // FIXME
                        .qualified_name = try self.gc.copyString(qualified_name.written()),
                        .zig_type = zig_type,
                        .buzz_type = buzz_fields,
                        .fields = get_set_fields,
                    },
                },
            },
        ),
        .zig_type = zig_type,
        .name = name,
    };

    return zdef;
}

fn structContainer(self: *Self, name: []const u8, container: Ast.full.ContainerDecl) Error!*ForeignDef {
    var fields = std.ArrayList(ZigType.StructField).empty;
    var get_set_fields = std.StringArrayHashMapUnmanaged(o.ObjForeignContainer.ContainerDef.Field).empty;
    var buzz_fields = std.StringArrayHashMapUnmanaged(*o.ObjTypeDef).empty;
    var decls = std.ArrayList(ZigType.Declaration).empty;
    var offset: usize = 0;
    var next_field: ?*ForeignDef = null;
    for (container.ast.members, 0..) |member, idx| {
        if (next_field orelse try self.getZdef(member)) |member_zdef| {
            next_field = if (idx < container.ast.members.len - 1)
                try self.getZdef(container.ast.members[idx + 1])
            else
                null;

            try fields.append(
                self.gc.allocator,
                ZigType.StructField{
                    .name = member_zdef.name,
                    .type = &member_zdef.zig_type,
                    .default_value = null,
                    .is_comptime = false,
                    .alignment = member_zdef.zig_type.alignment(),
                },
            );

            try decls.append(
                self.gc.allocator,
                ZigType.Declaration{
                    .name = member_zdef.name,
                },
            );

            try buzz_fields.put(
                self.gc.allocator,
                member_zdef.name,
                member_zdef.type_def,
            );

            try get_set_fields.put(
                self.gc.allocator,
                member_zdef.name,
                .{
                    .offset = offset,
                    .getter = undefined,
                    .setter = undefined,
                },
            );

            offset += member_zdef.zig_type.size();

            // Round up the end of the previous field to a multiple of the next field's alignment
            if (next_field) |next| {
                const next_field_align = next.zig_type.alignment();
                const current_field_size = member_zdef.zig_type.size();

                const div = @as(f64, @floatFromInt(current_field_size)) / @as(f64, @floatFromInt(next_field_align));
                const fpart = std.math.modf(div).fpart;
                const padding = @as(usize, @intFromFloat(fpart * @as(f64, @floatFromInt(next_field_align))));

                offset += padding;
            }
        }
    }

    const zig_type = ZigType{
        .Struct = .{
            .layout = .@"extern",
            .fields = fields.items,
            .decls = decls.items,
            .is_tuple = false,
        },
    };

    const foreign_def = o.ObjForeignContainer.ContainerDef{
        .location = self.state.?.source,
        .name = try self.gc.copyString(name),
        // FIXME
        .qualified_name = try self.gc.copyString(name),
        .zig_type = zig_type,
        .buzz_type = buzz_fields,
        .fields = get_set_fields,
    };

    const type_def = o.ObjTypeDef{
        .def_type = .ForeignContainer,
        .resolved_type = .{ .ForeignContainer = foreign_def },
    };

    const zdef = try self.gc.allocator.create(ForeignDef);
    zdef.* = .{
        .type_def = try self.gc.type_registry.getTypeDef(type_def),
        .zig_type = zig_type,
        .name = name,
    };

    return zdef;
}

fn containerField(self: *Self, decl_index: Ast.Node.Index) Error!?*ForeignDef {
    const container_field = self.state.?.ast.containerFieldInit(decl_index);

    if (try self.getZdef(container_field.ast.type_expr.unwrap().?)) |zdef| {
        zdef.name = self.state.?.ast.tokenSlice(self.state.?.ast.nodeMainToken(decl_index));
        return zdef;
    }

    return null;
}

fn identifier(self: *Self, decl_index: Ast.Node.Index) Error!*ForeignDef {
    const id = self.state.?.ast.tokenSlice(self.state.?.ast.nodeMainToken(decl_index));

    var type_def = if (basic_types.get(id)) |basic_type|
        basic_type
    else
        null;
    var zig_type = if (zig_basic_types.get(id)) |zig_basic_type|
        zig_basic_type
    else
        null;

    if ((type_def == null or zig_type == null) and self.state.?.parser != null) {
        // FIXME: should this account for the current namespace?
        const global_idx = glb: {
            for (self.state.?.parser.?.globals.items, 0..) |global, idx| {
                if (std.mem.eql(
                    u8,
                    id,
                    self.state.?.parser.?.ast.tokens.items(.lexeme)[global.qualified_name.name],
                )) {
                    break :glb idx;
                }
            }
            break :glb null;
        };

        const global = if (global_idx) |idx|
            self.state.?.parser.?.globals.items[idx]
        else
            null;

        if (global != null and global.?.type_def.def_type == .ForeignContainer) {
            type_def = global.?.type_def.*;
            zig_type = global.?.type_def.resolved_type.?.ForeignContainer.zig_type;
        }

        if (global == null) {
            if (self.state.?.structs.get(id)) |container| {
                type_def = container.type_def.*;
                zig_type = container.zig_type;
            }
        }
    }

    if (type_def == null or zig_type == null) {
        const location = self.state.?.buzz_ast.?.tokens.get(self.state.?.source - 4);

        self.reporter.reportErrorFmt(
            .zdef,
            location,
            location,
            "Unknown or unsupported type `{s}`",
            .{id},
        );

        zig_type = null;
    }

    const zdef = try self.gc.allocator.create(ForeignDef);
    zdef.* = .{
        .type_def = try self.gc.type_registry.getTypeDef(type_def orelse .{ .def_type = .Void }),
        .zig_type = zig_type orelse ZigType{ .Void = {} },
        .name = id,
    };

    return zdef;
}

fn ptrType(self: *Self, tag: Ast.Node.Tag, decl_index: Ast.Node.Index) Error!*ForeignDef {
    const ptr_type = switch (tag) {
        .ptr_type_aligned => self.state.?.ast.ptrTypeAligned(decl_index),
        .ptr_type_sentinel => self.state.?.ast.ptrTypeSentinel(decl_index),
        .ptr_type => self.state.?.ast.ptrType(decl_index),
        else => unreachable,
    };

    if (try self.getZdef(ptr_type.ast.child_type)) |child_type| {
        const sentinel_node_tag = if (ptr_type.ast.sentinel.unwrap()) |sentinel| self.state.?.ast.nodeTag(sentinel) else null;
        const sentinel_node_main_token = if (ptr_type.ast.sentinel.unwrap()) |sentinel| self.state.?.ast.nodeMainToken(sentinel) else null;

        // Is it a null terminated string?
        const zdef = try self.gc.allocator.create(ForeignDef);
        zdef.* = if (ptr_type.const_token != null and
            child_type.zig_type == .Int and
            child_type.zig_type.Int.bits == 8 and
            sentinel_node_tag == .number_literal and
            std.mem.eql(u8, self.state.?.ast.tokenSlice(sentinel_node_main_token.?), "0"))
            .{
                .type_def = self.gc.type_registry.str_type,
                .zig_type = ZigType{
                    .Pointer = .{
                        .size = .c,
                        .is_const = ptr_type.const_token != null,
                        .is_volatile = undefined,
                        .alignment = undefined,
                        .address_space = undefined,
                        .child = &child_type.zig_type,
                        .is_allowzero = undefined,
                        .sentinel = undefined,
                    },
                },
                .name = "ptr",
            }
        else if (child_type.type_def.def_type == .ForeignContainer)
            .{
                .type_def = child_type.type_def,
                .zig_type = ZigType{
                    .Pointer = .{
                        .size = .c,
                        .is_const = ptr_type.const_token != null,
                        .is_volatile = undefined,
                        .alignment = undefined,
                        .address_space = undefined,
                        .child = &child_type.zig_type,
                        .is_allowzero = undefined,
                        .sentinel = undefined,
                    },
                },
                .name = "ptr",
            }
        else
            .{
                .type_def = self.gc.type_registry.ud_type,
                .zig_type = ZigType{
                    .Pointer = .{
                        .size = .c,
                        .is_const = ptr_type.const_token != null,
                        .is_volatile = undefined,
                        .alignment = undefined,
                        .address_space = undefined,
                        .child = &child_type.zig_type,
                        .is_allowzero = undefined,
                        .sentinel = undefined,
                    },
                },
                .name = "ptr",
            };

        return zdef;
    }

    return error.CantCompile;
}

fn fnProto(self: *Self, tag: Ast.Node.Tag, decl_index: Ast.Node.Index) Error!*ForeignDef {
    var buffer = [1]Ast.Node.Index{undefined};
    const fn_proto = switch (tag) {
        .fn_proto_simple => self.state.?.ast.fnProtoSimple(&buffer, decl_index),
        .fn_proto_one => self.state.?.ast.fnProtoOne(&buffer, decl_index),
        .fn_proto => self.state.?.ast.fnProto(decl_index),
        .fn_proto_multi => self.state.?.ast.fnProtoMulti(decl_index),
        else => unreachable,
    };
    const return_type_zdef = try self.getZdef(fn_proto.ast.return_type.unwrap().?);

    const name = if (fn_proto.name_token) |token| self.state.?.ast.tokenSlice(token) else null;

    if (name == null) {
        const location = self.state.?.buzz_ast.?.tokens.get(self.state.?.source - 4);
        self.reporter.report(
            .zdef,
            location,
            location,
            "Functions must be named",
        );
    }

    var function_def = o.ObjFunction.FunctionDef{
        .id = o.ObjFunction.FunctionDef.nextId(),
        .name = try self.gc.copyString(name orelse "unknown"),
        .script_name = try self.gc.copyString(
            self.state.?.buzz_ast.?.tokens.items(.script_name)[self.state.?.source],
        ),
        .return_type = if (return_type_zdef) |return_type|
            return_type.type_def
        else
            self.gc.type_registry.void_type,
        .yield_type = self.gc.type_registry.void_type,
        .function_type = .Extern,
    };

    var parameters_zig_types = std.ArrayList(ZigType.FnType.Param).empty;
    var zig_fn_type = ZigType.FnType{
        .calling_convention = .c,
        // How could it be something else?
        .alignment = 4,
        .is_generic = false,
        .is_var_args = false,
        .return_type = if (return_type_zdef) |return_type|
            &return_type.zig_type
        else
            null,
        .params = undefined,
    };

    var it = fn_proto.iterate(&self.state.?.ast);
    while (it.next()) |param| {
        const param_name = if (param.name_token) |param_name_token|
            self.state.?.ast.tokenSlice(param_name_token)
        else
            null;

        if (param_name == null) {
            const location = self.state.?.buzz_ast.?.tokens.get(self.state.?.source - 4);
            self.reporter.report(
                .zdef,
                location,
                location,
                "Please provide name to functions arguments",
            );
        }

        const param_zdef = if (param.type_expr) |expr|
            try self.getZdef(expr)
        else
            null;
        if (param_zdef == null) break;

        try function_def.parameters.put(
            self.gc.allocator,
            try self.gc.copyString(param_name orelse "$"),
            param_zdef.?.type_def,
        );

        try parameters_zig_types.append(
            self.gc.allocator,
            .{
                .is_generic = false,
                .is_noalias = false,
                .type = &param_zdef.?.zig_type,
            },
        );
    }

    zig_fn_type.params = try parameters_zig_types.toOwnedSlice(self.gc.allocator);

    const type_def = o.ObjTypeDef{
        .def_type = .Function,
        .resolved_type = .{ .Function = function_def },
    };

    const zdef = try self.gc.allocator.create(ForeignDef);
    zdef.* = .{
        .zig_type = ZigType{ .Fn = zig_fn_type },
        .type_def = try self.gc.type_registry.getTypeDef(type_def),
        .name = name orelse "unknown",
    };

    return zdef;
}

fn reportZigError(self: *Self, err: Ast.Error) void {
    if (self.state.?.buzz_ast == null) {
        return;
    }

    var message = std.Io.Writer.Allocating.init(self.gc.allocator);
    defer message.deinit();

    message.writer.print("zdef could not be parsed: {}", .{err.tag}) catch unreachable;

    const location = self.state.?.buzz_ast.?.tokens.get(self.state.?.source - 4);
    self.reporter.report(
        .zdef,
        location,
        location,
        message.written(),
    );
}
