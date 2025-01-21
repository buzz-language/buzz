const std = @import("std");
const Ast = std.zig.Ast;

const BuzzAst = @import("Ast.zig");
const o = @import("obj.zig");
const Token = @import("Token.zig");
const m = @import("memory.zig");
const v = @import("value.zig");
const Parser = @import("Parser.zig");
const ZigType = @import("zigtypes.zig").Type;
const Reporter = @import("Reporter.zig");

const Self = @This();

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

        .{ "bool", o.ObjTypeDef{ .def_type = .Bool } },

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

pub const Zdef = struct {
    name: []const u8,
    type_def: *o.ObjTypeDef,
    zig_type: ZigType,
};

pub const State = struct {
    script: []const u8,
    source: Token,
    ast: Ast,
    buzz_ast: ?BuzzAst.Slice = null,
    parser: ?*Parser,
    parsing_type_expr: bool = false,
    structs: std.StringHashMap(*Zdef),
};

gc: *m.GarbageCollector,
reporter: Reporter,
state: ?State = null,
type_expr_cache: std.StringHashMap(?*Zdef),

pub fn init(gc: *m.GarbageCollector) Self {
    return .{
        .gc = gc,
        .reporter = .{
            .allocator = gc.allocator,
            .error_prefix = "FFI",
        },
        .type_expr_cache = .init(gc.allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.type_expr_cache.deinit();
}

pub fn parseTypeExpr(self: *Self, ztype: []const u8) !?*Zdef {
    if (self.type_expr_cache.get(ztype)) |zdef| {
        return zdef;
    }

    var full = std.ArrayList(u8).init(self.gc.allocator);
    defer full.deinit();

    full.writer().print("const zig_type: {s};", .{ztype}) catch @panic("Out of memory");

    const zdef = try self.parse(
        null,
        Token.identifier(full.items),
        true,
    );

    std.debug.assert(zdef == null or zdef.?.len == 1);

    self.type_expr_cache.put(
        ztype,
        if (zdef) |z| z[0] else null,
    ) catch @panic("Out of memory");

    return if (zdef) |z| z[0] else null;
}

pub fn parse(self: *Self, parser: ?*Parser, source: Token, parsing_type_expr: bool) !?[]*Zdef {
    // TODO: maybe an Arena allocator for those kinds of things that can live for the whole process lifetime
    const duped = self.gc.allocator.dupeZ(u8, source.literal_string.?) catch @panic("Out of memory");
    // defer self.gc.allocator.free(duped);

    self.state = .{
        .script = if (parser) |uparser|
            try std.mem.replaceOwned(u8, self.gc.allocator, uparser.script_name, "/", ".")
        else
            "zdef",
        .parsing_type_expr = parsing_type_expr,
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
        .structs = .init(self.gc.allocator),
    };
    defer {
        self.state.?.structs.deinit();
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
        self.reporter.report(
            .zdef,
            self.state.?.source,
            "At least one declaration is required in zdef",
        );

        return null;
    }

    var zdefs = std.ArrayList(*Zdef).init(self.gc.allocator);

    for (root_decls) |decl| {
        if (try self.getZdef(decl)) |zdef| {
            try zdefs.append(zdef);
        }
    }

    zdefs.shrinkAndFree(zdefs.items.len);

    return zdefs.items;
}

fn getZdef(self: *Self, decl_index: Ast.Node.Index) !?*Zdef {
    const decl = self.state.?.ast.nodes.get(decl_index);
    const ast = self.state.?.ast;

    return switch (decl.tag) {
        .fn_proto_simple,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto,
        => try self.fnProto(decl.tag, decl_index),

        .identifier => try self.identifier(decl_index),

        .ptr_type_aligned,
        .ptr_type_sentinel,
        .ptr_type,
        => try self.ptrType(decl.tag, decl_index),

        .simple_var_decl => var_decl: {
            // Allow simple type if we're parsing type expr, or struct type
            if (self.state.?.parsing_type_expr) {
                break :var_decl try self.getZdef(ast.simpleVarDecl(decl_index).ast.type_node);
            }

            switch (ast.nodes.get(ast.simpleVarDecl(decl_index).ast.init_node).tag) {
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
                        ast.simpleVarDecl(decl_index).ast.init_node,
                    );

                    try self.state.?.structs.put(name, zdef);

                    break :var_decl zdef;
                },
                else => {},
            }

            self.reporter.reportErrorFmt(
                .zdef,
                self.state.?.source,
                "Unsupported zig node `{}`: only C ABI compatible function signatures, structs and variables are supported",
                .{decl.tag},
            );
            break :var_decl null;
        },

        // TODO: do we support container_field and container_field_align?
        .container_field_init => try self.containerField(decl_index),

        .optional_type => opt: {
            const zdef = try self.getZdef(decl.data.lhs);

            if (zdef) |uzdef| {
                const opt_zdef = try self.gc.allocator.create(Zdef);
                opt_zdef.* = Zdef{
                    .zig_type = .{
                        .Optional = .{
                            .child = &uzdef.zig_type,
                        },
                    },
                    .type_def = try uzdef.type_def.cloneOptional(&self.gc.type_registry),
                    .name = uzdef.name,
                };

                if (uzdef.zig_type != .Pointer) {
                    self.reporter.reportErrorAt(
                        .zdef,
                        self.state.?.source,
                        "Optionals only allowed on pointers",
                    );
                }

                break :opt opt_zdef;
            } else {
                break :opt null;
            }
        },

        else => fail: {
            self.reporter.reportErrorFmt(
                .zdef,
                self.state.?.source,
                "Unsupported zig node `{}`: only C ABI compatible function signatures, structs and variables are supported",
                .{decl.tag},
            );
            break :fail null;
        },
    };
}

fn containerDecl(self: *Self, name: []const u8, decl_index: Ast.Node.Index) anyerror!*Zdef {
    const container_node = self.state.?.ast.nodes.get(decl_index);

    var buf: [2]Ast.Node.Index = undefined;
    const container = switch (container_node.tag) {
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
        self.reporter.reportErrorAt(
            .zdef,
            self.state.?.source,
            "Only `extern` structs are supported",
        );
    }

    return switch (main_token) {
        .keyword_opaque, .keyword_struct => self.structContainer(name, container),
        .keyword_union => self.unionContainer(name, container),

        else => unsupported: {
            self.reporter.reportErrorFmt(
                .zdef,
                self.state.?.source,
                "Unsupported container {s}",
                .{self.state.?.ast.tokenSlice(container.ast.main_token)},
            );

            const zdef = try self.gc.allocator.create(Zdef);
            zdef.* = .{
                .type_def = self.gc.type_registry.void_type,
                .zig_type = ZigType{ .Void = {} },
                .name = name,
            };

            break :unsupported zdef;
        },
    };
}

fn unionContainer(self: *Self, name: []const u8, container: Ast.full.ContainerDecl) anyerror!*Zdef {
    var fields = std.ArrayList(ZigType.UnionField).init(self.gc.allocator);
    var get_set_fields = std.StringArrayHashMap(o.ObjForeignContainer.ContainerDef.Field).init(self.gc.allocator);
    var buzz_fields = std.StringArrayHashMap(*o.ObjTypeDef).init(self.gc.allocator);
    var decls = std.ArrayList(ZigType.Declaration).init(self.gc.allocator);
    var next_field: ?*Zdef = null;
    for (container.ast.members, 0..) |member, idx| {
        const member_zdef = next_field orelse try self.getZdef(member);

        next_field = if (idx < container.ast.members.len - 1)
            try self.getZdef(container.ast.members[idx + 1])
        else
            null;

        try fields.append(
            ZigType.UnionField{
                .name = member_zdef.?.name,
                .type = &member_zdef.?.zig_type,
                .alignment = member_zdef.?.zig_type.alignment(),
            },
        );

        try decls.append(
            ZigType.Declaration{
                .name = member_zdef.?.name,
            },
        );

        try buzz_fields.put(
            member_zdef.?.name,
            member_zdef.?.type_def,
        );

        try get_set_fields.put(
            member_zdef.?.name,
            .{
                // Always 0 since this is an enum
                .offset = 0,
                .getter = undefined,
                .setter = undefined,
            },
        );
    }

    const zig_type = ZigType{
        .Union = .{
            .layout = .@"extern",
            .fields = fields.items,
            .decls = decls.items,
            .tag_type = null,
        },
    };

    var qualified_name = std.ArrayList(u8).init(self.gc.allocator);
    defer qualified_name.deinit();

    try qualified_name.writer().print(
        "{s}.{s}",
        .{
            self.state.?.script,
            name,
        },
    );

    const zdef = try self.gc.allocator.create(Zdef);
    zdef.* = .{
        .type_def = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .ForeignContainer,
                .resolved_type = .{
                    .ForeignContainer = .{
                        .location = self.state.?.source,
                        .name = try self.gc.copyString(name),
                        // FIXME
                        .qualified_name = try self.gc.copyString(qualified_name.items),
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

fn structContainer(self: *Self, name: []const u8, container: Ast.full.ContainerDecl) anyerror!*Zdef {
    var fields = std.ArrayList(ZigType.StructField).init(self.gc.allocator);
    var get_set_fields = std.StringArrayHashMap(o.ObjForeignContainer.ContainerDef.Field).init(self.gc.allocator);
    var buzz_fields = std.StringArrayHashMap(*o.ObjTypeDef).init(self.gc.allocator);
    var decls = std.ArrayList(ZigType.Declaration).init(self.gc.allocator);
    var offset: usize = 0;
    var next_field: ?*Zdef = null;
    for (container.ast.members, 0..) |member, idx| {
        const member_zdef = next_field orelse try self.getZdef(member);

        next_field = if (idx < container.ast.members.len - 1)
            try self.getZdef(container.ast.members[idx + 1])
        else
            null;

        try fields.append(
            ZigType.StructField{
                .name = member_zdef.?.name,
                .type = &member_zdef.?.zig_type,
                .default_value = null,
                .is_comptime = false,
                .alignment = member_zdef.?.zig_type.alignment(),
            },
        );

        try decls.append(
            ZigType.Declaration{
                .name = member_zdef.?.name,
            },
        );

        try buzz_fields.put(
            member_zdef.?.name,
            member_zdef.?.type_def,
        );

        try get_set_fields.put(
            member_zdef.?.name,
            .{
                .offset = offset,
                .getter = undefined,
                .setter = undefined,
            },
        );

        offset += member_zdef.?.zig_type.size();

        // Round up the end of the previous field to a multiple of the next field's alignment
        if (next_field) |next| {
            const next_field_align = next.zig_type.alignment();
            const current_field_size = member_zdef.?.zig_type.size();

            const div = @as(f64, @floatFromInt(current_field_size)) / @as(f64, @floatFromInt(next_field_align));
            const fpart = std.math.modf(div).fpart;
            const padding = @as(usize, @intFromFloat(fpart * @as(f64, @floatFromInt(next_field_align))));

            offset += padding;
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

    const zdef = try self.gc.allocator.create(Zdef);
    zdef.* = .{
        .type_def = try self.gc.type_registry.getTypeDef(type_def),
        .zig_type = zig_type,
        .name = name,
    };

    return zdef;
}

fn containerField(self: *Self, decl_index: Ast.Node.Index) anyerror!*Zdef {
    const container_field = self.state.?.ast.containerFieldInit(decl_index);

    var zdef = (try self.getZdef(container_field.ast.type_expr)).?;
    zdef.name = self.state.?.ast.tokenSlice(self.state.?.ast.nodes.get(decl_index).main_token);

    return zdef;
}

fn identifier(self: *Self, decl_index: Ast.Node.Index) anyerror!*Zdef {
    const id = self.state.?.ast.tokenSlice(self.state.?.ast.nodes.get(decl_index).main_token);

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
                    self.state.?.parser.?.ast.tokens.items(.lexeme)[global.name[global.name.len - 1]],
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
        // TODO: search for struct names
        self.reporter.reportErrorFmt(
            .zdef,
            self.state.?.source,
            "Unknown or unsupported type `{s}`",
            .{id},
        );
    }

    const zdef = try self.gc.allocator.create(Zdef);
    zdef.* = .{
        .type_def = try self.gc.type_registry.getTypeDef(type_def orelse .{ .def_type = .Void }),
        .zig_type = zig_type orelse ZigType{ .Void = {} },
        .name = id,
    };

    return zdef;
}

fn ptrType(self: *Self, tag: Ast.Node.Tag, decl_index: Ast.Node.Index) anyerror!*Zdef {
    const ptr_type = switch (tag) {
        .ptr_type_aligned => self.state.?.ast.ptrTypeAligned(decl_index),
        .ptr_type_sentinel => self.state.?.ast.ptrTypeSentinel(decl_index),
        .ptr_type => self.state.?.ast.ptrType(decl_index),
        else => unreachable,
    };

    const child_type = (try self.getZdef(ptr_type.ast.child_type)).?;
    const sentinel_node = self.state.?.ast.nodes.get(ptr_type.ast.sentinel);

    // Is it a null terminated string?
    const zdef = try self.gc.allocator.create(Zdef);
    zdef.* = if (ptr_type.const_token != null and
        child_type.zig_type == .Int and
        child_type.zig_type.Int.bits == 8 and
        sentinel_node.tag == .number_literal and
        std.mem.eql(u8, self.state.?.ast.tokenSlice(sentinel_node.main_token), "0"))
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

fn fnProto(self: *Self, tag: Ast.Node.Tag, decl_index: Ast.Node.Index) anyerror!*Zdef {
    var buffer = [1]Ast.Node.Index{undefined};
    const fn_proto = switch (tag) {
        .fn_proto_simple => self.state.?.ast.fnProtoSimple(&buffer, decl_index),
        .fn_proto_one => self.state.?.ast.fnProtoOne(&buffer, decl_index),
        .fn_proto => self.state.?.ast.fnProto(decl_index),
        .fn_proto_multi => self.state.?.ast.fnProtoMulti(decl_index),
        else => unreachable,
    };
    const return_type_zdef = try self.getZdef(fn_proto.ast.return_type);

    const name = if (fn_proto.name_token) |token| self.state.?.ast.tokenSlice(token) else null;

    if (name == null) {
        self.reporter.report(
            .zdef,
            self.state.?.source,
            "Functions must be named",
        );
    }

    var function_def = o.ObjFunction.FunctionDef{
        .id = o.ObjFunction.FunctionDef.nextId(),
        .name = try self.gc.copyString(name orelse "unknown"),
        .script_name = try self.gc.copyString(self.state.?.source.script_name),
        .return_type = if (return_type_zdef) |return_type|
            return_type.type_def
        else
            self.gc.type_registry.void_type,
        .yield_type = self.gc.type_registry.void_type,
        .parameters = .init(self.gc.allocator),
        .defaults = .init(self.gc.allocator),
        .function_type = .Extern,
        .generic_types = .init(self.gc.allocator),
    };

    var parameters_zig_types = std.ArrayList(ZigType.FnType.Param).init(self.gc.allocator);
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
            self.reporter.report(
                .zdef,
                self.state.?.source,
                "Please provide name to functions arguments",
            );
        }

        const param_zdef = try self.getZdef(param.type_expr);
        if (param_zdef == null) break;

        try function_def.parameters.put(
            try self.gc.copyString(param_name orelse "$"),
            param_zdef.?.type_def,
        );

        try parameters_zig_types.append(
            .{
                .is_generic = false,
                .is_noalias = false,
                .type = &param_zdef.?.zig_type,
            },
        );
    }

    parameters_zig_types.shrinkAndFree(parameters_zig_types.items.len);
    zig_fn_type.params = parameters_zig_types.items;

    const type_def = o.ObjTypeDef{
        .def_type = .Function,
        .resolved_type = .{ .Function = function_def },
    };

    const zdef = try self.gc.allocator.create(Zdef);
    zdef.* = .{
        .zig_type = ZigType{ .Fn = zig_fn_type },
        .type_def = try self.gc.type_registry.getTypeDef(type_def),
        .name = name orelse "unknown",
    };

    return zdef;
}

fn reportZigError(self: *Self, err: Ast.Error) void {
    var message = std.ArrayList(u8).init(self.gc.allocator);
    defer message.deinit();

    message.writer().print("zdef could not be parsed: {}", .{err.tag}) catch unreachable;

    self.reporter.report(
        .zdef,
        self.state.?.source,
        message.items,
    );
}
