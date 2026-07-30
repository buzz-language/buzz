const std = @import("std");
const builtin = @import("builtin");
const v = @import("vm.zig");
const Value = @import("value.zig").Value;
const o = @import("obj.zig");
const dumpStack = @import("disassembler.zig").dumpStack;
const BuildOptions = @import("build_options");
const Token = @import("Token.zig");
const buzz_api = @import("buzz_api.zig");
const Reporter = @import("Reporter.zig");
const is_wasm = builtin.cpu.arch.isWasm();
const io = @import("io.zig");
const GC = @import("GC.zig");

const TypeRegistry = @This();

pub const TypeDefHash = u64;

gc: *GC,
registry: std.AutoHashMapUnmanaged(TypeDefHash, o.ObjTypeDef.Idx) = .empty,

// Common types we reuse all the time
void_type: o.ObjTypeDef.Idx,
str_type: o.ObjTypeDef.Idx,
int_type: o.ObjTypeDef.Idx,
double_type: o.ObjTypeDef.Idx,
bool_type: o.ObjTypeDef.Idx,
any_type: o.ObjTypeDef.Idx,
pat_type: o.ObjTypeDef.Idx,
ud_type: o.ObjTypeDef.Idx,
rg_type: o.ObjTypeDef.Idx,
type_type: o.ObjTypeDef.Idx,

pub fn init(gc: *GC) !TypeRegistry {
    var self = TypeRegistry{
        .gc = gc,
        .void_type = undefined,
        .str_type = undefined,
        .int_type = undefined,
        .double_type = undefined,
        .bool_type = undefined,
        .any_type = undefined,
        .pat_type = undefined,
        .ud_type = undefined,
        .rg_type = undefined,
        .type_type = undefined,
    };

    self.void_type = (try self.getTypeDef(.{ .def_type = .Void })).toIdx();
    self.str_type = (try self.getTypeDef(.{ .def_type = .String })).toIdx();
    self.int_type = (try self.getTypeDef(.{ .def_type = .Integer })).toIdx();
    self.double_type = (try self.getTypeDef(.{ .def_type = .Double })).toIdx();
    self.bool_type = (try self.getTypeDef(.{ .def_type = .Boolean })).toIdx();
    self.any_type = (try self.getTypeDef(
        .{ .def_type = .Any },
    )).toIdx();
    self.pat_type = (try self.getTypeDef(.{ .def_type = .Pattern })).toIdx();
    self.ud_type = (try self.getTypeDef(.{ .def_type = .UserData })).toIdx();
    self.rg_type = (try self.getTypeDef(.{ .def_type = .Range })).toIdx();
    self.type_type = (try self.getTypeDef(.{ .def_type = .Type })).toIdx();

    return self;
}

pub fn deinit(self: *TypeRegistry) void {
    self.registry.deinit(self.gc.allocator);
}

pub fn dump(self: *TypeRegistry) void {
    io.print("\n====== Type Registry ======\n", .{});
    var it = self.registry.iterator();
    while (it.next()) |entry| {
        io.print(
            "#{} = @{} `{s}`\n",
            .{
                entry.key_ptr.*,
                @intFromPtr(entry.value_ptr.*),
                self.gc.getTypeDef(entry.value_ptr.*).toStringAlloc(self.gc.allocator, false, self.gc) catch unreachable,
            },
        );
    }
    io.print("===========================\n\n", .{});
}

pub fn getTypeDef(self: *TypeRegistry, type_def: o.ObjTypeDef) !*o.ObjTypeDef {
    const hash = typeDefHash(self.gc, type_def);

    // We don't return a cached version of a placeholder since they all maintain a particular state (link)
    if (type_def.def_type != .Placeholder) {
        if (self.registry.get(hash)) |type_def_ptr| {
            return self.gc.getTypeDef(type_def_ptr);
        }
    }

    const type_def_ptr = try self.gc.allocateObject(type_def);

    if (BuildOptions.debug_placeholders or BuildOptions.debug_type_registry) {
        io.print(
            "`{s}` #{} @{}\n",
            .{
                type_def_ptr.toStringAlloc(self.gc.allocator, true, self.gc) catch unreachable,
                hash,
                @intFromPtr(type_def_ptr),
            },
        );
    }

    // Since the key buffer is reused, we clone the key
    try self.registry.put(
        self.gc.allocator,
        hash,
        type_def_ptr.toIdx(),
    );

    return type_def_ptr;
}

pub fn setTypeDef(self: *TypeRegistry, type_def: *o.ObjTypeDef) !void {
    const hash = typeDefHash(self.gc, type_def.*);

    std.debug.assert(type_def.def_type != .Placeholder);

    if (self.registry.get(hash) != null) {
        return;
    }

    try self.registry.put(
        self.gc.allocator,
        hash,
        type_def.toIdx(),
    );

    if (BuildOptions.debug_placeholders or BuildOptions.debug_type_registry) {
        io.print(
            "`{s}` type set to #{} @{}\n",
            .{
                type_def.toStringAlloc(self.gc.allocator, true, self.gc) catch unreachable,
                hash,
                @intFromPtr(type_def),
            },
        );
    }
}

pub inline fn getTypeDefByName(self: *TypeRegistry, name: []const u8) ?*o.ObjTypeDef {
    return if (self.registry.get(name)) |type_def| type_def.get(self.gc) else null;
}

pub fn mark(self: *TypeRegistry) !void {
    var it = self.registry.iterator();
    while (it.next()) |kv| {
        try self.gc.markObj(o.ObjIdx.init(.Type, kv.value_ptr.*.index));
    }
}

fn hashHelper(gc: *GC, hasher: *std.hash.Wyhash, type_def: *const o.ObjTypeDef) void {
    std.hash.autoHash(hasher, type_def.def_type);
    std.hash.autoHash(hasher, type_def.optional);
    if (type_def.resolved_type) |resolved| {
        switch (resolved) {
            // We actually hash the ObjTypeDef and not its pointer since we don't put Placeholders in the registry
            // BUT: when going deeper in those type we might encounter a pointer to a Placeholder ObjTypeDef,
            // in that case we wan't to use the pointer (real this time) as hash value
            .Placeholder => std.hash.autoHash(hasher, type_def),

            .Boolean,
            .Double,
            .Integer,
            .Pattern,
            .String,
            .Type, // Something that holds a type, not an actual type
            .UserData,
            .Void,
            .Range,
            => {},

            .Any => std.hash.autoHash(hasher, resolved.Any),
            .Enum => std.hash.autoHash(hasher, resolved.Enum.qualified_name.index),
            .EnumInstance => {
                std.hash.autoHash(hasher, resolved.EnumInstance.mutable);
                hashHelper(gc, hasher, gc.getTypeDef(resolved.EnumInstance.of));
            },
            .Fiber => {
                hashHelper(gc, hasher, gc.getTypeDef(resolved.Fiber.return_type));
                hashHelper(gc, hasher, gc.getTypeDef(resolved.Fiber.yield_type));
            },
            .ForeignContainer => std.hash.autoHash(hasher, resolved.ForeignContainer.qualified_name.index),
            .Function => {
                std.hash.autoHash(hasher, resolved.Function.name.index);
                std.hash.autoHash(hasher, resolved.Function.script_name.index);
                hashHelper(gc, hasher, gc.getTypeDef(resolved.Function.return_type));
                hashHelper(gc, hasher, gc.getTypeDef(resolved.Function.yield_type));
                if (resolved.Function.error_types) |types| {
                    for (types) |error_type| {
                        hashHelper(gc, hasher, gc.getTypeDef(error_type));
                    }
                }

                {
                    const parameter_types = resolved.Function.parameters.values();
                    for (parameter_types) |parameter_type| {
                        hashHelper(gc, hasher, gc.getTypeDef(parameter_type));
                    }
                }

                std.hash.autoHash(hasher, resolved.Function.defaults.count());
                if (resolved.Function.defaults.count() > 0) {
                    const default_keys = resolved.Function.defaults.keys();
                    const default_values = resolved.Function.defaults.values();
                    for (default_keys, default_values) |default_key, default_value| {
                        std.hash.autoHash(hasher, default_key.index);
                        std.hash.autoHash(hasher, default_value.val);
                    }
                }

                std.hash.autoHash(hasher, resolved.Function.default_nodes.count());
                if (resolved.Function.default_nodes.count() > 0) {
                    const default_node_keys = resolved.Function.default_nodes.keys();
                    const default_node_values = resolved.Function.default_nodes.values();
                    for (default_node_keys, default_node_values) |default_key, default_node| {
                        std.hash.autoHash(hasher, default_key.index);
                        std.hash.autoHash(hasher, default_node);
                    }
                }

                std.hash.autoHash(hasher, resolved.Function.function_type);
                std.hash.autoHash(hasher, resolved.Function.generic_types.count());

                if (resolved.Function.resolved_generics) |types| {
                    for (types) |gen_type| {
                        hashHelper(gc, hasher, gc.getTypeDef(gen_type));
                    }
                }
            },
            .Generic => std.hash.autoHash(hasher, resolved.Generic),
            .List => {
                hashHelper(gc, hasher, resolved.List.item_type);
                std.hash.autoHash(hasher, resolved.List.mutable);
            },
            .Map => {
                hashHelper(gc, hasher, resolved.Map.key_type);
                hashHelper(gc, hasher, resolved.Map.value_type);
                std.hash.autoHash(hasher, resolved.Map.mutable);
            },
            .Object => {
                if (resolved.Object.anonymous) {
                    // If anonymous, we must take the whole type into account
                    // But since it'type_def anonymous, we only need to worry about fields type knowing there'type_def no method, static, etc.
                    std.hash.autoHash(hasher, resolved.Object.is_tuple);
                    var it = resolved.Object.fields.iterator();
                    while (it.next()) |kv| {
                        std.hash.autoHash(
                            hasher,
                            std.hash_map.hashString(kv.key_ptr.*),
                        );
                        hashHelper(gc, hasher, gc.getTypeDef(kv.value_ptr.type_def));
                    }
                } else {
                    // Actual object: name + resolved generics is distinction enough
                    std.hash.autoHash(hasher, resolved.Object.qualified_name.index);

                    if (resolved.Object.resolved_generics) |rg| {
                        for (rg) |gen| {
                            hashHelper(gc, hasher, gc.getTypeDef(gen));
                        }
                    }
                }
            },
            .ObjectInstance => {
                std.hash.autoHash(hasher, resolved.ObjectInstance.mutable);
                hashHelper(gc, hasher, gc.getTypeDef(resolved.ObjectInstance.of));
            },
            .Protocol => std.hash.autoHash(hasher, resolved.Protocol.qualified_name.index),
            .ProtocolInstance => {
                std.hash.autoHash(hasher, resolved.ProtocolInstance.mutable);
                hashHelper(gc, hasher, gc.getTypeDef(resolved.ProtocolInstance.of));
            },
        }
    }
}

pub fn typeDefHash(gc: *GC, type_def: o.ObjTypeDef) TypeDefHash {
    var hasher = std.hash.Wyhash.init(0);

    hashHelper(gc, &hasher, &type_def);

    return hasher.final();
}
