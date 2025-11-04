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
registry: std.AutoHashMapUnmanaged(TypeDefHash, *o.ObjTypeDef) = .empty,

// Common types we reuse all the time
void_type: *o.ObjTypeDef,
str_type: *o.ObjTypeDef,
int_type: *o.ObjTypeDef,
float_type: *o.ObjTypeDef,
bool_type: *o.ObjTypeDef,
any_type: *o.ObjTypeDef,
pat_type: *o.ObjTypeDef,
ud_type: *o.ObjTypeDef,
rg_type: *o.ObjTypeDef,
type_type: *o.ObjTypeDef,

pub fn init(gc: *GC) !TypeRegistry {
    var self = TypeRegistry{
        .gc = gc,
        .void_type = undefined,
        .str_type = undefined,
        .int_type = undefined,
        .float_type = undefined,
        .bool_type = undefined,
        .any_type = undefined,
        .pat_type = undefined,
        .ud_type = undefined,
        .rg_type = undefined,
        .type_type = undefined,
    };

    self.void_type = try self.getTypeDef(.{ .def_type = .Void });
    self.str_type = try self.getTypeDef(.{ .def_type = .String });
    self.int_type = try self.getTypeDef(.{ .def_type = .Integer });
    self.float_type = try self.getTypeDef(.{ .def_type = .Double });
    self.bool_type = try self.getTypeDef(.{ .def_type = .Bool });
    self.any_type = try self.getTypeDef(
        .{
            .def_type = .Any,
            .resolved_type = .{
                .Any = false,
            },
        },
    );
    self.pat_type = try self.getTypeDef(.{ .def_type = .Pattern });
    self.ud_type = try self.getTypeDef(.{ .def_type = .UserData });
    self.rg_type = try self.getTypeDef(.{ .def_type = .Range });
    self.type_type = try self.getTypeDef(.{ .def_type = .Type });

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
                entry.value_ptr.*.toStringAlloc(self.gc.allocator) catch unreachable,
            },
        );
    }
    io.print("===========================\n\n", .{});
}

pub fn getTypeDef(self: *TypeRegistry, type_def: o.ObjTypeDef) !*o.ObjTypeDef {
    const hash = typeDefHash(type_def);

    // We don't return a cached version of a placeholder since they all maintain a particular state (link)
    if (type_def.def_type != .Placeholder) {
        if (self.registry.get(hash)) |type_def_ptr| {
            return type_def_ptr;
        }
    }

    const type_def_ptr = try self.gc.allocateObject(type_def);

    if (BuildOptions.debug_placeholders or BuildOptions.debug_type_registry) {
        io.print(
            "`{s}` #{} @{}\n",
            .{
                type_def_ptr.toStringAlloc(self.gc.allocator, true) catch unreachable,
                hash,
                @intFromPtr(type_def_ptr),
            },
        );
    }

    // Since the key buffer is reused, we clone the key
    try self.registry.put(
        self.gc.allocator,
        hash,
        type_def_ptr,
    );

    return type_def_ptr;
}

pub fn setTypeDef(self: *TypeRegistry, type_def: *o.ObjTypeDef) !void {
    const hash = typeDefHash(type_def.*);

    std.debug.assert(type_def.def_type != .Placeholder);

    try self.registry.put(
        self.gc.allocator,
        hash,
        type_def,
    );

    if (BuildOptions.debug_placeholders or BuildOptions.debug_type_registry) {
        io.print(
            "`{s}` type set to #{} @{}\n",
            .{
                type_def.toStringAlloc(self.gc.allocator, true) catch unreachable,
                hash,
                @intFromPtr(type_def),
            },
        );
    }
}

pub inline fn getTypeDefByName(self: *TypeRegistry, name: []const u8) ?*o.ObjTypeDef {
    return self.registry.get(name);
}

pub fn mark(self: *TypeRegistry) !void {
    var it = self.registry.iterator();
    while (it.next()) |kv| {
        try self.gc.markObj(@constCast(kv.value_ptr.*).toObj());
    }
}

fn hashHelper(hasher: *std.hash.Wyhash, type_def: *const o.ObjTypeDef) void {
    std.hash.autoHash(hasher, type_def.def_type);
    std.hash.autoHash(hasher, type_def.optional);
    if (type_def.resolved_type) |resolved| {
        switch (resolved) {
            // We actually hash the ObjTypeDef and not its pointer since we don't put Placeholders in the registry
            // BUT: when going deeper in those type we might encounter a pointer to a Placeholder ObjTypeDef,
            // in that case we wan't to use the pointer (real this time) as hash value
            .Placeholder => std.hash.autoHash(hasher, type_def),

            .Bool,
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
            .Enum => std.hash.autoHash(
                hasher,
                std.hash_map.hashString(resolved.Enum.qualified_name.string),
            ),
            .EnumInstance => {
                std.hash.autoHash(hasher, resolved.EnumInstance.mutable);
                hashHelper(hasher, resolved.EnumInstance.of);
            },
            .Fiber => {
                hashHelper(hasher, resolved.Fiber.return_type);
                hashHelper(hasher, resolved.Fiber.yield_type);
            },
            .ForeignContainer => std.hash.autoHash(
                hasher,
                std.hash_map.hashString(resolved.ForeignContainer.qualified_name.string),
            ),
            .Function => {
                std.hash.autoHash(
                    hasher,
                    std.hash_map.hashString(resolved.Function.name.string),
                );
                std.hash.autoHash(
                    hasher,
                    std.hash_map.hashString(resolved.Function.script_name.string),
                );
                hashHelper(hasher, resolved.Function.return_type);
                hashHelper(hasher, resolved.Function.yield_type);
                if (resolved.Function.error_types) |types| {
                    for (types) |error_type| {
                        hashHelper(hasher, error_type);
                    }
                }

                {
                    var it = resolved.Function.parameters.iterator();
                    while (it.next()) |kv| {
                        std.hash.autoHash(
                            hasher,
                            std.hash_map.hashString(kv.key_ptr.*.string),
                        );
                        hashHelper(hasher, kv.value_ptr.*);
                    }
                }

                {
                    var it = resolved.Function.defaults.iterator();
                    while (it.next()) |kv| {
                        std.hash.autoHash(
                            hasher,
                            std.hash_map.hashString(kv.key_ptr.*.string),
                        );
                        std.hash.autoHash(hasher, kv.value_ptr.*);
                    }
                }

                std.hash.autoHash(hasher, resolved.Function.function_type);
                std.hash.autoHash(hasher, resolved.Function.lambda);

                for (resolved.Function.generic_types.keys()) |generic| {
                    std.hash.autoHash(
                        hasher,
                        std.hash_map.hashString(generic.string),
                    );
                }

                if (resolved.Function.resolved_generics) |types| {
                    for (types) |gen_type| {
                        hashHelper(hasher, gen_type);
                    }
                }
            },
            .Generic => std.hash.autoHash(hasher, resolved.Generic),
            .List => {
                hashHelper(hasher, resolved.List.item_type);
                std.hash.autoHash(hasher, resolved.List.mutable);
            },
            .Map => {
                hashHelper(hasher, resolved.Map.key_type);
                hashHelper(hasher, resolved.Map.value_type);
                std.hash.autoHash(hasher, resolved.Map.mutable);
            },
            .Object => {
                if (resolved.Object.anonymous) {
                    // If anonymous, we must take the whole type into account
                    // But since it'type_def anonymous, we only need to worry about fields type knowing there'type_def no method, static, etc.
                    var it = resolved.Object.fields.iterator();
                    while (it.next()) |kv| {
                        std.hash.autoHash(
                            hasher,
                            std.hash_map.hashString(kv.key_ptr.*),
                        );
                        hashHelper(hasher, kv.value_ptr.type_def);
                    }
                } else {
                    // Actual object: name + resolved generics is distinction enough
                    std.hash.autoHash(
                        hasher,
                        std.hash_map.hashString(resolved.Object.qualified_name.string),
                    );

                    if (resolved.Object.resolved_generics) |rg| {
                        for (rg) |gen| {
                            hashHelper(hasher, gen);
                        }
                    }
                }
            },
            .ObjectInstance => {
                std.hash.autoHash(hasher, resolved.ObjectInstance.mutable);
                hashHelper(hasher, resolved.ObjectInstance.of);
            },
            .Protocol => std.hash.autoHash(
                hasher,
                std.hash_map.hashString(resolved.Protocol.qualified_name.string),
            ),
            .ProtocolInstance => {
                std.hash.autoHash(hasher, resolved.ProtocolInstance.mutable);
                hashHelper(hasher, resolved.ProtocolInstance.of);
            },
        }
    }
}

pub fn typeDefHash(type_def: o.ObjTypeDef) TypeDefHash {
    var hasher = std.hash.Wyhash.init(0);

    hashHelper(&hasher, &type_def);

    return hasher.final();
}
