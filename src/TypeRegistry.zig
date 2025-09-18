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
const GarbageCollector = @import("GarbageCollector.zig");

const TypeRegistry = @This();

gc: *GarbageCollector,
registry: std.StringHashMap(*o.ObjTypeDef),

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

// Buffer resused when we build a type key
type_def_key_buffer: std.ArrayList(u8) = .{},

pub fn init(gc: *GarbageCollector) !TypeRegistry {
    var self = TypeRegistry{
        .gc = gc,
        .registry = .init(gc.allocator),
        .void_type = undefined,
        .str_type = undefined,
        .int_type = undefined,
        .float_type = undefined,
        .bool_type = undefined,
        .any_type = undefined,
        .pat_type = undefined,
        .ud_type = undefined,
        .rg_type = undefined,
    };

    self.void_type = try self.getTypeDef(.{ .def_type = .Void });
    self.str_type = try self.getTypeDef(.{ .def_type = .String });
    self.int_type = try self.getTypeDef(.{ .def_type = .Integer });
    self.float_type = try self.getTypeDef(.{ .def_type = .Double });
    self.bool_type = try self.getTypeDef(.{ .def_type = .Bool });
    self.any_type = try self.getTypeDef(.{ .def_type = .Any });
    self.pat_type = try self.getTypeDef(.{ .def_type = .Pattern });
    self.ud_type = try self.getTypeDef(.{ .def_type = .UserData });
    self.rg_type = try self.getTypeDef(.{ .def_type = .Range });

    return self;
}

pub fn deinit(self: *TypeRegistry) void {
    self.registry.deinit();
    self.type_def_key_buffer.deinit();
}

pub fn dump(self: *TypeRegistry) void {
    io.print("\n====== Type Registry ======\n", .{});
    var it = self.registry.iterator();
    while (it.next()) |entry| {
        io.print(
            "`{s}` = @{} `{s}`\n",
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
    self.type_def_key_buffer.shrinkRetainingCapacity(0);
    try type_def.toString(&self.type_def_key_buffer.writer(self.gc.allocator), true);

    // We don't return a cached version of a placeholder since they all maintain a particular state (link)
    if (type_def.def_type != .Placeholder) {
        if (self.registry.get(self.type_def_key_buffer.items)) |type_def_ptr| {
            return type_def_ptr;
        }
    }

    const type_def_ptr = try self.gc.allocateObject(o.ObjTypeDef, type_def);

    if (BuildOptions.debug_placeholders or BuildOptions.debug_type_registry) {
        io.print(
            "`{s}` @{}\n",
            .{
                self.type_def_key_buffer.items,
                @intFromPtr(type_def_ptr),
            },
        );
    }

    // Since the key buffer is reused, we clone the key
    var copy = try self.type_def_key_buffer.clone(self.gc.allocator);

    const slot = try self.registry.getOrPut(try copy.toOwnedSlice(self.gc.allocator));

    slot.value_ptr.* = type_def_ptr;

    if (slot.found_existing) {
        copy.deinit(self.gc.allocator);
    }

    return type_def_ptr;
}

pub fn setTypeDef(self: *TypeRegistry, type_def: *o.ObjTypeDef) !void {
    const type_def_str = try type_def.toStringAlloc(self.gc.allocator, true);

    std.debug.assert(type_def.def_type != .Placeholder);

    _ = try self.registry.put(type_def_str, type_def);

    if (BuildOptions.debug_placeholders or BuildOptions.debug_type_registry) {
        io.print(
            "`{s}` type set to @{}\n",
            .{
                type_def_str,
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
