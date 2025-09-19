const std = @import("std");
const builtin = @import("builtin");
const v = @import("vm.zig");
const Value = @import("value.zig").Value;
const o = @import("obj.zig");
const TypeRegistry = @import("TypeRegistry.zig");
const BuildOptions = @import("build_options");
const buzz_api = @import("buzz_api.zig");
const is_wasm = builtin.cpu.arch.isWasm();

/// Cheney's semi-space GC
const GC = @This();

allocator: std.mem.Allocator,
from: Space,
to: Space,

strings: std.StringHashMapUnmanaged(*o.ObjString) = .empty,
type_registry: TypeRegistry,

active_vms: std.AutoHashMapUnmanaged(*v.VM, void) = .empty,
collect_ongoing: bool = false,

// Types we generaly don't wan't to ever be collected
objfiber_members: []?*o.ObjNative,
objfiber_memberDefs: []?*o.ObjTypeDef,
objpattern_members: []?*o.ObjNative,
objpattern_memberDefs: []?*o.ObjTypeDef,
objstring_members: []?*o.ObjNative,
objstring_memberDefs: []?*o.ObjTypeDef,
objrange_members: []?*o.ObjNative,
objrange_memberDefs: []?*o.ObjTypeDef,

pub fn init(allocator: std.mem.Allocator) Space.Error!GC {
    var self = GC{
        .allocator = allocator,
        .type_registry = undefined,
        .from = try Space.init(allocator, BuildOptions.initial_gc_size * 1024),
        .to = try Space.init(allocator, BuildOptions.initial_gc_size * 1024),

        .objfiber_members = try allocator.alloc(?*o.ObjNative, o.ObjFiber.members.len),
        .objfiber_memberDefs = try allocator.alloc(?*o.ObjTypeDef, o.ObjFiber.members.len),
        .objpattern_members = try allocator.alloc(?*o.ObjNative, o.ObjPattern.members.len),
        .objpattern_memberDefs = try allocator.alloc(?*o.ObjTypeDef, o.ObjPattern.members.len),
        .objstring_members = try allocator.alloc(?*o.ObjNative, o.ObjString.members.len),
        .objstring_memberDefs = try allocator.alloc(?*o.ObjTypeDef, o.ObjString.members.len),
        .objrange_members = try allocator.alloc(?*o.ObjNative, o.ObjRange.members.len),
        .objrange_memberDefs = try allocator.alloc(?*o.ObjTypeDef, o.ObjRange.members.len),
    };

    for (0..o.ObjFiber.members.len) |i| {
        self.objfiber_members[i] = null;
        self.objfiber_memberDefs[i] = null;
    }

    for (0..o.ObjPattern.members.len) |i| {
        self.objpattern_members[i] = null;
        self.objpattern_memberDefs[i] = null;
    }

    for (0..o.ObjString.members.len) |i| {
        self.objstring_members[i] = null;
        self.objstring_memberDefs[i] = null;
    }

    for (0..o.ObjRange.members.len) |i| {
        self.objrange_members[i] = null;
        self.objrange_memberDefs[i] = null;
    }

    self.type_registry = try TypeRegistry.init(&self);

    return self;
}

pub fn deinit(self: *GC) void {
    self.from.deinit(self.allocator);
    self.to.deinit(self.allocator);
    self.strings.deinit(self.allocator);
    self.active_vms.deinit(self.allocator);

    self.allocator.free(self.objfiber_members);
    self.allocator.free(self.objfiber_memberDefs);
    self.allocator.free(self.objpattern_members);
    self.allocator.free(self.objpattern_memberDefs);
    self.allocator.free(self.objstring_members);
    self.allocator.free(self.objstring_memberDefs);
    self.allocator.free(self.objrange_members);
    self.allocator.free(self.objrange_memberDefs);
}

pub fn registerVM(self: *GC, vm: *v.VM) !void {
    try self.active_vms.put(
        self.allocator,
        vm,
        {},
    );
}

pub fn unregisterVM(self: *GC, vm: *v.VM) void {
    std.debug.assert(self.active_vms.remove(vm));
}

fn getActiveVM(self: *GC) ?*v.VM {
    var vm_it = self.active_vms.iterator();
    const first = vm_it.next();

    return if (first) |kv|
        if (kv.key_ptr.*.flavor != .Repl)
            kv.key_ptr.*
        else
            null
    else
        null;
}

pub fn allocate(self: *GC, comptime T: type, value: T) Space.Error!*T {
    // If we're already collecting, this allocate might have been triggered by an object's collect method
    // We simply allocate directly in the to-space
    if (self.collect_ongoing) {
        if (BuildOptions.gc_debug) {
            std.log.debug("Allocate while collecting", .{});
        }

        if (self.to.hasSpaceFor(T)) {
            return self.to.allocate(value);
        } else {
            // Not recoverable
            return Space.Error.OutOfSpace;
        }
    }

    if (self.from.hasSpaceFor(T)) {
        return self.from.allocate(value);
    } else {
        try self.collect();

        // Still no space left, we need to resize the spaces
        if (!self.from.hasSpaceFor(T)) {
            try self.resize(self.to.memory.len * BuildOptions.next_gc_ratio);
        }

        // Try again
        return self.allocate(T, value);
    }
}

pub fn collect(self: *GC) Space.Error!void {
    // Collect might have been triggered by a object's collect method
    if (self.collect_ongoing) {
        if (BuildOptions.gc_debug) {
            std.log.debug("Collect triggered while already collecting", .{});
        }

        return;
    }

    if (BuildOptions.gc_debug) {
        std.log.debug("Collecting...", .{});
    }

    if (self.getActiveVM()) |vm| {
        self.collect_ongoing = true;
        defer self.collect_ongoing = false;

        self.to.reset();

        if (BuildOptions.gc_debug) {
            std.log.debug("Moving roots...", .{});
        }

        try self.moveRoots(vm);

        if (BuildOptions.gc_debug) {
            std.log.debug("Scanning...", .{});
        }

        // Now scan from there
        var scan_header = self.to.first_header;
        while (scan_header) |header| : (scan_header = header.next) {
            try switch (header.type) {
                .String => self.scan(@as(*o.ObjString, @fieldParentPtr("obj", header))),
                .Type => self.scan(@as(*o.ObjTypeDef, @fieldParentPtr("obj", header))),
                .UpValue => self.scan(@as(*o.ObjUpValue, @fieldParentPtr("obj", header))),
                .Closure => self.scan(@as(*o.ObjClosure, @fieldParentPtr("obj", header))),
                .Function => self.scan(@as(*o.ObjFunction, @alignCast(@fieldParentPtr("obj", header)))),
                .ObjectInstance => self.scan(@as(*o.ObjObjectInstance, @fieldParentPtr("obj", header))),
                .Object => self.scan(@as(*o.ObjObject, @fieldParentPtr("obj", header))),
                .List => self.scan(@as(*o.ObjList, @fieldParentPtr("obj", header))),
                .Map => self.scan(@as(*o.ObjMap, @fieldParentPtr("obj", header))),
                .Enum => self.scan(@as(*o.ObjEnum, @fieldParentPtr("obj", header))),
                .EnumInstance => self.scan(@as(*o.ObjEnumInstance, @fieldParentPtr("obj", header))),
                .Bound => self.scan(@as(*o.ObjBoundMethod, @fieldParentPtr("obj", header))),
                .Native => self.scan(@as(*o.ObjNative, @fieldParentPtr("obj", header))),
                .UserData => self.scan(@as(*o.ObjUserData, @fieldParentPtr("obj", header))),
                .Pattern => self.scan(@as(*o.ObjPattern, @fieldParentPtr("obj", header))),
                .Fiber => self.scan(@as(*o.ObjFiber, @fieldParentPtr("obj", header))),
                .ForeignContainer => self.scan(@as(*o.ObjForeignContainer, @fieldParentPtr("obj", header))),
                .Range => self.scan(@as(*o.ObjRange, @fieldParentPtr("obj", header))),
            };
        }

        // Fixed interned strings ptr
        var new_strings = std.StringHashMapUnmanaged(*o.ObjString).empty;
        var it = self.strings.iterator();
        while (it.next()) |kv| {
            try new_strings.put(
                self.allocator,
                kv.key_ptr.*,
                if (kv.value_ptr.*.obj.forward) |forward|
                    @fieldParentPtr("obj", forward)
                else
                    kv.value_ptr.*,
            );
        }
        self.strings.deinit(self.allocator);
        self.strings = new_strings;

        if (BuildOptions.gc_debug) {
            std.log.debug("Deinit dead objects...", .{});
        }

        // Deinit dead objects
        var from_header = self.from.first_header;
        var collected_count: usize = 0;
        var collected_bytes: usize = 0;
        while (from_header) |header| : (from_header = header.next) {
            // No forward adress, the object is dead
            if (header.forward == null) {
                collected_count += 1;
                collected_bytes += self.deinitObj(header);
            }
        }

        // Now we switch spaces
        const tmp = self.to;
        self.to = self.from;
        self.from = tmp;

        if (BuildOptions.gc_debug) {
            std.log.debug(
                "Collected {} objects for {} bytes",
                .{
                    collected_count,
                    collected_bytes,
                },
            );
        }

        // If spaces are too empty, shrink them
        if (self.from.occupiedSpace() < BuildOptions.shrink_gc_ratio) {
            try self.resize(self.from.occupiedBytes() * BuildOptions.next_gc_ratio);
        }
    }
}

fn resize(self: *GC, size: usize) error{ OutOfMemory, OutOfSpace }!void {
    if (BuildOptions.gc_debug) {
        std.log.debug(
            "Resizing from {} to {}",
            .{
                self.to.memory.len,
                size,
            },
        );
    }

    const new = try Space.init(self.allocator, size);

    self.to.deinit(self.allocator);
    self.to = new;

    // Now do a collect, it will move every live object from the old from-space to the new to-space and then switch them
    try self.collect();

    // Now replace the to-space (because the previous new space is now the from-space) with a new space
    const new_to = try Space.init(self.allocator, size);
    self.to = new_to;
}

/// Move value from -> to and returns the new header ptr, if already moved, returns the forward ptr
fn move(self: *GC, value: anytype) error{OutOfSpace}!@TypeOf(value) {
    const T = @typeInfo(@TypeOf(value)).pointer.child;

    // If it was already moved, return the forward ptr
    if (value.obj.forward) |forward| {
        return forward.cast(T, forward.type).?;
    }

    // Copy value to to-space
    const copy = try self.to.allocate(value.*);

    // In old value header, set forward address
    value.obj.forward = &copy.obj;

    return copy;
}

fn deinitObj(self: *GC, obj: *o.Obj) usize {
    return switch (obj.type) {
        .String => self.deinitValue(o.ObjString, o.ObjString.cast(obj).?),
        .Type => self.deinitValue(o.ObjTypeDef, o.ObjTypeDef.cast(obj).?),
        .UpValue => self.deinitValue(o.ObjUpValue, o.ObjUpValue.cast(obj).?),
        .Closure => self.deinitValue(o.ObjClosure, o.ObjClosure.cast(obj).?),
        .Function => self.deinitValue(o.ObjFunction, o.ObjFunction.cast(obj).?),
        .ObjectInstance => self.deinitValue(o.ObjObjectInstance, o.ObjObjectInstance.cast(obj).?),
        .Object => self.deinitValue(o.ObjObject, o.ObjObject.cast(obj).?),
        .List => self.deinitValue(o.ObjList, o.ObjList.cast(obj).?),
        .Map => self.deinitValue(o.ObjMap, o.ObjMap.cast(obj).?),
        .Enum => self.deinitValue(o.ObjEnum, o.ObjEnum.cast(obj).?),
        .EnumInstance => self.deinitValue(o.ObjEnumInstance, o.ObjEnumInstance.cast(obj).?),
        .Bound => self.deinitValue(o.ObjBoundMethod, o.ObjBoundMethod.cast(obj).?),
        .Native => self.deinitValue(o.ObjNative, o.ObjNative.cast(obj).?),
        .UserData => self.deinitValue(o.ObjUserData, o.ObjUserData.cast(obj).?),
        .Pattern => self.deinitValue(o.ObjPattern, o.ObjPattern.cast(obj).?),
        .Fiber => self.deinitValue(o.ObjFiber, o.ObjFiber.cast(obj).?),
        .ForeignContainer => self.deinitValue(o.ObjForeignContainer, o.ObjForeignContainer.cast(obj).?),
        .Range => self.deinitValue(o.ObjRange, o.ObjRange.cast(obj).?),
    };
}

fn deinitValue(self: *GC, comptime T: type, value: *T) usize {
    if (BuildOptions.gc_debug) {
        std.log.debug("Deinit {*}", .{value});
    }

    var bytes: usize = @sizeOf(T);
    switch (T) {
        o.ObjEnumInstance,
        o.ObjBoundMethod,
        o.ObjNative,
        o.ObjUserData,
        o.ObjRange,
        => {
            // Nothing to do
        },
        o.ObjClosure,
        o.ObjFunction,
        o.ObjObject,
        o.ObjList,
        o.ObjMap,
        => value.deinit(self.allocator),
        o.ObjString => {
            // Remove it from interned strings
            _ = self.strings.remove(value.string);

            bytes += value.string.len;
            self.allocator.free(value.string);
        },
        o.ObjPattern => {
            if (!is_wasm) {
                value.pattern.free();
            }
        },
        o.ObjTypeDef => {
            // Remove from type registry
            const hash = TypeRegistry.typeDefHash(value.*);
            if (self.type_registry.registry.get(hash)) |type_def| {
                // Because of how we resolve Placeholders, this type might be valid but out of the registry
                if (type_def == value) {
                    _ = self.type_registry.registry.remove(hash);
                }
            }

            value.deinit();
        },
        o.ObjUpValue => {
            if (value.closed) |closed| {
                if (closed.isObj()) {
                    bytes += self.deinitObj(closed.obj());
                }
            }
        },
        o.ObjObjectInstance => {
            // Calling eventual destructor method
            if (value.object) |object| {
                if (object.type_def.resolved_type.?.Object.fields.get("collect")) |field| {
                    if (field.method and !field.static) {
                        buzz_api.bz_invoke(
                            value.vm,
                            value.toValue(),
                            field.index,
                            null,
                            0,
                            null,
                        );

                        // Remove void result of the collect call
                        _ = value.vm.pop();
                    }
                }
            }

            bytes += @sizeOf(Value) * value.fields.len;

            value.deinit(self.allocator);
        },
        o.ObjEnum => {
            bytes += value.cases.len * @sizeOf(Value);

            self.allocator.free(value.cases);
        },
        o.ObjFiber => {
            bytes += (@sizeOf(Value) * value.fiber.stack.len) +
                (@sizeOf(v.CallFrame) * value.fiber.frames.items.len) +
                @sizeOf(v.Fiber);

            value.fiber.deinit();
            self.allocator.destroy(value.fiber);
        },
        o.ObjForeignContainer => {
            bytes += value.data.len;

            self.allocator.free(value.data);
        },
        else => unreachable,
    }

    return bytes;
}

/// Move value and return a new one pointing to the eventually moved o.Obj
fn moveValue(self: *GC, value: Value) error{OutOfSpace}!Value {
    if (value.isObj()) {
        const obj = value.obj();
        return Value.fromObj(
            switch (obj.type) {
                .String => (try self.move(@as(*o.ObjString, @fieldParentPtr("obj", obj)))).toObj(),
                .Type => (try self.move(@as(*o.ObjTypeDef, @fieldParentPtr("obj", obj)))).toObj(),
                .UpValue => (try self.move(@as(*o.ObjUpValue, @fieldParentPtr("obj", obj)))).toObj(),
                .Closure => (try self.move(@as(*o.ObjClosure, @fieldParentPtr("obj", obj)))).toObj(),
                .Function => (try self.move(@as(*o.ObjFunction, @alignCast(@fieldParentPtr("obj", obj))))).toObj(),
                .ObjectInstance => (try self.move(@as(*o.ObjObjectInstance, @fieldParentPtr("obj", obj)))).toObj(),
                .Object => (try self.move(@as(*o.ObjObject, @fieldParentPtr("obj", obj)))).toObj(),
                .List => (try self.move(@as(*o.ObjList, @fieldParentPtr("obj", obj)))).toObj(),
                .Map => (try self.move(@as(*o.ObjMap, @fieldParentPtr("obj", obj)))).toObj(),
                .Enum => (try self.move(@as(*o.ObjEnum, @fieldParentPtr("obj", obj)))).toObj(),
                .EnumInstance => (try self.move(@as(*o.ObjEnumInstance, @fieldParentPtr("obj", obj)))).toObj(),
                .Bound => (try self.move(@as(*o.ObjBoundMethod, @fieldParentPtr("obj", obj)))).toObj(),
                .Native => (try self.move(@as(*o.ObjNative, @fieldParentPtr("obj", obj)))).toObj(),
                .UserData => (try self.move(@as(*o.ObjUserData, @fieldParentPtr("obj", obj)))).toObj(),
                .Pattern => (try self.move(@as(*o.ObjPattern, @fieldParentPtr("obj", obj)))).toObj(),
                .Fiber => (try self.move(@as(*o.ObjFiber, @fieldParentPtr("obj", obj)))).toObj(),
                .ForeignContainer => (try self.move(@as(*o.ObjForeignContainer, @fieldParentPtr("obj", obj)))).toObj(),
                .Range => (try self.move(@as(*o.ObjRange, @fieldParentPtr("obj", obj)))).toObj(),
            },
        );
    }

    return value;
}

fn scan(self: *GC, value: anytype) Space.Error!void {
    const T = @typeInfo(@TypeOf(value)).pointer.child;

    // Move children too
    switch (T) {
        o.ObjString,
        o.ObjNative,
        o.ObjUserData,
        o.ObjPattern,
        o.ObjRange,
        => {},
        o.ObjTypeDef => {
            if (value.resolved_type) |*resolved| {
                if (resolved.* == .ObjectInstance) {
                    resolved.ObjectInstance.of = try self.move(resolved.ObjectInstance.of);
                } else if (resolved.* == .EnumInstance) {
                    resolved.EnumInstance.of = try self.move(resolved.EnumInstance.of);
                } else if (resolved.* == .Object) {
                    resolved.Object.name = try self.move(resolved.Object.name);
                    resolved.Object.qualified_name = try self.move(resolved.Object.qualified_name);

                    {
                        var it = resolved.Object.fields.iterator();
                        while (it.next()) |kv| {
                            kv.value_ptr.*.type_def = try self.move(kv.value_ptr.*.type_def);
                        }
                    }

                    {
                        var it = resolved.Object.placeholders.iterator();
                        while (it.next()) |kv| {
                            kv.value_ptr.*.placeholder = try self.move(kv.value_ptr.*.placeholder);
                        }
                    }

                    {
                        var it = resolved.Object.static_placeholders.iterator();
                        while (it.next()) |kv| {
                            kv.value_ptr.*.placeholder = try self.move(kv.value_ptr.*.placeholder);
                        }
                    }

                    {
                        var new_conforms_to = std.AutoHashMapUnmanaged(*o.ObjTypeDef, void).empty;
                        var it = resolved.Object.conforms_to.iterator();
                        while (it.next()) |kv| {
                            try new_conforms_to.put(
                                self.allocator,
                                try self.move(kv.key_ptr.*),
                                {},
                            );
                        }
                        resolved.Object.conforms_to.deinit(self.allocator);
                        resolved.Object.conforms_to = new_conforms_to;
                    }

                    {
                        var new_generic_types = std.AutoArrayHashMapUnmanaged(*o.ObjString, *o.ObjTypeDef).empty;
                        var it = resolved.Object.generic_types.iterator();
                        while (it.next()) |kv| {
                            try new_generic_types.put(
                                self.allocator,
                                try self.move(kv.key_ptr.*),
                                try self.move(kv.value_ptr.*),
                            );
                        }
                        resolved.Object.generic_types.deinit(self.allocator);
                        resolved.Object.generic_types = new_generic_types;
                    }
                } else if (resolved.* == .Protocol) {
                    resolved.Protocol.name = try self.move(resolved.Protocol.name);
                    resolved.Protocol.qualified_name = try self.move(resolved.Protocol.qualified_name);

                    var it = resolved.Protocol.methods.iterator();
                    while (it.next()) |kv| {
                        kv.value_ptr.*.type_def = try self.move(kv.value_ptr.*.type_def);
                    }
                } else if (resolved.* == .Enum) {
                    resolved.Enum.name = try self.move(resolved.Enum.name);
                    resolved.Enum.qualified_name = try self.move(resolved.Enum.qualified_name);
                    resolved.Enum.enum_type = try self.move(resolved.Enum.enum_type);
                } else if (resolved.* == .Function) {
                    resolved.Function.name = try self.move(resolved.Function.name);
                    resolved.Function.script_name = try self.move(resolved.Function.script_name);
                    resolved.Function.return_type = try self.move(resolved.Function.return_type);
                    resolved.Function.yield_type = try self.move(resolved.Function.yield_type);

                    {
                        var new_parameters = std.AutoArrayHashMapUnmanaged(*o.ObjString, *o.ObjTypeDef).empty;
                        var it = resolved.Function.parameters.iterator();
                        while (it.next()) |parameter| {
                            try new_parameters.put(
                                self.allocator,
                                try self.move(parameter.key_ptr.*),
                                try self.move(parameter.value_ptr.*),
                            );
                        }
                        resolved.Function.parameters.deinit(self.allocator);
                        resolved.Function.parameters = new_parameters;
                    }

                    {
                        var new_defaults = std.AutoArrayHashMapUnmanaged(*o.ObjString, Value).empty;
                        var it = resolved.Function.defaults.iterator();
                        while (it.next()) |default| {
                            try new_defaults.put(
                                self.allocator,
                                try self.move(default.key_ptr.*),
                                try self.moveValue(default.value_ptr.*),
                            );
                        }
                        resolved.Function.defaults.deinit(self.allocator);
                        resolved.Function.defaults = new_defaults;
                    }

                    if (resolved.Function.error_types) |error_types| {
                        for (error_types, 0..) |error_item, i| {
                            resolved.Function.error_types.?[i] = try self.move(error_item);
                        }
                    }

                    {
                        var new_generic_types = std.AutoArrayHashMapUnmanaged(*o.ObjString, *o.ObjTypeDef).empty;
                        var it = resolved.Function.generic_types.iterator();
                        while (it.next()) |kv| {
                            try new_generic_types.put(
                                self.allocator,
                                try self.move(kv.key_ptr.*),
                                try self.move(kv.value_ptr.*),
                            );
                        }
                        resolved.Function.generic_types.deinit(self.allocator);
                        resolved.Function.generic_types = new_generic_types;
                    }
                } else if (resolved.* == .List) {
                    resolved.List.item_type = try self.move(resolved.List.item_type);
                    var it = resolved.List.methods.iterator();
                    while (it.next()) |method| {
                        method.value_ptr.*.type_def = try self.move(method.value_ptr.*.type_def);
                    }
                } else if (resolved.* == .Map) {
                    resolved.Map.key_type = try self.move(resolved.Map.key_type);
                    resolved.Map.value_type = try self.move(resolved.Map.value_type);
                    var it = resolved.Map.methods.iterator();
                    while (it.next()) |method| {
                        method.value_ptr.*.type_def = try self.move(method.value_ptr.*.type_def);
                    }
                } else if (resolved.* == .Fiber) {
                    resolved.Fiber.return_type = try self.move(resolved.Fiber.return_type);
                    resolved.Fiber.yield_type = try self.move(resolved.Fiber.yield_type);
                } else if (resolved.* == .Placeholder) {
                    // unreachable;
                } else if (resolved.* == .ForeignContainer) {
                    resolved.ForeignContainer.name = try self.move(resolved.ForeignContainer.name);
                    resolved.ForeignContainer.qualified_name = try self.move(resolved.ForeignContainer.qualified_name);
                    var it = resolved.ForeignContainer.buzz_type.iterator();
                    while (it.next()) |kv| {
                        kv.value_ptr.* = try self.move(kv.value_ptr.*);
                    }
                }
            }
        },
        o.ObjUpValue => {
            value.location.* = try self.moveValue(value.location.*);
            if (value.closed) |uclosed| {
                value.closed = try self.moveValue(uclosed);
            }
        },
        o.ObjClosure => {
            value.function = try self.move(value.function);
            for (value.upvalues, 0..) |upvalue, i| {
                value.upvalues[i] = try self.move(upvalue);
            }
            for (value.globals.items, 0..) |global, i| {
                value.globals.items[i] = try self.moveValue(global);
            }
        },
        o.ObjFunction => {
            value.type_def = try self.move(value.type_def);
            for (value.chunk.constants.items, 0..) |constant, i| {
                value.chunk.constants.items[i] = try self.moveValue(constant);
            }
        },
        o.ObjObjectInstance => {
            if (value.object) |object| {
                value.object = try self.move(object);
            }
            value.type_def = try self.move(value.type_def);
            for (value.fields, 0..) |field, i| {
                value.fields[i] = try self.moveValue(field);
            }
        },
        o.ObjObject => {
            value.type_def = try self.move(value.type_def);

            for (value.fields, 0..) |field, i| {
                value.fields[i] = try self.moveValue(field);
            }

            for (value.defaults, 0..) |field_opt, i| {
                if (field_opt) |field| {
                    value.defaults[i] = try self.moveValue(field);
                }
            }
        },
        o.ObjList => {
            for (value.items.items, 0..) |item, i| {
                value.items.items[i] = try self.moveValue(item);
            }
            value.type_def = try self.move(value.type_def);

            for (value.methods, 0..) |method_opt, i| {
                if (method_opt) |method| {
                    value.methods[i] = try self.move(method);
                }
            }
        },
        o.ObjMap => {
            var new_map = std.AutoArrayHashMapUnmanaged(Value, Value).empty;
            var it = value.map.iterator();
            while (it.next()) |kv| {
                try new_map.put(
                    self.allocator,
                    try self.moveValue(kv.key_ptr.*),
                    try self.moveValue(kv.value_ptr.*),
                );
            }
            value.map.deinit(self.allocator);
            value.map = new_map;

            for (value.methods, 0..) |method_opt, i| {
                if (method_opt) |method| {
                    value.methods[i] = try self.move(method);
                }
            }

            value.type_def = try self.move(value.type_def);
        },
        o.ObjEnum => {
            value.type_def = try self.move(value.type_def);
            for (value.cases, 0..) |case, i| {
                value.cases[i] = try self.moveValue(case);
            }
        },
        o.ObjEnumInstance => value.enum_ref = try self.move(value.enum_ref),
        o.ObjBoundMethod => {
            value.receiver = try self.moveValue(value.receiver);
            if (value.closure) |closure| {
                value.closure = try self.move(closure);
            }
            if (value.native) |native| {
                value.native = try self.move(native);
            }
        },
        // FIXME: moveFiber will move its parent fibers too, do we wan't that?
        o.ObjFiber => try self.moveFiber(value.fiber),
        o.ObjForeignContainer => value.type_def = try self.move(value.type_def),
        else => unreachable,
    }
}

fn moveFiber(self: *GC, fiber: *v.Fiber) error{OutOfSpace}!void {
    var current_fiber: ?*v.Fiber = fiber;
    while (current_fiber) |ufiber| {
        ufiber.type_def = try self.move(ufiber.type_def);

        // Move main fiber
        var i: [*]Value = @ptrCast(fiber.stack);
        while (@intFromPtr(i) < @intFromPtr(fiber.stack_top)) : (i += 1) {
            i[0] = try self.moveValue(i[0]);
        }

        // Move closure
        for (fiber.frames.items) |*frame| {
            frame.closure = try self.move(frame.closure);
            if (frame.error_value) |error_value| {
                frame.error_value = try self.moveValue(error_value);
            }
            if (frame.native_call_error_value) |error_value| {
                frame.native_call_error_value = try self.moveValue(error_value);
            }
        }

        // Move opened upvalues
        if (fiber.open_upvalues) |open_upvalues| {
            fiber.open_upvalues = try self.move(open_upvalues);
            var previous = fiber.open_upvalues;
            while (previous) |current| : (previous = current.next) {
                if (current.next) |next| {
                    current.next = try self.move(next);
                }
            }
        }

        current_fiber = ufiber.parent_fiber;
    }
}

fn moveRoots(self: *GC, vm: *v.VM) Space.Error!void {
    // FIXME: We should not need this, but we don't know how to prevent
    // collection before the VM actually starts making reference to them
    {
        var new_registry = std.AutoHashMapUnmanaged(
            TypeRegistry.TypeDefHash,
            *o.ObjTypeDef,
        ).empty;

        var it = self.type_registry.registry.iterator();
        while (it.next()) |kv| {
            try new_registry.put(
                self.allocator,
                kv.key_ptr.*,
                try self.move(kv.value_ptr.*),
            );
        }

        self.type_registry.registry.deinit(self.allocator);
        self.type_registry.registry = new_registry;
    }

    // Move basic types methods
    for (self.objfiber_members, 0..) |member, i| {
        if (member) |umember| {
            self.objfiber_members[i] = try self.move(umember);
        }
    }

    for (self.objfiber_memberDefs, 0..) |def, i| {
        if (def) |udef| {
            self.objfiber_memberDefs[i] = try self.move(udef);
        }
    }

    for (self.objrange_members, 0..) |member, i| {
        if (member) |umember| {
            self.objrange_members[i] = try self.move(umember);
        }
    }

    for (self.objrange_memberDefs, 0..) |def, i| {
        if (def) |udef| {
            self.objrange_memberDefs[i] = try self.move(udef);
        }
    }

    for (self.objstring_members, 0..) |member, i| {
        if (member) |umember| {
            self.objstring_members[i] = try self.move(umember);
        }
    }

    for (self.objstring_memberDefs, 0..) |def, i| {
        if (def) |udef| {
            self.objstring_memberDefs[i] = try self.move(udef);
        }
    }

    for (self.objpattern_members, 0..) |member, i| {
        if (member) |umember| {
            self.objpattern_members[i] = try self.move(umember);
        }
    }

    for (self.objpattern_memberDefs, 0..) |def, i| {
        if (def) |udef| {
            self.objpattern_memberDefs[i] = try self.move(udef);
        }
    }

    // Move special strings we always need
    if (self.strings.get("$")) |dollar| {
        try self.strings.put(
            self.allocator,
            "$",
            try self.move(dollar),
        );
    }

    // Move import registry
    var new_import_registry = v.ImportRegistry.empty;
    var it = vm.import_registry.iterator();
    while (it.next()) |kv| {
        var import_cache = std.ArrayList(Value).empty;

        for (kv.value_ptr.*) |value| {
            try import_cache.append(self.allocator, try self.moveValue(value));
        }

        try new_import_registry.put(
            self.allocator,
            try self.move(kv.key_ptr.*),
            try import_cache.toOwnedSlice(self.allocator),
        );
    }
    vm.import_registry.deinit(self.allocator);
    vm.import_registry.* = new_import_registry;

    // Move current fiber and its parent fibers
    try self.moveFiber(vm.current_fiber);

    // Move globals
    for (vm.globals.items, 0..) |global, i| {
        vm.globals.items[i] = try self.moveValue(global);
    }

    // Move ast constant values (some are only referenced by the JIT so might be collected before)
    for (vm.current_ast.nodes.items(.value), 0..) |value_opt, i| {
        if (value_opt) |value| {
            vm.current_ast.nodes.items(.value)[i] = try self.moveValue(value);
        }
    }
}

pub fn copyString(self: *GC, chars: []const u8) !*o.ObjString {
    if (self.strings.get(chars)) |interned| {
        return interned;
    }

    const copy: []u8 = try self.allocator.alloc(u8, chars.len);
    std.mem.copyForwards(u8, copy, chars);

    const string: *o.ObjString = try self.allocate(
        o.ObjString,
        o.ObjString{ .string = chars },
    );

    try self.strings.put(
        self.allocator,
        string.string,
        string,
    );

    return string;
}

const Space = struct {
    pub const Error = error{
        OutOfSpace,
        OutOfMemory,
    };

    /// Contiguous piece of memory used by the space
    memory: []u8,
    /// Next available slot address
    next: [*]u8,
    /// First header
    first_header: ?*o.Obj = null,
    /// Scanning current o.Obj
    current_header: ?*o.Obj = null,

    pub fn init(allocator: std.mem.Allocator, capacity: usize) Space.Error!Space {
        const memory = try allocator.alloc(u8, capacity);

        return .{
            .memory = memory,
            .next = @ptrCast(&memory[0]),
        };
    }

    pub fn deinit(self: *Space, allocator: std.mem.Allocator) void {
        allocator.free(self.memory);
    }

    pub fn reset(self: *Space) void {
        self.current_header = null;
        self.first_header = null;
        self.next = @ptrCast(&self.memory[0]);
    }

    pub fn isFull(self: *Space) bool {
        return @intFromPtr(self.next) > @intFromPtr(&self.memory[self.memory.len - 1]);
    }

    pub fn isValidObj(comptime T: type) void {
        if (!@hasField(T, "obj")) {
            @compileError("Type is missing header");
        }

        if (@FieldType(T, "obj") != o.Obj) {
            @compileError("o.Obj header is not of expected type o.Obj");
        }
    }

    pub fn occupiedBytes(self: *Space) usize {
        return @intFromPtr(self.next) - @intFromPtr(&self.memory[0]);
    }

    pub fn totalBytes(self: *Space) usize {
        return @intFromPtr(&self.memory[self.memory.len - 1]) - @intFromPtr(&self.memory[0]);
    }

    pub fn occupiedSpace(self: *Space) usize {
        const start = @intFromPtr(&self.memory[0]);
        const occupied = @intFromPtr(self.next - 1) - start;
        const end = @intFromPtr(&self.memory[self.memory.len - 1]) - start;

        return (occupied * end) / end;
    }

    pub fn hasSpaceFor(self: *Space, comptime T: type) bool {
        comptime {
            isValidObj(T);
        }

        return @intFromPtr(self.next + @sizeOf(T) - 1) <= @intFromPtr(&self.memory[self.memory.len - 1]);
    }

    pub fn allocate(self: *Space, value: anytype) error{OutOfSpace}!*@TypeOf(value) {
        const T = @TypeOf(value);

        comptime {
            isValidObj(T);
        }

        // Not enough space left, we fail here, the GC will allocate a new larger Space and make a sweep using it as a new to-space
        if (@intFromPtr(self.next + @sizeOf(T) - 1) > @intFromPtr(&self.memory[self.memory.len - 1])) {
            return error.OutOfSpace;
        }

        // TODO: compute next -> end length
        if (std.mem.alignInBytes(self.next[0..], @alignOf(T))) |slot| {
            // Copy the value into the Space's memory
            @memcpy(slot, std.mem.toBytes(value)[0..]);

            const copy: *T = @alignCast(
                std.mem.bytesAsValue(
                    T,
                    self.next[0..@sizeOf(T)],
                ),
            );

            // Link to the current_header and replace it
            if (self.current_header) |current_header| {
                current_header.next = &copy.obj;
                self.current_header = &copy.obj;
            } else {
                self.current_header = &copy.obj;
                self.first_header = &copy.obj;
            }

            // Move cursor forward
            self.next += @sizeOf(T);

            if (BuildOptions.gc_debug) {
                std.log.debug(
                    "Allocated `{s}` for {} bytes if Space {*}",
                    .{
                        @typeName(T),
                        @sizeOf(T),
                        self,
                    },
                );
            }

            return copy;
        }

        return Error.OutOfSpace;
    }
};
