const std = @import("std");
const v = @import("vm.zig");
const Value = @import("value.zig").Value;
const o = @import("obj.zig");
const TypeRegistry = @import("TypeRegistry.zig");
const BuildOptions = @import("build_options");

/// Cheney's semi-space GC
const GC = @This();

allocator: std.mem.Allocator,
from: Space,
to: Space,

strings: std.StringHashMapUnmanaged(*o.ObjString) = .empty,
type_registry: TypeRegistry,

pub fn init(allocator: std.mem.Allocator, capacity: usize) error{OutOfMemory}!GC {
    var self = .{
        .allocator = allocator,
        .type_registry = undefined,
        .from = try Space.init(allocator, capacity),
        .to = try Space.init(allocator, capacity),
    };

    self.type_registry = try TypeRegistry.init(&self);

    return self;
}

pub fn allocate(self: *GC, vm: *v.VM, comptime T: type, value: T) Space.Error!*T {
    if (self.from.hasSpaceFor(T)) {
        return self.from.allocate(T, value);
    } else {
        self.collect(vm);

        // Still no space left, we need to resize the spaces
        if (!self.from.hasSpaceFor(T)) {
            try self.resize(self.to.memory.len * BuildOptions.next_gc_ratio);
        }

        // Try again
        return self.allocate(T, value);
    }
}

pub fn collect(self: *GC, vm: *v.VM) error{OutOfSpace}!void {
    self.to.reset();

    self.moveRoots(vm);

    // Now scan from there
    var scan_header: ?*Header = self.to.first_header.?;
    while (scan_header) |header| : (scan_header = scan_header.next) {
        try switch (header.type) {
            .String => self.scan(@as(o.ObjString, @fieldParentPtr("obj", header))),
            .Type => self.scan(@as(o.ObjTypeDef, @fieldParentPtr("obj", header))),
            .UpValue => self.scan(@as(o.ObjUpValue, @fieldParentPtr("obj", header))),
            .Closure => self.scan(@as(o.ObjClosure, @fieldParentPtr("obj", header))),
            .Function => self.scan(@as(o.ObjFunction, @fieldParentPtr("obj", header))),
            .ObjectInstance => self.scan(@as(o.ObjObjectInstance, @fieldParentPtr("obj", header))),
            .Object => self.scan(@as(o.ObjObject, @fieldParentPtr("obj", header))),
            .List => self.scan(@as(o.ObjList, @fieldParentPtr("obj", header))),
            .Map => self.scan(@as(o.ObjMap, @fieldParentPtr("obj", header))),
            .Enum => self.scan(@as(o.ObjEnum, @fieldParentPtr("obj", header))),
            .EnumInstance => self.scan(@as(o.ObjEnumInstance, @fieldParentPtr("obj", header))),
            .Bound => self.scan(@as(o.ObjBoundMethod, @fieldParentPtr("obj", header))),
            .Native => self.scan(@as(o.ObjNative, @fieldParentPtr("obj", header))),
            .UserData => self.scan(@as(o.ObjUserData, @fieldParentPtr("obj", header))),
            .Pattern => self.scan(@as(o.ObjPattern, @fieldParentPtr("obj", header))),
            .Fiber => self.scan(@as(o.ObjFiber, @fieldParentPtr("obj", header))),
            .ForeignContainer => self.scan(@as(o.ObjForeignContainer, @fieldParentPtr("obj", header))),
            .Range => self.scan(@as(o.ObjRange, @fieldParentPtr("obj", header))),
        };
    }

    // Fixed interned strings ptr
    var new_strings = std.StringHashMapUnmanaged(*o.ObjString).empty;
    var it = self.strings.iterator();
    while (it.next()) |kv| {
        try new_strings.put(
            kv.key_ptr.*,
            if (kv.value_ptr.*.obj.forward) |forward|
                @fieldParentPtr("obj", forward)
            else
                kv.value_ptr.*,
        );
    }
    self.strings.deinit(self.allocator);
    self.strings = new_strings;

    // Now we switch spaces
    const tmp = self.to;
    self.to = self.from;
    self.from = tmp;

    // If spaces are too empty, shrink them
    if (self.from.occupiedSpace() < BuildOptions.shrink_gc_ratio) {
        self.resize(
            vm,
            self.from.occupiedBytes() * BuildOptions.next_gc_ratio,
        );
    }
}

fn resize(self: *GC, vm: *v.VM, size: usize) error{ OutOfMemory, OutOfSpace }!void {
    const new = try Space.init(self.allocator, size);

    self.to.deinit(self.allocator);
    self.to = new;

    // Now do a collect, it will move every live object from the old from-space to the new to-space and then switch them
    try self.collect(vm);

    // Now replace the to-space (because the previous new space is now the from-space) with a new space
    const new_to = try Space.init(self.allocator, size);
    self.to = new_to;
}

/// Move value from -> to and returns the new header ptr, if already moved, returns the forward ptr
fn move(self: *GC, value: anytype) error{OutOfSpace}!@TypeOf(value) {
    const T = @typeInfo(@TypeOf(value)).pointer.child;

    // If it was already moved, return the forward ptr
    if (value.obj.forward) |forward| {
        return forward;
    }

    // Copy value to to-space
    const copy = try self.to.allocate(T, value.*);

    // In old value header, set forward address
    value.obj.forward = &copy.obj;

    return copy;
}

/// Move value and return a new one pointing to the eventually moved Obj
fn moveValue(self: *GC, value: Value) error{OutOfSpace}!Value {
    if (value.isObj()) {
        const obj = value.obj();
        return Value.fromObj(
            try switch (obj.obj_type) {
                .String => self.move(@as(o.ObjString, @fieldParentPtr("obj", obj))).toObj(),
                .Type => self.move(@as(o.ObjTypeDef, @fieldParentPtr("obj", obj))).toObj(),
                .UpValue => self.move(@as(o.ObjUpValue, @fieldParentPtr("obj", obj))).toObj(),
                .Closure => self.move(@as(o.ObjClosure, @fieldParentPtr("obj", obj))).toObj(),
                .Function => self.move(@as(o.ObjFunction, @fieldParentPtr("obj", obj))).toObj(),
                .ObjectInstance => self.move(@as(o.ObjObjectInstance, @fieldParentPtr("obj", obj))).toObj(),
                .Object => self.move(@as(o.ObjObject, @fieldParentPtr("obj", obj))).toObj(),
                .List => self.move(@as(o.ObjList, @fieldParentPtr("obj", obj))).toObj(),
                .Map => self.move(@as(o.ObjMap, @fieldParentPtr("obj", obj))).toObj(),
                .Enum => self.move(@as(o.ObjEnum, @fieldParentPtr("obj", obj))).toObj(),
                .EnumInstance => self.move(@as(o.ObjEnumInstance, @fieldParentPtr("obj", obj))).toObj(),
                .Bound => self.move(@as(o.ObjBoundMethod, @fieldParentPtr("obj", obj))).toObj(),
                .Native => self.move(@as(o.ObjNative, @fieldParentPtr("obj", obj))).toObj(),
                .UserData => self.move(@as(o.ObjUserData, @fieldParentPtr("obj", obj))).toObj(),
                .Pattern => self.move(@as(o.ObjPattern, @fieldParentPtr("obj", obj))).toObj(),
                .Fiber => self.move(@as(o.ObjFiber, @fieldParentPtr("obj", obj))).toObj(),
                .ForeignContainer => self.move(@as(o.ObjForeignContainer, @fieldParentPtr("obj", obj))).toObj(),
                .Range => self.move(@as(o.ObjRange, @fieldParentPtr("obj", obj))).toObj(),
            },
        );
    }

    return value;
}

fn scan(self: *GC, value: anytype) error{OutOfSpace}!void {
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
                    resolved.ObjectInstance.of = @fieldParentPtr(
                        "obj",
                        try self.move(resolved.ObjectInstance.of),
                    );
                } else if (resolved.* == .EnumInstance) {
                    resolved.EnumInstance.of = @fieldParentPtr(
                        "obj",
                        try self.move(resolved.EnumInstance.of),
                    );
                } else if (resolved.* == .Object) {
                    resolved.Object.name = @fieldParentPtr(
                        "obj",
                        try self.move(resolved.Object.name),
                    );
                    resolved.Object.qualified_name = @fieldParentPtr(
                        "obj",
                        try self.move(resolved.Object.qualified_name),
                    );

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
                                self.allocataor,
                                try self.move(default.key_ptr.*),
                                try self.move(default.value_ptr.*),
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

            for (value.methods) |method_opt| {
                if (method_opt) |method| {
                    value.methods = try self.move(method);
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
            i = try self.moveValue(i[0]);
        }

        // Move closure
        for (fiber.frames.items) |frame| {
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
            var previous = fiber.open_upvalues.?;
            while (previous.next) |next| : (previous = previous.next) {
                previous.next = try self.move(next);
            }
        }

        current_fiber = ufiber.parent_fiber;
    }
}

fn moveRoots(self: *GC, vm: *v.VM) error{OutOfMemory}!void {
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
                self.move(o.ObjTypeDef, kv.value_ptr.*),
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
        try self.strings.put("$", try self.move(dollar));
    }

    // Move import registry
    var new_import_registry = v.ImportRegistry.empty;
    var it = vm.import_registry.iterator();
    while (it.next()) |kv| {
        var import_cache = std.ArrayList(Value).empty;

        for (kv.value_ptr.*) |value| {
            try import_cache.append(try self.moveValue(value));
        }

        try new_import_registry.put(
            self.allocator,
            try self.move(kv.key_ptr.*),
            import_cache.toOwnedSlice(self.allocator),
        );
    }
    vm.import_registry.deinit(self.allocator);
    vm.import_registry = new_import_registry;

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

pub const Header = struct {
    /// Obj type
    type: o.ObjType,
    /// Next obj in the space
    next: ?*Header = null,
    /// When obj is move to to-space, we leave a forward address in the form-space
    forward: ?*Header = null,
};

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
    first_header: ?*Header = null,
    /// Scanning current Header
    current_header: ?*Header = null,

    pub fn init(allocator: std.mem.Allocator, capacity: usize) error{OutOfMemory}!Space {
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

        if (@FieldType(T, "obj") != Header) {
            @compileError("Obj header is not of expected type Header");
        }
    }

    pub fn occupiedBytes(self: *Space) usize {
        return @intFromPtr(self.next - 1) - @intFromPtr(&self.memory[0]);
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

        return @intFromPtr(self.next + @sizeOf(T) - 1) > @intFromPtr(&self.memory[self.memory.len - 1]);
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

        // Copy the value into the Space's memory
        @memcpy(
            self.next[0..@sizeOf(T)],
            std.mem.toBytes(value)[0..],
        );

        const copy: *T = @ptrCast(@alignCast(self.next));

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

        return copy;
    }
};

test "Space" {
    const Obj = struct {
        obj: Header = .{
            .type = o.ObjType.String,
        },
        name: []const u8,
        age: u8,
    };

    var space = try Space.init(
        std.testing.allocator,
        @sizeOf(Obj) * 2, // Space for 2
    );
    defer space.deinit(std.testing.allocator);

    try std.testing.expect(!space.isFull());

    const allocated_a = try space.allocate(
        Obj{
            .name = "hello",
            .age = 20,
        },
    );

    const allocated_b = try space.allocate(
        Obj{
            .name = "bye",
            .age = 21,
        },
    );

    // a should be linked to b
    try std.testing.expect(allocated_a.obj.next != null);
    try std.testing.expectEqual(allocated_a.obj.next.?, &allocated_b.obj);

    // Next should now be base + 2 * sizeof(Obj)
    try std.testing.expectEqual(
        @as([*]u8, @ptrCast(&space.memory[0])) + (@sizeOf(Obj) * 2),
        space.next,
    );

    // Now full
    try std.testing.expect(space.isFull());

    // Adding more to it should fail
    try std.testing.expectError(
        Space.Error.OutOfSpace,
        space.allocate(
            Obj,
            .{
                .name = "boom",
                .age = 99,
            },
        ),
    );
}
