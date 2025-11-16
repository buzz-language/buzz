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
const TypeRegistry = @import("TypeRegistry.zig");
const p = @import("pool.zig");
const Pool = p.Pool;
const MultiPool = p.MultiPool;

// Sticky Mark Bits Generational GC basic idea:
// 1. First GC: do a normal mark, don't clear `marked` at the end
// 2. Young GC:
//     - Already marked objects are 'old', don't trace them (because all referenced object from it are also old and marked)and don't clear them
//     - Write barrier: anytime an object is modified, mark it as 'dirty' and add it to the gray_stack so its traced
//     - Old but dirty object is traced, then marked clean again (because any reference to 'young' objects will be 'old' after this gc sweep)
// 3. Old GC:
//     - Trace only already marked objects
// 4. Full GC:
//     - Unmark all objects
//     - Do a mark and sweep
//
// For now we only have either young or old objects but we could improve on it with more categories with each its threshold
const GC = @This();

const Mode = enum {
    Young,
    // Old,
    Full,
};

allocator: std.mem.Allocator,

pools: MultiPool(
    &.{
        o.ObjString,
        o.ObjTypeDef,
        o.ObjUpValue,
        o.ObjClosure,
        o.ObjFunction,
        o.ObjObjectInstance,
        o.ObjObject,
        o.ObjList,
        o.ObjMap,
        o.ObjEnum,
        o.ObjEnumInstance,
        o.ObjBoundMethod,
        o.ObjNative,
        o.ObjUserData,
        o.ObjPattern,
        o.ObjFiber,
        o.ObjForeignContainer,
        o.ObjRange,
    },
) = .empty,

strings: std.StringHashMapUnmanaged(Pool(o.ObjString).Idx) = .empty,
type_registry: TypeRegistry,
bytes_allocated: usize = 0,
// next_gc == next_full_gc at first so the first cycle is a full gc
next_gc: usize = if (builtin.mode == .Debug) 1024 else 1024 * BuildOptions.initial_gc,
next_full_gc: usize = if (builtin.mode == .Debug) 1024 else 1024 * BuildOptions.initial_gc,
last_gc: ?Mode = null,
gray_stack: std.ArrayList(o.ObjIdx) = .empty,
active_vms: std.AutoHashMapUnmanaged(*v.VM, void) = .empty,
// o.Obj being collected, useful to avoid setting object instance dirty while running its collector method
obj_collected: ?o.ObjIdx = null,

where: ?Token = null,

// Types we generaly don't wan't to ever be collected
objfiber_members: []?Pool(o.ObjNative).Idx,
objfiber_memberDefs: []?Pool(o.ObjTypeDef).Idx,
objpattern_members: []?Pool(o.ObjNative).Idx,
objpattern_memberDefs: []?Pool(o.ObjTypeDef).Idx,
objstring_members: []?Pool(o.ObjNative).Idx,
objstring_memberDefs: []?Pool(o.ObjTypeDef).Idx,
objrange_memberDefs: []?Pool(o.ObjTypeDef).Idx,
objrange_members: []?Pool(o.ObjNative).Idx,

full_collection_count: usize = 0,
light_collection_count: usize = 0,
max_allocated: usize = 0,

gc_time: usize = 0,

pub fn init(allocator: std.mem.Allocator) !GC {
    const self = GC{
        .allocator = allocator,
        .type_registry = undefined,
        // FIXME: we actually know the length of those at compile time so those could be arrays
        .objfiber_members = try allocator.alloc(?Pool(o.ObjNative).Idx, o.ObjFiber.members.len),
        .objfiber_memberDefs = try allocator.alloc(?Pool(o.ObjTypeDef).Idx, o.ObjFiber.members.len),
        .objpattern_members = try allocator.alloc(?Pool(o.ObjNative).Idx, o.ObjPattern.members.len),
        .objpattern_memberDefs = try allocator.alloc(?Pool(o.ObjTypeDef).Idx, o.ObjPattern.members.len),
        .objstring_members = try allocator.alloc(?Pool(o.ObjNative).Idx, o.ObjString.members.len),
        .objstring_memberDefs = try allocator.alloc(?Pool(o.ObjTypeDef).Idx, o.ObjString.members.len),
        .objrange_members = try allocator.alloc(?Pool(o.ObjNative).Idx, o.ObjRange.members.len),
        .objrange_memberDefs = try allocator.alloc(?Pool(o.ObjTypeDef).Idx, o.ObjRange.members.len),
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

    return self;
}

pub fn registerVM(self: *GC, vm: *v.VM) !void {
    try self.active_vms.put(
        self.allocator,
        vm,
        {},
    );
}

pub fn unregisterVM(self: *GC, vm: *v.VM) void {
    _ = self.active_vms.remove(vm);
}

pub fn deinit(self: *GC) void {
    self.pools.deinit(self.allocator);

    self.gray_stack.deinit(self.allocator);
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

pub fn get(self: *GC, comptime T: type, index: Pool(T).Idx) ?*T {
    return self.pools.get(T, index);
}

pub fn getObj(self: *GC, obj_idx: o.ObjIdx) ?*o.Obj {
    return switch (obj_idx.obj_type) {
        .Range => if (self.get(o.ObjRange, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .String => if (self.get(o.ObjString, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .Pattern => if (self.get(o.ObjPattern, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .Fiber => if (self.get(o.ObjFiber, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .Type => if (self.get(o.ObjTypeDef, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .Object => if (self.get(o.ObjObject, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .Enum => if (self.get(o.ObjEnum, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .ObjectInstance => if (self.get(o.ObjObjectInstance, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .EnumInstance => if (self.get(o.ObjEnumInstance, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .Function => if (self.get(o.ObjFunction, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .UpValue => if (self.get(o.ObjUpValue, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .Closure => if (self.get(o.ObjClosure, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .List => if (self.get(o.ObjList, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .Map => if (self.get(o.ObjMap, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .Bound => if (self.get(o.ObjBoundMethod, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .ForeignContainer => if (self.get(o.ObjForeignContainer, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .UserData => if (self.get(o.ObjUserData, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
        .Native => if (self.get(o.ObjNative, .idx(obj_idx.index))) |instance|
            instance.toObj()
        else
            null,
    };
}

pub fn allocate(self: *GC, comptime T: type) !Pool(T).Idx {
    var timer = if (!is_wasm) std.time.Timer.start() catch unreachable else {};

    self.bytes_allocated += @sizeOf(T);

    if (self.bytes_allocated > self.max_allocated) {
        self.max_allocated = self.bytes_allocated;
    }

    if (self.bytes_allocated > self.next_gc and BuildOptions.gc) {
        try self.collectGarbage();
    }

    if (BuildOptions.memory_limit != null and self.bytes_allocated > BuildOptions.memory_limit.?) {
        return error.ReachedMaximumMemoryUsage;
    }

    const allocated = try self.pools.append(self.allocator, T, undefined);

    if (BuildOptions.gc_debug) {
        std.log.info(
            "Allocated @{} {} for {B} (now {B}/{B})",
            .{
                @intFromPtr(allocated),
                T,
                @sizeOf(T),
                self.bytes_allocated,
                self.next_gc,
            },
        );
    }

    if (!is_wasm) {
        self.gc_time += timer.read();
    }
    return allocated;
}

pub fn allocateMany(self: *GC, comptime T: type, count: usize) ![]T {
    var timer = if (!is_wasm)
        std.time.Timer.start() catch unreachable
    else {};

    self.bytes_allocated += (@sizeOf(T) * count);

    if (self.bytes_allocated > self.max_allocated) {
        self.max_allocated = self.bytes_allocated;
    }

    if (self.bytes_allocated > self.next_gc and BuildOptions.gc) {
        try self.collectGarbage();
    }

    if (!is_wasm) {
        self.gc_time += timer.read();
    }
    return try self.allocator.alloc(T, count);
}

pub fn allocateObject(self: *GC, data: anytype) !Pool(@TypeOf(data)).Idx {
    const T = @TypeOf(data);

    self.bytes_allocated += @sizeOf(T);

    if (self.bytes_allocated > self.max_allocated) {
        self.max_allocated = self.bytes_allocated;
    }

    if (self.bytes_allocated > self.next_gc and BuildOptions.gc) {
        try self.collectGarbage();
    }

    if (BuildOptions.memory_limit != null and self.bytes_allocated > BuildOptions.memory_limit.?) {
        return error.ReachedMaximumMemoryUsage;
    }

    const idx = try self.pools.append(
        self.allocator,
        T,
        data,
    );

    // Add new object at start of vm.objects linked list
    // self.objects.append(
    //     &self.get(T, idx).node,
    // );

    return idx;
}

pub fn allocateString(self: *GC, chars: []const u8) !Pool(o.ObjString).Idx {
    const idx = try allocateObject(
        self,
        o.ObjString{ .string = chars },
    );

    const string = self.get(o.ObjString, idx).?;

    try self.strings.put(
        self.allocator,
        string.string,
        idx,
    );

    return idx;
}

pub fn copyString(self: *GC, chars: []const u8) !Pool(o.ObjString).Idx {
    if (self.strings.get(chars)) |interned| {
        return interned;
    }

    const copy: []u8 = try self.allocateMany(u8, chars.len);
    std.mem.copyForwards(u8, copy, chars);

    if (BuildOptions.gc_debug) {
        std.log.info("Allocated slice {*} `{s}`", .{ copy, copy });
    }

    return try allocateString(self, copy);
}

fn free(self: *GC, comptime T: type, index: Pool(T).Idx) !void {
    var timer = if (!is_wasm) std.time.Timer.start() catch unreachable else {};

    if (BuildOptions.gc_debug) {
        std.log.info("Going to free {s}.{}", .{ @typeName(T), index });
    }

    self.bytes_allocated -= @sizeOf(T);
    try self.pools.remove(self.allocator, T, index);

    if (BuildOptions.gc_debug) {
        std.log.info(
            "(from {B}), collected {B}, {B} allocated",
            .{
                self.bytes_allocated + @sizeOf(T),
                @sizeOf(T),
                self.bytes_allocated,
            },
        );
    }

    if (!is_wasm) {
        self.gc_time += timer.read();
    }
}

fn freeObj(self: *GC, comptime T: type, index: Pool(T).Idx) (std.mem.Allocator.Error || std.fmt.BufPrintError)!void {
    if (BuildOptions.gc_debug) {
        std.log.info(
            ">> freeing {} {}",
            .{
                index,
                @typeName(T),
            },
        );
    }

    const instance = self.get(T, index).?;
    self.obj_collected = .{
        .index = index.index,
        .obj_type = instance.obj.obj_type,
    };
    defer self.obj_collected = null;

    switch (T) {
        o.ObjString => {
            // Remove it from interned strings
            _ = self.strings.remove(instance.string);
            freeMany(self, u8, instance.string);
        },
        o.ObjPattern => if (!is_wasm) {
            instance.pattern.free();
        },
        o.ObjTypeDef => {
            const hash = TypeRegistry.typeDefHash(self, instance.*);

            if (self.type_registry.registry.get(hash)) |registered_obj| {
                if (registered_obj.index == index.index) {
                    _ = self.type_registry.registry.remove(hash);
                    if (BuildOptions.gc_debug) {
                        std.log.info(
                            "Removed registered type @{} #{} `{s}`",
                            .{
                                @intFromPtr(registered_obj),
                                hash,
                                instance.toStringAlloc(self.allocator, true) catch unreachable,
                            },
                        );
                    }
                } else {
                    // std.log.info(
                    //     "ObjTypeDef {*} `{s}` was allocated outside of type registry\n",
                    //     .{
                    //         instance,
                    //         try instance.toStringAlloc(self.allocator),
                    //     },
                    // );
                    // unreachable;
                    // FIXME: this should not occur. Right now this because of the way we resolve placeholders by changing their content and replacing the type in the typ registry
                    // Previously registered same type is now outside of the type registry and is collected.
                    return;
                }
            }

            instance.deinit();
        },
        o.ObjUpValue => if (instance.closed) |value| {
            if (value.isObj()) {
                try self.freeObj(o.ObjUpValue, .{ .index = value.obj().index });
            }
        },
        o.ObjClosure => instance.deinit(self.allocator),
        o.ObjFunction => instance.deinit(),
        o.ObjObjectInstance => {
            // Calling eventual destructor method
            if (instance.object) |object_idx| {
                const object = self.get(o.ObjObject, object_idx).?;
                const type_def = self.get(o.ObjTypeDef, object.type_def).?;
                if (type_def.resolved_type.?.Object.fields.get("collect")) |field| {
                    if (field.method and !field.static) {
                        buzz_api.bz_invoke(
                            instance.vm,
                            Value.fromObj(.{ .index = index.index, .obj_type = .ObjectInstance }),
                            field.index,
                            null,
                            0,
                            null,
                        );

                        // Remove void result of the collect call
                        _ = instance.vm.pop();
                    }
                }
            }
            instance.deinit(self.allocator);
        },
        o.ObjObject => instance.deinit(self.allocator),
        o.ObjList => instance.deinit(self.allocator),
        o.ObjMap => instance.deinit(self.allocator),
        o.ObjEnum => self.allocator.free(instance.cases),
        o.ObjFiber => {
            instance.fiber.deinit();
            self.allocator.destroy(instance.fiber);
        },
        o.ObjForeignContainer => self.freeMany(u8, instance.data),
        o.ObjEnumInstance,
        o.ObjBoundMethod,
        o.ObjNative,
        o.ObjUserData,
        o.ObjRange,
        => {},
        else => std.debug.assert(false),
    }

    try free(self, T, index);
}

fn freeMany(self: *GC, comptime T: type, pointer: []const T) void {
    var timer = if (!is_wasm) std.time.Timer.start() catch unreachable else {};

    if (BuildOptions.gc_debug) {
        std.log.info("Going to free slice {*} `{s}`", .{ pointer, pointer });
    }

    const n: usize = (@sizeOf(T) * pointer.len);
    self.bytes_allocated -= n;
    self.allocator.free(pointer);

    if (BuildOptions.gc_debug) {
        std.log.info(
            "(from {B}), collected {B}, {B} allocated",
            .{
                self.bytes_allocated + n,
                n,
                self.bytes_allocated,
            },
        );
    }

    if (!is_wasm) {
        self.gc_time += timer.read();
    }
}

pub fn markObjDirty(self: *GC, comptime T: type, idx: Pool(T).Idx) !void {
    const instance = self.get(T, idx);
    if (!instance.obj.dirty and
        self.obj_collected == null or
        (self.obj_collected.?.obj_type != instance.obj.obj_type and self.obj_collected.?.index != idx.index))
    {
        instance.obj.dirty = true;

        // A dirty obj is: an old object with reference to potential young objects that will need to be marked
        // Since old object are ignored when tracing references, this will force tracing for it
        try self.gray_stack.append(
            self.allocator,
            .{ .obj_type = instance.obj.obj_type, .idx = idx },
        );
    }
}

pub fn markObj(self: *GC, comptime T: type, idx: Pool(T).Idx) !void {
    if (self.get(T, idx)) |instance| {
        if (instance.obj.marked or
            (self.obj_collected != null and
                self.obj_collected.?.obj_type == instance.obj.obj_type and
                self.obj_collected.?.index == idx.index))
        {
            if (BuildOptions.gc_debug) {
                std.log.info(
                    "{} {s} already marked or old",
                    .{
                        idx,
                        try instance.toValue().toStringAlloc(self.allocator),
                    },
                );
            }
            return;
        }

        if (BuildOptions.gc_debug) {
            std.log.info(
                "marking {}: `{s}`",
                .{
                    idx,
                    try instance.toValue().toStringAlloc(self.allocator),
                },
            );
        }

        instance.obj.marked = true;

        // Move marked obj to tail so the sweeping can stop going through objects when finding the first marked object
        // self.objects.remove(&instance.obj.node);
        // // Just to be safe, reset node before inserting it again
        // instance.obj.node = .{
        //     .prev = null,
        //     .next = null,
        // };
        // self.objects.append(&instance.obj.node);

        try self.gray_stack.append(
            self.allocator,
            .{ .obj_type = instance.obj.obj_type, .index = idx.index },
        );
    }
}

fn blackenObject(self: *GC, obj_idx: o.ObjIdx) !void {
    const obj = self.getObj(obj_idx).?;

    if (BuildOptions.gc_debug) {
        std.log.info(
            "blackening @{} {}",
            .{
                obj_idx.index,
                obj_idx.obj_type,
            },
        );
    }

    obj.dirty = false;

    try switch (obj.obj_type) {
        .String => self.get(o.ObjString, .{ .index = obj_idx.index }).?.mark(self),
        .Pattern => self.get(o.ObjPattern, .{ .index = obj_idx.index }).?.mark(self),
        .UserData => self.get(o.ObjUserData, .{ .index = obj_idx.index }).?.mark(self),
        .Native => self.get(o.ObjNative, .{ .index = obj_idx.index }).?.mark(self),
        .Range => self.get(o.ObjRange, .{ .index = obj_idx.index }).?.mark(self),
        .Type => self.get(o.ObjTypeDef, .{ .index = obj_idx.index }).?.mark(self),
        .UpValue => self.get(o.ObjUpValue, .{ .index = obj_idx.index }).?.mark(self),
        .Closure => self.get(o.ObjClosure, .{ .index = obj_idx.index }).?.mark(self),
        .Function => self.get(o.ObjFunction, .{ .index = obj_idx.index }).?.mark(self),
        .ObjectInstance => self.get(o.ObjObjectInstance, .{ .index = obj_idx.index }).?.mark(self),
        .Object => self.get(o.ObjObject, .{ .index = obj_idx.index }).?.mark(self),
        .List => self.get(o.ObjList, .{ .index = obj_idx.index }).?.mark(self),
        .Map => self.get(o.ObjMap, .{ .index = obj_idx.index }).?.mark(self),
        .Enum => self.get(o.ObjEnum, .{ .index = obj_idx.index }).?.mark(self),
        .EnumInstance => self.get(o.ObjEnumInstance, .{ .index = obj_idx.index }).?.mark(self),
        .Bound => self.get(o.ObjBoundMethod, .{ .index = obj_idx.index }).?.mark(self),
        .Fiber => self.get(o.ObjFiber, .{ .index = obj_idx.index }).?.mark(self),
        .ForeignContainer => self.get(o.ObjForeignContainer, .{ .index = obj_idx.index }).?.mark(self),
    };

    if (BuildOptions.gc_debug) {
        std.log.info(
            "done blackening @{} {}",
            .{
                obj_idx.index,
                obj_idx.obj_type,
            },
        );
    }
}

pub fn markValue(self: *GC, value: Value) !void {
    if (value.isObj()) {
        const obj_idx = value.obj();
        try switch (obj_idx.obj_type) {
            .String => self.markObj(o.ObjString, .{ .index = obj_idx.index }),
            .Pattern => self.markObj(o.ObjPattern, .{ .index = obj_idx.index }),
            .UserData => self.markObj(o.ObjUserData, .{ .index = obj_idx.index }),
            .Native => self.markObj(o.ObjNative, .{ .index = obj_idx.index }),
            .Range => self.markObj(o.ObjRange, .{ .index = obj_idx.index }),
            .Type => self.markObj(o.ObjTypeDef, .{ .index = obj_idx.index }),
            .UpValue => self.markObj(o.ObjUpValue, .{ .index = obj_idx.index }),
            .Closure => self.markObj(o.ObjClosure, .{ .index = obj_idx.index }),
            .Function => self.markObj(o.ObjFunction, .{ .index = obj_idx.index }),
            .ObjectInstance => self.markObj(o.ObjObjectInstance, .{ .index = obj_idx.index }),
            .Object => self.markObj(o.ObjObject, .{ .index = obj_idx.index }),
            .List => self.markObj(o.ObjList, .{ .index = obj_idx.index }),
            .Map => self.markObj(o.ObjMap, .{ .index = obj_idx.index }),
            .Enum => self.markObj(o.ObjEnum, .{ .index = obj_idx.index }),
            .EnumInstance => self.markObj(o.ObjEnumInstance, .{ .index = obj_idx.index }),
            .Bound => self.markObj(o.ObjBoundMethod, .{ .index = obj_idx.index }),
            .Fiber => self.markObj(o.ObjFiber, .{ .index = obj_idx.index }),
            .ForeignContainer => self.markObj(o.ObjForeignContainer, .{ .index = obj_idx.index }),
        };
    }
}

pub fn markFiber(self: *GC, fiber: *v.Fiber) !void {
    var current_fiber: ?*v.Fiber = fiber;
    while (current_fiber) |ufiber| {
        try self.markObj(o.ObjTypeDef, ufiber.type_def);
        // Mark main fiber
        if (BuildOptions.gc_debug) {
            std.log.info("MARKING STACK OF FIBER @{}", .{@intFromPtr(ufiber)});
        }
        var i: [*]Value = @ptrCast(fiber.stack);
        while (@intFromPtr(i) < @intFromPtr(fiber.stack_top)) : (i += 1) {
            try self.markValue(i[0]);
        }
        if (BuildOptions.gc_debug) {
            std.log.info("DONE MARKING STACK OF FIBER @{}", .{@intFromPtr(ufiber)});
        }

        // Mark closure
        if (BuildOptions.gc_debug) {
            std.log.info("MARKING FRAMES OF FIBER @{}", .{@intFromPtr(ufiber)});
        }
        for (fiber.frames.items) |frame| {
            try self.markObj(o.ObjClosure, frame.closure);
            if (frame.error_value) |error_value| {
                try self.markValue(error_value);
            }
            if (frame.native_call_error_value) |error_value| {
                try self.markValue(error_value);
            }
        }
        if (BuildOptions.gc_debug) {
            std.log.info("DONE MARKING FRAMES OF FIBER @{}", .{@intFromPtr(ufiber)});
        }

        // Mark opened upvalues
        if (BuildOptions.gc_debug) {
            std.log.info("MARKING UPVALUES OF FIBER @{}", .{@intFromPtr(ufiber)});
        }
        if (fiber.open_upvalues) |open_upvalues| {
            var upvalue: ?Pool(o.ObjUpValue).Idx = open_upvalues;
            while (upvalue) |unwrapped| : (upvalue = self.get(o.ObjUpValue, unwrapped).?.next) {
                try self.markObj(o.ObjUpValue, unwrapped);
            }
        }
        if (BuildOptions.gc_debug) {
            std.log.info("DONE MARKING UPVALUES OF FIBER @{}", .{@intFromPtr(ufiber)});
        }

        current_fiber = ufiber.parent_fiber;
    }
}

fn markMethods(self: *GC) !void {
    if (BuildOptions.gc_debug) {
        std.log.info("MARKING BASIC TYPES METHOD", .{});
    }
    // Mark basic types methods
    for (self.objfiber_members) |member| {
        if (member) |umember| {
            try self.markObj(o.ObjNative, umember);
        }
    }

    for (self.objrange_members) |member| {
        if (member) |umember| {
            try self.markObj(o.ObjNative, umember);
        }
    }

    for (self.objstring_members) |member| {
        if (member) |umember| {
            try self.markObj(o.ObjNative, umember);
        }
    }

    for (self.objpattern_members) |member| {
        if (member) |umember| {
            try self.markObj(o.ObjNative, umember);
        }
    }

    if (BuildOptions.gc_debug) {
        std.log.info("DONE MARKING BASIC TYPES METHOD", .{});
    }
}

fn markRoots(self: *GC, vm: *v.VM) !void {
    // FIXME: We should not need this, but we don't know how to prevent collection before the VM actually starts making reference to them
    try self.type_registry.mark();

    try self.markMethods();

    // Mark special strings we always need
    if (self.strings.get("$")) |dollar| {
        try self.markObj(o.ObjString, dollar);
    }

    // Mark import registry
    var it = vm.import_registry.iterator();
    while (it.next()) |kv| {
        try self.markObj(o.ObjString, kv.key_ptr.*);
        for (kv.value_ptr.*) |global| {
            try self.markValue(global);
        }
    }

    // Mark current fiber and its parent fibers
    try markFiber(self, vm.current_fiber);

    // Mark globals
    if (BuildOptions.gc_debug) {
        std.log.info("MARKING GLOBALS OF VM @{}", .{@intFromPtr(vm)});
    }
    for (vm.globals.items) |global| {
        try self.markValue(global);
    }
    if (BuildOptions.gc_debug) {
        std.log.info("DONE MARKING GLOBALS OF VM @{}", .{@intFromPtr(vm)});
    }

    // Mark ast constant values (some are only referenced by the JIT so might be collected before)
    // TODO: does this takes too long or are we saved by vertue of MultiArrayList?
    for (vm.current_ast.nodes.items(.value)) |valueOpt| {
        if (valueOpt) |value| {
            try self.markValue(value);
        }
    }
}

fn traceReference(self: *GC) !void {
    if (BuildOptions.gc_debug) {
        std.log.info("TRACING REFERENCE", .{});
    }
    while (self.gray_stack.pop()) |obj| {
        try blackenObject(self, obj);
    }
    if (BuildOptions.gc_debug) {
        std.log.info("DONE TRACING REFERENCE", .{});
    }
}

fn sweep(self: *GC, mode: Mode) !void {
    const swept: usize = self.bytes_allocated;

    var obj_count: usize = 0;
    var count: usize = 0;

    inline for (@typeInfo(@TypeOf(self.pools.pools)).@"struct".fields) |field| {
        const pool = @field(self.pools.pools, field.name);

        for (pool.slots.items, 0..) |*slot, i| {
            if (slot.*) |*instance| {
                count += 1;

                if (instance.obj.marked) {
                    if (BuildOptions.gc_debug and mode == .Full) {
                        std.log.info(
                            "UNMARKING @{s}{}",
                            .{
                                field.name,
                                i,
                            },
                        );
                    }

                    // If not a full gc, we reset marked, this object is now 'old'
                    instance.obj.marked = if (mode == .Full) false else instance.obj.marked;

                    // If a young collection we don't reset marked flags and since we move all marked object
                    // to the tail of the list, we can stop here, there's no more objects to collect
                    // if (mode == .Young) {
                    //     break;
                    // }
                } else {
                    try self.freeObj(@TypeOf(instance.*), .{ .index = @intCast(i) });
                    obj_count += 1;
                }
            }
        }
    }

    if (BuildOptions.gc_debug or BuildOptions.gc_debug_light) {
        if (swept < self.bytes_allocated) {
            std.log.warn("Sweep gained memory, possibly due to an o.Object collector that takes up memory", .{});
        }

        std.log.info(
            "Swept {} objects for {B}, now {B}",
            .{
                obj_count,
                @max(swept, self.bytes_allocated) - self.bytes_allocated,
                self.bytes_allocated,
            },
        );
    }
}

pub fn collectGarbage(self: *GC) !void {
    var timer = if (!is_wasm) std.time.Timer.start() catch unreachable else {};

    // Don't collect until a VM is actually running
    var vm_it = self.active_vms.iterator();
    const first_vm = vm_it.next();
    if (first_vm == null or first_vm.?.key_ptr.*.flavor == .Repl) {
        return;
    }

    // Avoid triggering another sweep while running collectors
    if (self.obj_collected != null) {
        return;
    }

    const mode: Mode = if (self.bytes_allocated > self.next_full_gc and self.last_gc != null) .Full else .Young;

    if (BuildOptions.gc_debug or BuildOptions.gc_debug_light) {
        std.log.info(
            "-- gc starts mode {s}, {B}, {} objects",
            .{
                @tagName(mode),
                self.bytes_allocated,
                self.objects.len(),
            },
        );
    }

    var it = self.active_vms.iterator();
    while (it.next()) |kv| {
        var vm = kv.key_ptr.*;

        if (BuildOptions.gc_debug) {
            std.log.info(
                "Marking VM @{}, on fiber @{} and closure @{} (function @{} {s})",
                .{
                    @intFromPtr(vm),
                    @intFromPtr(vm.current_fiber),
                    @intFromPtr(vm.currentFrame().?.closure),
                    @intFromPtr(vm.currentFrame().?.closure.function),
                    vm.currentFrame().?.closure.function.type_def.resolved_type.?.Function.name.string,
                },
            );
        }

        try markRoots(self, vm);
    }

    try traceReference(self);

    try sweep(self, mode);

    if (mode == .Full) {
        self.full_collection_count += 1;
    } else {
        self.light_collection_count += 1;
    }

    self.next_gc = self.bytes_allocated * BuildOptions.next_gc_ratio;
    if (mode == .Full) {
        self.next_full_gc = self.bytes_allocated * BuildOptions.next_full_gc_ratio;
    }
    self.last_gc = mode;

    if (BuildOptions.gc_debug or BuildOptions.gc_debug_light) {
        std.log.info(
            "-- gc end, {B}, {} objects, next_gc {B}, next_full_gc {B}",
            .{
                self.bytes_allocated,
                self.objects.len(),
                self.next_gc,
                self.next_full_gc,
            },
        );
    }

    if (!is_wasm) {
        self.gc_time += timer.read();
    }
}
