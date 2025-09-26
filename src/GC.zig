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
strings: std.StringHashMapUnmanaged(*o.ObjString) = .empty,
type_registry: TypeRegistry,
bytes_allocated: usize = 0,
// next_gc == next_full_gc at first so the first cycle is a full gc
next_gc: usize = if (builtin.mode == .Debug) 1024 else 1024 * BuildOptions.initial_gc,
next_full_gc: usize = if (builtin.mode == .Debug) 1024 else 1024 * BuildOptions.initial_gc,
last_gc: ?Mode = null,
objects: std.DoublyLinkedList = .{},
gray_stack: std.ArrayList(*o.Obj) = .empty,
active_vms: std.AutoHashMapUnmanaged(*v.VM, void) = .empty,
// o.Obj being collected, useful to avoid setting object instance dirty while running its collector method
obj_collected: ?*o.Obj = null,

debugger: ?Debugger,
where: ?Token = null,

// Types we generaly don't wan't to ever be collected
objfiber_members: []?*o.ObjNative,
objfiber_memberDefs: []?*o.ObjTypeDef,
objpattern_members: []?*o.ObjNative,
objpattern_memberDefs: []?*o.ObjTypeDef,
objstring_members: []?*o.ObjNative,
objstring_memberDefs: []?*o.ObjTypeDef,
objrange_memberDefs: []?*o.ObjTypeDef,
objrange_members: []?*o.ObjNative,

full_collection_count: usize = 0,
light_collection_count: usize = 0,
max_allocated: usize = 0,

gc_time: usize = 0,

pub fn init(allocator: std.mem.Allocator) !GC {
    const self = GC{
        .allocator = allocator,
        .type_registry = undefined,
        .debugger = if (BuildOptions.gc_debug_access) Debugger.init(allocator) else null,

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
    const removed = self.active_vms.remove(vm);

    std.debug.assert(removed);
}

pub fn deinit(self: *GC) void {
    self.gray_stack.deinit(self.allocator);
    self.strings.deinit(self.allocator);
    self.active_vms.deinit(self.allocator);
    if (BuildOptions.gc_debug_access) {
        self.debugger.?.deinit();
    }

    self.allocator.free(self.objfiber_members);
    self.allocator.free(self.objfiber_memberDefs);
    self.allocator.free(self.objpattern_members);
    self.allocator.free(self.objpattern_memberDefs);
    self.allocator.free(self.objstring_members);
    self.allocator.free(self.objstring_memberDefs);
    self.allocator.free(self.objrange_members);
    self.allocator.free(self.objrange_memberDefs);
}

pub fn allocate(self: *GC, comptime T: type) !*T {
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

    const allocated = try self.allocator.create(T);

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

pub fn allocateObject(self: *GC, data: anytype) !*@TypeOf(data) {
    const T = @TypeOf(data);

    const obj: *T = try self.allocate(T);
    obj.* = data;

    const object = obj.toObj();

    // Add new object at start of vm.objects linked list
    try self.addObject(object);

    if (BuildOptions.gc_debug_access) {
        self.debugger.?.allocated(
            object,
            self.where,
            switch (T) {
                o.ObjString => .String,
                o.ObjTypeDef => .Type,
                o.ObjUpValue => .UpValue,
                o.ObjClosure => .Closure,
                o.ObjFunction => .Function,
                o.ObjObjectInstance => .ObjectInstance,
                o.ObjObject => .Object,
                o.ObjList => .List,
                o.ObjMap => .Map,
                o.ObjEnum => .Enum,
                o.ObjEnumInstance => .EnumInstance,
                o.ObjBoundMethod => .Bound,
                o.ObjNative => .Native,
                o.ObjUserData => .UserData,
                o.ObjPattern => .Pattern,
                o.ObjFiber => .Fiber,
                o.ObjForeignContainer => .ForeignContainer,
                o.ObjRange => .Range,
                else => @panic("Unknown object type being allocated"),
            },
        );
    }

    return obj;
}

fn addObject(self: *GC, obj: *o.Obj) !void {
    self.objects.prepend(&obj.node);
}

pub fn allocateString(self: *GC, chars: []const u8) !*o.ObjString {
    const string: *o.ObjString = try allocateObject(
        self,
        o.ObjString{ .string = chars },
    );

    try self.strings.put(
        self.allocator,
        string.string,
        string,
    );

    return string;
}

pub fn copyString(self: *GC, chars: []const u8) !*o.ObjString {
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

fn free(self: *GC, comptime T: type, pointer: *T) void {
    var timer = if (!is_wasm) std.time.Timer.start() catch unreachable else {};

    if (BuildOptions.gc_debug) {
        std.log.info("Going to free {*}", .{pointer});
    }

    self.bytes_allocated -= @sizeOf(T);
    self.allocator.destroy(pointer);

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

pub fn markObjDirty(self: *GC, obj: *o.Obj) !void {
    if (!obj.dirty and self.obj_collected != obj) {
        obj.dirty = true;

        // A dirty obj is: an old object with reference to potential young objects that will need to be marked
        // Since old object are ignored when tracing references, this will force tracing for it
        try self.gray_stack.append(self.allocator, obj);
    }
}

pub fn markObj(self: *GC, obj: *o.Obj) !void {
    if (obj.marked or self.obj_collected == obj) {
        if (BuildOptions.gc_debug) {
            std.log.info(
                "{*} {s} already marked or old",
                .{
                    obj,
                    try Value.fromObj(obj).toStringAlloc(self.allocator),
                },
            );
        }
        return;
    }

    if (BuildOptions.gc_debug) {
        std.log.info(
            "marking {*}: `{s}`",
            .{
                obj,
                try Value.fromObj(obj).toStringAlloc(self.allocator),
            },
        );
    }

    obj.marked = true;

    // Move marked obj to tail so the sweeping can stop going through objects when finding the first marked object
    self.objects.remove(&obj.node);
    // Just to be safe, reset node before inserting it again
    obj.node = .{
        .prev = null,
        .next = null,
    };
    self.objects.append(&obj.node);

    try self.gray_stack.append(self.allocator, obj);
}

fn blackenObject(self: *GC, obj: *o.Obj) !void {
    if (BuildOptions.gc_debug) {
        std.log.info(
            "blackening @{} {}",
            .{
                @intFromPtr(obj),
                obj.obj_type,
            },
        );
    }

    obj.dirty = false;

    _ = try switch (obj.obj_type) {
        .String, .Pattern, .UserData, .Native, .Range => {},
        .Type => obj.access(o.ObjTypeDef, .Type, self).?.mark(self),
        .UpValue => obj.access(o.ObjUpValue, .UpValue, self).?.mark(self),
        .Closure => obj.access(o.ObjClosure, .Closure, self).?.mark(self),
        .Function => obj.access(o.ObjFunction, .Function, self).?.mark(self),
        .ObjectInstance => obj.access(o.ObjObjectInstance, .ObjectInstance, self).?.mark(self),
        .Object => obj.access(o.ObjObject, .Object, self).?.mark(self),
        .List => obj.access(o.ObjList, .List, self).?.mark(self),
        .Map => obj.access(o.ObjMap, .Map, self).?.mark(self),
        .Enum => obj.access(o.ObjEnum, .Enum, self).?.mark(self),
        .EnumInstance => obj.access(o.ObjEnumInstance, .EnumInstance, self).?.mark(self),
        .Bound => obj.access(o.ObjBoundMethod, .Bound, self).?.mark(self),
        .Fiber => obj.access(o.ObjFiber, .Fiber, self).?.mark(self),
        .ForeignContainer => obj.access(o.ObjForeignContainer, .ForeignContainer, self).?.mark(self),
    };

    if (BuildOptions.gc_debug) {
        std.log.info(
            "done blackening @{} {}",
            .{
                @intFromPtr(obj),
                obj.obj_type,
            },
        );
    }
}

fn freeObj(self: *GC, obj: *o.Obj) (std.mem.Allocator.Error || std.fmt.BufPrintError)!void {
    if (BuildOptions.gc_debug) {
        std.log.info(
            ">> freeing {} {}",
            .{
                @intFromPtr(obj),
                obj.obj_type,
            },
        );
    }

    if (BuildOptions.gc_debug_access) {
        self.debugger.?.collected(obj, self.where.?);
    }

    self.obj_collected = obj;
    defer self.obj_collected = null;

    switch (obj.obj_type) {
        .String => {
            const obj_string = o.ObjString.cast(obj).?;

            // Remove it from interned strings
            _ = self.strings.remove(obj_string.string);

            freeMany(self, u8, obj_string.string);
            free(self, o.ObjString, obj_string);
        },
        .Pattern => {
            var obj_pattern = o.ObjPattern.cast(obj).?;
            if (!is_wasm) {
                obj_pattern.pattern.free();
            }

            free(self, o.ObjPattern, obj_pattern);
        },
        .Type => {
            var obj_typedef = o.ObjTypeDef.cast(obj).?;
            const hash = TypeRegistry.typeDefHash(obj_typedef.*);

            if (self.type_registry.registry.get(hash)) |registered_obj| {
                if (registered_obj == obj_typedef) {
                    _ = self.type_registry.registry.remove(hash);
                    if (BuildOptions.gc_debug) {
                        std.log.info(
                            "Removed registered type @{} #{} `{s}`",
                            .{
                                @intFromPtr(registered_obj),
                                hash,
                                obj_typedef.toStringAlloc(self.allocator, true) catch unreachable,
                            },
                        );
                    }
                } else {
                    // std.log.info(
                    //     "ObjTypeDef {*} `{s}` was allocated outside of type registry\n",
                    //     .{
                    //         obj_typedef,
                    //         try obj_typedef.toStringAlloc(self.allocator),
                    //     },
                    // );
                    // unreachable;
                    // FIXME: this should not occur. Right now this because of the way we resolve placeholders by changing their content and replacing the type in the typ registry
                    // Previously registered same type is now outside of the type registry and is collected.
                    return;
                }
            }

            obj_typedef.deinit();

            free(self, o.ObjTypeDef, obj_typedef);
        },
        .UpValue => {
            const obj_upvalue = o.ObjUpValue.cast(obj).?;
            if (obj_upvalue.closed) |value| {
                if (value.isObj()) {
                    try freeObj(self, value.obj());
                }
            }

            free(self, o.ObjUpValue, obj_upvalue);
        },
        .Closure => {
            var obj_closure = o.ObjClosure.cast(obj).?;
            obj_closure.deinit(self.allocator);

            free(self, o.ObjClosure, obj_closure);
        },
        .Function => {
            var obj_function = o.ObjFunction.cast(obj).?;
            obj_function.deinit();

            free(self, o.ObjFunction, obj_function);
        },
        .ObjectInstance => {
            var obj_objectinstance = o.ObjObjectInstance.cast(obj).?;

            // Calling eventual destructor method
            if (obj_objectinstance.object) |object| {
                if (object.type_def.resolved_type.?.Object.fields.get("collect")) |field| {
                    if (field.method and !field.static) {
                        if (BuildOptions.gc_debug_access) {
                            self.debugger.?.invoking_collector = true;
                        }
                        buzz_api.bz_invoke(
                            obj_objectinstance.vm,
                            obj_objectinstance.toValue(),
                            field.index,
                            null,
                            0,
                            null,
                        );
                        if (BuildOptions.gc_debug_access) {
                            self.debugger.?.invoking_collector = false;
                        }

                        // Remove void result of the collect call
                        _ = obj_objectinstance.vm.pop();
                    }
                }
            }

            obj_objectinstance.deinit(self.allocator);

            free(self, o.ObjObjectInstance, obj_objectinstance);
        },
        .Object => {
            var obj_object = o.ObjObject.cast(obj).?;
            obj_object.deinit(self.allocator);

            free(self, o.ObjObject, obj_object);
        },
        .List => {
            var obj_list = o.ObjList.cast(obj).?;
            obj_list.deinit(self.allocator);

            free(self, o.ObjList, obj_list);
        },
        .Map => {
            var obj_map = o.ObjMap.cast(obj).?;
            obj_map.deinit(self.allocator);

            free(self, o.ObjMap, obj_map);
        },
        .Enum => {
            const obj_enum = o.ObjEnum.cast(obj).?;
            self.allocator.free(obj_enum.cases);

            free(self, o.ObjEnum, obj_enum);
        },
        .EnumInstance => free(self, o.ObjEnumInstance, o.ObjEnumInstance.cast(obj).?),
        .Bound => free(self, o.ObjBoundMethod, o.ObjBoundMethod.cast(obj).?),
        .Native => free(self, o.ObjNative, o.ObjNative.cast(obj).?),
        .UserData => free(self, o.ObjUserData, o.ObjUserData.cast(obj).?),
        .Fiber => {
            var obj_fiber = o.ObjFiber.cast(obj).?;
            obj_fiber.fiber.deinit();

            self.allocator.destroy(obj_fiber.fiber);

            free(self, o.ObjFiber, obj_fiber);
        },
        .ForeignContainer => {
            const obj_foreignstruct = o.ObjForeignContainer.cast(obj).?;

            self.freeMany(u8, obj_foreignstruct.data);

            free(self, o.ObjForeignContainer, obj_foreignstruct);
        },
        .Range => {
            free(self, o.ObjRange, o.ObjRange.cast(obj).?);
        },
    }
}

pub fn markValue(self: *GC, value: Value) !void {
    if (value.isObj()) {
        try self.markObj(value.obj());
    }
}

pub fn markFiber(self: *GC, fiber: *v.Fiber) !void {
    var current_fiber: ?*v.Fiber = fiber;
    while (current_fiber) |ufiber| {
        try self.markObj(@constCast(ufiber.type_def.toObj()));
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
            try self.markObj(frame.closure.toObj());
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
            var upvalue: ?*o.ObjUpValue = open_upvalues;
            while (upvalue) |unwrapped| : (upvalue = unwrapped.next) {
                try self.markObj(unwrapped.toObj());
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
            try self.markObj(umember.toObj());
        }
    }

    for (self.objrange_members) |member| {
        if (member) |umember| {
            try self.markObj(umember.toObj());
        }
    }

    for (self.objstring_members) |member| {
        if (member) |umember| {
            try self.markObj(umember.toObj());
        }
    }

    for (self.objpattern_members) |member| {
        if (member) |umember| {
            try self.markObj(umember.toObj());
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
        try self.markObj(dollar.toObj());
    }

    // Mark import registry
    var it = vm.import_registry.iterator();
    while (it.next()) |kv| {
        try self.markObj(kv.key_ptr.*.toObj());
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
    while (self.gray_stack.items.len > 0) {
        try blackenObject(self, self.gray_stack.pop().?);
    }
    if (BuildOptions.gc_debug) {
        std.log.info("DONE TRACING REFERENCE", .{});
    }
}

fn sweep(self: *GC, mode: Mode) !void {
    const swept: usize = self.bytes_allocated;

    var obj_count: usize = 0;
    var obj_node = self.objects.first;
    var count: usize = 0;
    while (obj_node) |node| : (count += 1) {
        const obj: *o.Obj = @fieldParentPtr("node", node);
        const marked = obj.marked;
        if (marked) {
            if (BuildOptions.gc_debug and mode == .Full) {
                std.log.info(
                    "UNMARKING @{}",
                    .{
                        @intFromPtr(
                            @as(*o.Obj, @fieldParentPtr("node", node)),
                        ),
                    },
                );
            }
            // If not a full gc, we reset marked, this object is now 'old'
            obj.marked = if (mode == .Full) false else marked;

            // If a young collection we don't reset marked flags and since we move all marked object
            // to the tail of the list, we can stop here, there's no more objects to collect
            if (mode == .Young) {
                break;
            }

            obj_node = node.next;
        } else {
            const unreached: *o.Obj = obj;
            obj_node = node.next;

            self.objects.remove(node);

            try freeObj(self, unreached);
            obj_count += 1;
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

pub const Debugger = struct {
    const Self = @This();

    pub const Ptr = struct {
        what: o.ObjType,
        allocated_at: ?Token,
        collected_at: ?Token = null,
    };

    allocator: std.mem.Allocator,
    reporter: Reporter,
    tracker: std.AutoHashMapUnmanaged(*o.Obj, Ptr),
    invoking_collector: bool = false,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .tracker = std.AutoHashMapUnmanaged(*o.Obj, Ptr){},
            .reporter = Reporter{
                .allocator = allocator,
            },
        };
    }

    pub fn deinit(self: *Self) void {
        self.tracker.deinit(self.allocator);
    }

    pub fn allocated(self: *Self, ptr: *o.Obj, at: ?Token, what: o.ObjType) void {
        std.debug.assert(self.tracker.get(ptr) == null);
        self.tracker.put(
            self.allocator,
            ptr,
            Ptr{
                .what = what,
                .allocated_at = at,
            },
        ) catch @panic("Could not track object");
    }

    pub fn collected(self: *Self, ptr: *o.Obj, at: Token) void {
        if (self.tracker.getPtr(ptr)) |tracked| {
            if (tracked.collected_at) |collected_at| {
                self.reporter.reportWithOrigin(
                    .gc,
                    at,
                    at,
                    collected_at,
                    collected_at,
                    "Trying to collected already collected {} {*}",
                    .{ tracked.what, ptr },
                    "first collected here",
                );

                @panic("Double collect");
            }

            tracked.collected_at = at;
        } else {
            @panic("Collect of untracked object");
        }
    }

    pub fn accessed(self: *Self, ptr: *o.Obj, at: ?Token) void {
        if (self.invoking_collector) return;

        if (self.tracker.getPtr(ptr)) |tracked| {
            if (tracked.collected_at) |collected_at| {
                var items = std.array_list.Managed(Reporter.ReportItem).init(self.allocator);
                defer items.deinit();

                var message = std.array_list.Managed(u8).init(self.allocator);
                defer message.deinit();

                message.writer().print(
                    "Access to already collected {} {*}",
                    .{
                        tracked.what,
                        ptr,
                    },
                ) catch unreachable;

                items.append(
                    .{
                        .location = at.?,
                        .end_location = at.?,
                        .kind = .@"error",
                        .message = message.items,
                    },
                ) catch unreachable;

                if (tracked.allocated_at) |allocated_at| {
                    items.append(
                        .{
                            .location = allocated_at,
                            .end_location = allocated_at,
                            .kind = .hint,
                            .message = "allocated here",
                        },
                    ) catch unreachable;
                }

                items.append(
                    .{
                        .location = collected_at,
                        .end_location = collected_at,
                        .kind = .hint,
                        .message = "collected here",
                    },
                ) catch unreachable;

                var report = Reporter.Report{
                    .message = message.items,
                    .error_type = .gc,
                    .items = items.items,
                };

                report.reportStderr(&self.reporter) catch unreachable;

                @panic("Access to already collected object");
            }
        } else {
            if (at) |accessed_at| {
                self.reporter.reportErrorFmt(
                    .gc,
                    accessed_at,
                    accessed_at,
                    "Untracked obj {*}",
                    .{
                        ptr,
                    },
                );
            } else {
                std.log.warn(
                    "Untracked obj {*}",
                    .{
                        ptr,
                    },
                );
            }

            @panic("Access to untracked object");
        }
    }
};
