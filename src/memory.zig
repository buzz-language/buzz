const std = @import("std");
const builtin = @import("builtin");
const _vm = @import("./vm.zig");
const Fiber = _vm.Fiber;
const _value = @import("./value.zig");
const _obj = @import("./obj.zig");
const dumpStack = @import("./disassembler.zig").dumpStack;
const BuildOptions = @import("build_options");
const VM = @import("./vm.zig").VM;
const assert = std.debug.assert;
const Token = @import("./token.zig").Token;
const buzz_api = @import("./buzz_api.zig");

pub const pcre = @import("./pcre.zig").pcre;

const Value = _value.Value;
const valueToStringAlloc = _value.valueToStringAlloc;
const Obj = _obj.Obj;
const ObjString = _obj.ObjString;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjUpValue = _obj.ObjUpValue;
const ObjClosure = _obj.ObjClosure;
const ObjFunction = _obj.ObjFunction;
const ObjObjectInstance = _obj.ObjObjectInstance;
const ObjObject = _obj.ObjObject;
const ObjList = _obj.ObjList;
const ObjMap = _obj.ObjMap;
const ObjEnum = _obj.ObjEnum;
const ObjEnumInstance = _obj.ObjEnumInstance;
const ObjBoundMethod = _obj.ObjBoundMethod;
const ObjNative = _obj.ObjNative;
const ObjUserData = _obj.ObjUserData;
const ObjPattern = _obj.ObjPattern;
const ObjFiber = _obj.ObjFiber;
const ObjForeignStruct = _obj.ObjForeignStruct;

pub const TypeRegistry = struct {
    const Self = @This();

    gc: *GarbageCollector,
    registry: std.StringHashMap(*ObjTypeDef),

    pub fn init(gc: *GarbageCollector) Self {
        return .{ .gc = gc, .registry = std.StringHashMap(*ObjTypeDef).init(gc.allocator) };
    }

    pub fn deinit(self: *Self) void {
        self.registry.deinit();
    }

    pub fn getTypeDef(self: *Self, type_def: ObjTypeDef) !*ObjTypeDef {
        var type_def_buf = std.ArrayList(u8).init(self.gc.allocator);
        try type_def.toString(&type_def_buf.writer());
        const type_def_str: []const u8 = type_def_buf.items;

        // We don't return a cached version of a placeholder since they all maintain a particular state (link)
        if (type_def.def_type != .Placeholder) {
            if (self.registry.get(type_def_str)) |type_def_ptr| {
                type_def_buf.deinit(); // If already in map, we don't need this string anymore
                return type_def_ptr;
            }
        }

        var type_def_ptr: *ObjTypeDef = try self.gc.allocateObject(ObjTypeDef, type_def);

        if (BuildOptions.debug_placeholders) {
            std.debug.print("`{s}` @{}\n", .{ type_def_str, @intFromPtr(type_def_ptr) });
        }
        _ = try self.registry.put(type_def_str, type_def_ptr);

        return type_def_ptr;
    }

    pub fn setTypeDef(self: *Self, type_def: *ObjTypeDef) !void {
        const type_def_str = try type_def.toStringAlloc(self.gc.allocator);

        assert(type_def.def_type != .Placeholder);

        _ = try self.registry.put(type_def_str.items, type_def);
    }

    pub inline fn getTypeDefByName(self: *Self, name: []const u8) ?*ObjTypeDef {
        return self.registry.get(name);
    }

    pub fn mark(self: *Self) !void {
        var it = self.registry.iterator();
        while (it.next()) |kv| {
            try self.gc.markObj(kv.value_ptr.*.toObj());
        }
    }
};

// Sticky Mark Bits Generational GC basic idea:
// 1. First GC: do a normal mark, don't clear `is_marked` at the end
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
// Writer barrier on those OpCodes:
//     * OP_LIST_APPEND
//     * OP_SET_MAP
//     * OP_SET_SUBSCRIPT
//     * OP_INHERIT
//     * OP_INSTANCE
//     * OP_METHOD
//     * OP_PROPERTY
//     * OP_SET_PROPERTY
//
// Can we avoid going through the whole linked list of objects when sweeping?
//
// For now we only have either young or old objects but we could improve on it with more categories with each its threshold
pub const GarbageCollector = struct {
    const Self = @This();

    const Mode = enum {
        Young,
        // Old,
        Full,
    };

    allocator: std.mem.Allocator,
    strings: std.StringHashMap(*ObjString),
    type_registry: TypeRegistry,
    bytes_allocated: usize = 0,
    // next_gc == next_full_gc at first so the first cycle is a full gc
    next_gc: usize = if (builtin.mode == .Debug) 1024 else 1024 * BuildOptions.initial_gc,
    next_full_gc: usize = if (builtin.mode == .Debug) 1024 else 1024 * BuildOptions.initial_gc,
    last_gc: ?Mode = null,
    objects: std.TailQueue(*Obj) = .{},
    gray_stack: std.ArrayList(*Obj),
    active_vms: std.AutoHashMap(*VM, void),

    debugger: ?GarbageCollectorDebugger,
    where: ?Token = null,

    // Types we generaly don't wan't to ever be collected
    objfiber_members: std.AutoHashMap(*ObjString, *ObjNative),
    objfiber_memberDefs: std.StringHashMap(*ObjTypeDef),
    objpattern_members: std.AutoHashMap(*ObjString, *ObjNative),
    objpattern_memberDefs: std.StringHashMap(*ObjTypeDef),
    objstring_members: std.AutoHashMap(*ObjString, *ObjNative),
    objstring_memberDefs: std.StringHashMap(*ObjTypeDef),

    full_collection_count: usize = 0,
    light_collection_count: usize = 0,
    max_allocated: usize = 0,

    gc_time: usize = 0,

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .allocator = allocator,
            .strings = std.StringHashMap(*ObjString).init(allocator),
            .type_registry = undefined,
            .gray_stack = std.ArrayList(*Obj).init(allocator),
            .active_vms = std.AutoHashMap(*VM, void).init(allocator),
            .debugger = if (BuildOptions.gc_debug_access) GarbageCollectorDebugger.init(allocator) else null,

            .objfiber_members = std.AutoHashMap(*ObjString, *ObjNative).init(allocator),
            .objfiber_memberDefs = std.StringHashMap(*ObjTypeDef).init(allocator),
            .objpattern_members = std.AutoHashMap(*ObjString, *ObjNative).init(allocator),
            .objpattern_memberDefs = std.StringHashMap(*ObjTypeDef).init(allocator),
            .objstring_members = std.AutoHashMap(*ObjString, *ObjNative).init(allocator),
            .objstring_memberDefs = std.StringHashMap(*ObjTypeDef).init(allocator),
        };
    }

    pub fn registerVM(self: *Self, vm: *VM) !void {
        try self.active_vms.put(vm, {});
    }

    pub fn unregisterVM(self: *Self, vm: *VM) void {
        assert(self.active_vms.remove(vm));
    }

    pub fn deinit(self: *Self) void {
        self.gray_stack.deinit();
        self.strings.deinit();
        self.active_vms.deinit();
        if (self.debugger) |debugger| {
            debugger.deinit();
        }

        self.objfiber_members.deinit();
        self.objfiber_memberDefs.deinit();
        self.objpattern_members.deinit();
        self.objpattern_memberDefs.deinit();
        self.objstring_members.deinit();
        self.objstring_memberDefs.deinit();
    }

    pub fn allocate(self: *Self, comptime T: type) !*T {
        var timer = std.time.Timer.start() catch unreachable;

        self.bytes_allocated += @sizeOf(T);

        if (self.bytes_allocated > self.max_allocated) {
            self.max_allocated = self.bytes_allocated;
        }

        if (self.bytes_allocated > self.next_gc and !BuildOptions.gc_off) {
            try self.collectGarbage();
        }

        var allocated = try self.allocator.create(T);

        if (BuildOptions.gc_debug) {
            std.debug.print("Allocated @{} {}\n", .{ @intFromPtr(allocated), T });
        }

        self.gc_time += timer.read();
        return allocated;
    }

    pub fn allocateMany(self: *Self, comptime T: type, count: usize) ![]T {
        var timer = std.time.Timer.start() catch unreachable;

        self.bytes_allocated += (@sizeOf(T) * count);

        if (self.bytes_allocated > self.max_allocated) {
            self.max_allocated = self.bytes_allocated;
        }

        if (self.bytes_allocated > self.next_gc and !BuildOptions.gc_off) {
            try self.collectGarbage();
        }

        self.gc_time += timer.read();
        return try self.allocator.alloc(T, count);
    }

    pub fn allocateObject(self: *Self, comptime T: type, data: T) !*T {
        // var before: usize = self.bytes_allocated;

        var obj: *T = try self.allocate(T);
        obj.* = data;

        var object: *Obj = switch (T) {
            ObjString => ObjString.toObj(obj),
            ObjTypeDef => ObjTypeDef.toObj(obj),
            ObjUpValue => ObjUpValue.toObj(obj),
            ObjClosure => ObjClosure.toObj(obj),
            ObjFunction => ObjFunction.toObj(obj),
            ObjObjectInstance => ObjObjectInstance.toObj(obj),
            ObjObject => ObjObject.toObj(obj),
            ObjList => ObjList.toObj(obj),
            ObjMap => ObjMap.toObj(obj),
            ObjEnum => ObjEnum.toObj(obj),
            ObjEnumInstance => ObjEnumInstance.toObj(obj),
            ObjBoundMethod => ObjBoundMethod.toObj(obj),
            ObjNative => ObjNative.toObj(obj),
            ObjUserData => ObjUserData.toObj(obj),
            ObjPattern => ObjPattern.toObj(obj),
            ObjFiber => ObjFiber.toObj(obj),
            ObjForeignStruct => ObjForeignStruct.toObj(obj),
            else => {},
        };

        // if (BuildOptions.gc_debug) {
        //     std.debug.print("allocated {*} {*}\n", .{ obj, object });
        //     std.debug.print("(from {}) {} allocated, total {}\n", .{ before, self.bytes_allocated - before, self.bytes_allocated });
        // }

        // Add new object at start of vm.objects linked list
        try self.addObject(object);

        if (BuildOptions.gc_debug_access) {
            self.debugger.?.allocated(
                object,
                self.where,
                switch (T) {
                    ObjString => .String,
                    ObjTypeDef => .Type,
                    ObjUpValue => .UpValue,
                    ObjClosure => .Closure,
                    ObjFunction => .Function,
                    ObjObjectInstance => .ObjectInstance,
                    ObjObject => .Object,
                    ObjList => .List,
                    ObjMap => .Map,
                    ObjEnum => .Enum,
                    ObjEnumInstance => .EnumInstance,
                    ObjBoundMethod => .Bound,
                    ObjNative => .Native,
                    ObjUserData => .UserData,
                    ObjPattern => .Pattern,
                    ObjFiber => .Fiber,
                    ObjForeignStruct => .ForeignStruct,
                    else => {},
                },
            );
        }

        return obj;
    }

    fn addObject(self: *Self, obj: *Obj) !void {
        var new_node = try self.allocator.create(std.TailQueue(*Obj).Node);
        new_node.* = .{
            .data = obj,
        };
        obj.node = new_node;
        self.objects.prepend(new_node);
    }

    pub fn allocateString(self: *Self, chars: []const u8) !*ObjString {
        var string: *ObjString = try allocateObject(
            self,
            ObjString,
            ObjString{ .string = chars },
        );

        try self.strings.put(string.string, string);

        return string;
    }

    pub fn copyString(self: *Self, chars: []const u8) !*ObjString {
        if (self.strings.get(chars)) |interned| {
            return interned;
        }

        var copy: []u8 = try self.allocateMany(u8, chars.len);
        std.mem.copy(u8, copy, chars);

        if (BuildOptions.gc_debug) {
            std.debug.print("Allocated slice {*} `{s}`\n", .{ copy, copy });
        }

        return try allocateString(self, copy);
    }

    fn free(self: *Self, comptime T: type, pointer: *T) void {
        var timer = std.time.Timer.start() catch unreachable;

        if (BuildOptions.gc_debug) {
            std.debug.print("Going to free {*}\n", .{pointer});
        }

        self.bytes_allocated -= @sizeOf(T);
        self.allocator.destroy(pointer);

        if (BuildOptions.gc_debug) {
            std.debug.print(
                "(from {}), collected {}, {} allocated\n",
                .{ self.bytes_allocated + @sizeOf(T), @sizeOf(T), self.bytes_allocated },
            );
        }

        self.gc_time += timer.read();
    }

    fn freeMany(self: *Self, comptime T: type, pointer: []const T) void {
        var timer = std.time.Timer.start() catch unreachable;

        if (BuildOptions.gc_debug) {
            std.debug.print("Going to free slice {*} `{s}`\n", .{ pointer, pointer });
        }

        const n: usize = (@sizeOf(T) * pointer.len);
        self.bytes_allocated -= n;
        self.allocator.free(pointer);

        if (BuildOptions.gc_debug) {
            std.debug.print(
                "(from {}), collected {}, {} allocated\n",
                .{
                    self.bytes_allocated + n,
                    n,
                    self.bytes_allocated,
                },
            );
        }

        self.gc_time += timer.read();
    }

    pub fn markObjDirty(self: *Self, obj: *Obj) !void {
        if (!obj.is_dirty) {
            obj.is_dirty = true;

            // std.debug.print(
            //     "Marked obj @{} {} dirty, gray_stack @{} or GC @{} will be {} items long\n",
            //     .{
            //         @intFromPtr(obj),
            //         obj.obj_type,
            //         @intFromPtr(&self.gray_stack),
            //         @intFromPtr(self),
            //         self.gray_stack.items.len,
            //     },
            // );

            // A dirty obj is: an old object with reference to potential young objects that will need to be marked
            // Since old object are ignored when tracing references, this will force tracing for it
            try self.gray_stack.append(obj);
        }
    }

    pub fn markObj(self: *Self, obj: *Obj) !void {
        if (obj.is_marked) {
            if (BuildOptions.gc_debug) {
                std.debug.print(
                    "{*} {s} already marked or old\n",
                    .{
                        obj,
                        (try valueToStringAlloc(self.allocator, Value.fromObj(obj))).items,
                    },
                );
            }
            return;
        }

        if (BuildOptions.gc_debug) {
            std.debug.print("marking {*}: ", .{obj});
            std.debug.print(
                "{s}\n",
                .{
                    (try valueToStringAlloc(self.allocator, Value.fromObj(obj))).items,
                },
            );
        }

        obj.is_marked = true;

        // Move marked obj to tail so we sweeping can stop going through objects when finding the first marked object
        self.objects.remove(obj.node.?);
        // Just to be safe, reset node before inserting it again
        obj.node.?.* = .{
            .prev = null,
            .next = null,
            .data = obj,
        };
        self.objects.append(obj.node.?);

        try self.gray_stack.append(obj);
    }

    fn blackenObject(self: *Self, obj: *Obj) !void {
        if (BuildOptions.gc_debug) {
            std.debug.print(
                "blackening @{} {}\n",
                .{
                    @intFromPtr(obj),
                    obj.obj_type,
                },
            );
        }

        obj.is_dirty = false;

        _ = try switch (obj.obj_type) {
            .String => ObjString.cast(obj).?.mark(self),
            .Type => ObjTypeDef.cast(obj).?.mark(self),
            .UpValue => ObjUpValue.cast(obj).?.mark(self),
            .Closure => ObjClosure.cast(obj).?.mark(self),
            .Function => ObjFunction.cast(obj).?.mark(self),
            .ObjectInstance => ObjObjectInstance.cast(obj).?.mark(self),
            .Object => ObjObject.cast(obj).?.mark(self),
            .List => ObjList.cast(obj).?.mark(self),
            .Map => ObjMap.cast(obj).?.mark(self),
            .Enum => ObjEnum.cast(obj).?.mark(self),
            .EnumInstance => ObjEnumInstance.cast(obj).?.mark(self),
            .Bound => ObjBoundMethod.cast(obj).?.mark(self),
            .Native => ObjNative.cast(obj).?.mark(self),
            .UserData => ObjUserData.cast(obj).?.mark(self),
            .Pattern => ObjPattern.cast(obj).?.mark(self),
            .Fiber => try ObjFiber.cast(obj).?.mark(self),
            .ForeignStruct => try ObjForeignStruct.cast(obj).?.mark(self),
        };

        if (BuildOptions.gc_debug) {
            std.debug.print(
                "done blackening @{} {}\n",
                .{
                    @intFromPtr(obj),
                    obj.obj_type,
                },
            );
        }
    }

    fn freeObj(self: *Self, obj: *Obj) (std.mem.Allocator.Error || std.fmt.BufPrintError)!void {
        if (BuildOptions.gc_debug) {
            std.debug.print(">> freeing {} {}\n", .{ @intFromPtr(obj), obj.obj_type });
        }

        if (BuildOptions.gc_debug_access) {
            self.debugger.?.collected(obj, self.where.?);
        }

        self.allocator.destroy(obj.node.?);

        switch (obj.obj_type) {
            .String => {
                var obj_string = ObjString.cast(obj).?;

                // Remove it from interned strings
                _ = self.strings.remove(obj_string.string);

                freeMany(self, u8, obj_string.string);
                free(self, ObjString, obj_string);
            },
            .Pattern => {
                var obj_pattern = ObjPattern.cast(obj).?;
                pcre.pcre_free.?(obj_pattern.pattern);

                free(self, ObjPattern, obj_pattern);
            },
            .Type => {
                var obj_typedef = ObjTypeDef.cast(obj).?;

                const str = try obj_typedef.toStringAlloc(self.allocator);
                defer str.deinit();

                if (self.type_registry.registry.get(str.items)) |registered_obj| {
                    if (registered_obj == obj_typedef) {
                        _ = self.type_registry.registry.remove(str.items);
                        if (BuildOptions.gc_debug) {
                            std.debug.print("Removed registered type @{} `{s}`\n", .{ @intFromPtr(registered_obj), str.items });
                        }
                    } else {
                        // std.debug.print(
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

                free(self, ObjTypeDef, obj_typedef);
            },
            .UpValue => {
                var obj_upvalue = ObjUpValue.cast(obj).?;
                if (obj_upvalue.closed) |value| {
                    if (value.isObj()) {
                        try freeObj(self, value.obj());
                    }
                }

                free(self, ObjUpValue, obj_upvalue);
            },
            .Closure => {
                var obj_closure = ObjClosure.cast(obj).?;
                obj_closure.deinit();

                free(self, ObjClosure, obj_closure);
            },
            .Function => {
                var obj_function = ObjFunction.cast(obj).?;
                obj_function.deinit();

                free(self, ObjFunction, obj_function);
            },
            .ObjectInstance => {
                var obj_objectinstance = ObjObjectInstance.cast(obj).?;

                // Calling eventual destructor method
                if (obj_objectinstance.object) |object| {
                    const collect_key = self.strings.get("collect").?;
                    if (object.methods.get(collect_key) != null) {
                        buzz_api.bz_invoke(
                            obj_objectinstance.vm,
                            obj_objectinstance.toValue(),
                            collect_key,
                            null,
                            0,
                            null,
                        );

                        // Remove void result of the collect call
                        _ = obj_objectinstance.vm.pop();
                    }
                }

                obj_objectinstance.deinit();

                free(self, ObjObjectInstance, obj_objectinstance);
            },
            .Object => {
                var obj_object = ObjObject.cast(obj).?;
                obj_object.deinit();

                free(self, ObjObject, obj_object);
            },
            .List => {
                var obj_list = ObjList.cast(obj).?;
                obj_list.deinit();

                free(self, ObjList, obj_list);
            },
            .Map => {
                var obj_map = ObjMap.cast(obj).?;
                obj_map.deinit();

                free(self, ObjMap, obj_map);
            },
            .Enum => {
                var obj_enum = ObjEnum.cast(obj).?;
                obj_enum.deinit();

                free(self, ObjEnum, obj_enum);
            },
            .EnumInstance => free(self, ObjEnumInstance, ObjEnumInstance.cast(obj).?),
            .Bound => free(self, ObjBoundMethod, ObjBoundMethod.cast(obj).?),
            .Native => free(self, ObjNative, ObjNative.cast(obj).?),
            .UserData => free(self, ObjUserData, ObjUserData.cast(obj).?),
            .Fiber => {
                var obj_fiber = ObjFiber.cast(obj).?;
                obj_fiber.fiber.deinit();

                free(self, ObjFiber, obj_fiber);
            },
            .ForeignStruct => {
                var obj_foreignstruct = ObjForeignStruct.cast(obj).?;

                self.freeMany(u8, obj_foreignstruct.data);

                free(self, ObjForeignStruct, obj_foreignstruct);
            },
        }
    }

    pub fn markValue(self: *Self, value: Value) !void {
        if (value.isObj()) {
            try self.markObj(value.obj());
        }
    }

    pub fn markFiber(self: *Self, fiber: *Fiber) !void {
        var current_fiber: ?*Fiber = fiber;
        while (current_fiber) |ufiber| {
            // Mark main fiber
            if (BuildOptions.gc_debug) {
                std.debug.print("MARKING STACK OF FIBER @{}\n", .{@intFromPtr(ufiber)});
            }
            var i: [*]Value = @ptrCast(fiber.stack);
            while (@intFromPtr(i) < @intFromPtr(fiber.stack_top)) : (i += 1) {
                try self.markValue(i[0]);
            }
            if (BuildOptions.gc_debug) {
                std.debug.print("DONE MARKING STACK OF FIBER @{}\n", .{@intFromPtr(ufiber)});
            }

            // Mark closure
            if (BuildOptions.gc_debug) {
                std.debug.print("MARKING FRAMES OF FIBER @{}\n", .{@intFromPtr(ufiber)});
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
                std.debug.print("DONE MARKING FRAMES OF FIBER @{}\n", .{@intFromPtr(ufiber)});
            }

            // Mark opened upvalues
            if (BuildOptions.gc_debug) {
                std.debug.print("MARKING UPVALUES OF FIBER @{}\n", .{@intFromPtr(ufiber)});
            }
            if (fiber.open_upvalues) |open_upvalues| {
                var upvalue: ?*ObjUpValue = open_upvalues;
                while (upvalue) |unwrapped| : (upvalue = unwrapped.next) {
                    try self.markObj(unwrapped.toObj());
                }
            }
            if (BuildOptions.gc_debug) {
                std.debug.print("DONE MARKING UPVALUES OF FIBER @{}\n", .{@intFromPtr(ufiber)});
            }

            current_fiber = ufiber.parent_fiber;
        }
    }

    fn markMethods(self: *Self) !void {
        if (BuildOptions.gc_debug) {
            std.debug.print("MARKING BASIC TYPES METHOD\n", .{});
        }
        // Mark basic types methods
        {
            var it = self.objfiber_members.iterator();
            while (it.next()) |umember| {
                try self.markObj(umember.key_ptr.*.toObj());
                try self.markObj(umember.value_ptr.*.toObj());
            }
        }

        {
            var it = self.objfiber_memberDefs.iterator();
            while (it.next()) |umember| {
                try self.markObj(umember.value_ptr.*.toObj());
            }
        }

        {
            var it = self.objpattern_members.iterator();
            while (it.next()) |kv| {
                try self.markObj(kv.key_ptr.*.toObj());
                try self.markObj(kv.value_ptr.*.toObj());
            }
        }

        {
            var it = self.objpattern_memberDefs.iterator();
            while (it.next()) |kv| {
                try self.markObj(kv.value_ptr.*.toObj());
            }
        }

        {
            var it = self.objstring_members.iterator();
            while (it.next()) |kv| {
                try self.markObj(kv.key_ptr.*.toObj());
                try self.markObj(kv.value_ptr.*.toObj());
            }
        }

        {
            var it = self.objstring_memberDefs.iterator();
            while (it.next()) |kv| {
                try self.markObj(kv.value_ptr.*.toObj());
            }
        }

        if (BuildOptions.gc_debug) {
            std.debug.print("DONE MARKING BASIC TYPES METHOD\n", .{});
        }
    }

    fn markRoots(self: *Self, vm: *VM) !void {
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
            for (kv.value_ptr.*.items) |global| {
                try self.markValue(global);
            }
        }

        // Mark current fiber and its parent fibers
        try markFiber(self, vm.current_fiber);

        // Mark globals
        if (BuildOptions.gc_debug) {
            std.debug.print("MARKING GLOBALS OF VM @{}\n", .{@intFromPtr(vm)});
        }
        for (vm.globals.items) |global| {
            try self.markValue(global);
        }
        if (BuildOptions.gc_debug) {
            std.debug.print("DONE MARKING GLOBALS OF VM @{}\n", .{@intFromPtr(vm)});
        }
    }

    fn traceReference(self: *Self) !void {
        if (BuildOptions.gc_debug) {
            std.debug.print("TRACING REFERENCE\n", .{});
        }
        while (self.gray_stack.items.len > 0) {
            try blackenObject(self, self.gray_stack.pop());
        }
        if (BuildOptions.gc_debug) {
            std.debug.print("DONE TRACING REFERENCE\n", .{});
        }
    }

    fn sweep(self: *Self, mode: Mode) !void {
        var swept: usize = self.bytes_allocated;

        var obj_count: usize = 0;
        var obj_node: ?*std.TailQueue(*Obj).Node = self.objects.first;
        var count: usize = 0;
        while (obj_node) |node| : (count += 1) {
            if (node.data.is_marked) {
                if (BuildOptions.gc_debug and mode == .Full) {
                    std.debug.print("UNMARKING @{}\n", .{@intFromPtr(node.data)});
                }
                // If not a full gc, we reset is_marked, this object is now 'old'
                node.data.is_marked = if (mode == .Full) false else node.data.is_marked;

                // If a young collection we don't reset is_marked flags and since we move all marked object
                // to the tail of the list, we can stop here, there's no more objects to collect
                if (mode == .Young) {
                    break;
                }

                obj_node = node.next;
            } else {
                var unreached: *Obj = node.data;
                obj_node = node.next;

                self.objects.remove(node);

                try freeObj(self, unreached);
                obj_count += 1;
            }
        }

        if (BuildOptions.gc_debug or BuildOptions.gc_debug_light) {
            if (swept < self.bytes_allocated) {
                std.debug.print("Warn: sweep gained memory, possibly due to an Object collector that takes up memory\n", .{});
            }

            std.debug.print(
                "\nSwept {} objects for {} bytes, now {} bytes\n",
                .{
                    obj_count,
                    @max(swept, self.bytes_allocated) - self.bytes_allocated,
                    self.bytes_allocated,
                },
            );
        }
    }

    pub fn collectGarbage(self: *Self) !void {
        var timer = std.time.Timer.start() catch unreachable;

        // Don't collect until a VM is actually running
        if (self.active_vms.count() == 0) {
            return;
        }

        const mode: Mode = if (self.bytes_allocated > self.next_full_gc and self.last_gc != null) .Full else .Young;

        if (BuildOptions.gc_debug or BuildOptions.gc_debug_light) {
            std.debug.print("-- gc starts mode {}, {} bytes, {} objects\n", .{ mode, self.bytes_allocated, self.objects.len });

            // try dumpStack(self);
        }

        var it = self.active_vms.iterator();
        while (it.next()) |kv| {
            var vm = kv.key_ptr.*;

            if (BuildOptions.gc_debug) {
                std.debug.print(
                    "\tMarking VM @{}, on fiber @{} and closure @{} (function @{} {s})\n",
                    .{
                        @intFromPtr(vm),
                        @intFromPtr(vm.current_fiber),
                        @intFromPtr(vm.currentFrame().?.closure),
                        @intFromPtr(vm.currentFrame().?.closure.function),
                        vm.currentFrame().?.closure.function.name.string,
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
        self.next_full_gc = self.next_gc * BuildOptions.next_full_gc_ratio;
        self.last_gc = mode;

        if (BuildOptions.gc_debug or BuildOptions.gc_debug_light) {
            std.debug.print("-- gc end, {} bytes, {} objects\n", .{ self.bytes_allocated, self.objects.len });
        }
        // std.debug.print("gc took {}ms\n", .{timer.read() / 1000000});

        self.gc_time += timer.read();
    }
};

pub const GarbageCollectorDebugger = struct {
    const Self = @This();

    pub const Ptr = struct {
        what: _obj.ObjType,
        allocated_at: ?Token,
        collected_at: ?Token = null,
    };

    allocator: std.mem.Allocator,
    tracker: std.AutoHashMap(*Obj, Ptr),

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .tracker = std.AutoHashMap(*Obj, Ptr).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.tracker.deinit();
    }

    pub fn allocated(self: *Self, ptr: *Obj, at: ?Token, what: _obj.ObjType) void {
        assert(self.tracker.get(ptr) == null);
        self.tracker.put(
            ptr,
            Ptr{
                .what = what,
                .allocated_at = at,
            },
        ) catch unreachable;
    }

    pub fn collected(self: *Self, ptr: *Obj, at: Token) void {
        if (self.tracker.getPtr(ptr)) |tracked| {
            tracked.collected_at = at;
        } else {
            unreachable;
        }
    }

    pub fn accessed(self: *Self, ptr: *Obj, at: ?Token) void {
        if (self.tracker.getPtr(ptr)) |tracked| {
            if (tracked.collected_at) |collected_at| {
                std.debug.print(
                    "Access to already collected {}:",
                    .{
                        tracked.what,
                    },
                );

                if (tracked.allocated_at) |allocated_at| {
                    std.debug.print(
                        "\nAllocated at {s}:{d}:{d}:",
                        .{
                            allocated_at.script_name,
                            allocated_at.line + 1,
                            allocated_at.column,
                        },
                    );
                    self.printTokenContext(allocated_at);
                } else {
                    std.debug.print("\n\nProbably allocated during compilation\n", .{});
                }

                std.debug.print(
                    "\nCollected at {s}:{d}:{d}:",
                    .{
                        collected_at.script_name,
                        collected_at.line + 1,
                        collected_at.column,
                    },
                );
                self.printTokenContext(collected_at);

                if (at) |accessed_at| {
                    std.debug.print(
                        "\nBad access at {s}:{d}:{d}:",
                        .{
                            accessed_at.script_name,
                            accessed_at.line + 1,
                            accessed_at.column,
                        },
                    );
                    self.printTokenContext(accessed_at);
                } else {
                    std.debug.print("\nProbably accessed at VM start up\n", .{});
                }
            }
        } else {
            unreachable;
        }
    }

    fn printTokenContext(self: *Self, token: Token) void {
        const lines: std.ArrayList([]const u8) = token.getLines(self.allocator, 3) catch unreachable;
        defer lines.deinit();
        var report_line = std.ArrayList(u8).init(self.allocator);
        defer report_line.deinit();
        var writer = report_line.writer();

        writer.print("\n", .{}) catch unreachable;
        var l: usize = if (token.line > 0) token.line - 1 else 0;
        for (lines.items) |line| {
            if (l != token.line) {
                writer.print("\u{001b}[2m", .{}) catch unreachable;
            }

            var prefix_len: usize = report_line.items.len;
            writer.print(" {: >5} |", .{l + 1}) catch unreachable;
            prefix_len = report_line.items.len - prefix_len;
            writer.print(" {s}\n\u{001b}[0m", .{line}) catch unreachable;

            if (l == token.line) {
                writer.writeByteNTimes(' ', (if (token.column > 0) token.column - 1 else 0) + prefix_len) catch unreachable;
                writer.print("\u{001b}[31m^\u{001b}[0m\n", .{}) catch unreachable;
            }

            l += 1;
        }
        std.debug.print("{s}", .{report_line.items});
    }
};
