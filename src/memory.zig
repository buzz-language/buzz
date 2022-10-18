const std = @import("std");
const builtin = @import("builtin");
const _vm = @import("./vm.zig");
const Fiber = _vm.Fiber;
const _value = @import("./value.zig");
const _obj = @import("./obj.zig");
const dumpStack = @import("./disassembler.zig").dumpStack;
const Config = @import("./config.zig").Config;
const VM = @import("./vm.zig").VM;
const assert = std.debug.assert;

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
        try type_def.toString(type_def_buf.writer());
        const type_def_str: []const u8 = type_def_buf.items;

        // We don't return a cached version of a placeholder since they all maintain a particular state (link)
        if (type_def.def_type != .Placeholder) {
            if (self.registry.get(type_def_str)) |type_def_ptr| {
                self.gc.allocator.free(type_def_str); // If already in map, we don't need this string anymore
                return type_def_ptr;
            }
        }

        var type_def_ptr: *ObjTypeDef = try self.gc.allocateObject(ObjTypeDef, type_def);

        if (Config.debug_placeholders) {
            std.debug.print("`{s}` @{}\n", .{ type_def_str, @ptrToInt(type_def_ptr) });
        }
        _ = try self.registry.put(type_def_str, type_def_ptr);

        return type_def_ptr;
    }

    pub fn setTypeDef(self: *Self, type_def: *ObjTypeDef) !void {
        const type_def_str: []const u8 = try type_def.toStringAlloc(self.gc.allocator);

        assert(type_def.def_type != .Placeholder);

        _ = try self.registry.put(type_def_str, type_def);
    }

    pub inline fn getTypeDefByName(self: *Self, name: []const u8) ?*ObjTypeDef {
        return self.registry.get(name);
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
    next_gc: usize = if (builtin.mode == .Debug) 1024 else 1024 * Config.gc.initial_gc,
    next_full_gc: usize = if (builtin.mode == .Debug) 1024 else 1024 * Config.gc.initial_gc,
    objects: std.TailQueue(*Obj) = .{},
    gray_stack: std.ArrayList(*Obj),
    active_vms: std.AutoHashMap(*VM, void),

    // Types we generaly don't wan't to ever be collected
    objfiber_members: std.AutoHashMap(*ObjString, *ObjNative),
    objfiber_memberDefs: std.StringHashMap(*ObjTypeDef),
    objpattern_members: std.AutoHashMap(*ObjString, *ObjNative),
    objpattern_memberDefs: std.StringHashMap(*ObjTypeDef),
    objstring_members: std.AutoHashMap(*ObjString, *ObjNative),
    objstring_memberDefs: std.StringHashMap(*ObjTypeDef),

    // Objects pools
    objStringPool: std.ArrayList(*ObjString),
    objTypeDefPool: std.ArrayList(*ObjTypeDef),
    objUpValuePool: std.ArrayList(*ObjUpValue),
    objClosurePool: std.ArrayList(*ObjClosure),
    objFunctionPool: std.ArrayList(*ObjFunction),
    objObjectInstancePool: std.ArrayList(*ObjObjectInstance),
    objObjectPool: std.ArrayList(*ObjObject),
    objListPool: std.ArrayList(*ObjList),
    objMapPool: std.ArrayList(*ObjMap),
    objEnumPool: std.ArrayList(*ObjEnum),
    objEnumInstancePool: std.ArrayList(*ObjEnumInstance),
    objBoundMethodPool: std.ArrayList(*ObjBoundMethod),
    objNativePool: std.ArrayList(*ObjNative),
    objUserDataPool: std.ArrayList(*ObjUserData),
    objPatternPool: std.ArrayList(*ObjPattern),
    objFiberPool: std.ArrayList(*ObjFiber),

    full_collection_count: usize = 0,
    light_collection_count: usize = 0,
    max_allocated: usize = 0,

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .allocator = allocator,
            .strings = std.StringHashMap(*ObjString).init(allocator),
            .type_registry = undefined,
            .gray_stack = std.ArrayList(*Obj).init(allocator),
            .active_vms = std.AutoHashMap(*VM, void).init(allocator),

            .objfiber_members = std.AutoHashMap(*ObjString, *ObjNative).init(allocator),
            .objfiber_memberDefs = std.StringHashMap(*ObjTypeDef).init(allocator),
            .objpattern_members = std.AutoHashMap(*ObjString, *ObjNative).init(allocator),
            .objpattern_memberDefs = std.StringHashMap(*ObjTypeDef).init(allocator),
            .objstring_members = std.AutoHashMap(*ObjString, *ObjNative).init(allocator),
            .objstring_memberDefs = std.StringHashMap(*ObjTypeDef).init(allocator),

            .objStringPool = std.ArrayList(*ObjString).init(allocator),
            .objTypeDefPool = std.ArrayList(*ObjTypeDef).init(allocator),
            .objUpValuePool = std.ArrayList(*ObjUpValue).init(allocator),
            .objClosurePool = std.ArrayList(*ObjClosure).init(allocator),
            .objFunctionPool = std.ArrayList(*ObjFunction).init(allocator),
            .objObjectInstancePool = std.ArrayList(*ObjObjectInstance).init(allocator),
            .objObjectPool = std.ArrayList(*ObjObject).init(allocator),
            .objListPool = std.ArrayList(*ObjList).init(allocator),
            .objMapPool = std.ArrayList(*ObjMap).init(allocator),
            .objEnumPool = std.ArrayList(*ObjEnum).init(allocator),
            .objEnumInstancePool = std.ArrayList(*ObjEnumInstance).init(allocator),
            .objBoundMethodPool = std.ArrayList(*ObjBoundMethod).init(allocator),
            .objNativePool = std.ArrayList(*ObjNative).init(allocator),
            .objUserDataPool = std.ArrayList(*ObjUserData).init(allocator),
            .objPatternPool = std.ArrayList(*ObjPattern).init(allocator),
            .objFiberPool = std.ArrayList(*ObjFiber).init(allocator),
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

        self.objfiber_members.deinit();
        self.objfiber_memberDefs.deinit();
        self.objpattern_members.deinit();
        self.objpattern_memberDefs.deinit();
        self.objstring_members.deinit();
        self.objstring_memberDefs.deinit();

        self.objStringPool.deinit();
        self.objTypeDefPool.deinit();
        self.objUpValuePool.deinit();
        self.objClosurePool.deinit();
        self.objFunctionPool.deinit();
        self.objObjectInstancePool.deinit();
        self.objObjectPool.deinit();
        self.objListPool.deinit();
        self.objMapPool.deinit();
        self.objEnumPool.deinit();
        self.objEnumInstancePool.deinit();
        self.objBoundMethodPool.deinit();
        self.objNativePool.deinit();
        self.objUserDataPool.deinit();
        self.objPatternPool.deinit();
        self.objFiberPool.deinit();
    }

    pub fn allocate(self: *Self, comptime T: type) !*T {
        self.bytes_allocated += @sizeOf(T);

        if (self.bytes_allocated > self.max_allocated) {
            self.max_allocated = self.bytes_allocated;
        }

        if (self.bytes_allocated > self.next_gc and !Config.debug_turn_off_gc) {
            try self.collectGarbage();
        }

        var allocated = try self.allocator.create(T);

        if (Config.debug_gc) {
            std.debug.print("Allocated @{} {}\n", .{ @ptrToInt(allocated), T });
        }

        return allocated;
    }

    pub fn allocateMany(self: *Self, comptime T: type, count: usize) ![]T {
        self.bytes_allocated += (@sizeOf(T) * count);

        if (self.bytes_allocated > self.max_allocated) {
            self.max_allocated = self.bytes_allocated;
        }

        if (self.bytes_allocated > self.next_gc and !Config.debug_turn_off_gc) {
            try self.collectGarbage();
        }

        return try self.allocator.alloc(T, count);
    }

    pub fn allocateObject(self: *Self, comptime T: type, data: T) !*T {
        // var before: usize = self.bytes_allocated;

        var recycled: ?*T = switch (T) {
            ObjString => if (self.objStringPool.items.len > 0) self.objStringPool.pop() else null,
            ObjTypeDef => if (self.objTypeDefPool.items.len > 0) self.objTypeDefPool.pop() else null,
            ObjUpValue => if (self.objUpValuePool.items.len > 0) self.objUpValuePool.pop() else null,
            ObjClosure => if (self.objClosurePool.items.len > 0) self.objClosurePool.pop() else null,
            ObjFunction => if (self.objFunctionPool.items.len > 0) self.objFunctionPool.pop() else null,
            ObjObjectInstance => if (self.objObjectInstancePool.items.len > 0) self.objObjectInstancePool.pop() else null,
            ObjObject => if (self.objObjectPool.items.len > 0) self.objObjectPool.pop() else null,
            ObjList => if (self.objListPool.items.len > 0) self.objListPool.pop() else null,
            ObjMap => if (self.objMapPool.items.len > 0) self.objMapPool.pop() else null,
            ObjEnum => if (self.objEnumPool.items.len > 0) self.objEnumPool.pop() else null,
            ObjEnumInstance => if (self.objEnumInstancePool.items.len > 0) self.objEnumInstancePool.pop() else null,
            ObjBoundMethod => if (self.objBoundMethodPool.items.len > 0) self.objBoundMethodPool.pop() else null,
            ObjNative => if (self.objNativePool.items.len > 0) self.objNativePool.pop() else null,
            ObjUserData => if (self.objUserDataPool.items.len > 0) self.objUserDataPool.pop() else null,
            ObjPattern => if (self.objPatternPool.items.len > 0) self.objPatternPool.pop() else null,
            ObjFiber => if (self.objFiberPool.items.len > 0) self.objFiberPool.pop() else null,
            else => null,
        };

        if (Config.debug_gc and recycled != null) {
            std.debug.print("Recycled @{}\n", .{@ptrToInt(recycled.?)});
        }

        var obj: *T = recycled orelse try self.allocate(T);
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
            else => {},
        };

        // if (Config.debug_gc) {
        //     std.debug.print("allocated {*} {*}\n", .{ obj, object });
        //     std.debug.print("(from {}) {} allocated, total {}\n", .{ before, self.bytes_allocated - before, self.bytes_allocated });
        // }

        // Add new object at start of vm.objects linked list
        try self.addObject(object);

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
        if (self.strings.get(chars)) |interned| {
            return interned;
        } else {
            var string: *ObjString = try allocateObject(
                self,
                ObjString,
                ObjString{ .string = chars },
            );

            // std.debug.print("allocated new string @{} `{s}`\n", .{ @ptrToInt(&string.obj), string.string });
            try self.strings.put(string.string, string);

            return string;
        }
    }

    pub fn copyString(self: *Self, chars: []const u8) !*ObjString {
        if (self.strings.get(chars)) |interned| {
            return interned;
        }

        var copy: []u8 = try self.allocateMany(u8, chars.len);
        std.mem.copy(u8, copy, chars);

        if (Config.debug_gc) {
            std.debug.print("Allocated slice {*} `{s}`\n", .{ copy, copy });
        }

        return try allocateString(self, copy);
    }

    fn free(self: *Self, comptime T: type, pointer: *T) void {
        if (Config.debug_gc) {
            std.debug.print("Going to free {*}\n", .{pointer});
        }

        self.bytes_allocated -= @sizeOf(T);
        self.allocator.destroy(pointer);

        if (Config.debug_gc) {
            std.debug.print(
                "(from {}), freed {}, {} allocated\n",
                .{ self.bytes_allocated + @sizeOf(T), @sizeOf(T), self.bytes_allocated },
            );
        }
    }

    fn freeMany(self: *Self, comptime T: type, pointer: []const T) void {
        if (Config.debug_gc) {
            std.debug.print("Going to free slice {*} `{s}`\n", .{ pointer, pointer });
        }

        const n: usize = (@sizeOf(T) * pointer.len);
        self.bytes_allocated -= n;
        self.allocator.free(pointer);

        if (Config.debug_gc) {
            std.debug.print(
                "(from {}), freed {}, {} allocated\n",
                .{
                    self.bytes_allocated + n,
                    n,
                    self.bytes_allocated,
                },
            );
        }
    }

    pub fn markObjDirty(self: *Self, obj: *Obj) !void {
        if (!obj.is_dirty) {
            obj.is_dirty = true;

            // std.debug.print(
            //     "Marked obj @{} {} dirty, gray_stack @{} or GC @{} will be {} items long\n",
            //     .{
            //         @ptrToInt(obj),
            //         obj.obj_type,
            //         @ptrToInt(&self.gray_stack),
            //         @ptrToInt(self),
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
            if (Config.debug_gc) {
                std.debug.print("{*} {s} already marked or old\n", .{ obj, try valueToStringAlloc(self.allocator, Value{ .Obj = obj }) });
            }
            return;
        }

        if (Config.debug_gc) {
            std.debug.print("marking {*}: ", .{obj});
            std.debug.print("{s}\n", .{try valueToStringAlloc(self.allocator, Value{ .Obj = obj })});
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
        if (Config.debug_gc) {
            std.debug.print(
                "blackening @{} {}\n",
                .{
                    @ptrToInt(obj),
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
        };

        if (Config.debug_gc) {
            std.debug.print(
                "done blackening @{} {}\n",
                .{
                    @ptrToInt(obj),
                    obj.obj_type,
                },
            );
        }
    }

    fn freeObj(self: *Self, obj: *Obj) (std.mem.Allocator.Error || std.fmt.BufPrintError)!void {
        if (Config.debug_gc) {
            std.debug.print(">> freeing {} {}\n", .{ @ptrToInt(obj), obj.obj_type });
        }

        self.allocator.destroy(obj.node.?);

        switch (obj.obj_type) {
            .String => {
                var obj_string = ObjString.cast(obj).?;

                // Remove it from interned strings
                _ = self.strings.remove(obj_string.string);

                // Pool can't be larger than 10% of total allocated memory
                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjString)) * 0.1);
                if (self.objStringPool.items.len < pool_threshold) {
                    try self.objStringPool.append(obj_string);
                } else {
                    freeMany(self, u8, obj_string.string);
                    free(self, ObjString, obj_string);
                }
            },
            .Pattern => {
                var obj_pattern = ObjPattern.cast(obj).?;

                // Pool can't be larger than 10% of total allocated memory
                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjPattern)) * 0.1);
                if (self.objPatternPool.items.len < pool_threshold) {
                    try self.objPatternPool.append(obj_pattern);
                } else {
                    pcre.pcre_free.?(obj_pattern.pattern);
                    free(self, ObjPattern, obj_pattern);
                }
            },
            .Type => {
                var obj_typedef = ObjTypeDef.cast(obj).?;

                const str = try obj_typedef.toStringAlloc(self.allocator);
                defer self.allocator.free(str);

                if (self.type_registry.registry.get(str)) |registered_obj| {
                    if (registered_obj == obj_typedef) {
                        _ = self.type_registry.registry.remove(str);
                        if (Config.debug_gc) {
                            std.debug.print("Removed registered type @{} `{s}`\n", .{ @ptrToInt(registered_obj), str });
                        }
                    }
                }

                // Pool can't be larger than 10% of total allocated memory
                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjTypeDef)) * 0.1);
                if (self.objTypeDefPool.items.len < pool_threshold) {
                    try self.objTypeDefPool.append(obj_typedef);
                } else {
                    obj_typedef.deinit();
                    free(self, ObjTypeDef, obj_typedef);
                }
            },
            .UpValue => {
                var obj_upvalue = ObjUpValue.cast(obj).?;
                if (obj_upvalue.closed) |value| {
                    if (value == .Obj) {
                        try freeObj(self, value.Obj);
                    }
                }

                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjUpValue)) * 0.1);
                if (self.objUpValuePool.items.len < pool_threshold) {
                    try self.objUpValuePool.append(obj_upvalue);
                } else {
                    free(self, ObjUpValue, obj_upvalue);
                }
            },
            .Closure => {
                var obj_closure = ObjClosure.cast(obj).?;

                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjClosure)) * 0.1);
                if (self.objClosurePool.items.len < pool_threshold) {
                    try self.objClosurePool.append(obj_closure);
                } else {
                    obj_closure.deinit();
                    free(self, ObjClosure, obj_closure);
                }
            },
            .Function => {
                var obj_function = ObjFunction.cast(obj).?;

                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjFunction)) * 0.1);
                if (self.objFunctionPool.items.len < pool_threshold) {
                    try self.objFunctionPool.append(obj_function);
                } else {
                    obj_function.deinit();
                    free(self, ObjFunction, obj_function);
                }
            },
            .ObjectInstance => {
                var obj_objectinstance = ObjObjectInstance.cast(obj).?;

                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjObjectInstance)) * 0.1);
                if (self.objObjectInstancePool.items.len < pool_threshold) {
                    try self.objObjectInstancePool.append(obj_objectinstance);
                } else {
                    obj_objectinstance.deinit();
                    free(self, ObjObjectInstance, obj_objectinstance);
                }
            },
            .Object => {
                var obj_object = ObjObject.cast(obj).?;

                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjObject)) * 0.1);
                if (self.objObjectPool.items.len < pool_threshold) {
                    try self.objObjectPool.append(obj_object);
                } else {
                    obj_object.deinit();
                    free(self, ObjObject, obj_object);
                }
            },
            .List => {
                var obj_list = ObjList.cast(obj).?;

                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjList)) * 0.1);
                if (self.objListPool.items.len < pool_threshold) {
                    try self.objListPool.append(obj_list);
                } else {
                    obj_list.deinit();
                    free(self, ObjList, obj_list);
                }
            },
            .Map => {
                var obj_map = ObjMap.cast(obj).?;

                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjMap)) * 0.1);
                if (self.objMapPool.items.len < pool_threshold) {
                    try self.objMapPool.append(obj_map);
                } else {
                    obj_map.deinit();
                    free(self, ObjMap, obj_map);
                }
            },
            .Enum => {
                var obj_enum = ObjEnum.cast(obj).?;

                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjEnum)) * 0.1);
                if (self.objEnumPool.items.len < pool_threshold) {
                    try self.objEnumPool.append(obj_enum);
                } else {
                    obj_enum.deinit();
                    free(self, ObjEnum, obj_enum);
                }
            },
            .EnumInstance => {
                var obj_enuminstance = ObjEnumInstance.cast(obj).?;

                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjEnumInstance)) * 0.1);
                if (self.objEnumInstancePool.items.len < pool_threshold) {
                    try self.objEnumInstancePool.append(obj_enuminstance);
                } else {
                    free(self, ObjEnumInstance, ObjEnumInstance.cast(obj).?);
                }
            },
            .Bound => {
                var obj_boundmethod = ObjBoundMethod.cast(obj).?;

                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjBoundMethod)) * 0.1);
                if (self.objBoundMethodPool.items.len < pool_threshold) {
                    try self.objBoundMethodPool.append(obj_boundmethod);
                } else {
                    free(self, ObjBoundMethod, ObjBoundMethod.cast(obj).?);
                }
            },
            .Native => {
                var obj_native = ObjNative.cast(obj).?;

                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjNative)) * 0.1);
                if (self.objNativePool.items.len < pool_threshold) {
                    try self.objNativePool.append(obj_native);
                } else {
                    free(self, ObjNative, ObjNative.cast(obj).?);
                }
            },
            .UserData => {
                var obj_userdata = ObjUserData.cast(obj).?;

                const pool_threshold = @floatToInt(usize, @intToFloat(f64, self.bytes_allocated / @sizeOf(ObjUserData)) * 0.1);
                if (self.objUserDataPool.items.len < pool_threshold) {
                    try self.objUserDataPool.append(obj_userdata);
                } else {
                    free(self, ObjUserData, ObjUserData.cast(obj).?);
                }
            },
            .Fiber => {
                var obj_fiber = ObjFiber.cast(obj).?;
                obj_fiber.fiber.deinit();
                free(self, ObjFiber, obj_fiber);
            },
        }
    }

    pub fn markValue(self: *Self, value: Value) !void {
        if (value == .Obj) {
            try self.markObj(value.Obj);
        }
    }

    pub fn markFiber(self: *Self, fiber: *Fiber) !void {
        var current_fiber: ?*Fiber = fiber;
        while (current_fiber) |ufiber| {
            // Mark main fiber
            if (Config.debug_gc) {
                std.debug.print("MARKING STACK OF FIBER @{}\n", .{@ptrToInt(ufiber)});
            }
            var i = @ptrCast([*]Value, fiber.stack);
            while (@ptrToInt(i) < @ptrToInt(fiber.stack_top)) : (i += 1) {
                try self.markValue(i[0]);
            }
            if (Config.debug_gc) {
                std.debug.print("DONE MARKING STACK OF FIBER @{}\n", .{@ptrToInt(ufiber)});
            }

            // Mark closure
            if (Config.debug_gc) {
                std.debug.print("MARKING FRAMES OF FIBER @{}\n", .{@ptrToInt(ufiber)});
            }
            for (fiber.frames.items) |frame| {
                try self.markObj(frame.closure.toObj());
                for (frame.error_handlers.items) |handler| {
                    try self.markObj(handler.toObj());
                }
            }
            if (Config.debug_gc) {
                std.debug.print("DONE MARKING FRAMES OF FIBER @{}\n", .{@ptrToInt(ufiber)});
            }

            // Mark opened upvalues
            if (Config.debug_gc) {
                std.debug.print("MARKING UPVALUES OF FIBER @{}\n", .{@ptrToInt(ufiber)});
            }
            if (fiber.open_upvalues) |open_upvalues| {
                var upvalue: ?*ObjUpValue = open_upvalues;
                while (upvalue) |unwrapped| : (upvalue = unwrapped.next) {
                    try self.markObj(unwrapped.toObj());
                }
            }
            if (Config.debug_gc) {
                std.debug.print("DONE MARKING UPVALUES OF FIBER @{}\n", .{@ptrToInt(ufiber)});
            }

            current_fiber = ufiber.parent_fiber;
        }
    }

    fn markMethods(self: *Self) !void {
        if (Config.debug_gc) {
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

        if (Config.debug_gc) {
            std.debug.print("DONE MARKING BASIC TYPES METHOD\n", .{});
        }
    }

    fn markRoots(self: *Self, vm: *VM) !void {
        try self.markMethods();

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
        if (Config.debug_gc) {
            std.debug.print("MARKING GLOBALS OF VM @{}\n", .{@ptrToInt(vm)});
        }
        for (vm.globals.items) |global| {
            try self.markValue(global);
        }
        if (Config.debug_gc) {
            std.debug.print("DONE MARKING GLOBALS OF VM @{}\n", .{@ptrToInt(vm)});
        }
    }

    fn traceReference(self: *Self) !void {
        if (Config.debug_gc) {
            std.debug.print("TRACING REFERENCE\n", .{});
        }
        while (self.gray_stack.items.len > 0) {
            try blackenObject(self, self.gray_stack.pop());
        }
        if (Config.debug_gc) {
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
                if (Config.debug_gc and mode == .Full) {
                    std.debug.print("UNMARKING @{}\n", .{@ptrToInt(node.data)});
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

        if (Config.debug_gc or Config.debug_gc_light) {
            std.debug.print("\nSwept {} objects for {} bytes, now {} bytes\n", .{ obj_count, swept - self.bytes_allocated, self.bytes_allocated });
        }
    }

    pub fn collectGarbage(self: *Self) !void {
        // var timer = std.time.Timer.start() catch unreachable;

        // Don't collect until a VM is actually running
        if (self.active_vms.count() == 0) {
            return;
        }

        const mode: Mode = if (self.bytes_allocated > self.next_full_gc) .Full else .Young;

        if (Config.debug_gc or Config.debug_gc_light) {
            std.debug.print("-- gc starts mode {}, {} bytes, {} objects\n", .{ mode, self.bytes_allocated, self.objects.len });

            // try dumpStack(self);
        }

        var it = self.active_vms.iterator();
        while (it.next()) |kv| {
            var vm = kv.key_ptr.*;

            if (Config.debug_gc) {
                std.debug.print(
                    "\tMarking VM @{}, on fiber @{} and closure @{} (function @{} {s})\n",
                    .{
                        @ptrToInt(vm),
                        @ptrToInt(vm.current_fiber),
                        @ptrToInt(vm.currentFrame().?.closure),
                        @ptrToInt(vm.currentFrame().?.closure.function),
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

        self.next_gc = self.bytes_allocated * Config.gc.next_gc_ratio;
        self.next_full_gc = self.next_gc * Config.gc.next_full_gc_ratio;

        if (Config.debug_gc or Config.debug_gc_light) {
            std.debug.print("-- gc end, {} bytes, {} objects\n", .{ self.bytes_allocated, self.objects.len });
        }
        // std.debug.print("gc took {}ms\n", .{timer.read() / 1000000});
    }
};
