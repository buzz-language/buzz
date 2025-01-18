const std = @import("std");
const builtin = @import("builtin");
const _vm = @import("vm.zig");
const Fiber = _vm.Fiber;
const _value = @import("value.zig");
const _obj = @import("obj.zig");
const dumpStack = @import("disassembler.zig").dumpStack;
const BuildOptions = @import("build_options");
const VM = @import("vm.zig").VM;
const assert = std.debug.assert;
const Token = @import("Token.zig");
const buzz_api = @import("buzz_api.zig");
const Reporter = @import("Reporter.zig");
const is_wasm = builtin.cpu.arch.isWasm();
const io = @import("io.zig");

const Value = _value.Value;
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
const ObjForeignContainer = _obj.ObjForeignContainer;
const ObjRange = _obj.ObjRange;

pub const TypeRegistry = struct {
    const Self = @This();

    gc: *GarbageCollector,
    registry: std.StringHashMap(*ObjTypeDef),

    // Common types we reuse all the time
    void_type: *ObjTypeDef,
    str_type: *ObjTypeDef,
    int_type: *ObjTypeDef,
    float_type: *ObjTypeDef,
    bool_type: *ObjTypeDef,
    any_type: *ObjTypeDef,
    pat_type: *ObjTypeDef,
    ud_type: *ObjTypeDef,
    rg_type: *ObjTypeDef,

    // Buffer resused when we build a type key
    type_def_key_buffer: std.ArrayList(u8),

    pub fn init(gc: *GarbageCollector) !Self {
        var self = Self{
            .gc = gc,
            .registry = .init(gc.allocator),
            .type_def_key_buffer = .init(gc.allocator),
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

    pub fn deinit(self: *Self) void {
        self.registry.deinit();
        self.type_def_key_buffer.deinit();
    }

    pub fn dump(self: *Self) void {
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

    pub fn getTypeDef(self: *Self, type_def: ObjTypeDef) !*ObjTypeDef {
        self.type_def_key_buffer.shrinkRetainingCapacity(0);
        try type_def.toString(&self.type_def_key_buffer.writer());

        // We don't return a cached version of a placeholder since they all maintain a particular state (link)
        if (type_def.def_type != .Placeholder) {
            if (self.registry.get(self.type_def_key_buffer.items)) |type_def_ptr| {
                return type_def_ptr;
            }
        }

        const type_def_ptr = try self.gc.allocateObject(ObjTypeDef, type_def);

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
        var copy = try self.type_def_key_buffer.clone();
        copy.shrinkAndFree(copy.items.len);

        const slot = try self.registry.getOrPut(copy.items);

        slot.value_ptr.* = type_def_ptr;

        if (slot.found_existing) {
            copy.deinit();
        }

        return type_def_ptr;
    }

    pub fn setTypeDef(self: *Self, type_def: *ObjTypeDef) !void {
        const type_def_str = try type_def.toStringAlloc(self.gc.allocator);

        assert(type_def.def_type != .Placeholder);

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

    pub inline fn getTypeDefByName(self: *Self, name: []const u8) ?*ObjTypeDef {
        return self.registry.get(name);
    }

    pub fn mark(self: *Self) !void {
        var it = self.registry.iterator();
        while (it.next()) |kv| {
            try self.gc.markObj(@constCast(kv.value_ptr.*).toObj());
        }
    }
};

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
pub const GarbageCollector = struct {
    const Self = @This();

    const Mode = enum {
        Young,
        // Old,
        Full,
    };

    allocator: std.mem.Allocator,
    strings: std.StringHashMapUnmanaged(*ObjString),
    type_registry: TypeRegistry,
    bytes_allocated: usize = 0,
    // next_gc == next_full_gc at first so the first cycle is a full gc
    next_gc: usize = if (builtin.mode == .Debug) 1024 else 1024 * BuildOptions.initial_gc,
    next_full_gc: usize = if (builtin.mode == .Debug) 1024 else 1024 * BuildOptions.initial_gc,
    last_gc: ?Mode = null,
    objects: std.DoublyLinkedList(*Obj) = .{},
    gray_stack: std.ArrayListUnmanaged(*Obj),
    active_vms: std.AutoHashMapUnmanaged(*VM, void),
    // Obj being collected, useful to avoid setting object instance dirty while running its collector method
    obj_collected: ?*Obj = null,

    debugger: ?GarbageCollectorDebugger,
    where: ?Token = null,

    // Types we generaly don't wan't to ever be collected
    objfiber_members: []?*ObjNative,
    objfiber_memberDefs: []?*ObjTypeDef,
    objpattern_members: []?*ObjNative,
    objpattern_memberDefs: []?*ObjTypeDef,
    objstring_members: []?*ObjNative,
    objstring_memberDefs: []?*ObjTypeDef,
    objrange_memberDefs: []?*ObjTypeDef,
    objrange_members: []?*ObjNative,

    full_collection_count: usize = 0,
    light_collection_count: usize = 0,
    max_allocated: usize = 0,

    gc_time: usize = 0,

    pub fn init(allocator: std.mem.Allocator) !Self {
        const self = Self{
            .allocator = allocator,
            .strings = std.StringHashMapUnmanaged(*ObjString){},
            .type_registry = undefined,
            .gray_stack = std.ArrayListUnmanaged(*Obj){},
            .active_vms = std.AutoHashMapUnmanaged(*VM, void){},
            .debugger = if (BuildOptions.gc_debug_access) GarbageCollectorDebugger.init(allocator) else null,

            .objfiber_members = try allocator.alloc(?*ObjNative, ObjFiber.members.len),
            .objfiber_memberDefs = try allocator.alloc(?*ObjTypeDef, ObjFiber.members.len),
            .objpattern_members = try allocator.alloc(?*ObjNative, ObjPattern.members.len),
            .objpattern_memberDefs = try allocator.alloc(?*ObjTypeDef, ObjPattern.members.len),
            .objstring_members = try allocator.alloc(?*ObjNative, ObjString.members.len),
            .objstring_memberDefs = try allocator.alloc(?*ObjTypeDef, ObjString.members.len),
            .objrange_members = try allocator.alloc(?*ObjNative, ObjRange.members.len),
            .objrange_memberDefs = try allocator.alloc(?*ObjTypeDef, ObjRange.members.len),
        };

        for (0..ObjFiber.members.len) |i| {
            self.objfiber_members[i] = null;
            self.objfiber_memberDefs[i] = null;
        }

        for (0..ObjPattern.members.len) |i| {
            self.objpattern_members[i] = null;
            self.objpattern_memberDefs[i] = null;
        }

        for (0..ObjString.members.len) |i| {
            self.objstring_members[i] = null;
            self.objstring_memberDefs[i] = null;
        }

        for (0..ObjRange.members.len) |i| {
            self.objrange_members[i] = null;
            self.objrange_memberDefs[i] = null;
        }

        return self;
    }

    pub fn registerVM(self: *Self, vm: *VM) !void {
        try self.active_vms.put(
            self.allocator,
            vm,
            {},
        );
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

        self.allocator.free(self.objfiber_members);
        self.allocator.free(self.objfiber_memberDefs);
        self.allocator.free(self.objpattern_members);
        self.allocator.free(self.objpattern_memberDefs);
        self.allocator.free(self.objstring_members);
        self.allocator.free(self.objstring_memberDefs);
        self.allocator.free(self.objrange_members);
        self.allocator.free(self.objrange_memberDefs);
    }

    pub fn allocate(self: *Self, comptime T: type) !*T {
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
            io.print(
                "Allocated @{} {} for {} (now {}/{})\n",
                .{
                    @intFromPtr(allocated),
                    T,
                    std.fmt.fmtIntSizeDec(@sizeOf(T)),
                    std.fmt.fmtIntSizeDec(self.bytes_allocated),
                    std.fmt.fmtIntSizeDec(self.next_gc),
                },
            );
        }

        if (!is_wasm) {
            self.gc_time += timer.read();
        }
        return allocated;
    }

    pub fn allocateMany(self: *Self, comptime T: type, count: usize) ![]T {
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

    pub fn allocateObject(self: *Self, comptime T: type, data: T) !*T {
        // var before: usize = self.bytes_allocated;

        const obj: *T = try self.allocate(T);
        obj.* = data;

        const object: *Obj = switch (T) {
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
            ObjForeignContainer => ObjForeignContainer.toObj(obj),
            ObjRange => ObjRange.toObj(obj),
            else => {},
        };

        // if (BuildOptions.gc_debug) {
        //     io.print("allocated {*} {*}\n", .{ obj, object });
        //     io.print("(from {}) {} allocated, total {}\n", .{ before, self.bytes_allocated - before, self.bytes_allocated });
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
                    ObjForeignContainer => .ForeignContainer,
                    ObjRange => .Range,
                    else => {},
                },
            );
        }

        return obj;
    }

    fn addObject(self: *Self, obj: *Obj) !void {
        const new_node = try self.allocator.create(std.DoublyLinkedList(*Obj).Node);
        new_node.* = .{
            .data = obj,
        };
        obj.node = new_node;
        self.objects.prepend(new_node);
    }

    pub fn allocateString(self: *Self, chars: []const u8) !*ObjString {
        const string: *ObjString = try allocateObject(
            self,
            ObjString,
            ObjString{ .string = chars },
        );

        try self.strings.put(
            self.allocator,
            string.string,
            string,
        );

        return string;
    }

    pub fn copyString(self: *Self, chars: []const u8) !*ObjString {
        if (self.strings.get(chars)) |interned| {
            return interned;
        }

        const copy: []u8 = try self.allocateMany(u8, chars.len);
        std.mem.copyForwards(u8, copy, chars);

        if (BuildOptions.gc_debug) {
            io.print("Allocated slice {*} `{s}`\n", .{ copy, copy });
        }

        return try allocateString(self, copy);
    }

    fn free(self: *Self, comptime T: type, pointer: *T) void {
        var timer = if (!is_wasm) std.time.Timer.start() catch unreachable else {};

        if (BuildOptions.gc_debug) {
            io.print("Going to free {*}\n", .{pointer});
        }

        self.bytes_allocated -= @sizeOf(T);
        self.allocator.destroy(pointer);

        if (BuildOptions.gc_debug) {
            io.print(
                "(from {}), collected {}, {} allocated\n",
                .{
                    std.fmt.fmtIntSizeDec(self.bytes_allocated + @sizeOf(T)),
                    std.fmt.fmtIntSizeDec(@sizeOf(T)),
                    std.fmt.fmtIntSizeDec(self.bytes_allocated),
                },
            );
        }

        if (!is_wasm) {
            self.gc_time += timer.read();
        }
    }

    fn freeMany(self: *Self, comptime T: type, pointer: []const T) void {
        var timer = if (!is_wasm) std.time.Timer.start() catch unreachable else {};

        if (BuildOptions.gc_debug) {
            io.print("Going to free slice {*} `{s}`\n", .{ pointer, pointer });
        }

        const n: usize = (@sizeOf(T) * pointer.len);
        self.bytes_allocated -= n;
        self.allocator.free(pointer);

        if (BuildOptions.gc_debug) {
            io.print(
                "(from {}), collected {}, {} allocated\n",
                .{
                    std.fmt.fmtIntSizeDec(self.bytes_allocated + n),
                    std.fmt.fmtIntSizeDec(n),
                    std.fmt.fmtIntSizeDec(self.bytes_allocated),
                },
            );
        }

        if (!is_wasm) {
            self.gc_time += timer.read();
        }
    }

    pub fn markObjDirty(self: *Self, obj: *Obj) !void {
        if (!obj.dirty and self.obj_collected != obj) {
            obj.dirty = true;

            // io.print(
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
            try self.gray_stack.append(self.allocator, obj);
        }
    }

    pub fn markObj(self: *Self, obj: *Obj) !void {
        if (obj.marked or self.obj_collected == obj) {
            if (BuildOptions.gc_debug) {
                io.print(
                    "{*} {s} already marked or old\n",
                    .{
                        obj,
                        try Value.fromObj(obj).toStringAlloc(self.allocator),
                    },
                );
            }
            return;
        }

        if (BuildOptions.gc_debug) {
            io.print("marking {*}: ", .{obj});
            io.print(
                "{s}\n",
                .{
                    try Value.fromObj(obj).toStringAlloc(self.allocator),
                },
            );
        }

        obj.marked = true;

        // Move marked obj to tail so we sweeping can stop going through objects when finding the first marked object
        self.objects.remove(obj.node.?);
        // Just to be safe, reset node before inserting it again
        obj.node.?.* = .{
            .prev = null,
            .next = null,
            .data = obj,
        };
        self.objects.append(obj.node.?);

        try self.gray_stack.append(self.allocator, obj);
    }

    fn blackenObject(self: *Self, obj: *Obj) !void {
        if (BuildOptions.gc_debug) {
            io.print(
                "blackening @{} {}\n",
                .{
                    @intFromPtr(obj),
                    obj.obj_type,
                },
            );
        }

        obj.dirty = false;

        _ = try switch (obj.obj_type) {
            .String => obj.access(ObjString, .String, self).?.mark(self),
            .Type => obj.access(ObjTypeDef, .Type, self).?.mark(self),
            .UpValue => obj.access(ObjUpValue, .UpValue, self).?.mark(self),
            .Closure => obj.access(ObjClosure, .Closure, self).?.mark(self),
            .Function => obj.access(ObjFunction, .Function, self).?.mark(self),
            .ObjectInstance => obj.access(ObjObjectInstance, .ObjectInstance, self).?.mark(self),
            .Object => obj.access(ObjObject, .Object, self).?.mark(self),
            .List => obj.access(ObjList, .List, self).?.mark(self),
            .Map => obj.access(ObjMap, .Map, self).?.mark(self),
            .Enum => obj.access(ObjEnum, .Enum, self).?.mark(self),
            .EnumInstance => obj.access(ObjEnumInstance, .EnumInstance, self).?.mark(self),
            .Bound => obj.access(ObjBoundMethod, .Bound, self).?.mark(self),
            .Native => obj.access(ObjNative, .Native, self).?.mark(self),
            .UserData => obj.access(ObjUserData, .UserData, self).?.mark(self),
            .Pattern => obj.access(ObjPattern, .Pattern, self).?.mark(self),
            .Fiber => obj.access(ObjFiber, .Fiber, self).?.mark(self),
            .ForeignContainer => obj.access(ObjForeignContainer, .ForeignContainer, self).?.mark(self),
            .Range => obj.access(ObjRange, .Range, self).?.mark(self),
        };

        if (BuildOptions.gc_debug) {
            io.print(
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
            io.print(">> freeing {} {}\n", .{ @intFromPtr(obj), obj.obj_type });
        }

        if (BuildOptions.gc_debug_access) {
            self.debugger.?.collected(obj, self.where.?);
        }

        self.obj_collected = obj;
        defer self.obj_collected = null;

        self.allocator.destroy(obj.node.?);

        switch (obj.obj_type) {
            .String => {
                const obj_string = ObjString.cast(obj).?;

                // Remove it from interned strings
                _ = self.strings.remove(obj_string.string);

                freeMany(self, u8, obj_string.string);
                free(self, ObjString, obj_string);
            },
            .Pattern => {
                var obj_pattern = ObjPattern.cast(obj).?;
                if (!is_wasm) {
                    obj_pattern.pattern.free();
                }

                free(self, ObjPattern, obj_pattern);
            },
            .Type => {
                var obj_typedef = ObjTypeDef.cast(obj).?;

                const str = try obj_typedef.toStringAlloc(self.allocator);
                defer self.allocator.free(str);

                if (self.type_registry.registry.get(str)) |registered_obj| {
                    if (registered_obj == obj_typedef) {
                        _ = self.type_registry.registry.remove(str);
                        if (BuildOptions.gc_debug) {
                            io.print("Removed registered type @{} `{s}`\n", .{ @intFromPtr(registered_obj), str });
                        }
                    } else {
                        // io.print(
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
                const obj_upvalue = ObjUpValue.cast(obj).?;
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
                    if (object.type_def.resolved_type.?.Object.fields.get("collect")) |field| {
                        if (field.method and !field.static) {
                            if (self.debugger != null) {
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
                            if (self.debugger != null) {
                                self.debugger.?.invoking_collector = false;
                            }

                            // Remove void result of the collect call
                            _ = obj_objectinstance.vm.pop();
                        }
                    }
                }

                obj_objectinstance.deinit(self.allocator);

                free(self, ObjObjectInstance, obj_objectinstance);
            },
            .Object => {
                var obj_object = ObjObject.cast(obj).?;
                obj_object.deinit(self.allocator);

                free(self, ObjObject, obj_object);
            },
            .List => {
                var obj_list = ObjList.cast(obj).?;
                obj_list.deinit(self.allocator);

                free(self, ObjList, obj_list);
            },
            .Map => {
                var obj_map = ObjMap.cast(obj).?;
                obj_map.deinit(self.allocator);

                free(self, ObjMap, obj_map);
            },
            .Enum => {
                const obj_enum = ObjEnum.cast(obj).?;
                self.allocator.free(obj_enum.cases);

                free(self, ObjEnum, obj_enum);
            },
            .EnumInstance => free(self, ObjEnumInstance, ObjEnumInstance.cast(obj).?),
            .Bound => free(self, ObjBoundMethod, ObjBoundMethod.cast(obj).?),
            .Native => free(self, ObjNative, ObjNative.cast(obj).?),
            .UserData => free(self, ObjUserData, ObjUserData.cast(obj).?),
            .Fiber => {
                var obj_fiber = ObjFiber.cast(obj).?;
                obj_fiber.fiber.deinit();

                self.allocator.destroy(obj_fiber.fiber);

                free(self, ObjFiber, obj_fiber);
            },
            .ForeignContainer => {
                const obj_foreignstruct = ObjForeignContainer.cast(obj).?;

                self.freeMany(u8, obj_foreignstruct.data);

                free(self, ObjForeignContainer, obj_foreignstruct);
            },
            .Range => {
                free(self, ObjRange, ObjRange.cast(obj).?);
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
            try self.markObj(@constCast(ufiber.type_def.toObj()));
            // Mark main fiber
            if (BuildOptions.gc_debug) {
                io.print("MARKING STACK OF FIBER @{}\n", .{@intFromPtr(ufiber)});
            }
            var i: [*]Value = @ptrCast(fiber.stack);
            while (@intFromPtr(i) < @intFromPtr(fiber.stack_top)) : (i += 1) {
                try self.markValue(i[0]);
            }
            if (BuildOptions.gc_debug) {
                io.print("DONE MARKING STACK OF FIBER @{}\n", .{@intFromPtr(ufiber)});
            }

            // Mark closure
            if (BuildOptions.gc_debug) {
                io.print("MARKING FRAMES OF FIBER @{}\n", .{@intFromPtr(ufiber)});
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
                io.print("DONE MARKING FRAMES OF FIBER @{}\n", .{@intFromPtr(ufiber)});
            }

            // Mark opened upvalues
            if (BuildOptions.gc_debug) {
                io.print("MARKING UPVALUES OF FIBER @{}\n", .{@intFromPtr(ufiber)});
            }
            if (fiber.open_upvalues) |open_upvalues| {
                var upvalue: ?*ObjUpValue = open_upvalues;
                while (upvalue) |unwrapped| : (upvalue = unwrapped.next) {
                    try self.markObj(unwrapped.toObj());
                }
            }
            if (BuildOptions.gc_debug) {
                io.print("DONE MARKING UPVALUES OF FIBER @{}\n", .{@intFromPtr(ufiber)});
            }

            current_fiber = ufiber.parent_fiber;
        }
    }

    fn markMethods(self: *Self) !void {
        if (BuildOptions.gc_debug) {
            io.print("MARKING BASIC TYPES METHOD\n", .{});
        }
        // Mark basic types methods
        for (self.objfiber_members) |member| {
            if (member) |umember| {
                try self.markObj(umember.toObj());
            }
        }

        for (self.objfiber_memberDefs) |def| {
            if (def) |udef| {
                try self.markObj(udef.toObj());
            }
        }

        for (self.objrange_members) |member| {
            if (member) |umember| {
                try self.markObj(umember.toObj());
            }
        }

        for (self.objrange_memberDefs) |def| {
            if (def) |udef| {
                try self.markObj(udef.toObj());
            }
        }

        for (self.objstring_members) |member| {
            if (member) |umember| {
                try self.markObj(umember.toObj());
            }
        }

        for (self.objstring_memberDefs) |def| {
            if (def) |udef| {
                try self.markObj(udef.toObj());
            }
        }

        for (self.objpattern_members) |member| {
            if (member) |umember| {
                try self.markObj(umember.toObj());
            }
        }

        for (self.objpattern_memberDefs) |def| {
            if (def) |udef| {
                try self.markObj(udef.toObj());
            }
        }

        if (BuildOptions.gc_debug) {
            io.print("DONE MARKING BASIC TYPES METHOD\n", .{});
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
            io.print("MARKING GLOBALS OF VM @{}\n", .{@intFromPtr(vm)});
        }
        for (vm.globals.items) |global| {
            try self.markValue(global);
        }
        if (BuildOptions.gc_debug) {
            io.print("DONE MARKING GLOBALS OF VM @{}\n", .{@intFromPtr(vm)});
        }

        // Mark ast constant values (some are only referenced by the JIT so might be collected before)
        // TODO: does this takes too long or are we saved by vertue of MultiArrayList?
        for (vm.current_ast.nodes.items(.value)) |valueOpt| {
            if (valueOpt) |value| {
                try self.markValue(value);
            }
        }
    }

    fn traceReference(self: *Self) !void {
        if (BuildOptions.gc_debug) {
            io.print("TRACING REFERENCE\n", .{});
        }
        while (self.gray_stack.items.len > 0) {
            try blackenObject(self, self.gray_stack.pop());
        }
        if (BuildOptions.gc_debug) {
            io.print("DONE TRACING REFERENCE\n", .{});
        }
    }

    fn sweep(self: *Self, mode: Mode) !void {
        const swept: usize = self.bytes_allocated;

        var obj_count: usize = 0;
        var obj_node: ?*std.DoublyLinkedList(*Obj).Node = self.objects.first;
        var count: usize = 0;
        while (obj_node) |node| : (count += 1) {
            if (node.data.marked) {
                if (BuildOptions.gc_debug and mode == .Full) {
                    io.print("UNMARKING @{}\n", .{@intFromPtr(node.data)});
                }
                // If not a full gc, we reset marked, this object is now 'old'
                node.data.marked = if (mode == .Full) false else node.data.marked;

                // If a young collection we don't reset marked flags and since we move all marked object
                // to the tail of the list, we can stop here, there's no more objects to collect
                if (mode == .Young) {
                    break;
                }

                obj_node = node.next;
            } else {
                const unreached: *Obj = node.data;
                obj_node = node.next;

                self.objects.remove(node);

                try freeObj(self, unreached);
                obj_count += 1;
            }
        }

        if (BuildOptions.gc_debug or BuildOptions.gc_debug_light) {
            if (swept < self.bytes_allocated) {
                io.print("Warn: sweep gained memory, possibly due to an Object collector that takes up memory\n", .{});
            }

            io.print(
                "\nSwept {} objects for {}, now {}\n",
                .{
                    obj_count,
                    std.fmt.fmtIntSizeDec(@max(swept, self.bytes_allocated) - self.bytes_allocated),
                    std.fmt.fmtIntSizeDec(self.bytes_allocated),
                },
            );
        }
    }

    pub fn collectGarbage(self: *Self) !void {
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
            io.print(
                "-- gc starts mode {s}, {}, {} objects\n",
                .{
                    @tagName(mode),
                    std.fmt.fmtIntSizeDec(self.bytes_allocated),
                    self.objects.len,
                },
            );

            // var it = self.active_vms.iterator();
            // dumpStack(it.next().?.key_ptr.*);
        }

        var it = self.active_vms.iterator();
        while (it.next()) |kv| {
            var vm = kv.key_ptr.*;

            if (BuildOptions.gc_debug) {
                io.print(
                    "\tMarking VM @{}, on fiber @{} and closure @{} (function @{} {s})\n",
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
            io.print(
                "-- gc end, {}, {} objects, next_gc {}, next_full_gc {}\n",
                .{
                    std.fmt.fmtIntSizeDec(self.bytes_allocated),
                    self.objects.len,
                    std.fmt.fmtIntSizeDec(self.next_gc),
                    std.fmt.fmtIntSizeDec(self.next_full_gc),
                },
            );
        }
        // io.print("gc took {}ms\n", .{timer.read() / 1000000});

        if (!is_wasm) {
            self.gc_time += timer.read();
        }
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
    reporter: Reporter,
    tracker: std.AutoHashMapUnmanaged(*Obj, Ptr),
    invoking_collector: bool = false,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .tracker = std.AutoHashMapUnmanaged(*Obj, Ptr){},
            .reporter = Reporter{
                .allocator = allocator,
            },
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
            if (tracked.collected_at) |collected_at| {
                self.reporter.reportWithOrigin(
                    .gc,
                    at,
                    collected_at,
                    "Trying to collected already collected {} {*}",
                    .{ tracked.what, ptr },
                    "first collected here",
                );

                unreachable;
            }

            tracked.collected_at = at;
        } else {
            unreachable;
        }
    }

    pub fn accessed(self: *Self, ptr: *Obj, at: ?Token) void {
        if (self.invoking_collector) return;

        if (self.tracker.getPtr(ptr)) |tracked| {
            if (tracked.collected_at) |collected_at| {
                var items = std.ArrayList(Reporter.ReportItem).init(self.allocator);
                defer items.deinit();

                var message = std.ArrayList(u8).init(self.allocator);
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
                        .kind = .@"error",
                        .message = message.items,
                    },
                ) catch unreachable;

                if (tracked.allocated_at) |allocated_at| {
                    items.append(
                        .{
                            .location = allocated_at,
                            .kind = .hint,
                            .message = "allocated here",
                        },
                    ) catch unreachable;
                }

                items.append(
                    .{
                        .location = collected_at,
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

                unreachable;
            }
        } else {
            if (at) |accessed_at| {
                self.reporter.reportErrorFmt(
                    .gc,
                    accessed_at,
                    "Untracked obj {*}",
                    .{
                        ptr,
                    },
                );
            } else {
                io.print(
                    "Untracked obj {*}\n",
                    .{
                        ptr,
                    },
                );
            }

            unreachable;
        }
    }
};
