const std = @import("std");
const builtin = @import("builtin");
const _vm = @import("./vm.zig");
const Fiber = _vm.Fiber;
const _value = @import("./value.zig");
const _obj = @import("./obj.zig");
const dumpStack = @import("./disassembler.zig").dumpStack;
const Config = @import("./config.zig").Config;
const VM = @import("./vm.zig").VM;

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

pub const GarbageCollector = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    strings: std.StringHashMap(*ObjString),
    bytes_allocated: usize = 0,
    next_gc: usize = if (Config.debug_gc) 1024 else 1024 * 1024,
    objects: ?*Obj = null,
    gray_stack: std.ArrayList(*Obj),
    active_vms: std.AutoHashMap(*VM, void),

    objfiber_members: std.AutoHashMap(*ObjString, *ObjNative),
    objfiber_memberDefs: std.StringHashMap(*ObjTypeDef),
    objpattern_members: std.AutoHashMap(*ObjString, *ObjNative),
    objpattern_memberDefs: std.StringHashMap(*ObjTypeDef),
    objstring_members: std.AutoHashMap(*ObjString, *ObjNative),
    objstring_memberDefs: std.StringHashMap(*ObjTypeDef),

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .allocator = allocator,
            .strings = std.StringHashMap(*ObjString).init(allocator),
            .gray_stack = std.ArrayList(*Obj).init(allocator),
            .active_vms = std.AutoHashMap(*VM, void).init(allocator),
            .objfiber_members = std.AutoHashMap(*ObjString, *ObjNative).init(allocator),
            .objfiber_memberDefs = std.StringHashMap(*ObjTypeDef).init(allocator),
            .objpattern_members = std.AutoHashMap(*ObjString, *ObjNative).init(allocator),
            .objpattern_memberDefs = std.StringHashMap(*ObjTypeDef).init(allocator),
            .objstring_members = std.AutoHashMap(*ObjString, *ObjNative).init(allocator),
            .objstring_memberDefs = std.StringHashMap(*ObjTypeDef).init(allocator),
        };
    }

    pub fn registerVM(self: *Self, vm: *VM) !void {
        try self.active_vms.put(vm, .{});
    }

    pub fn unregisterVM(self: *Self, vm: *VM) void {
        _ = self.active_vms.remove(vm);
    }

    pub fn deinit(self: *Self) void {
        self.gray_stack.deinit();
        self.strings.deinit();
        self.active_vms.deinit();

        var obj = self.objects;
        while (obj) |uobj| {
            var next = uobj.next;
            self.freeObj(uobj);
            obj = next;
        }

        self.objfiber_members.deinit();
        self.objfiber_memberDefs.deinit();
        self.objpattern_members.deinit();
        self.objpattern_memberDefs.deinit();
        self.objstring_members.deinit();
        self.objstring_memberDefs.deinit();
    }

    pub fn allocate(self: *Self, comptime T: type) !*T {
        self.bytes_allocated += @sizeOf(T);

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

        if (self.bytes_allocated > self.next_gc and !Config.debug_turn_off_gc) {
            try self.collectGarbage();
        }

        return try self.allocator.alloc(T, count);
    }

    pub fn free(self: *Self, comptime T: type, pointer: *T) void {
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

    pub fn freeMany(self: *Self, comptime T: type, pointer: []const T) void {
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

    pub fn markObj(self: *Self, obj: *Obj) !void {
        if (obj.is_marked) {
            if (Config.debug_gc) {
                std.debug.print("{*} {s} already marked\n", .{ obj, try valueToStringAlloc(self.allocator, Value{ .Obj = obj }) });
            }
            return;
        }

        if (Config.debug_gc) {
            std.debug.print("marking {*}: ", .{obj});
            std.debug.print("{s}\n", .{try valueToStringAlloc(self.allocator, Value{ .Obj = obj })});
        }

        obj.is_marked = true;

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

    fn freeObj(self: *Self, obj: *Obj) void {
        if (Config.debug_gc) {
            std.debug.print(">> freeing {} {}\n", .{ @ptrToInt(obj), obj.obj_type });
        }

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
                obj_typedef.deinit();
                free(self, ObjTypeDef, obj_typedef);
            },
            .UpValue => {
                var obj_upvalue = ObjUpValue.cast(obj).?;
                if (obj_upvalue.closed) |value| {
                    if (value == .Obj) {
                        freeObj(self, value.Obj);
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

    fn countObj(self: *Self) !void {
        if (Config.debug_gc) {
            var count: usize = 0;
            var marked_count: usize = 0;
            var obj: ?*Obj = self.objects;
            while (obj) |uobj| : (count += 1) {
                if (uobj.is_marked) {
                    marked_count += 1;
                }
                obj = uobj.next;
            }

            std.debug.print(">>>> {} objects in GC with {} marked\n", .{ count, marked_count });
        }
    }

    fn sweep(self: *Self) !void {
        try self.countObj();

        var swept: usize = self.bytes_allocated;

        var obj_count: usize = 0;
        var previous: ?*Obj = null;
        var obj: ?*Obj = self.objects;
        while (obj) |uobj| {
            if (uobj.is_marked) {
                if (Config.debug_gc) {
                    std.debug.print("UNMARKING @{}\n", .{@ptrToInt(uobj)});
                }
                uobj.is_marked = false;

                previous = uobj;
                obj = uobj.next;
            } else {
                var unreached: *Obj = uobj;
                obj = uobj.next;

                if (previous) |uprevious| {
                    uprevious.next = obj;
                } else {
                    self.objects = obj;
                }

                freeObj(self, unreached);
                obj_count += 1;
            }
        }

        if (Config.debug_gc) {
            std.debug.print("Swept {} objects for {} bytes, now {} bytes\n", .{ obj_count, swept - self.bytes_allocated, self.bytes_allocated });
        }
    }

    pub fn collectGarbage(self: *Self) !void {
        // Don't collect until a VM is actually running
        if (self.active_vms.count() == 0) {
            return;
        }

        if (Config.debug_gc) {
            std.debug.print("-- gc starts\n", .{});

            // try dumpStack(self);
        }

        try self.markMethods();

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
            try markRoots(self, kv.key_ptr.*);
        }

        try traceReference(self);

        try sweep(self);

        self.next_gc = self.bytes_allocated * 2;

        if (Config.debug_gc) {
            std.debug.print("-- gc end\n", .{});
        }
    }
};
