const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;
const StringHashMap = std.StringHashMap;
const Chunk = @import("Chunk.zig");
const _vm = @import("vm.zig");
const VM = _vm.VM;
const Fiber = _vm.Fiber;
const Parser = @import("Parser.zig");
const _memory = @import("memory.zig");
const GarbageCollector = _memory.GarbageCollector;
const TypeRegistry = _memory.TypeRegistry;
const Integer = @import("value.zig").Integer;
const Value = @import("value.zig").Value;
const Token = @import("Token.zig");
const is_wasm = builtin.cpu.arch.isWasm();
const BuildOptions = @import("build_options");
const CodeGen = @import("Codegen.zig");
const buzz_api = @import("buzz_api.zig");
const buzz_builtin = @import("builtin.zig");
const ZigType = @import("zigtypes.zig").Type;
const Ast = @import("Ast.zig");
const io = @import("io.zig");

pub const pcre = if (!is_wasm) @import("pcre.zig") else void;

pub const SerializeError = error{
    CircularReference,
    NotSerializable,
    OutOfMemory,
};

pub const ObjType = enum {
    String,
    Type,
    UpValue,
    Closure,
    Function,
    ObjectInstance,
    Object,
    List,
    Map,
    Enum,
    EnumInstance,
    Bound,
    Native,
    UserData,
    Pattern,
    Fiber,
    ForeignContainer,
    Range,
};

pub const Obj = struct {
    const Self = @This();

    obj_type: ObjType,
    marked: bool = false,
    // True when old obj and was modified
    dirty: bool = false,
    node: ?*std.DoublyLinkedList(*Obj).Node = null,

    pub fn cast(obj: *Obj, comptime T: type, obj_type: ObjType) ?*T {
        if (obj.obj_type != obj_type) {
            return null;
        }

        return @alignCast(@fieldParentPtr("obj", obj));
    }

    pub fn access(obj: *Obj, comptime T: type, obj_type: ObjType, gc: *GarbageCollector) ?*T {
        if (BuildOptions.gc_debug_access) {
            gc.debugger.?.accessed(obj, gc.where);
        }

        return obj.cast(T, obj_type);
    }

    pub fn serialize(self: *Self, vm: *VM, seen: *std.AutoHashMap(*Self, void)) SerializeError!Value {
        if (seen.get(self) != null) {
            return error.CircularReference;
        }

        try seen.put(self, {});

        switch (self.obj_type) {
            .String => return Value.fromObj(self),

            .Pattern => {
                const pattern = self.access(ObjPattern, .Pattern, vm.gc).?;

                return (vm.gc.copyString(pattern.source) catch return error.OutOfMemory).toValue();
            },

            .Fiber,
            .Object,
            .Enum,
            .Function,
            .Closure,
            .Bound,
            .UserData,
            .Native,
            => return error.NotSerializable,

            .UpValue => {
                const upvalue = self.access(ObjUpValue, .UpValue, vm.gc).?;

                return try (upvalue.closed orelse upvalue.location.*).serialize(vm, seen);
            },

            // We could also serialize the actual structure of a typedef
            // But do we need to? At deserialization we can just parse the type?
            .Type => {
                const type_def = self.access(ObjTypeDef, .Type, vm.gc).?;

                const type_str = type_def.toStringAlloc(vm.gc.allocator) catch return error.OutOfMemory;
                defer vm.gc.allocator.free(type_str);

                return (vm.gc.copyString(type_str) catch return error.OutOfMemory).toValue();
            },

            .EnumInstance => {
                const enum_instance = self.access(ObjEnumInstance, .EnumInstance, vm.gc).?;

                return try enum_instance.enum_ref.cases[enum_instance.case].serialize(vm, seen);
            },

            .Range => {
                const range = self.access(ObjRange, .Range, vm.gc).?;

                const map_type = vm.gc.type_registry.getTypeDef(
                    .{
                        .optional = false,
                        .def_type = .Map,
                        .resolved_type = .{
                            .Map = ObjMap.MapDef.init(
                                vm.gc.type_registry.str_type,
                                vm.gc.type_registry.int_type,
                                true,
                            ),
                        },
                    },
                ) catch return error.OutOfMemory;

                const serialized_range = vm.gc.allocateObject(
                    ObjMap,
                    ObjMap.init(
                        vm.gc.allocator,
                        map_type,
                    ) catch return error.OutOfMemory,
                ) catch return error.OutOfMemory;

                serialized_range.map.put(
                    vm.gc.allocator,
                    (vm.gc.copyString("low") catch return error.OutOfMemory).toValue(),
                    Value.fromInteger(range.low),
                ) catch return error.OutOfMemory;

                serialized_range.map.put(
                    vm.gc.allocator,
                    (vm.gc.copyString("high") catch return error.OutOfMemory).toValue(),
                    Value.fromInteger(range.high),
                ) catch return error.OutOfMemory;

                return serialized_range.toValue();
            },

            .List => {
                const list = self.access(ObjList, .List, vm.gc).?;

                const list_type = vm.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .List,
                        .resolved_type = .{
                            .List = ObjList.ListDef.init(
                                vm.gc.type_registry.any_type,
                                list.type_def.resolved_type.?.List.mutable,
                            ),
                        },
                    },
                ) catch return error.OutOfMemory;

                const serialized_list = vm.gc.allocateObject(
                    ObjList,
                    ObjList.init(
                        vm.gc.allocator,
                        list_type,
                    ) catch return error.OutOfMemory,
                ) catch return error.OutOfMemory;

                for (list.items.items) |item| {
                    try serialized_list.rawAppend(
                        vm.gc,
                        try item.serialize(vm, seen),
                    );
                }

                return serialized_list.toValue();
            },

            .Map => {
                const map = self.access(ObjMap, .Map, vm.gc).?;

                const map_def = ObjMap.MapDef.init(
                    vm.gc.type_registry.any_type,
                    vm.gc.type_registry.any_type,
                    map.type_def.resolved_type.?.Map.mutable,
                );

                const resolved_type = ObjTypeDef.TypeUnion{ .Map = map_def };

                const map_type = vm.gc.type_registry.getTypeDef(
                    .{
                        .optional = false,
                        .def_type = .Map,
                        .resolved_type = resolved_type,
                    },
                ) catch return error.OutOfMemory;

                const serialized_map = vm.gc.allocateObject(
                    ObjMap,
                    ObjMap.init(
                        vm.gc.allocator,
                        map_type,
                    ) catch return error.OutOfMemory,
                ) catch return error.OutOfMemory;

                for (map.map.keys()) |key| {
                    try serialized_map.set(
                        vm.gc,
                        try key.serialize(vm, seen),
                        try map.map.get(key).?.serialize(vm, seen),
                    );
                }

                return serialized_map.toValue();
            },

            .ObjectInstance => {
                const instance = self.access(ObjObjectInstance, .ObjectInstance, vm.gc).?;
                const object_def = instance.type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object;

                const map_def = ObjMap.MapDef.init(
                    vm.gc.type_registry.any_type,
                    vm.gc.type_registry.any_type,
                    instance.type_def.resolved_type.?.ObjectInstance.mutable,
                );

                const resolved_type = ObjTypeDef.TypeUnion{ .Map = map_def };

                const map_type = vm.gc.type_registry.getTypeDef(
                    .{
                        .optional = false,
                        .def_type = .Map,
                        .resolved_type = resolved_type,
                    },
                ) catch return error.OutOfMemory;

                const serialized_instance = vm.gc.allocateObject(
                    ObjMap,
                    ObjMap.init(
                        vm.gc.allocator,
                        map_type,
                    ) catch return error.OutOfMemory,
                ) catch return error.OutOfMemory;

                var it = object_def.fields.iterator();
                while (it.next()) |kv| {
                    const field = kv.value_ptr.*;
                    if (!field.static and !field.method) {
                        const property_str = (vm.gc.copyString(field.name) catch return error.OutOfMemory);
                        serialized_instance.set(
                            vm.gc,
                            property_str.toValue(),
                            try instance.fields[field.index].serialize(vm, seen),
                        ) catch return error.OutOfMemory;
                    }
                }

                return serialized_instance.toValue();
            },

            .ForeignContainer => {
                const container = self.access(ObjForeignContainer, .ForeignContainer, vm.gc).?;
                const container_def = container.type_def.resolved_type.?.ForeignContainer;

                const map_def = ObjMap.MapDef.init(
                    vm.gc.type_registry.any_type,
                    vm.gc.type_registry.any_type,
                    true,
                );

                const resolved_type = ObjTypeDef.TypeUnion{ .Map = map_def };

                const map_type = vm.gc.type_registry.getTypeDef(
                    .{
                        .optional = false,
                        .def_type = .Map,
                        .resolved_type = resolved_type,
                    },
                ) catch return error.OutOfMemory;

                const serialized_instance = vm.gc.allocateObject(
                    ObjMap,
                    ObjMap.init(
                        vm.gc.allocator,
                        map_type,
                    ) catch return error.OutOfMemory,
                ) catch return error.OutOfMemory;

                var it = container_def.fields.iterator();
                while (it.next()) |kv| {
                    const dupped = try vm.gc.allocator.dupeZ(u8, kv.key_ptr.*);
                    defer vm.gc.allocator.free(dupped);

                    try serialized_instance.set(
                        vm.gc,
                        (vm.gc.copyString(kv.key_ptr.*) catch return error.OutOfMemory).toValue(),
                        kv.value_ptr.*.getter(
                            vm,
                            @ptrCast(dupped),
                        ),
                    );
                }

                return serialized_instance.toValue();
            },
        }
    }

    pub fn typeOf(self: *Self, gc: *GarbageCollector) error{ OutOfMemory, NoSpaceLeft, ReachedMaximumMemoryUsage }!*ObjTypeDef {
        return switch (self.obj_type) {
            .Range => gc.type_registry.rg_type,
            .String => gc.type_registry.str_type,
            .Pattern => gc.type_registry.pat_type,
            .Fiber => ObjFiber.cast(self).?.fiber.type_def,
            .Type => try gc.type_registry.getTypeDef(.{ .def_type = .Type }),
            .Object => ObjObject.cast(self).?.type_def,
            .Enum => ObjEnum.cast(self).?.type_def,
            .ObjectInstance => ObjObjectInstance.cast(self).?.type_def,
            .EnumInstance => try ObjEnumInstance.cast(self).?.enum_ref.type_def.toInstance(
                &gc.type_registry,
                false,
            ),
            .Function => ObjFunction.cast(self).?.type_def,
            .UpValue => upvalue: {
                const upvalue: *ObjUpValue = ObjUpValue.cast(self).?;

                break :upvalue (upvalue.closed orelse upvalue.location.*).typeOf(gc);
            },
            .Closure => ObjClosure.cast(self).?.function.type_def,
            .List => ObjList.cast(self).?.type_def,
            .Map => ObjMap.cast(self).?.type_def,
            .Bound => bound: {
                const bound: *ObjBoundMethod = ObjBoundMethod.cast(self).?;
                break :bound try (if (bound.closure) |cls| cls.function.toValue() else bound.native.?.toValue()).typeOf(gc);
            },
            .ForeignContainer => ObjForeignContainer.cast(self).?.type_def,
            .UserData => gc.type_registry.ud_type,
            // FIXME: apart from list/map types we actually can embark typedef of objnatives at runtime
            // Or since native are ptr to unique function we can keep a map of ptr => typedef
            .Native => unreachable,
        };
    }

    pub fn is(self: *Self, type_def: *ObjTypeDef) bool {
        if (type_def.def_type == .Any) {
            return true;
        }

        return switch (self.obj_type) {
            .Range => type_def.def_type == .Range,
            .String => type_def.def_type == .String,
            .Pattern => type_def.def_type == .Pattern,
            .Fiber => ObjFiber.cast(self).?.is(type_def),

            .Type, .Object, .Enum => type_def.def_type == .Type,

            .ObjectInstance => (type_def.def_type == .ObjectInstance or type_def.def_type == .Object or type_def.def_type == .Protocol or type_def.def_type == .ProtocolInstance) and
                ObjObjectInstance.cast(self).?.is(type_def),
            .EnumInstance => (type_def.def_type == .Enum and ObjEnumInstance.cast(self).?.enum_ref.type_def == type_def) or
                (type_def.def_type == .EnumInstance and ObjEnumInstance.cast(self).?.enum_ref.type_def == type_def.resolved_type.?.EnumInstance.of),
            .Function => function: {
                const function: *ObjFunction = ObjFunction.cast(self).?;
                break :function function.type_def.eql(type_def);
            },

            .UpValue => upvalue: {
                const upvalue: *ObjUpValue = ObjUpValue.cast(self).?;
                break :upvalue Value.fromObj(type_def.toObj()).is(
                    upvalue.closed orelse upvalue.location.*,
                );
            },
            .Closure => ObjClosure.cast(self).?.function.toObj().is(type_def),
            .List => ObjList.cast(self).?.type_def.eql(type_def),
            .Map => ObjMap.cast(self).?.type_def.eql(type_def),
            .Bound => bound: {
                const bound: *ObjBoundMethod = ObjBoundMethod.cast(self).?;
                break :bound Value.fromObj(type_def.toObj()).is(
                    Value.fromObj(if (bound.closure) |cls| cls.function.toObj() else bound.native.?.toObj()),
                );
            },
            .ForeignContainer => type_def.def_type == .ForeignContainer and ObjForeignContainer.cast(self).?.is(type_def),
            .UserData => type_def.def_type == .UserData,
            .Native => unreachable, // TODO: we don't know how to embark NativeFn type at runtime yet
        };
    }

    pub fn typeEql(self: *Self, type_def: *ObjTypeDef) bool {
        return switch (self.obj_type) {
            .Pattern => type_def.def_type == .Pattern,
            .String => type_def.def_type == .String,
            .Type => type_def.def_type == .Type,
            .UpValue => uv: {
                var upvalue: *ObjUpValue = ObjUpValue.cast(self).?;
                break :uv (upvalue.closed orelse upvalue.location.*).typeEql(type_def);
            },
            .EnumInstance => ei: {
                var instance: *ObjEnumInstance = ObjEnumInstance.cast(self).?;
                break :ei type_def.def_type == .EnumInstance and instance.enum_ref.type_def.eql(type_def.resolved_type.?.EnumInstance);
            },
            .ObjectInstance => oi: {
                var instance: *ObjObjectInstance = ObjObjectInstance.cast(self).?;
                break :oi type_def.def_type == .ObjectInstance and instance.is(type_def.resolved_type.?.ObjectInstance);
            },
            .Enum => ObjEnum.cast(self).?.type_def.eql(type_def),
            .Object => ObjObject.cast(self).?.type_def.eql(type_def),
            .Function => ObjFunction.cast(self).?.type_def.eql(type_def),
            .Closure => ObjClosure.cast(self).?.function.type_def.eql(type_def),
            .Bound => bound: {
                const bound = ObjBoundMethod.cast(self).?;
                break :bound if (bound.closure) |cls| cls.function.type_def.eql(type_def) else unreachable; // TODO
            },
            .List => ObjList.cast(self).?.type_def.eql(type_def),
            .Map => ObjMap.cast(self).?.type_def.eql(type_def),
            .Fiber => ObjFiber.cast(self).?.type_def.eql(type_def),
            .UserData, .Native => unreachable, // TODO
        };
    }

    pub fn eql(self: *Self, other: *Self) bool {
        if (self.obj_type != other.obj_type) {
            return false;
        }

        switch (self.obj_type) {
            .Pattern => {
                return mem.eql(u8, ObjPattern.cast(self).?.source, ObjPattern.cast(other).?.source);
            },
            .String => {
                if (BuildOptions.debug) {
                    assert(self != other or mem.eql(u8, ObjString.cast(self).?.string, ObjString.cast(other).?.string));
                    assert(self == other or !mem.eql(u8, ObjString.cast(self).?.string, ObjString.cast(other).?.string));
                }

                // since string are interned this should be enough
                return self == other;
            },
            .Type => {
                const self_type: *ObjTypeDef = ObjTypeDef.cast(self).?;
                const other_type: *ObjTypeDef = ObjTypeDef.cast(other).?;

                return self_type.optional == other_type.optional and self_type.eql(other_type);
            },
            .UpValue => {
                const self_upvalue: *ObjUpValue = ObjUpValue.cast(self).?;
                const other_upvalue: *ObjUpValue = ObjUpValue.cast(other).?;

                return (self_upvalue.closed orelse self_upvalue.location.*).eql(other_upvalue.closed orelse other_upvalue.location.*);
            },
            .EnumInstance => {
                const self_enum_instance: *ObjEnumInstance = ObjEnumInstance.cast(self).?;
                const other_enum_instance: *ObjEnumInstance = ObjEnumInstance.cast(other).?;

                return self_enum_instance.enum_ref == other_enum_instance.enum_ref and self_enum_instance.case == other_enum_instance.case;
            },
            .UserData => {
                const self_userdata: *ObjUserData = ObjUserData.cast(self).?;
                const other_userdata: *ObjUserData = ObjUserData.cast(other).?;

                return self_userdata.userdata == other_userdata.userdata;
            },
            .Fiber => {
                const self_fiber: *ObjFiber = ObjFiber.cast(self).?;
                const other_fiber: *ObjFiber = ObjFiber.cast(other).?;

                return self_fiber.fiber == other_fiber.fiber;
            },
            .Range => {
                const self_range = ObjRange.cast(self).?;
                const other_range = ObjRange.cast(other).?;

                return self_range.low == other_range.low and self_range.high == other_range.high;
            },
            .Bound,
            .Closure,
            .Function,
            .ObjectInstance,
            .Object,
            .List,
            .Map,
            .Enum,
            .Native,
            .ForeignContainer,
            => {
                return self == other;
            },
        }
    }

    pub fn toString(obj: *Obj, writer: *const std.ArrayList(u8).Writer) (Allocator.Error || std.fmt.BufPrintError)!void {
        return switch (obj.obj_type) {
            .String => {
                const str = ObjString.cast(obj).?.string;

                try writer.print("{s}", .{str});
            },
            .Pattern => {
                const pattern = ObjPattern.cast(obj).?.source;

                try writer.print("{s}", .{pattern});
            },
            .Fiber => {
                const fiber = ObjFiber.cast(obj).?.fiber;

                try writer.print("fiber: 0x{x}", .{@intFromPtr(fiber)});
            },
            .Type => {
                const type_def: *ObjTypeDef = ObjTypeDef.cast(obj).?;

                try writer.print("type: 0x{x} `", .{
                    @intFromPtr(type_def),
                });

                try type_def.toString(writer);

                try writer.writeAll("`");
            },
            .UpValue => {
                const upvalue: *ObjUpValue = ObjUpValue.cast(obj).?;

                try (upvalue.closed orelse upvalue.location.*).toString(writer);
            },
            .Closure => try writer.print("closure: 0x{x} `{s}`", .{
                @intFromPtr(ObjClosure.cast(obj).?),
                ObjClosure.cast(obj).?.function.type_def.resolved_type.?.Function.name.string,
            }),
            .Function => try writer.print("function: 0x{x} `{s}`", .{
                @intFromPtr(ObjFunction.cast(obj).?),
                ObjFunction.cast(obj).?.type_def.resolved_type.?.Function.name.string,
            }),
            .ObjectInstance => {
                const instance = ObjObjectInstance.cast(obj).?;

                if (instance.object) |object| {
                    try writer.print(
                        "object instance: 0x{x} {s}`{s}`",
                        .{
                            @intFromPtr(instance),
                            object.type_def.resolved_type.?.Object.name.string,
                            if (instance.type_def.resolved_type.?.ObjectInstance.mutable)
                                "mut "
                            else
                                "",
                        },
                    );
                } else {
                    try writer.print(
                        "object instance: 0x{x} {s}obj{{ ",
                        .{
                            @intFromPtr(instance),
                            if (instance.type_def.resolved_type.?.ObjectInstance.mutable)
                                "mut "
                            else
                                "",
                        },
                    );

                    for (0..instance.fields.len) |i| {
                        const object_def = instance
                            .type_def
                            .resolved_type.?
                            .ObjectInstance
                            .of
                            .resolved_type.?
                            .Object;

                        const field_name = object_def.fields.keys()[i];

                        try object_def
                            .fields.get(field_name).?
                            .type_def
                            .toString(writer);

                        try writer.print(" {s}, ", .{field_name});
                    }
                    try writer.writeAll("}");
                }
            },
            .Object => try writer.print(
                "object: 0x{x} `{s}`",
                .{
                    @intFromPtr(ObjObject.cast(obj).?),
                    ObjObject.cast(obj).?.type_def.resolved_type.?.Object.name.string,
                },
            ),
            .Range => {
                const range = ObjRange.cast(obj).?;

                try writer.print(
                    "range: 0x{x} {}..{}",
                    .{
                        @intFromPtr(range),
                        range.low,
                        range.high,
                    },
                );
            },
            .List => {
                const list: *ObjList = ObjList.cast(obj).?;

                try writer.print(
                    "list: 0x{x} {s}[",
                    .{
                        @intFromPtr(list),
                        if (list.type_def.resolved_type.?.List.mutable)
                            "mut "
                        else
                            "",
                    },
                );

                try list.type_def.resolved_type.?.List.item_type.toString(writer);

                try writer.writeAll("]");
            },
            .Map => {
                const map: *ObjMap = ObjMap.cast(obj).?;

                try writer.print(
                    "map: 0x{x} {s}{{",
                    .{
                        @intFromPtr(map),
                        if (map.type_def.resolved_type.?.Map.mutable)
                            "mut "
                        else
                            "",
                    },
                );

                try map.type_def.resolved_type.?.Map.key_type.toString(writer);

                try writer.writeAll(", ");

                try map.type_def.resolved_type.?.Map.value_type.toString(writer);

                try writer.writeAll("}");
            },
            .Enum => try writer.print("enum: 0x{x} `{s}`", .{
                @intFromPtr(ObjEnum.cast(obj).?),
                ObjEnum.cast(obj).?.type_def.resolved_type.?.Enum.name.string,
            }),
            .EnumInstance => enum_instance: {
                const instance: *ObjEnumInstance = ObjEnumInstance.cast(obj).?;
                const enum_: *ObjEnum = instance.enum_ref;

                break :enum_instance try writer.print("{s}.{s}", .{
                    enum_.type_def.resolved_type.?.Enum.name.string,
                    enum_.type_def.resolved_type.?.Enum.cases[instance.case],
                });
            },
            .Bound => {
                const bound: *ObjBoundMethod = ObjBoundMethod.cast(obj).?;

                if (bound.closure) |closure| {
                    const closure_name: []const u8 = closure.function.type_def.resolved_type.?.Function.name.string;
                    try writer.writeAll("bound method: ");

                    try (bound.receiver).toString(writer);

                    try writer.print(" to {s}", .{closure_name});
                } else {
                    assert(bound.native != null);
                    try writer.writeAll("bound method: ");

                    try (bound.receiver).toString(writer);

                    try writer.print(" to native 0x{}", .{@intFromPtr(bound.native.?)});
                }
            },
            .Native => {
                const native: *ObjNative = ObjNative.cast(obj).?;

                try writer.print("native: 0x{x}", .{@intFromPtr(native)});
            },
            .UserData => {
                const userdata: *ObjUserData = ObjUserData.cast(obj).?;

                try writer.print("userdata: 0x{x}", .{userdata.userdata});
            },
            .ForeignContainer => {
                const foreign = ObjForeignContainer.cast(obj).?;

                try writer.print("foreign struct: 0x{x} `{s}`", .{
                    @intFromPtr(foreign.data.ptr),
                    foreign.type_def.resolved_type.?.ForeignContainer.name.string,
                });
            },
        };
    }
};

pub const ObjFiber = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Fiber },

    fiber: *Fiber,

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markFiber(self.fiber);
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .Fiber);
    }

    pub const members = [_]NativeFn{
        buzz_builtin.fiber.cancel,
        buzz_builtin.fiber.isMain,
        buzz_builtin.fiber.over,
    };

    pub const members_typedef = [_][]const u8{
        "extern fun cancel() > void",
        "extern fun isMain() > bool",
        "extern fun over() > bool",
    };

    pub const members_name = std.StaticStringMap(usize).initComptime(
        .{
            .{ "cancel", 0 },
            .{ "isMain", 1 },
            .{ "over", 2 },
        },
    );

    pub fn memberByName(vm: *VM, name: []const u8) !?Value {
        return if (members_name.get(name)) |idx|
            try member(vm, idx)
        else
            null;
    }

    pub fn member(vm: *VM, method_idx: usize) !Value {
        if (vm.gc.objfiber_members[method_idx]) |umethod| {
            return umethod.toValue();
        }

        var native: *ObjNative = try vm.gc.allocateObject(
            ObjNative,
            .{
                // Complains about const qualifier discard otherwise
                .native = @as(*anyopaque, @ptrFromInt(@intFromPtr(members[method_idx]))),
            },
        );

        vm.gc.objfiber_members[method_idx] = native;

        // We need to mark it otherwise it could be collected by a Young gc and then badly accessed by a Full gc
        vm.gc.markObj(native.toObj()) catch @panic("Could not mark obj");

        return native.toValue();
    }

    pub fn memberDefByName(parser: *Parser, name: []const u8) !?*ObjTypeDef {
        return if (members_name.get(name)) |idx|
            try memberDef(parser, idx)
        else
            null;
    }

    pub fn memberDef(parser: *Parser, method_idx: usize) !*ObjTypeDef {
        if (parser.gc.objfiber_memberDefs[method_idx]) |umethod| {
            return umethod;
        }

        const native_type = try parser.parseTypeDefFrom(members_typedef[method_idx]);

        parser.gc.objfiber_memberDefs[method_idx] = native_type;

        return native_type;
    }

    fn is(self: *Self, type_def: *ObjTypeDef) bool {
        return type_def.eql(self.fiber.type_def);
    }

    pub const FiberDef = struct {
        const SelfFiberDef = @This();

        return_type: *ObjTypeDef,
        yield_type: *ObjTypeDef,

        pub fn mark(self: *SelfFiberDef, gc: *GarbageCollector) !void {
            try gc.markObj(@constCast(self.return_type.toObj()));
            try gc.markObj(@constCast(self.yield_type.toObj()));
        }
    };
};

pub const Pattern = if (!is_wasm)
    *pcre.pcre2_code
else
    void;

// Patterns are pcre regex, @see https://www.pcre.org/original/doc/html/index.html
pub const ObjPattern = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Pattern },

    source: []const u8,
    pattern: Pattern,

    pub fn mark(_: *Self, _: *GarbageCollector) !void {}

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .Pattern);
    }

    pub const members = if (!is_wasm)
        [_]NativeFn{
            buzz_builtin.pattern.match,
            buzz_builtin.pattern.matchAll,
            buzz_builtin.pattern.replace,
            buzz_builtin.pattern.replaceAll,
        }
    else
        [_]NativeFn{
            buzz_builtin.pattern.replace,
            buzz_builtin.pattern.replaceAll,
        };

    const members_typedef = if (!is_wasm)
        [_][]const u8{
            "extern fun match(subject: str) > [str]?",
            "extern fun matchAll(subject: str) > [[str]]?",
            "extern fun replace(subject: str, with: str) > str",
            "extern fun replaceAll(subject: str, with: str) > str",
        }
    else
        [_][]const u8{
            "extern fun replace(subject: str, with: str) > str",
            "extern fun replaceAll(subject: str, with: str) > str",
        };

    pub const members_name = std.StaticStringMap(usize).initComptime(
        if (!is_wasm)
            .{
                .{ "match", 0 },
                .{ "matchAll", 1 },
                .{ "replace", 2 },
                .{ "replaceAll", 3 },
            }
        else
            .{
                .{ "replace", 0 },
                .{ "replaceAll", 1 },
            },
    );

    pub fn member(vm: *VM, method_idx: usize) !Value {
        if (vm.gc.objpattern_members[method_idx]) |umethod| {
            return umethod.toValue();
        }

        var native: *ObjNative = try vm.gc.allocateObject(
            ObjNative,
            .{
                // Complains about const qualifier discard otherwise
                .native = @as(*anyopaque, @ptrFromInt(@intFromPtr(members[method_idx]))),
            },
        );

        vm.gc.objpattern_members[method_idx] = native;

        // We need to mark it otherwise it could be collected by a Young gc and then badly accessed by a Full gc
        vm.gc.markObj(native.toObj()) catch @panic("Could not mark obj");

        return native.toValue();
    }

    pub fn memberByName(vm: *VM, name: []const u8) !?Value {
        return if (members_name.get(name)) |idx|
            try member(vm, idx)
        else
            null;
    }

    pub fn memberDef(parser: *Parser, method_idx: usize) !*ObjTypeDef {
        if (parser.gc.objpattern_memberDefs[method_idx]) |umethod| {
            return umethod;
        }

        const native_type = try parser.parseTypeDefFrom(members_typedef[method_idx]);

        parser.gc.objpattern_memberDefs[method_idx] = native_type;

        return native_type;
    }

    pub fn memberDefByName(parser: *Parser, name: []const u8) !?*ObjTypeDef {
        return if (members_name.get(name)) |idx|
            try memberDef(parser, idx)
        else
            null;
    }
};

/// User data, type around an opaque pointer
pub const ObjUserData = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .UserData },

    userdata: u64,

    pub fn mark(_: *Self, _: *GarbageCollector) void {}

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .UserData);
    }
};

/// A String
pub const ObjString = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .String },

    /// The actual string
    string: []const u8,

    pub fn mark(_: *Self, _: *GarbageCollector) !void {}

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .String);
    }

    pub fn concat(self: *Self, vm: *VM, other: *Self) !*Self {
        var new_string: std.ArrayList(u8) = std.ArrayList(u8).init(vm.gc.allocator);
        try new_string.appendSlice(self.string);
        try new_string.appendSlice(other.string);

        return vm.gc.copyString(new_string.items);
    }

    pub fn next(self: *Self, vm: *VM, str_index: ?i32) !?i32 {
        if (str_index) |index| {
            if (index < 0 or index >= @as(i32, @intCast(self.string.len))) {
                try vm.throw(
                    VM.Error.OutOfBound,
                    (try vm.gc.copyString("Out of bound access to str")).toValue(),
                    null,
                    null,
                );
            }

            return if (index + 1 >= @as(i32, @intCast(self.string.len)))
                null
            else
                index + 1;
        } else {
            return if (self.string.len > 0) @as(i32, 0) else null;
        }
    }

    pub const members = [_]NativeFn{
        buzz_builtin.str.bin,
        buzz_builtin.str.byte,
        buzz_builtin.str.decodeBase64,
        buzz_builtin.str.encodeBase64,
        buzz_builtin.str.endsWith,
        buzz_builtin.str.hex,
        buzz_builtin.str.indexOf,
        buzz_builtin.str.len,
        buzz_builtin.str.lower,
        buzz_builtin.str.repeat,
        buzz_builtin.str.replace,
        buzz_builtin.str.split,
        buzz_builtin.str.startsWith,
        buzz_builtin.str.sub,
        buzz_builtin.str.trim,
        buzz_builtin.str.upper,
        buzz_builtin.str.utf8Codepoints,
        buzz_builtin.str.utf8Len,
        buzz_builtin.str.utf8Valid,
    };

    pub const members_typedef = [_][]const u8{
        "extern fun bin() > str",
        "extern fun byte(at: int = 0) > int",
        "extern fun decodeBase64() > str",
        "extern fun encodeBase64() > str",
        "extern fun endsWith(needle: str) > bool",
        "extern fun hex() > str",
        "extern fun indexOf(needle: str) > int?",
        "extern fun len() > int",
        "extern fun lower() > str",
        "extern fun repeat(n: int) > str",
        "extern fun replace(needle: str, with: str) > str",
        "extern fun split(separator: str) > [str]",
        "extern fun startsWith(needle: str) > bool",
        "extern fun sub(start: int, len: int?) > str",
        "extern fun trim() > str",
        "extern fun upper() > str",
        "extern fun utf8Codepoints() > [str]",
        "extern fun utf8Len() > int",
        "extern fun utf8Valid() > bool",
    };

    pub const members_name = std.StaticStringMap(usize).initComptime(
        .{
            .{ "bin", 0 },
            .{ "byte", 1 },
            .{ "decodeBase64", 2 },
            .{ "encodeBase64", 3 },
            .{ "endsWith", 4 },
            .{ "hex", 5 },
            .{ "indexOf", 6 },
            .{ "len", 7 },
            .{ "lower", 8 },
            .{ "repeat", 9 },
            .{ "replace", 10 },
            .{ "split", 11 },
            .{ "startsWith", 12 },
            .{ "sub", 13 },
            .{ "trim", 14 },
            .{ "upper", 15 },
            .{ "utf8Codepoints", 16 },
            .{ "utf8Len", 17 },
            .{ "utf8Valid", 18 },
        },
    );

    pub fn memberByName(vm: *VM, name: []const u8) !?Value {
        return if (members_name.get(name)) |idx|
            try member(vm, idx)
        else
            null;
    }

    pub fn member(vm: *VM, method_idx: usize) !Value {
        if (vm.gc.objstring_members[method_idx]) |umethod| {
            return umethod.toValue();
        }

        var native: *ObjNative = try vm.gc.allocateObject(
            ObjNative,
            .{
                // Complains about const qualifier discard otherwise
                .native = @as(*anyopaque, @ptrFromInt(@intFromPtr(members[method_idx]))),
            },
        );

        vm.gc.objstring_members[method_idx] = native;

        // We need to mark it otherwise it could be collected by a Young gc and then badly accessed by a Full gc
        vm.gc.markObj(native.toObj()) catch @panic("Could not mark obj");

        return native.toValue();
    }

    pub fn memberDefByName(parser: *Parser, name: []const u8) !?*ObjTypeDef {
        return if (members_name.get(name)) |idx|
            try memberDef(parser, idx)
        else
            null;
    }

    pub fn memberDef(parser: *Parser, method_idx: usize) !*ObjTypeDef {
        if (parser.gc.objstring_memberDefs[method_idx]) |umethod| {
            return umethod;
        }

        const native_type = try parser.parseTypeDefFrom(members_typedef[method_idx]);

        parser.gc.objstring_memberDefs[method_idx] = native_type;

        return native_type;
    }
};

/// Upvalue
pub const ObjUpValue = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .UpValue },

    /// Slot on the stack
    location: *Value,
    closed: ?Value,
    next: ?*ObjUpValue = null,

    pub fn init(slot: *Value) Self {
        return Self{
            .closed = null,
            .location = slot,
            .next = null,
        };
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markValue(self.location.*); // Useless
        if (self.closed) |uclosed| {
            try gc.markValue(uclosed);
        }
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .UpValue);
    }
};

/// Closure
pub const ObjClosure = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Closure },

    // Buzz function
    function: *ObjFunction,

    upvalues: std.ArrayList(*ObjUpValue),
    // Pointer to the global with which the function was declared
    globals: *std.ArrayList(Value),

    pub fn init(allocator: Allocator, vm: *VM, function: *ObjFunction) !Self {
        return Self{
            // TODO: copy?
            .globals = &vm.globals,
            .function = function,
            .upvalues = try std.ArrayList(*ObjUpValue).initCapacity(allocator, function.upvalue_count),
        };
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markObj(self.function.toObj());
        for (self.upvalues.items) |upvalue| {
            try gc.markObj(upvalue.toObj());
        }
        for (self.globals.items) |global| {
            try gc.markValue(global);
        }
    }

    pub fn deinit(self: *Self) void {
        self.upvalues.deinit();
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .Closure);
    }
};

pub const NativeCtx = extern struct {
    vm: *VM,
    globals: [*]Value,
    upvalues: [*]*ObjUpValue,
    // Where to reset the stack when we exit the function
    base: [*]Value,
    // Pointer to the stack_top field of the current fiber
    // !! Needs to change when current fiber changes !!
    stack_top: *[*]Value,
};

// 1 = return value on stack, 0 = no return value, -1 = error
pub const Native = fn (ctx: *NativeCtx) c_int;
pub const NativeFn = *const Native;

pub const Compiled = fn (ctx: *NativeCtx) Value;
pub const CompiledFn = *const Compiled;

/// Native function
pub const ObjNative = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Native },

    // TODO: issue is list.member which separate its type definition from its runtime creation
    // type_def: *ObjTypeDef,
    native: *anyopaque,

    pub fn mark(_: *Self, _: *GarbageCollector) void {}

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .Native);
    }
};

/// Function
pub const ObjFunction = struct {
    const Self = @This();

    pub const FunctionType = enum {
        Function,
        Method,
        Script, // Imported script
        ScriptEntryPoint, // main script
        EntryPoint, // main function
        Test,
        Anonymous,
        Extern,
        Abstract, // for protocol method, so we don't parse a body
        Repl,

        pub fn canHaveErrorSet(self: FunctionType) bool {
            return self != .Script and self != .ScriptEntryPoint and self != .Test and self != .Repl;
        }

        pub fn canYield(self: FunctionType) bool {
            return self == .Method or self == .Function or self == .Anonymous or self == .Abstract;
        }

        pub fn canOmitReturn(self: FunctionType) bool {
            return self != .Abstract and self != .Extern and self != .Script and self != .ScriptEntryPoint;
        }

        pub fn canOmitBody(self: FunctionType) bool {
            return self == .Abstract or self == .Extern;
        }
    };

    obj: Obj = .{ .obj_type = .Function },

    type_def: *ObjTypeDef = undefined, // Undefined because function initialization is in several steps

    chunk: Chunk,
    upvalue_count: u8 = 0,

    // So we can JIT the function at runtime
    node: Ast.Node.Index,
    // How many time the function was called
    call_count: u128 = 0,

    // JIT compiled function
    native_raw: ?*anyopaque = null,

    // JIT compiled function callable by buzz VM
    native: ?*anyopaque = null,

    pub fn init(allocator: Allocator, ast: Ast.Slice, node: Ast.Node.Index) !Self {
        return Self{
            .node = node,
            .chunk = Chunk.init(allocator, ast),
        };
    }

    pub fn deinit(self: *Self) void {
        self.chunk.deinit();
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markObj(@constCast(self.type_def.toObj()));
        if (BuildOptions.gc_debug) {
            io.print(
                "MARKING CONSTANTS OF FUNCTION @{} {s}\n",
                .{
                    @intFromPtr(self),
                    self.type_def.resolved_type.?.Function.name.string,
                },
            );
        }
        for (self.chunk.constants.items) |constant| {
            try gc.markValue(constant);
        }
        if (BuildOptions.gc_debug) {
            io.print(
                "DONE MARKING CONSTANTS OF FUNCTION @{} {s}\n",
                .{
                    @intFromPtr(self),
                    self.type_def.resolved_type.?.Function.name.string,
                },
            );
        }
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .Function);
    }

    pub const FunctionDef = struct {
        const FunctionDefSelf = @This();

        var next_id: usize = 0;

        id: usize,
        name: *ObjString,
        script_name: *ObjString,
        return_type: *ObjTypeDef,
        yield_type: *ObjTypeDef,
        error_types: ?[]const *ObjTypeDef = null,
        // TODO: rename 'arguments'
        parameters: std.AutoArrayHashMap(*ObjString, *ObjTypeDef),
        // Storing here the defaults means they can only be non-Obj values
        defaults: std.AutoArrayHashMap(*ObjString, Value),
        function_type: FunctionType = .Function,
        lambda: bool = false,

        generic_types: std.AutoArrayHashMap(*ObjString, *ObjTypeDef),
        resolved_generics: ?[]*ObjTypeDef = null,

        pub fn nextId() usize {
            FunctionDefSelf.next_id += 1;

            return FunctionDefSelf.next_id;
        }

        pub fn mark(self: *FunctionDefSelf, gc: *GarbageCollector) !void {
            try gc.markObj(self.name.toObj());
            try gc.markObj(self.script_name.toObj());
            try gc.markObj(@constCast(self.return_type.toObj()));
            try gc.markObj(@constCast(self.yield_type.toObj()));

            var it = self.parameters.iterator();
            while (it.next()) |parameter| {
                try gc.markObj(parameter.key_ptr.*.toObj());
                try gc.markObj(@constCast(parameter.value_ptr.*.toObj()));
            }

            var it2 = self.defaults.iterator();
            while (it2.next()) |default| {
                try gc.markObj(default.key_ptr.*.toObj());
                try gc.markValue(default.value_ptr.*);
            }

            if (self.error_types) |error_types| {
                for (error_types) |error_item| {
                    try gc.markObj(@constCast(error_item.toObj()));
                }
            }

            var it3 = self.generic_types.iterator();
            while (it3.next()) |kv| {
                try gc.markObj(kv.key_ptr.*.toObj());
                try gc.markObj(@constCast(kv.value_ptr.*.toObj()));
            }
        }
    };
};

/// Object instance
pub const ObjObjectInstance = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .ObjectInstance },

    /// Object (null when anonymous)
    object: ?*ObjObject,
    /// Populated object type
    type_def: *ObjTypeDef,
    /// Fields value
    fields: []Value,
    /// VM in which the instance was created, we need this so the instance destructor can be called in the appropriate vm
    vm: *VM,

    pub fn setField(self: *Self, gc: *GarbageCollector, key: usize, value: Value) !void {
        self.fields[key] = value;
        try gc.markObjDirty(&self.obj);
    }

    /// Should not be called by runtime when possible
    pub fn setFieldByName(self: *Self, gc: *GarbageCollector, key: *ObjString, value: Value) !void {
        const object_def = self.type_def.resolved_type.?.ObjectInstance.resolved_type.?.Object;
        const index = std.mem.indexOf(
            *ObjString,
            object_def.fields.keys(),
            key,
        );

        self.setField(
            gc,
            index,
            value,
        );
    }

    pub fn init(
        vm: *VM,
        object: ?*ObjObject,
        type_def: *ObjTypeDef,
        gc: *GarbageCollector,
    ) !Self {
        return .{
            .vm = vm,
            .object = object,
            .type_def = type_def,
            .fields = try gc.allocateMany(
                Value,
                type_def.resolved_type.?.ObjectInstance.of
                    .resolved_type.?.Object
                    .propertiesCount(),
            ),
        };
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        if (self.object) |object| {
            try gc.markObj(object.toObj());
        }
        try gc.markObj(@constCast(self.type_def.toObj()));
        for (self.fields) |field| {
            try gc.markValue(field);
        }
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.fields);
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .ObjectInstance);
    }

    fn is(self: *Self, type_def: *ObjTypeDef) bool {
        if (type_def.def_type == .ObjectInstance) {
            return self.type_def.resolved_type.?.ObjectInstance.of == type_def.resolved_type.?.ObjectInstance.of;
        } else if (type_def.def_type == .Protocol) {
            return self.type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object.conforms_to.get(type_def) != null;
        } else if (type_def.def_type == .Object) {
            return self.type_def.resolved_type.?.ObjectInstance.of == type_def;
        } else if (type_def.def_type == .ProtocolInstance) {
            return self.type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object.conforms_to.get(type_def.resolved_type.?.ProtocolInstance.of) != null;
        }

        return false;
    }
};

/// FFI struct or union
pub const ObjForeignContainer = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .ForeignContainer },

    type_def: *ObjTypeDef,
    data: []u8,

    pub fn init(vm: *VM, type_def: *ObjTypeDef) !Self {
        const zig_type = type_def.resolved_type.?.ForeignContainer.zig_type;

        return .{
            .type_def = type_def,
            .data = try vm.gc.allocateMany(u8, zig_type.size()),
        };
    }

    pub fn setField(self: *Self, vm: *VM, field_idx: usize, value: Value) !void {
        self.type_def.resolved_type.?.ForeignContainer.fields.values()[field_idx].setter(
            vm,
            self.data.ptr,
            value,
        );
        try vm.gc.markObjDirty(&self.obj);
    }

    pub fn getField(self: *Self, vm: *VM, field_idx: usize) Value {
        return self.type_def.resolved_type.?.ForeignContainer.fields.values()[field_idx].getter(
            vm,
            self.data.ptr,
        );
    }

    fn is(self: *Self, type_def: *ObjTypeDef) bool {
        return self.type_def == type_def;
    }

    pub const ContainerDef = struct {
        pub const Getter = fn (vm: *VM, data: [*]u8) Value;
        pub const Setter = fn (vm: *VM, data: [*]u8, value: Value) void;

        pub const Field = struct {
            offset: usize,
            getter: *Getter,
            setter: *Setter,
        };

        location: Token,
        name: *ObjString,
        qualified_name: *ObjString,

        zig_type: ZigType,
        buzz_type: std.StringArrayHashMap(*ObjTypeDef),

        // Filled by codegen
        fields: std.StringArrayHashMap(Field),

        pub fn mark(def: *ContainerDef, gc: *GarbageCollector) !void {
            try gc.markObj(def.name.toObj());
            try gc.markObj(def.qualified_name.toObj());
            var it = def.buzz_type.iterator();
            while (it.next()) |kv| {
                try gc.markObj(@constCast(kv.value_ptr.*.toObj()));
            }
        }
    };

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markObj(@constCast(self.type_def.toObj()));
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .ForeignContainer);
    }
};

/// Object
pub const ObjObject = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Object },

    type_def: *ObjTypeDef,

    /// Static fields and methods
    fields: []Value,
    /// Properties default values (null if none)
    defaults: []?Value,

    /// To avoid counting object fields that are instance properties
    property_count: ?usize = 0,

    pub fn init(allocator: Allocator, type_def: *ObjTypeDef) !Self {
        const self = Self{
            .fields = try allocator.alloc(
                Value,
                type_def.resolved_type.?.Object.fields.count(),
            ),
            .defaults = try allocator.alloc(
                ?Value,
                type_def.resolved_type.?.Object.propertiesCount(),
            ),
            .type_def = type_def,
        };

        return self;
    }

    pub fn propertyCount(self: *Self) usize {
        if (self.property_count) |pc| {
            return pc;
        }

        var property_count: usize = 0;
        var it = self.fields.iterator();
        while (it.next()) |kv| {
            property_count += if (!kv.value_ptr.*.static and !kv.value_ptr.*.method)
                1
            else
                0;
        }

        self.property_count = property_count;

        return property_count;
    }

    pub fn setField(self: *Self, gc: *GarbageCollector, key: usize, value: Value) !void {
        self.fields[key] = value;
        try gc.markObjDirty(&self.obj);
    }

    pub fn setDefault(self: *Self, gc: *GarbageCollector, key: usize, value: Value) !void {
        self.defaults[key] = value;
        try gc.markObjDirty(&self.obj);
    }

    pub fn setPropertyDefaultValue(self: *Self, gc: *GarbageCollector, key: usize, value: Value) !void {
        self.defaults[key] = value;
        try gc.markObjDirty(&self.obj);
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markObj(@constCast(self.type_def.toObj()));

        for (self.fields) |field| {
            try gc.markValue(field);
        }

        for (self.defaults) |field_opt| {
            if (field_opt) |field| {
                try gc.markValue(field);
            }
        }
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.fields);
        allocator.free(self.defaults);
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .Object);
    }

    pub const ProtocolDef = struct {
        const ProtocolDefSelf = @This();

        pub const Method = struct {
            mutable: bool,
            type_def: *ObjTypeDef,
        };

        location: Token,
        name: *ObjString,
        qualified_name: *ObjString,
        methods: std.StringArrayHashMapUnmanaged(Method),
        methods_locations: std.StringArrayHashMapUnmanaged(Token),

        pub fn init(location: Token, name: *ObjString, qualified_name: *ObjString) ProtocolDefSelf {
            return ProtocolDefSelf{
                .name = name,
                .location = location,
                .qualified_name = qualified_name,
                .methods = std.StringArrayHashMapUnmanaged(Method){},
                .methods_locations = std.StringArrayHashMapUnmanaged(Token){},
            };
        }

        pub fn deinit(self: *ProtocolDefSelf) void {
            self.methods.deinit();
            self.methods_locations.deinit();
        }

        pub fn mark(self: *ProtocolDefSelf, gc: *GarbageCollector) !void {
            try gc.markObj(self.name.toObj());
            try gc.markObj(self.qualified_name.toObj());

            var it = self.methods.iterator();
            while (it.next()) |kv| {
                try gc.markObj(@constCast(kv.value_ptr.*.type_def.toObj()));
            }
        }
    };

    pub const ObjectDef = struct {
        var next_id: usize = 0;

        pub const Field = struct {
            name: []const u8,
            location: Token,
            type_def: *ObjTypeDef,
            final: bool,
            method: bool,
            static: bool,
            has_default: bool,
            mutable: bool,
            // If the field is a static property or a method or an instance property, the index is not the same
            index: usize,

            pub fn eql(self: Field, other: Field) bool {
                return std.mem.eql(u8, self.name, other.name) and
                    self.type_def.eql(other.type_def) and
                    self.final == other.final and
                    self.method == other.method and
                    self.static == other.static and
                    self.mutable == other.mutable and
                    self.has_default == other.has_default;
            }
        };

        id: usize,
        location: Token,
        name: *ObjString,
        qualified_name: *ObjString,
        fields: std.StringArrayHashMapUnmanaged(Field),
        // When we have placeholders we don't know if they are properties or methods
        // That information is available only when the placeholder is resolved
        placeholders: std.StringHashMapUnmanaged(*ObjTypeDef),
        static_placeholders: std.StringHashMapUnmanaged(*ObjTypeDef),
        anonymous: bool,
        conforms_to: std.AutoHashMapUnmanaged(*ObjTypeDef, void),

        generic_types: std.AutoArrayHashMapUnmanaged(*ObjString, *ObjTypeDef),
        resolved_generics: ?[]*ObjTypeDef = null,

        pub fn nextId() usize {
            ObjectDef.next_id += 1;

            return ObjectDef.next_id;
        }

        pub fn init(
            location: Token,
            name: *ObjString,
            qualified_name: *ObjString,
            anonymous: bool,
        ) ObjectDef {
            return ObjectDef{
                .id = ObjectDef.nextId(),
                .name = name,
                .location = location,
                .qualified_name = qualified_name,
                .fields = std.StringArrayHashMapUnmanaged(Field){},
                .placeholders = std.StringHashMapUnmanaged(*ObjTypeDef){},
                .static_placeholders = std.StringHashMapUnmanaged(*ObjTypeDef){},
                .anonymous = anonymous,
                .conforms_to = std.AutoHashMapUnmanaged(*ObjTypeDef, void){},
                .generic_types = std.AutoArrayHashMapUnmanaged(*ObjString, *ObjTypeDef){},
            };
        }

        fn compareStrings(_: void, lhs: []const u8, rhs: []const u8) bool {
            return std.mem.order(u8, lhs, rhs).compare(std.math.CompareOperator.lt);
        }

        // Anonymous object can have the same fields but in a different order
        // This function will sort them by name and fix their index accordingly
        pub fn sortFieldIndexes(self: *ObjectDef, allocator: Allocator) !void {
            // Don't do anything on regular objects
            if (!self.anonymous) {
                return;
            }

            const fields = try allocator.alloc(
                []const u8,
                self.fields.keys().len,
            );
            defer allocator.free(fields);
            std.mem.copyForwards(
                []const u8,
                fields,
                self.fields.keys(),
            );

            std.mem.sort(
                []const u8,
                fields,
                {},
                compareStrings,
            );

            for (fields, 0..) |field_name, index| {
                const original_field = self.fields.get(field_name).?;
                try self.fields.put(
                    allocator,
                    field_name,
                    .{
                        .name = original_field.name,
                        .type_def = original_field.type_def,
                        .final = original_field.final,
                        .method = original_field.method,
                        .static = original_field.static,
                        .location = original_field.location,
                        .has_default = original_field.has_default,
                        .mutable = original_field.mutable,
                        .index = index,
                    },
                );
            }
        }

        pub fn deinit(self: *ObjectDef) void {
            self.fields.deinit();
            self.placeholders.deinit();
            self.static_placeholders.deinit();
            self.conforms_to.deinit();
            self.generic_types.deinit();
        }

        pub fn propertiesCount(self: ObjectDef) usize {
            var count: usize = 0;
            var it = self.fields.iterator();
            while (it.next()) |kv| {
                const field = kv.value_ptr.*;

                if (!field.method and !field.static) {
                    count += 1;
                }
            }

            return count;
        }

        // Do they both conform to a common protocol?
        pub fn bothConforms(self: ObjectDef, other: ObjectDef) ?*ObjTypeDef {
            var it = self.conforms_to.iterator();
            while (it.next()) |kv| {
                if (other.conforms_to.get(kv.key_ptr.*) != null) {
                    return kv.key_ptr.*;
                }
            }

            return null;
        }

        pub fn mark(self: *ObjectDef, gc: *GarbageCollector) !void {
            try gc.markObj(self.name.toObj());
            try gc.markObj(self.qualified_name.toObj());

            var it = self.fields.iterator();
            while (it.next()) |kv| {
                try gc.markObj(@constCast(kv.value_ptr.*.type_def.toObj()));
            }

            var it5 = self.placeholders.iterator();
            while (it5.next()) |kv| {
                try gc.markObj(@constCast(kv.value_ptr.*.toObj()));
            }

            var it6 = self.static_placeholders.iterator();
            while (it6.next()) |kv| {
                try gc.markObj(@constCast(kv.value_ptr.*.toObj()));
            }

            var it7 = self.conforms_to.iterator();
            while (it7.next()) |kv| {
                try gc.markObj(@constCast(kv.key_ptr.*.toObj()));
            }

            var it8 = self.generic_types.iterator();
            while (it8.next()) |kv| {
                try gc.markObj(kv.key_ptr.*.toObj());
                try gc.markObj(@constCast(kv.value_ptr.*.toObj()));
            }
        }
    };
};

/// List
pub const ObjList = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .List },

    type_def: *ObjTypeDef,

    /// List items
    items: std.ArrayListUnmanaged(Value),

    methods: []?*ObjNative,

    pub fn init(allocator: Allocator, type_def: *ObjTypeDef) !Self {
        const self = Self{
            .items = std.ArrayListUnmanaged(Value){},
            .type_def = type_def,
            .methods = try allocator.alloc(
                ?*ObjNative,
                Self.members.len,
            ),
        };

        for (0..Self.members.len) |i| {
            self.methods[i] = null;
        }

        return self;
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        for (self.items.items) |value| {
            try gc.markValue(value);
        }
        try gc.markObj(@constCast(self.type_def.toObj()));

        for (self.methods) |method_opt| {
            if (method_opt) |method| {
                try gc.markObj(method.toObj());
            }
        }
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.items.deinit(allocator);
        allocator.free(self.methods);
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .List);
    }

    pub const members = [_]NativeFn{
        buzz_builtin.list.append,
        buzz_builtin.list.fill,
        buzz_builtin.list.filter,
        buzz_builtin.list.forEach,
        buzz_builtin.list.indexOf,
        buzz_builtin.list.insert,
        buzz_builtin.list.join,
        buzz_builtin.list.len,
        buzz_builtin.list.map,
        buzz_builtin.list.next,
        buzz_builtin.list.pop,
        buzz_builtin.list.reduce,
        buzz_builtin.list.remove,
        buzz_builtin.list.reverse,
        buzz_builtin.list.sort,
        buzz_builtin.list.sub,
        buzz_builtin.list.cloneMutable,
        buzz_builtin.list.cloneImmutable,
    };

    // TODO: could probably build this in a comptime block?
    pub const members_name = std.StaticStringMap(usize).initComptime(
        .{
            .{ "append", 0 },
            .{ "fill", 1 },
            .{ "filter", 2 },
            .{ "forEach", 3 },
            .{ "indexOf", 4 },
            .{ "insert", 5 },
            .{ "join", 6 },
            .{ "len", 7 },
            .{ "map", 8 },
            .{ "next", 9 },
            .{ "pop", 10 },
            .{ "reduce", 11 },
            .{ "remove", 12 },
            .{ "reverse", 13 },
            .{ "sort", 14 },
            .{ "sub", 15 },
            .{ "cloneMutable", 16 },
            .{ "cloneImmutable", 17 },
            // The difference between clone and copy is only a semantic one so we use the same functions
            .{ "copyMutable", 16 },
            .{ "copyImmutable", 17 },
        },
    );

    pub fn member(self: *Self, vm: *VM, method_idx: usize) !Value {
        if (self.methods[method_idx]) |native| {
            return native.toValue();
        }

        var native: *ObjNative = try vm.gc.allocateObject(
            ObjNative,
            .{
                // Complains about const qualifier discard otherwise
                .native = @as(*anyopaque, @ptrFromInt(@intFromPtr(members[method_idx]))),
            },
        );

        self.methods[method_idx] = native;

        // We need to mark it otherwise it could be collected by a Young gc and then badly accessed by a Full gc
        vm.gc.markObj(native.toObj()) catch @panic("Could not mark obj");

        return native.toValue();
    }

    pub fn rawAppend(self: *Self, gc: *GarbageCollector, value: Value) !void {
        try self.items.append(
            gc.allocator,
            value,
        );
        try gc.markObjDirty(&self.obj);
    }

    pub fn rawInsert(self: *Self, gc: *GarbageCollector, index: usize, value: Value) !void {
        try self.items.insert(
            gc.allocator,
            index,
            value,
        );
        try gc.markObjDirty(&self.obj);
    }

    pub fn set(self: *Self, gc: *GarbageCollector, index: usize, value: Value) !void {
        self.items.items[index] = value;
        try gc.markObjDirty(&self.obj);
    }

    // Used also by the VM
    pub fn rawNext(self: *Self, vm: *VM, list_index: ?i32) !?i32 {
        if (list_index) |index| {
            if (index < 0 or index >= @as(i32, @intCast(self.items.items.len))) {
                try vm.throw(
                    VM.Error.OutOfBound,
                    (try vm.gc.copyString("Out of bound access to list")).toValue(),
                    null,
                    null,
                );
            }

            return if (index + 1 >= @as(i32, @intCast(self.items.items.len)))
                null
            else
                index + 1;
        } else {
            return if (self.items.items.len > 0) @as(i32, 0) else null;
        }
    }

    pub const ListDef = struct {
        const SelfListDef = @This();

        pub const Method = struct {
            mutable: bool,
            type_def: *ObjTypeDef,
        };

        item_type: *ObjTypeDef,
        methods: std.StringHashMapUnmanaged(Method),
        mutable: bool,

        pub fn init(item_type: *ObjTypeDef, mutable: bool) SelfListDef {
            return .{
                .item_type = item_type,
                .mutable = mutable,
                .methods = std.StringHashMapUnmanaged(Method){},
            };
        }

        pub fn deinit(self: *SelfListDef) void {
            self.methods.deinit();
        }

        pub fn mark(self: *SelfListDef, gc: *GarbageCollector) !void {
            try gc.markObj(@constCast(self.item_type.toObj()));
            var it = self.methods.iterator();
            while (it.next()) |method| {
                try gc.markObj(@constCast(method.value_ptr.*.type_def.toObj()));
            }
        }

        pub fn member(obj_list: *ObjTypeDef, parser: *Parser, method: []const u8) !?Method {
            var self = &obj_list.resolved_type.?.List;

            if (self.methods.get(method)) |native_def| {
                return native_def;
            }

            if (mem.eql(u8, method, "append")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                // `value` arg is of item_type
                try parameters.put(try parser.gc.copyString("value"), self.item_type);

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("append"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = parser.gc.type_registry.void_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = true,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "append",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "remove")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                try parameters.put(try parser.gc.copyString("at"), parser.gc.type_registry.int_type);

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("remove"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = try parser.gc.type_registry.getTypeDef(.{
                                    .optional = true,
                                    .def_type = self.item_type.def_type,
                                    .resolved_type = self.item_type.resolved_type,
                                }),
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = true,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "remove",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "len")) {
                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("len"),
                                .parameters = .init(parser.gc.allocator),
                                .defaults = .init(parser.gc.allocator),
                                .return_type = parser.gc.type_registry.int_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "len",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "next")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                // `key` arg is number
                try parameters.put(
                    try parser.gc.copyString("key"),
                    try parser.gc.type_registry.getTypeDef(
                        ObjTypeDef{
                            .def_type = .Integer,
                            .optional = true,
                        },
                    ),
                );
                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("next"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                // When reached end of list, returns null
                                .return_type = try parser.gc.type_registry.getTypeDef(
                                    ObjTypeDef{
                                        .def_type = .Integer,
                                        .optional = true,
                                    },
                                ),
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "next",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "sub")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put(
                    try parser.gc.copyString("start"),
                    parser.gc.type_registry.int_type,
                );

                const len_str = try parser.gc.copyString("len");
                try parameters.put(
                    len_str,
                    try parser.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Integer,
                            .optional = true,
                        },
                    ),
                );

                var defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator);
                try defaults.put(len_str, Value.Null);

                const native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("sub"),
                                .parameters = parameters,
                                .defaults = defaults,
                                .return_type = obj_list,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "sub",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "fill")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put(
                    try parser.gc.copyString("value"),
                    parser.gc.type_registry.any_type,
                );

                const start_str = try parser.gc.copyString("start");
                try parameters.put(
                    start_str,
                    try parser.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Integer,
                            .optional = true,
                        },
                    ),
                );

                const len_str = try parser.gc.copyString("len");
                try parameters.put(
                    len_str,
                    try parser.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Integer,
                            .optional = true,
                        },
                    ),
                );

                var defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator);
                try defaults.put(len_str, Value.Null);
                try defaults.put(start_str, Value.Null);

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("fill"),
                                .parameters = parameters,
                                .defaults = defaults,
                                .return_type = obj_list,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = true,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "fill",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "indexOf")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put(try parser.gc.copyString("needle"), self.item_type);

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("indexOf"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = try parser.gc.type_registry.getTypeDef(
                                    .{
                                        .def_type = .Integer,
                                        .optional = true,
                                    },
                                ),
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "indexOf",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "join")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put(try parser.gc.copyString("separator"), parser.gc.type_registry.str_type);

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("join"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = parser.gc.type_registry.str_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "join",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "insert")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                try parameters.put(
                    try parser.gc.copyString("index"),
                    parser.gc.type_registry.int_type,
                );

                // `value` arg is of item_type
                try parameters.put(try parser.gc.copyString("value"), self.item_type);

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("insert"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = try self.item_type.cloneOptional(&parser.gc.type_registry),
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = true,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "insert",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "pop")) {
                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("pop"),
                                .parameters = .init(parser.gc.allocator),
                                .defaults = .init(parser.gc.allocator),
                                .return_type = try self.item_type.cloneOptional(&parser.gc.type_registry),
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = true,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "pop",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "forEach")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(
                    try parser.gc.copyString("index"),
                    parser.gc.type_registry.int_type,
                );
                try callback_parameters.put(
                    try parser.gc.copyString("element"),
                    self.item_type,
                );

                const callback_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = parser.gc.type_registry.void_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("forEach"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = parser.gc.type_registry.void_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "forEach",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "map")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                const map_origin = ObjFunction.FunctionDef.nextId();
                const generic_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Generic,
                        .resolved_type = .{
                            .Generic = .{
                                .origin = map_origin,
                                .index = 0,
                            },
                        },
                    },
                );

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(
                    try parser.gc.copyString("index"),
                    parser.gc.type_registry.int_type,
                );
                try callback_parameters.put(
                    try parser.gc.copyString("element"),
                    self.item_type,
                );

                const callback_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = generic_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                // FIXME: user could provide an .Extern function and JIT will be lost here
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );

                const new_list_def = ObjList.ListDef.init(
                    generic_type,
                    false,
                );

                const new_list_type = ObjTypeDef.TypeUnion{ .List = new_list_def };

                var method_def = ObjFunction.FunctionDef{
                    .id = map_origin,
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("map"),
                    .parameters = parameters,
                    .defaults = .init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(.{
                        .def_type = .List,
                        .resolved_type = new_list_type,
                    }),
                    .yield_type = parser.gc.type_registry.void_type,
                    .generic_types = .init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                try method_def.generic_types.put(
                    try parser.gc.copyString("T"),
                    generic_type,
                );

                const resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                const native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = resolved_type,
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "map",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "filter")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(
                    try parser.gc.copyString("index"),
                    parser.gc.type_registry.int_type,
                );

                try callback_parameters.put(
                    try parser.gc.copyString("element"),
                    self.item_type,
                );

                const callback_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = parser.gc.type_registry.bool_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("filter"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = obj_list,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "filter",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "reduce")) {
                const reduce_origin = ObjFunction.FunctionDef.nextId();

                // Mapped type
                const generic_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Generic,
                        .resolved_type = .{
                            .Generic = .{
                                .origin = reduce_origin,
                                .index = 0,
                            },
                        },
                    },
                );

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(
                    try parser.gc.copyString("index"),
                    parser.gc.type_registry.int_type,
                );
                try callback_parameters.put(
                    try parser.gc.copyString("element"),
                    self.item_type,
                );
                try callback_parameters.put(
                    try parser.gc.copyString("accumulator"),
                    generic_type,
                );

                const callback_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = generic_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );
                try parameters.put(
                    try parser.gc.copyString("initial"),
                    generic_type,
                );

                var generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);
                try generic_types.put(try parser.gc.copyString("T"), generic_type);

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = reduce_origin,
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("reduce"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = generic_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = generic_types,
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "reduce",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "sort")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(
                    try parser.gc.copyString("left"),
                    self.item_type,
                );
                try callback_parameters.put(
                    try parser.gc.copyString("right"),
                    self.item_type,
                );

                const callback_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = parser.gc.type_registry.bool_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("sort"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = obj_list,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = true,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "sort",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "reverse")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("reverse"),
                                .parameters = .init(parser.gc.allocator),
                                .defaults = .init(parser.gc.allocator),
                                .return_type = obj_list,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "reverse",
                    member_def,
                );

                return member_def;
            } else if ((self.mutable and (mem.eql(u8, method, "cloneImmutable") or
                mem.eql(u8, method, "copyMutable"))) or
                (!self.mutable and (mem.eql(u8, method, "cloneMutable") or
                mem.eql(u8, method, "copyImmutable"))))
            {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                const member_def = Method{
                    .type_def = try parser.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Function,
                            .resolved_type = .{
                                .Function = .{
                                    .id = ObjFunction.FunctionDef.nextId(),
                                    .script_name = try parser.gc.copyString("builtin"),
                                    .name = try parser.gc.copyString(method),
                                    .parameters = .init(parser.gc.allocator),
                                    .defaults = .init(parser.gc.allocator),
                                    .return_type = try obj_list.cloneMutable(
                                        &parser.gc.type_registry,
                                        mem.eql(u8, method, "cloneMutable") or mem.eql(u8, method, "copyMutable"),
                                    ),
                                    .yield_type = parser.gc.type_registry.void_type,
                                    .generic_types = .init(parser.gc.allocator),
                                    .function_type = .Extern,
                                },
                            },
                        },
                    ),
                    .mutable = mem.eql(u8, method, "cloneImmutable") or mem.eql(u8, method, "copyMutable"),
                };

                try self.methods.put(
                    parser.gc.allocator,
                    method,
                    member_def,
                );

                return member_def;
            }

            return null;
        }
    };
};

pub const ObjRange = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Range },

    low: Integer,
    high: Integer,

    pub fn mark(_: *Self, _: *GarbageCollector) void {}

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .Range);
    }

    pub const members = [_]NativeFn{
        buzz_builtin.range.high,
        buzz_builtin.range.intersect,
        buzz_builtin.range.invert,
        buzz_builtin.range.len,
        buzz_builtin.range.low,
        buzz_builtin.range.subsetOf,
        buzz_builtin.range.toList,
        buzz_builtin.range.@"union",
        buzz_builtin.range.contains,
    };

    const members_typedef = [_][]const u8{
        "extern fun high() > int",
        "extern fun intersect(other: rg) > rg",
        "extern fun invert() > rg",
        "extern fun len() > int",
        "extern fun low() > int",
        "extern fun subsetOf(other: rg) > bool",
        "extern fun toList() > [int]",
        "extern fun union(other: rg) > rg",
        "extern fun contains(value: int) > bool",
    };

    pub const members_name = std.StaticStringMap(usize).initComptime(
        .{
            .{ "high", 0 },
            .{ "intersect", 1 },
            .{ "invert", 2 },
            .{ "len", 3 },
            .{ "low", 4 },
            .{ "subsetOf", 5 },
            .{ "toList", 6 },
            .{ "union", 7 },
            .{ "contains", 8 },
        },
    );

    pub fn memberByName(vm: *VM, name: []const u8) !?Value {
        return if (members_name.get(name)) |idx|
            try member(vm, idx)
        else
            null;
    }

    pub fn member(vm: *VM, method_idx: usize) !Value {
        if (vm.gc.objrange_members[method_idx]) |native| {
            return native.toValue();
        }

        var native: *ObjNative = try vm.gc.allocateObject(
            ObjNative,
            .{
                // Complains about const qualifier discard otherwise
                .native = @as(*anyopaque, @ptrFromInt(@intFromPtr(members[method_idx]))),
            },
        );

        vm.gc.objrange_members[method_idx] = native;

        // We need to mark it otherwise it could be collected by a Young gc and then badly accessed by a Full gc
        vm.gc.markObj(native.toObj()) catch @panic("Could not mark obj");

        return native.toValue();
    }

    pub fn memberDefByName(parser: *Parser, name: []const u8) !?*ObjTypeDef {
        return if (members_name.get(name)) |idx|
            try memberDef(parser, idx)
        else
            null;
    }

    pub fn memberDef(parser: *Parser, method_idx: usize) !*ObjTypeDef {
        if (parser.gc.objrange_memberDefs[method_idx]) |umethod| {
            return umethod;
        }

        const native_type = try parser.parseTypeDefFrom(members_typedef[method_idx]);

        parser.gc.objrange_memberDefs[method_idx] = native_type;

        return native_type;
    }
};

/// Map
pub const ObjMap = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Map },

    type_def: *ObjTypeDef,

    // We need an ArrayHashMap for `next`
    map: std.AutoArrayHashMapUnmanaged(Value, Value),

    methods: []?*ObjNative,

    pub fn init(allocator: Allocator, type_def: *ObjTypeDef) !Self {
        const self = Self{
            .type_def = type_def,
            .map = std.AutoArrayHashMapUnmanaged(Value, Value){},
            .methods = try allocator.alloc(
                ?*ObjNative,
                Self.members.len,
            ),
        };

        for (0..Self.members.len) |i| {
            self.methods[i] = null;
        }

        return self;
    }

    pub fn set(self: *Self, gc: *GarbageCollector, key: Value, value: Value) !void {
        try self.map.put(
            gc.allocator,
            key,
            value,
        );
        try gc.markObjDirty(&self.obj);
    }

    pub const members = [_]NativeFn{
        buzz_builtin.map.cloneMutable,
        buzz_builtin.map.diff,
        buzz_builtin.map.filter,
        buzz_builtin.map.forEach,
        buzz_builtin.map.intersect,
        buzz_builtin.map.keys,
        buzz_builtin.map.map,
        buzz_builtin.map.reduce,
        buzz_builtin.map.remove,
        buzz_builtin.map.size,
        buzz_builtin.map.sort,
        buzz_builtin.map.values,
        buzz_builtin.map.cloneImmutable,
        buzz_builtin.map.cloneMutable,
        buzz_builtin.map.cloneImmutable,
    };

    pub const members_name = std.StaticStringMap(usize).initComptime(
        .{
            .{ "cloneMutable", 0 },
            .{ "diff", 1 },
            .{ "filter", 2 },
            .{ "forEach", 3 },
            .{ "intersect", 4 },
            .{ "keys", 5 },
            .{ "map", 6 },
            .{ "reduce", 7 },
            .{ "remove", 8 },
            .{ "size", 9 },
            .{ "sort", 10 },
            .{ "values", 11 },
            .{ "cloneImmutable", 12 },
            .{ "copyMutable", 13 },
            .{ "copyImmutable", 14 },
        },
    );

    pub fn member(self: *Self, vm: *VM, method_idx: usize) !Value {
        if (self.methods[method_idx]) |native| {
            return native.toValue();
        }

        const native: *ObjNative = try vm.gc.allocateObject(
            ObjNative,
            .{
                // Complains about const qualifier discard otherwise
                .native = @constCast(members[method_idx]),
            },
        );

        self.methods[method_idx] = native;

        // We need to mark it otherwise it could be collected by a Young gc and then badly accessed by a Full gc
        vm.gc.markObj(native.toObj()) catch @panic("Could not mark obj");

        return native.toValue();
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        var it = self.map.iterator();
        while (it.next()) |kv| {
            try gc.markValue(kv.key_ptr.*);
            try gc.markValue(kv.value_ptr.*);
        }

        for (self.methods) |method_opt| {
            if (method_opt) |method| {
                try gc.markObj(method.toObj());
            }
        }

        try gc.markObj(@constCast(self.type_def.toObj()));
    }

    pub fn rawNext(self: *Self, key: ?Value) ?Value {
        const map_keys: []Value = self.map.keys();

        if (key) |ukey| {
            const index: usize = self.map.getIndex(ukey).?;

            if (index < map_keys.len - 1) {
                return map_keys[index + 1];
            } else {
                return null;
            }
        } else {
            return if (map_keys.len > 0) map_keys[0] else null;
        }
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.map.deinit(allocator);
        allocator.free(self.methods);
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .Map);
    }

    pub const MapDef = struct {
        const SelfMapDef = @This();

        pub const Method = struct {
            mutable: bool,
            type_def: *ObjTypeDef,
        };

        key_type: *ObjTypeDef,
        value_type: *ObjTypeDef,
        mutable: bool,

        methods: std.StringHashMapUnmanaged(Method),

        pub fn init(key_type: *ObjTypeDef, value_type: *ObjTypeDef, mutable: bool) SelfMapDef {
            return .{
                .key_type = key_type,
                .value_type = value_type,
                .mutable = mutable,
                .methods = std.StringHashMapUnmanaged(Method){},
            };
        }

        pub fn deinit(self: *SelfMapDef) void {
            self.methods.deinit();
        }

        pub fn mark(self: *SelfMapDef, gc: *GarbageCollector) !void {
            try gc.markObj(@constCast(self.key_type.toObj()));
            try gc.markObj(@constCast(self.value_type.toObj()));
            var it = self.methods.iterator();
            while (it.next()) |method| {
                try gc.markObj(@constCast(method.value_ptr.*.type_def.toObj()));
            }
        }

        pub fn member(obj_map: *ObjTypeDef, parser: *Parser, method: []const u8) !?Method {
            var self = &obj_map.resolved_type.?.Map;

            if (self.methods.get(method)) |native_def| {
                return native_def;
            }

            if (mem.eql(u8, method, "size")) {
                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("size"),
                                .parameters = .init(parser.gc.allocator),
                                .defaults = .init(parser.gc.allocator),
                                .return_type = parser.gc.type_registry.int_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "size",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "remove")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                try parameters.put(try parser.gc.copyString("at"), self.key_type);

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("remove"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = try parser.gc.type_registry.getTypeDef(
                                    .{
                                        .optional = true,
                                        .def_type = self.value_type.def_type,
                                        .resolved_type = self.value_type.resolved_type,
                                    },
                                ),
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = true,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "remove",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "keys")) {
                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("keys"),
                                .parameters = .init(parser.gc.allocator),
                                .defaults = .init(parser.gc.allocator),
                                .return_type = try parser.gc.type_registry.getTypeDef(
                                    .{
                                        .def_type = .List,
                                        .optional = false,
                                        .resolved_type = .{
                                            .List = ObjList.ListDef.init(
                                                self.key_type,
                                                false,
                                            ),
                                        },
                                    },
                                ),
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "keys",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "values")) {
                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("values"),
                                .parameters = .init(parser.gc.allocator),
                                .defaults = .init(parser.gc.allocator),
                                .return_type = try parser.gc.type_registry.getTypeDef(.{
                                    .def_type = .List,
                                    .optional = false,
                                    .resolved_type = .{
                                        .List = ObjList.ListDef.init(
                                            self.value_type,
                                            false,
                                        ),
                                    },
                                }),
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "values",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "sort")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(try parser.gc.copyString("left"), self.key_type);
                try callback_parameters.put(try parser.gc.copyString("right"), self.key_type);

                const callback_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = parser.gc.type_registry.bool_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("sort"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = obj_map,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = true,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "sort",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "diff")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("other"),
                    obj_map,
                );

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("diff"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = obj_map,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "diff",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "intersect")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("other"),
                    obj_map,
                );

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("intersect"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = obj_map,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "intersect",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "forEach")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(
                    try parser.gc.copyString("key"),
                    self.key_type,
                );
                try callback_parameters.put(
                    try parser.gc.copyString("value"),
                    self.value_type,
                );

                const callback_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = parser.gc.type_registry.void_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("forEach"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = parser.gc.type_registry.void_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "forEach",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "map")) {
                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(
                    try parser.gc.copyString("key"),
                    self.key_type,
                );
                try callback_parameters.put(
                    try parser.gc.copyString("value"),
                    self.value_type,
                );

                var callback_method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    // TODO: is this ok?
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("anonymous"),
                    .parameters = callback_parameters,
                    .defaults = .init(parser.gc.allocator),
                    .return_type = undefined,
                    .yield_type = parser.gc.type_registry.void_type,
                    .generic_types = .init(parser.gc.allocator),
                    // FIXME: user could provide an .Extern function and JIT will be lost here
                };

                const map_origin = ObjFunction.FunctionDef.nextId();

                // Mapped type
                const generic_key_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Generic,
                        .resolved_type = .{
                            .Generic = .{
                                .origin = map_origin,
                                .index = 0,
                            },
                        },
                    },
                );

                const generic_value_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Generic,
                        .resolved_type = .{
                            .Generic = .{
                                .origin = map_origin,
                                .index = 1,
                            },
                        },
                    },
                );

                // Calback return type
                var entry_def = ObjObject.ObjectDef.init(
                    Token.identifier("user callback"),
                    try parser.gc.copyString("anonymous"),
                    try parser.gc.copyString("anonymous"),
                    true,
                );

                try entry_def.fields.put(
                    parser.gc.allocator,
                    "key",
                    .{
                        .name = "key",
                        .type_def = generic_key_type,
                        .final = false,
                        .method = false,
                        .static = false,
                        .has_default = false,
                        .mutable = false,
                        .location = Token.identifier("key"),
                        .index = 0,
                    },
                );
                try entry_def.fields.put(
                    parser.gc.allocator,
                    "value",
                    .{
                        .name = "value",
                        .type_def = generic_value_type,
                        .final = false,
                        .method = false,
                        .static = false,
                        .has_default = false,
                        .mutable = false,
                        .location = Token.identifier("value"),
                        .index = 1,
                    },
                );

                const entry_type_def = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Object,
                        .resolved_type = .{ .Object = entry_def },
                    },
                );

                callback_method_def.return_type = try entry_type_def.toInstance(
                    &parser.gc.type_registry,
                    false,
                );

                const callback_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{ .Function = callback_method_def },
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );

                var method_def = ObjFunction.FunctionDef{
                    .id = map_origin,
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("map"),
                    .parameters = parameters,
                    .defaults = .init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(.{
                        .def_type = .Map,
                        .resolved_type = .{
                            .Map = ObjMap.MapDef.init(
                                generic_key_type,
                                generic_value_type,
                                false,
                            ),
                        },
                    }),
                    .yield_type = parser.gc.type_registry.void_type,
                    .generic_types = .init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                try method_def.generic_types.put(
                    try parser.gc.copyString("K"),
                    generic_key_type,
                );

                try method_def.generic_types.put(
                    try parser.gc.copyString("V"),
                    generic_value_type,
                );

                const native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = .{ .Function = method_def },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "map",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "filter")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(
                    try parser.gc.copyString("key"),
                    self.key_type,
                );
                try callback_parameters.put(
                    try parser.gc.copyString("value"),
                    self.value_type,
                );

                const callback_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = parser.gc.type_registry.bool_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("filter"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = obj_map,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "filter",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "reduce")) {
                const reduce_origin = ObjFunction.FunctionDef.nextId();

                // Mapped type
                const generic_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Generic,
                        .resolved_type = .{
                            .Generic = .{
                                .origin = reduce_origin,
                                .index = 0,
                            },
                        },
                    },
                );

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(
                    try parser.gc.copyString("key"),
                    self.key_type,
                );
                try callback_parameters.put(
                    try parser.gc.copyString("value"),
                    self.value_type,
                );
                try callback_parameters.put(
                    try parser.gc.copyString("accumulator"),
                    generic_type,
                );

                const callback_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = generic_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = .init(parser.gc.allocator),
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );
                try parameters.put(
                    try parser.gc.copyString("initial"),
                    generic_type,
                );

                var generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);
                try generic_types.put(try parser.gc.copyString("T"), generic_type);

                const native_type = try parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = reduce_origin,
                                .script_name = try parser.gc.copyString("builtin"),
                                .name = try parser.gc.copyString("reduce"),
                                .parameters = parameters,
                                .defaults = .init(parser.gc.allocator),
                                .return_type = generic_type,
                                .yield_type = parser.gc.type_registry.void_type,
                                .generic_types = generic_types,
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try self.methods.put(
                    parser.gc.allocator,
                    "reduce",
                    method_def,
                );

                return method_def;
            } else if ((self.mutable and (mem.eql(u8, method, "cloneImmutable") or
                mem.eql(u8, method, "copyMutable"))) or
                (!self.mutable and (mem.eql(u8, method, "cloneMutable") or
                mem.eql(u8, method, "copyImmutable"))))
            {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                const method_def = Method{
                    .type_def = try parser.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Function,
                            .resolved_type = .{
                                .Function = .{
                                    .id = ObjFunction.FunctionDef.nextId(),
                                    .script_name = try parser.gc.copyString("builtin"),
                                    .name = try parser.gc.copyString(method),
                                    .parameters = .init(parser.gc.allocator),
                                    .defaults = .init(parser.gc.allocator),
                                    .return_type = try obj_map.cloneMutable(
                                        &parser.gc.type_registry,
                                        mem.eql(u8, method, "cloneMutable") or mem.eql(u8, method, "copyMutable"),
                                    ),
                                    .yield_type = parser.gc.type_registry.void_type,
                                    .generic_types = .init(parser.gc.allocator),
                                    .function_type = .Extern,
                                },
                            },
                        },
                    ),
                    .mutable = mem.eql(u8, method, "cloneImmutable") or mem.eql(u8, method, "copyMutable"),
                };

                try self.methods.put(
                    parser.gc.allocator,
                    method,
                    method_def,
                );

                return method_def;
            }

            return null;
        }
    };
};

/// Enum
pub const ObjEnum = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Enum },

    /// Used to allow type checking at runtime
    type_def: *ObjTypeDef,

    cases: []Value,

    pub fn init(def: *ObjTypeDef) Self {
        return Self{
            .type_def = def,
            .cases = undefined,
        };
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markObj(@constCast(self.type_def.toObj()));
        for (self.cases) |case| {
            try gc.markValue(case);
        }
    }

    pub fn rawNext(self: *Self, vm: *VM, enum_case: ?*ObjEnumInstance) !?*ObjEnumInstance {
        if (enum_case) |case| {
            assert(case.enum_ref == self);

            if (case.case == self.cases.len - 1) {
                return null;
            }

            return try vm.gc.allocateObject(ObjEnumInstance, ObjEnumInstance{
                .enum_ref = self,
                .case = @as(u8, @intCast(case.case + 1)),
            });
        } else {
            return try vm.gc.allocateObject(ObjEnumInstance, ObjEnumInstance{
                .enum_ref = self,
                .case = 0,
            });
        }
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(&self.obj);
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .Enum);
    }

    pub const EnumDef = struct {
        const EnumDefSelf = @This();

        name: *ObjString,
        qualified_name: *ObjString,
        enum_type: *ObjTypeDef,
        // TODO: should be a slice
        cases: [][]const u8,
        // Circular reference but needed so that we can generate enum case at compile time
        value: ?*ObjEnum = null,

        pub fn init(name: *ObjString, qualified_name: *ObjString, enum_type: *ObjTypeDef, cases: [][]const u8) EnumDefSelf {
            return EnumDefSelf{
                .name = name,
                .qualified_name = qualified_name,
                .cases = cases,
                .enum_type = enum_type,
            };
        }

        pub fn deinit(self: *EnumDefSelf) void {
            self.cases.deinit();
        }

        pub fn mark(self: *EnumDefSelf, gc: *GarbageCollector) !void {
            try gc.markObj(self.name.toObj());
            try gc.markObj(self.qualified_name.toObj());
            try gc.markObj(@constCast(self.enum_type.toObj()));
        }
    };
};

pub const ObjEnumInstance = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .EnumInstance },

    enum_ref: *ObjEnum,
    case: u8,

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markObj(self.enum_ref.toObj());
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .EnumInstance);
    }

    pub fn value(self: *Self) Value {
        return self.enum_ref.cases[self.case];
    }
};

/// Bound
pub const ObjBoundMethod = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Bound },

    receiver: Value,
    closure: ?*ObjClosure = null,
    native: ?*ObjNative = null,

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markValue(self.receiver);
        if (self.closure) |closure| {
            try gc.markObj(closure.toObj());
        }
        if (self.native) |native| {
            try gc.markObj(native.toObj());
        }
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .Bound);
    }
};

/// Type
pub const ObjTypeDef = struct {
    const Self = @This();

    // WARN: order is important
    pub const Type = enum(u8) {
        Any,
        Bool,
        Double,
        Integer,
        Pattern,
        String,
        Type, // Something that holds a type, not an actual type
        UserData,
        Void,
        Range,

        Enum,
        EnumInstance,
        Fiber,
        ForeignContainer,
        Function,
        Generic,
        List,
        Map,
        Object,
        ObjectInstance,
        Placeholder, // Used in first-pass when we refer to a not yet parsed type
        Protocol,
        ProtocolInstance,
    };

    pub const GenericDef = struct {
        // Function def id rather than pointer since we don't get a definitive pointer until the function signature is fully parsed
        origin: usize,
        index: usize, // Index in generic list
    };

    // Always keep types with void value first.
    pub const TypeUnion = union(Type) {
        Any: bool, // true if mutable
        Bool: void,
        Double: void,
        Integer: void,
        Pattern: void,
        String: void,
        Type: void,
        UserData: void,
        Void: void,
        Range: void,

        Enum: ObjEnum.EnumDef,
        EnumInstance: Instance,
        Fiber: ObjFiber.FiberDef,
        ForeignContainer: ObjForeignContainer.ContainerDef,
        Function: ObjFunction.FunctionDef,
        Generic: GenericDef,
        List: ObjList.ListDef,
        Map: ObjMap.MapDef,
        Object: ObjObject.ObjectDef,
        ObjectInstance: Instance,
        Placeholder: PlaceholderDef,
        Protocol: ObjObject.ProtocolDef,
        ProtocolInstance: Instance,
    };

    pub const Instance = struct {
        of: *ObjTypeDef,
        mutable: bool,
    };

    obj: Obj = .{ .obj_type = .Type },

    /// True means its an optional (e.g `str?`)
    optional: bool = false,
    def_type: Type,
    /// Used when the type is not a basic type
    resolved_type: ?TypeUnion = null,

    pub fn isMutable(self: *Self) bool {
        return switch (self.def_type) {
            .List => self.resolved_type.?.List.mutable,
            .Map => self.resolved_type.?.Map.mutable,
            .ObjectInstance => self.resolved_type.?.ObjectInstance.mutable,
            .EnumInstance => self.resolved_type.?.EnumInstance.mutable,
            .ProtocolInstance => self.resolved_type.?.ProtocolInstance.mutable,
            .Placeholder => self.resolved_type.?.Placeholder.mutable orelse false,
            else => false,
        };
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        if (self.resolved_type) |*resolved| {
            if (resolved.* == .ObjectInstance) {
                try gc.markObj(@constCast(resolved.ObjectInstance.of.toObj()));
            } else if (resolved.* == .EnumInstance) {
                try gc.markObj(@constCast(resolved.EnumInstance.of.toObj()));
            } else if (resolved.* == .Object) {
                try resolved.Object.mark(gc);
            } else if (resolved.* == .Protocol) {
                try resolved.Protocol.mark(gc);
            } else if (resolved.* == .Enum) {
                try resolved.Enum.mark(gc);
            } else if (resolved.* == .Function) {
                try resolved.Function.mark(gc);
            } else if (resolved.* == .List) {
                try resolved.List.mark(gc);
            } else if (resolved.* == .Map) {
                try resolved.Map.mark(gc);
            } else if (resolved.* == .Fiber) {
                try resolved.Fiber.mark(gc);
            } else if (resolved.* == .Placeholder) {
                // unreachable;
            } else if (resolved.* == .ForeignContainer) {
                try resolved.ForeignContainer.mark(gc);
            }
        }
    }

    pub fn populateGenerics(
        self: *Self,
        where: Ast.TokenIndex,
        origin: ?usize,
        generics: []*Self,
        type_registry: *TypeRegistry,
        visited: ?*std.AutoHashMap(*Self, void),
    ) !*Self {
        var visited_nodes = if (visited == null)
            std.AutoHashMap(*Self, void).init(type_registry.gc.allocator)
        else
            null;
        defer {
            if (visited == null) {
                visited_nodes.?.deinit();
            }
        }

        var visited_ptr = visited orelse &visited_nodes.?;

        if (visited_ptr.get(self) != null) {
            return self;
        }

        try visited_ptr.put(self, {});

        if (generics.len == 0) {
            return self;
        }

        const result = switch (self.def_type) {
            .Bool,
            .Integer,
            .Double,
            .String,
            .Pattern,
            .Void,
            .UserData,
            .Type,
            .Enum, // Enum are defined in global scope without any generic possible
            .EnumInstance,
            .Protocol,
            .ProtocolInstance,
            .Any,
            .ForeignContainer,
            .Range,
            => self,

            .Placeholder => placeholder: {
                var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                    .Placeholder = PlaceholderDef.init(
                        where,
                        self.resolved_type.?.Placeholder.mutable,
                    ),
                };

                placeholder_resolved_type.Placeholder.resolved_generics = generics;

                const placeholder = try type_registry.getTypeDef(
                    .{
                        .def_type = .Placeholder,
                        .resolved_type = placeholder_resolved_type,
                    },
                );

                try PlaceholderDef.link(
                    type_registry.gc.allocator,
                    self,
                    placeholder,
                    .GenericResolve,
                );

                break :placeholder placeholder;
            },

            .Generic => generic: {
                if (self.resolved_type.?.Generic.origin == origin) {
                    break :generic generics[self.resolved_type.?.Generic.index];
                }

                break :generic self;
            },

            .Fiber => fiber: {
                const new_fiber_def = ObjFiber.FiberDef{
                    .return_type = try self.resolved_type.?.Fiber.return_type.populateGenerics(
                        where,
                        origin,
                        generics,
                        type_registry,
                        visited_ptr,
                    ),
                    .yield_type = try (try self.resolved_type.?.Fiber.yield_type.populateGenerics(
                        where,
                        origin,
                        generics,
                        type_registry,
                        visited_ptr,
                    )).cloneOptional(type_registry),
                };

                const resolved = ObjTypeDef.TypeUnion{ .Fiber = new_fiber_def };

                const new_fiber = ObjTypeDef{
                    .def_type = .Fiber,
                    .optional = self.optional,
                    .resolved_type = resolved,
                };

                break :fiber try type_registry.getTypeDef(new_fiber);
            },
            .ObjectInstance => try (try self.resolved_type.?.ObjectInstance.of.populateGenerics(
                where,
                origin,
                generics,
                type_registry,
                visited_ptr,
            )).toInstance(
                type_registry,
                self.resolved_type.?.ObjectInstance.mutable,
            ),
            .Object => object: {
                // Only anonymous objects can be with generics so no need to check anything other than fields
                const old_object_def = self.resolved_type.?.Object;

                var resolved = ObjObject.ObjectDef.init(
                    old_object_def.location,
                    old_object_def.name,
                    old_object_def.qualified_name,
                    old_object_def.anonymous,
                );

                resolved.generic_types.deinit(type_registry.gc.allocator);
                resolved.generic_types = try old_object_def.generic_types.clone(type_registry.gc.allocator);
                resolved.resolved_generics = generics;

                {
                    var fields = std.StringArrayHashMapUnmanaged(ObjObject.ObjectDef.Field){};
                    var it = old_object_def.fields.iterator();
                    while (it.next()) |kv| {
                        try fields.put(
                            type_registry.gc.allocator,
                            kv.key_ptr.*,
                            .{
                                .name = kv.value_ptr.*.name,
                                .final = kv.value_ptr.*.final,
                                .static = kv.value_ptr.*.static,
                                .location = kv.value_ptr.*.location,
                                .method = kv.value_ptr.*.method,
                                .mutable = kv.value_ptr.*.mutable,
                                .type_def = try kv.value_ptr.*.type_def.populateGenerics(
                                    where,
                                    origin,
                                    generics,
                                    type_registry,
                                    visited_ptr,
                                ),
                                .has_default = kv.value_ptr.*.has_default,
                                .index = kv.value_ptr.*.index,
                            },
                        );
                    }
                    resolved.fields = fields;
                }

                const resolved_type_union = ObjTypeDef.TypeUnion{ .Object = resolved };

                const new_object = ObjTypeDef{
                    .def_type = .Object,
                    .optional = self.optional,
                    .resolved_type = resolved_type_union,
                };

                break :object try type_registry.getTypeDef(new_object);
            },
            .List => list: {
                const old_list_def = self.resolved_type.?.List;

                var methods = std.StringHashMapUnmanaged(ObjList.ListDef.Method){};
                var it = old_list_def.methods.iterator();
                while (it.next()) |kv| {
                    try methods.put(
                        type_registry.gc.allocator,
                        kv.key_ptr.*,
                        .{
                            .type_def = try kv.value_ptr.*.type_def.populateGenerics(
                                where,
                                origin,
                                generics,
                                type_registry,
                                visited_ptr,
                            ),
                            .mutable = kv.value_ptr.*.mutable,
                        },
                    );
                }

                break :list try type_registry.getTypeDef(
                    .{
                        .def_type = .List,
                        .optional = self.optional,
                        .resolved_type = .{
                            .List = .{
                                .mutable = old_list_def.mutable,
                                .item_type = try (try old_list_def.item_type.populateGenerics(
                                    where,
                                    origin,
                                    generics,
                                    type_registry,
                                    visited_ptr,
                                )).toInstance(
                                    type_registry,
                                    old_list_def.item_type.isMutable(),
                                ),
                                .methods = methods,
                            },
                        },
                    },
                );
            },
            .Map => map: {
                const old_map_def = self.resolved_type.?.Map;

                var methods = std.StringHashMapUnmanaged(ObjMap.MapDef.Method){};
                var it = old_map_def.methods.iterator();
                while (it.next()) |kv| {
                    try methods.put(
                        type_registry.gc.allocator,
                        kv.key_ptr.*,
                        .{
                            .type_def = try kv.value_ptr.*.type_def.populateGenerics(
                                where,
                                origin,
                                generics,
                                type_registry,
                                visited_ptr,
                            ),
                            .mutable = kv.value_ptr.*.mutable,
                        },
                    );
                }

                break :map try type_registry.getTypeDef(
                    .{
                        .def_type = .Map,
                        .optional = self.optional,
                        .resolved_type = .{
                            .Map = .{
                                .mutable = old_map_def.mutable,
                                .key_type = try old_map_def.key_type.populateGenerics(
                                    where,
                                    origin,
                                    generics,
                                    type_registry,
                                    visited_ptr,
                                ),
                                .value_type = try old_map_def.value_type.populateGenerics(
                                    where,
                                    origin,
                                    generics,
                                    type_registry,
                                    visited_ptr,
                                ),
                                .methods = methods,
                            },
                        },
                    },
                );
            },
            .Function => function: {
                const old_fun_def = self.resolved_type.?.Function;

                var error_types: ?std.ArrayList(*ObjTypeDef) = null;
                if (old_fun_def.error_types) |old_error_types| {
                    error_types = std.ArrayList(*ObjTypeDef).init(type_registry.gc.allocator);
                    for (old_error_types) |error_type| {
                        try error_types.?.append(try error_type.populateGenerics(
                            where,
                            origin,
                            generics,
                            type_registry,
                            visited_ptr,
                        ));
                    }
                }

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(type_registry.gc.allocator);
                {
                    var it = old_fun_def.parameters.iterator();
                    while (it.next()) |kv| {
                        try parameters.put(
                            kv.key_ptr.*,
                            try (try kv.value_ptr.*.populateGenerics(
                                where,
                                origin,
                                generics,
                                type_registry,
                                visited_ptr,
                            )).toInstance(
                                type_registry,
                                kv.value_ptr.*.isMutable(),
                            ),
                        );
                    }
                }

                const new_fun_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .name = old_fun_def.name,
                    .script_name = old_fun_def.script_name,
                    .return_type = try (try old_fun_def.return_type.populateGenerics(
                        where,
                        origin,
                        generics,
                        type_registry,
                        visited_ptr,
                    ))
                        .toInstance(
                        type_registry,
                        old_fun_def.return_type.isMutable(),
                    ),
                    .yield_type = try (try (try old_fun_def.yield_type.populateGenerics(
                        where,
                        origin,
                        generics,
                        type_registry,
                        visited_ptr,
                    ))
                        .toInstance(
                        type_registry,
                        old_fun_def.yield_type.isMutable(),
                    ))
                        .cloneOptional(type_registry),
                    .error_types = if (error_types) |types| types.items else null,
                    .parameters = parameters,
                    .defaults = old_fun_def.defaults,
                    .function_type = old_fun_def.function_type,
                    .lambda = old_fun_def.lambda,
                    .generic_types = try old_fun_def.generic_types.clone(),
                    .resolved_generics = generics,
                };

                const resolved = ObjTypeDef.TypeUnion{ .Function = new_fun_def };

                const new_fun_type = ObjTypeDef{
                    .def_type = .Function,
                    .optional = self.optional,
                    .resolved_type = resolved,
                };

                break :function try type_registry.getTypeDef(new_fun_type);
            },
        };

        _ = visited_ptr.remove(self);

        return result;
    }

    pub fn rawCloneOptional(self: *Self) ObjTypeDef {
        return .{
            .obj = .{ .obj_type = self.obj.obj_type },
            .optional = true,
            .def_type = self.def_type,
            .resolved_type = self.resolved_type,
        };
    }

    pub fn rawCloneNonOptional(self: *Self) ObjTypeDef {
        return .{
            .obj = .{ .obj_type = self.obj.obj_type },
            .optional = false,
            .def_type = self.def_type,
            .resolved_type = self.resolved_type,
        };
    }

    pub fn cloneOptional(self: *Self, type_registry: *TypeRegistry) !*ObjTypeDef {
        // If already optional return itself
        if ((self.optional or self.def_type == .Any) and self.def_type != .Placeholder) {
            return self;
        }

        const optional = try type_registry.getTypeDef(self.rawCloneOptional());

        if (self.def_type == .Placeholder) {
            // Destroyed copied placeholder link
            optional.resolved_type.?.Placeholder.parent = null;
            optional.resolved_type.?.Placeholder.parent_relation = null;
            optional.resolved_type.?.Placeholder.children = std.ArrayListUnmanaged(*ObjTypeDef){};

            // Make actual link
            try PlaceholderDef.link(
                type_registry.gc.allocator,
                self,
                optional,
                .Optional,
            );
        }

        return optional;
    }

    pub fn cloneNonOptional(self: *Self, type_registry: *TypeRegistry) !*ObjTypeDef {
        // If already non optional return itself
        if (!self.optional and self.def_type != .Placeholder) {
            return self;
        }

        const non_optional = try type_registry.getTypeDef(self.rawCloneNonOptional());

        if (self.def_type == .Placeholder) {
            // Destroyed copied placeholder link
            non_optional.resolved_type.?.Placeholder.parent = null;
            non_optional.resolved_type.?.Placeholder.parent_relation = null;
            non_optional.resolved_type.?.Placeholder.children = std.ArrayListUnmanaged(*ObjTypeDef){};

            // Make actual link
            try PlaceholderDef.link(
                type_registry.gc.allocator,
                self,
                non_optional,
                .Unwrap,
            );
        }

        return non_optional;
    }

    pub fn cloneMutable(self: *Self, type_registry: *TypeRegistry, mutable: bool) !*ObjTypeDef {
        if (self.isMutable() == mutable) {
            return self;
        }

        return switch (self.def_type) {
            .Placeholder => placeholder: {
                var clone = self.*;

                clone.resolved_type.?.Placeholder.mutable = mutable;

                const placeholder = try type_registry.getTypeDef(clone);

                // Link won't be made if parent already exists
                placeholder.resolved_type.?.Placeholder.parent = null;
                try PlaceholderDef.link(
                    type_registry.gc.allocator,
                    clone.resolved_type.?.Placeholder.parent.?,
                    placeholder,
                    clone.resolved_type.?.Placeholder.parent_relation.?,
                );

                // FIXME: this previous clone placeholder becomes useless?

                break :placeholder placeholder;
            },
            .List => list: {
                var clone = self.*;

                clone.resolved_type.?.List.mutable = mutable;

                break :list try type_registry.getTypeDef(clone);
            },
            .Map => map: {
                var clone = self.*;

                clone.resolved_type.?.Map.mutable = mutable;

                break :map try type_registry.getTypeDef(clone);
            },
            .ObjectInstance => instance: {
                var clone = self.*;

                clone.resolved_type.?.ObjectInstance.mutable = mutable;

                break :instance try type_registry.getTypeDef(clone);
            },
            .ProtocolInstance => instance: {
                var clone = self.*;

                clone.resolved_type.?.ProtocolInstance.mutable = mutable;

                break :instance try type_registry.getTypeDef(clone);
            },
            .EnumInstance => instance: {
                var clone = self.*;

                clone.resolved_type.?.EnumInstance.mutable = mutable;

                break :instance try type_registry.getTypeDef(clone);
            },
            // TODO: should have .ForeignContainerInstance type
            // .ForeignContainer => instance: {
            //     var clone = self.*;
            //
            //     clone.resolved_type.?.ForeignContainer.mutable = mutable;
            //
            //     break :instance try type_registry.getTypeDef(clone);
            // },
            else => self,
        };
    }

    pub fn deinit(_: *Self) void {
        // FIXME
    }

    pub fn toStringAlloc(self: *const Self, allocator: Allocator) (Allocator.Error || std.fmt.BufPrintError)![]const u8 {
        var str = std.ArrayList(u8).init(allocator);

        try self.toString(&str.writer());
        str.shrinkAndFree(str.items.len);

        return str.items;
    }

    pub fn toString(self: *const Self, writer: *const std.ArrayList(u8).Writer) (Allocator.Error || std.fmt.BufPrintError)!void {
        try self.toStringRaw(writer, true);
    }

    pub fn toStringUnqualified(self: *const Self, writer: *const std.ArrayList(u8).Writer) (Allocator.Error || std.fmt.BufPrintError)!void {
        try self.toStringRaw(writer, false);
    }

    fn toStringRaw(self: *const Self, writer: *const std.ArrayList(u8).Writer, qualified: bool) (Allocator.Error || std.fmt.BufPrintError)!void {
        switch (self.def_type) {
            .Generic => try writer.print("generic type #{}-{}", .{ self.resolved_type.?.Generic.origin, self.resolved_type.?.Generic.index }),
            .UserData => try writer.writeAll("ud"),
            .Bool => try writer.writeAll("bool"),
            .Integer => try writer.writeAll("int"),
            .Double => try writer.writeAll("double"),
            .String => try writer.writeAll("str"),
            .Pattern => try writer.writeAll("pat"),
            .Any => try writer.writeAll("any"),
            .Range => try writer.writeAll("rg"),
            .Fiber => {
                try writer.writeAll("fib<");
                try self.resolved_type.?.Fiber.return_type.toStringRaw(writer, qualified);
                try writer.writeAll(", ");
                try self.resolved_type.?.Fiber.yield_type.toStringRaw(writer, qualified);
                try writer.writeAll(">");
            },

            // TODO: Find a key for vm.getTypeDef which is unique for each class even with the same name
            .Object => {
                const object_def = self.resolved_type.?.Object;

                if (object_def.anonymous) {
                    try writer.writeAll("obj{ ");
                    var it = object_def.fields.iterator();
                    const count = object_def.fields.count();
                    var i: usize = 0;
                    while (it.next()) |kv| {
                        try writer.print("{s}: ", .{kv.key_ptr.*});
                        try kv.value_ptr.*.type_def.toStringRaw(writer, qualified);

                        if (i < count - 1) {
                            try writer.writeAll(", ");
                        }

                        i += 1;
                    }
                    try writer.writeAll(" }");
                } else {
                    try writer.writeAll("object ");
                    if (qualified) {
                        try writer.writeAll(object_def.qualified_name.string);
                    } else {
                        try writer.writeAll(object_def.name.string);
                    }
                }

                const size = object_def.generic_types.count();
                if (size > 0) {
                    try writer.writeAll("::<");
                    var i: usize = 0;
                    var it = object_def.generic_types.iterator();
                    while (it.next()) |kv| : (i = i + 1) {
                        if (object_def.resolved_generics != null and i < object_def.resolved_generics.?.len) {
                            try object_def.resolved_generics.?[i].toStringRaw(writer, qualified);
                        } else {
                            try writer.print("{s}", .{kv.key_ptr.*.string});
                        }

                        if (i < size - 1) {
                            try writer.writeAll(", ");
                        }
                    }

                    try writer.writeAll(">");
                }
            },
            .Protocol => {
                const protocol_def = self.resolved_type.?.Protocol;

                if (qualified) {
                    try writer.print("protocol {s}", .{protocol_def.qualified_name.string});
                } else {
                    try writer.print("protocol {s}", .{protocol_def.name.string});
                }
            },
            .Enum => {
                try writer.writeAll("enum ");
                if (qualified) {
                    try writer.writeAll(self.resolved_type.?.Enum.qualified_name.string);
                } else {
                    try writer.writeAll(self.resolved_type.?.Enum.name.string);
                }
            },

            .ObjectInstance => {
                const object_def = self.resolved_type.?.ObjectInstance.of.resolved_type.?.Object;

                try writer.print(
                    "{s}",
                    .{
                        if (self.resolved_type.?.ObjectInstance.mutable)
                            "mut "
                        else
                            "",
                    },
                );
                if (object_def.anonymous) {
                    try writer.writeAll(".{ ");
                    var it = object_def.fields.iterator();
                    const count = object_def.fields.count();
                    var i: usize = 0;
                    while (it.next()) |kv| {
                        try writer.print("{s}: ", .{kv.key_ptr.*});
                        try kv.value_ptr.*.type_def.toStringRaw(writer, qualified);

                        if (i < count - 1) {
                            try writer.writeAll(", ");
                        }

                        i += 1;
                    }
                    try writer.writeAll(" }");
                } else {
                    if (qualified) {
                        try writer.writeAll(object_def.qualified_name.string);
                    } else {
                        try writer.writeAll(object_def.name.string);
                    }
                }

                const size = object_def.generic_types.count();
                if (size > 0) {
                    try writer.writeAll("::<");
                    var i: usize = 0;
                    var it = object_def.generic_types.iterator();
                    while (it.next()) |kv| : (i += 1) {
                        if (object_def.resolved_generics != null and i < object_def.resolved_generics.?.len) {
                            try object_def.resolved_generics.?[i].toStringRaw(writer, qualified);
                        } else {
                            try writer.print("{s}", .{kv.key_ptr.*.string});
                        }

                        if (i < size - 1) {
                            try writer.writeAll(", ");
                        }
                    }

                    try writer.writeAll(">");
                }
            },
            .ForeignContainer => {
                if (qualified) {
                    try writer.writeAll(self.resolved_type.?.ForeignContainer.qualified_name.string);
                } else {
                    try writer.writeAll(self.resolved_type.?.ForeignContainer.name.string);
                }
            },
            .ProtocolInstance => {
                try writer.print(
                    "{s}",
                    .{
                        if (self.resolved_type.?.ProtocolInstance.mutable)
                            "mut "
                        else
                            "",
                    },
                );

                const protocol_def = self.resolved_type.?.ProtocolInstance.of.resolved_type.?.Protocol;

                if (qualified) {
                    try writer.writeAll(protocol_def.qualified_name.string);
                } else {
                    try writer.writeAll(protocol_def.name.string);
                }
            },
            .EnumInstance => {
                if (qualified) {
                    try writer.writeAll(self.resolved_type.?.EnumInstance.of.resolved_type.?.Enum.qualified_name.string);
                } else {
                    try writer.writeAll(self.resolved_type.?.EnumInstance.of.resolved_type.?.Enum.name.string);
                }
            },

            .List => {
                try writer.print(
                    "{s}[",
                    .{
                        if (self.resolved_type.?.List.mutable)
                            "mut "
                        else
                            "",
                    },
                );
                try self.resolved_type.?.List.item_type.toStringRaw(writer, qualified);
                try writer.writeAll("]");
            },
            .Map => {
                try writer.print(
                    "{s}",
                    .{
                        if (self.resolved_type.?.Map.mutable)
                            "mut "
                        else
                            "",
                    },
                );
                try writer.writeAll("{");
                try self.resolved_type.?.Map.key_type.toStringRaw(writer, qualified);
                try writer.writeAll(": ");
                try self.resolved_type.?.Map.value_type.toStringRaw(writer, qualified);
                try writer.writeAll("}");
            },
            .Function => {
                var function_def = self.resolved_type.?.Function;

                if (function_def.function_type == .Extern) {
                    try writer.writeAll("extern ");
                }
                try writer.writeAll("fun ");
                try writer.writeAll(function_def.name.string);

                {
                    const size = function_def.generic_types.count();
                    if (size > 0) {
                        try writer.writeAll("::<");
                        var i: usize = 0;
                        var it = function_def.generic_types.iterator();
                        while (it.next()) |kv| : (i = i + 1) {
                            if (function_def.resolved_generics != null and i < function_def.resolved_generics.?.len) {
                                try function_def.resolved_generics.?[i].toString(writer);
                            } else {
                                try writer.print("{s}", .{kv.key_ptr.*.string});
                            }

                            if (i < size - 1) {
                                try writer.writeAll(", ");
                            }
                        }

                        if (function_def.parameters.count() > 0) {
                            try writer.writeAll(">, ");
                        } else {
                            try writer.writeAll(">");
                        }
                    }
                }

                try writer.writeAll("(");

                {
                    const size = function_def.parameters.count();
                    var i: usize = 0;
                    var it = function_def.parameters.iterator();
                    while (it.next()) |kv| : (i = i + 1) {
                        try writer.writeAll(kv.key_ptr.*.string);
                        try writer.writeAll(": ");
                        try kv.value_ptr.*.toStringRaw(writer, qualified);

                        if (i < size - 1) {
                            try writer.writeAll(", ");
                        }
                    }
                }

                try writer.writeAll(")");

                if (function_def.yield_type.def_type != .Void) {
                    try writer.writeAll(" > ");
                    try function_def.yield_type.toStringRaw(writer, qualified);
                }

                try writer.writeAll(" > ");
                try function_def.return_type.toStringRaw(writer, qualified);

                if (function_def.error_types != null and function_def.error_types.?.len > 0) {
                    try writer.writeAll(" !> ");
                    for (function_def.error_types.?, 0..) |error_type, index| {
                        try error_type.toStringRaw(writer, qualified);

                        if (index < function_def.error_types.?.len - 1) {
                            try writer.writeAll(", ");
                        }
                    }
                }
            },
            .Type => try writer.writeAll("type"),
            .Void => try writer.writeAll("void"),

            .Placeholder => {
                const placeholder = self.resolved_type.?.Placeholder;
                try writer.print(
                    "{{{s}Placeholder @{}",
                    .{
                        if (placeholder.mutable != null and placeholder.mutable.?)
                            "mut "
                        else
                            "",
                        @intFromPtr(self),
                    },
                );

                if (placeholder.parent_relation) |relation| {
                    try writer.print(
                        " {s} of ",
                        .{
                            @tagName(relation),
                        },
                    );
                    try placeholder.parent.?.toStringRaw(writer, qualified);
                }

                try writer.print("}}", .{});
            },
        }

        if (self.optional) {
            try writer.writeAll("?");
        }
    }

    pub inline fn toObj(self: *Self) *Obj {
        return @constCast(&self.obj);
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub fn toParentType(self: *Self, type_registry: *TypeRegistry) !*Self {
        return switch (self.def_type) {
            .ObjectInstance => self.resolved_type.?.ObjectInstance.of,
            .ProtocolInstance => self.resolved_type.?.ProtocolInstance.of,
            .EnumInstance => self.resolved_type.?.EnumInstance.of,
            .Placeholder => placeholder: {
                if (self.resolved_type.?.Placeholder.parent_relation != null and self.resolved_type.?.Placeholder.parent_relation.? == .Instance) {
                    return self.resolved_type.?.Placeholder.parent.?;
                }

                const placeholder = try type_registry.getTypeDef(Self{
                    .def_type = .Placeholder,
                    .resolved_type = .{
                        .Placeholder = PlaceholderDef.init(
                            self.resolved_type.?.Placeholder.where,
                            self.resolved_type.?.Placeholder.mutable,
                        ),
                    },
                });

                try PlaceholderDef.link(
                    type_registry.gc.allocator,
                    self,
                    placeholder,
                    .Parent,
                );

                break :placeholder placeholder;
            },
            else => self,
        };
    }

    pub fn toInstance(self: *Self, type_registry: *TypeRegistry, mutable: bool) !*Self {
        // Avoid placeholder double links like: Object Placeholder -> Instance -> Instance
        if (self.def_type == .Placeholder and self.resolved_type.?.Placeholder.parent_relation != null and self.resolved_type.?.Placeholder.parent_relation.? == .Instance) {
            return self;
        }

        const instance_type = try type_registry.getTypeDef(
            switch (self.def_type) {
                .Object => Self{
                    .optional = self.optional,
                    .def_type = .ObjectInstance,
                    .resolved_type = .{
                        .ObjectInstance = .{
                            .of = try self.cloneNonOptional(type_registry),
                            .mutable = mutable,
                        },
                    },
                },
                .Protocol => Self{
                    .optional = self.optional,
                    .def_type = .ProtocolInstance,
                    .resolved_type = .{
                        .ProtocolInstance = .{ .of = try self.cloneNonOptional(type_registry), .mutable = mutable },
                    },
                },
                .Enum => Self{
                    .optional = self.optional,
                    .def_type = .EnumInstance,
                    .resolved_type = .{
                        .EnumInstance = .{
                            .of = try self.cloneNonOptional(type_registry),
                            .mutable = false,
                        },
                    },
                },
                .Placeholder => placeholder: {
                    break :placeholder Self{
                        .def_type = .Placeholder,
                        .resolved_type = .{
                            .Placeholder = PlaceholderDef.init(
                                self.resolved_type.?.Placeholder.where,
                                mutable,
                            ),
                        },
                    };
                },
                .ObjectInstance, .ProtocolInstance, .EnumInstance => instance: {
                    if (BuildOptions.debug) {
                        std.debug.print("toInstance invoked on instance type\n", .{});
                    }

                    break :instance self.*;
                },
                else => self.*,
            },
        );

        if (self.def_type == .Placeholder and instance_type.def_type == .Placeholder) {
            try PlaceholderDef.link(
                type_registry.gc.allocator,
                self,
                instance_type,
                .Instance,
            );
        }

        return instance_type;
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        return obj.cast(Self, .Type);
    }

    // Compare two type definitions
    pub fn eqlTypeUnion(expected: TypeUnion, actual: TypeUnion) bool {
        if (@as(Type, expected) != @as(Type, actual) and (expected != .ProtocolInstance or actual != .ObjectInstance)) {
            return false;
        }

        return switch (expected) {
            .Bool,
            .Integer,
            .Double,
            .String,
            .Void,
            .Pattern,
            .UserData,
            .Type,
            .Range,
            .Any,
            => unreachable,

            .ForeignContainer => std.mem.eql(u8, expected.ForeignContainer.name.string, actual.ForeignContainer.name.string),

            .Generic => expected.Generic.origin == actual.Generic.origin and expected.Generic.index == actual.Generic.index,

            .Fiber => expected.Fiber.return_type.eql(actual.Fiber.return_type) and expected.Fiber.yield_type.eql(actual.Fiber.yield_type),

            .ObjectInstance => expected.ObjectInstance.of.eql(actual.ObjectInstance.of) or expected.ObjectInstance.of == actual.ObjectInstance.of,
            .ProtocolInstance => {
                if (actual == .ProtocolInstance) {
                    return expected.ProtocolInstance.of.eql(actual.ProtocolInstance.of) or expected.ProtocolInstance.of == actual.ProtocolInstance.of;
                } else {
                    assert(actual == .ObjectInstance);
                    return actual.ObjectInstance.of.resolved_type.?.Object.conforms_to.get(expected.ProtocolInstance.of) != null;
                }
            },
            .EnumInstance => expected.EnumInstance.of.eql(actual.EnumInstance.of),

            .Object => {
                // If both are anonymous object type, we can deeply compare them
                if (!expected.Object.anonymous or
                    !actual.Object.anonymous or
                    expected.Object.fields.count() != actual.Object.fields.count())
                {
                    return false;
                }

                var it = expected.Object.fields.iterator();
                while (it.next()) |entry| {
                    if (actual.Object.fields.get(entry.key_ptr.*)) |other_field| {
                        if (!entry.value_ptr.*.eql(other_field)) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }

                return true;
            },

            .Protocol, .Enum => false, // Those are never equal even if definition is the same

            .List => expected.List.item_type.eql(actual.List.item_type),
            .Map => expected.Map.key_type.eql(actual.Map.key_type) and expected.Map.value_type.eql(actual.Map.value_type),
            .Function => {
                // Compare return type
                if (!expected.Function.return_type.eql(actual.Function.return_type)) {
                    return false;
                }

                // Compare yield type
                if (!expected.Function.yield_type.eql(actual.Function.yield_type)) {
                    return false;
                }

                // Compare arity
                if (expected.Function.parameters.count() != actual.Function.parameters.count()) {
                    return false;
                }

                // Compare parameters (we ignore argument names and only compare types)
                const a_keys: []*ObjString = expected.Function.parameters.keys();
                const b_keys: []*ObjString = actual.Function.parameters.keys();

                if (a_keys.len != b_keys.len) {
                    return false;
                }

                for (a_keys, 0..) |_, index| {
                    if (!expected.Function.parameters.get(a_keys[index]).?
                        .eql(actual.Function.parameters.get(b_keys[index]).?))
                    {
                        return false;
                    }
                }

                return true;
            },

            .Placeholder => true, // TODO: should it be false?
        };
    }

    // Compare two type definitions
    pub fn eql(expected: *Self, actual: *Self) bool {
        if (expected == actual or
            (expected.optional and actual.def_type == .Void) or
            (actual.optional and expected.def_type == .Void) or
            expected.def_type == .Any)
        {
            return true;
        }

        const type_eql = (expected.resolved_type == null and actual.resolved_type == null and expected.def_type == actual.def_type) or
            (expected.def_type == .UserData and actual.def_type == .ForeignContainer) or // FIXME: we should not need this anymore
            (expected.def_type == .Type and actual.def_type == .ForeignContainer) or
            (expected.resolved_type != null and actual.resolved_type != null and eqlTypeUnion(expected.resolved_type.?, actual.resolved_type.?));

        return (type_eql or actual.def_type == .Placeholder or expected.def_type == .Placeholder) and
            (expected.optional or !actual.optional) and // A nullable slot can receive a non-null value
            (!expected.isMutable() or actual.isMutable()); // A immutable slot can receive a mutable value (the immutability is the promise that the receiver won't mutate the value)
    }

    // Strict compare two type definitions
    pub fn strictEql(expected: *Self, actual: *Self) bool {
        const type_eql = (expected.resolved_type == null and actual.resolved_type == null and expected.def_type == actual.def_type) or
            (expected.resolved_type != null and actual.resolved_type != null and eqlTypeUnion(
            expected.resolved_type.?,
            actual.resolved_type.?,
        ));

        // TODO: in an ideal world comparing pointers should be enough, but typedef can come from different type_registries and we can't reconcile them like we can with strings
        // FIXME: previous comment should be wrong now? we do share type_registries between fibers and this should be enough ?
        return expected == actual or
            (expected.optional and actual.def_type == .Void) or // Void is equal to any optional type
            ((type_eql or actual.def_type == .Placeholder or expected.def_type == .Placeholder) and (expected.optional or !actual.optional));
    }

    pub fn isConstant(self: *Self) bool {
        return switch (self.def_type) {
            .Bool,
            .Double,
            .Integer,
            .Pattern,
            .String,
            .Type,
            .UserData,
            .Void,
            .Range,
            .Enum,
            .EnumInstance,
            .Function,
            .Generic,
            .Object,
            .Placeholder,
            .Protocol,
            => true,

            .Any, .Protocol => false,

            .ObjectInstance => inst: {
                // All properties must be const and of const type
                const object_def = self.resolved_type.?.ObjectInstance.resolved_type.?.Object;

                var it = object_def.fields.iterator();
                while (it.next()) |kv| {
                    if ((!kv.value_ptr.?.constant and !kv.value_ptr.*.static) or !kv.value_ptr.*.type_def.isConstant()) {
                        break :inst false;
                    }
                }

                break :inst true;
            },
        };
    }
};

pub fn cloneObject(obj: *Obj, vm: *VM) !Value {
    switch (obj.obj_type) {
        .String,
        .Type,
        .UpValue,
        .Closure,
        .Function,
        .Object,
        .Enum,
        .EnumInstance,
        .Bound,
        .Native,
        .UserData,
        .Pattern,
        .Fiber,
        .ForeignContainer,
        .Range,
        => return Value.fromObj(obj),

        .List => {
            const list = ObjList.cast(obj).?;

            return (try vm.gc.allocateObject(
                ObjList,
                .{
                    .type_def = list.type_def,
                    .items = try list.items.clone(vm.gc.allocator),
                    .methods = list.methods,
                },
            )).toValue();
        },

        .Map => {
            const map = ObjMap.cast(obj).?;

            return (try vm.gc.allocateObject(
                ObjMap,
                .{
                    .type_def = map.type_def,
                    .map = try map.map.clone(vm.gc.allocator),
                    .methods = map.methods,
                },
            )).toValue();
        },

        // TODO
        .ObjectInstance => unreachable,
    }
}

pub const PlaceholderDef = struct {
    const Self = @This();

    pub const PlaceholderRelation = enum {
        Call,
        Yield,
        Subscript,
        UnwrappedSubscript,
        Key,
        FieldAccess,
        Assignment,
        Instance,
        Parent,
        Optional,
        Unwrap,
        GenericResolve,
    };

    where: Ast.TokenIndex, // Where the placeholder was created
    // When accessing/calling/subscrit/assign a placeholder we produce another. We keep them linked so we
    // can trace back the root of the unknown type.
    parent: ?*ObjTypeDef = null,
    // What's the relation with the parent?
    parent_relation: ?PlaceholderRelation = null,
    // Children adds themselves here
    children: std.ArrayListUnmanaged(*ObjTypeDef),
    mutable: ?bool,

    // If the placeholder is a function return, we need to remember eventual generic types defined in that call
    resolved_generics: ?[]*ObjTypeDef = null,

    pub fn init(where: Ast.TokenIndex, mutable: ?bool) Self {
        return Self{
            .where = where,
            .children = std.ArrayListUnmanaged(*ObjTypeDef){},
            .mutable = mutable,
        };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.children.deinit(allocator);
    }

    pub fn link(allocator: Allocator, parent: *ObjTypeDef, child: *ObjTypeDef, relation: PlaceholderRelation) !void {
        assert(parent.def_type == .Placeholder);
        assert(child.def_type == .Placeholder);

        if (parent == child) {
            return;
        }

        if (child.resolved_type.?.Placeholder.parent != null) {
            if (BuildOptions.debug_placeholders) {
                io.print(
                    ">>> Placeholder @{} has already a {} relation with @{}\n",
                    .{
                        @intFromPtr(child),
                        child.resolved_type.?.Placeholder.parent_relation.?,
                        @intFromPtr(child.resolved_type.?.Placeholder.parent.?),
                    },
                );
            }
            return;
        }

        child.resolved_type.?.Placeholder.parent = parent;
        try parent.resolved_type.?.Placeholder.children.append(allocator, child);
        child.resolved_type.?.Placeholder.parent_relation = relation;

        if (BuildOptions.debug_placeholders) {
            io.print(
                "Linking @{} (root: {}) with @{} as {s}\n",
                .{
                    @intFromPtr(parent),
                    parent.resolved_type.?.Placeholder.parent == null,
                    @intFromPtr(child),
                    @tagName(relation),
                },
            );
        }
    }
};
