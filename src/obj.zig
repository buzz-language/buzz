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
const GC = @import("GC.zig");
const TypeRegistry = @import("TypeRegistry.zig");
const v = @import("value.zig");
const Integer = v.Integer;
const Value = v.Value;
const Token = @import("Token.zig");
const is_wasm = builtin.cpu.arch.isWasm();
const BuildOptions = @import("build_options");
const CodeGen = @import("Codegen.zig");
const buzz_api = @import("buzz_api.zig");
const buzz_builtin = @import("builtin.zig");
const ZigType = @import("zigtypes.zig").Type;
const Ast = @import("Ast.zig");
const io = @import("io.zig");
const Pool = @import("pool.zig").Pool;

pub const pcre = if (!is_wasm) @import("pcre.zig") else void;

pub const SerializeError = error{
    CircularReference,
    NotSerializable,
    OutOfMemory,
};

pub const ObjType = enum(u5) {
    String = 0,
    Type = 1,
    UpValue = 2,
    Closure = 3,
    Function = 4,
    ObjectInstance = 5,
    Object = 6,
    List = 7,
    Map = 8,
    Enum = 9,
    EnumInstance = 10,
    Bound = 11,
    Native = 12,
    UserData = 13,
    Pattern = 14,
    Fiber = 15,
    ForeignContainer = 16,
    Range = 17,

    pub fn @"type"(self: @This()) type {
        return switch (self.obj_type) {
            .Range => ObjRange,
            .String => ObjString,
            .Pattern => ObjPattern,
            .Fiber => ObjFiber,
            .Type => ObjType,
            .Object => ObjObject,
            .Enum => ObjEnum,
            .ObjectInstance => ObjObjectInstance,
            .EnumInstance => ObjEnumInstance,
            .Function => ObjFunction,
            .UpValue => ObjUpValue,
            .Closure => ObjClosure,
            .List => ObjList,
            .Map => ObjMap,
            .Bound => ObjBoundMethod,
            .ForeignContainer => ObjForeignContainer,
            .UserData => ObjUserData,
            .Native => ObjNative,
        };
    }
};

pub const ObjIdx = struct {
    obj_type: ObjType,
    index: u43,

    pub fn typeOf(self: @This(), gc: *GC) error{ OutOfMemory, NoSpaceLeft, ReachedMaximumMemoryUsage }!Pool(ObjTypeDef).Idx {
        return switch (self.obj_type) {
            .Range => gc.type_registry.rg_type,
            .String => gc.type_registry.str_type,
            .Pattern => gc.type_registry.pat_type,
            .Fiber => gc.ptr(ObjFiber, .idx(self.index)).?.fiber.type_def,
            .Type => try gc.type_registry.getTypeDef(.{ .def_type = .Type }),
            .Object => gc.ptr(ObjObject, .idx(self.index)).?.type_def,
            .Enum => gc.ptr(ObjEnum, .idx(self.index)).?.type_def,
            .ObjectInstance => gc.ptr(ObjObjectInstance, .idx(self.index)).?.type_def,
            .EnumInstance => try ObjTypeDef.toInstance(
                gc.ptr(ObjEnumInstance, .idx(self.index)).?
                    .enum_ref.get(gc).type_def,
                &gc.type_registry,
                false,
            ),
            .Function => gc.ptr(ObjFunction, .idx(self.index)).?.type_def,
            .UpValue => upvalue: {
                const upvalue = gc.ptr(ObjUpValue, .idx(self.index)).?;

                break :upvalue (upvalue.closed orelse upvalue.location.*).typeOf(gc);
            },
            .Closure => gc.ptr(ObjClosure, .idx(self.index)).?.function.get(gc).type_def,
            .List => gc.ptr(ObjList, .idx(self.index)).?.type_def,
            .Map => gc.ptr(ObjMap, .idx(self.index)).?.type_def,
            .Bound => bound: {
                const bound = gc.ptr(ObjBoundMethod, .idx(self.index)).?;
                break :bound try (if (bound.closure) |cls|
                    Value.fromObj(
                        .{
                            .index = cls.get(gc).function.index,
                            .obj_type = .Function,
                        },
                    )
                else
                    Value.fromObj(
                        .{
                            .index = bound.native.?.index,
                            .obj_type = .Native,
                        },
                    ))
                    .typeOf(gc);
            },
            .ForeignContainer => gc.ptr(ObjForeignContainer, .idx(self.index)).?.type_def,
            .UserData => gc.type_registry.ud_type,
            // FIXME: apart from list/map types we actually can embark typedef of objnatives at runtime
            // Or since native are ptr to unique function we can keep a map of ptr => typedef
            .Native => try gc.type_registry.getTypeDef(
                .{
                    .def_type = .Function,
                    .resolved_type = .{
                        .Function = .{
                            .id = 0,
                            .name = try gc.copyString("native"),
                            .script_name = try gc.copyString("native"),
                            .return_type = gc.type_registry.any_type,
                            .yield_type = gc.type_registry.any_type,
                        },
                    },
                },
            ),
        };
    }

    pub fn clone(self: @This(), gc: *GC) !Value {
        switch (self.obj_type) {
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
            => return Value.fromObj(self),

            .List => {
                const list = gc.ptr(ObjList, .idx(self.index)).?;

                return Value.fromObj(
                    .{
                        .index = (try gc.allocateObject(
                            ObjList{
                                .type_def = list.type_def,
                                .items = try list.items.clone(gc.allocator),
                                .methods = list.methods,
                            },
                        )).index,
                        .obj_type = .List,
                    },
                );
            },

            .Map => {
                const map = gc.ptr(ObjMap, .idx(self.index)).?;

                return Value.fromObj(
                    .{
                        .index = (try gc.allocateObject(
                            ObjMap{
                                .type_def = map.type_def,
                                .map = try map.map.clone(gc.allocator),
                                .methods = map.methods,
                            },
                        )).index,
                        .obj_type = .Map,
                    },
                );
            },

            // TODO
            .ObjectInstance => unreachable,
        }
    }

    pub fn eql(self_idx: ObjIdx, other_idx: ObjIdx, gc: *GC) bool {
        if (self_idx.obj_type != other_idx.obj_type) {
            return false;
        }

        switch (self_idx.obj_type) {
            .Pattern => {
                return mem.eql(
                    u8,
                    gc.ptr(ObjPattern, .idx(self_idx.index)).?.source,
                    gc.ptr(ObjPattern, .idx(other_idx.index)).?.source,
                );
            },
            .String => {
                if (BuildOptions.debug) {
                    assert(
                        self_idx.index != other_idx.index or
                            mem.eql(
                                u8,
                                gc.ptr(ObjString, .idx(self_idx.index)).?.string,
                                gc.ptr(ObjString, .idx(other_idx.index)).?.string,
                            ),
                    );
                    assert(
                        self_idx.index == other_idx.index or
                            !mem.eql(
                                u8,
                                gc.ptr(ObjString, .idx(self_idx.index)).?.string,
                                gc.ptr(ObjString, .idx(other_idx.index)).?.string,
                            ),
                    );
                }

                // since string are interned this should be enough
                return self_idx.index == other_idx.index;
            },
            .Type => {
                const self_type = gc.ptr(ObjTypeDef, .idx(self_idx.index)).?;
                const other_type = gc.ptr(ObjTypeDef, .idx(other_idx.index)).?;

                return self_type.optional == other_type.optional and
                    ObjTypeDef.eql(
                        .idx(self_idx.index),
                        .idx(other_idx.index),
                        gc,
                    );
            },
            .UpValue => {
                const self_upvalue = gc.ptr(ObjUpValue, .idx(self_idx.index)).?;
                const other_upvalue = gc.ptr(ObjUpValue, .idx(other_idx.index)).?;

                return (self_upvalue.closed orelse self_upvalue.location.*).eql(
                    other_upvalue.closed orelse other_upvalue.location.*,
                    gc,
                );
            },
            .EnumInstance => {
                const self_enum_instance = gc.ptr(ObjEnumInstance, .idx(self_idx.index)).?;
                const other_enum_instance = gc.ptr(ObjEnumInstance, .idx(other_idx.index)).?;

                return self_enum_instance.enum_ref.index == other_enum_instance.enum_ref.index and
                    self_enum_instance.case == other_enum_instance.case;
            },
            .UserData => {
                const self_userdata = gc.ptr(ObjUserData, .idx(self_idx.index)).?;
                const other_userdata = gc.ptr(ObjUserData, .idx(other_idx.index)).?;

                return self_userdata.userdata == other_userdata.userdata;
            },
            .Fiber => {
                const self_fiber = gc.ptr(ObjFiber, .idx(self_idx.index)).?;
                const other_fiber = gc.ptr(ObjFiber, .idx(other_idx.index)).?;

                return self_fiber.fiber == other_fiber.fiber;
            },
            .Range => {
                const self_range = gc.ptr(ObjRange, .idx(self_idx.index)).?;
                const other_range = gc.ptr(ObjRange, .idx(other_idx.index)).?;

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
                return self_idx.index == other_idx.index;
            },
        }
    }

    pub fn is(self: ObjIdx, type_def_idx: Pool(ObjTypeDef).Idx, gc: *GC) bool {
        const type_def = type_def_idx.get(gc);

        if (type_def.def_type == .Any) {
            return true;
        }

        return switch (self.obj_type) {
            .Range => type_def.def_type == .Range,
            .String => type_def.def_type == .String,
            .Pattern => type_def.def_type == .Pattern,
            .Fiber => ObjFiber.is(.idx(self.index), type_def_idx, gc),

            .Type, .Object, .Enum => type_def.def_type == .Type,

            .ObjectInstance => (type_def.def_type == .ObjectInstance or type_def.def_type == .Object or type_def.def_type == .Protocol or type_def.def_type == .ProtocolInstance) and
                ObjObjectInstance.is(.idx(self.index), type_def_idx, gc),
            .EnumInstance => (type_def.def_type == .Enum and
                gc.ptr(ObjEnumInstance, .idx(self.index)).?.enum_ref.get(gc).type_def.index == type_def_idx.index) or
                (type_def.def_type == .EnumInstance and
                    gc.ptr(ObjEnumInstance, .idx(self.index)).?.enum_ref.get(gc).type_def.index == type_def.resolved_type.?.EnumInstance.of.index),
            .Function => ObjTypeDef.eql(
                gc.ptr(ObjFunction, .idx(self.index)).?.type_def,
                type_def_idx,
                gc,
            ),

            .UpValue => upvalue: {
                const upvalue = gc.ptr(ObjUpValue, .idx(self.index)).?;
                break :upvalue Value.fromObj(.{ .index = type_def_idx.index, .obj_type = .Type }).is(
                    upvalue.closed orelse upvalue.location.*,
                    gc,
                );
            },
            .Closure => cls: {
                const function_idx = gc.ptr(ObjClosure, .idx(self.index)).?.function;
                break :cls (ObjIdx{
                    .index = function_idx.index,
                    .obj_type = .Closure,
                }).is(type_def_idx, gc);
            },
            .List => ObjTypeDef.eql(
                gc.ptr(ObjList, .idx(self.index)).?.type_def,
                type_def_idx,
                gc,
            ),
            .Map => ObjTypeDef.eql(
                gc.ptr(ObjMap, .idx(self.index)).?.type_def,
                type_def_idx,
                gc,
            ),
            .Bound => bound: {
                const bound = gc.ptr(ObjBoundMethod, .idx(self.index)).?;
                break :bound Value.fromObj(.{ .index = type_def_idx.index, .obj_type = .Type }).is(
                    .fromObj(
                        .{
                            .index = if (bound.closure) |cls|
                                cls.get(gc).function.index
                            else
                                bound.native.?.index,
                            .obj_type = if (bound.closure != null) .Closure else .Native,
                        },
                    ),
                    gc,
                );
            },
            .ForeignContainer => type_def.def_type == .ForeignContainer and ObjForeignContainer.is(.idx(self.index), type_def_idx, gc),
            .UserData => type_def.def_type == .UserData,
            .Native => unreachable, // TODO: we don't know how to embark NativeFn type at runtime yet
        };
    }

    pub fn toString(obj: ObjIdx, gc: *GC, writer: *std.Io.Writer) (Allocator.Error || std.fmt.BufPrintError || error{WriteFailed})!void {
        return switch (obj.obj_type) {
            .String => {
                const str = gc.ptr(ObjString, .idx(obj.index)).?.string;

                try writer.print("{s}", .{str});
            },
            .Pattern => {
                const pattern = gc.ptr(ObjPattern, .idx(obj.index)).?.source;

                try writer.print("{s}", .{pattern});
            },
            .Fiber => {
                const fiber = gc.ptr(ObjFiber, .idx(obj.index)).?.fiber;

                try writer.print("fiber: 0x{x}", .{@intFromPtr(fiber)});
            },
            .Type => {
                const type_def = Pool(ObjTypeDef).Idx.idx(obj.index);

                try writer.print("type: 0x{x} `", .{type_def.index});

                try ObjTypeDef.toString(type_def, gc, writer, true);

                try writer.writeAll("`");
            },
            .UpValue => {
                const upvalue = gc.ptr(ObjUpValue, .idx(obj.index)).?;

                try (upvalue.closed orelse upvalue.location.*).toString(gc, writer);
            },
            .Closure => try writer.print(
                "closure: 0x{x} `{s}`",
                .{
                    obj.index,
                    gc.ptr(ObjClosure, .idx(obj.index)).?
                        .function.get(gc)
                        .type_def.get(gc)
                        .resolved_type.?.Function.name
                        .get(gc).string,
                },
            ),
            .Function => try writer.print(
                "function: 0x{x} `{s}`",
                .{
                    obj.index,
                    gc.ptr(ObjFunction, .idx(obj.index)).?
                        .type_def.get(gc)
                        .resolved_type.?.Function.name.get(gc)
                        .string,
                },
            ),
            .ObjectInstance => {
                const instance = gc.ptr(ObjObjectInstance, .idx(obj.index)).?;

                if (instance.object) |object| {
                    try writer.print(
                        "object instance: 0x{x} {s}`{s}`",
                        .{
                            obj.index,
                            object.get(gc)
                                .type_def.get(gc)
                                .resolved_type.?.Object.name.get(gc)
                                .string,
                            if (instance.type_def.get(gc).resolved_type.?.ObjectInstance.mutable)
                                "mut "
                            else
                                "",
                        },
                    );
                } else {
                    try writer.print(
                        "object instance: 0x{x} {s}obj{{ ",
                        .{
                            obj.index,
                            if (instance.type_def.get(gc).resolved_type.?.ObjectInstance.mutable)
                                "mut "
                            else
                                "",
                        },
                    );

                    const object_def = instance.type_def.get(gc)
                        .resolved_type.?
                        .ObjectInstance
                        .of.get(gc)
                        .resolved_type.?
                        .Object;

                    for (0..instance.fields.len) |i| {
                        const field_name = object_def.fields.keys()[i];

                        try ObjTypeDef.toString(
                            object_def.fields.get(field_name).?.type_def,
                            gc,
                            writer,
                            true,
                        );

                        try writer.print(" {s}, ", .{field_name});
                    }
                    try writer.writeAll("}");
                }
            },
            .Object => try writer.print(
                "object: 0x{x} `{s}`",
                .{
                    obj.index,
                    gc.ptr(ObjObject, .idx(obj.index)).?
                        .type_def.get(gc)
                        .resolved_type.?.Object.name.get(gc).string,
                },
            ),
            .Range => {
                const range = gc.ptr(ObjRange, .idx(obj.index)).?;

                try writer.print(
                    "range: 0x{x} {}..{}",
                    .{
                        obj.index,
                        range.low,
                        range.high,
                    },
                );
            },
            .List => {
                const list = gc.ptr(ObjList, .idx(obj.index)).?;
                const type_def = list.type_def.get(gc);

                try writer.print(
                    "list: 0x{x} {s}[",
                    .{
                        obj.index,
                        if (type_def.resolved_type.?.List.mutable)
                            "mut "
                        else
                            "",
                    },
                );

                try ObjTypeDef.toString(
                    type_def.resolved_type.?.List.item_type,
                    gc,
                    writer,
                    true,
                );

                try writer.writeAll("]");
            },
            .Map => {
                const map = gc.ptr(ObjMap, .idx(obj.index)).?;
                const type_def = map.type_def.get(gc);

                try writer.print(
                    "map: 0x{x} {s}{{",
                    .{
                        obj.index,
                        if (type_def.resolved_type.?.Map.mutable)
                            "mut "
                        else
                            "",
                    },
                );

                try ObjTypeDef.toString(
                    type_def.resolved_type.?.Map.key_type,
                    gc,
                    writer,
                    true,
                );

                try writer.writeAll(", ");

                try ObjTypeDef.toString(
                    type_def.resolved_type.?.Map.value_type,
                    gc,
                    writer,
                    true,
                );

                try writer.writeAll("}");
            },
            .Enum => try writer.print(
                "enum: 0x{x} `{s}`",
                .{
                    obj.index,
                    gc.ptr(ObjEnum, .idx(obj.index)).?
                        .type_def.get(gc)
                        .resolved_type.?.Enum.name.get(gc).string,
                },
            ),
            .EnumInstance => enum_instance: {
                const instance = gc.ptr(ObjEnumInstance, .idx(obj.index)).?;
                const enum_ = instance.enum_ref.get(gc);
                const type_def = enum_.type_def.get(gc);

                break :enum_instance try writer.print(
                    "{s}.{s}",
                    .{
                        type_def.resolved_type.?.Enum.name.get(gc).string,
                        type_def.resolved_type.?.Enum.cases[instance.case],
                    },
                );
            },
            .Bound => {
                const bound = gc.ptr(ObjBoundMethod, .idx(obj.index)).?;

                if (bound.closure) |closure| {
                    const closure_name = closure.get(gc)
                        .function.get(gc)
                        .type_def.get(gc)
                        .resolved_type.?.Function.name.get(gc).string;

                    try writer.print("bound method: ", .{});

                    try bound.receiver.toString(gc, writer);

                    try writer.print(
                        " to {s}",
                        .{
                            closure_name,
                        },
                    );
                } else {
                    assert(bound.native != null);
                    try writer.print("bound method: ", .{});

                    try bound.receiver.toString(gc, writer);

                    try writer.print(
                        " to 0x{}",
                        .{
                            bound.native.?.index,
                        },
                    );
                }
            },
            .Native => try writer.print("native: 0x{x}", .{obj.index}),
            .UserData => {
                const userdata = gc.ptr(ObjUserData, .idx(obj.index)).?;

                try writer.print("userdata: 0x{x}", .{userdata.userdata});
            },
            .ForeignContainer => {
                const foreign = gc.ptr(ObjForeignContainer, .idx(obj.index)).?;

                try writer.print(
                    "foreign struct: 0x{x} `{s}`",
                    .{
                        @intFromPtr(foreign.data.ptr),
                        foreign.type_def.get(gc)
                            .resolved_type.?.ForeignContainer.name.get(gc).string,
                    },
                );
            },
        };
    }

    pub fn serialize(self: ObjIdx, vm: *VM, seen: *std.AutoHashMapUnmanaged(ObjIdx, void)) SerializeError!Value {
        if (seen.get(self) != null) {
            return error.CircularReference;
        }

        try seen.put(vm.gc.allocator, self, {});

        switch (self.obj_type) {
            .String => return Value.fromObj(self),

            .Pattern => {
                const pattern = vm.gc.ptr(ObjPattern, .idx(self.index)).?;

                return .fromObj(
                    .{
                        .index = (vm.gc.copyString(pattern.source) catch return error.OutOfMemory).index,
                        .obj_type = .String,
                    },
                );
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
                const upvalue = vm.gc.ptr(ObjUpValue, .idx(self.index)).?;

                return try (upvalue.closed orelse upvalue.location.*).serialize(vm, seen);
            },

            // We could also serialize the actual structure of a typedef
            // But do we need to? At deserialization we can just parse the type?
            .Type => {
                const type_str = ObjTypeDef.toStringAlloc(
                    .idx(self.index),
                    vm.gc,
                    true,
                ) catch return error.OutOfMemory;
                defer vm.gc.allocator.free(type_str);

                return .fromObj(
                    .{
                        .index = (vm.gc.copyString(type_str) catch return error.OutOfMemory).index,
                        .obj_type = .String,
                    },
                );
            },

            .EnumInstance => {
                const enum_instance = vm.gc.ptr(ObjEnumInstance, .idx(self.index)).?;
                const enum_ = enum_instance.enum_ref.get(vm.gc);

                return try enum_.cases[enum_instance.case].serialize(vm, seen);
            },

            .Range => {
                const range = vm.gc.ptr(ObjRange, .idx(self.index)).?;

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

                const serialized_range_idx = vm.gc.allocateObject(
                    ObjMap.init(
                        vm.gc.allocator,
                        map_type,
                    ) catch return error.OutOfMemory,
                ) catch return error.OutOfMemory;
                const serialized_range = serialized_range_idx.get(vm.gc);

                serialized_range.map.put(
                    vm.gc.allocator,
                    .fromObj(
                        .{
                            .index = (vm.gc.copyString("low") catch return error.OutOfMemory).index,
                            .obj_type = .String,
                        },
                    ),
                    Value.fromInteger(range.low),
                ) catch return error.OutOfMemory;

                serialized_range.map.put(
                    vm.gc.allocator,
                    .fromObj(
                        .{
                            .index = (vm.gc.copyString("high") catch return error.OutOfMemory).index,
                            .obj_type = .String,
                        },
                    ),
                    Value.fromInteger(range.high),
                ) catch return error.OutOfMemory;

                return .fromObj(
                    .{
                        .index = serialized_range_idx.index,
                        .obj_type = .Map,
                    },
                );
            },

            .List => {
                const list = vm.gc.ptr(ObjList, .idx(self.index)).?;

                const list_type = vm.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .List,
                        .resolved_type = .{
                            .List = ObjList.ListDef.init(
                                vm.gc.type_registry.any_type,
                                list.type_def.get(vm.gc)
                                    .resolved_type.?.List.mutable,
                            ),
                        },
                    },
                ) catch return error.OutOfMemory;

                const serialized_list_idx = vm.gc.allocateObject(
                    ObjList.init(
                        vm.gc.allocator,
                        list_type,
                    ) catch return error.OutOfMemory,
                ) catch return error.OutOfMemory;

                for (list.items.items) |item| {
                    try ObjList.rawAppend(
                        serialized_list_idx,
                        vm.gc,
                        try item.serialize(vm, seen),
                    );
                }

                return .fromObj(
                    .{
                        .index = serialized_list_idx.index,
                        .obj_type = .List,
                    },
                );
            },

            .Map => {
                const map = vm.gc.ptr(ObjMap, .idx(self.index)).?;

                const map_type = vm.gc.type_registry.getTypeDef(
                    .{
                        .optional = false,
                        .def_type = .Map,
                        .resolved_type = .{
                            .Map = .init(
                                vm.gc.type_registry.any_type,
                                vm.gc.type_registry.any_type,
                                map.type_def.get(vm.gc)
                                    .resolved_type.?.Map.mutable,
                            ),
                        },
                    },
                ) catch return error.OutOfMemory;

                const serialized_map = vm.gc.allocateObject(
                    ObjMap.init(
                        vm.gc.allocator,
                        map_type,
                    ) catch return error.OutOfMemory,
                ) catch return error.OutOfMemory;

                for (map.map.keys()) |key| {
                    try ObjMap.set(
                        serialized_map,
                        vm.gc,
                        try key.serialize(vm, seen),
                        try map.map.get(key).?.serialize(vm, seen),
                    );
                }

                return .fromObj(
                    .{
                        .index = serialized_map.index,
                        .obj_type = .Map,
                    },
                );
            },

            .ObjectInstance => {
                const instance = vm.gc.ptr(ObjObjectInstance, .idx(self.index)).?;
                const object_def = instance.type_def.get(vm.gc)
                    .resolved_type.?.ObjectInstance.of.get(vm.gc)
                    .resolved_type.?.Object;

                const map_type = vm.gc.type_registry.getTypeDef(
                    .{
                        .optional = false,
                        .def_type = .Map,
                        .resolved_type = .{
                            .Map = ObjMap.MapDef.init(
                                vm.gc.type_registry.any_type,
                                vm.gc.type_registry.any_type,
                                instance.type_def.get(vm.gc)
                                    .resolved_type.?.ObjectInstance.mutable,
                            ),
                        },
                    },
                ) catch return error.OutOfMemory;

                const serialized_instance = vm.gc.allocateObject(
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
                        ObjMap.set(
                            serialized_instance,
                            vm.gc,
                            .fromObj(
                                .{
                                    .index = property_str.index,
                                    .obj_type = .String,
                                },
                            ),
                            try instance.fields[field.index].serialize(vm, seen),
                        ) catch return error.OutOfMemory;
                    }
                }

                return .fromObj(
                    .{
                        .index = serialized_instance.index,
                        .obj_type = .Map,
                    },
                );
            },

            .ForeignContainer => {
                const container = vm.gc.ptr(ObjForeignContainer, .idx(self.index)).?;
                const container_def = container.type_def.get(vm.gc)
                    .resolved_type.?.ForeignContainer;

                const map_type = vm.gc.type_registry.getTypeDef(
                    .{
                        .optional = false,
                        .def_type = .Map,
                        .resolved_type = .{
                            .Map = ObjMap.MapDef.init(
                                vm.gc.type_registry.any_type,
                                vm.gc.type_registry.any_type,
                                true,
                            ),
                        },
                    },
                ) catch return error.OutOfMemory;

                const serialized_instance = vm.gc.allocateObject(
                    ObjMap.init(
                        vm.gc.allocator,
                        map_type,
                    ) catch return error.OutOfMemory,
                ) catch return error.OutOfMemory;

                var it = container_def.fields.iterator();
                while (it.next()) |kv| {
                    const dupped = try vm.gc.allocator.dupeZ(u8, kv.key_ptr.*);
                    defer vm.gc.allocator.free(dupped);

                    try ObjMap.set(
                        serialized_instance,
                        vm.gc,
                        .fromObj(
                            .{
                                .index = (vm.gc.copyString(kv.key_ptr.*) catch return error.OutOfMemory).index,
                                .obj_type = .String,
                            },
                        ),
                        kv.value_ptr.*.getter(
                            vm,
                            @ptrCast(dupped),
                        ),
                    );
                }

                return .fromObj(
                    .{
                        .index = serialized_instance.index,
                        .obj_type = .Map,
                    },
                );
            },
        }
    }
};

pub const Obj = struct {
    const Self = @This();

    obj_type: ObjType,
    marked: bool = false,
    // True when old obj and was modified
    dirty: bool = false,

    // FIXME: restore object chaining?

    pub fn typeEql(self: *Self, type_def: *ObjTypeDef) bool {
        return switch (self.obj_type) {
            .Pattern => type_def.def_type == .Pattern,
            .String => type_def.def_type == .String,
            .Type => type_def.def_type == .Type,
            .UpValue => uv: {
                var upvalue = ObjUpValue.cast(self).?;
                break :uv (upvalue.closed orelse upvalue.location.*).typeEql(type_def);
            },
            .EnumInstance => ei: {
                var instance = ObjEnumInstance.cast(self).?;
                break :ei type_def.def_type == .EnumInstance and instance.enum_ref.type_def.eql(type_def.resolved_type.?.EnumInstance);
            },
            .ObjectInstance => oi: {
                var instance = ObjObjectInstance.cast(self).?;
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
};

pub const ObjFiber = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Fiber },

    fiber: *Fiber,

    pub fn mark(self: *Self, gc: *GC) !void {
        try gc.markFiber(self.fiber);
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
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
            return Value.fromObj(.{ .index = umethod.index, .obj_type = .Native });
        }

        var native = try vm.gc.allocateObject(
            ObjNative{
                // Complains about const qualifier discard otherwise
                .native = @as(*anyopaque, @ptrFromInt(@intFromPtr(members[method_idx]))),
            },
        );

        vm.gc.objfiber_members[method_idx] = native;

        // We need to mark it otherwise it could be collected by a Young gc and then badly accessed by a Full gc
        vm.gc.markObj(ObjNative, native) catch @panic("Could not mark obj");

        return Value.fromObj(.{ .index = native.index, .obj_type = .Native });
    }

    pub fn memberDefByName(parser: *Parser, name: []const u8) !?Pool(ObjTypeDef).Idx {
        return if (members_name.get(name)) |idx|
            try memberDef(parser, idx)
        else
            null;
    }

    pub fn memberDef(parser: *Parser, method_idx: usize) !Pool(ObjTypeDef).Idx {
        if (parser.gc.objfiber_memberDefs[method_idx]) |umethod| {
            return umethod;
        }

        const native_type = try parser.parseTypeDefFrom(members_typedef[method_idx]);

        parser.gc.objfiber_memberDefs[method_idx] = native_type;

        return native_type;
    }

    fn is(self_idx: Pool(Self).Idx, type_def: Pool(ObjTypeDef).Idx, gc: *GC) bool {
        return ObjTypeDef.eql(
            type_def,
            self_idx.get(gc).fiber.type_def,
            gc,
        );
    }

    pub const FiberDef = struct {
        const SelfFiberDef = @This();

        return_type: Pool(ObjTypeDef).Idx,
        yield_type: Pool(ObjTypeDef).Idx,

        pub fn mark(self: *SelfFiberDef, gc: *GC) !void {
            try gc.markObj(ObjTypeDef, self.return_type);
            try gc.markObj(ObjTypeDef, self.yield_type);
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

    pub fn mark(_: *Self, _: *GC) !void {}

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
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
            "extern fun match(subject: str) > [obj{ capture: str, start: int, end: int }]?",
            "extern fun matchAll(subject: str) > [[obj{ capture: str, start: int, end: int }]]?",
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
            return Value.fromObj(.{ .index = umethod.index, .obj_type = .Native });
        }

        var native = try vm.gc.allocateObject(
            ObjNative{
                // Complains about const qualifier discard otherwise
                .native = @as(*anyopaque, @ptrFromInt(@intFromPtr(members[method_idx]))),
            },
        );

        vm.gc.objpattern_members[method_idx] = native;

        // We need to mark it otherwise it could be collected by a Young gc and then badly accessed by a Full gc
        vm.gc.markObj(ObjNative, native) catch @panic("Could not mark obj");

        return Value.fromObj(.{ .index = native.index, .obj_type = .Native });
    }

    pub fn memberByName(vm: *VM, name: []const u8) !?Value {
        return if (members_name.get(name)) |idx|
            try member(vm, idx)
        else
            null;
    }

    pub fn memberDef(parser: *Parser, method_idx: usize) !Pool(ObjTypeDef).Idx {
        if (parser.gc.objpattern_memberDefs[method_idx]) |umethod| {
            return umethod;
        }

        const native_type = try parser.parseTypeDefFrom(members_typedef[method_idx]);

        parser.gc.objpattern_memberDefs[method_idx] = native_type;

        return native_type;
    }

    pub fn memberDefByName(parser: *Parser, name: []const u8) !?Pool(ObjTypeDef).Idx {
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

    pub fn mark(_: *Self, _: *GC) void {}

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }
};

/// A String
pub const ObjString = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .String },

    /// The actual string
    string: []const u8,

    pub fn mark(_: *Self, _: *GC) !void {}

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn concat(self: *Self, vm: *VM, other: *Self) !Pool(Self).Idx {
        var new_string = std.ArrayList(u8).empty;
        try new_string.appendSlice(vm.gc.allocator, self.string);
        try new_string.appendSlice(vm.gc.allocator, other.string);

        return vm.gc.copyString(new_string.items);
    }

    pub fn next(self: *Self, vm: *VM, str_index: ?Integer) !?Integer {
        if (str_index) |index| {
            if (index < 0 or index >= @as(Integer, @intCast(self.string.len))) {
                try vm.throw(
                    VM.Error.OutOfBound,
                    .fromObj(
                        .{
                            .index = (try vm.gc.copyString("Out of bound access to str")).index,
                            .obj_type = .String,
                        },
                    ),
                    null,
                    null,
                );
            }

            return if (index + 1 >= @as(Integer, @intCast(self.string.len)))
                null
            else
                index + 1;
        } else {
            return if (self.string.len > 0) @as(Integer, 0) else null;
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
            return Value.fromObj(.{ .index = umethod.index, .obj_type = .Native });
        }

        const native = try vm.gc.allocateObject(
            ObjNative{
                // Complains about const qualifier discard otherwise
                .native = @as(*anyopaque, @ptrFromInt(@intFromPtr(members[method_idx]))),
            },
        );

        vm.gc.objstring_members[method_idx] = native;

        // We need to mark it otherwise it could be collected by a Young gc and then badly accessed by a Full gc
        vm.gc.markObj(ObjNative, native) catch @panic("Could not mark obj");

        return Value.fromObj(.{ .index = native.index, .obj_type = .Native });
    }

    pub fn memberDefByName(parser: *Parser, name: []const u8) !?Pool(ObjTypeDef).Idx {
        return if (members_name.get(name)) |idx|
            try memberDef(parser, idx)
        else
            null;
    }

    pub fn memberDef(parser: *Parser, method_idx: usize) !Pool(ObjTypeDef).Idx {
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
    next: ?Pool(ObjUpValue).Idx = null,

    pub fn init(slot: *Value) Self {
        return Self{
            .closed = null,
            .location = slot,
            .next = null,
        };
    }

    pub fn mark(self: *Self, gc: *GC) !void {
        try gc.markValue(self.location.*); // Useless
        if (self.closed) |uclosed| {
            try gc.markValue(uclosed);
        }
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }
};

/// Closure
pub const ObjClosure = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Closure },

    // Buzz function
    function: Pool(ObjFunction).Idx,

    upvalues: []Pool(ObjUpValue).Idx,
    // Pointer to the global with which the function was declared
    globals: *std.ArrayList(Value),

    pub fn init(allocator: Allocator, vm: *VM, function: Pool(ObjFunction).Idx) !Self {
        return Self{
            .globals = &vm.globals,
            .function = function,
            .upvalues = try allocator.alloc(
                Pool(ObjUpValue).Idx,
                function.get(vm.gc).upvalue_count,
            ),
        };
    }

    pub fn mark(self: *Self, gc: *GC) !void {
        try gc.markObj(ObjFunction, self.function);
        for (self.upvalues) |upvalue| {
            try gc.markObj(ObjUpValue, upvalue);
        }
        for (self.globals.items) |global| {
            try gc.markValue(global);
        }
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.upvalues);
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }
};

pub const NativeCtx = extern struct {
    vm: *VM,
    globals: [*]Value,
    upvalues: [*]Pool(ObjUpValue).Idx,
    // Where to reset the stack when we exit the function
    base: [*]Value,
    // Pointer to the stack_top field of the current fiber
    // !! Needs to change when current fiber changes !!
    stack_top: *[*]Value,
};

// 1 = return value on stack, 0 = no return value, -1 = error
pub const Native = fn (ctx: *NativeCtx) callconv(.c) c_int;
pub const NativeFn = *const Native;

pub const Compiled = fn (ctx: *NativeCtx) callconv(.c) Value;
pub const CompiledFn = *const Compiled;

/// Native function
pub const ObjNative = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Native },

    // TODO: issue is list.member which separate its type definition from its runtime creation
    // type_def: *ObjTypeDef,
    native: *anyopaque,

    pub fn mark(_: *Self, _: *GC) void {}

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
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

    type_def: Pool(ObjTypeDef).Idx = undefined, // Undefined because function initialization is in several steps

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

    pub fn mark(self: *Self, gc: *GC) !void {
        try gc.markObj(ObjTypeDef, self.type_def);
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

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub const FunctionDef = struct {
        const FunctionDefSelf = @This();

        var next_id: usize = 0;

        id: usize,
        name: Pool(ObjString).Idx,
        script_name: Pool(ObjString).Idx,
        return_type: Pool(ObjTypeDef).Idx,
        yield_type: Pool(ObjTypeDef).Idx,
        error_types: ?[]const Pool(ObjTypeDef).Idx = null,
        // TODO: rename 'arguments'
        parameters: std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx) = .empty,
        // Storing here the defaults means they can only be non-Obj values
        defaults: std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Value) = .empty,
        function_type: FunctionType = .Function,
        lambda: bool = false,

        generic_types: std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx) = .empty,
        resolved_generics: ?[]Pool(ObjTypeDef).Idx = null,

        pub fn nextId() usize {
            FunctionDefSelf.next_id += 1;

            return FunctionDefSelf.next_id;
        }

        pub fn mark(self: *FunctionDefSelf, gc: *GC) !void {
            try gc.markObj(ObjString, self.name);
            try gc.markObj(ObjString, self.script_name);
            try gc.markObj(ObjTypeDef, self.return_type);
            try gc.markObj(ObjTypeDef, self.yield_type);

            var it = self.parameters.iterator();
            while (it.next()) |parameter| {
                try gc.markObj(ObjString, parameter.key_ptr.*);
                try gc.markObj(ObjTypeDef, parameter.value_ptr.*);
            }

            var it2 = self.defaults.iterator();
            while (it2.next()) |default| {
                try gc.markObj(ObjString, default.key_ptr.*);
                try gc.markValue(default.value_ptr.*);
            }

            if (self.error_types) |error_types| {
                for (error_types) |error_item| {
                    try gc.markObj(ObjTypeDef, error_item);
                }
            }

            var it3 = self.generic_types.iterator();
            while (it3.next()) |kv| {
                try gc.markObj(ObjString, kv.key_ptr.*);
                try gc.markObj(ObjTypeDef, kv.value_ptr.*);
            }
        }
    };
};

/// Object instance
pub const ObjObjectInstance = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .ObjectInstance },

    /// Object (null when anonymous)
    object: ?Pool(ObjObject).Idx,
    /// Populated object type
    type_def: Pool(ObjTypeDef).Idx,
    /// Fields value
    fields: []Value,
    /// VM in which the instance was created, we need this so the instance destructor can be called in the appropriate vm
    vm: *VM,

    pub fn setField(self_idx: Pool(Self).Idx, gc: *GC, key: usize, value: Value) !void {
        const self = self_idx.get(gc);

        self.fields[key] = value;
        try gc.markObjDirty(Self, self_idx);
    }

    /// Should not be called by runtime when possible
    pub fn setFieldByName(self: *Self, gc: *GC, key: Pool(ObjString).Idx, value: Value) !void {
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
        object: ?Pool(ObjObject).Idx,
        type_def: Pool(ObjTypeDef).Idx,
        gc: *GC,
    ) !Self {
        return .{
            .vm = vm,
            .object = object,
            .type_def = type_def,
            .fields = try gc.allocateMany(
                Value,
                type_def.get(vm.gc)
                    .resolved_type.?.ObjectInstance.of.get(vm.gc)
                    .resolved_type.?.Object
                    .propertiesCount(),
            ),
        };
    }

    pub fn mark(self: *Self, gc: *GC) !void {
        if (self.object) |object| {
            try gc.markObj(ObjObject, object);
        }
        try gc.markObj(ObjTypeDef, self.type_def);
        for (self.fields) |field| {
            try gc.markValue(field);
        }
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.fields);
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    fn is(self_idx: Pool(Self).Idx, type_def_idx: Pool(ObjTypeDef).Idx, gc: *GC) bool {
        const self = self_idx.get(gc);
        const type_def = type_def_idx.get(gc);
        const self_type_def = self.type_def.get(gc);
        if (type_def.def_type == .ObjectInstance) {
            return self_type_def.resolved_type.?.ObjectInstance.of.index == type_def.resolved_type.?.ObjectInstance.of.index;
        } else if (type_def.def_type == .Protocol) {
            return self_type_def.resolved_type.?.ObjectInstance.of.get(gc)
                .resolved_type.?.Object
                .conforms_to.get(type_def_idx) != null;
        } else if (type_def.def_type == .Object) {
            return self_type_def.resolved_type.?.ObjectInstance.of.index == type_def_idx.index;
        } else if (type_def.def_type == .ProtocolInstance) {
            return self_type_def.resolved_type.?.ObjectInstance.of.get(gc)
                .resolved_type.?.Object
                .conforms_to.get(type_def.resolved_type.?.ProtocolInstance.of) != null;
        }

        return false;
    }
};

/// FFI struct or union
pub const ObjForeignContainer = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .ForeignContainer },

    type_def: Pool(ObjTypeDef).Idx,
    data: []u8,

    pub fn init(vm: *VM, type_def: Pool(ObjTypeDef).Idx) !Self {
        const zig_type = type_def.get(vm.gc)
            .resolved_type.?.ForeignContainer.zig_type;

        return .{
            .type_def = type_def,
            .data = try vm.gc.allocateMany(u8, zig_type.size()),
        };
    }

    pub fn setField(self_idx: Pool(Self).Idx, vm: *VM, field_idx: usize, value: Value) !void {
        const self = self_idx.get(vm.gc);

        self.type_def.get(vm.gc)
            .resolved_type.?.ForeignContainer
            .fields.values()[field_idx]
            .setter(
            vm,
            self.data.ptr,
            value,
        );
        try vm.gc.markObjDirty(Self, self_idx);
    }

    pub fn getField(self: *Self, vm: *VM, field_idx: usize) Value {
        return self.type_def.get(vm.gc)
            .resolved_type.?.ForeignContainer.fields.values()[field_idx].getter(
            vm,
            self.data.ptr,
        );
    }

    fn is(self: Pool(Self).Idx, type_def_idx: Pool(ObjTypeDef).Idx, gc: *GC) bool {
        return self.get(gc).type_def.index == type_def_idx.index;
    }

    pub const ContainerDef = struct {
        pub const Getter = fn (vm: *VM, data: [*]u8) callconv(.c) Value;
        pub const Setter = fn (vm: *VM, data: [*]u8, value: Value) callconv(.c) void;

        pub const Field = struct {
            offset: usize,
            getter: *Getter,
            setter: *Setter,
        };

        location: Ast.TokenIndex,
        name: Pool(ObjString).Idx,
        qualified_name: Pool(ObjString).Idx,

        zig_type: ZigType,
        buzz_type: std.StringArrayHashMapUnmanaged(Pool(ObjTypeDef).Idx),

        // Filled by codegen
        fields: std.StringArrayHashMapUnmanaged(Field),

        pub fn mark(def: *ContainerDef, gc: *GC) !void {
            try gc.markObj(ObjString, def.name);
            try gc.markObj(ObjString, def.qualified_name);
            var it = def.buzz_type.iterator();
            while (it.next()) |kv| {
                try gc.markObj(ObjTypeDef, kv.value_ptr.*);
            }
        }
    };

    pub fn mark(self: *Self, gc: *GC) !void {
        try gc.markObj(ObjTypeDef, self.type_def);
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }
};

/// Object
pub const ObjObject = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Object },

    type_def: Pool(ObjTypeDef).Idx,

    /// Static fields and methods
    fields: []Value,
    /// Properties default values (null if none)
    defaults: []?Value,

    /// To avoid counting object fields that are instance properties
    property_count: ?usize = 0,

    pub fn init(gc: *GC, type_def: Pool(ObjTypeDef).Idx) !Self {
        const object_def = type_def.get(gc)
            .resolved_type.?.Object;

        const self = Self{
            .fields = try gc.allocator.alloc(
                Value,
                object_def.fields.count(),
            ),
            .defaults = try gc.allocator.alloc(
                ?Value,
                object_def.propertiesCount(),
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

    pub fn setField(self: Pool(Self).Idx, gc: *GC, key: usize, value: Value) !void {
        self.get(gc).fields[key] = value;
        try gc.markObjDirty(Self, self);
    }

    pub fn setDefault(self: Pool(Self).Idx, gc: *GC, key: usize, value: Value) !void {
        self.get(gc).defaults[key] = value;
        try gc.markObjDirty(Self, self);
    }

    pub fn setPropertyDefaultValue(self: Pool(Self).Idx, gc: *GC, key: usize, value: Value) !void {
        self.get(gc).defaults[key] = value;
        try gc.markObjDirty(Self, self);
    }

    pub fn mark(self: *Self, gc: *GC) !void {
        try gc.markObj(ObjTypeDef, self.type_def);

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

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub const ProtocolDef = struct {
        const ProtocolDefSelf = @This();

        pub const Method = struct {
            mutable: bool,
            type_def: Pool(ObjTypeDef).Idx,
        };

        location: Ast.TokenIndex,
        name: Pool(ObjString).Idx,
        qualified_name: Pool(ObjString).Idx,
        methods: std.StringArrayHashMapUnmanaged(Method),
        methods_locations: std.StringArrayHashMapUnmanaged(Ast.TokenIndex),

        pub fn init(location: Ast.TokenIndex, name: Pool(ObjString).Idx, qualified_name: Pool(ObjString).Idx) ProtocolDefSelf {
            return ProtocolDefSelf{
                .name = name,
                .location = location,
                .qualified_name = qualified_name,
                .methods = .empty,
                .methods_locations = .empty,
            };
        }

        pub fn deinit(self: *ProtocolDefSelf) void {
            self.methods.deinit();
            self.methods_locations.deinit();
        }

        pub fn mark(self: *ProtocolDefSelf, gc: *GC) !void {
            try gc.markObj(ObjString, self.name);
            try gc.markObj(ObjString, self.qualified_name);

            var it = self.methods.iterator();
            while (it.next()) |kv| {
                try gc.markObj(ObjTypeDef, kv.value_ptr.*.type_def);
            }
        }
    };

    pub const ObjectDef = struct {
        var next_id: usize = 0;

        pub const Field = struct {
            name: []const u8,
            location: Ast.TokenIndex,
            type_def: Pool(ObjTypeDef).Idx,
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

        pub const Placeholder = struct {
            placeholder: Pool(ObjTypeDef).Idx,
            referrers: std.ArrayList(Ast.Node.Index) = .{},
        };

        id: usize,
        location: Ast.TokenIndex,
        name: Pool(ObjString).Idx,
        qualified_name: Pool(ObjString).Idx,
        fields: std.StringArrayHashMapUnmanaged(Field) = .empty,
        // When we have placeholders we don't know if they are properties or methods
        // That information is available only when the placeholder is resolved
        placeholders: std.StringHashMapUnmanaged(Placeholder) = .empty,
        static_placeholders: std.StringHashMapUnmanaged(Placeholder) = .empty,
        anonymous: bool,
        conforms_to: std.AutoHashMapUnmanaged(Pool(ObjTypeDef).Idx, void) = .empty,

        generic_types: std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx) = .empty,
        resolved_generics: ?[]Pool(ObjTypeDef).Idx = null,

        pub fn nextId() usize {
            ObjectDef.next_id += 1;

            return ObjectDef.next_id;
        }

        pub fn init(
            location: Ast.TokenIndex,
            name: Pool(ObjString).Idx,
            qualified_name: Pool(ObjString).Idx,
            anonymous: bool,
        ) ObjectDef {
            return ObjectDef{
                .id = ObjectDef.nextId(),
                .name = name,
                .location = location,
                .qualified_name = qualified_name,
                .anonymous = anonymous,
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
        pub fn bothConforms(self: ObjectDef, other: ObjectDef) ?Pool(ObjTypeDef).Idx {
            var it = self.conforms_to.iterator();
            while (it.next()) |kv| {
                if (other.conforms_to.get(kv.key_ptr.*) != null) {
                    return kv.key_ptr.*;
                }
            }

            return null;
        }

        pub fn mark(self: *ObjectDef, gc: *GC) !void {
            try gc.markObj(ObjString, self.name);
            try gc.markObj(ObjString, self.qualified_name);

            var it = self.fields.iterator();
            while (it.next()) |kv| {
                try gc.markObj(ObjTypeDef, kv.value_ptr.*.type_def);
            }

            var it5 = self.placeholders.iterator();
            while (it5.next()) |kv| {
                try gc.markObj(ObjTypeDef, kv.value_ptr.*.placeholder);
            }

            var it6 = self.static_placeholders.iterator();
            while (it6.next()) |kv| {
                try gc.markObj(ObjTypeDef, kv.value_ptr.*.placeholder);
            }

            var it7 = self.conforms_to.iterator();
            while (it7.next()) |kv| {
                try gc.markObj(ObjTypeDef, kv.key_ptr.*);
            }

            var it8 = self.generic_types.iterator();
            while (it8.next()) |kv| {
                try gc.markObj(ObjString, kv.key_ptr.*);
                try gc.markObj(ObjTypeDef, kv.value_ptr.*);
            }
        }
    };
};

/// List
pub const ObjList = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .List },

    type_def: Pool(ObjTypeDef).Idx,

    /// List items
    items: std.ArrayList(Value),

    methods: []?Pool(ObjNative).Idx,

    pub fn init(allocator: Allocator, type_def: Pool(ObjTypeDef).Idx) !Self {
        const self = Self{
            .items = std.ArrayList(Value){},
            .type_def = type_def,
            .methods = try allocator.alloc(
                ?Pool(ObjNative).Idx,
                Self.members.len,
            ),
        };

        for (0..Self.members.len) |i| {
            self.methods[i] = null;
        }

        return self;
    }

    pub fn mark(self: *Self, gc: *GC) !void {
        for (self.items.items) |value| {
            try gc.markValue(value);
        }
        try gc.markObj(ObjTypeDef, self.type_def);

        for (self.methods) |method_opt| {
            if (method_opt) |method| {
                try gc.markObj(ObjNative, method);
            }
        }
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.items.deinit(allocator);
        allocator.free(self.methods);
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
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
            return Value.fromObj(.{ .index = native.index, .obj_type = .Native });
        }

        var native = try vm.gc.allocateObject(
            ObjNative{
                // Complains about const qualifier discard otherwise
                .native = @as(*anyopaque, @ptrFromInt(@intFromPtr(members[method_idx]))),
            },
        );

        self.methods[method_idx] = native;

        // We need to mark it otherwise it could be collected by a Young gc and then badly accessed by a Full gc
        vm.gc.markObj(ObjNative, native) catch @panic("Could not mark obj");

        return Value.fromObj(.{ .index = native.index, .obj_type = .Native });
    }

    pub fn rawAppend(self: Pool(Self).Idx, gc: *GC, value: Value) !void {
        try self.get(gc).items.append(
            gc.allocator,
            value,
        );
        try gc.markObjDirty(Self, self);
    }

    pub fn rawInsert(self: Pool(Self).Idx, gc: *GC, index: usize, value: Value) !void {
        try self.get(gc).items.insert(
            gc.allocator,
            index,
            value,
        );
        try gc.markObjDirty(Self, self);
    }

    pub fn set(self: Pool(Self).Idx, gc: *GC, index: usize, value: Value) !void {
        self.get(gc).items.items[index] = value;
        try gc.markObjDirty(Self, self);
    }

    // Used also by the VM
    pub fn rawNext(self: *Self, vm: *VM, list_index: ?Integer) !?Integer {
        if (list_index) |index| {
            if (index < 0 or index >= @as(Integer, @intCast(self.items.items.len))) {
                try vm.throw(
                    VM.Error.OutOfBound,
                    .fromObj(
                        .{
                            .index = (try vm.gc.copyString("Out of bound access to list")).index,
                            .obj_type = .String,
                        },
                    ),
                    null,
                    null,
                );
            }

            return if (index + 1 >= @as(Integer, @intCast(self.items.items.len)))
                null
            else
                index + 1;
        } else {
            return if (self.items.items.len > 0) @as(Integer, 0) else null;
        }
    }

    pub const ListDef = struct {
        const SelfListDef = @This();

        pub const Method = struct {
            mutable: bool,
            type_def: Pool(ObjTypeDef).Idx,
        };

        item_type: Pool(ObjTypeDef).Idx,
        methods: std.StringHashMapUnmanaged(Method),
        mutable: bool,

        pub fn init(item_type: Pool(ObjTypeDef).Idx, mutable: bool) SelfListDef {
            return .{
                .item_type = item_type,
                .mutable = mutable,
                .methods = std.StringHashMapUnmanaged(Method){},
            };
        }

        pub fn deinit(self: *SelfListDef) void {
            self.methods.deinit();
        }

        pub fn mark(self: *SelfListDef, gc: *GC) !void {
            try gc.markObj(ObjTypeDef, self.item_type);
            var it = self.methods.iterator();
            while (it.next()) |method| {
                try gc.markObj(ObjTypeDef, method.value_ptr.*.type_def);
            }
        }

        pub fn member(obj_list_idx: Pool(ObjTypeDef).Idx, gc: *GC, method: []const u8) !?Method {
            const obj_list = obj_list_idx.get(gc);
            var self = &obj_list.resolved_type.?.List;

            if (self.methods.get(method)) |native_def| {
                return native_def;
            }

            if (mem.eql(u8, method, "append")) {
                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                // `value` arg is of item_type
                try parameters.put(
                    gc.allocator,
                    try gc.copyString("value"),
                    self.item_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("append"),
                                .parameters = parameters,
                                .return_type = gc.type_registry.void_type,
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "append",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "remove")) {
                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("at"),
                    gc.type_registry.int_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("remove"),
                                .parameters = parameters,
                                .return_type = try gc.type_registry.getTypeDef(
                                    .{
                                        .optional = true,
                                        .def_type = self.item_type.get(gc).def_type,
                                        .resolved_type = self.item_type.get(gc).resolved_type,
                                    },
                                ),
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "remove",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "len")) {
                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("len"),
                                .return_type = gc.type_registry.int_type,
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "len",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "next")) {
                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                // `key` arg is number
                try parameters.put(
                    gc.allocator,
                    try gc.copyString("key"),
                    try gc.type_registry.getTypeDef(
                        ObjTypeDef{
                            .def_type = .Integer,
                            .optional = true,
                        },
                    ),
                );
                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("next"),
                                .parameters = parameters,
                                // When reached end of list, returns null
                                .return_type = try gc.type_registry.getTypeDef(
                                    ObjTypeDef{
                                        .def_type = .Integer,
                                        .optional = true,
                                    },
                                ),
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "next",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "sub")) {
                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("start"),
                    gc.type_registry.int_type,
                );

                const len_str = try gc.copyString("len");
                try parameters.put(
                    gc.allocator,
                    len_str,
                    try gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Integer,
                            .optional = true,
                        },
                    ),
                );

                var defaults = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Value).empty;
                try defaults.put(gc.allocator, len_str, Value.Null);

                const native_type = try gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("sub"),
                                .parameters = parameters,
                                .defaults = defaults,
                                .return_type = obj_list_idx,
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "sub",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "fill")) {
                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("value"),
                    gc.type_registry.any_type,
                );

                const start_str = try gc.copyString("start");
                try parameters.put(
                    gc.allocator,
                    start_str,
                    try gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Integer,
                            .optional = true,
                        },
                    ),
                );

                const len_str = try gc.copyString("len");
                try parameters.put(
                    gc.allocator,
                    len_str,
                    try gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Integer,
                            .optional = true,
                        },
                    ),
                );

                var defaults = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Value).empty;
                try defaults.put(gc.allocator, len_str, Value.Null);
                try defaults.put(gc.allocator, start_str, Value.Null);

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("fill"),
                                .parameters = parameters,
                                .defaults = defaults,
                                .return_type = obj_list_idx,
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "fill",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "indexOf")) {
                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("needle"),
                    self.item_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("indexOf"),
                                .parameters = parameters,
                                .return_type = try gc.type_registry.getTypeDef(
                                    .{
                                        .def_type = .Integer,
                                        .optional = true,
                                    },
                                ),
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "indexOf",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "join")) {
                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("separator"),
                    gc.type_registry.str_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("join"),
                                .parameters = parameters,
                                .return_type = gc.type_registry.str_type,
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "join",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "insert")) {
                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("index"),
                    gc.type_registry.int_type,
                );

                // `value` arg is of item_type
                try parameters.put(
                    gc.allocator,
                    try gc.copyString("value"),
                    self.item_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("insert"),
                                .parameters = parameters,
                                .return_type = try ObjTypeDef.cloneOptional(self.item_type, &gc.type_registry),
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "insert",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "pop")) {
                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("pop"),
                                .return_type = try ObjTypeDef.cloneOptional(self.item_type, &gc.type_registry),
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "pop",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "forEach")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("index"),
                    gc.type_registry.int_type,
                );
                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("element"),
                    self.item_type,
                );

                const callback_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .return_type = gc.type_registry.void_type,
                                .yield_type = gc.type_registry.void_type,
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("callback"),
                    callback_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("forEach"),
                                .parameters = parameters,
                                .return_type = gc.type_registry.void_type,
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "forEach",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "map")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                const map_origin = ObjFunction.FunctionDef.nextId();
                const generic_type = try gc.type_registry.getTypeDef(
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

                var callback_parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("index"),
                    gc.type_registry.int_type,
                );
                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("element"),
                    self.item_type,
                );

                const callback_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .return_type = generic_type,
                                .yield_type = gc.type_registry.void_type,
                                // FIXME: user could provide an .Extern function and JIT will be lost here
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("callback"),
                    callback_type,
                );

                const new_list_def = ObjList.ListDef.init(
                    generic_type,
                    false,
                );

                const new_list_type = ObjTypeDef.TypeUnion{ .List = new_list_def };

                var method_def = ObjFunction.FunctionDef{
                    .id = map_origin,
                    .script_name = try gc.copyString("builtin"),
                    .name = try gc.copyString("map"),
                    .parameters = parameters,
                    .return_type = try gc.type_registry.getTypeDef(.{
                        .def_type = .List,
                        .resolved_type = new_list_type,
                    }),
                    .yield_type = gc.type_registry.void_type,
                    .function_type = .Extern,
                };

                try method_def.generic_types.put(
                    gc.allocator,
                    try gc.copyString("T"),
                    generic_type,
                );

                const resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                const native_type = try gc.type_registry.getTypeDef(
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
                    gc.allocator,
                    "map",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "filter")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("index"),
                    gc.type_registry.int_type,
                );

                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("element"),
                    self.item_type,
                );

                const callback_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .return_type = gc.type_registry.bool_type,
                                .yield_type = gc.type_registry.void_type,
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("callback"),
                    callback_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("filter"),
                                .parameters = parameters,
                                .return_type = obj_list_idx,
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "filter",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "reduce")) {
                const reduce_origin = ObjFunction.FunctionDef.nextId();

                // Mapped type
                const generic_type = try gc.type_registry.getTypeDef(
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

                var callback_parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("index"),
                    gc.type_registry.int_type,
                );
                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("element"),
                    self.item_type,
                );
                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("accumulator"),
                    generic_type,
                );

                const callback_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .return_type = generic_type,
                                .yield_type = gc.type_registry.void_type,
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("callback"),
                    callback_type,
                );
                try parameters.put(
                    gc.allocator,
                    try gc.copyString("initial"),
                    generic_type,
                );

                var generic_types = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;
                try generic_types.put(
                    gc.allocator,
                    try gc.copyString("T"),
                    generic_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = reduce_origin,
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("reduce"),
                                .parameters = parameters,
                                .return_type = generic_type,
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "reduce",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "sort")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("left"),
                    self.item_type,
                );
                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("right"),
                    self.item_type,
                );

                const callback_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .return_type = gc.type_registry.bool_type,
                                .yield_type = gc.type_registry.void_type,
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("callback"),
                    callback_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("sort"),
                                .parameters = parameters,
                                .return_type = obj_list_idx,
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
                    "sort",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "reverse")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("reverse"),
                                .return_type = obj_list_idx,
                                .yield_type = gc.type_registry.void_type,
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
                    gc.allocator,
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
                    .type_def = try gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Function,
                            .resolved_type = .{
                                .Function = .{
                                    .id = ObjFunction.FunctionDef.nextId(),
                                    .script_name = try gc.copyString("builtin"),
                                    .name = try gc.copyString(method),
                                    .return_type = try ObjTypeDef.cloneMutable(
                                        obj_list_idx,
                                        &gc.type_registry,
                                        mem.eql(u8, method, "cloneMutable") or mem.eql(u8, method, "copyMutable"),
                                    ),
                                    .yield_type = gc.type_registry.void_type,
                                    .function_type = .Extern,
                                },
                            },
                        },
                    ),
                    .mutable = mem.eql(u8, method, "cloneImmutable") or mem.eql(u8, method, "copyMutable"),
                };

                try self.methods.put(
                    gc.allocator,
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

    pub fn mark(_: *Self, _: *GC) void {}

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
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
            return Value.fromObj(.{ .index = native.index, .obj_type = .Native });
        }

        var native = try vm.gc.allocateObject(
            ObjNative{
                // Complains about const qualifier discard otherwise
                .native = @as(*anyopaque, @ptrFromInt(@intFromPtr(members[method_idx]))),
            },
        );

        vm.gc.objrange_members[method_idx] = native;

        // We need to mark it otherwise it could be collected by a Young gc and then badly accessed by a Full gc
        vm.gc.markObj(ObjNative, native) catch @panic("Could not mark obj");

        return Value.fromObj(.{ .index = native.index, .obj_type = .Native });
    }

    pub fn memberDefByName(parser: *Parser, name: []const u8) !?Pool(ObjTypeDef).Idx {
        return if (members_name.get(name)) |idx|
            try memberDef(parser, idx)
        else
            null;
    }

    pub fn memberDef(parser: *Parser, method_idx: usize) !Pool(ObjTypeDef).Idx {
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

    pub const Map = std.AutoArrayHashMapUnmanaged(Value, Value);

    obj: Obj = .{ .obj_type = .Map },

    type_def: Pool(ObjTypeDef).Idx,

    // We need an ArrayHashMap for `next`
    map: Map = .empty,

    methods: []?Pool(ObjNative).Idx,

    pub fn init(allocator: Allocator, type_def: Pool(ObjTypeDef).Idx) !Self {
        const self = Self{
            .type_def = type_def,
            .methods = try allocator.alloc(
                ?Pool(ObjNative).Idx,
                Self.members.len,
            ),
        };

        for (0..Self.members.len) |i| {
            self.methods[i] = null;
        }

        return self;
    }

    pub fn set(self: Pool(Self).Idx, gc: *GC, key: Value, value: Value) !void {
        try self.get(gc).map.put(
            gc.allocator,
            key,
            value,
        );
        try gc.markObjDirty(Self, self);
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
            return Value.fromObj(.{ .index = native.index, .obj_type = .Native });
        }

        const native = try vm.gc.allocateObject(
            ObjNative{
                // Complains about const qualifier discard otherwise
                .native = @constCast(members[method_idx]),
            },
        );

        self.methods[method_idx] = native;

        // We need to mark it otherwise it could be collected by a Young gc and then badly accessed by a Full gc
        vm.gc.markObj(ObjNative, native) catch @panic("Could not mark obj");

        return Value.fromObj(.{ .index = native.index, .obj_type = .Native });
    }

    pub fn mark(self: *Self, gc: *GC) !void {
        var it = self.map.iterator();
        while (it.next()) |kv| {
            try gc.markValue(kv.key_ptr.*);
            try gc.markValue(kv.value_ptr.*);
        }

        for (self.methods) |method_opt| {
            if (method_opt) |method| {
                try gc.markObj(ObjNative, method);
            }
        }

        try gc.markObj(ObjTypeDef, self.type_def);
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

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub const MapDef = struct {
        const SelfMapDef = @This();

        pub const Method = struct {
            mutable: bool,
            type_def: Pool(ObjTypeDef).Idx,
        };

        key_type: Pool(ObjTypeDef).Idx,
        value_type: Pool(ObjTypeDef).Idx,
        mutable: bool,

        methods: std.StringHashMapUnmanaged(Method),

        pub fn init(key_type: Pool(ObjTypeDef).Idx, value_type: Pool(ObjTypeDef).Idx, mutable: bool) SelfMapDef {
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

        pub fn mark(self: *SelfMapDef, gc: *GC) !void {
            try gc.markObj(ObjTypeDef, self.key_type);
            try gc.markObj(ObjTypeDef, self.value_type);
            var it = self.methods.iterator();
            while (it.next()) |method| {
                try gc.markObj(ObjTypeDef, method.value_ptr.*.type_def);
            }
        }

        pub fn member(obj_map_idx: Pool(ObjTypeDef).Idx, gc: *GC, method: []const u8) !?Method {
            const obj_map = obj_map_idx.get(gc);
            const map_def = &obj_map.resolved_type.?.Map;
            const key_type = map_def.key_type;
            const value_type = map_def.value_type;
            const is_mutable = map_def.mutable;

            if (map_def.methods.get(method)) |native_def| {
                return native_def;
            }

            if (mem.eql(u8, method, "size")) {
                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("size"),
                                .return_type = gc.type_registry.int_type,
                                .yield_type = gc.type_registry.void_type,
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try obj_map_idx.get(gc).resolved_type.?.Map.methods.put(
                    gc.allocator,
                    "size",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "remove")) {
                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("at"),
                    key_type,
                );

                const value_type_def = value_type.get(gc);
                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("remove"),
                                .parameters = parameters,
                                .return_type = try gc.type_registry.getTypeDef(
                                    .{
                                        .optional = true,
                                        .def_type = value_type_def.def_type,
                                        .resolved_type = value_type_def.resolved_type,
                                    },
                                ),
                                .yield_type = gc.type_registry.void_type,
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = true,
                };
                try obj_map_idx.get(gc).resolved_type.?.Map.methods.put(
                    gc.allocator,
                    "remove",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "keys")) {
                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("keys"),
                                .return_type = try gc.type_registry.getTypeDef(
                                    .{
                                        .def_type = .List,
                                        .optional = false,
                                        .resolved_type = .{
                                            .List = ObjList.ListDef.init(
                                                key_type,
                                                false,
                                            ),
                                        },
                                    },
                                ),
                                .yield_type = gc.type_registry.void_type,
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try obj_map_idx.get(gc).resolved_type.?.Map.methods.put(
                    gc.allocator,
                    "keys",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "values")) {
                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("values"),
                                .return_type = try gc.type_registry.getTypeDef(.{
                                    .def_type = .List,
                                    .optional = false,
                                    .resolved_type = .{
                                        .List = .init(
                                            value_type,
                                            false,
                                        ),
                                    },
                                }),
                                .yield_type = gc.type_registry.void_type,
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try obj_map_idx.get(gc).resolved_type.?.Map.methods.put(
                    gc.allocator,
                    "values",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "sort")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("left"),
                    key_type,
                );
                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("right"),
                    key_type,
                );

                const callback_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .return_type = gc.type_registry.bool_type,
                                .yield_type = gc.type_registry.void_type,
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("callback"),
                    callback_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("sort"),
                                .parameters = parameters,
                                .return_type = obj_map_idx,
                                .yield_type = gc.type_registry.void_type,
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = true,
                };
                try obj_map_idx.get(gc).resolved_type.?.Map.methods.put(
                    gc.allocator,
                    "sort",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "diff")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("other"),
                    obj_map_idx,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("diff"),
                                .parameters = parameters,
                                .return_type = obj_map_idx,
                                .yield_type = gc.type_registry.void_type,
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try obj_map_idx.get(gc).resolved_type.?.Map.methods.put(
                    gc.allocator,
                    "diff",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "intersect")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("other"),
                    obj_map_idx,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("intersect"),
                                .parameters = parameters,
                                .return_type = obj_map_idx,
                                .yield_type = gc.type_registry.void_type,
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try obj_map_idx.get(gc).resolved_type.?.Map.methods.put(
                    gc.allocator,
                    "intersect",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "forEach")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("key"),
                    key_type,
                );
                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("value"),
                    value_type,
                );

                const callback_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .return_type = gc.type_registry.void_type,
                                .yield_type = gc.type_registry.void_type,
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("callback"),
                    callback_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("forEach"),
                                .parameters = parameters,
                                .return_type = gc.type_registry.void_type,
                                .yield_type = gc.type_registry.void_type,
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const method_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try obj_map_idx.get(gc).resolved_type.?.Map.methods.put(
                    gc.allocator,
                    "forEach",
                    method_def,
                );

                return method_def;
            } else if (mem.eql(u8, method, "map")) {
                var callback_parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("key"),
                    key_type,
                );
                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("value"),
                    value_type,
                );

                var callback_method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    // TODO: is this ok?
                    .script_name = try gc.copyString("builtin"),
                    .name = try gc.copyString("anonymous"),
                    .parameters = callback_parameters,
                    .return_type = undefined,
                    .yield_type = gc.type_registry.void_type,
                    // FIXME: user could provide an .Extern function and JIT will be lost here
                };

                const map_origin = ObjFunction.FunctionDef.nextId();

                // Mapped type
                const generic_key_type = try gc.type_registry.getTypeDef(
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

                const generic_value_type = try gc.type_registry.getTypeDef(
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
                    0,
                    try gc.copyString("anonymous"),
                    try gc.copyString("anonymous"),
                    true,
                );

                try entry_def.fields.put(
                    gc.allocator,
                    "key",
                    .{
                        .name = "key",
                        .type_def = generic_key_type,
                        .final = false,
                        .method = false,
                        .static = false,
                        .has_default = false,
                        .mutable = false,
                        .location = 0,
                        .index = 0,
                    },
                );
                try entry_def.fields.put(
                    gc.allocator,
                    "value",
                    .{
                        .name = "value",
                        .type_def = generic_value_type,
                        .final = false,
                        .method = false,
                        .static = false,
                        .has_default = false,
                        .mutable = false,
                        .location = 0,
                        .index = 1,
                    },
                );

                const entry_type_def = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Object,
                        .resolved_type = .{ .Object = entry_def },
                    },
                );

                callback_method_def.return_type = try ObjTypeDef.toInstance(
                    entry_type_def,
                    &gc.type_registry,
                    false,
                );

                const callback_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{ .Function = callback_method_def },
                    },
                );

                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("callback"),
                    callback_type,
                );

                var method_def = ObjFunction.FunctionDef{
                    .id = map_origin,
                    .script_name = try gc.copyString("builtin"),
                    .name = try gc.copyString("map"),
                    .parameters = parameters,
                    .return_type = try gc.type_registry.getTypeDef(.{
                        .def_type = .Map,
                        .resolved_type = .{
                            .Map = ObjMap.MapDef.init(
                                generic_key_type,
                                generic_value_type,
                                false,
                            ),
                        },
                    }),
                    .yield_type = gc.type_registry.void_type,
                    .function_type = .Extern,
                };

                try method_def.generic_types.put(
                    gc.allocator,
                    try gc.copyString("K"),
                    generic_key_type,
                );

                try method_def.generic_types.put(
                    gc.allocator,
                    try gc.copyString("V"),
                    generic_value_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = .{ .Function = method_def },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try obj_map_idx.get(gc).resolved_type.?.Map.methods.put(
                    gc.allocator,
                    "map",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "filter")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("key"),
                    key_type,
                );
                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("value"),
                    value_type,
                );

                const callback_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .return_type = gc.type_registry.bool_type,
                                .yield_type = gc.type_registry.void_type,
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("callback"),
                    callback_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("filter"),
                                .parameters = parameters,
                                .return_type = obj_map_idx,
                                .yield_type = gc.type_registry.void_type,
                                .function_type = .Extern,
                            },
                        },
                    },
                );

                const member_def = Method{
                    .type_def = native_type,
                    .mutable = false,
                };
                try obj_map_idx.get(gc).resolved_type.?.Map.methods.put(
                    gc.allocator,
                    "filter",
                    member_def,
                );

                return member_def;
            } else if (mem.eql(u8, method, "reduce")) {
                const reduce_origin = ObjFunction.FunctionDef.nextId();

                // Mapped type
                const generic_type = try gc.type_registry.getTypeDef(
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

                var callback_parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("key"),
                    key_type,
                );
                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("value"),
                    value_type,
                );
                try callback_parameters.put(
                    gc.allocator,
                    try gc.copyString("accumulator"),
                    generic_type,
                );

                const callback_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = ObjFunction.FunctionDef.nextId(),
                                // TODO: is this ok?
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("anonymous"),
                                .parameters = callback_parameters,
                                .return_type = generic_type,
                                .yield_type = gc.type_registry.void_type,
                            },
                        },
                    },
                );

                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;

                try parameters.put(
                    gc.allocator,
                    try gc.copyString("callback"),
                    callback_type,
                );
                try parameters.put(
                    gc.allocator,
                    try gc.copyString("initial"),
                    generic_type,
                );

                var generic_types = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;
                try generic_types.put(
                    gc.allocator,
                    try gc.copyString("T"),
                    generic_type,
                );

                const native_type = try gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Function,
                        .resolved_type = .{
                            .Function = .{
                                .id = reduce_origin,
                                .script_name = try gc.copyString("builtin"),
                                .name = try gc.copyString("reduce"),
                                .parameters = parameters,
                                .return_type = generic_type,
                                .yield_type = gc.type_registry.void_type,
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
                try obj_map_idx.get(gc).resolved_type.?.Map.methods.put(
                    gc.allocator,
                    "reduce",
                    method_def,
                );

                return method_def;
            } else if ((is_mutable and (mem.eql(u8, method, "cloneImmutable") or
                mem.eql(u8, method, "copyMutable"))) or
                (!is_mutable and (mem.eql(u8, method, "cloneMutable") or
                    mem.eql(u8, method, "copyImmutable"))))
            {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                const method_def = Method{
                    .type_def = try gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Function,
                            .resolved_type = .{
                                .Function = .{
                                    .id = ObjFunction.FunctionDef.nextId(),
                                    .script_name = try gc.copyString("builtin"),
                                    .name = try gc.copyString(method),
                                    .return_type = try ObjTypeDef.cloneMutable(
                                        obj_map_idx,
                                        &gc.type_registry,
                                        mem.eql(u8, method, "cloneMutable") or mem.eql(u8, method, "copyMutable"),
                                    ),
                                    .yield_type = gc.type_registry.void_type,
                                    .function_type = .Extern,
                                },
                            },
                        },
                    ),
                    .mutable = mem.eql(u8, method, "cloneImmutable") or mem.eql(u8, method, "copyMutable"),
                };

                try obj_map_idx.get(gc).resolved_type.?.Map.methods.put(
                    gc.allocator,
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
    type_def: Pool(ObjTypeDef).Idx,

    cases: []Value,

    pub fn init(def: Pool(ObjTypeDef).Idx) Self {
        return Self{
            .type_def = def,
            .cases = undefined,
        };
    }

    pub fn mark(self: *Self, gc: *GC) !void {
        try gc.markObj(ObjTypeDef, self.type_def);
        for (self.cases) |case| {
            try gc.markValue(case);
        }
    }

    pub fn rawNext(self_idx: Pool(Self).Idx, vm: *VM, enum_case: ?Pool(ObjEnumInstance).Idx) !?Pool(ObjEnumInstance).Idx {
        const self = self_idx.get(vm.gc);

        if (enum_case) |case_idx| {
            const case = case_idx.get(vm.gc);

            assert(case.enum_ref.index == self_idx.index);

            if (case.case == self.cases.len - 1) {
                return null;
            }

            return try vm.gc.allocateObject(
                ObjEnumInstance{
                    .enum_ref = self_idx,
                    .case = @as(u8, @intCast(case.case + 1)),
                },
            );
        } else {
            return try vm.gc.allocateObject(
                ObjEnumInstance{
                    .enum_ref = self_idx,
                    .case = 0,
                },
            );
        }
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub const EnumDef = struct {
        const EnumDefSelf = @This();

        name: Pool(ObjString).Idx,
        qualified_name: Pool(ObjString).Idx,
        location: Ast.TokenIndex,
        enum_type: Pool(ObjTypeDef).Idx,
        cases: [][]const u8,
        cases_locations: []Ast.TokenIndex,
        // Circular reference but needed so that we can generate enum case at compile time
        value: ?Pool(ObjEnum).Idx = null,

        pub fn mark(self: *EnumDefSelf, gc: *GC) !void {
            try gc.markObj(ObjString, self.name);
            try gc.markObj(ObjString, self.qualified_name);
            try gc.markObj(ObjTypeDef, self.enum_type);
        }
    };
};

pub const ObjEnumInstance = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .EnumInstance },

    enum_ref: Pool(ObjEnum).Idx,
    case: u24,

    pub fn mark(self: *Self, gc: *GC) !void {
        try gc.markObj(ObjEnum, self.enum_ref);
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
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
    closure: ?Pool(ObjClosure).Idx = null,
    native: ?Pool(ObjNative).Idx = null,

    pub fn mark(self: *Self, gc: *GC) !void {
        try gc.markValue(self.receiver);
        if (self.closure) |closure| {
            try gc.markObj(ObjClosure, closure);
        }
        if (self.native) |native| {
            try gc.markObj(ObjNative, native);
        }
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
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
        of: Pool(ObjTypeDef).Idx,
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

    pub fn mark(self: *Self, gc: *GC) !void {
        if (self.resolved_type) |*resolved| {
            switch (self.def_type) {
                .ObjectInstance => try gc.markObj(ObjTypeDef, resolved.ObjectInstance.of),
                .EnumInstance => try gc.markObj(ObjTypeDef, resolved.EnumInstance.of),
                .Object => try resolved.Object.mark(gc),
                .Protocol => try resolved.Protocol.mark(gc),
                .Enum => try resolved.Enum.mark(gc),
                .Function => try resolved.Function.mark(gc),
                .List => try resolved.List.mark(gc),
                .Map => try resolved.Map.mark(gc),
                .Fiber => try resolved.Fiber.mark(gc),
                .ForeignContainer => try resolved.ForeignContainer.mark(gc),
                else => {},
            }
        }
    }

    pub fn populateGenerics(
        self_idx: Pool(Self).Idx,
        where: Ast.TokenIndex,
        origin: ?usize,
        generics: []Pool(Self).Idx,
        type_registry: *TypeRegistry,
        visited: ?*std.AutoHashMapUnmanaged(*Self, void),
    ) !Pool(Self).Idx {
        const self = self_idx.get(type_registry.gc);
        var visited_nodes = if (visited == null)
            std.AutoHashMapUnmanaged(*Self, void).empty
        else
            null;
        defer {
            if (visited == null) {
                visited_nodes.?.deinit(type_registry.gc.allocator);
            }
        }

        var visited_ptr = visited orelse &visited_nodes.?;

        if (visited_ptr.get(self) != null) {
            return self_idx;
        }

        try visited_ptr.put(type_registry.gc.allocator, self, {});

        if (generics.len == 0) {
            return self_idx;
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
            => self_idx,

            .Placeholder => placeholder: {
                var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                    .Placeholder = PlaceholderDef.init(
                        where,
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
                    type_registry.gc,
                    self_idx,
                    placeholder,
                    .GenericResolve,
                );

                break :placeholder placeholder;
            },

            .Generic => if (self.resolved_type.?.Generic.origin == origin and generics.len > self.resolved_type.?.Generic.index)
                generics[self.resolved_type.?.Generic.index]
            else
                self_idx,

            .Fiber => fiber: {
                const new_fiber_def = ObjFiber.FiberDef{
                    .return_type = try ObjTypeDef.populateGenerics(
                        self.resolved_type.?.Fiber.return_type,
                        where,
                        origin,
                        generics,
                        type_registry,
                        visited_ptr,
                    ),
                    .yield_type = try ObjTypeDef.cloneOptional(
                        (try ObjTypeDef.populateGenerics(
                            self.resolved_type.?.Fiber.yield_type,
                            where,
                            origin,
                            generics,
                            type_registry,
                            visited_ptr,
                        )),
                        type_registry,
                    ),
                };

                const resolved = ObjTypeDef.TypeUnion{ .Fiber = new_fiber_def };

                const new_fiber = ObjTypeDef{
                    .def_type = .Fiber,
                    .optional = self.optional,
                    .resolved_type = resolved,
                };

                break :fiber try type_registry.getTypeDef(new_fiber);
            },
            .ObjectInstance => try ObjTypeDef.toInstance(
                (try ObjTypeDef.populateGenerics(
                    self.resolved_type.?.ObjectInstance.of,
                    where,
                    origin,
                    generics,
                    type_registry,
                    visited_ptr,
                )),
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

                if (resolved.generic_types.count() == generics.len) {
                    resolved.resolved_generics = generics;
                }

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
                                .type_def = try ObjTypeDef.populateGenerics(
                                    kv.value_ptr.*.type_def,
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
                            .type_def = try ObjTypeDef.populateGenerics(
                                kv.value_ptr.*.type_def,
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
                                .item_type = try ObjTypeDef.toInstance(
                                    (try ObjTypeDef.populateGenerics(
                                        old_list_def.item_type,
                                        where,
                                        origin,
                                        generics,
                                        type_registry,
                                        visited_ptr,
                                    )),
                                    type_registry,
                                    old_list_def.item_type.get(type_registry.gc).isMutable(),
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
                            .type_def = try ObjTypeDef.populateGenerics(
                                kv.value_ptr.*.type_def,
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
                                .key_type = try ObjTypeDef.populateGenerics(
                                    old_map_def.key_type,
                                    where,
                                    origin,
                                    generics,
                                    type_registry,
                                    visited_ptr,
                                ),
                                .value_type = try ObjTypeDef.populateGenerics(
                                    old_map_def.value_type,
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

                var error_types: ?std.ArrayList(Pool(ObjTypeDef).Idx) = null;
                if (old_fun_def.error_types) |old_error_types| {
                    error_types = .empty;
                    for (old_error_types) |error_type| {
                        try error_types.?.append(
                            type_registry.gc.allocator,
                            try ObjTypeDef.populateGenerics(
                                error_type,
                                where,
                                origin,
                                generics,
                                type_registry,
                                visited_ptr,
                            ),
                        );
                    }
                }

                var parameters = std.AutoArrayHashMapUnmanaged(Pool(ObjString).Idx, Pool(ObjTypeDef).Idx).empty;
                {
                    var it = old_fun_def.parameters.iterator();
                    while (it.next()) |kv| {
                        try parameters.put(
                            type_registry.gc.allocator,
                            kv.key_ptr.*,
                            try ObjTypeDef.toInstance(
                                (try ObjTypeDef.populateGenerics(
                                    kv.value_ptr.*,
                                    where,
                                    origin,
                                    generics,
                                    type_registry,
                                    visited_ptr,
                                )),
                                type_registry,
                                kv.value_ptr.*.get(type_registry.gc).isMutable(),
                            ),
                        );
                    }
                }

                const new_fun_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .name = old_fun_def.name,
                    .script_name = old_fun_def.script_name,
                    .return_type = try ObjTypeDef
                        .toInstance(
                        (try ObjTypeDef.populateGenerics(
                            old_fun_def.return_type,
                            where,
                            origin,
                            generics,
                            type_registry,
                            visited_ptr,
                        )),
                        type_registry,
                        old_fun_def.return_type.get(type_registry.gc).isMutable(),
                    ),
                    .yield_type = try ObjTypeDef
                        .cloneOptional(
                        try ObjTypeDef
                            .toInstance(
                            try ObjTypeDef.populateGenerics(
                                old_fun_def.yield_type,
                                where,
                                origin,
                                generics,
                                type_registry,
                                visited_ptr,
                            ),
                            type_registry,
                            old_fun_def.yield_type.get(type_registry.gc).isMutable(),
                        ),
                        type_registry,
                    ),
                    .error_types = if (error_types) |*types| try types.toOwnedSlice(type_registry.gc.allocator) else null,
                    .parameters = parameters,
                    .defaults = old_fun_def.defaults,
                    .function_type = old_fun_def.function_type,
                    .lambda = old_fun_def.lambda,
                    .generic_types = try old_fun_def.generic_types.clone(type_registry.gc.allocator),
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

    pub fn cloneOptional(self_idx: Pool(Self).Idx, type_registry: *TypeRegistry) !Pool(ObjTypeDef).Idx {
        const self = self_idx.get(type_registry.gc);
        // If already optional return itself
        if ((self.optional or self.def_type == .Any) and self.def_type != .Placeholder) {
            return try type_registry.getTypeDef(self.*);
        }

        const optional_idx = try type_registry.getTypeDef(self.rawCloneOptional());
        const optional = optional_idx.get(type_registry.gc);

        if (self.def_type == .Placeholder) {
            // Destroyed copied placeholder link
            optional.resolved_type.?.Placeholder.parent = null;
            optional.resolved_type.?.Placeholder.parent_relation = null;
            optional.resolved_type.?.Placeholder.children = .empty;

            // Make actual link
            try PlaceholderDef.link(
                type_registry.gc,
                self_idx,
                optional_idx,
                .Optional,
            );
        }

        return optional_idx;
    }

    pub fn cloneNonOptional(self_idx: Pool(Self).Idx, type_registry: *TypeRegistry) !Pool(ObjTypeDef).Idx {
        const self = self_idx.get(type_registry.gc);
        // If already non optional return itself
        if (!self.optional and self.def_type != .Placeholder) {
            return try type_registry.getTypeDef(self.*);
        }

        const non_optional_idx = try type_registry.getTypeDef(self.rawCloneNonOptional());
        const non_optional = non_optional_idx.get(type_registry.gc);

        if (self.def_type == .Placeholder) {
            // Destroyed copied placeholder link
            non_optional.resolved_type.?.Placeholder.parent = null;
            non_optional.resolved_type.?.Placeholder.parent_relation = null;
            non_optional.resolved_type.?.Placeholder.children = .empty;

            // Make actual link
            try PlaceholderDef.link(
                type_registry.gc,
                self_idx,
                non_optional_idx,
                .Unwrap,
            );
        }

        return non_optional_idx;
    }

    pub fn cloneMutable(self_idx: Pool(Self).Idx, type_registry: *TypeRegistry, mutable: bool) !Pool(ObjTypeDef).Idx {
        if (self_idx.get(type_registry.gc).isMutable() == mutable) {
            return self_idx;
        }

        return switch (self_idx.get(type_registry.gc).def_type) {
            .Placeholder => placeholder: {
                var clone = self_idx.get(type_registry.gc).*;

                clone.resolved_type.?.Placeholder.mutable = mutable;

                const placeholder_idx = try type_registry.getTypeDef(clone);
                const placeholder = placeholder_idx.get(type_registry.gc);

                // Link won't be made if parent already exists
                placeholder.resolved_type.?.Placeholder.parent = null;
                if (clone.resolved_type.?.Placeholder.parent) |parent| {
                    try PlaceholderDef.link(
                        type_registry.gc,
                        parent,
                        placeholder_idx,
                        clone.resolved_type.?.Placeholder.parent_relation.?,
                    );
                }

                // FIXME: this previous clone placeholder becomes useless?

                break :placeholder placeholder_idx;
            },
            .List => list: {
                var clone = self_idx.get(type_registry.gc).*;

                clone.resolved_type.?.List.mutable = mutable;

                break :list try type_registry.getTypeDef(clone);
            },
            .Map => map: {
                var clone = self_idx.get(type_registry.gc).*;

                clone.resolved_type.?.Map.mutable = mutable;

                break :map try type_registry.getTypeDef(clone);
            },
            .ObjectInstance => instance: {
                var clone = self_idx.get(type_registry.gc).*;

                clone.resolved_type.?.ObjectInstance.mutable = mutable;

                break :instance try type_registry.getTypeDef(clone);
            },
            .ProtocolInstance => instance: {
                var clone = self_idx.get(type_registry.gc).*;

                clone.resolved_type.?.ProtocolInstance.mutable = mutable;

                break :instance try type_registry.getTypeDef(clone);
            },
            .EnumInstance => instance: {
                var clone = self_idx.get(type_registry.gc).*;

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
            else => self_idx,
        };
    }

    pub fn deinit(_: *Self) void {
        // FIXME
    }

    pub fn toStringAlloc(self_idx: Pool(Self).Idx, gc: *GC, qualified: bool) (Allocator.Error || std.fmt.BufPrintError || error{WriteFailed})![]const u8 {
        var str = std.Io.Writer.Allocating.init(gc.allocator);

        try Self.toString(
            self_idx,
            gc,
            &str.writer,
            qualified,
        );

        return try str.toOwnedSlice();
    }

    pub fn toString(self_idx: Pool(Self).Idx, gc: *GC, writer: anytype, qualified: bool) (Allocator.Error || std.fmt.BufPrintError || error{WriteFailed})!void {
        try Self.toStringRaw(self_idx, gc, writer, qualified);
    }

    pub fn toStringUnqualified(self_idx: Pool(Self).Idx, gc: *GC, writer: anytype) (Allocator.Error || std.fmt.BufPrintError || error{WriteFailed})!void {
        try Self.toStringRaw(self_idx, writer, gc, false);
    }

    fn toStringRaw(self_idx: Pool(Self).Idx, gc: *GC, writer: anytype, qualified: bool) (Allocator.Error || std.fmt.BufPrintError || error{WriteFailed})!void {
        const self = self_idx.get(gc);
        switch (self.def_type) {
            .Generic => try writer.print(
                "generic type #{}-{}",
                .{
                    self.resolved_type.?.Generic.origin,
                    self.resolved_type.?.Generic.index,
                },
            ),
            .UserData => try writer.writeAll("ud"),
            .Bool => try writer.writeAll("bool"),
            .Integer => try writer.writeAll("int"),
            .Double => try writer.writeAll("double"),
            .String => try writer.writeAll("str"),
            .Pattern => try writer.writeAll("pat"),
            .Any => {
                if (self.resolved_type.?.Any) {
                    try writer.writeAll("mut ");
                }
                try writer.writeAll("any");
            },
            .Range => try writer.writeAll("rg"),
            .Fiber => {
                try writer.writeAll("fib<");
                try ObjTypeDef.toStringRaw(
                    self.resolved_type.?.Fiber.return_type,
                    gc,
                    writer,
                    qualified,
                );
                try writer.writeAll(", ");
                try ObjTypeDef.toStringRaw(
                    self.resolved_type.?.Fiber.yield_type,
                    gc,
                    writer,
                    qualified,
                );
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
                        try ObjTypeDef.toStringRaw(
                            kv.value_ptr.*.type_def,
                            gc,
                            writer,
                            qualified,
                        );

                        if (i < count - 1) {
                            try writer.writeAll(", ");
                        }

                        i += 1;
                    }
                    try writer.writeAll(" }");
                } else {
                    try writer.writeAll("object ");
                    if (qualified) {
                        try writer.writeAll(
                            object_def.qualified_name.get(gc).string,
                        );
                    } else {
                        try writer.writeAll(
                            object_def.name.get(gc).string,
                        );
                    }
                }

                const size = object_def.generic_types.count();
                if (size > 0) {
                    try writer.writeAll("::<");
                    var i: usize = 0;
                    var it = object_def.generic_types.iterator();
                    while (it.next()) |kv| : (i = i + 1) {
                        if (object_def.resolved_generics != null and i < object_def.resolved_generics.?.len) {
                            try ObjTypeDef.toStringRaw(
                                object_def.resolved_generics.?[i],
                                gc,
                                writer,
                                qualified,
                            );
                        } else {
                            try writer.print(
                                "{s}",
                                .{
                                    kv.key_ptr.*.get(gc).string,
                                },
                            );
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
                    try writer.print(
                        "protocol {s}",
                        .{
                            protocol_def.qualified_name.get(gc).string,
                        },
                    );
                } else {
                    try writer.print(
                        "protocol {s}",
                        .{
                            protocol_def.name.get(gc).string,
                        },
                    );
                }
            },
            .Enum => {
                try writer.writeAll("enum ");
                if (qualified) {
                    try writer.writeAll(
                        self.resolved_type.?.Enum.qualified_name.get(gc).string,
                    );
                } else {
                    try writer.writeAll(
                        self.resolved_type.?.Enum.name.get(gc).string,
                    );
                }
            },

            .ObjectInstance => {
                const object_def = self.resolved_type.?.ObjectInstance.of.get(gc)
                    .resolved_type.?.Object;

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
                        try ObjTypeDef.toStringRaw(
                            kv.value_ptr.*.type_def,
                            gc,
                            writer,
                            qualified,
                        );

                        if (i < count - 1) {
                            try writer.writeAll(", ");
                        }

                        i += 1;
                    }
                    try writer.writeAll(" }");
                } else {
                    if (qualified) {
                        try writer.writeAll(
                            object_def.qualified_name.get(gc).string,
                        );
                    } else {
                        try writer.writeAll(
                            object_def.name.get(gc).string,
                        );
                    }
                }

                const size = object_def.generic_types.count();
                if (size > 0) {
                    try writer.writeAll("::<");
                    var i: usize = 0;
                    var it = object_def.generic_types.iterator();
                    while (it.next()) |kv| : (i += 1) {
                        if (object_def.resolved_generics != null and i < object_def.resolved_generics.?.len) {
                            try ObjTypeDef.toStringRaw(
                                object_def.resolved_generics.?[i],
                                gc,
                                writer,
                                qualified,
                            );
                        } else {
                            try writer.print(
                                "{s}",
                                .{
                                    kv.key_ptr.*.get(gc).string,
                                },
                            );
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
                    try writer.writeAll(
                        self.resolved_type.?.ForeignContainer.qualified_name.get(gc).string,
                    );
                } else {
                    try writer.writeAll(
                        self.resolved_type.?.ForeignContainer.name.get(gc).string,
                    );
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

                const protocol_def = self.resolved_type.?.ProtocolInstance.of.get(gc)
                    .resolved_type.?.Protocol;

                if (qualified) {
                    try writer.writeAll(protocol_def.qualified_name.get(gc).string);
                } else {
                    try writer.writeAll(protocol_def.name.get(gc).string);
                }
            },
            .EnumInstance => {
                if (qualified) {
                    try writer.writeAll(
                        self.resolved_type.?.EnumInstance.of.get(gc)
                            .resolved_type.?.Enum.qualified_name.get(gc).string,
                    );
                } else {
                    try writer.writeAll(
                        self.resolved_type.?.EnumInstance.of.get(gc)
                            .resolved_type.?.Enum.name.get(gc).string,
                    );
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
                try ObjTypeDef.toStringRaw(
                    self.resolved_type.?.List.item_type,
                    gc,
                    writer,
                    qualified,
                );
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
                try ObjTypeDef.toStringRaw(
                    self.resolved_type.?.Map.key_type,
                    gc,
                    writer,
                    qualified,
                );
                try writer.writeAll(": ");
                try ObjTypeDef.toStringRaw(
                    self.resolved_type.?.Map.value_type,
                    gc,
                    writer,
                    qualified,
                );
                try writer.writeAll("}");
            },
            .Function => {
                var function_def = self.resolved_type.?.Function;

                if (function_def.function_type == .Extern) {
                    try writer.writeAll("extern ");
                }
                try writer.writeAll("fun ");
                try writer.writeAll(function_def.name.get(gc).string);

                {
                    const size = function_def.generic_types.count();
                    if (size > 0) {
                        try writer.writeAll("::<");
                        var i: usize = 0;
                        var it = function_def.generic_types.iterator();
                        while (it.next()) |kv| : (i = i + 1) {
                            if (function_def.resolved_generics != null and i < function_def.resolved_generics.?.len) {
                                try ObjTypeDef.toString(
                                    function_def.resolved_generics.?[i],
                                    gc,
                                    writer,
                                    true,
                                );
                            } else {
                                try writer.print(
                                    "{s}",
                                    .{
                                        kv.key_ptr.*.get(gc).string,
                                    },
                                );
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
                        try writer.writeAll(
                            kv.key_ptr.*.get(gc).string,
                        );
                        try writer.writeAll(": ");
                        try ObjTypeDef.toStringRaw(
                            kv.value_ptr.*,
                            gc,
                            writer,
                            qualified,
                        );

                        if (i < size - 1) {
                            try writer.writeAll(", ");
                        }
                    }
                }

                try writer.writeAll(")");

                if (function_def.yield_type.get(gc).def_type != .Void) {
                    try writer.writeAll(" > ");
                    try ObjTypeDef.toStringRaw(
                        function_def.yield_type,
                        gc,
                        writer,
                        qualified,
                    );
                }

                try writer.writeAll(" > ");
                try ObjTypeDef.toStringRaw(
                    function_def.return_type,
                    gc,
                    writer,
                    qualified,
                );

                if (function_def.error_types != null and function_def.error_types.?.len > 0) {
                    try writer.writeAll(" !> ");
                    for (function_def.error_types.?, 0..) |error_type, index| {
                        try ObjTypeDef.toStringRaw(
                            error_type,
                            gc,
                            writer,
                            qualified,
                        );

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
                        self_idx.index,
                    },
                );

                if (placeholder.parent_relation) |relation| {
                    try writer.print(
                        " {s} of ",
                        .{
                            @tagName(relation),
                        },
                    );
                    try ObjTypeDef.toStringRaw(
                        placeholder.parent.?,
                        gc,
                        writer,
                        qualified,
                    );
                }

                try writer.print("}}", .{});
            },
        }

        if (self.optional) {
            try writer.writeAll("?");
        }
    }

    pub fn toObj(self: *Self) *Obj {
        return @constCast(&self.obj);
    }

    pub fn toParentType(self_idx: Pool(Self).Idx, type_registry: *TypeRegistry) !Pool(Self).Idx {
        const self = self_idx.get(type_registry.gc);
        return switch (self.def_type) {
            .ObjectInstance => self.resolved_type.?.ObjectInstance.of,
            .ProtocolInstance => self.resolved_type.?.ProtocolInstance.of,
            .EnumInstance => self.resolved_type.?.EnumInstance.of,
            .Placeholder => placeholder: {
                if (self.resolved_type.?.Placeholder.parent_relation != null and self.resolved_type.?.Placeholder.parent_relation.? == .Instance) {
                    return self.resolved_type.?.Placeholder.parent.?;
                }

                const placeholder = try type_registry.getTypeDef(
                    .{
                        .def_type = .Placeholder,
                        .resolved_type = .{
                            .Placeholder = PlaceholderDef.init(
                                self.resolved_type.?.Placeholder.where,
                                self.resolved_type.?.Placeholder.where_end,
                                self.resolved_type.?.Placeholder.mutable,
                            ),
                        },
                    },
                );

                try PlaceholderDef.link(
                    type_registry.gc,
                    self_idx,
                    placeholder,
                    .Parent,
                );

                break :placeholder placeholder;
            },
            else => self_idx,
        };
    }

    pub fn toInstance(self_idx: Pool(Self).Idx, type_registry: *TypeRegistry, mutable: bool) !Pool(Self).Idx {
        const self = self_idx.get(type_registry.gc);

        // Avoid placeholder double links like: Object Placeholder -> Instance -> Instance
        if (self.def_type == .Placeholder and self.resolved_type.?.Placeholder.parent_relation != null and self.resolved_type.?.Placeholder.parent_relation.? == .Instance) {
            return self_idx;
        }

        const instance_type = try type_registry.getTypeDef(
            switch (self.def_type) {
                .Object => Self{
                    .optional = self.optional,
                    .def_type = .ObjectInstance,
                    .resolved_type = .{
                        .ObjectInstance = .{
                            .of = try ObjTypeDef.cloneNonOptional(self_idx, type_registry),
                            .mutable = mutable,
                        },
                    },
                },
                .Protocol => Self{
                    .optional = self.optional,
                    .def_type = .ProtocolInstance,
                    .resolved_type = .{
                        .ProtocolInstance = .{
                            .of = try ObjTypeDef.cloneNonOptional(self_idx, type_registry),
                            .mutable = mutable,
                        },
                    },
                },
                .Enum => Self{
                    .optional = self.optional,
                    .def_type = .EnumInstance,
                    .resolved_type = .{
                        .EnumInstance = .{
                            .of = try ObjTypeDef.cloneNonOptional(self_idx, type_registry),
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
                                self.resolved_type.?.Placeholder.where_end,
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

        if (self_idx.get(type_registry.gc).def_type == .Placeholder) {
            try PlaceholderDef.link(
                type_registry.gc,
                self_idx,
                instance_type,
                .Instance,
            );
        }

        return instance_type;
    }

    // Compare two type definitions
    pub fn eqlTypeUnion(expected: TypeUnion, actual: TypeUnion, gc: *GC) bool {
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
            => unreachable,

            .Any => expected.Any == actual.Any,

            .ForeignContainer => std.mem.eql(
                u8,
                expected.ForeignContainer.name.get(gc).string,
                actual.ForeignContainer.name.get(gc).string,
            ),

            .Generic => expected.Generic.origin == actual.Generic.origin and expected.Generic.index == actual.Generic.index,

            .Fiber => ObjTypeDef.eql(expected.Fiber.return_type, actual.Fiber.return_type, gc) and
                ObjTypeDef.eql(expected.Fiber.yield_type, actual.Fiber.yield_type, gc),

            .ObjectInstance => ObjTypeDef.eql(expected.ObjectInstance.of, actual.ObjectInstance.of, gc) or
                expected.ObjectInstance.of.eql(actual.ObjectInstance.of),
            .ProtocolInstance => {
                if (actual == .ProtocolInstance) {
                    return ObjTypeDef.eql(expected.ProtocolInstance.of, actual.ProtocolInstance.of, gc) or
                        expected.ProtocolInstance.of.eql(actual.ProtocolInstance.of);
                } else {
                    assert(actual == .ObjectInstance);
                    return actual.ObjectInstance.of.get(gc)
                        .resolved_type.?.Object.conforms_to.get(expected.ProtocolInstance.of) != null;
                }
            },
            .EnumInstance => ObjTypeDef.eql(
                expected.EnumInstance.of,
                actual.EnumInstance.of,
                gc,
            ),

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

            .List => ObjTypeDef.eql(expected.List.item_type, actual.List.item_type, gc),
            .Map => ObjTypeDef.eql(expected.Map.key_type, actual.Map.key_type, gc) and
                ObjTypeDef.eql(expected.Map.value_type, actual.Map.value_type, gc),
            .Function => {
                // Compare return type
                if (!ObjTypeDef.eql(expected.Function.return_type, actual.Function.return_type, gc)) {
                    return false;
                }

                // Compare yield type
                if (!ObjTypeDef.eql(expected.Function.yield_type, actual.Function.yield_type, gc)) {
                    return false;
                }

                // Compare arity
                if (expected.Function.parameters.count() != actual.Function.parameters.count()) {
                    return false;
                }

                // Compare parameters (we ignore argument names and only compare types)
                const a_keys = expected.Function.parameters.keys();
                const b_keys = actual.Function.parameters.keys();

                if (a_keys.len != b_keys.len) {
                    return false;
                }

                for (a_keys, 0..) |_, index| {
                    if (!ObjTypeDef.eql(
                        expected.Function.parameters.get(a_keys[index]).?,
                        actual.Function.parameters.get(b_keys[index]).?,
                        gc,
                    )) {
                        return false;
                    }
                }

                return true;
            },

            .Placeholder => true, // TODO: should it be false?
        };
    }

    // Compare two type definitions
    pub fn eql(expected_idx: Pool(Self).Idx, actual_idx: Pool(Self).Idx, gc: *GC) bool {
        const expected = expected_idx.get(gc);
        const actual = actual_idx.get(gc);

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
            (expected.resolved_type != null and actual.resolved_type != null and eqlTypeUnion(expected.resolved_type.?, actual.resolved_type.?, gc));

        return (type_eql or actual.def_type == .Placeholder or expected.def_type == .Placeholder) and
            (expected.optional or !actual.optional) and // A nullable slot can receive a non-null value
            (!expected.isMutable() or actual.isMutable()); // A immutable slot can receive a mutable value (the immutability is the promise that the receiver won't mutate the value)
    }

    // Strict compare two type definitions
    pub fn strictEql(expected_idx: Pool(Self).Idx, actual_idx: Pool(Self).Idx, gc: *GC) bool {
        const expected = expected_idx.get(gc);
        const actual = actual_idx.get(gc);

        const type_eql = (expected.resolved_type == null and actual.resolved_type == null and expected.def_type == actual.def_type) or
            (expected.resolved_type != null and actual.resolved_type != null and eqlTypeUnion(
                expected.resolved_type.?,
                actual.resolved_type.?,
                gc,
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

pub const PlaceholderDef = struct {
    const Self = @This();

    pub const Relation = enum {
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

    /// Where the placeholder was created
    where: Ast.TokenIndex,
    where_end: Ast.TokenIndex,
    /// When accessing/calling/subscrit/assign a placeholder we produce another. We keep them linked so we
    /// can trace back the root of the unknown type.
    parent: ?Pool(ObjTypeDef).Idx = null,
    /// What's the relation with the parent?
    parent_relation: ?Relation = null,
    /// Children adds themselves here
    children: std.ArrayList(Pool(ObjTypeDef).Idx) = .empty,
    mutable: ?bool,

    /// If the placeholder is a function return, we need to remember eventual generic types defined in that call
    resolved_generics: ?[]Pool(ObjTypeDef).Idx = null,

    pub fn init(where: Ast.TokenIndex, where_end: Ast.TokenIndex, mutable: ?bool) Self {
        return Self{
            .where = where,
            .where_end = where_end,
            .mutable = mutable,
        };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.children.deinit(allocator);
    }

    pub fn link(gc: *GC, parent_idx: Pool(ObjTypeDef).Idx, child_idx: Pool(ObjTypeDef).Idx, relation: Relation) !void {
        const parent = parent_idx.get(gc);
        const child = child_idx.get(gc);

        assert(parent.def_type == .Placeholder);
        assert(child.def_type == .Placeholder);

        if (parent_idx.index == child_idx.index) {
            return;
        }

        if (child.resolved_type.?.Placeholder.parent != null) {
            if (BuildOptions.debug_placeholders) {
                io.print(
                    ">>> Placeholder @{} has already a {} relation with @{}\n",
                    .{
                        child_idx.index,
                        child.resolved_type.?.Placeholder.parent_relation.?,
                        child.resolved_type.?.Placeholder.parent.?.index,
                    },
                );
            }
            return;
        }

        child.resolved_type.?.Placeholder.parent = parent_idx;
        try parent.resolved_type.?.Placeholder.children.append(gc.allocator, child_idx);
        child.resolved_type.?.Placeholder.parent_relation = relation;

        if (BuildOptions.debug_placeholders) {
            io.print(
                "Linking @{} (root: {}) with @{} as {s}\n",
                .{
                    parent_idx.index,
                    parent.resolved_type.?.Placeholder.parent == null,
                    child_idx.index,
                    @tagName(relation),
                },
            );
        }
    }
};
