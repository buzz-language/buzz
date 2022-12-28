const std = @import("std");
const builtin = @import("builtin");
const VM = @import("./vm.zig").VM;
const _obj = @import("./obj.zig");
const _value = @import("./value.zig");
const memory = @import("./memory.zig");
const _parser = @import("./parser.zig");
const _codegen = @import("./codegen.zig");
const BuildOptions = @import("build_options");

const Value = _value.Value;
const valueToString = _value.valueToString;
const valueToStringAlloc = _value.valueToStringAlloc;
const ObjString = _obj.ObjString;
const ObjPattern = _obj.ObjPattern;
const ObjMap = _obj.ObjMap;
const ObjUpValue = _obj.ObjUpValue;
const ObjEnum = _obj.ObjEnum;
const ObjEnumInstance = _obj.ObjEnumInstance;
const ObjObject = _obj.ObjObject;
const ObjObjectInstance = _obj.ObjObjectInstance;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjFunction = _obj.ObjFunction;
const ObjList = _obj.ObjList;
const ObjUserData = _obj.ObjUserData;
const ObjClosure = _obj.ObjClosure;
const UserData = _obj.UserData;
const TypeRegistry = memory.TypeRegistry;
const Parser = _parser.Parser;
const CodeGen = _codegen.CodeGen;
const GarbageCollector = memory.GarbageCollector;

var gpa = std.heap.GeneralPurposeAllocator(.{
    .safety = true,
}){};

var allocator: std.mem.Allocator = if (builtin.mode == .Debug)
    gpa.allocator()
else if (BuildOptions.use_mimalloc)
    @import("./mimalloc.zig").mim_allocator
else
    std.heap.c_allocator;

// Stack manipulation

/// Push a Value to the stack
export fn bz_push(self: *VM, value: *Value) void {
    self.push(value.*);
}

/// Pop a Value from the stack and returns it
export fn bz_pop(self: *VM) *Value {
    self.current_fiber.stack_top -= 1;
    return @ptrCast(*Value, self.current_fiber.stack_top);
}

/// Peeks at the stack at [distance] from the stack top
export fn bz_peek(self: *VM, distance: u32) *Value {
    return @ptrCast(*Value, self.current_fiber.stack_top - 1 - distance);
}

// Value manipulations

/// Push a boolean value on the stack
export fn bz_pushBool(self: *VM, value: bool) void {
    self.push(Value{ .Boolean = value });
}

/// Push a float value on the stack
export fn bz_pushFloat(self: *VM, value: f64) void {
    self.push(Value{ .Float = value });
}

/// Push a integer value on the stack
export fn bz_pushInteger(self: *VM, value: i64) void {
    self.push(Value{ .Integer = value });
}

/// Push null on the stack
export fn bz_pushNull(self: *VM) void {
    self.push(Value{ .Null = {} });
}

/// Push void on the stack
export fn bz_pushVoid(self: *VM) void {
    self.push(Value{ .Void = {} });
}

/// Push string on the stack
export fn bz_pushString(self: *VM, value: *ObjString) void {
    self.push(value.toValue());
}

/// Push list on the stack
export fn bz_pushList(self: *VM, value: *ObjList) void {
    self.push(value.toValue());
}

/// Push a uesrdata value on the stack
export fn bz_pushUserData(self: *VM, value: *ObjUserData) void {
    self.push(value.toValue());
}

/// Converts a value to a boolean
export fn bz_valueToBool(value: *Value) bool {
    return value.Boolean;
}

/// Converts a value to a string
export fn bz_valueToString(value: *Value, len: *usize) ?[*]const u8 {
    if (value.* != .Obj or value.Obj.obj_type != .String) {
        return null;
    }

    const string = ObjString.cast(value.Obj).?.string;

    len.* = string.len;

    return if (string.len > 0) if (string.len > 0) @ptrCast([*]const u8, string) else null else null;
}

/// Dump value
export fn bz_valueDump(value_ptr: *const Value, vm: *VM) void {
    const value = value_ptr.*;

    switch (value) {
        .Boolean,
        .Float,
        .Integer,
        .Null,
        .Void,
        => {
            const string = valueToStringAlloc(vm.gc.allocator, value) catch "";
            defer vm.gc.allocator.free(string);

            std.debug.print("{s}", .{string});
        },

        .Obj => {
            switch (value.Obj.obj_type) {
                .Type,
                .Closure,
                .Function,
                .Bound,
                .Native,
                .UserData,
                .Fiber,
                .EnumInstance,
                => {
                    const string = valueToStringAlloc(vm.gc.allocator, value) catch "";
                    defer vm.gc.allocator.free(string);

                    std.debug.print("{s}", .{string});
                },

                .UpValue => {
                    const upvalue = ObjUpValue.cast(value.Obj).?;

                    bz_valueDump(if (upvalue.closed != null) &upvalue.closed.? else upvalue.location, vm);
                },

                .String => {
                    const string = ObjString.cast(value.Obj).?;

                    std.debug.print("\"{s}\"", .{string.string});
                },

                .Pattern => {
                    const pattern = ObjPattern.cast(value.Obj).?;

                    std.debug.print("_{s}_", .{pattern.source});
                },

                .List => {
                    const list = ObjList.cast(value.Obj).?;

                    std.debug.print("[ ", .{});
                    for (list.items.items) |item| {
                        bz_valueDump(&item, vm);
                        std.debug.print(", ", .{});
                    }
                    std.debug.print("]", .{});
                },

                .Map => {
                    const map = ObjMap.cast(value.Obj).?;

                    std.debug.print("{{ ", .{});
                    var it = map.map.iterator();
                    while (it.next()) |kv| {
                        const key = _value.hashableToValue(kv.key_ptr.*);

                        bz_valueDump(&key, vm);
                        std.debug.print(": ", .{});
                        bz_valueDump(kv.value_ptr, vm);
                        std.debug.print(", ", .{});
                    }
                    std.debug.print("}}", .{});
                },

                .Enum => {
                    const enumeration = ObjEnum.cast(value.Obj).?;
                    const enum_type_def = enumeration.type_def.resolved_type.?.Enum;

                    std.debug.print("enum({s}) {s} {{ ", .{ enum_type_def.name.string, enumeration.name.string });
                    for (enum_type_def.cases.items) |case, i| {
                        std.debug.print("{s} -> ", .{case});
                        bz_valueDump(&enumeration.cases.items[i], vm);
                        std.debug.print(", ", .{});
                    }
                    std.debug.print("}}", .{});
                },

                .Object => {
                    const object = ObjObject.cast(value.Obj).?;
                    const object_def = object.type_def.resolved_type.?.Object;

                    std.debug.print("object", .{});
                    if (object_def.conforms_to.count() > 0) {
                        std.debug.print("(", .{});
                        var it = object_def.conforms_to.iterator();
                        while (it.next()) |kv| {
                            std.debug.print("{s}, ", .{kv.key_ptr.*.resolved_type.?.Protocol.name.string});
                        }
                        std.debug.print(")", .{});
                    }

                    std.debug.print("{s} {{ ", .{object_def.name.string});

                    var it = object_def.static_fields.iterator();
                    while (it.next()) |kv| {
                        const static_field_type_str = kv.value_ptr.*.toStringAlloc(vm.gc.allocator) catch "";
                        defer vm.gc.allocator.free(static_field_type_str);

                        std.debug.print("static {s} {s}", .{ static_field_type_str, kv.key_ptr.* });

                        var static_it = object.static_fields.iterator();
                        while (static_it.next()) |static_kv| {
                            if (std.mem.eql(u8, static_kv.key_ptr.*.string, kv.key_ptr.*)) {
                                std.debug.print(" = ", .{});
                                bz_valueDump(&static_kv.value_ptr.*, vm);
                                break;
                            }
                        }

                        std.debug.print(", ", .{});
                    }

                    it = object_def.fields.iterator();
                    while (it.next()) |kv| {
                        const field_type_str = kv.value_ptr.*.toStringAlloc(vm.gc.allocator) catch "";
                        defer vm.gc.allocator.free(field_type_str);

                        std.debug.print("{s} {s}", .{ field_type_str, kv.key_ptr.* });

                        var field_it = object.fields.iterator();
                        while (field_it.next()) |field_kv| {
                            if (std.mem.eql(u8, field_kv.key_ptr.*.string, kv.key_ptr.*)) {
                                std.debug.print(" = ", .{});
                                bz_valueDump(&field_kv.value_ptr.*, vm);
                                break;
                            }
                        }

                        std.debug.print(", ", .{});
                    }

                    it = object_def.methods.iterator();
                    while (it.next()) |kv| {
                        const method_type_str = kv.value_ptr.*.toStringAlloc(vm.gc.allocator) catch "";
                        defer vm.gc.allocator.free(method_type_str);

                        std.debug.print("{s}, ", .{method_type_str});
                    }

                    std.debug.print("}}", .{});
                },

                .ObjectInstance => {
                    const object_instance = ObjObjectInstance.cast(value.Obj).?;

                    std.debug.print("{s}{{ ", .{if (object_instance.object) |object| object.type_def.resolved_type.?.Object.name.string else "."});
                    var it = object_instance.fields.iterator();
                    while (it.next()) |kv| {
                        std.debug.print("{s} = ", .{kv.key_ptr.*.string});
                        bz_valueDump(kv.value_ptr, vm);
                        std.debug.print(", ", .{});
                    }
                    std.debug.print("}}", .{});
                },
            }
        },
    }
}

/// Converts a value to a float
export fn bz_valueToFloat(value: *Value) f64 {
    return if (value.* == .Integer) @intToFloat(f64, value.Integer) else value.Float;
}

/// Converts a value to a integer, returns null if float value with decimal part
export fn bz_valueToInteger(value: *Value) i64 {
    return if (value.* == .Integer) value.Integer else @floatToInt(i64, value.Float);
}

export fn bz_valueToUserData(value: *Value) *UserData {
    return ObjUserData.cast(value.Obj).?.userdata;
}

export fn bz_valueIsInteger(value: *Value) bool {
    return value.* == .Integer;
}
export fn bz_valueIsFloat(value: *Value) bool {
    return value.* == .Float;
}

// Obj manipulations

/// Converts a c string to a *ObjString
export fn bz_string(vm: *VM, string: ?[*]const u8, len: usize) ?*ObjString {
    return (if (string) |ustring| vm.gc.copyString(ustring[0..len]) else vm.gc.copyString("")) catch null;
}

// Other stuff

// Type helpers

// TODO: should always return the same instance
/// Returns the [bool] type
export fn bz_boolType() ?*ObjTypeDef {
    var bool_type: ?*ObjTypeDef = allocator.create(ObjTypeDef) catch null;

    if (bool_type == null) {
        return null;
    }

    bool_type.?.* = ObjTypeDef{ .def_type = .Bool, .optional = false };

    return bool_type;
}

/// Returns the [str] type
export fn bz_stringType() ?*ObjTypeDef {
    var bool_type: ?*ObjTypeDef = allocator.create(ObjTypeDef) catch null;

    if (bool_type == null) {
        return null;
    }

    bool_type.?.* = ObjTypeDef{ .def_type = .String, .optional = false };

    return bool_type;
}

/// Returns the [void] type
export fn bz_voidType() ?*ObjTypeDef {
    var void_type: ?*ObjTypeDef = allocator.create(ObjTypeDef) catch null;

    if (void_type == null) {
        return null;
    }

    void_type.?.* = ObjTypeDef{ .def_type = .Void, .optional = false };

    return void_type;
}

export fn bz_allocated(self: *VM) usize {
    return self.gc.bytes_allocated;
}

export fn bz_collect(self: *VM) bool {
    self.gc.collectGarbage() catch {
        return false;
    };

    return true;
}

export fn bz_newList(vm: *VM, of_type: *ObjTypeDef) ?*ObjList {
    var list_def: ObjList.ListDef = ObjList.ListDef.init(
        vm.gc.allocator,
        of_type,
    );

    var list_def_union: ObjTypeDef.TypeUnion = .{
        .List = list_def,
    };

    var list_def_type: *ObjTypeDef = vm.gc.type_registry.getTypeDef(ObjTypeDef{
        .def_type = .List,
        .optional = false,
        .resolved_type = list_def_union,
    }) catch {
        return null;
    };

    return vm.gc.allocateObject(
        ObjList,
        ObjList.init(vm.gc.allocator, list_def_type),
    ) catch {
        return null;
    };
}

export fn bz_listAppend(self: *ObjList, gc: *GarbageCollector, value: *Value) bool {
    self.rawAppend(gc, value.*) catch {
        return false;
    };

    return true;
}

export fn bz_valueToList(value: *Value) *ObjList {
    return ObjList.cast(value.Obj).?;
}

export fn bz_listGet(self: *ObjList, index: usize) *Value {
    return &self.items.items[index];
}

export fn bz_listLen(self: *ObjList) usize {
    return self.items.items.len;
}

export fn bz_newUserData(vm: *VM, userdata: *UserData) ?*ObjUserData {
    return vm.gc.allocateObject(
        ObjUserData,
        ObjUserData{ .userdata = userdata },
    ) catch {
        return null;
    };
}

export fn bz_getUserData(userdata: *ObjUserData) *UserData {
    return userdata.userdata;
}

export fn bz_throw(vm: *VM, value: *Value) void {
    vm.push(value.*);
}

export fn bz_throwString(vm: *VM, message: ?[*]const u8, len: usize) void {
    bz_pushString(vm, bz_string(vm, message.?, len) orelse {
        _ = std.io.getStdErr().write((message.?)[0..len]) catch unreachable;
        std.os.exit(1);
    });
}

export fn bz_newVM(self: *VM) ?*VM {
    var vm = self.gc.allocator.create(VM) catch {
        return null;
    };
    var gc = self.gc.allocator.create(GarbageCollector) catch {
        return null;
    };
    // FIXME: should share strings between gc
    gc.* = GarbageCollector.init(self.gc.allocator);
    gc.type_registry = TypeRegistry{
        .gc = gc,
        .registry = std.StringHashMap(*ObjTypeDef).init(self.gc.allocator),
    };

    vm.* = VM.init(gc, self.import_registry) catch {
        return null;
    };

    return vm;
}

export fn bz_deinitVM(_: *VM) void {
    // self.deinit();
}

export fn bz_getGC(vm: *VM) *memory.GarbageCollector {
    return vm.gc;
}

export fn bz_compile(self: *VM, source: ?[*]const u8, source_len: usize, file_name: ?[*]const u8, file_name_len: usize) ?*ObjFunction {
    if (source == null or file_name_len == 0 or source_len == 0 or file_name_len == 0) {
        return null;
    }

    var imports = std.StringHashMap(Parser.ScriptImport).init(self.gc.allocator);
    var strings = std.StringHashMap(*ObjString).init(self.gc.allocator);
    var parser = Parser.init(self.gc, &imports, false);
    var codegen = CodeGen.init(self.gc, &parser, false);
    defer {
        codegen.deinit();
        imports.deinit();
        parser.deinit();
        strings.deinit();
        // FIXME: fails
        // gc.deinit();
        // self.gc.allocator.destroy(self.gc);
    }

    if (parser.parse(source.?[0..source_len], file_name.?[0..file_name_len]) catch null) |function_node| {
        return function_node.toByteCode(function_node, &codegen, null) catch null;
    } else {
        return null;
    }
}

export fn bz_interpret(self: *VM, function: *ObjFunction) bool {
    self.interpret(function, null) catch {
        return false;
    };

    return true;
}

pub export fn bz_call(self: *VM, closure: *ObjClosure, arguments: [*]const *const Value, len: u8, catch_value: ?*Value) void {
    self.push(closure.toValue());
    var i: usize = 0;
    while (i < len) : (i += 1) {
        self.push(arguments[i].*);
    }

    // TODO: catch properly
    self.callValue(closure.toValue(), len, if (catch_value) |v| v.* else null) catch unreachable;

    self.run();
}

// Assumes the global exists
export fn bz_pushError(self: *VM, qualified_name: [*]const u8, len: usize) void {
    const object = bz_getQualified(self, qualified_name, len).?;

    self.push(
        // Dismiss error because if we fail to create the error payload there's not much to salvage anyway
        (self.gc.allocateObject(
            ObjObjectInstance,
            ObjObjectInstance.init(self.gc.allocator, ObjObject.cast(object.Obj).?, null),
        ) catch unreachable).toValue(),
    );
}

export fn bz_pushErrorEnum(self: *VM, qualified_name: [*]const u8, name_len: usize, case: [*]const u8, case_len: usize) void {
    const enum_set = ObjEnum.cast(bz_getQualified(self, qualified_name, name_len).?.Obj).?;

    self.push(
        bz_getEnumCase(enum_set, self, case, case_len).?.toValue(),
    );
}

export fn bz_getQualified(self: *VM, qualified_name: [*]const u8, len: usize) ?*Value {
    for (self.globals.items) |*global| {
        if (global.* == .Obj) {
            switch (global.Obj.obj_type) {
                .Enum => {
                    const obj_enum = ObjEnum.cast(global.Obj).?;

                    if (std.mem.eql(u8, qualified_name[0..len], obj_enum.type_def.resolved_type.?.Enum.qualified_name.string)) {
                        return global;
                    }
                },
                .Object => {
                    const obj_enum = ObjObject.cast(global.Obj).?;

                    if (std.mem.eql(u8, qualified_name[0..len], obj_enum.type_def.resolved_type.?.Object.qualified_name.string)) {
                        return global;
                    }
                },
                else => {},
            }
        }
    }

    return null;
}

export fn bz_instance(self: *ObjObject, vm: *VM) ?*ObjObjectInstance {
    return vm.gc.allocateObject(
        ObjObjectInstance,
        ObjObjectInstance.init(vm.gc.allocator, self, null),
    ) catch null;
}

export fn bz_valueToObject(value: *Value) *ObjObject {
    return ObjObject.cast(value.Obj).?;
}

export fn bz_pushObjectInstance(vm: *VM, payload: *ObjObjectInstance) void {
    vm.push(payload.toValue());
}

export fn bz_getEnumCase(self: *ObjEnum, vm: *VM, case: [*]const u8, len: usize) ?*ObjEnumInstance {
    var case_index: usize = 0;

    for (self.type_def.resolved_type.?.Enum.cases.items) |enum_case, index| {
        if (std.mem.eql(u8, case[0..len], enum_case)) {
            case_index = index;
            break;
        }
    }

    return vm.gc.allocateObject(
        ObjEnumInstance,
        ObjEnumInstance{
            .enum_ref = self,
            .case = @intCast(u8, case_index),
        },
    ) catch null;
}

export fn bz_pushEnumInstance(vm: *VM, payload: *ObjEnumInstance) void {
    vm.push(payload.toValue());
}
