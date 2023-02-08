const std = @import("std");
const builtin = @import("builtin");
const buzz_builtin = @import("./builtin.zig");
const _vm = @import("./vm.zig");
const VM = _vm.VM;
const TryCtx = _vm.TryCtx;
const _obj = @import("./obj.zig");
const _value = @import("./value.zig");
const memory = @import("./memory.zig");
const _parser = @import("./parser.zig");
const _codegen = @import("./codegen.zig");
const BuildOptions = @import("build_options");
const jmp = @import("jmp.zig").jmp;
const FunctionNode = @import("./node.zig").FunctionNode;
const dumpStack = @import("./disassembler.zig").dumpStack;

const Value = _value.Value;
const valueToStringAlloc = _value.valueToStringAlloc;
const valueEql = _value.valueEql;
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
const ObjNative = _obj.ObjNative;
const ObjBoundMethod = _obj.ObjBoundMethod;
const ObjFiber = _obj.ObjFiber;
const NativeFn = _obj.NativeFn;
const NativeCtx = _obj.NativeCtx;
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
export fn bz_push(self: *VM, value: Value) void {
    self.push(value);
}

/// Pop a Value from the stack and returns it
export fn bz_pop(self: *VM) Value {
    self.current_fiber.stack_top -= 1;
    return @ptrCast(*Value, self.current_fiber.stack_top).*;
}

/// Peeks at the stack at [distance] from the stack top
export fn bz_peek(self: *VM, distance: u32) Value {
    return @ptrCast(*Value, self.current_fiber.stack_top - 1 - distance).*;
}

// Value manipulations

/// Push a boolean value on the stack
export fn bz_pushBool(self: *VM, value: bool) void {
    self.push(Value.fromBoolean(value));
}

/// Push a float value on the stack
export fn bz_pushFloat(self: *VM, value: f64) void {
    self.push(Value.fromFloat(value));
}

/// Push a integer value on the stack
export fn bz_pushInteger(self: *VM, value: i32) void {
    self.push(Value.fromInteger(value));
}

/// Push null on the stack
export fn bz_pushNull(self: *VM) void {
    self.push(Value.Null);
}

/// Push void on the stack
export fn bz_pushVoid(self: *VM) void {
    self.push(Value.Void);
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

/// Converts a value to a string
export fn bz_valueToString(value: Value, len: *usize) ?[*]const u8 {
    if (!value.isObj() or value.obj().obj_type != .String) {
        return null;
    }

    const string = ObjString.cast(value.obj()).?.string;

    len.* = string.len;

    return if (string.len > 0) @ptrCast([*]const u8, string) else null;
}

/// Dump value
export fn bz_valueDump(value: Value, vm: *VM) void {
    if (!value.isObj()) {
        const string = valueToStringAlloc(vm.gc.allocator, value) catch "";
        defer vm.gc.allocator.free(string);

        std.debug.print("{s}", .{string});
    } else {
        switch (value.obj().obj_type) {
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
                const upvalue = ObjUpValue.cast(value.obj()).?;

                bz_valueDump(if (upvalue.closed != null) upvalue.closed.? else upvalue.location.*, vm);
            },

            .String => {
                const string = ObjString.cast(value.obj()).?;

                std.debug.print("\"{s}\"", .{string.string});
            },

            .Pattern => {
                const pattern = ObjPattern.cast(value.obj()).?;

                std.debug.print("_{s}_", .{pattern.source});
            },

            .List => {
                const list = ObjList.cast(value.obj()).?;

                std.debug.print("[ ", .{});
                for (list.items.items) |item| {
                    bz_valueDump(item, vm);
                    std.debug.print(", ", .{});
                }
                std.debug.print("]", .{});
            },

            .Map => {
                const map = ObjMap.cast(value.obj()).?;

                std.debug.print("{{ ", .{});
                var it = map.map.iterator();
                while (it.next()) |kv| {
                    const key = kv.key_ptr.*;

                    bz_valueDump(key, vm);
                    std.debug.print(": ", .{});
                    bz_valueDump(kv.value_ptr.*, vm);
                    std.debug.print(", ", .{});
                }
                std.debug.print("}}", .{});
            },

            .Enum => {
                const enumeration = ObjEnum.cast(value.obj()).?;
                const enum_type_def = enumeration.type_def.resolved_type.?.Enum;

                std.debug.print("enum({s}) {s} {{ ", .{ enum_type_def.name.string, enumeration.name.string });
                for (enum_type_def.cases.items) |case, i| {
                    std.debug.print("{s} -> ", .{case});
                    bz_valueDump(enumeration.cases.items[i], vm);
                    std.debug.print(", ", .{});
                }
                std.debug.print("}}", .{});
            },

            .Object => {
                const object = ObjObject.cast(value.obj()).?;
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
                            bz_valueDump(static_kv.value_ptr.*, vm);
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
                            bz_valueDump(field_kv.value_ptr.*, vm);
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
                const object_instance = ObjObjectInstance.cast(value.obj()).?;

                std.debug.print("{s}{{ ", .{if (object_instance.object) |object| object.type_def.resolved_type.?.Object.name.string else "."});
                var it = object_instance.fields.iterator();
                while (it.next()) |kv| {
                    std.debug.print("{s} = ", .{kv.key_ptr.*.string});
                    bz_valueDump(kv.value_ptr.*, vm);
                    std.debug.print(", ", .{});
                }
                std.debug.print("}}", .{});
            },
        }
    }
}

export fn bz_valueToUserData(value: Value) *UserData {
    return ObjUserData.cast(value.obj()).?.userdata;
}

// Obj manipulations

/// Converts a c string to a *ObjString
export fn bz_string(vm: *VM, string: ?[*]const u8, len: usize) ?*ObjString {
    return (if (string) |ustring| vm.gc.copyString(ustring[0..len]) else vm.gc.copyString("")) catch null;
}

/// ObjString -> [*]const u8 + len
export fn bz_objStringToString(obj_string: *ObjString, len: *usize) ?[*]const u8 {
    len.* = obj_string.string.len;

    return if (obj_string.string.len > 0) @ptrCast([*]const u8, obj_string.string) else null;
}

/// ObjString -> Value
export fn bz_objStringToValue(obj_string: *ObjString) Value {
    return obj_string.toValue();
}

export fn bz_objStringConcat(vm: *VM, obj_string: Value, other: Value) Value {
    return (ObjString.cast(obj_string.obj()).?.concat(
        vm,
        ObjString.cast(other.obj()).?,
    ) catch @panic("Could not concat strings")).toValue();
}

export fn bz_objStringSubscript(vm: *VM, obj_string: Value, index_value: Value) Value {
    const str = ObjString.cast(obj_string.obj()).?;
    const index = index_value.integer();

    if (index < 0) {
        bz_throw(
            vm,
            (vm.gc.copyString("Out of bound string access.") catch unreachable).toValue(),
        );

        return Value.Error;
    }

    const str_index: usize = @intCast(usize, index);

    if (str_index < str.string.len) {
        return (vm.gc.copyString(&([_]u8{str.string[str_index]})) catch unreachable).toValue();
    } else {
        bz_throw(
            vm,
            (vm.gc.copyString("Out of bound str access.") catch unreachable).toValue(),
        );

        return Value.Error;
    }
}

export fn bz_toString(vm: *VM, value: Value) Value {
    const str = valueToStringAlloc(vm.gc.allocator, value) catch {
        @panic("Could not convert value to string");
    };
    defer vm.gc.allocator.free(str);

    return Value.fromObj(
        (vm.gc.copyString(str) catch {
            @panic("Could not convert value to string");
        }).toObj(),
    );
}

// Other stuff

// Type helpers

/// Returns the [str] type
export fn bz_stringType() Value {
    const bool_type = allocator.create(ObjTypeDef) catch @panic("Could not create type");

    bool_type.* = ObjTypeDef{ .def_type = .String, .optional = false };

    return bool_type.toValue();
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

export fn bz_newList(vm: *VM, of_type: Value) Value {
    var list_def: ObjList.ListDef = ObjList.ListDef.init(
        vm.gc.allocator,
        ObjTypeDef.cast(of_type.obj()).?,
    );

    var list_def_union: ObjTypeDef.TypeUnion = .{
        .List = list_def,
    };

    var list_def_type: *ObjTypeDef = vm.gc.type_registry.getTypeDef(ObjTypeDef{
        .def_type = .List,
        .optional = false,
        .resolved_type = list_def_union,
    }) catch @panic("Could not create list");

    return (vm.gc.allocateObject(
        ObjList,
        ObjList.init(vm.gc.allocator, list_def_type),
    ) catch @panic("Could not create list")).toValue();
}

export fn bz_listAppend(vm: *VM, list: Value, value: Value) void {
    ObjList.cast(list.obj()).?.rawAppend(vm.gc, value) catch @panic("Could not add element to list");
}

export fn bz_valueToList(value: Value) *ObjList {
    return ObjList.cast(value.obj()).?;
}

export fn bz_listGet(self: Value, index: usize) Value {
    return ObjList.cast(self.obj()).?.items.items[index];
}

export fn bz_listSet(vm: *VM, self: Value, index: usize, value: Value) void {
    ObjList.cast(self.obj()).?.set(
        vm.gc,
        index,
        value,
    ) catch @panic("Could not set element in list");
}

export fn bz_listLen(self: *ObjList) usize {
    return self.items.items.len;
}

export fn bz_listConcat(vm: *VM, list: Value, other_list: Value) Value {
    const left: *ObjList = ObjList.cast(list.obj()).?;
    const right: *ObjList = ObjList.cast(other_list.obj()).?;

    var new_list = std.ArrayList(Value).init(vm.gc.allocator);
    new_list.appendSlice(left.items.items) catch @panic("Could not concatenate lists");
    new_list.appendSlice(right.items.items) catch @panic("Could not concatenate lists");

    return (vm.gc.allocateObject(
        ObjList,
        ObjList{
            .type_def = left.type_def,
            .methods = left.methods,
            .items = new_list,
        },
    ) catch @panic("Could not concatenate lists")).toValue();
}

export fn bz_mapConcat(vm: *VM, map: Value, other_map: Value) Value {
    const left: *ObjMap = ObjMap.cast(map.obj()).?;
    const right: *ObjMap = ObjMap.cast(other_map.obj()).?;

    var new_map = left.map.clone() catch @panic("Could not concatenate maps");
    var it = right.map.iterator();
    while (it.next()) |entry| {
        new_map.put(entry.key_ptr.*, entry.value_ptr.*) catch @panic("Could not concatenate maps");
    }

    return (vm.gc.allocateObject(ObjMap, ObjMap{
        .type_def = left.type_def,
        .methods = left.methods,
        .map = new_map,
    }) catch @panic("Could not concatenate maps")).toValue();
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

export fn bz_userDataToValue(userdata: *ObjUserData) Value {
    return userdata.toValue();
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

    // FIXME: give reference to JIT?
    vm.* = VM.init(gc, self.import_registry, self.testing) catch {
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
    self.callValue(
        closure.toValue(),
        len,
        if (catch_value) |v| v.* else null,
    ) catch unreachable;

    // If not compiled, run it with the VM loop
    if (closure.function.native == null) {
        self.run();
    }
}

// Assumes the global exists
export fn bz_pushError(self: *VM, qualified_name: [*]const u8, len: usize) void {
    const object = bz_getQualified(self, qualified_name, len);

    self.push(
        // Dismiss error because if we fail to create the error payload there's not much to salvage anyway
        (self.gc.allocateObject(
            ObjObjectInstance,
            ObjObjectInstance.init(self.gc.allocator, ObjObject.cast(object.obj()).?, null),
        ) catch unreachable).toValue(),
    );
}

export fn bz_pushErrorEnum(self: *VM, qualified_name: [*]const u8, name_len: usize, case_str: [*]const u8, case_len: usize) void {
    const enum_set = ObjEnum.cast(bz_getQualified(self, qualified_name, name_len).obj()).?;
    const case = self.gc.copyString(case_str[0..case_len]) catch @panic("Could not create error payload");

    self.push(
        bz_getEnumCase(
            self,
            enum_set.toValue(),
            case.toValue(),
        ),
    );
}

export fn bz_getQualified(self: *VM, qualified_name: [*]const u8, len: usize) Value {
    for (self.globals.items) |global| {
        if (global.isObj()) {
            switch (global.obj().obj_type) {
                .Enum => {
                    const obj_enum = ObjEnum.cast(global.obj()).?;

                    if (std.mem.eql(u8, qualified_name[0..len], obj_enum.type_def.resolved_type.?.Enum.qualified_name.string)) {
                        return global;
                    }
                },
                .Object => {
                    const obj_enum = ObjObject.cast(global.obj()).?;

                    if (std.mem.eql(u8, qualified_name[0..len], obj_enum.type_def.resolved_type.?.Object.qualified_name.string)) {
                        return global;
                    }
                },
                else => {},
            }
        }
    }

    unreachable;
}

export fn bz_instance(vm: *VM, object_value: Value, typedef_value: Value) Value {
    const object = if (object_value.isObj()) ObjObject.cast(object_value.obj()).? else null;
    const typedef = if (typedef_value.isObj()) ObjTypeDef.cast(typedef_value.obj()).? else null;

    const instance = vm.gc.allocateObject(
        ObjObjectInstance,
        ObjObjectInstance.init(
            vm.gc.allocator,
            object,
            typedef,
        ),
    ) catch @panic("Could not instanciate object");

    // If not anonymous, set default fields
    if (object) |uobject| {
        var it = uobject.fields.iterator();
        while (it.next()) |kv| {
            instance.setField(
                vm.gc,
                kv.key_ptr.*,
                vm.cloneValue(kv.value_ptr.*) catch @panic("Could not set object field"),
            ) catch @panic("Could not set object field");
        }
    }

    return instance.toValue();
}

export fn bz_getObjectField(object_value: Value, field_name_value: Value) Value {
    const object = ObjObject.cast(object_value.obj()).?;

    return object.static_fields.get(ObjString.cast(field_name_value.obj()).?).?;
}

export fn bz_setObjectField(vm: *VM, object_value: Value, field_name_value: Value, value: Value) void {
    const object = ObjObject.cast(object_value.obj()).?;
    const field = ObjString.cast(field_name_value.obj()).?;

    object.setStaticField(vm.gc, field, value) catch @panic("Could not set static field");
}

export fn bz_setInstanceField(vm: *VM, instance_value: Value, field_name_value: Value, value: Value) void {
    ObjObjectInstance.cast(instance_value.obj()).?.setField(
        vm.gc,
        ObjString.cast(field_name_value.obj()).?,
        value,
    ) catch @panic("Could not set instance field");
}

export fn bz_getInstanceField(vm: *VM, instance_value: Value, field_name_value: Value, bind: bool) Value {
    const instance = ObjObjectInstance.cast(instance_value.obj()).?;
    if (instance.fields.get(ObjString.cast(field_name_value.obj()).?)) |field| {
        return field;
    }

    const method = instance.object.?.methods.get(ObjString.cast(field_name_value.obj()).?).?.toValue();

    return if (bind)
        bz_bindMethod(
            vm,
            instance.toValue(),
            method,
            Value.Null,
        )
    else
        method;
}

export fn bz_bindMethod(vm: *VM, receiver: Value, method_value: Value, native_value: Value) Value {
    return (vm.gc.allocateObject(
        ObjBoundMethod,
        .{
            .receiver = receiver,
            .closure = if (method_value.isObj()) ObjClosure.cast(method_value.obj()).? else null,
            .native = if (native_value.isObj()) ObjNative.cast(native_value.obj()).? else null,
        },
    ) catch @panic("Could not bind method")).toValue();
}

export fn bz_valueToObject(value: Value) *ObjObject {
    return ObjObject.cast(value.obj()).?;
}

export fn bz_pushObjectInstance(vm: *VM, payload: *ObjObjectInstance) void {
    vm.push(payload.toValue());
}

export fn bz_getEnumCase(vm: *VM, enum_value: Value, case_name_value: Value) Value {
    const self = ObjEnum.cast(enum_value.obj()).?;
    const case = ObjString.cast(case_name_value.obj()).?.string;
    var case_index: usize = 0;

    for (self.type_def.resolved_type.?.Enum.cases.items) |enum_case, index| {
        if (std.mem.eql(u8, case, enum_case)) {
            case_index = index;
            break;
        }
    }

    return (vm.gc.allocateObject(
        ObjEnumInstance,
        ObjEnumInstance{
            .enum_ref = self,
            .case = @intCast(u8, case_index),
        },
    ) catch @panic("Could not create enum case")).toValue();
}

export fn bz_getEnumCaseValue(enum_instance_value: Value) Value {
    const instance = ObjEnumInstance.cast(enum_instance_value.obj()).?;

    return instance.enum_ref.cases.items[instance.case];
}

export fn bz_getEnumCaseFromValue(vm: *VM, enum_value: Value, case_value: Value) Value {
    const enum_ = ObjEnum.cast(enum_value.obj()).?;

    for (enum_.cases.items) |case, index| {
        if (valueEql(case, case_value)) {
            var enum_case: *ObjEnumInstance = vm.gc.allocateObject(ObjEnumInstance, ObjEnumInstance{
                .enum_ref = enum_,
                .case = @intCast(u8, index),
            }) catch @panic("Could not create enum instance");

            return Value.fromObj(enum_case.toObj());
        }
    }

    return Value.Null;
}

export fn bz_pushEnumInstance(vm: *VM, payload: *ObjEnumInstance) void {
    vm.push(payload.toValue());
}

export fn bz_valueIsBuzzFn(value: Value) bool {
    if (!value.isObj()) {
        return false;
    }

    if (ObjClosure.cast(value.obj())) |closure| {
        return closure.function.native == null;
    }

    return false;
}

export fn bz_valueToClosure(value: Value) *ObjClosure {
    return ObjClosure.cast(value.obj()).?;
}

export fn bz_toObjNative(value: Value) *ObjNative {
    return ObjNative.cast(value.obj()).?;
}

export fn bz_toObjNativeOpt(value: Value) ?*ObjNative {
    return ObjNative.cast(value.obj());
}

export fn bz_valueToExternNativeFn(value: Value) *anyopaque {
    return ObjNative.cast(value.obj()).?.native;
}

export fn bz_valueToRawNative(value: Value) *anyopaque {
    return ObjClosure.cast(value.obj()).?.function.native_raw.?;
}

export fn bz_valueEqual(self: Value, other: Value) Value {
    return Value.fromBoolean(_value.valueEql(self, other));
}

export fn bz_newMap(vm: *VM, map_type: Value) Value {
    var map: *ObjMap = vm.gc.allocateObject(ObjMap, ObjMap.init(
        vm.gc.allocator,
        ObjTypeDef.cast(map_type.obj()).?,
    )) catch @panic("Could not create map");

    return Value.fromObj(map.toObj());
}

export fn bz_mapSet(vm: *VM, map: Value, key: Value, value: Value) void {
    ObjMap.cast(map.obj()).?.set(
        vm.gc,
        key,
        value,
    ) catch @panic("Could not set map element");
}

export fn bz_mapGet(map: Value, key: Value) Value {
    return ObjMap.cast(map.obj()).?.map.get(_value.floatToInteger(key)) orelse Value.Null;
}

export fn bz_valueIs(self: Value, type_def: Value) Value {
    return Value.fromBoolean(_value.valueIs(type_def, self));
}

export fn bz_setTryCtx(self: *VM) *TryCtx {
    var try_ctx = self.gc.allocator.create(TryCtx) catch @panic("Could not create try context");
    try_ctx.* = .{
        .previous = self.current_fiber.try_context,
        .env = undefined,
    };

    self.current_fiber.try_context = try_ctx;

    // Doesn't setjmp itself so it is done in the correct function context

    return try_ctx;
}

export fn bz_popTryCtx(self: *VM) void {
    if (self.current_fiber.try_context) |try_ctx| {
        self.current_fiber.try_context = try_ctx.previous;

        self.gc.allocator.destroy(try_ctx);
    }
}

// Like bz_throw but assumes the error payload is already on the stack
export fn bz_rethrow(vm: *VM) void {
    // Are we in a JIT compiled function and within a try-catch?
    if (vm.currentFrame().?.in_native_call and vm.current_fiber.try_context != null) {
        // FIXME: close try scope

        if (builtin.os.tag == .macos or builtin.os.tag == .linux) {
            jmp._longjmp(&vm.current_fiber.try_context.?.env, 1);
        } else {
            jmp.longjmp(&vm.current_fiber.try_context.?.env, 1);
        }

        unreachable;
    }
}

export fn bz_throw(vm: *VM, value: Value) void {
    vm.push(value);

    bz_rethrow(vm);
}

export fn bz_closeUpValues(vm: *VM, last: *Value) void {
    vm.closeUpValues(last);
    _ = vm.pop();
}

export fn bz_getUpValue(ctx: *NativeCtx, slot: usize) Value {
    return ctx.upvalues[slot].location.*;
}

export fn bz_setUpValue(ctx: *NativeCtx, slot: usize, value: Value) void {
    ctx.upvalues[slot].location.* = value;
}

export fn bz_getUpValues(closure: Value) [*]*ObjUpValue {
    return ObjClosure.cast(closure.obj()).?.upvalues.items.ptr;
}

export fn bz_getGlobals(closure: Value) [*]Value {
    return ObjClosure.cast(closure.obj()).?.globals.items.ptr;
}

export fn bz_context(ctx: *NativeCtx, closure_value: Value, new_ctx: *NativeCtx, arg_count: usize) *anyopaque {
    const bound = if (closure_value.obj().obj_type == .Bound) ObjBoundMethod.cast(closure_value.obj()).? else null;
    const closure = if (bound) |bd|
        bd.closure
    else
        ObjClosure.cast(closure_value.obj());
    const native = if (bound == null and closure == null) ObjNative.cast(closure_value.obj()).? else null;

    // If bound method, replace closure on the stack by the receiver
    if (bound != null) {
        (ctx.vm.current_fiber.stack_top - arg_count - 1)[0] = bound.?.receiver;
    }

    new_ctx.* = NativeCtx{
        .vm = ctx.vm,
        .globals = if (closure) |cls| cls.globals.items.ptr else ctx.globals,
        .upvalues = if (closure) |cls| cls.upvalues.items.ptr else ctx.upvalues,
        .base = ctx.vm.current_fiber.stack_top - arg_count - 1,
        .stack_top = &ctx.vm.current_fiber.stack_top,
    };

    if (closure != null and ctx.vm.jit != null and ctx.vm.jit.?.shouldCompileFunction(closure.?)) {
        ctx.vm.jit.?.compileFunction(closure.?) catch @panic("Failed compiling function");
    }

    return if (closure) |cls| cls.function.native_raw.? else native.?.native;
}

export fn bz_closure(
    ctx: *NativeCtx,
    function_node: *FunctionNode,
    native: *anyopaque,
    native_raw: *anyopaque,
) Value {
    const function_def = function_node.node.type_def.?.resolved_type.?.Function;

    // Create an ObjFunction
    var obj_function = ctx.vm.gc.allocateObject(
        ObjFunction,
        ObjFunction.init(
            ctx.vm.gc.allocator,
            function_node,
            function_def.name,
        ) catch @panic("Could not instanciate closure"),
    ) catch @panic("Could not instanciate closure");
    obj_function.type_def = function_node.node.type_def.?;
    obj_function.upvalue_count = @intCast(u8, function_node.upvalue_binding.count());
    obj_function.native = native;
    obj_function.native_raw = native_raw;

    const closure: *ObjClosure = ctx.vm.gc.allocateObject(
        ObjClosure,
        ObjClosure.init(
            ctx.vm.gc.allocator,
            ctx.vm,
            obj_function,
        ) catch @panic("Could not instanciate closure"),
    ) catch @panic("Could not instanciate closure");

    // On stack to prevent collection
    ctx.vm.push(closure.toValue());

    ctx.vm.jit.?.compiled_closures.put(closure, {}) catch @panic("Could not get closure");

    var it = function_node.upvalue_binding.iterator();
    while (it.next()) |kv| {
        const is_local = kv.value_ptr.*;
        const index = kv.key_ptr.*;

        if (is_local) {
            closure.upvalues.append(
                ctx.vm.captureUpvalue(&(ctx.base[index])) catch @panic("Could not instanciate closure"),
            ) catch @panic("Could not instanciate closure");
        } else {
            closure.upvalues.append(ctx.upvalues[index]) catch @panic("Could not instanciate closure");
        }
    }

    return ctx.vm.pop();
}

export fn bz_dumpStack(ctx: *NativeCtx, off: usize) void {
    std.debug.print("base is {}, top is {}\n", .{ @ptrToInt(ctx.base), @ptrToInt(ctx.vm.current_fiber.stack_top) });
    std.debug.print("#{}:\n", .{off});
    dumpStack(ctx.vm);
}

export fn bz_getStringField(vm: *VM, string_value: Value, field_name_value: Value, bind: bool) Value {
    const string = ObjString.cast(string_value.obj()).?;
    const method = (ObjString.member(vm, ObjString.cast(field_name_value.obj()).?) catch @panic("Could not get string method")).?;

    return if (bind)
        bz_bindMethod(
            vm,
            string.toValue(),
            Value.Null,
            method.toValue(),
        )
    else
        method.toValue();
}

export fn bz_getPatternField(vm: *VM, pattern_value: Value, field_name_value: Value, bind: bool) Value {
    const pattern = ObjPattern.cast(pattern_value.obj()).?;
    const method = (ObjPattern.member(vm, ObjString.cast(field_name_value.obj()).?) catch @panic("Could not get pattern method")).?;

    return if (bind)
        bz_bindMethod(
            vm,
            pattern.toValue(),
            Value.Null,
            method.toValue(),
        )
    else
        method.toValue();
}

export fn bz_getFiberField(vm: *VM, fiber_value: Value, field_name_value: Value, bind: bool) Value {
    const fiber = ObjFiber.cast(fiber_value.obj()).?;
    const method = (ObjFiber.member(vm, ObjString.cast(field_name_value.obj()).?) catch @panic("Could not get fiber method")).?;

    return if (bind)
        bz_bindMethod(
            vm,
            fiber.toValue(),
            Value.Null,
            method.toValue(),
        )
    else
        method.toValue();
}

export fn bz_getListField(vm: *VM, list_value: Value, field_name_value: Value, bind: bool) Value {
    const list = ObjList.cast(list_value.obj()).?;
    const method = (list.member(vm, ObjString.cast(field_name_value.obj()).?) catch @panic("Could not get list method")).?;

    return if (bind)
        bz_bindMethod(
            vm,
            list.toValue(),
            Value.Null,
            method.toValue(),
        )
    else
        method.toValue();
}

export fn bz_getMapField(vm: *VM, map_value: Value, field_name_value: Value, bind: bool) Value {
    const map = ObjMap.cast(map_value.obj()).?;
    const method = (map.member(vm, ObjString.cast(field_name_value.obj()).?) catch @panic("Could not get map method")).?;

    return if (bind)
        bz_bindMethod(
            vm,
            map.toValue(),
            Value.Null,
            method.toValue(),
        )
    else
        method.toValue();
}

export fn bz_stringNext(vm: *VM, string_value: Value, index: *Value) Value {
    const string = ObjString.cast(string_value.obj()).?;

    if (string.next(
        vm,
        if (index.isNull()) null else index.integer(),
    ) catch @panic("Could not get next string index")) |new_index| {
        index.* = Value.fromInteger(new_index);

        return (vm.gc.copyString(&[_]u8{string.string[@intCast(usize, new_index)]}) catch @panic("Could not iterate on string")).toValue();
    }

    index.* = Value.Null;
    return Value.Null;
}

export fn bz_listNext(vm: *VM, list_value: Value, index: *Value) Value {
    const list = ObjList.cast(list_value.obj()).?;

    if (list.rawNext(
        vm,
        if (index.isNull()) null else index.integer(),
    ) catch @panic("Could not get next list index")) |new_index| {
        index.* = Value.fromInteger(new_index);
        return list.items.items[@intCast(usize, new_index)];
    }

    index.* = Value.Null;
    return Value.Null;
}

export fn bz_mapNext(_: *VM, map_value: Value, key: *Value) Value {
    const map = ObjMap.cast(map_value.obj()).?;

    if (map.rawNext(if (key.isNull()) null else key.*)) |new_key| {
        key.* = new_key;

        return map.map.get(new_key) orelse Value.Null;
    }

    key.* = Value.Null;
    return Value.Null;
}

export fn bz_enumNext(vm: *VM, enum_value: Value, case: Value) Value {
    const enum_ = ObjEnum.cast(enum_value.obj()).?;

    if (enum_.rawNext(
        vm,
        if (case.isNull()) null else ObjEnumInstance.cast(case.obj()),
    ) catch @panic("Could not iterate over enum")) |new_case| {
        return new_case.toValue();
    }

    return Value.Null;
}
