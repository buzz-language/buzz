const std = @import("std");
const builtin = @import("builtin");
const buzz_builtin = @import("builtin.zig");
const _vm = @import("vm.zig");
const VM = _vm.VM;
const TryCtx = _vm.TryCtx;
const _obj = @import("obj.zig");
const Value = @import("value.zig").Value;
const memory = @import("memory.zig");
const Parser = @import("Parser.zig");
const CodeGen = @import("Codegen.zig");
const BuildOptions = @import("build_options");
const jmp = @import("jmp.zig").jmp;
const dumpStack = @import("disassembler.zig").dumpStack;
const ZigType = @import("zigtypes.zig").Type;
const Token = @import("Token.zig");
const Ast = @import("Ast.zig");

const eql = Value.eql;
const Obj = _obj.Obj;
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
const ObjForeignContainer = _obj.ObjForeignContainer;
const NativeFn = _obj.NativeFn;
const NativeCtx = _obj.NativeCtx;
const TypeRegistry = memory.TypeRegistry;
const GarbageCollector = memory.GarbageCollector;

var gpa = std.heap.GeneralPurposeAllocator(.{
    .safety = true,
}){};

pub const allocator: std.mem.Allocator = if (builtin.mode == .Debug)
    gpa.allocator()
else if (BuildOptions.mimalloc)
    @import("mimalloc.zig").mim_allocator
else
    std.heap.c_allocator;

// Stack manipulation

/// Push a Value to the stack
export fn bz_push(vm: *VM, value: Value) void {
    vm.push(value);
}

/// Pop a Value from the stack and returns it
export fn bz_pop(vm: *VM) Value {
    return vm.pop();
}

/// Peeks at the stack at [distance] from the stack top
export fn bz_peek(vm: *VM, dist: u32) Value {
    return vm.peek(dist);
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

    return if (string.len > 0) @as([*]const u8, @ptrCast(string)) else null;
}

export fn bz_valueToCString(value: Value) ?[*:0]const u8 {
    if (!value.isObj() or value.obj().obj_type != .String) {
        return null;
    }

    return @ptrCast(ObjString.cast(value.obj()).?.string.ptr);
}

fn valueDump(value: Value, vm: *VM, seen: *std.AutoHashMap(*_obj.Obj, void), depth: usize) void {
    if (depth > 50) {
        std.debug.print("...", .{});
        return;
    }

    if (value.isNull()) {
        std.debug.print("null", .{});
    } else if (!value.isObj() or seen.get(value.obj()) != null) {
        const string = value.toStringAlloc(vm.gc.allocator) catch std.ArrayList(u8).init(vm.gc.allocator);
        defer string.deinit();

        std.debug.print("{s}", .{string.items});
    } else {
        seen.put(value.obj(), {}) catch unreachable;

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
                const string = value.toStringAlloc(vm.gc.allocator) catch std.ArrayList(u8).init(vm.gc.allocator);
                defer string.deinit();

                std.debug.print("{s}", .{string.items});
            },

            .UpValue => {
                const upvalue = ObjUpValue.cast(value.obj()).?;

                valueDump(if (upvalue.closed != null) upvalue.closed.? else upvalue.location.*, vm, seen, depth);
            },

            .String => {
                const string = ObjString.cast(value.obj()).?;

                std.debug.print("\"{s}\"", .{string.string});
            },

            .Pattern => {
                const pattern = ObjPattern.cast(value.obj()).?;

                std.debug.print("$\"{s}\"", .{pattern.source});
            },

            .List => {
                const list = ObjList.cast(value.obj()).?;

                std.debug.print("[ ", .{});
                for (list.items.items) |item| {
                    valueDump(item, vm, seen, depth + 1);
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

                    valueDump(key, vm, seen, depth + 1);
                    std.debug.print(": ", .{});
                    valueDump(kv.value_ptr.*, vm, seen, depth + 1);
                    std.debug.print(", ", .{});
                }
                std.debug.print("}}", .{});
            },

            .Enum => {
                const enumeration = ObjEnum.cast(value.obj()).?;
                const enum_type_def = enumeration.type_def.resolved_type.?.Enum;

                std.debug.print("enum({s}) {s} {{ ", .{ enum_type_def.name.string, enumeration.name.string });
                for (enum_type_def.cases.items, 0..) |case, i| {
                    std.debug.print("{s} -> ", .{case});
                    valueDump(enumeration.cases[i], vm, seen, depth);
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

                std.debug.print(" {s} {{ ", .{object_def.name.string});

                var it = object_def.static_fields.iterator();
                while (it.next()) |kv| {
                    const static_field_type_str = kv.value_ptr.*.toStringAlloc(vm.gc.allocator) catch std.ArrayList(u8).init(vm.gc.allocator);
                    defer static_field_type_str.deinit();

                    std.debug.print("static {s} {s}", .{ static_field_type_str.items, kv.key_ptr.* });

                    var static_it = object.static_fields.iterator();
                    while (static_it.next()) |static_kv| {
                        if (std.mem.eql(u8, static_kv.key_ptr.*.string, kv.key_ptr.*)) {
                            std.debug.print(" = ", .{});
                            valueDump(static_kv.value_ptr.*, vm, seen, depth + 1);
                            break;
                        }
                    }

                    std.debug.print(", ", .{});
                }

                it = object_def.fields.iterator();
                while (it.next()) |kv| {
                    const field_type_str = kv.value_ptr.*.toStringAlloc(vm.gc.allocator) catch std.ArrayList(u8).init(vm.gc.allocator);
                    defer field_type_str.deinit();

                    std.debug.print("{s} {s}", .{ field_type_str.items, kv.key_ptr.* });

                    if (object.fields.get(vm.gc.copyString(kv.key_ptr.*) catch unreachable)) |v| {
                        std.debug.print(" = ", .{});
                        valueDump(v, vm, seen, depth + 1);
                    }

                    std.debug.print(", ", .{});
                }

                it = object_def.methods.iterator();
                while (it.next()) |kv| {
                    const method_type_str = kv.value_ptr.*.toStringAlloc(vm.gc.allocator) catch std.ArrayList(u8).init(vm.gc.allocator);
                    defer method_type_str.deinit();

                    std.debug.print("{s}, ", .{method_type_str.items});
                }

                std.debug.print("}}", .{});
            },

            .ObjectInstance => {
                const object_instance = ObjObjectInstance.cast(value.obj()).?;

                std.debug.print(
                    "{s}{{ ",
                    .{
                        if (object_instance.object) |object|
                            object.type_def.resolved_type.?.Object.name.string
                        else
                            ".",
                    },
                );
                var it = object_instance.fields.iterator();
                while (it.next()) |kv| {
                    std.debug.print("{s} = ", .{kv.key_ptr.*.string});
                    valueDump(kv.value_ptr.*, vm, seen, depth + 1);
                    std.debug.print(", ", .{});
                }
                std.debug.print("}}", .{});
            },

            .ForeignContainer => {
                const foreign = ObjForeignContainer.cast(value.obj()).?;
                const foreign_def = foreign.type_def.resolved_type.?.ForeignContainer;

                std.debug.print(
                    "{s}{{ ",
                    .{foreign_def.name.string},
                );

                var it = foreign_def.fields.iterator();
                while (it.next()) |kv| {
                    std.debug.print("{s} = ", .{kv.key_ptr.*});
                    valueDump(
                        kv.value_ptr.*.getter(
                            vm,
                            foreign.data.ptr,
                        ),
                        vm,
                        seen,
                        depth + 1,
                    );
                    std.debug.print(", ", .{});
                }
                std.debug.print("}}", .{});
            },
        }

        _ = seen.remove(value.obj());
    }
}

/// Dump value
pub export fn bz_valueDump(value: Value, vm: *VM) void {
    var seen = std.AutoHashMap(*_obj.Obj, void).init(vm.gc.allocator);
    defer seen.deinit();

    valueDump(value, vm, &seen, 0);
}

// Obj manipulations

export fn bz_valueToUserData(value: Value) u64 {
    return ObjUserData.cast(value.obj()).?.userdata;
}

export fn bz_valueToObjUserData(value: Value) *ObjUserData {
    return ObjUserData.cast(value.obj()).?;
}

// FIXME: move this is ObjForeignContainer?
export fn bz_valueToForeignContainerPtr(value: Value) [*]u8 {
    return ObjForeignContainer.cast(value.obj()).?.data.ptr;
}

/// Converts a c string to a *ObjString
export fn bz_string(vm: *VM, string: ?[*]const u8, len: usize) ?*ObjString {
    return (if (string) |ustring| vm.gc.copyString(ustring[0..len]) else vm.gc.copyString("")) catch null;
}

export fn bz_stringZ(vm: *VM, string: [*:0]const u8) Value {
    // Keeping the sentinel
    return (vm.gc.copyString(string[0..(std.mem.len(string) + 1)]) catch @panic("Out of memory")).toValue();
}

export fn bz_valueToObjString(value: Value) *ObjString {
    return ObjString.cast(value.obj()).?;
}

export fn bz_valueToObjTypeDef(value: Value) *ObjTypeDef {
    return ObjTypeDef.cast(value.obj()).?;
}

/// ObjString -> [*]const u8 + len
export fn bz_objStringToString(obj_string: *ObjString, len: *usize) ?[*]const u8 {
    len.* = obj_string.string.len;

    return if (obj_string.string.len > 0) @as([*]const u8, @ptrCast(obj_string.string)) else null;
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

    const str_index: usize = @intCast(index);

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
    const str = value.toStringAlloc(vm.gc.allocator) catch {
        @panic("Could not convert value to string");
    };
    defer str.deinit();

    return Value.fromObj(
        (vm.gc.copyString(str.items) catch {
            @panic("Could not convert value to string");
        }).toObj(),
    );
}

// Other stuff

// Type helpers

/// Returns the [str] type
export fn bz_stringType(vm: *VM) Value {
    return (vm.gc.type_registry.getTypeDef(
        ObjTypeDef{ .def_type = .String, .optional = false },
    ) catch @panic("Out of memory")).toValue();
}

export fn bz_mapType(vm: *VM, key_type: Value, value_type: Value) Value {
    const map_def = ObjMap.MapDef.init(
        vm.gc.allocator,
        ObjTypeDef.cast(key_type.obj()).?,
        ObjTypeDef.cast(value_type.obj()).?,
    );

    const resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
        .Map = map_def,
    };

    const typedef = vm.gc.type_registry.getTypeDef(
        .{
            .def_type = .Map,
            .optional = false,
            .resolved_type = resolved_type,
        },
    ) catch @panic("Out of memory");

    return typedef.toValue();
}

export fn bz_containerTypeSize(type_def: *ObjTypeDef) usize {
    const struct_def = type_def.resolved_type.?.ForeignContainer;

    return struct_def.zig_type.size();
}

export fn bz_containerTypeAlign(type_def: *ObjTypeDef) usize {
    const struct_def = type_def.resolved_type.?.ForeignContainer;

    return struct_def.zig_type.alignment();
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

export fn bz_newUserData(vm: *VM, userdata: u64) ?*ObjUserData {
    return vm.gc.allocateObject(
        ObjUserData,
        ObjUserData{ .userdata = userdata },
    ) catch {
        return null;
    };
}

export fn bz_getUserDataPtr(userdata: *ObjUserData) u64 {
    return userdata.userdata;
}

export fn bz_userDataToValue(userdata: *ObjUserData) Value {
    return userdata.toValue();
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
    vm.* = VM.init(gc, self.import_registry, self.flavor) catch {
        return null;
    };

    return vm;
}

export fn bz_deinitVM(_: *VM) void {
    // self.deinit();
}

export fn bz_compile(
    self: *VM,
    source: ?[*]const u8,
    source_len: usize,
    file_name: ?[*]const u8,
    file_name_len: usize,
) ?*ObjFunction {
    if (source == null or file_name_len == 0 or source_len == 0 or file_name_len == 0) {
        return null;
    }

    var imports = std.StringHashMap(Parser.ScriptImport).init(self.gc.allocator);
    var strings = std.StringHashMap(*ObjString).init(self.gc.allocator);
    var parser = Parser.init(
        self.gc,
        &imports,
        false,
        self.flavor,
    );
    var codegen = CodeGen.init(
        self.gc,
        &parser,
        self.flavor,
        null,
    );
    defer {
        codegen.deinit();
        imports.deinit();
        parser.deinit();
        strings.deinit();
    }

    if (parser.parse(source.?[0..source_len], file_name.?[0..file_name_len]) catch null) |ast| {
        return codegen.generate(ast) catch null;
    } else {
        return null;
    }
}

export fn bz_interpret(self: *VM, ast: *anyopaque, function: *ObjFunction) bool {
    self.interpret(@as(*Ast, @ptrCast(@alignCast(ast))).*, function, null) catch {
        return false;
    };

    return true;
}

export fn bz_run(
    self: *VM,
    source: ?[*]const u8,
    source_len: usize,
    file_name: ?[*]const u8,
    file_name_len: usize,
) bool {
    if (source == null or file_name_len == 0 or source_len == 0 or file_name_len == 0) {
        return false;
    }

    var imports = std.StringHashMap(Parser.ScriptImport).init(self.gc.allocator);
    var strings = std.StringHashMap(*ObjString).init(self.gc.allocator);
    var parser = Parser.init(
        self.gc,
        &imports,
        false,
        self.flavor,
    );
    var codegen = CodeGen.init(
        self.gc,
        &parser,
        self.flavor,
        null,
    );
    defer {
        codegen.deinit();
        imports.deinit();
        parser.deinit();
        strings.deinit();
    }

    if (parser.parse(source.?[0..source_len], file_name.?[0..file_name_len]) catch null) |ast| {
        if (codegen.generate(ast) catch null) |function| {
            self.interpret(ast, function, null) catch {
                return false;
            };

            return true;
        }
    }

    return false;
}

fn calleeIsCompiled(value: Value) bool {
    var obj: *Obj = value.obj();
    return switch (obj.obj_type) {
        .Bound => bound: {
            const bound = ObjBoundMethod.cast(obj).?;

            if (bound.native != null) {
                break :bound true;
            }

            if (bound.closure) |cls| {
                break :bound cls.function.native != null;
            }

            break :bound false;
        },
        .Closure => ObjClosure.cast(obj).?.function.native != null,
        .Native => true,
        else => false,
    };
}

pub export fn bz_invoke(
    self: *VM,
    instance: Value,
    method: *ObjString,
    arguments: ?[*]const *const Value,
    len: u8,
    catch_value: ?*Value,
) void {
    self.push(instance);
    var i: usize = 0;
    while (i < len) : (i += 1) {
        self.push(arguments.?[i].*);
    }

    const was_in_native_call = self.currentFrame().?.in_native_call;
    self.currentFrame().?.in_native_call = true;

    // TODO: catch properly
    const callee = self.invoke(
        method,
        len,
        if (catch_value) |v| v.* else null,
        false,
    ) catch unreachable;

    // If not compiled, run it with the VM loop
    if (!calleeIsCompiled(callee)) {
        self.run();
    }

    self.currentFrame().?.in_native_call = was_in_native_call;
}

pub export fn bz_call(
    self: *VM,
    closure: *ObjClosure,
    arguments: ?[*]const *const Value,
    len: u8,
    catch_value: ?*Value,
) void {
    self.push(closure.toValue());
    var i: usize = 0;
    while (i < len) : (i += 1) {
        self.push(arguments.?[i].*);
    }

    // TODO: catch properly
    self.callValue(
        closure.toValue(),
        len,
        if (catch_value) |v| v.* else null,
        false,
    ) catch unreachable;

    // If not compiled, run it with the VM loop
    if (closure.function.native == null) {
        self.run();
    }
}

export fn bz_instanceQualified(self: *VM, qualified_name: [*]const u8, len: usize) Value {
    const object = ObjObject.cast(bz_getQualified(self, qualified_name, len).obj()).?;

    const instance: *ObjObjectInstance = self.gc.allocateObject(
        ObjObjectInstance,
        ObjObjectInstance.init(
            self,
            object,
            object.type_def.toInstance(self.gc.allocator, &self.gc.type_registry) catch @panic("Out of memory"),
        ),
    ) catch {
        @panic("Could not create error");
    };

    // Set instance fields with default values
    var it = object.fields.iterator();
    while (it.next()) |kv| {
        instance.setField(
            self.gc,
            kv.key_ptr.*,
            self.cloneValue(kv.value_ptr.*) catch @panic("Could not set object property"),
        ) catch @panic("Could not set object property");
    }

    return instance.toValue();
}

fn instanciateError(
    vm: *VM,
    qualified_name: [*]const u8,
    len: usize,
    message: ?[*]const u8,
    mlen: usize,
) Value {
    const instance = bz_instanceQualified(vm, qualified_name, len);

    if (message) |msg| {
        const obj_instance = ObjObjectInstance.cast(instance.obj()).?;
        const message_key = vm.gc.strings.get("message").?;

        if (obj_instance.fields.get(message_key) != null) {
            obj_instance.fields.put(
                message_key,
                (vm.gc.copyString(msg[0..mlen]) catch @panic("Out of memory")).toValue(),
            ) catch @panic("Out of memory");
        }
    }

    return instance;
}

// Assumes the global exists
export fn bz_pushError(
    self: *VM,
    qualified_name: [*]const u8,
    len: usize,
    message: ?[*]const u8,
    mlen: usize,
) void {
    self.push(
        instanciateError(
            self,
            qualified_name,
            len,
            message,
            mlen,
        ),
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

    std.debug.panic("bz_getQualified no name: {s}", .{qualified_name[0..len]});
}

export fn bz_instance(vm: *VM, object_value: Value, typedef_value: Value) Value {
    const object = if (object_value.isObj()) ObjObject.cast(object_value.obj()).? else null;
    const typedef = ObjTypeDef.cast(typedef_value.obj()).?;

    const instance = vm.gc.allocateObject(
        ObjObjectInstance,
        ObjObjectInstance.init(
            vm,
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

export fn bz_valueToObjectInstance(value: Value) *ObjObjectInstance {
    return ObjObjectInstance.cast(value.obj()).?;
}

export fn bz_valueToObjEnumInstance(value: Value) *ObjEnumInstance {
    return ObjEnumInstance.cast(value.obj()).?;
}

export fn bz_valueToObjList(value: Value) *ObjList {
    return ObjList.cast(value.obj()).?;
}

export fn bz_valueToObjMap(value: Value) *ObjMap {
    return ObjMap.cast(value.obj()).?;
}

export fn bz_valueToObjFiber(value: Value) *ObjFiber {
    return ObjFiber.cast(value.obj()).?;
}

export fn bz_valueToObjPattern(value: Value) *ObjPattern {
    return ObjPattern.cast(value.obj()).?;
}

export fn bz_pushObjectInstance(vm: *VM, payload: *ObjObjectInstance) void {
    vm.push(payload.toValue());
}

export fn bz_getEnumCase(vm: *VM, enum_value: Value, case_name_value: Value) Value {
    const self = ObjEnum.cast(enum_value.obj()).?;
    const case = ObjString.cast(case_name_value.obj()).?.string;
    var case_index: usize = 0;

    for (self.type_def.resolved_type.?.Enum.cases.items, 0..) |enum_case, index| {
        if (std.mem.eql(u8, case, enum_case)) {
            case_index = index;
            break;
        }
    }

    return (vm.gc.allocateObject(
        ObjEnumInstance,
        ObjEnumInstance{
            .enum_ref = self,
            .case = @intCast(case_index),
        },
    ) catch @panic("Could not create enum case")).toValue();
}

export fn bz_getEnumCaseValue(enum_instance_value: Value) Value {
    const instance = ObjEnumInstance.cast(enum_instance_value.obj()).?;

    return instance.enum_ref.cases[instance.case];
}

export fn bz_getEnumCaseFromValue(vm: *VM, enum_value: Value, case_value: Value) Value {
    const enum_ = ObjEnum.cast(enum_value.obj()).?;

    for (enum_.cases, 0..) |case, index| {
        if (eql(case, case_value)) {
            var enum_case: *ObjEnumInstance = vm.gc.allocateObject(ObjEnumInstance, ObjEnumInstance{
                .enum_ref = enum_,
                .case = @intCast(index),
            }) catch @panic("Could not create enum instance");

            return Value.fromObj(enum_case.toObj());
        }
    }

    return Value.Null;
}

export fn bz_pushEnumInstance(vm: *VM, payload: *ObjEnumInstance) void {
    vm.push(payload.toValue());
}

export fn bz_valueToClosure(value: Value) *ObjClosure {
    return ObjClosure.cast(value.obj()).?;
}

export fn bz_toObjNative(value: Value) *ObjNative {
    return ObjNative.cast(value.obj()).?;
}

export fn bz_valueEqual(self: Value, other: Value) Value {
    return Value.fromBoolean(self.eql(other));
}

export fn bz_valueTypeOf(self: Value, vm: *VM) Value {
    return (self.typeOf(vm.gc) catch @panic("Out of memory")).toValue();
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
    return ObjMap.cast(map.obj()).?.map.get(key) orelse Value.Null;
}

export fn bz_valueIs(self: Value, type_def: Value) Value {
    return Value.fromBoolean(type_def.is(self));
}

export fn bz_setTryCtx(self: *VM) *TryCtx {
    // It would be better that this was in an ALLOCA, but with it memory keeps slowing leaking
    // Maybe the jmp throws off the stack?
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
    if ((vm.currentFrame() == null or vm.currentFrame().?.in_native_call) and vm.current_fiber.try_context != null) {
        // FIXME: close try scope

        if (builtin.os.tag == .macos or builtin.os.tag == .linux or builtin.os.windows) {
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

export fn bz_context(ctx: *NativeCtx, closure_value: Value, new_ctx: *NativeCtx, arg_count: usize) *anyopaque {
    const bound = if (closure_value.obj().obj_type == .Bound)
        ObjBoundMethod.cast(closure_value.obj()).?
    else
        null;

    const closure = if (bound) |bd|
        bd.closure
    else
        ObjClosure.cast(closure_value.obj());

    const native = if (closure == null and bound != null)
        bound.?.native
    else if (closure == null and bound == null)
        ObjNative.cast(closure_value.obj()).?
    else
        null;

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

    if (closure != null and closure.?.function.native_raw == null and closure.?.function.native == null) {
        ctx.vm.jit.?.compileFunction(ctx.vm.current_ast, closure.?) catch @panic("Failed compiling function");
    }

    return if (closure) |cls| cls.function.native_raw.? else native.?.native;
}

export fn bz_closure(
    ctx: *NativeCtx,
    function_node: Ast.Node.Index,
    native: *anyopaque,
    native_raw: *anyopaque,
) Value {
    // Set native pointers in objfunction
    var obj_function = ctx.vm.current_ast.nodes.items(.components)[function_node].Function.function.?;
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

    var it = ctx.vm.current_ast.nodes.items(.components)[function_node].Function.upvalue_binding.iterator();
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
    std.debug.print("base is {}, top is {}\n", .{ @intFromPtr(ctx.base), @intFromPtr(ctx.vm.current_fiber.stack_top) });
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

        return (vm.gc.copyString(&[_]u8{string.string[@as(usize, @intCast(new_index))]}) catch @panic("Could not iterate on string")).toValue();
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
        return list.items.items[@as(usize, @intCast(new_index))];
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

export fn bz_clone(vm: *VM, value: Value) Value {
    return vm.cloneValue(value) catch @panic("Could not clone value");
}

export fn dumpInt(value: u64) void {
    std.debug.print("-> {x}\n", .{value});
}

export fn bz_zigType(vm: *VM, ztype: [*]const u8, len: usize, expected_type: *Value) ?*const ZigType {
    const zdef = vm.ffi.parseTypeExpr(ztype[0..len]) catch return null;

    if (zdef) |uzdef| {
        expected_type.* = uzdef.type_def.toValue();

        return &uzdef.zig_type;
    }

    return null;
}

pub export fn bz_zigValueSize(ztype: *ZigType) usize {
    return ztype.size();
}

export fn bz_checkBuzzType(
    vm: *VM,
    value: Value,
    ztype: *ZigType,
    btype: Value,
) bool {
    if (!bz_valueIs(value, btype).boolean()) {
        var err = std.ArrayList(u8).init(vm.gc.allocator);
        defer err.deinit();

        const typedef = ObjTypeDef.cast(btype.obj()).?.toStringAlloc(vm.gc.allocator) catch @panic("Out of memory");
        defer typedef.deinit();

        err.writer().print(
            "Expected buzz value of type `{s}` to match FFI type `{}`",
            .{
                typedef.items,
                ztype.*,
            },
        ) catch @panic("Out of memory");

        bz_pushError(
            vm,
            "ffi.FFITypeMismatchError",
            "ffi.FFITypeMismatchError".len,
            err.items.ptr,
            err.items.len,
        );

        return false;
    }

    return true;
}

export fn bz_readZigValueFromBuffer(
    vm: *VM,
    ztype: *ZigType,
    at: usize,
    buf: [*]u8,
    len: usize,
) Value {
    var buffer = std.ArrayList(u8).fromOwnedSlice(vm.gc.allocator, buf[0..len]);
    buffer.capacity = len;

    // All those cases are necessary because bytesAsValue require arrays and not slices
    const value = switch (ztype.*) {
        .Bool => Value.fromBoolean(buffer.items[at] == 1),
        .Int => integer: {
            const offset = at * ztype.size();
            const bytes = buffer.items[offset .. offset + (ztype.Int.bits / 8)];

            switch (ztype.Int.bits) {
                64 => {
                    const userdata = vm.gc.allocateObject(
                        ObjUserData,
                        .{
                            .userdata = @as(
                                u64,
                                if (ztype.Int.signedness == .unsigned)
                                    @intCast(
                                        @as(
                                            usize,
                                            @intCast(
                                                std.mem.bytesToValue(
                                                    u64,
                                                    bytes[0..8],
                                                ),
                                            ),
                                        ),
                                    )
                                else
                                    @intCast(
                                        @as(
                                            usize,
                                            @intCast(
                                                std.mem.bytesToValue(
                                                    i64,
                                                    bytes[0..8],
                                                ),
                                            ),
                                        ),
                                    ),
                            ),
                        },
                    ) catch @panic("Out of memory");

                    break :integer userdata.toValue();
                },
                32 => {
                    if (ztype.Int.signedness == .signed) {
                        break :integer Value.fromInteger(
                            std.mem.bytesToValue(
                                i32,
                                bytes[0..4],
                            ),
                        );
                    } else {
                        break :integer Value.fromFloat(
                            @floatFromInt(
                                std.mem.bytesToValue(
                                    u32,
                                    bytes[0..4],
                                ),
                            ),
                        );
                    }
                },
                16 => {
                    break :integer Value.fromInteger(
                        if (ztype.Int.signedness == .signed)
                            std.mem.bytesToValue(
                                i16,
                                bytes[0..2],
                            )
                        else
                            std.mem.bytesToValue(
                                u16,
                                bytes[0..2],
                            ),
                    );
                },
                8 => {
                    break :integer Value.fromInteger(
                        if (ztype.Int.signedness == .signed)
                            std.mem.bytesToValue(
                                i8,
                                bytes[0..1],
                            )
                        else
                            std.mem.bytesToValue(
                                u8,
                                bytes[0..1],
                            ),
                    );
                },
                else => break :integer Value.Void,
            }
        },
        .Float => float: {
            const offset = at * ztype.size();
            const bytes = buffer.items[offset .. offset + (ztype.Float.bits / 8)];

            switch (ztype.Float.bits) {
                32 => {
                    break :float Value.fromFloat(
                        @as(
                            f64,
                            @floatCast(
                                std.mem.bytesToValue(f32, bytes[0..4]),
                            ),
                        ),
                    );
                },
                64 => {
                    break :float Value.fromFloat(
                        std.mem.bytesToValue(f64, bytes[0..8]),
                    );
                },
                else => break :float Value.Void,
            }
        },
        .Pointer,
        .Fn,
        .Opaque,
        => ptr: {
            const offset = at * 8;
            const bytes = buffer.items[offset .. offset + 8];

            const userdata = vm.gc.allocateObject(
                ObjUserData,
                .{
                    .userdata = std.mem.bytesToValue(u64, bytes[0..8]),
                },
            ) catch @panic("Out of memory");

            break :ptr userdata.toValue();
        },
        else => Value.Void,
    };

    return value;
}

export fn bz_writeZigValueToBuffer(
    vm: *VM,
    value: Value,
    ztype: *const ZigType,
    at: usize,
    buf: [*]u8,
    capacity: usize,
) void {
    var buffer = std.ArrayList(u8).fromOwnedSlice(vm.gc.allocator, buf[0..capacity]);
    buffer.capacity = capacity;

    switch (ztype.*) {
        // Does C ABI has u1 booleans?
        .Bool => {
            const unwrapped = (if (value.boolean())
                @as(u8, 1)
            else
                @as(u8, 0));
            var bytes = std.mem.asBytes(&unwrapped);

            buffer.replaceRange(at, bytes.len, bytes) catch @panic("Out of memory");
        },
        // Integer can just be truncated
        .Int => {
            switch (ztype.Int.bits) {
                64 => {
                    const unwrapped = ObjUserData.cast(value.obj()).?.userdata;
                    var bytes = std.mem.asBytes(&unwrapped);

                    buffer.replaceRange(at, bytes.len, bytes) catch @panic("Out of memory");
                },
                1...32 => {
                    const unwrapped = value.integer();
                    var bytes = std.mem.asBytes(&unwrapped)[0..(ztype.Int.bits / 8)];

                    buffer.replaceRange(at, bytes.len, bytes) catch @panic("Out of memory");
                },
                else => {},
            }
        },
        .Float => switch (ztype.Float.bits) {
            32 => {
                const unwrapped = @as(f32, @floatCast(value.float()));
                var bytes = std.mem.asBytes(&unwrapped);

                buffer.replaceRange(at, bytes.len, bytes) catch @panic("Out of memory");
            },
            64 => {
                const unwrapped = value.float();
                var bytes = std.mem.asBytes(&unwrapped);

                buffer.replaceRange(at, bytes.len, bytes) catch @panic("Out of memory");
            },
            else => {},
        },
        .Null, .Void => {},
        .Optional => if (!value.isNull())
            bz_writeZigValueToBuffer(
                vm,
                value,
                ztype.Optional.child,
                at,
                buf,
                capacity,
            )
        else {},
        .Pointer,
        .Fn,
        .Opaque,
        => {
            const unwrapped = ObjUserData.cast(value.obj()).?.userdata;
            var bytes = std.mem.asBytes(&unwrapped);

            buffer.replaceRange(at, bytes.len, bytes) catch @panic("Out of memory");
        },
        else => {},
    }
}

export fn bz_containerGet(vm: *VM, value: Value, field: [*]const u8, len: usize) Value {
    const container = ObjForeignContainer.cast(value.obj()).?;
    // Oh right that's beautiful enough...
    return container.type_def.resolved_type.?.ForeignContainer.fields.get(field[0..len]).?.getter(
        vm,
        container.data.ptr,
    );
}

export fn bz_containerSet(vm: *VM, value: Value, field: [*]const u8, len: usize, new_value: Value) void {
    const container = ObjForeignContainer.cast(value.obj()).?;
    // Oh right that's beautiful enough...
    return container.type_def.resolved_type.?.ForeignContainer.fields.get(field[0..len]).?.setter(
        vm,
        container.data.ptr,
        new_value,
    );
}

export fn bz_containerInstance(vm: *VM, typedef_value: Value) Value {
    return (vm.gc.allocateObject(
        ObjForeignContainer,
        ObjForeignContainer.init(
            vm,
            ObjTypeDef.cast(typedef_value.obj()).?,
        ) catch @panic("Out of memory"),
    ) catch @panic("Out of memory")).toValue();
}

export fn bz_memcpy(dest: [*]u8, dest_len: usize, source: [*]u8, source_len: usize) void {
    @memcpy(dest[0..dest_len], source[0..source_len]);
}

export fn bz_containerSlice(container_value: Value, len: *usize) [*]u8 {
    const container = ObjForeignContainer.cast(container_value.obj()).?;

    len.* = container.data.len;

    return container.data.ptr;
}

export fn bz_containerFromSlice(vm: *VM, type_def: *ObjTypeDef, ptr: [*]u8, len: usize) Value {
    var container = (vm.gc.allocateObject(
        ObjForeignContainer,
        .{
            .type_def = type_def,
            .data = ptr[0..len],
        },
    ) catch @panic("Out of memory"));

    return container.toValue();
}

export fn bz_zigTypeSize(self: *ZigType) usize {
    return self.size();
}

export fn bz_zigTypeAlignment(self: *ZigType) u16 {
    return self.alignment();
}

export fn bz_serialize(vm: *VM, value: Value, error_value: *Value) Value {
    var seen = std.AutoHashMap(*Obj, void).init(vm.gc.allocator);
    defer seen.deinit();

    return value.serialize(vm, &seen) catch |err| s: {
        switch (err) {
            error.CircularReference => {
                // TODO: not ideal to do this here, will fail if serialize was not imported in the script...
                error_value.* = instanciateError(
                    vm,
                    "serialize.CircularReference",
                    "serialize.CircularReference".len,
                    null,
                    0,
                );
                break :s Value.Void;
            },
            error.NotSerializable => {
                error_value.* = instanciateError(
                    vm,
                    "serialize.NotSerializable",
                    "serialize.NotSerializable".len,
                    null,
                    0,
                );
                break :s Value.Void;
            },
            else => @panic("Out of memory"),
        }
    };
}

export fn bz_currentFiber(vm: *VM) Value {
    return (vm.gc.allocateObject(
        ObjFiber,
        .{
            .fiber = vm.current_fiber,
        },
    ) catch @panic("Out of memory")).toValue();
}
