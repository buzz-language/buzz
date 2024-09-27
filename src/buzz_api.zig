const std = @import("std");
const builtin = @import("builtin");
const buzz_builtin = @import("builtin.zig");
const _vm = @import("vm.zig");
const VM = _vm.VM;
const TryCtx = _vm.TryCtx;
const _obj = @import("obj.zig");
const _value = @import("value.zig");
const Integer = _value.Integer;
const Double = _value.Double;
const Value = _value.Value;
const memory = @import("memory.zig");
const Parser = @import("Parser.zig");
const CodeGen = @import("Codegen.zig");
const BuildOptions = @import("build_options");
const is_wasm = builtin.cpu.arch.isWasm();
const jmp = if (!is_wasm) @import("jmp.zig").jmp else void;
const dumpStack = @import("disassembler.zig").dumpStack;
const ZigType = @import("zigtypes.zig").Type;
const Token = @import("Token.zig");
const Ast = @import("Ast.zig");
const io = @import("io.zig");

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

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
const ObjRange = _obj.ObjRange;
const NativeFn = _obj.NativeFn;
const NativeCtx = _obj.NativeCtx;
const TypeRegistry = memory.TypeRegistry;
const GarbageCollector = memory.GarbageCollector;

var gpa = std.heap.GeneralPurposeAllocator(.{
    .safety = builtin.mode == .Debug,
}){};

pub const allocator: std.mem.Allocator = if (builtin.mode == .Debug or is_wasm)
    gpa.allocator()
else if (BuildOptions.mimalloc)
    @import("mimalloc.zig").mim_allocator
else if (!is_wasm)
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
        io.print("...", .{});
        return;
    }

    if (value.isNull()) {
        io.print("null", .{});
    } else if (!value.isObj() or seen.get(value.obj()) != null) {
        const string = value.toStringAlloc(vm.gc.allocator) catch std.ArrayList(u8).init(vm.gc.allocator);
        defer string.deinit();

        io.print("{s}", .{string.items});
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

                io.print("{s}", .{string.items});
            },

            .UpValue => {
                const upvalue = ObjUpValue.cast(value.obj()).?;

                valueDump(if (upvalue.closed != null) upvalue.closed.? else upvalue.location.*, vm, seen, depth);
            },

            .String => {
                const string = ObjString.cast(value.obj()).?;

                io.print("\"{s}\"", .{string.string});
            },

            .Pattern => {
                const pattern = ObjPattern.cast(value.obj()).?;

                io.print("$\"{s}\"", .{pattern.source});
            },

            .List => {
                const list = ObjList.cast(value.obj()).?;

                io.print("[ ", .{});
                for (list.items.items) |item| {
                    valueDump(item, vm, seen, depth + 1);
                    io.print(", ", .{});
                }
                io.print("]", .{});
            },

            .Range => {
                const range = ObjRange.cast(value.obj()).?;

                io.print("{}..{}", .{ range.low, range.high });
            },

            .Map => {
                const map = ObjMap.cast(value.obj()).?;

                io.print("{{ ", .{});
                var it = map.map.iterator();
                while (it.next()) |kv| {
                    const key = kv.key_ptr.*;

                    valueDump(key, vm, seen, depth + 1);
                    io.print(": ", .{});
                    valueDump(kv.value_ptr.*, vm, seen, depth + 1);
                    io.print(", ", .{});
                }
                io.print("}}", .{});
            },

            .Enum => {
                const enumeration = ObjEnum.cast(value.obj()).?;
                const enum_type_def = enumeration.type_def.resolved_type.?.Enum;

                io.print(
                    "enum({s}) {s} {{ ",
                    .{
                        enum_type_def.name.string,
                        enum_type_def.name.string,
                    },
                );
                for (enum_type_def.cases.items, 0..) |case, i| {
                    io.print("{s} -> ", .{case});
                    valueDump(enumeration.cases[i], vm, seen, depth);
                    io.print(", ", .{});
                }
                io.print("}}", .{});
            },

            .Object => {
                const object = ObjObject.cast(value.obj()).?;
                const object_def = object.type_def.resolved_type.?.Object;

                io.print("object", .{});
                if (object_def.conforms_to.count() > 0) {
                    io.print("(", .{});
                    var it = object_def.conforms_to.iterator();
                    while (it.next()) |kv| {
                        io.print("{s}, ", .{kv.key_ptr.*.resolved_type.?.Protocol.name.string});
                    }
                    io.print(")", .{});
                }

                io.print(" {s} {{ ", .{object_def.name.string});

                {
                    var it = object_def.fields.iterator();
                    while (it.next()) |kv| {
                        const field = kv.value_ptr.*;
                        const field_type_str = field
                            .type_def
                            .toStringAlloc(vm.gc.allocator) catch std.ArrayList(u8).init(vm.gc.allocator);
                        defer field_type_str.deinit();

                        if (!field.method) {
                            io.print(
                                "{s}{s}{s}: {s}",
                                .{
                                    if (field.static) "static" else "",
                                    if (field.constant) "const" else "",
                                    field.name,
                                    field_type_str.items,
                                },
                            );

                            if (if (field.static)
                                object.fields[field.index]
                            else if (field.has_default)
                                object.defaults[field.index]
                            else
                                null) |v|
                            {
                                io.print(" = ", .{});
                                valueDump(v, vm, seen, depth + 1);
                            }

                            io.print(", ", .{});
                        } else {
                            io.print(
                                "{s}, ",
                                .{field_type_str.items},
                            );
                        }
                    }
                }

                io.print("}}", .{});
            },

            .ObjectInstance => {
                const object_instance = ObjObjectInstance.cast(value.obj()).?;
                const object_def = object_instance.type_def.resolved_type.?.ObjectInstance
                    .resolved_type.?.Object;

                io.print(
                    "{s}{{ ",
                    .{
                        if (object_instance.object) |object|
                            object.type_def.resolved_type.?.Object.name.string
                        else
                            ".",
                    },
                );
                for (object_instance.fields, 0..) |field, i| {
                    var it = object_def.fields.iterator();
                    const field_def: ObjObject.ObjectDef.Field = field_def: {
                        while (it.next()) |kv| {
                            const f = kv.value_ptr.*;
                            if (f.index == i and !f.static and !f.method) {
                                break :field_def f;
                            }
                        }

                        unreachable;
                    };

                    io.print("{s} = ", .{field_def.name});
                    valueDump(field, vm, seen, depth + 1);
                    io.print(", ", .{});
                }
                io.print("}}", .{});
            },

            .ForeignContainer => {
                const foreign = ObjForeignContainer.cast(value.obj()).?;
                const foreign_def = foreign.type_def.resolved_type.?.ForeignContainer;

                io.print(
                    "{s}{{ ",
                    .{foreign_def.name.string},
                );

                var it = foreign_def.fields.iterator();
                while (it.next()) |kv| {
                    io.print("{s} = ", .{kv.key_ptr.*});
                    valueDump(
                        kv.value_ptr.*.getter(
                            vm,
                            foreign.data.ptr,
                        ),
                        vm,
                        seen,
                        depth + 1,
                    );
                    io.print(", ", .{});
                }
                io.print("}}", .{});
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

export fn bz_valueToForeignContainerPtr(value: Value) [*]u8 {
    return ObjForeignContainer.cast(value.obj()).?.data.ptr;
}

/// Converts a c string to a *ObjString
export fn bz_stringToValue(vm: *VM, string: ?[*]const u8, len: usize) Value {
    return ((if (string != null and len > 0)
        vm.gc.copyString(string.?[0..len])
    else
        vm.gc.copyString("")) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_stringToValueZ(vm: *VM, string: [*:0]const u8) Value {
    // Keeping the sentinel
    return (vm.gc.copyString(string[0..(std.mem.len(string) + 1)]) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_stringConcat(obj_string: Value, other: Value, vm: *VM) Value {
    return (ObjString.cast(obj_string.obj()).?.concat(
        vm,
        ObjString.cast(other.obj()).?,
    ) catch @panic("Could not concat strings")).toValue();
}

export fn bz_stringSubscript(obj_string: Value, index_value: Value, checked: bool, vm: *VM) Value {
    const str = ObjString.cast(obj_string.obj()).?;
    const index = index_value.integer();

    if (index < 0) {
        if (checked) {
            return Value.Null;
        }

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
        if (checked) {
            return Value.Null;
        }

        bz_throw(
            vm,
            (vm.gc.copyString("Out of bound str access.") catch unreachable).toValue(),
        );

        return Value.Error;
    }
}

export fn bz_valueCastToString(value: Value, vm: *VM) Value {
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
    return vm.gc.type_registry.str_type.toValue();
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
    ) catch {
        vm.panic("Out of memory");
        unreachable;
    };

    return typedef.toValue();
}

export fn bz_containerTypeSize(type_def: Value) usize {
    return ObjTypeDef.cast(type_def.obj()).?
        .resolved_type.?
        .ForeignContainer
        .zig_type.size();
}

export fn bz_containerTypeAlign(type_def: Value) usize {
    return ObjTypeDef.cast(type_def.obj()).?
        .resolved_type.?
        .ForeignContainer
        .zig_type.alignment();
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

export fn bz_newRange(vm: *VM, low: Integer, high: Integer) Value {
    return Value.fromObj((vm.gc.allocateObject(
        ObjRange,
        ObjRange{
            .low = low,
            .high = high,
        },
    ) catch @panic("Could not create range")).toObj());
}

export fn bz_newList(vm: *VM, of_type: Value) Value {
    const list_def: ObjList.ListDef = ObjList.ListDef.init(
        vm.gc.allocator,
        ObjTypeDef.cast(of_type.obj()).?,
    );

    const list_def_union: ObjTypeDef.TypeUnion = .{
        .List = list_def,
    };

    const list_def_type: *ObjTypeDef = vm.gc.type_registry.getTypeDef(ObjTypeDef{
        .def_type = .List,
        .optional = false,
        .resolved_type = list_def_union,
    }) catch @panic("Could not create list");

    return (vm.gc.allocateObject(
        ObjList,
        ObjList.init(vm.gc.allocator, list_def_type) catch @panic("Out of memory"),
    ) catch @panic("Could not create list")).toValue();
}

export fn bz_listAppend(list: Value, value: Value, vm: *VM) void {
    ObjList.cast(list.obj()).?.rawAppend(vm.gc, value) catch @panic("Could not add element to list");
}

export fn bz_listGet(self: Value, index: i32, checked: bool) Value {
    const list = ObjList.cast(self.obj()).?;

    if (index < 0 or index >= list.items.items.len) {
        if (checked) {
            return Value.Null;
        } else {
            @panic("Out of bound list access.");
        }
    }

    return list.items.items[@intCast(index)];
}

export fn bz_listSet(self: Value, index: usize, value: Value, vm: *VM) void {
    ObjList.cast(self.obj()).?.set(
        vm.gc,
        index,
        value,
    ) catch @panic("Could not set element in list");
}

export fn bz_listLen(self: Value) usize {
    return ObjList.cast(self.obj()).?.items.items.len;
}

export fn bz_listConcat(list: Value, other_list: Value, vm: *VM) Value {
    const left: *ObjList = ObjList.cast(list.obj()).?;
    const right: *ObjList = ObjList.cast(other_list.obj()).?;

    var new_list = std.ArrayListUnmanaged(Value){};
    new_list.appendSlice(vm.gc.allocator, left.items.items) catch @panic("Could not concatenate lists");
    new_list.appendSlice(vm.gc.allocator, right.items.items) catch @panic("Could not concatenate lists");

    return (vm.gc.allocateObject(
        ObjList,
        ObjList{
            .type_def = left.type_def,
            .methods = left.methods,
            .items = new_list,
        },
    ) catch @panic("Could not concatenate lists")).toValue();
}

export fn bz_mapConcat(map: Value, other_map: Value, vm: *VM) Value {
    const left = ObjMap.cast(map.obj()).?;
    const right = ObjMap.cast(other_map.obj()).?;

    var new_map = left.map.clone(vm.gc.allocator) catch @panic("Could not concatenate maps");
    var it = right.map.iterator();
    while (it.next()) |entry| {
        new_map.put(
            vm.gc.allocator,
            entry.key_ptr.*,
            entry.value_ptr.*,
        ) catch @panic("Could not concatenate maps");
    }

    return (vm.gc.allocateObject(ObjMap, ObjMap{
        .type_def = left.type_def,
        .methods = left.methods,
        .map = new_map,
    }) catch @panic("Could not concatenate maps")).toValue();
}

export fn bz_newUserData(vm: *VM, userdata: u64) Value {
    return (vm.gc.allocateObject(
        ObjUserData,
        ObjUserData{ .userdata = userdata },
    ) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_getUserDataPtr(userdata: Value) u64 {
    return ObjUserData.cast(userdata.obj()).?.userdata;
}

export fn bz_newVM(self: *VM) ?*VM {
    const vm = self.gc.allocator.create(VM) catch {
        return null;
    };
    var gc = self.gc.allocator.create(GarbageCollector) catch {
        return null;
    };
    // FIXME: should share strings between gc
    gc.* = GarbageCollector.init(self.gc.allocator) catch {
        vm.panic("Out of memory");
        unreachable;
    };
    gc.type_registry = TypeRegistry.init(gc) catch {
        vm.panic("Out of memory");
        unreachable;
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

export fn bz_panic(vm: *VM, msg: [*]const u8, len: usize) void {
    vm.panic(msg[0..len]);
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
    const obj: *Obj = value.obj();
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
    method_idx: usize,
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
        false,
        method_idx,
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
    closure_value: Value,
    arguments: ?[*]const *const Value,
    len: u8,
    catch_value: ?*Value,
) void {
    std.debug.assert(closure_value.obj().obj_type == .Closure);

    self.push(closure_value);
    var i: usize = 0;
    while (i < len) : (i += 1) {
        self.push(arguments.?[i].*);
    }

    // TODO: catch properly
    self.callValue(
        closure_value,
        len,
        if (catch_value) |v| v.* else null,
    ) catch unreachable;

    // If not compiled, run it with the VM loop
    if (closure_value.obj().access(
        ObjClosure,
        .Closure,
        self.gc,
    ).?.function.native == null) {
        self.run();
    }
}

export fn bz_newQualifiedObjectInstance(self: *VM, qualified_name: [*]const u8, len: usize) Value {
    const object = ObjObject.cast(bz_getQualified(self, qualified_name, len).obj()).?;

    const instance: *ObjObjectInstance = self.gc.allocateObject(
        ObjObjectInstance,
        ObjObjectInstance.init(
            self,
            object,
            object.type_def.toInstance(self.gc.allocator, &self.gc.type_registry) catch {
                self.panic("Out of memory");
                unreachable;
            },
            self.gc,
        ) catch @panic("Out of memory"),
    ) catch {
        @panic("Could not create error");
    };

    // Set instance fields with default values
    for (object.defaults, 0..) |default, idx| {
        if (default) |udefault| {
            instance.setField(
                self.gc,
                idx,
                self.cloneValue(udefault) catch @panic("Could not set object property"),
            ) catch @panic("Could not set object property");
        }
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
    const instance = bz_newQualifiedObjectInstance(vm, qualified_name, len);

    if (message) |msg| {
        const obj_instance = ObjObjectInstance.cast(instance.obj()).?;
        const object_def = obj_instance.type_def.resolved_type.?.ObjectInstance
            .resolved_type.?.Object
            .fields;

        if (object_def.get("message")) |field| {
            obj_instance.fields[field.index] = (vm.gc.copyString(msg[0..mlen]) catch {
                vm.panic("Out of memory");
                unreachable;
            }).toValue();
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
    const case = self.gc.copyString(case_str[0..case_len]) catch @panic("Could not create error payload");

    self.push(
        bz_getEnumCase(
            bz_getQualified(self, qualified_name, name_len),
            case.toValue(),
            self,
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

export fn bz_newObjectInstance(vm: *VM, object_value: Value, typedef_value: Value) Value {
    const object = if (object_value.isObj()) ObjObject.cast(object_value.obj()).? else null;
    const typedef = ObjTypeDef.cast(typedef_value.obj()).?;

    const instance = vm.gc.allocateObject(
        ObjObjectInstance,
        ObjObjectInstance.init(
            vm,
            object,
            typedef,
            vm.gc,
        ) catch @panic("Out of memory"),
    ) catch @panic("Could not instanciate object");

    // If not anonymous, set default fields
    if (object) |uobject| {
        for (uobject.defaults, 0..) |default, idx| {
            if (default) |udefault| {
                instance.setField(
                    vm.gc,
                    idx,
                    vm.cloneValue(udefault) catch @panic("Could not set object field"),
                ) catch @panic("Could not set object field");
            }
        }
    }

    return instance.toValue();
}

export fn bz_getObjectField(object_value: Value, field_idx: usize) Value {
    return ObjObject.cast(object_value.obj()).?.fields[field_idx];
}

export fn bz_setObjectField(object_value: Value, field_idx: usize, value: Value, vm: *VM) void {
    ObjObject.cast(object_value.obj()).?.setField(
        vm.gc,
        field_idx,
        value,
    ) catch @panic("Could not set static field");
}

export fn bz_setObjectInstanceProperty(instance_value: Value, field_idx: usize, value: Value, vm: *VM) void {
    ObjObjectInstance.cast(instance_value.obj()).?.setField(
        vm.gc,
        field_idx,
        value,
    ) catch @panic("Could not set instance field");
}

export fn bz_getObjectInstanceProperty(instance_value: Value, property_idx: usize) Value {
    return ObjObjectInstance.cast(instance_value.obj()).?
        .fields[property_idx];
}

export fn bz_getObjectInstanceMethod(instance_value: Value, method_idx: usize, bind: bool, vm: *VM) Value {
    const method = ObjObjectInstance.cast(instance_value.obj()).?
        .object.?
        .fields[method_idx];

    return if (bind)
        bz_bindMethod(
            vm,
            method,
            method,
            Value.Null,
        )
    else
        method;
}

export fn bz_getProtocolMethod(instance_value: Value, method_name: Value, vm: *VM) Value {
    const instance = instance_value.obj().access(
        ObjObjectInstance,
        .ObjectInstance,
        vm.gc,
    ).?;

    const name = method_name.obj()
        .access(ObjString, .String, vm.gc).?
        .string;

    const method_idx = instance.type_def.resolved_type.?.ObjectInstance
        .resolved_type.?.Object
        .fields.get(name).?.index;

    return bz_bindMethod(
        vm,
        instance_value,
        instance.object.?.fields[method_idx],
        Value.Null,
    );
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

export fn bz_getEnumCase(enum_value: Value, case_name_value: Value, vm: *VM) Value {
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

export fn bz_getEnumInstanceValue(enum_instance_value: Value) Value {
    const instance = ObjEnumInstance.cast(enum_instance_value.obj()).?;

    return instance.enum_ref.cases[instance.case];
}

export fn bz_getEnumCaseFromValue(enum_value: Value, case_value: Value, vm: *VM) Value {
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

export fn bz_valueEqual(self: Value, other: Value) Value {
    return Value.fromBoolean(self.eql(other));
}

export fn bz_valueTypeOf(self: Value, vm: *VM) Value {
    return (self.typeOf(vm.gc) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_newMap(vm: *VM, map_type: Value) Value {
    var map: *ObjMap = vm.gc.allocateObject(
        ObjMap,
        ObjMap.init(
            vm.gc.allocator,
            ObjTypeDef.cast(map_type.obj()).?,
        ) catch @panic("Could not create map"),
    ) catch @panic("Could not create map");

    return Value.fromObj(map.toObj());
}

export fn bz_mapSet(map: Value, key: Value, value: Value, vm: *VM) void {
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
    if (is_wasm) {
        unreachable;
    }

    // It would be better that this was in an ALLOCA, but with it memory keeps slowing leaking
    // Maybe the jmp throws off the stack?
    const try_ctx = self.gc.allocator.create(TryCtx) catch @panic("Could not create try context");
    try_ctx.* = .{
        .previous = self.current_fiber.try_context,
        .env = undefined,
    };

    self.current_fiber.try_context = try_ctx;

    // Doesn't setjmp itself so it is done in the correct function context

    return try_ctx;
}

export fn bz_popTryCtx(self: *VM) void {
    if (is_wasm) {
        unreachable;
    }

    if (self.current_fiber.try_context) |try_ctx| {
        self.current_fiber.try_context = try_ctx.previous;

        self.gc.allocator.destroy(try_ctx);
    }
}

// Like bz_throw but assumes the error payload is already on the stack
export fn bz_rethrow(vm: *VM) void {
    if (is_wasm) {
        unreachable;
    }

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
    if (is_wasm) {
        unreachable;
    }

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

    if (BuildOptions.recursive_call_limit) |recursive_call_limit| {
        // If recursive call, update counter
        ctx.vm.current_fiber.recursive_count = if (closure != null and closure.?.function == ctx.vm.current_fiber.current_compiled_function)
            ctx.vm.current_fiber.recursive_count + 1
        else
            0;

        if (ctx.vm.current_fiber.recursive_count > recursive_call_limit) {
            ctx.vm.throw(
                VM.Error.ReachedMaximumRecursiveCall,
                (ctx.vm.gc.copyString("Maximum recursive call reached") catch @panic("Maximum recursive call reached")).toValue(),
                null,
                null,
            ) catch @panic("Maximum recursive call reached");
        }
    }

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

    if (closure) |cls| {
        ctx.vm.current_fiber.current_compiled_function = cls.function;
    }

    return if (closure) |cls| cls.function.native_raw.? else native.?.native;
}

export fn bz_closure(
    ctx: *NativeCtx,
    function_node: Ast.Node.Index,
    native: *anyopaque,
    native_raw: *anyopaque,
) Value {
    if (is_wasm) {
        unreachable;
    }

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

    // ctx.vm.jit.?.compiled_functions.put(closure, {}) catch @panic("Could not get closure");

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
    io.print("base is {}, top is {}\n", .{ @intFromPtr(ctx.base), @intFromPtr(ctx.vm.current_fiber.stack_top) });
    io.print("#{}:\n", .{off});
    dumpStack(ctx.vm);
}

export fn bz_getStringProperty(vm: *VM, string_value: Value, property_idx: usize, bind: bool) Value {
    const method = ObjString.member(vm, property_idx) catch @panic("Out of memory").?;

    return if (bind)
        bz_bindMethod(
            vm,
            string_value,
            Value.Null,
            method,
        )
    else
        method;
}

export fn bz_getPatternProperty(vm: *VM, pattern_value: Value, property_idx: usize, bind: bool) Value {
    const method = ObjPattern.member(vm, property_idx) catch @panic("Could not get pattern method");

    return if (bind)
        bz_bindMethod(
            vm,
            pattern_value,
            Value.Null,
            method,
        )
    else
        method;
}

export fn bz_getFiberProperty(vm: *VM, fiber_value: Value, property_idx: usize, bind: bool) Value {
    const method = ObjFiber.member(vm, property_idx) catch @panic("Could not get fiber method");

    return if (bind)
        bz_bindMethod(
            vm,
            fiber_value,
            Value.Null,
            method,
        )
    else
        method;
}

export fn bz_getListProperty(vm: *VM, list_value: Value, property_idx: usize, bind: bool) Value {
    const method = ObjList.cast(list_value.obj()).?
        .member(vm, property_idx) catch @panic("Could not get list method");

    return if (bind)
        bz_bindMethod(
            vm,
            list_value,
            Value.Null,
            method,
        )
    else
        method;
}

export fn bz_getRangeProperty(vm: *VM, range_value: Value, property_idx: usize, bind: bool) Value {
    const method = ObjRange.member(vm, property_idx) catch @panic("Out of memory");

    return if (bind)
        bz_bindMethod(
            vm,
            range_value,
            Value.Null,
            method,
        )
    else
        method;
}

export fn bz_getMapProperty(vm: *VM, map_value: Value, property_idx: usize, bind: bool) Value {
    const method = ObjMap.cast(map_value.obj()).?
        .member(vm, property_idx) catch @panic("Could not get map method");

    return if (bind)
        bz_bindMethod(
            vm,
            map_value,
            Value.Null,
            method,
        )
    else
        method;
}

export fn bz_stringNext(string_value: Value, index: *Value, vm: *VM) Value {
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

export fn bz_listNext(list_value: Value, index: *Value, vm: *VM) Value {
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

export fn bz_rangeNext(range_value: Value, index_slot: Value) Value {
    const range = ObjRange.cast(range_value.obj()).?;

    if (index_slot.integerOrNull()) |index| {
        if (range.low < range.high) {
            return if (index + 1 >= range.high)
                Value.Null
            else
                Value.fromInteger(index + 1);
        } else {
            return if (index - 1 <= range.high)
                Value.Null
            else
                Value.fromInteger(index - 1);
        }
    }

    return Value.fromInteger(range.low);
}

export fn bz_mapNext(map_value: Value, key: *Value) Value {
    const map = ObjMap.cast(map_value.obj()).?;

    if (map.rawNext(if (key.isNull()) null else key.*)) |new_key| {
        key.* = new_key;

        return map.map.get(new_key) orelse Value.Null;
    }

    key.* = Value.Null;
    return Value.Null;
}

export fn bz_enumNext(enum_value: Value, case: Value, vm: *VM) Value {
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

export fn bz_zigType(vm: *VM, ztype: [*]const u8, len: usize, expected_type: *Value) ?*const ZigType {
    if (is_wasm) {
        return null;
    }

    const zdef = vm.ffi.parseTypeExpr(ztype[0..len]) catch return null;

    if (zdef) |uzdef| {
        expected_type.* = uzdef.type_def.toValue();

        return &uzdef.zig_type;
    }

    return null;
}

export fn bz_foreignContainerGet(value: Value, field_idx: usize, vm: *VM) Value {
    const container = ObjForeignContainer.cast(value.obj()).?;

    return container.type_def.resolved_type.?
        .ForeignContainer
        .fields.values()[field_idx]
        .getter(
        vm,
        container.data.ptr,
    );
}

export fn bz_foreignContainerSet(value: Value, field_idx: usize, new_value: Value, vm: *VM) void {
    const container = ObjForeignContainer.cast(value.obj()).?;
    // Oh right that's beautiful enough...
    return container.type_def.resolved_type.?.ForeignContainer
        .fields.values()[field_idx]
        .setter(
        vm,
        container.data.ptr,
        new_value,
    );
}

export fn bz_newForeignContainerInstance(vm: *VM, typedef_value: Value) Value {
    return (vm.gc.allocateObject(
        ObjForeignContainer,
        ObjForeignContainer.init(
            vm,
            ObjTypeDef.cast(typedef_value.obj()).?,
        ) catch {
            vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_foreignContainerSlice(container_value: Value, len: *usize) [*]u8 {
    const container = ObjForeignContainer.cast(container_value.obj()).?;

    len.* = container.data.len;

    return container.data.ptr;
}

export fn bz_newForeignContainerFromSlice(vm: *VM, type_def: Value, ptr: [*]u8, len: usize) Value {
    var container = (vm.gc.allocateObject(
        ObjForeignContainer,
        .{
            .type_def = ObjTypeDef.cast(type_def.obj()).?,
            .data = ptr[0..len],
        },
    ) catch {
        vm.panic("Out of memory");
        unreachable;
    });

    return container.toValue();
}

export fn bz_zigTypeSize(self: *ZigType) usize {
    return self.size();
}

export fn bz_zigTypeAlignment(self: *ZigType) u16 {
    return self.alignment();
}

export fn bz_zigTypeToCString(self: *ZigType, vm: *VM) [*:0]const u8 {
    var out = std.ArrayList(u8).init(vm.gc.allocator);

    out.writer().print("{}\x00", .{self.*}) catch {
        vm.panic("Out of memory");
        unreachable;
    };

    return @ptrCast(out.items.ptr);
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
            else => {
                vm.panic("Out of memory");
                unreachable;
            },
        }
    };
}

export fn bz_currentFiber(vm: *VM) Value {
    return (vm.gc.allocateObject(
        ObjFiber,
        .{
            .fiber = vm.current_fiber,
        },
    ) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_memcpy(dest: [*]u8, dest_len: usize, source: [*]u8, source_len: usize) void {
    @memcpy(dest[0..dest_len], source[0..source_len]);
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
                    ) catch {
                        vm.panic("Out of memory");
                        unreachable;
                    };

                    break :integer userdata.toValue();
                },
                32 => {
                    if (ztype.Int.signedness == .signed) {
                        break :integer Value.fromInteger(
                            std.mem.bytesToValue(
                                Integer,
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
        .Double => double: {
            const offset = at * ztype.size();
            const bytes = buffer.items[offset .. offset + (ztype.Double.bits / 8)];

            switch (ztype.Double.bits) {
                32 => {
                    break :double Value.fromFloat(
                        @floatCast(
                            std.mem.bytesToValue(f32, bytes[0..4]),
                        ),
                    );
                },
                64 => {
                    break :double Value.fromFloat(
                        std.mem.bytesToValue(Double, bytes[0..8]),
                    );
                },
                else => break :double Value.Void,
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
            ) catch {
                vm.panic("Out of memory");
                unreachable;
            };

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
            const bytes = std.mem.asBytes(&unwrapped);

            buffer.replaceRange(at, bytes.len, bytes) catch {
                vm.panic("Out of memory");
                unreachable;
            };
        },
        // Integer can just be truncated
        .Int => {
            switch (ztype.Int.bits) {
                64 => {
                    const unwrapped = ObjUserData.cast(value.obj()).?.userdata;
                    const bytes = std.mem.asBytes(&unwrapped);

                    buffer.replaceRange(at, bytes.len, bytes) catch {
                        vm.panic("Out of memory");
                        unreachable;
                    };
                },
                1...32 => {
                    const unwrapped = value.integer();
                    const bytes = std.mem.asBytes(&unwrapped)[0..(ztype.Int.bits / 8)];

                    buffer.replaceRange(at, bytes.len, bytes) catch {
                        vm.panic("Out of memory");
                        unreachable;
                    };
                },
                else => {},
            }
        },
        .Double => switch (ztype.Double.bits) {
            32 => {
                const unwrapped = @as(f32, @floatCast(value.double()));
                const bytes = std.mem.asBytes(&unwrapped);

                buffer.replaceRange(at, bytes.len, bytes) catch {
                    vm.panic("Out of memory");
                    unreachable;
                };
            },
            64 => {
                const unwrapped = value.double();
                const bytes = std.mem.asBytes(&unwrapped);

                buffer.replaceRange(at, bytes.len, bytes) catch {
                    vm.panic("Out of memory");
                    unreachable;
                };
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
            const bytes = std.mem.asBytes(&unwrapped);

            buffer.replaceRange(at, bytes.len, bytes) catch {
                vm.panic("Out of memory");
                unreachable;
            };
        },
        else => {},
    }
}
