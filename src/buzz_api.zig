const std = @import("std");
const builtin = @import("builtin");
const buzz_builtin = @import("builtin.zig");
const vmachine = @import("vm.zig");
const VM = vmachine.VM;
const TryCtx = vmachine.TryCtx;
const ImportRegistry = vmachine.ImportRegistry;
const o = @import("obj.zig");
const v = @import("value.zig");
const GC = @import("GC.zig");
const TypeRegistry = @import("TypeRegistry.zig");
const Parser = @import("Parser.zig");
const CodeGen = @import("Codegen.zig");
const BuildOptions = @import("build_options");
const is_wasm = builtin.cpu.arch.isWasm();
const jmp = if (!is_wasm) @import("jmp.zig") else void;
const dumpStack = @import("disassembler.zig").dumpStack;
const ZigType = @import("zigtypes.zig").Type;
const Token = @import("Token.zig");
const Ast = @import("Ast.zig");
const io = @import("io.zig");
const Pool = @import("pool.zig").Pool;

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

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
export fn bz_push(vm: *VM, value: v.Value) callconv(.c) void {
    vm.push(value);
}

/// Pop a Value from the stack and returns it
export fn bz_pop(vm: *VM) callconv(.c) v.Value {
    return vm.pop();
}

/// Peeks at the stack at [distance] from the stack top
export fn bz_peek(vm: *VM, dist: u32) callconv(.c) v.Value {
    return vm.peek(dist);
}

/// Absolute access to the stack.
export fn bz_at(vm: *VM, at: u32) callconv(.c) v.Value {
    if (at < vm.current_fiber.stack.len) {
        return vm.current_fiber.stack[at];
    }

    return .Null;
}

/// Converts a value to a string
export fn bz_valueToString(vm: *VM, value: v.Value, len: *usize) callconv(.c) ?[*]const u8 {
    if (!value.isObj() or value.obj().obj_type != .String) {
        return null;
    }

    const string = vm.gc.ptr(o.ObjString, .idx(value.obj().index)).?.string;

    len.* = string.len;

    return if (string.len > 0) @as([*]const u8, @ptrCast(string)) else null;
}

export fn bz_valueToCString(vm: *VM, value: v.Value) callconv(.c) ?[*:0]const u8 {
    if (!value.isObj() or value.obj().obj_type != .String) {
        return null;
    }

    return @ptrCast(vm.gc.ptr(o.ObjString, .idx(value.obj().index)).?.string.ptr);
}

fn valueDump(value: v.Value, vm: *VM, seen: *std.AutoHashMapUnmanaged(o.ObjIdx, void), depth: usize) void {
    if (depth > 50) {
        io.print("...", .{});
        return;
    }

    if (value.isNull()) {
        io.print("null", .{});
    } else if (!value.isObj() or seen.get(value.obj()) != null) {
        const string = value.toStringAlloc(vm.gc) catch @panic("Out of memory");
        defer vm.gc.allocator.free(string);

        io.print("{s}", .{string});
    } else {
        seen.put(vm.gc.allocator, value.obj(), {}) catch unreachable;

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
                const string = value.toStringAlloc(vm.gc) catch @panic("Out of memory");
                defer vm.gc.allocator.free(string);

                io.print("{s}", .{string});
            },

            .UpValue => {
                const upvalue = vm.gc.ptr(o.ObjUpValue, .idx(value.obj().index)).?;

                valueDump(
                    if (upvalue.closed != null) upvalue.closed.? else upvalue.location.*,
                    vm,
                    seen,
                    depth,
                );
            },

            .String => {
                const string = vm.gc.ptr(o.ObjString, .idx(value.obj().index)).?;

                io.print("\"{s}\"", .{string.string});
            },

            .Pattern => {
                const pattern = vm.gc.ptr(o.ObjPattern, .idx(value.obj().index)).?;

                io.print("$\"{s}\"", .{pattern.source});
            },

            .List => {
                const list = vm.gc.ptr(o.ObjList, .idx(value.obj().index)).?;

                io.print(
                    "{s}[ ",
                    .{
                        if (list.type_def.get(vm.gc).resolved_type.?.List.mutable)
                            "mut "
                        else
                            "",
                    },
                );
                for (list.items.items) |item| {
                    valueDump(item, vm, seen, depth + 1);
                    io.print(", ", .{});
                }
                io.print("]", .{});
            },

            .Range => {
                const range = vm.gc.ptr(o.ObjRange, .idx(value.obj().index)).?;

                io.print("{}..{}", .{ range.low, range.high });
            },

            .Map => {
                const map = vm.gc.ptr(o.ObjMap, .idx(value.obj().index)).?;

                io.print(
                    "{s}{{ ",
                    .{
                        if (map.type_def.get(vm.gc).resolved_type.?.Map.mutable)
                            "mut "
                        else
                            "",
                    },
                );
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
                const enumeration = vm.gc.ptr(o.ObjEnum, .idx(value.obj().index)).?;
                const enum_type_def = vm.gc.ptr(o.ObjTypeDef, enumeration.type_def).?
                    .resolved_type.?.Enum;
                const backing_type_name = o.ObjTypeDef.toStringAlloc(
                    enum_type_def.enum_type,
                    vm.gc,
                    false,
                ) catch @panic("Out of memory");
                defer vm.gc.allocator.free(backing_type_name);

                io.print(
                    "enum<{s}> {s} {{ ",
                    .{
                        backing_type_name,
                        enum_type_def.name.get(vm.gc).string,
                    },
                );
                for (enum_type_def.cases, 0..) |case, i| {
                    io.print("{s} -> ", .{case});
                    valueDump(enumeration.cases[i], vm, seen, depth);
                    io.print(", ", .{});
                }
                io.print("}}", .{});
            },

            .Object => {
                const object = vm.gc.ptr(o.ObjObject, .idx(value.obj().index)).?;
                const object_def = vm.gc.ptr(o.ObjTypeDef, object.type_def).?
                    .resolved_type.?.Object;

                io.print("object", .{});
                if (object_def.conforms_to.count() > 0) {
                    io.print("(", .{});
                    var it = object_def.conforms_to.iterator();
                    while (it.next()) |kv| {
                        io.print(
                            "{s}, ",
                            .{
                                kv.key_ptr.*.get(vm.gc)
                                    .resolved_type.?.Protocol
                                    .name.get(vm.gc).string,
                            },
                        );
                    }
                    io.print(")", .{});
                }

                io.print(
                    " {s} {{ ",
                    .{
                        object_def.name.get(vm.gc).string,
                    },
                );

                {
                    var it = object_def.fields.iterator();
                    while (it.next()) |kv| {
                        const field = kv.value_ptr.*;
                        const field_type_str = o.ObjTypeDef.toStringAlloc(
                            field.type_def,
                            vm.gc,
                            false,
                        ) catch @panic("Out of memory");
                        defer vm.gc.allocator.free(field_type_str);

                        if (!field.method) {
                            io.print(
                                "{s}{s}{s}: {s}",
                                .{
                                    if (field.static) "static" else "",
                                    if (field.final) "final" else "",
                                    field.name,
                                    field_type_str,
                                },
                            );

                            if (if (field.static)
                                object.fields[field.index]
                            else if (field.has_default)
                                object.defaults[field.index]
                            else
                                null) |val|
                            {
                                io.print(" = ", .{});
                                valueDump(val, vm, seen, depth + 1);
                            }

                            io.print(", ", .{});
                        } else {
                            io.print(
                                "{s}{s}, ",
                                .{
                                    if (field.mutable) "mut " else "",
                                    field_type_str,
                                },
                            );
                        }
                    }
                }

                io.print("}}", .{});
            },

            .ObjectInstance => {
                const object_instance = vm.gc.ptr(o.ObjObjectInstance, .idx(value.obj().index)).?;
                const instance_type_def = object_instance.type_def.get(vm.gc);
                const object_def = instance_type_def
                    .resolved_type.?.ObjectInstance.of.get(vm.gc).resolved_type.?.Object;

                io.print(
                    "{s}{s}{{ ",
                    .{
                        if (instance_type_def.resolved_type.?.ObjectInstance.mutable)
                            "mut "
                        else
                            "",
                        if (object_instance.object) |object|
                            object.get(vm.gc)
                                .type_def.get(vm.gc)
                                .resolved_type.?.Object.name.get(vm.gc).string
                        else
                            ".",
                    },
                );

                var it = object_def.fields.iterator();
                while (it.next()) |field| {
                    io.print("{s} = ", .{field.value_ptr.name});
                    valueDump(
                        object_instance.fields[field.value_ptr.index],
                        vm,
                        seen,
                        depth + 1,
                    );
                    io.print(", ", .{});
                }

                io.print("}}", .{});
            },

            .ForeignContainer => {
                const foreign = vm.gc.ptr(o.ObjForeignContainer, .idx(value.obj().index)).?;
                const foreign_def = foreign.type_def.get(vm.gc)
                    .resolved_type.?.ForeignContainer;

                io.print(
                    "{s}{{ ",
                    .{foreign_def.name.get(vm.gc).string},
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
pub export fn bz_valueDump(vm: *VM, value: v.Value) callconv(.c) void {
    var seen = std.AutoHashMapUnmanaged(o.ObjIdx, void).empty;
    defer seen.deinit(vm.gc.allocator);

    valueDump(value, vm, &seen, 0);
}

export fn bz_valueToForeignContainerPtr(vm: *VM, value: v.Value) callconv(.c) [*]u8 {
    return vm.gc.ptr(o.ObjForeignContainer, .idx(value.obj().index)).?.data.ptr;
}

/// Converts a c string to a *o.ObjString
export fn bz_stringToValue(vm: *VM, string: ?[*]const u8, len: usize) callconv(.c) v.Value {
    return .fromObj(
        .{
            .index = ((if (string != null and len > 0)
                vm.gc.copyString(string.?[0..len])
            else
                vm.gc.copyString("")) catch {
                vm.panic("Out of memory");
                unreachable;
            }).index,
            .obj_type = .String,
        },
    );
}

export fn bz_stringToValueZ(vm: *VM, string: [*:0]const u8) callconv(.c) v.Value {
    // Keeping the sentinel
    return .fromObj(
        .{
            .index = (vm.gc.copyString(string[0..(std.mem.len(string) + 1)]) catch {
                vm.panic("Out of memory");
                unreachable;
            }).index,
            .obj_type = .String,
        },
    );
}

export fn bz_stringConcat(vm: *VM, obj_string: v.Value, other: v.Value) callconv(.c) v.Value {
    return .fromObj(
        .{
            .index = (vm.gc.ptr(o.ObjString, .idx(obj_string.obj().index)).?.concat(
                vm,
                vm.gc.ptr(o.ObjString, .idx(other.obj().index)).?,
            ) catch @panic("Could not concat strings")).index,
            .obj_type = .String,
        },
    );
}

export fn bz_stringSubscript(vm: *VM, obj_string: v.Value, index_value: v.Value, checked: bool) callconv(.c) v.Value {
    const str = vm.gc.ptr(o.ObjString, .idx(obj_string.obj().index)).?;
    const index = index_value.integer();

    if (index < 0) {
        if (checked) {
            return .Null;
        }

        bz_throw(
            vm,
            .fromObj(
                .{
                    .index = (vm.gc.copyString("Out of bound string access.") catch unreachable).index,
                    .obj_type = .String,
                },
            ),
        );

        return .Error;
    }

    const str_index: usize = @intCast(index);

    if (str_index < str.string.len) {
        return .fromObj(
            .{
                .index = (vm.gc.copyString(&([_]u8{str.string[str_index]})) catch unreachable).index,
                .obj_type = .String,
            },
        );
    } else {
        if (checked) {
            return .Null;
        }

        bz_throw(
            vm,
            .fromObj(
                .{
                    .index = (vm.gc.copyString("Out of bound str access.") catch unreachable).index,
                    .obj_type = .String,
                },
            ),
        );

        return .Error;
    }
}

export fn bz_valueCastToString(vm: *VM, value: v.Value) callconv(.c) v.Value {
    const str = value.toStringAlloc(vm.gc) catch
        @panic("Out of memory");
    defer vm.gc.allocator.free(str);

    return .fromObj(
        .{
            .index = (vm.gc.copyString(str) catch @panic("Out of memory")).index,
            .obj_type = .String,
        },
    );
}

// Other stuff

// Type helpers

/// Returns the [str] type
export fn bz_stringType(vm: *VM) callconv(.c) v.Value {
    return .fromObj(
        .{
            .index = vm.gc.type_registry.str_type.index,
            .obj_type = .Type,
        },
    );
}

export fn bz_intType(vm: *VM) callconv(.c) v.Value {
    return .fromObj(
        .{
            .index = vm.gc.type_registry.int_type.index,
            .obj_type = .Type,
        },
    );
}

export fn bz_listType(vm: *VM, item_type: v.Value, mutable: bool) callconv(.c) v.Value {
    return .fromObj(
        .{
            .index = (vm.gc.type_registry.getTypeDef(
                .{
                    .def_type = .List,
                    .optional = false,
                    .resolved_type = .{
                        .List = o.ObjList.ListDef.init(
                            .idx(item_type.obj().index),
                            mutable,
                        ),
                    },
                },
            ) catch {
                vm.panic("Out of memory");
                unreachable;
            }).index,
            .obj_type = .Type,
        },
    );
}

export fn bz_mapType(vm: *VM, key_type: v.Value, value_type: v.Value, mutable: bool) callconv(.c) v.Value {
    return .fromObj(
        .{
            .index = (vm.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Map,
                    .optional = false,
                    .resolved_type = .{
                        .Map = o.ObjMap.MapDef.init(
                            .idx(key_type.obj().index),
                            .idx(value_type.obj().index),
                            mutable,
                        ),
                    },
                },
            ) catch {
                vm.panic("Out of memory");
                unreachable;
            }).index,
            .obj_type = .Type,
        },
    );
}

export fn bz_containerTypeSize(vm: *VM, type_def: v.Value) callconv(.c) usize {
    return vm.gc.ptr(o.ObjTypeDef, .idx(type_def.obj().index)).?
        .resolved_type.?
        .ForeignContainer
        .zig_type.size();
}

export fn bz_containerTypeAlign(vm: *VM, type_def: v.Value) callconv(.c) usize {
    return vm.gc.ptr(o.ObjTypeDef, .idx(type_def.obj().index)).?
        .resolved_type.?
        .ForeignContainer
        .zig_type.alignment();
}

export fn bz_allocated(self: *VM) callconv(.c) usize {
    return self.gc.bytes_allocated;
}

export fn bz_collect(self: *VM) callconv(.c) void {
    self.gc.collectGarbage() catch @panic("Could not collect");
}

export fn bz_newRange(vm: *VM, low: i64, high: i64) callconv(.c) v.Value {
    return .fromObj(
        .{
            .index = (vm.gc.allocateObject(
                o.ObjRange{
                    .low = @truncate(low),
                    .high = @truncate(high),
                },
            ) catch @panic("Could not create range")).index,
            .obj_type = .Range,
        },
    );
}

export fn bz_newList(vm: *VM, list_type: v.Value) callconv(.c) v.Value {
    return .fromObj(
        .{
            .index = (vm.gc.allocateObject(
                o.ObjList.init(
                    vm.gc.allocator,
                    .idx(list_type.obj().index),
                ) catch @panic("Out of memory"),
            ) catch @panic("Could not create list")).index,
            .obj_type = .List,
        },
    );
}

export fn bz_listAppend(vm: *VM, list: v.Value, value: v.Value) callconv(.c) void {
    o.ObjList.rawAppend(
        .idx(list.obj().index),
        vm.gc,
        value,
    ) catch @panic("Could not add element to list");
}

export fn bz_listGet(vm: *VM, list_value: v.Value, index: i64, checked: bool) callconv(.c) v.Value {
    const list = vm.gc.ptr(o.ObjList, .idx(list_value.obj().index)).?;

    if (index < 0 or index >= list.items.items.len) {
        if (checked) {
            return .Null;
        } else {
            @panic("Out of bound list access.");
        }
    }

    return list.items.items[@intCast(index)];
}

export fn bz_listSet(vm: *VM, list_value: v.Value, index: usize, value: v.Value) callconv(.c) void {
    o.ObjList.set(
        .idx(list_value.obj().index),
        vm.gc,
        index,
        value,
    ) catch @panic("Could not set element in list");
}

export fn bz_listLen(vm: *VM, list_value: v.Value) callconv(.c) usize {
    return vm.gc.ptr(o.ObjList, .idx(list_value.obj().index)).?
        .items.items.len;
}

export fn bz_listConcat(vm: *VM, list: v.Value, other_list: v.Value) callconv(.c) v.Value {
    const left = vm.gc.ptr(o.ObjList, .idx(list.obj().index)).?;
    const right = vm.gc.ptr(o.ObjList, .idx(other_list.obj().index)).?;

    var new_list = std.ArrayList(v.Value){};
    new_list.appendSlice(vm.gc.allocator, left.items.items) catch @panic("Could not concatenate lists");
    new_list.appendSlice(vm.gc.allocator, right.items.items) catch @panic("Could not concatenate lists");

    return .fromObj(
        .{
            .index = (vm.gc.allocateObject(
                o.ObjList{
                    .type_def = left.type_def,
                    .methods = left.methods,
                    .items = new_list,
                },
            ) catch @panic("Could not concatenate lists")).index,
            .obj_type = .List,
        },
    );
}

export fn bz_mapConcat(vm: *VM, map: v.Value, other_map: v.Value) callconv(.c) v.Value {
    const left = vm.gc.ptr(o.ObjMap, .idx(map.obj().index)).?;
    const right = vm.gc.ptr(o.ObjMap, .idx(other_map.obj().index)).?;

    var new_map = left.map.clone(vm.gc.allocator) catch @panic("Could not concatenate maps");
    var it = right.map.iterator();
    while (it.next()) |entry| {
        new_map.put(
            vm.gc.allocator,
            entry.key_ptr.*,
            entry.value_ptr.*,
        ) catch @panic("Could not concatenate maps");
    }

    return .fromObj(
        .{
            .index = (vm.gc.allocateObject(
                o.ObjMap{
                    .type_def = left.type_def,
                    .methods = left.methods,
                    .map = new_map,
                },
            ) catch @panic("Could not concatenate maps")).index,
            .obj_type = .Map,
        },
    );
}

export fn bz_newUserData(vm: *VM, userdata: u64) callconv(.c) v.Value {
    return .fromObj(
        .{
            .index = (vm.gc.allocateObject(
                o.ObjUserData{ .userdata = userdata },
            ) catch {
                vm.panic("Out of memory");
                unreachable;
            }).index,
            .obj_type = .UserData,
        },
    );
}

export fn bz_getUserDataPtr(vm: *VM, userdata: v.Value) callconv(.c) u64 {
    return vm.gc.ptr(o.ObjUserData, .idx(userdata.obj().index)).?.userdata;
}

export fn bz_newVM() *VM {
    const vm = allocator.create(VM) catch @panic("Out of memory");
    var gc = allocator.create(GC) catch @panic("Out of memory");
    // FIXME: should share strings between gc
    gc.* = GC.init(allocator) catch @panic("Out of memory");
    gc.type_registry = TypeRegistry.init(gc) catch @panic("Out of memory");
    const import_registry = allocator.create(ImportRegistry) catch @panic("Out of memory");
    import_registry.* = .{};

    // FIXME: give reference to JIT?
    vm.* = VM.init(
        gc,
        import_registry,
        .Run,
        null,
    ) catch @panic("Out of memory");

    return vm;
}

export fn bz_deinitVM(self: *VM) callconv(.c) void {
    self.deinit();
    self.import_registry.deinit(self.gc.allocator);
    self.gc.deinit();
}

export fn bz_panic(vm: *VM, msg: [*]const u8, len: usize) callconv(.c) void {
    vm.panic(msg[0..len]);
}

export fn bz_run(
    self: *VM,
    source: ?[*]const u8,
    source_len: usize,
    file_name: ?[*]const u8,
    file_name_len: usize,
) callconv(.c) bool {
    if (source == null or file_name_len == 0 or source_len == 0 or file_name_len == 0) {
        return false;
    }

    var imports = std.StringHashMapUnmanaged(Parser.ScriptImport){};
    var strings = std.StringHashMapUnmanaged(*o.ObjString).empty;
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
        self.debugger != null,
    );
    defer {
        codegen.deinit();
        imports.deinit(self.gc.allocator);
        parser.deinit();
        strings.deinit(self.gc.allocator);
    }

    if (parser.parse(source.?[0..source_len], null, file_name.?[0..file_name_len]) catch null) |ast| {
        const ast_slice = ast.slice();
        if (codegen.generate(ast_slice) catch null) |function| {
            self.interpret(ast_slice, function, null) catch {
                return false;
            };

            return true;
        }
    }

    return false;
}

fn calleeIsCompiled(vm: *VM, value: v.Value) bool {
    const obj = value.obj();
    return switch (obj.obj_type) {
        .Bound => bound: {
            const bound = vm.gc.ptr(o.ObjBoundMethod, .idx(obj.index)).?;

            if (bound.native != null) {
                break :bound true;
            }

            if (bound.closure) |cls| {
                break :bound cls.get(vm.gc)
                    .function.get(vm.gc)
                    .native != null;
            }

            break :bound false;
        },
        .Closure => vm.gc.ptr(o.ObjClosure, .idx(obj.index)).?
            .function.get(vm.gc)
            .native != null,
        .Native => true,
        else => false,
    };
}

pub export fn bz_invoke(
    vm: *VM,
    instance: v.Value,
    method_idx: usize,
    arguments: ?[*]const *const v.Value,
    len: u8,
    catch_value: ?*v.Value,
) callconv(.c) void {
    vm.push(instance);
    var i: usize = 0;
    while (i < len) : (i += 1) {
        vm.push(arguments.?[i].*);
    }

    const was_in_native_call = vm.currentFrame().?.in_native_call;
    vm.currentFrame().?.in_native_call = true;

    // TODO: catch properly
    const callee = vm.invoke(
        false,
        method_idx,
        len,
        if (catch_value) |val| val.* else null,
        false,
    ) catch unreachable;

    // If not compiled, run it with the VM loop
    if (!calleeIsCompiled(vm, callee)) {
        // TODO: catch properly
        vm.run() catch unreachable;
    }

    vm.currentFrame().?.in_native_call = was_in_native_call;
}

pub export fn bz_call(
    vm: *VM,
    closure_value: v.Value,
    arguments: ?[*]const *const v.Value,
    len: u8,
    catch_value: ?*v.Value,
) callconv(.c) bool {
    std.debug.assert(closure_value.obj().obj_type == .Closure);

    vm.push(closure_value);
    var i: usize = 0;
    while (i < len) : (i += 1) {
        vm.push(arguments.?[i].*);
    }

    vm.callValue(
        closure_value,
        len,
        if (catch_value) |val| val.* else null,
    ) catch return false;

    // If not compiled, run it with the VM loop
    if (vm.gc.ptr(o.ObjClosure, .idx(closure_value.obj().index)).?
        .function.get(vm.gc)
        .native == null)
    {
        vm.run() catch return false;

        return true;
    }

    return true;
}

export fn bz_newQualifiedObjectInstance(vm: *VM, qualified_name: [*]const u8, len: usize, mutable: bool) callconv(.c) v.Value {
    const object_idx = Pool(o.ObjObject).Idx.idx(
        bz_getQualified(vm, qualified_name, len).obj().index,
    );
    const object = object_idx.get(vm.gc);

    const instance = vm.gc.allocateObject(
        o.ObjObjectInstance.init(
            vm,
            object_idx,
            o.ObjTypeDef.toInstance(
                object.type_def,
                &vm.gc.type_registry,
                mutable,
            ) catch {
                vm.panic("Out of memory");
                unreachable;
            },
            vm.gc,
        ) catch @panic("Out of memory"),
    ) catch {
        @panic("Could not create error");
    };

    // Set instance fields with default values
    for (object.defaults, 0..) |default, idx| {
        if (default) |udefault| {
            o.ObjObjectInstance.setField(
                instance,
                vm.gc,
                idx,
                vm.cloneValue(udefault) catch @panic("Could not set object property"),
            ) catch @panic("Could not set object property");
        }
    }

    return .fromObj(
        .{
            .index = instance.index,
            .obj_type = .ObjectInstance,
        },
    );
}

fn instanciateError(
    vm: *VM,
    qualified_name: [*]const u8,
    len: usize,
    message: ?[*]const u8,
    mlen: usize,
) v.Value {
    const instance = bz_newQualifiedObjectInstance(
        vm,
        qualified_name,
        len,
        false,
    );

    if (message) |msg| {
        const obj_instance = vm.gc.ptr(o.ObjObjectInstance, .idx(instance.obj().index)).?;
        const object_def = obj_instance.type_def.get(vm.gc)
            .resolved_type.?.ObjectInstance.of.get(vm.gc)
            .resolved_type.?.Object
            .fields;

        if (object_def.get("message")) |field| {
            obj_instance.fields[field.index] = .fromObj(
                .{
                    .index = (vm.gc.copyString(msg[0..mlen]) catch {
                        vm.panic("Out of memory");
                        unreachable;
                    }).index,
                    .obj_type = .String,
                },
            );
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
) callconv(.c) void {
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

export fn bz_pushErrorEnum(
    self: *VM,
    qualified_name: [*]const u8,
    name_len: usize,
    case_str: [*]const u8,
    case_len: usize,
) callconv(.c) void {
    const case = self.gc.copyString(case_str[0..case_len]) catch @panic("Could not create error payload");

    self.push(
        bz_getEnumCase(
            self,
            bz_getQualified(self, qualified_name, name_len),
            .fromObj(.{ .index = case.index, .obj_type = .String }),
        ),
    );
}

export fn bz_getQualified(vm: *VM, qualified_name: [*]const u8, len: usize) callconv(.c) v.Value {
    for (vm.globals.items) |global| {
        if (global.isObj()) {
            switch (global.obj().obj_type) {
                .Enum => {
                    const obj_enum = vm.gc.ptr(o.ObjEnum, .idx(global.obj().index)).?;

                    if (std.mem.eql(
                        u8,
                        qualified_name[0..len],
                        obj_enum.type_def.get(vm.gc)
                            .resolved_type.?.Enum.qualified_name.get(vm.gc).string,
                    )) {
                        return global;
                    }
                },
                .Object => {
                    const obj_enum = vm.gc.ptr(o.ObjObject, .idx(global.obj().index)).?;

                    if (std.mem.eql(
                        u8,
                        qualified_name[0..len],
                        obj_enum.type_def.get(vm.gc)
                            .resolved_type.?.Object.qualified_name.get(vm.gc).string,
                    )) {
                        return global;
                    }
                },
                else => {},
            }
        }
    }

    std.debug.panic("bz_getQualified no name: {s}", .{qualified_name[0..len]});
}

export fn bz_newObjectInstance(vm: *VM, object_value: v.Value, typedef_value: v.Value) callconv(.c) v.Value {
    const object_idx = if (object_value.isObj())
        Pool(o.ObjObject).Idx.idx(object_value.obj().index)
    else
        null;
    const object = if (object_idx) |idx|
        idx.get(vm.gc)
    else
        null;
    const typedef = Pool(o.ObjTypeDef).Idx.idx(typedef_value.obj().index);

    const instance = vm.gc.allocateObject(
        o.ObjObjectInstance.init(
            vm,
            object_idx,
            typedef,
            vm.gc,
        ) catch @panic("Out of memory"),
    ) catch @panic("Could not instanciate object");

    // If not anonymous, set default fields
    if (object) |uobject| {
        for (uobject.defaults, 0..) |default, idx| {
            if (default) |udefault| {
                o.ObjObjectInstance.setField(
                    instance,
                    vm.gc,
                    idx,
                    vm.cloneValue(udefault) catch @panic("Could not set object field"),
                ) catch @panic("Could not set object field");
            }
        }
    }

    return .fromObj(
        .{
            .index = instance.index,
            .obj_type = .ObjectInstance,
        },
    );
}

export fn bz_getObjectField(vm: *VM, object_value: v.Value, field_idx: usize) callconv(.c) v.Value {
    return vm.gc.ptr(o.ObjObject, .idx(object_value.obj().index)).?.fields[field_idx];
}

export fn bz_setObjectField(vm: *VM, object_value: v.Value, field_idx: usize, value: v.Value) callconv(.c) void {
    o.ObjObject.setField(
        .idx(object_value.obj().index),
        vm.gc,
        field_idx,
        value,
    ) catch @panic("Could not set static field");
}

export fn bz_setObjectInstanceProperty(vm: *VM, instance_value: v.Value, field_idx: usize, value: v.Value) callconv(.c) void {
    o.ObjObjectInstance.setField(
        .idx(instance_value.obj().index),
        vm.gc,
        field_idx,
        value,
    ) catch @panic("Could not set instance field");
}

export fn bz_getObjectInstanceProperty(vm: *VM, instance_value: v.Value, property_idx: usize) callconv(.c) v.Value {
    return vm.gc.ptr(o.ObjObjectInstance, .idx(instance_value.obj().index)).?
        .fields[property_idx];
}

export fn bz_getObjectInstanceMethod(vm: *VM, instance_value: v.Value, method_idx: usize, bind: bool) callconv(.c) v.Value {
    const method = vm.gc.ptr(o.ObjObjectInstance, .idx(instance_value.obj().index)).?
        .object.?.get(vm.gc)
        .fields[method_idx];

    return if (bind)
        bz_bindMethod(
            vm,
            method,
            method,
            .Null,
        )
    else
        method;
}

export fn bz_getProtocolMethod(vm: *VM, instance_value: v.Value, method_name: v.Value) callconv(.c) v.Value {
    const instance = vm.gc.ptr(o.ObjObjectInstance, .idx(instance_value.obj().index)).?;

    const name = vm.gc.ptr(o.ObjString, .idx(method_name.obj().index)).?
        .string;

    const method_idx = instance.type_def.get(vm.gc)
        .resolved_type.?.ObjectInstance.of.get(vm.gc)
        .resolved_type.?.Object
        .fields.get(name).?.index;

    return bz_bindMethod(
        vm,
        instance_value,
        instance.object.?.get(vm.gc)
            .fields[method_idx],
        .Null,
    );
}

export fn bz_bindMethod(vm: *VM, receiver: v.Value, method_value: v.Value, native_value: v.Value) callconv(.c) v.Value {
    return .fromObj(
        .{
            .index = (vm.gc.allocateObject(
                o.ObjBoundMethod{
                    .receiver = receiver,
                    .closure = if (method_value.isObj())
                        .idx(method_value.obj().index)
                    else
                        null,
                    .native = if (native_value.isObj())
                        .idx(native_value.obj().index)
                    else
                        null,
                },
            ) catch @panic("Could not bind method")).index,
            .obj_type = .Bound,
        },
    );
}

export fn bz_getEnumCase(vm: *VM, enum_value: v.Value, case_name_value: v.Value) callconv(.c) v.Value {
    const self = vm.gc.ptr(o.ObjEnum, .idx(enum_value.obj().index)).?;
    const case = vm.gc.ptr(o.ObjString, .idx(case_name_value.obj().index)).?.string;
    var case_index: usize = 0;

    for (self.type_def.get(vm.gc).resolved_type.?.Enum.cases, 0..) |enum_case, index| {
        if (std.mem.eql(u8, case, enum_case)) {
            case_index = index;
            break;
        }
    }

    return .fromObj(
        .{
            .index = (vm.gc.allocateObject(
                o.ObjEnumInstance{
                    .enum_ref = .idx(enum_value.obj().index),
                    .case = @intCast(case_index),
                },
            ) catch @panic("Could not create enum case")).index,
            .obj_type = .EnumInstance,
        },
    );
}

export fn bz_getEnumInstanceValue(vm: *VM, enum_instance_value: v.Value) callconv(.c) v.Value {
    const instance = vm.gc.ptr(o.ObjEnumInstance, .idx(enum_instance_value.obj().index)).?;

    return instance.enum_ref.get(vm.gc)
        .cases[instance.case];
}

export fn bz_getEnumCaseFromValue(vm: *VM, enum_value: v.Value, case_value: v.Value) callconv(.c) v.Value {
    const enum_ = vm.gc.ptr(o.ObjEnum, .idx(enum_value.obj().index)).?;

    for (enum_.cases, 0..) |case, index| {
        if (v.Value.eql(case, case_value, vm.gc)) {
            const enum_case = vm.gc.allocateObject(
                o.ObjEnumInstance{
                    .enum_ref = .idx(enum_value.obj().index),
                    .case = @intCast(index),
                },
            ) catch @panic("Could not create enum instance");

            return .fromObj(
                .{
                    .index = enum_case.index,
                    .obj_type = .EnumInstance,
                },
            );
        }
    }

    return .Null;
}

export fn bz_valueEqual(vm: *VM, self: v.Value, other: v.Value) callconv(.c) v.Value {
    return .fromBoolean(self.eql(other, vm.gc));
}

export fn bz_valueTypeOf(vm: *VM, self: v.Value) callconv(.c) v.Value {
    return .fromObj(
        .{
            .index = (self.typeOf(vm.gc) catch {
                vm.panic("Out of memory");
                unreachable;
            }).index,
            .obj_type = .Type,
        },
    );
}

export fn bz_newMap(vm: *VM, map_type: v.Value) callconv(.c) v.Value {
    const map = vm.gc.allocateObject(
        o.ObjMap.init(
            vm.gc.allocator,
            .idx(map_type.obj().index),
        ) catch @panic("Could not create map"),
    ) catch @panic("Could not create map");

    return .fromObj(
        .{
            .index = map.index,
            .obj_type = .Map,
        },
    );
}

export fn bz_mapSet(vm: *VM, map: v.Value, key: v.Value, value: v.Value) callconv(.c) void {
    o.ObjMap.set(
        .idx(map.obj().index),
        vm.gc,
        key,
        value,
    ) catch @panic("Could not set map element");
}

export fn bz_mapGet(vm: *VM, map: v.Value, key: v.Value) callconv(.c) v.Value {
    return vm.gc.ptr(o.ObjMap, .idx(map.obj().index)).?
        .map.get(key) orelse
        .Null;
}

export fn bz_valueIs(vm: *VM, self: v.Value, type_def: v.Value) callconv(.c) v.Value {
    return .fromBoolean(
        o.ObjTypeDef.eql(
            .idx(type_def.obj().index),
            .idx(self.obj().index),
            vm.gc,
        ),
    );
}

export fn bz_setTryCtx(self: *VM) callconv(.c) *TryCtx {
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

export fn bz_popTryCtx(self: *VM) callconv(.c) void {
    if (is_wasm) {
        unreachable;
    }

    if (self.current_fiber.try_context) |try_ctx| {
        self.current_fiber.try_context = try_ctx.previous;

        self.gc.allocator.destroy(try_ctx);
    }
}

// Like bz_throw but assumes the error payload is already on the stack
export fn bz_rethrow(vm: *VM) callconv(.c) void {
    if (is_wasm) {
        unreachable;
    }

    // Are we in a JIT compiled function and within a try-catch?
    if ((vm.currentFrame() == null or vm.currentFrame().?.in_native_call) and vm.current_fiber.try_context != null) {
        // FIXME: close try scope

        if (builtin.os.tag == .macos or builtin.os.tag == .linux) {
            jmp._longjmp(&vm.current_fiber.try_context.?.env, 1);
        } else {
            jmp.longjmp(&vm.current_fiber.try_context.?.env, 1);
        }

        unreachable;
    }
}

export fn bz_throw(vm: *VM, value: v.Value) callconv(.c) void {
    vm.push(value);

    bz_rethrow(vm);
}

export fn bz_closeUpValues(vm: *VM, last: *v.Value) callconv(.c) void {
    vm.closeUpValues(last);
    _ = vm.pop();
}

export fn bz_getUpValue(vm: *VM, slot: usize) callconv(.c) v.Value {
    return vm.currentClosure().?.upvalues[slot].get(vm.gc).location.*;
}

export fn bz_setUpValue(vm: *VM, slot: usize, value: v.Value) callconv(.c) void {
    vm.currentClosure().?.upvalues[slot].get(vm.gc).location.* = value;
}

export fn bz_context(ctx: *o.NativeCtx, closure_value: v.Value, new_ctx: *o.NativeCtx, arg_count: usize) callconv(.c) *anyopaque {
    if (is_wasm) {
        unreachable;
    }

    const bound = if (closure_value.obj().obj_type == .Bound)
        ctx.vm.gc.ptr(o.ObjBoundMethod, .idx(closure_value.obj().index)).?
    else
        null;

    const closure: ?Pool(o.ObjClosure).Idx = if (bound) |bd|
        bd.closure
    else
        .idx(closure_value.obj().index);

    const native: ?Pool(o.ObjNative).Idx = if (closure == null and bound != null)
        bound.?.native
    else if (closure == null and bound == null)
        .idx(closure_value.obj().index)
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

    new_ctx.* = o.NativeCtx{
        .vm = ctx.vm,
        .globals = if (closure) |cls|
            cls.get(ctx.vm.gc).globals.items.ptr
        else
            ctx.globals,
        .upvalues = if (closure) |cls|
            cls.get(ctx.vm.gc).upvalues.ptr
        else
            ctx.upvalues,
        .base = ctx.vm.current_fiber.stack_top - arg_count - 1,
        .stack_top = &ctx.vm.current_fiber.stack_top,
    };

    if (closure) |cls_idx| {
        const cls = cls_idx.get(ctx.vm.gc);
        const function = cls.function.get(ctx.vm.gc);
        if (function.native_raw == null and function.native == null) {
            ctx.vm.jit.?.compileFunction(ctx.vm.current_ast, cls_idx) catch @panic("Failed compiling function");
        }

        ctx.vm.current_fiber.current_compiled_function = cls.function;

        return function.native_raw.?;
    }

    return native.?.get(ctx.vm.gc).native;
}

export fn bz_closure(
    ctx: *o.NativeCtx,
    function_node: Ast.Node.Index,
    native: *anyopaque,
    native_raw: *anyopaque,
) callconv(.c) v.Value {
    if (is_wasm) {
        unreachable;
    }

    // Set native pointers in objfunction
    const obj_function_idx = ctx.vm.current_ast.nodes.items(.components)[function_node].Function.function.?;
    const obj_function = obj_function_idx.get(ctx.vm.gc);
    obj_function.native = native;
    obj_function.native_raw = native_raw;

    const closure_idx = ctx.vm.gc.allocateObject(
        o.ObjClosure.init(
            ctx.vm.gc.allocator,
            ctx.vm,
            obj_function_idx,
        ) catch @panic("Could not instanciate closure"),
    ) catch @panic("Could not instanciate closure");

    // On stack to prevent collection
    ctx.vm.push(
        .fromObj(
            .{
                .index = closure_idx.index,
                .obj_type = .Closure,
            },
        ),
    );

    // ctx.vm.jit.?.compiled_functions.put(closure, {}) catch @panic("Could not get closure");

    const closure = closure_idx.get(ctx.vm.gc);
    var it = ctx.vm.current_ast.nodes.items(.components)[function_node].Function.upvalue_binding.iterator();
    var i: usize = 0;
    while (it.next()) |kv| : (i += 1) {
        const is_local = kv.value_ptr.*;
        const index = kv.key_ptr.*;

        if (is_local) {
            closure.upvalues[i] = ctx.vm.captureUpvalue(&(ctx.base[index])) catch @panic("Could not instanciate closure");
        } else {
            closure.upvalues[i] = ctx.upvalues[index];
        }
    }

    return ctx.vm.pop();
}

export fn bz_dumpStack(ctx: *o.NativeCtx, off: usize) callconv(.c) void {
    io.print("base is {}, top is {}\n", .{ @intFromPtr(ctx.base), @intFromPtr(ctx.vm.current_fiber.stack_top) });
    io.print("#{}:\n", .{off});
    dumpStack(ctx.vm);
}

export fn bz_getStringProperty(vm: *VM, string_value: v.Value, property_idx: usize, bind: bool) callconv(.c) v.Value {
    const method = o.ObjString.member(vm, property_idx) catch @panic("Out of memory").?;

    return if (bind)
        bz_bindMethod(
            vm,
            string_value,
            v.Value.Null,
            method,
        )
    else
        method;
}

export fn bz_getPatternProperty(vm: *VM, pattern_value: v.Value, property_idx: usize, bind: bool) callconv(.c) v.Value {
    const method = o.ObjPattern.member(vm, property_idx) catch @panic("Could not get pattern method");

    return if (bind)
        bz_bindMethod(
            vm,
            pattern_value,
            v.Value.Null,
            method,
        )
    else
        method;
}

export fn bz_getFiberProperty(vm: *VM, fiber_value: v.Value, property_idx: usize, bind: bool) callconv(.c) v.Value {
    const method = o.ObjFiber.member(vm, property_idx) catch @panic("Could not get fiber method");

    return if (bind)
        bz_bindMethod(
            vm,
            fiber_value,
            v.Value.Null,
            method,
        )
    else
        method;
}

export fn bz_getListProperty(vm: *VM, list_value: v.Value, property_idx: usize, bind: bool) callconv(.c) v.Value {
    const method = vm.gc.ptr(o.ObjList, .idx(list_value.obj().index)).?
        .member(vm, property_idx) catch @panic("Could not get list method");

    return if (bind)
        bz_bindMethod(
            vm,
            list_value,
            v.Value.Null,
            method,
        )
    else
        method;
}

export fn bz_getRangeProperty(vm: *VM, range_value: v.Value, property_idx: usize, bind: bool) callconv(.c) v.Value {
    const method = o.ObjRange.member(vm, property_idx) catch @panic("Out of memory");

    return if (bind)
        bz_bindMethod(
            vm,
            range_value,
            v.Value.Null,
            method,
        )
    else
        method;
}

export fn bz_getMapProperty(vm: *VM, map_value: v.Value, property_idx: usize, bind: bool) callconv(.c) v.Value {
    const method = vm.gc.ptr(o.ObjMap, .idx(map_value.obj().index)).?
        .member(vm, property_idx) catch @panic("Could not get map method");

    return if (bind)
        bz_bindMethod(
            vm,
            map_value,
            v.Value.Null,
            method,
        )
    else
        method;
}

export fn bz_stringNext(vm: *VM, string_value: v.Value, index: *v.Value) callconv(.c) v.Value {
    const string = vm.gc.ptr(o.ObjString, .idx(string_value.obj().index)).?;

    if (string.next(
        vm,
        if (index.isNull()) null else index.integer(),
    ) catch @panic("Could not get next string index")) |new_index| {
        index.* = v.Value.fromInteger(new_index);

        return .fromObj(
            .{
                .index = (vm.gc.copyString(
                    &[_]u8{
                        string.string[@as(usize, @intCast(new_index))],
                    },
                ) catch
                    @panic("Could not iterate on string")).index,
                .obj_type = .String,
            },
        );
    }

    index.* = .Null;
    return .Null;
}

export fn bz_listNext(vm: *VM, list_value: v.Value, index: *v.Value) callconv(.c) v.Value {
    const list = vm.gc.ptr(o.ObjList, .idx(list_value.obj().index)).?;

    if (list.rawNext(
        vm,
        if (index.isNull()) null else index.integer(),
    ) catch @panic("Could not get next list index")) |new_index| {
        index.* = v.Value.fromInteger(new_index);
        return list.items.items[@as(usize, @intCast(new_index))];
    }

    index.* = .Null;
    return .Null;
}

export fn bz_rangeNext(vm: *VM, range_value: v.Value, index_slot: v.Value) callconv(.c) v.Value {
    const range = vm.gc.ptr(o.ObjRange, .idx(range_value.obj().index)).?;

    if (index_slot.integerOrNull()) |index| {
        if (range.low < range.high) {
            return if (index + 1 >= range.high)
                .Null
            else
                .fromInteger(index + 1);
        } else {
            return if (index - 1 <= range.high)
                .Null
            else
                .fromInteger(index - 1);
        }
    }

    return .fromInteger(range.low);
}

export fn bz_mapNext(vm: *VM, map_value: v.Value, key: *v.Value) callconv(.c) v.Value {
    const map = vm.gc.ptr(o.ObjMap, .idx(map_value.obj().index)).?;

    if (map.rawNext(if (key.isNull()) null else key.*)) |new_key| {
        key.* = new_key;

        return map.map.get(new_key) orelse .Null;
    }

    key.* = .Null;
    return .Null;
}

export fn bz_enumNext(vm: *VM, enum_value: v.Value, case: v.Value) callconv(.c) v.Value {
    if (o.ObjEnum.rawNext(
        .idx(enum_value.obj().index),
        vm,
        if (case.isNull()) null else .idx(case.obj().index),
    ) catch @panic("Could not iterate over enum")) |new_case| {
        return .fromObj(
            .{
                .index = new_case.index,
                .obj_type = .EnumInstance,
            },
        );
    }

    return .Null;
}

export fn bz_clone(vm: *VM, value: v.Value) callconv(.c) v.Value {
    return vm.cloneValue(value) catch @panic("Could not clone value");
}

export fn bz_zigType(vm: *VM, ztype: [*]const u8, len: usize, expected_type: *v.Value) callconv(.c) ?*const ZigType {
    if (is_wasm) {
        return null;
    }

    const zdef = vm.ffi.parseTypeExpr(ztype[0..len]) catch return null;

    if (zdef) |uzdef| {
        expected_type.* = .fromObj(
            .{
                .index = uzdef.type_def.index,
                .obj_type = .Type,
            },
        );

        return &uzdef.zig_type;
    }

    return null;
}

export fn bz_foreignContainerGet(vm: *VM, value: v.Value, field_idx: usize) callconv(.c) v.Value {
    const container = vm.gc.ptr(o.ObjForeignContainer, .idx(value.obj().index)).?;
    const type_def = container.type_def.get(vm.gc);

    return type_def.resolved_type.?
        .ForeignContainer
        .fields.values()[field_idx]
        .getter(
        vm,
        container.data.ptr,
    );
}

export fn bz_foreignContainerSet(vm: *VM, value: v.Value, field_idx: usize, new_value: v.Value) callconv(.c) void {
    const container = vm.gc.ptr(o.ObjForeignContainer, .idx(value.obj().index)).?;
    const type_def = container.type_def.get(vm.gc);

    return type_def.resolved_type.?.ForeignContainer
        .fields.values()[field_idx]
        .setter(
        vm,
        container.data.ptr,
        new_value,
    );
}

export fn bz_newForeignContainerInstance(vm: *VM, typedef_value: v.Value) callconv(.c) v.Value {
    return .fromObj(
        .{
            .index = (vm.gc.allocateObject(
                o.ObjForeignContainer.init(
                    vm,
                    .idx(typedef_value.obj().index),
                ) catch {
                    vm.panic("Out of memory");
                    unreachable;
                },
            ) catch {
                vm.panic("Out of memory");
                unreachable;
            }).index,
            .obj_type = .ForeignContainer,
        },
    );
}

export fn bz_foreignContainerSlice(vm: *VM, container_value: v.Value, len: *usize) callconv(.c) [*]u8 {
    const container = vm.gc.ptr(o.ObjForeignContainer, .idx(container_value.obj().index)).?;

    len.* = container.data.len;

    return container.data.ptr;
}

export fn bz_valueIsForeignContainer(_: *VM, value: v.Value) callconv(.c) bool {
    return value.isObj() and value.obj().obj_type == .ForeignContainer;
}

export fn bz_newForeignContainerFromSlice(vm: *VM, type_def: v.Value, ptr: [*]u8, len: usize) callconv(.c) v.Value {
    const container = (vm.gc.allocateObject(
        o.ObjForeignContainer{
            .type_def = .idx(type_def.obj().index),
            .data = ptr[0..len],
        },
    ) catch {
        vm.panic("Out of memory");
        unreachable;
    });

    return .fromObj(
        .{
            .index = container.index,
            .obj_type = .ForeignContainer,
        },
    );
}

export fn bz_zigTypeSize(_: *VM, self: *ZigType) callconv(.c) usize {
    return self.size();
}

export fn bz_zigTypeAlignment(_: *VM, self: *ZigType) callconv(.c) u16 {
    return self.alignment();
}

export fn bz_zigTypeToCString(vm: *VM, self: *ZigType) callconv(.c) [*:0]const u8 {
    var out = std.Io.Writer.Allocating.init(vm.gc.allocator);

    out.writer.print("{}\x00", .{self.*}) catch {
        vm.panic("Out of memory");
        unreachable;
    };

    return @ptrCast(out.written().ptr);
}

export fn bz_serialize(vm: *VM, value: v.Value, error_value: *v.Value) callconv(.c) v.Value {
    var seen = std.AutoHashMapUnmanaged(o.ObjIdx, void).empty;
    defer seen.deinit(vm.gc.allocator);

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
                break :s v.Value.Void;
            },
            error.NotSerializable => {
                error_value.* = instanciateError(
                    vm,
                    "serialize.NotSerializable",
                    "serialize.NotSerializable".len,
                    null,
                    0,
                );
                break :s v.Value.Void;
            },
            else => {
                vm.panic("Out of memory");
                unreachable;
            },
        }
    };
}

export fn bz_currentFiber(vm: *VM) callconv(.c) v.Value {
    return .fromObj(
        .{
            .index = (vm.gc.allocateObject(
                o.ObjFiber{
                    .fiber = vm.current_fiber,
                },
            ) catch {
                vm.panic("Out of memory");
                unreachable;
            }).index,
            .obj_type = .Fiber,
        },
    );
}

export fn bz_memcpy(dest: [*]u8, dest_len: usize, source: [*]u8, source_len: usize) callconv(.c) void {
    @memcpy(dest[0..dest_len], source[0..source_len]);
}

export fn bz_readZigValueFromBuffer(
    vm: *VM,
    ztype: *ZigType,
    at: usize,
    buf: [*]u8,
    len: usize,
) callconv(.c) v.Value {
    var buffer = std.ArrayList(u8).fromOwnedSlice(
        buf[0..len],
    );
    buffer.capacity = len;

    // All those cases are necessary because bytesAsValue require arrays and not slices
    const value: v.Value = switch (ztype.*) {
        .Bool => .fromBoolean(buffer.items[at] == 1),
        .Int => integer: {
            const offset = at * ztype.size();
            const bytes = buffer.items[offset .. offset + (ztype.Int.bits / 8)];

            switch (ztype.Int.bits) {
                64 => {
                    const userdata = vm.gc.allocateObject(
                        o.ObjUserData{
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

                    break :integer .fromObj(
                        .{
                            .index = userdata.index,
                            .obj_type = .UserData,
                        },
                    );
                },
                32 => {
                    if (ztype.Int.signedness == .signed) {
                        break :integer v.Value.fromInteger(
                            @intCast(
                                std.mem.bytesToValue(
                                    i32,
                                    bytes[0..4],
                                ),
                            ),
                        );
                    } else {
                        break :integer v.Value.fromDouble(
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
                    break :integer v.Value.fromInteger(
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
                    break :integer v.Value.fromInteger(
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
                else => break :integer v.Value.Void,
            }
        },
        .Double => double: {
            const offset = at * ztype.size();
            const bytes = buffer.items[offset .. offset + (ztype.Double.bits / 8)];

            switch (ztype.Double.bits) {
                32 => {
                    break :double v.Value.fromDouble(
                        @floatCast(
                            std.mem.bytesToValue(f32, bytes[0..4]),
                        ),
                    );
                },
                64 => {
                    break :double v.Value.fromDouble(
                        std.mem.bytesToValue(v.Double, bytes[0..8]),
                    );
                },
                else => break :double v.Value.Void,
            }
        },
        .Pointer,
        .Fn,
        .Opaque,
        => ptr: {
            const offset = at * 8;
            const bytes = buffer.items[offset .. offset + 8];

            const userdata = vm.gc.allocateObject(
                o.ObjUserData{
                    .userdata = std.mem.bytesToValue(u64, bytes[0..8]),
                },
            ) catch {
                vm.panic("Out of memory");
                unreachable;
            };

            break :ptr .fromObj(
                .{
                    .index = userdata.index,
                    .obj_type = .UserData,
                },
            );
        },
        else => v.Value.Void,
    };

    return value;
}

export fn bz_writeZigValueToBuffer(
    vm: *VM,
    value: v.Value,
    ztype: *const ZigType,
    at: usize,
    buf: [*]u8,
    capacity: usize,
) callconv(.c) void {
    var buffer = std.ArrayList(u8).fromOwnedSlice(
        buf[0..capacity],
    );
    buffer.capacity = capacity;

    switch (ztype.*) {
        // Does C ABI has u1 booleans?
        .Bool => {
            const unwrapped = (if (value.boolean())
                @as(u8, 1)
            else
                @as(u8, 0));
            const bytes = std.mem.asBytes(&unwrapped);

            buffer.replaceRange(vm.gc.allocator, at, bytes.len, bytes) catch {
                vm.panic("Out of memory");
                unreachable;
            };
        },
        // Integer can just be truncated
        .Int => {
            switch (ztype.Int.bits) {
                64 => {
                    const unwrapped = vm.gc.ptr(o.ObjUserData, .idx(value.obj().index)).?.userdata;
                    const bytes = std.mem.asBytes(&unwrapped);

                    buffer.replaceRange(vm.gc.allocator, at, bytes.len, bytes) catch {
                        vm.panic("Out of memory");
                        unreachable;
                    };
                },
                1...32 => {
                    const unwrapped = value.integer();
                    const bytes = std.mem.asBytes(&unwrapped)[0..(ztype.Int.bits / 8)];

                    buffer.replaceRange(vm.gc.allocator, at, bytes.len, bytes) catch {
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

                buffer.replaceRange(vm.gc.allocator, at, bytes.len, bytes) catch {
                    vm.panic("Out of memory");
                    unreachable;
                };
            },
            64 => {
                const unwrapped = value.double();
                const bytes = std.mem.asBytes(&unwrapped);

                buffer.replaceRange(vm.gc.allocator, at, bytes.len, bytes) catch {
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
            const unwrapped = vm.gc.ptr(o.ObjUserData, .idx(value.obj().index)).?.userdata;
            const bytes = std.mem.asBytes(&unwrapped);

            buffer.replaceRange(vm.gc.allocator, at, bytes.len, bytes) catch {
                vm.panic("Out of memory");
                unreachable;
            };
        },
        else => {},
    }
}
