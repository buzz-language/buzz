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
const print = @import("io.zig").print;

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

var gpa = std.heap.DebugAllocator(.{
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

    return v.Value.Null;
}

/// Converts a value to a string
export fn bz_valueToString(value: v.Value, len: *usize, vm: *VM) callconv(.c) ?[*]const u8 {
    if (!value.isObj() or value.obj(vm.gc).obj_type != .String) {
        return null;
    }

    const string = o.ObjString.cast(value.obj(vm.gc)).?.string;

    len.* = string.len;

    return if (string.len > 0) @as([*]const u8, @ptrCast(string)) else null;
}

export fn bz_valueToCString(value: v.Value, vm: *VM) callconv(.c) ?[*:0]const u8 {
    if (!value.isObj() or value.obj(vm.gc).obj_type != .String) {
        return null;
    }

    return @ptrCast(o.ObjString.cast(value.obj(vm.gc)).?.string.ptr);
}

fn valueDump(value: v.Value, vm: *VM, seen: *std.AutoHashMapUnmanaged(*o.Obj, void), depth: usize) void {
    const io = if (is_wasm) {} else vm.process.io;
    if (depth > 50) {
        print(io, "...", .{});
        return;
    }

    if (value.isNull()) {
        print(io, "null", .{});
    } else if (!value.isObj() or seen.get(value.obj(vm.gc)) != null) {
        const string = value.toStringAlloc(vm.gc.allocator, vm.gc) catch @panic("Out of memory");
        defer vm.gc.allocator.free(string);

        print(io, "{s}", .{string});
    } else {
        seen.put(vm.gc.allocator, value.obj(vm.gc), {}) catch unreachable;

        switch (value.obj(vm.gc).obj_type) {
            .Type,
            .Closure,
            .Function,
            .Bound,
            .Native,
            .UserData,
            .Fiber,
            .EnumInstance,
            => {
                const string = value.toStringAlloc(vm.gc.allocator, vm.gc) catch @panic("Out of memory");
                defer vm.gc.allocator.free(string);

                print(io, "{s}", .{string});
            },

            .UpValue => {
                const upvalue = o.ObjUpValue.cast(value.obj(vm.gc)).?;

                valueDump(if (upvalue.closed != null) upvalue.closed.? else upvalue.location.*, vm, seen, depth);
            },

            .String => {
                const string = o.ObjString.cast(value.obj(vm.gc)).?;

                print(io, "\"{s}\"", .{string.string});
            },

            .Pattern => {
                const pattern = o.ObjPattern.cast(value.obj(vm.gc)).?;

                print(io, "$\"{s}\"", .{pattern.source});
            },

            .List => {
                const list = o.ObjList.cast(value.obj(vm.gc)).?;

                print(
                    io,
                    "{s}[ ",
                    .{
                        if (vm.gc.getTypeDef(list.type_def).resolved_type.?.List.mutable)
                            "mut "
                        else
                            "",
                    },
                );
                for (list.items.items) |item| {
                    valueDump(item, vm, seen, depth + 1);
                    print(io, ", ", .{});
                }
                print(io, "]", .{});
            },

            .Range => {
                const range = o.ObjRange.cast(value.obj(vm.gc)).?;

                print(io, "{}..{}", .{ range.low, range.high });
            },

            .Map => {
                const map = o.ObjMap.cast(value.obj(vm.gc)).?;

                print(
                    io,
                    "{s}{{ ",
                    .{
                        if (vm.gc.getTypeDef(map.type_def).resolved_type.?.Map.mutable)
                            "mut "
                        else
                            "",
                    },
                );
                var it = map.map.iterator();
                while (it.next()) |kv| {
                    const key = kv.key_ptr.*;

                    valueDump(key, vm, seen, depth + 1);
                    print(io, ": ", .{});
                    valueDump(kv.value_ptr.*, vm, seen, depth + 1);
                    print(io, ", ", .{});
                }
                print(io, "}}", .{});
            },

            .Enum => {
                const enumeration = o.ObjEnum.cast(value.obj(vm.gc)).?;
                const enum_type_def = vm.gc.getTypeDef(enumeration.type_def).resolved_type.?.Enum;

                print(
                    io,
                    "enum({s}) {s} {{ ",
                    .{
                        vm.gc.getString(enum_type_def.name).string,
                        vm.gc.getString(enum_type_def.name).string,
                    },
                );
                for (enum_type_def.cases, 0..) |case, i| {
                    print(io, "{s} -> ", .{case});
                    valueDump(enumeration.cases[i], vm, seen, depth);
                    print(io, ", ", .{});
                }
                print(io, "}}", .{});
            },

            .Object => {
                const object = o.ObjObject.cast(value.obj(vm.gc)).?;
                const object_def = vm.gc.getTypeDef(object.type_def).resolved_type.?.Object;

                print(io, "object", .{});
                if (object_def.conforms_to.count() > 0) {
                    print(io, "(", .{});
                    var it = object_def.conforms_to.iterator();
                    while (it.next()) |kv| {
                        print(io, "{s}, ", .{vm.gc.getString(vm.gc.getTypeDef(kv.key_ptr.*).resolved_type.?.Protocol.name).string});
                    }
                    print(io, ")", .{});
                }

                print(io, " {s} {{ ", .{vm.gc.getString(object_def.name).string});

                {
                    var it = object_def.fields.iterator();
                    while (it.next()) |kv| {
                        const field = kv.value_ptr.*;
                        const field_type_str = vm.gc.getTypeDef(field.type_def)
                            .toStringAlloc(vm.gc.allocator, false, vm.gc) catch @panic("Out of memory");
                        defer vm.gc.allocator.free(field_type_str);

                        if (!field.method) {
                            print(
                                io,
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
                                print(io, " = ", .{});
                                valueDump(val, vm, seen, depth + 1);
                            }

                            print(io, ", ", .{});
                        } else {
                            print(
                                io,
                                "{s}{s}, ",
                                .{
                                    if (field.mutable) "mut " else "",
                                    field_type_str,
                                },
                            );
                        }
                    }
                }

                print(io, "}}", .{});
            },

            .ObjectInstance => {
                const object_instance = o.ObjObjectInstance.cast(value.obj(vm.gc)).?;
                const object_def = vm.gc.getTypeDef(
                    vm.gc.getTypeDef(object_instance.type_def).resolved_type.?.ObjectInstance.of,
                ).resolved_type.?.Object;

                print(
                    io,
                    "{s}{s}{{ ",
                    .{
                        if (vm.gc.getTypeDef(object_instance.type_def).resolved_type.?.ObjectInstance.mutable)
                            "mut "
                        else
                            "",
                        if (object_instance.object) |object|
                            vm.gc.getString(vm.gc.getTypeDef(vm.gc.getObject(object).type_def).resolved_type.?.Object.name).string
                        else
                            ".",
                    },
                );

                var it = object_def.fields.iterator();
                while (it.next()) |field| {
                    print(io, "{s} = ", .{field.value_ptr.name});
                    valueDump(
                        object_instance.fields[field.value_ptr.index],
                        vm,
                        seen,
                        depth + 1,
                    );
                    print(io, ", ", .{});
                }

                print(io, "}}", .{});
            },

            .ForeignContainer => {
                const foreign = o.ObjForeignContainer.cast(value.obj(vm.gc)).?;
                const foreign_def = vm.gc.getTypeDef(foreign.type_def).resolved_type.?.ForeignContainer;

                print(
                    io,
                    "{s}{{ ",
                    .{vm.gc.getString(foreign_def.name).string},
                );

                var it = foreign_def.fields.iterator();
                while (it.next()) |kv| {
                    print(io, "{s} = ", .{kv.key_ptr.*});
                    valueDump(
                        kv.value_ptr.*.getter(
                            vm,
                            foreign.data.ptr,
                        ),
                        vm,
                        seen,
                        depth + 1,
                    );
                    print(io, ", ", .{});
                }
                print(io, "}}", .{});
            },
        }

        _ = seen.remove(value.obj(vm.gc));
    }
}

/// Dump value
pub export fn bz_valueDump(value: v.Value, vm: *VM) callconv(.c) void {
    var seen = std.AutoHashMapUnmanaged(*o.Obj, void).empty;
    defer seen.deinit(vm.gc.allocator);

    valueDump(value, vm, &seen, 0);
}

export fn bz_valueToForeignContainerPtr(value: v.Value, vm: *VM) callconv(.c) [*]u8 {
    return o.ObjForeignContainer.cast(value.obj(vm.gc)).?.data.ptr;
}

/// Converts a c string to a *o.ObjString
export fn bz_stringToValue(vm: *VM, string: ?[*]const u8, len: usize) callconv(.c) v.Value {
    return ((if (string != null and len > 0)
        vm.gc.copyString(string.?[0..len])
    else
        vm.gc.copyString("")) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_stringToValueZ(vm: *VM, string: [*:0]const u8) callconv(.c) v.Value {
    // Keeping the sentinel
    return (vm.gc.copyString(string[0..(std.mem.len(string) + 1)]) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_stringConcat(obj_string: v.Value, other: v.Value, vm: *VM) callconv(.c) v.Value {
    return (o.ObjString.cast(obj_string.obj(vm.gc)).?.concat(
        vm,
        o.ObjString.cast(other.obj(vm.gc)).?,
    ) catch @panic("Could not concat strings")).toValue();
}

export fn bz_stringSubscript(obj_string: v.Value, index_value: v.Value, checked: bool, vm: *VM) callconv(.c) v.Value {
    const str = o.ObjString.cast(obj_string.obj(vm.gc)).?;
    const index = index_value.integer();

    if (index < 0) {
        if (checked) {
            return v.Value.Null;
        }

        bz_throw(
            vm,
            (vm.gc.copyString("Out of bound string access.") catch unreachable).toValue(),
        );

        return v.Value.Error;
    }

    const str_index: usize = @intCast(index);

    if (str_index < str.string.len) {
        return (vm.gc.copyString(&([_]u8{str.string[str_index]})) catch unreachable).toValue();
    } else {
        if (checked) {
            return v.Value.Null;
        }

        bz_throw(
            vm,
            (vm.gc.copyString("Out of bound str access.") catch unreachable).toValue(),
        );

        return v.Value.Error;
    }
}

export fn bz_valueCastToString(value: v.Value, vm: *VM) callconv(.c) v.Value {
    const str = value.toStringAlloc(vm.gc.allocator, vm.gc) catch
        @panic("Out of memory");
    defer vm.gc.allocator.free(str);

    return (vm.gc.copyString(str) catch @panic("Out of memory")).toValue();
}

// Other stuff

// Type helpers

/// Returns the [str] type
export fn bz_stringType(vm: *VM) callconv(.c) v.Value {
    return vm.gc.getTypeDef(vm.gc.type_registry.str_type).toValue();
}

export fn bz_intType(vm: *VM) callconv(.c) v.Value {
    return vm.gc.getTypeDef(vm.gc.type_registry.int_type).toValue();
}

export fn bz_listType(vm: *VM, item_type: v.Value, mutable: bool) callconv(.c) v.Value {
    return (vm.gc.type_registry.getTypeDef(
        .{
            .def_type = .List,
            .optional = false,
            .resolved_type = .{
                    .List = o.ObjList.ListDef.init(
                    o.ObjTypeDef.cast(item_type.obj(vm.gc)).?,
                    mutable,
                ),
            },
        },
    ) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_mapType(vm: *VM, key_type: v.Value, value_type: v.Value, mutable: bool) callconv(.c) v.Value {
    return (vm.gc.type_registry.getTypeDef(
        .{
            .def_type = .Map,
            .optional = false,
            .resolved_type = .{
                    .Map = o.ObjMap.MapDef.init(
                    o.ObjTypeDef.cast(key_type.obj(vm.gc)).?,
                    o.ObjTypeDef.cast(value_type.obj(vm.gc)).?,
                    mutable,
                ),
            },
        },
    ) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_containerTypeSize(type_def: v.Value, vm: *VM) callconv(.c) usize {
    return o.ObjTypeDef.cast(type_def.obj(vm.gc)).?
        .resolved_type.?
        .ForeignContainer
        .zig_type.size();
}

export fn bz_containerTypeAlign(type_def: v.Value, vm: *VM) callconv(.c) usize {
    return o.ObjTypeDef.cast(type_def.obj(vm.gc)).?
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
    return (vm.gc.allocateObject(
        o.ObjRange{
            .low = @truncate(low),
            .high = @truncate(high),
        },
    ) catch @panic("Could not create range")).toValue();
}

export fn bz_newList(vm: *VM, list_type: v.Value) callconv(.c) v.Value {
    return (vm.gc.allocateObject(
        o.ObjList.init(
            vm.gc.allocator,
            o.ObjTypeDef.cast(list_type.obj(vm.gc)).?,
        ) catch @panic("Out of memory"),
    ) catch @panic("Could not create list")).toValue();
}

export fn bz_listAppend(list: v.Value, value: v.Value, vm: *VM) callconv(.c) void {
    o.ObjList.cast(list.obj(vm.gc)).?.rawAppend(vm.gc, value) catch @panic("Could not add element to list");
}

export fn bz_listGet(self: v.Value, index: i64, checked: bool, vm: *VM) callconv(.c) v.Value {
    const list = o.ObjList.cast(self.obj(vm.gc)).?;

    if (index < 0 or index >= list.items.items.len) {
        if (checked) {
            return v.Value.Null;
        } else {
            @panic("Out of bound list access.");
        }
    }

    return list.items.items[@intCast(index)];
}

export fn bz_listSet(self: v.Value, index: usize, value: v.Value, vm: *VM) callconv(.c) void {
    o.ObjList.cast(self.obj(vm.gc)).?.set(
        vm.gc,
        index,
        value,
    ) catch @panic("Could not set element in list");
}

export fn bz_listLen(self: v.Value, vm: *VM) callconv(.c) usize {
    return o.ObjList.cast(self.obj(vm.gc)).?.items.items.len;
}

export fn bz_listConcat(list: v.Value, other_list: v.Value, vm: *VM) callconv(.c) v.Value {
    const left: *o.ObjList = o.ObjList.cast(list.obj(vm.gc)).?;
    const right: *o.ObjList = o.ObjList.cast(other_list.obj(vm.gc)).?;

    var new_list = std.ArrayList(v.Value).empty;
    new_list.appendSlice(vm.gc.allocator, left.items.items) catch @panic("Could not concatenate lists");
    new_list.appendSlice(vm.gc.allocator, right.items.items) catch @panic("Could not concatenate lists");

    return (vm.gc.allocateObject(
        o.ObjList{
            .type_def = left.type_def,
            .methods = left.methods,
            .items = new_list,
        },
    ) catch @panic("Could not concatenate lists")).toValue();
}

export fn bz_mapConcat(map: v.Value, other_map: v.Value, vm: *VM) callconv(.c) v.Value {
    const left = o.ObjMap.cast(map.obj(vm.gc)).?;
    const right = o.ObjMap.cast(other_map.obj(vm.gc)).?;

    var new_map = left.map.clone(vm.gc.allocator) catch @panic("Could not concatenate maps");
    var it = right.map.iterator();
    while (it.next()) |entry| {
        new_map.put(
            vm.gc.allocator,
            entry.key_ptr.*,
            entry.value_ptr.*,
        ) catch @panic("Could not concatenate maps");
    }

    return (vm.gc.allocateObject(
        o.ObjMap{
            .type_def = left.type_def,
            .methods = left.methods,
            .map = new_map,
        },
    ) catch @panic("Could not concatenate maps")).toValue();
}

export fn bz_newUserData(vm: *VM, userdata: u64) callconv(.c) v.Value {
    return (vm.gc.allocateObject(
        o.ObjUserData{ .userdata = userdata },
    ) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_getUserDataPtr(userdata: v.Value, vm: *VM) callconv(.c) u64 {
    return o.ObjUserData.cast(userdata.obj(vm.gc)).?.userdata;
}

export fn bz_newVM(host: *VM) callconv(.c) *VM {
    return host.initFrom() catch @panic("Out of memory");
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
    var dlib = std.StringHashMapUnmanaged(Parser.Dlib).empty;
    var parser = Parser.init(
        self.process,
        self.gc,
        &imports,
        &dlib,
        false,
        self.flavor,
    );
    var codegen = CodeGen.init(
        self.process,
        self.gc,
        &parser,
        self.flavor,
        null,
        self.debugger != null,
    );
    defer {
        codegen.deinit();
        parser.deinit();
        var imports_it = imports.valueIterator();
        while (imports_it.next()) |import| {
            import.deinit(self.gc.allocator);
        }
        imports.deinit(self.gc.allocator);
        strings.deinit(self.gc.allocator);
    }

    const script_name = file_name.?[0..file_name_len];
    const root_dir = std.fs.path.dirname(script_name) orelse ".";

    if (parser.parse(source.?[0..source_len], root_dir, null, script_name) catch null) |ast| {
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

fn calleeIsCompiled(value: v.Value, vm: *VM) bool {
    const obj: *o.Obj = value.obj(vm.gc);
    return switch (obj.obj_type) {
        .Bound => bound: {
            const bound = o.ObjBoundMethod.cast(obj).?;

            if (bound.native != null) {
                break :bound true;
            }

            if (bound.closure) |cls| {
                break :bound vm.gc.getFunction(vm.gc.getClosure(cls).function).native != null;
            }

            break :bound false;
        },
        .Closure => vm.gc.getFunction(o.ObjClosure.cast(obj).?.function).native != null,
        .Native => true,
        else => false,
    };
}

pub export fn bz_invoke(
    self: *VM,
    instance: v.Value,
    method_idx: usize,
    arguments: ?[*]const *const v.Value,
    len: u8,
    catch_value: ?*v.Value,
) callconv(.c) void {
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
        if (catch_value) |val| val.* else null,
        false,
    ) catch unreachable;

    // If not compiled, run it with the VM loop
    if (!calleeIsCompiled(callee, self)) {
        // TODO: catch properly
        self.run() catch unreachable;
    }

    self.currentFrame().?.in_native_call = was_in_native_call;
}

pub export fn bz_call(
    self: *VM,
    closure_value: v.Value,
    arguments: ?[*]const *const v.Value,
    len: u8,
    catch_value: ?*v.Value,
) callconv(.c) bool {
    std.debug.assert(closure_value.obj(self.gc).obj_type == .Closure);

    self.push(closure_value);
    var i: usize = 0;
    while (i < len) : (i += 1) {
        self.push(arguments.?[i].*);
    }

    self.callValue(
        closure_value,
        len,
        if (catch_value) |val| val.* else null,
    ) catch return false;

    // If not compiled, run it with the VM loop
    if (closure_value.obj(self.gc).access(
        o.ObjClosure,
        .Closure,
        self.gc,
    ).?.getFunction(self.gc).native == null) {
        self.run() catch return false;

        return true;
    }

    return true;
}

export fn bz_newQualifiedObjectInstance(self: *VM, qualified_name: [*]const u8, len: usize, mutable: bool) callconv(.c) v.Value {
    const object = o.ObjObject.cast(bz_getQualified(self, qualified_name, len).obj(self.gc)).?;

    const instance: *o.ObjObjectInstance = self.gc.allocateObject(
        o.ObjObjectInstance.init(
            self,
            object,
            self.gc.getTypeDef(object.type_def).toInstance(
                &self.gc.type_registry,
                mutable,
            ) catch {
                self.panic("Out of memory");
                unreachable;
            },
            self.gc,
        ) catch @panic("Out of memory"),
    ) catch {
        @panic("Could not create error");
    };

    // Keep the new instance rooted while cloning defaults may allocate.
    self.push(instance.toValue());

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

    return self.pop();
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
        const obj_instance = o.ObjObjectInstance.cast(instance.obj(vm.gc)).?;
        const object_def = vm.gc.getTypeDef(
            vm.gc.getTypeDef(obj_instance.type_def).resolved_type.?.ObjectInstance.of,
        ).resolved_type.?.Object.fields;

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

export fn bz_pushErrorEnum(self: *VM, qualified_name: [*]const u8, name_len: usize, case_str: [*]const u8, case_len: usize) callconv(.c) void {
    const case = self.gc.copyString(case_str[0..case_len]) catch @panic("Could not create error payload");

    self.push(
        bz_getEnumCase(
            bz_getQualified(self, qualified_name, name_len),
            case.toValue(),
            self,
        ),
    );
}

export fn bz_getQualified(self: *VM, qualified_name: [*]const u8, len: usize) callconv(.c) v.Value {
    for (self.globals.items) |global| {
        if (global.isObj()) {
            switch (global.obj(self.gc).obj_type) {
                .Enum => {
                    const obj_enum = o.ObjEnum.cast(global.obj(self.gc)).?;

                    if (std.mem.eql(u8, qualified_name[0..len], self.gc.getString(self.gc.getTypeDef(obj_enum.type_def).resolved_type.?.Enum.qualified_name).string)) {
                        return global;
                    }
                },
                .Object => {
                    const obj_enum = o.ObjObject.cast(global.obj(self.gc)).?;

                    if (std.mem.eql(u8, qualified_name[0..len], self.gc.getString(self.gc.getTypeDef(obj_enum.type_def).resolved_type.?.Object.qualified_name).string)) {
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
    const object = if (object_value.isObj()) o.ObjObject.cast(object_value.obj(vm.gc)).? else null;
    const typedef = o.ObjTypeDef.cast(typedef_value.obj(vm.gc)).?;

    const instance = vm.gc.allocateObject(
        o.ObjObjectInstance.init(
            vm,
            object,
            typedef,
            vm.gc,
        ) catch @panic("Out of memory"),
    ) catch @panic("Could not instanciate object");

    // Keep the new instance rooted while cloning defaults may allocate.
    vm.push(instance.toValue());

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

    return vm.pop();
}

export fn bz_getObjectField(object_value: v.Value, field_idx: usize, vm: *VM) callconv(.c) v.Value {
    return o.ObjObject.cast(object_value.obj(vm.gc)).?.fields[field_idx];
}

export fn bz_setObjectField(object_value: v.Value, field_idx: usize, value: v.Value, vm: *VM) callconv(.c) void {
    o.ObjObject.cast(object_value.obj(vm.gc)).?.setField(
        vm.gc,
        field_idx,
        value,
    ) catch @panic("Could not set static field");
}

export fn bz_setObjectInstanceProperty(instance_value: v.Value, field_idx: usize, value: v.Value, vm: *VM) callconv(.c) void {
    o.ObjObjectInstance.cast(instance_value.obj(vm.gc)).?.setField(
        vm.gc,
        field_idx,
        value,
    ) catch @panic("Could not set instance field");
}

export fn bz_getObjectInstanceProperty(instance_value: v.Value, property_idx: usize, vm: *VM) callconv(.c) v.Value {
    return o.ObjObjectInstance.cast(instance_value.obj(vm.gc)).?
        .fields[property_idx];
}

export fn bz_getObjectInstanceMethod(instance_value: v.Value, method_idx: usize, bind: bool, vm: *VM) callconv(.c) v.Value {
    const instance = o.ObjObjectInstance.cast(instance_value.obj(vm.gc)).?;
    const method = vm.gc.getObject(instance.object.?).fields[method_idx];

    return if (bind)
        bz_bindMethod(
            vm,
            method,
            method,
            v.Value.Null,
        )
    else
        method;
}

export fn bz_getProtocolMethod(instance_value: v.Value, method_name: v.Value, vm: *VM) callconv(.c) v.Value {
    const instance = instance_value.obj(vm.gc).access(
        o.ObjObjectInstance,
        .ObjectInstance,
        vm.gc,
    ).?;

    const name = method_name.obj(vm.gc)
        .access(o.ObjString, .String, vm.gc).?
        .string;

    const method_idx = vm.gc.getTypeDef(
        vm.gc.getTypeDef(instance.type_def).resolved_type.?.ObjectInstance.of,
    ).resolved_type.?.Object.fields.get(name).?.index;

    return bz_bindMethod(
        vm,
        instance_value,
        vm.gc.getObject(instance.object.?).fields[method_idx],
        v.Value.Null,
    );
}

export fn bz_bindMethod(vm: *VM, receiver: v.Value, method_value: v.Value, native_value: v.Value) callconv(.c) v.Value {
    return (vm.gc.allocateObject(
        o.ObjBoundMethod{
            .receiver = receiver,
            .closure = if (method_value.isObj()) o.ObjClosure.cast(method_value.obj(vm.gc)).?.toIdx() else null,
            .native = if (native_value.isObj()) o.ObjNative.cast(native_value.obj(vm.gc)).?.toIdx() else null,
        },
    ) catch @panic("Could not bind method")).toValue();
}

export fn bz_getEnumCase(enum_value: v.Value, case_name_value: v.Value, vm: *VM) callconv(.c) v.Value {
    const self = o.ObjEnum.cast(enum_value.obj(vm.gc)).?;
    const case = o.ObjString.cast(case_name_value.obj(vm.gc)).?.string;
    var case_index: usize = 0;

    for (vm.gc.getTypeDef(self.type_def).resolved_type.?.Enum.cases, 0..) |enum_case, index| {
        if (std.mem.eql(u8, case, enum_case)) {
            case_index = index;
            break;
        }
    }

    return (vm.gc.allocateObject(
        o.ObjEnumInstance{
            .enum_ref = self.toIdx(),
            .case = @intCast(case_index),
        },
    ) catch @panic("Could not create enum case")).toValue();
}

export fn bz_getEnumInstanceValue(enum_instance_value: v.Value, vm: *VM) callconv(.c) v.Value {
    const instance = o.ObjEnumInstance.cast(enum_instance_value.obj(vm.gc)).?;

    return vm.gc.getEnum(instance.enum_ref).cases[instance.case];
}

export fn bz_getEnumCaseFromValue(enum_value: v.Value, case_value: v.Value, vm: *VM) callconv(.c) v.Value {
    const enum_ = o.ObjEnum.cast(enum_value.obj(vm.gc)).?;

    for (enum_.cases, 0..) |case, index| {
        if (v.Value.eql(case, case_value, vm.gc)) {
            var enum_case: *o.ObjEnumInstance = vm.gc.allocateObject(
                o.ObjEnumInstance{
                    .enum_ref = enum_.toIdx(),
                    .case = @intCast(index),
                },
            ) catch @panic("Could not create enum instance");

            return enum_case.toValue();
        }
    }

    return v.Value.Null;
}

export fn bz_valueEqual(self: v.Value, other: v.Value, vm: *VM) callconv(.c) v.Value {
    return v.Value.fromBoolean(self.eql(other, vm.gc));
}

export fn bz_rangeContains(range_value: v.Value, value: v.Value, vm: *VM) callconv(.c) v.Value {
    const range = o.ObjRange.cast(range_value.obj(vm.gc)).?;
    const number = if (value.isInteger())
        @as(v.Double, @floatFromInt(value.integer()))
    else
        value.double();
    const low: v.Double = @floatFromInt(range.low);
    const high: v.Double = @floatFromInt(range.high);

    return v.Value.fromBoolean(
        (high >= low and number >= low and number < high) or
            (low >= high and number >= high and number < low),
    );
}

export fn bz_patternMatches(vm: *VM, pattern_value: v.Value, subject_value: v.Value) callconv(.c) v.Value {
    if (comptime is_wasm) {
        return bz_patternMatchesWasm(vm, pattern_value, subject_value);
    } else {
        return bz_patternMatchesNative(vm, pattern_value, subject_value);
    }
}

fn bz_patternMatchesWasm(_: *VM, _: v.Value, _: v.Value) v.Value {
    return v.Value.False;
}

fn bz_patternMatchesNative(vm: *VM, pattern_value: v.Value, subject_value: v.Value) v.Value {
    const pattern = o.ObjPattern.cast(pattern_value.obj(vm.gc)).?;
    const subject = o.ObjString.cast(subject_value.obj(vm.gc)).?;

    var offset: usize = 0;
    return v.Value.fromBoolean(
        (buzz_builtin.pattern.rawMatch(
            pattern,
            vm,
            subject,
            &offset,
        ) catch {
            vm.panic("Out of memory");
            unreachable;
        }) != null,
    );
}

export fn bz_valueTypeOf(self: v.Value, vm: *VM) callconv(.c) v.Value {
    return (self.typeOf(vm.gc) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_newMap(vm: *VM, map_type: v.Value) callconv(.c) v.Value {
    var map = vm.gc.allocateObject(
        o.ObjMap.init(
            vm.gc.allocator,
            o.ObjTypeDef.cast(map_type.obj(vm.gc)).?,
        ) catch @panic("Could not create map"),
    ) catch @panic("Could not create map");

    return map.toValue();
}

export fn bz_mapSet(map: v.Value, key: v.Value, value: v.Value, vm: *VM) callconv(.c) void {
    o.ObjMap.cast(map.obj(vm.gc)).?.set(
        vm.gc,
        key,
        value,
    ) catch @panic("Could not set map element");
}

export fn bz_mapGet(map: v.Value, key: v.Value, vm: *VM) callconv(.c) v.Value {
    return o.ObjMap.cast(map.obj(vm.gc)).?.map.get(key) orelse v.Value.Null;
}

export fn bz_valueIs(self: v.Value, type_def: v.Value, vm: *VM) callconv(.c) v.Value {
    return v.Value.fromBoolean(type_def.is(self, vm.gc));
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

pub export fn bz_throw(vm: *VM, value: v.Value) callconv(.c) void {
    vm.push(value);

    bz_rethrow(vm);
}

export fn bz_closeUpValues(vm: *VM, last: *v.Value) callconv(.c) void {
    vm.closeUpValues(last);
    _ = vm.pop();
}

export fn bz_getUpValue(ctx: *o.NativeCtx, slot: usize) callconv(.c) v.Value {
    return ctx.getUpvalue(slot).location.*;
}

export fn bz_setUpValue(ctx: *o.NativeCtx, slot: usize, value: v.Value) callconv(.c) void {
    ctx.getUpvalue(slot).location.* = value;
}

export fn bz_callFromJit(ctx: *o.NativeCtx) callconv(.c) v.Value {
    const vm = ctx.vm;

    vm.callValue(
        ctx.callee,
        @intCast(ctx.arg_count),
        null,
    ) catch @panic("Failed calling function from JIT");

    // If the callee is interpreted, run it until its return reaches the native
    // caller frame. The VM leaves the result on the stack; RawFn returns it.
    if (!calleeIsCompiled(ctx.callee, vm)) {
        vm.run() catch @panic("Failed running function from JIT");
    }

    const result = vm.pop();
    vm.current_fiber.stack_top = ctx.base;

    return result;
}

export fn bz_context(ctx: *o.NativeCtx, closure_value: v.Value, new_ctx: *o.NativeCtx, arg_count: usize) callconv(.c) *anyopaque {
    if (is_wasm) {
        unreachable;
    }

    const bound = if (closure_value.obj(ctx.vm.gc).obj_type == .Bound)
        o.ObjBoundMethod.cast(closure_value.obj(ctx.vm.gc)).?
    else
        null;

    const closure = if (bound) |bd|
        if (bd.closure) |cls| ctx.vm.gc.getClosure(cls) else null
    else
        o.ObjClosure.cast(closure_value.obj(ctx.vm.gc));

    const native = if (closure == null and bound != null)
        if (bound.?.native) |nat| ctx.vm.gc.getNative(nat) else null
    else if (closure == null and bound == null)
        o.ObjNative.cast(closure_value.obj(ctx.vm.gc)).?
    else
        null;

    if (BuildOptions.recursive_call_limit) |recursive_call_limit| {
        // If recursive call, update counter
        ctx.vm.current_fiber.recursive_count = if (closure != null and ctx.vm.gc.getFunction(closure.?.function) == ctx.vm.current_fiber.current_compiled_function)
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
        .process = ctx.process,
        .vm = ctx.vm,
        .globals = if (closure) |cls| cls.globals.items.ptr else ctx.globals,
        .upvalues = if (closure) |cls| cls.upvalues.ptr else ctx.upvalues,
        .base = ctx.vm.current_fiber.stack_top - arg_count - 1,
        .stack_top = &ctx.vm.current_fiber.stack_top,
        .callee = closure_value,
        .arg_count = arg_count,
    };

    if (closure) |cls| {
        if (ctx.vm.gc.getFunction(cls.function).native_raw == null) {
            ctx.vm.jit.?.compileFunctionSynchronously(cls) catch |err| switch (err) {
                error.CantCompile => return @as(*anyopaque, @ptrFromInt(@intFromPtr(&bz_callFromJit))),
                else => @panic("Failed compiling function"),
            };
        }

        ctx.vm.current_fiber.current_compiled_function = ctx.vm.gc.getFunction(cls.function);

        return ctx.vm.gc.getFunction(cls.function).native_raw orelse @as(*anyopaque, @ptrFromInt(@intFromPtr(&bz_callFromJit)));
    }

    return native.?.native;
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
    const obj_function = ctx.vm.gc.getFunction(ctx.vm.current_ast.nodes.items(.components)[function_node].Function.function.?);
    obj_function.native = native;
    obj_function.native_raw = native_raw;

    const closure = ctx.vm.gc.allocateObject(
        o.ObjClosure.init(
            ctx.vm.gc.allocator,
            ctx.vm,
            obj_function,
        ) catch @panic("Could not instanciate closure"),
    ) catch @panic("Could not instanciate closure");

    // On stack to prevent collection
    ctx.vm.push(closure.toValue());

    // ctx.vm.jit.?.compiled_functions.put(closure, {}) catch @panic("Could not get closure");

    var it = ctx.vm.current_ast.nodes.items(.components)[function_node].Function.upvalue_binding.iterator();
    var i: usize = 0;
    while (it.next()) |kv| : (i += 1) {
        const is_local = kv.value_ptr.*;
        const index = kv.key_ptr.*;

        if (is_local) {
            closure.upvalues[i] = (ctx.vm.captureUpvalue(&(ctx.base[index])) catch @panic("Could not instanciate closure")).toIdx();
        } else {
            closure.upvalues[i] = ctx.upvalues[index];
        }
    }

    return ctx.vm.pop();
}

export fn bz_dumpStack(ctx: *o.NativeCtx, off: usize) callconv(.c) void {
    const io = if (is_wasm) {} else ctx.vm.process.io;
    print(
        io,
        "base is {}, top is {}\n",
        .{
            @intFromPtr(ctx.base),
            @intFromPtr(ctx.vm.current_fiber.stack_top),
        },
    );
    print(io, "#{}:\n", .{off});
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
    const method = o.ObjList.cast(list_value.obj(vm.gc)).?
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
    const method = o.ObjMap.cast(map_value.obj(vm.gc)).?
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

export fn bz_stringNext(string_value: v.Value, index: *v.Value, vm: *VM) callconv(.c) v.Value {
    const string = o.ObjString.cast(string_value.obj(vm.gc)).?;

    if (string.next(
        vm,
        if (index.isNull()) null else index.integer(),
    ) catch @panic("Could not get next string index")) |new_index| {
        index.* = v.Value.fromInteger(new_index);

        return (vm.gc.copyString(&[_]u8{string.string[@as(usize, @intCast(new_index))]}) catch @panic("Could not iterate on string")).toValue();
    }

    index.* = v.Value.Sentinel;
    return v.Value.Sentinel;
}

export fn bz_listNext(list_value: v.Value, index: *v.Value, vm: *VM) callconv(.c) v.Value {
    const list = o.ObjList.cast(list_value.obj(vm.gc)).?;

    if (list.rawNext(
        if (index.isNull()) null else index.integer(),
    ) catch @panic("Could not get next list index")) |new_index| {
        index.* = v.Value.fromInteger(new_index);
        return list.items.items[@as(usize, @intCast(new_index))];
    }

    index.* = v.Value.Sentinel;
    return v.Value.Sentinel;
}

export fn bz_rangeNext(range_value: v.Value, index_slot: v.Value, vm: *VM) callconv(.c) v.Value {
    const range = o.ObjRange.cast(range_value.obj(vm.gc)).?;

    if (index_slot.integerOrNull()) |index| {
        if (range.low < range.high) {
            return if (index + 1 >= range.high)
                v.Value.Sentinel
            else
                v.Value.fromInteger(index + 1);
        } else {
            return if (index - 1 <= range.high)
                v.Value.Sentinel
            else
                v.Value.fromInteger(index - 1);
        }
    }

    return v.Value.fromInteger(range.low);
}

export fn bz_mapNext(map_value: v.Value, key: *v.Value, vm: *VM) callconv(.c) v.Value {
    const map = o.ObjMap.cast(map_value.obj(vm.gc)).?;
    const map_keys = map.map.keys();

    // Sentinel is the external start/end marker here so real `null` keys stay iterable.
    const next_key = if (key.isSentinel())
        if (map_keys.len > 0) map_keys[0] else v.Value.Sentinel
    else next: {
        const index = map.map.getIndex(key.*) orelse break :next v.Value.Sentinel;
        break :next if (index + 1 < map_keys.len) map_keys[index + 1] else v.Value.Sentinel;
    };

    key.* = next_key;

    if (!next_key.isSentinel()) {
        return map.map.get(next_key) orelse v.Value.Null;
    }

    return v.Value.Sentinel;
}

export fn bz_mapForeachNext(
    map_value: v.Value,
    key: *v.Value,
    value: *v.Value,
    index: *v.Value,
    vm: *VM,
) callconv(.c) void {
    const map = o.ObjMap.cast(map_value.obj(vm.gc)).?;

    std.debug.assert(index.*.isDouble());

    const current_index: i64 = @intFromFloat(index.*.double());
    const next_index: i64 = current_index + 1;
    std.debug.assert(next_index >= 0);

    if (@as(u64, @intCast(next_index)) >= v.Value.MaxExactDoubleInteger) {
        bz_throw(
            vm,
            (vm.gc.copyString("Map foreach index exceeded maximum exact double integer (2^53)") catch {
                vm.panic("Out of memory");
                unreachable;
            }).toValue(),
        );
    }

    const next_index_usize: usize = @intCast(next_index);
    if (next_index_usize >= map.map.count()) {
        key.* = v.Value.Sentinel;
        return;
    }

    key.* = map.map.keys()[next_index_usize];
    value.* = map.map.values()[next_index_usize];
    index.* = v.Value.fromDouble(@floatFromInt(next_index));
}

export fn bz_enumNext(enum_value: v.Value, case: v.Value, vm: *VM) callconv(.c) v.Value {
    const enum_ = o.ObjEnum.cast(enum_value.obj(vm.gc)).?;

    if (enum_.rawNext(
        vm,
        if (case.isNull()) null else o.ObjEnumInstance.cast(case.obj(vm.gc)),
    ) catch @panic("Could not iterate over enum")) |new_case| {
        return new_case.toValue();
    }

    return v.Value.Sentinel;
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
        expected_type.* = vm.gc.getTypeDef(uzdef.type_def).toValue();

        return &uzdef.zig_type;
    }

    return null;
}

export fn bz_foreignContainerGet(value: v.Value, field_idx: usize, vm: *VM) callconv(.c) v.Value {
    const container = o.ObjForeignContainer.cast(value.obj(vm.gc)).?;

    return vm.gc.getTypeDef(container.type_def).resolved_type.?
        .ForeignContainer
        .fields.values()[field_idx]
        .getter(
        vm,
        container.data.ptr,
    );
}

export fn bz_foreignContainerSet(value: v.Value, field_idx: usize, new_value: v.Value, vm: *VM) callconv(.c) void {
    const container = o.ObjForeignContainer.cast(value.obj(vm.gc)).?;
    // Oh right that's beautiful enough...
    return vm.gc.getTypeDef(container.type_def).resolved_type.?.ForeignContainer
        .fields.values()[field_idx]
        .setter(
        vm,
        container.data.ptr,
        new_value,
    );
}

export fn bz_newForeignContainerInstance(vm: *VM, typedef_value: v.Value) callconv(.c) v.Value {
    return (vm.gc.allocateObject(
        o.ObjForeignContainer.init(
            vm,
            o.ObjTypeDef.cast(typedef_value.obj(vm.gc)).?,
        ) catch {
            vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_foreignContainerSlice(container_value: v.Value, len: *usize, vm: *VM) callconv(.c) [*]u8 {
    const container = o.ObjForeignContainer.cast(container_value.obj(vm.gc)).?;

    len.* = container.data.len;

    return container.data.ptr;
}

export fn bz_valueIsForeignContainer(value: v.Value, vm: *VM) callconv(.c) bool {
    return value.isObj() and value.obj(vm.gc).obj_type == .ForeignContainer;
}

export fn bz_newForeignContainerFromSlice(vm: *VM, type_def: v.Value, ptr: [*]u8, len: usize) callconv(.c) v.Value {
    var container = (vm.gc.allocateObject(
        o.ObjForeignContainer{
            .type_def = o.ObjTypeDef.cast(type_def.obj(vm.gc)).?.toIdx(),
            .data = ptr[0..len],
        },
    ) catch {
        vm.panic("Out of memory");
        unreachable;
    });

    return container.toValue();
}

export fn bz_zigTypeSize(self: *ZigType) callconv(.c) usize {
    return self.size();
}

export fn bz_zigTypeAlignment(self: *ZigType) callconv(.c) u16 {
    return self.alignment();
}

export fn bz_zigTypeToCString(self: *ZigType, vm: *VM) callconv(.c) v.Value {
    var out = std.Io.Writer.Allocating.init(vm.gc.allocator);

    out.writer.print("{}\x00", .{self.*}) catch {
        vm.panic("Out of memory");
        unreachable;
    };

    return (vm.gc.copyString(out.written()) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
}

export fn bz_serialize(vm: *VM, value: v.Value, error_value: *v.Value) callconv(.c) v.Value {
    var seen = std.AutoHashMapUnmanaged(*o.Obj, void).empty;
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
    return (vm.gc.allocateObject(
        o.ObjFiber{
            .fiber = vm.current_fiber,
        },
    ) catch {
        vm.panic("Out of memory");
        unreachable;
    }).toValue();
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
    const value = switch (ztype.*) {
        .Bool => v.Value.fromBoolean(buffer.items[at] == 1),
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

                    break :integer userdata.toValue();
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

            break :ptr userdata.toValue();
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
                    const unwrapped = o.ObjUserData.cast(value.obj(vm.gc)).?.userdata;
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
            const unwrapped = o.ObjUserData.cast(value.obj(vm.gc)).?.userdata;
            const bytes = std.mem.asBytes(&unwrapped);

            buffer.replaceRange(vm.gc.allocator, at, bytes.len, bytes) catch {
                vm.panic("Out of memory");
                unreachable;
            };
        },
        else => {},
    }
}
