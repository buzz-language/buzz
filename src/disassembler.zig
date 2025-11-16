const std = @import("std");
const io = @import("io.zig");
const print = io.print;
const Chunk = @import("Chunk.zig");
const Value = @import("value.zig").Value;
const obj = @import("obj.zig");
const _vm = @import("vm.zig");
const global_allocator = @import("buzz_api.zig").allocator;

const VM = _vm.VM;
const OpCode = Chunk.OpCode;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    print("\u{001b}[2m", .{}); // Dimmed
    print("=== {s} ===\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
    print("\u{001b}[0m", .{});
}

fn namedInvokeInstruction(code: OpCode, chunk: *Chunk, offset: usize) usize {
    const constant: u24 = @intCast(0x00ffffff & chunk.code.items[offset]);
    const arg_count: u8 = @intCast(chunk.code.items[offset + 1] >> 24);
    const catch_count: u24 = @intCast(0x00ffffff & chunk.code.items[offset + 1]);

    var value_str = chunk.constants.items[constant].toStringAlloc(global_allocator) catch @panic("Out of memory");
    defer global_allocator.free(value_str);

    print(
        "{s}\t{s}({} args, {} catches)",
        .{
            @tagName(code),
            value_str[0..@min(value_str.len, 100)],
            arg_count,
            catch_count,
        },
    );

    return offset + 2;
}

fn invokeInstruction(code: OpCode, chunk: *Chunk, offset: usize) usize {
    const method_idx: u24 = @intCast(0x00ffffff & chunk.code.items[offset]);
    const arg_count: u8 = @intCast(chunk.code.items[offset + 1] >> 24);
    const catch_count: u24 = @intCast(0x00ffffff & chunk.code.items[offset + 1]);

    print(
        "{s}\t{}({} args, {} catches)",
        .{
            @tagName(code),
            method_idx,
            arg_count,
            catch_count,
        },
    );

    return offset + 1;
}

fn simpleInstruction(code: OpCode, offset: usize) usize {
    print("{s}\t", .{@tagName(code)});

    return offset + 1;
}

fn byteInstruction(code: OpCode, chunk: *Chunk, offset: usize) usize {
    const slot: u24 = @intCast(0x00ffffff & chunk.code.items[offset]);
    print(
        "{s}\t{}",
        .{
            @tagName(code),
            slot,
        },
    );
    return offset + 1;
}

fn bytesInstruction(code: OpCode, chunk: *Chunk, offset: usize) usize {
    const a: u24 = @intCast(0x00ffffff & chunk.code.items[offset]);
    const b: u24 = @intCast(chunk.code.items[offset + 1]);

    print(
        "{s}\t{} {}",
        .{
            @tagName(code),
            a,
            b,
        },
    );
    return offset + 2;
}

fn triInstruction(code: OpCode, chunk: *Chunk, offset: usize) usize {
    const full_instruction: u32 = chunk.code.items[offset];

    const a: u8 = @intCast((0x00ffffff & full_instruction) >> 16);
    const b: u16 = @intCast(0x0000ffff & full_instruction);

    print(
        "{s}\t{} {}",
        .{
            @tagName(code),
            a,
            b,
        },
    );
    return offset + 1;
}

fn constantInstruction(code: OpCode, chunk: *Chunk, offset: usize) usize {
    const constant: u24 = @intCast(0x00ffffff & chunk.code.items[offset]);
    var value_str = chunk.constants.items[constant].toStringAlloc(global_allocator) catch @panic("Out of memory");
    defer global_allocator.free(value_str);

    print(
        "{s}\t{} {s}",
        .{
            @tagName(code),
            constant,
            value_str[0..@min(value_str.len, 100)],
        },
    );

    return offset + 1;
}

fn jumpInstruction(code: OpCode, chunk: *Chunk, direction: bool, offset: usize) usize {
    const jump: u24 = @intCast(0x00ffffff & chunk.code.items[offset]);

    if (direction) {
        print(
            "{s}\t{} + {} -> {}",
            .{
                @tagName(code),
                offset,
                jump,
                offset + 1 + 1 * jump,
            },
        );
    } else {
        print(
            "{s}\t{} - {} -> {}",
            .{
                @tagName(code),
                offset,
                jump,
                offset + 1 - 1 * jump,
            },
        );
    }

    return offset + 1;
}

fn tryInstruction(code: OpCode, chunk: *Chunk, offset: usize) usize {
    const jump: u24 = @intCast(0x00ffffff & chunk.code.items[offset]);

    print(
        "{s}\t{} -> {}",
        .{
            @tagName(code),
            offset,
            jump,
        },
    );

    return offset + 1;
}

pub fn dumpStack(vm: *VM) void {
    print("\u{001b}[2m", .{}); // Dimmed
    print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n", .{});

    var value: [*]Value = @ptrCast(vm.current_fiber.stack[0..]);
    var count: usize = 0;
    while (@intFromPtr(value) < @intFromPtr(vm.current_fiber.stack_top) and count < vm.current_fiber.stack.len) : (count += 1) {
        var value_str = value[0].toStringAlloc(vm.gc) catch unreachable;
        defer global_allocator.free(value_str);

        if (vm.currentFrame().?.slots == value) {
            print(
                "{} {} {s} frame\n ",
                .{
                    @intFromPtr(value),
                    value[0].val,
                    value_str[0..@min(value_str.len, 100)],
                },
            );
        } else {
            print(
                "{} {} {s}\n ",
                .{
                    @intFromPtr(value),
                    value[0].val,
                    value_str[0..@min(value_str.len, 100)],
                },
            );
        }

        value += 1;
    }
    print("{} top\n", .{@intFromPtr(vm.current_fiber.stack_top)});

    print("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n\n\n", .{});
    print("\u{001b}[0m", .{});
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    print("\n{:0>3} ", .{offset});
    const lines = chunk.ast.tokens.items(.line);

    if (offset > 0 and lines[chunk.locations.items[offset]] == lines[chunk.locations.items[offset - 1]]) {
        print("|   ", .{});
    } else {
        print("{:0>3} ", .{lines[chunk.locations.items[offset]]});
    }

    const full_instruction: u32 = chunk.code.items[offset];
    const instruction: OpCode = @enumFromInt(@as(u8, @intCast(full_instruction >> 24)));
    const arg: u24 = @intCast(0x00ffffff & full_instruction);
    return switch (instruction) {
        .OP_NULL,
        .OP_VOID,
        .OP_TRUE,
        .OP_FALSE,
        .OP_POP,
        .OP_EQUAL,
        .OP_IS,
        .OP_GREATER,
        .OP_LESS,
        .OP_ADD_F,
        .OP_ADD_I,
        .OP_ADD_STRING,
        .OP_ADD_LIST,
        .OP_ADD_MAP,
        .OP_SUBTRACT_I,
        .OP_SUBTRACT_F,
        .OP_MULTIPLY_I,
        .OP_MULTIPLY_F,
        .OP_DIVIDE_I,
        .OP_DIVIDE_F,
        .OP_NOT,
        .OP_NEGATE_I,
        .OP_NEGATE_F,
        .OP_BAND,
        .OP_BOR,
        .OP_XOR,
        .OP_BNOT,
        .OP_SHL,
        .OP_SHR,
        .OP_MOD_I,
        .OP_MOD_F,
        .OP_UNWRAP,
        .OP_GET_ENUM_CASE_VALUE,
        .OP_SET_LIST_SUBSCRIPT,
        .OP_SET_MAP_SUBSCRIPT,
        .OP_THROW,
        .OP_IMPORT,
        .OP_TO_STRING,
        .OP_INSTANCE,
        .OP_FCONTAINER_INSTANCE,
        .OP_STRING_FOREACH,
        .OP_LIST_FOREACH,
        .OP_ENUM_FOREACH,
        .OP_MAP_FOREACH,
        .OP_FIBER_FOREACH,
        .OP_RANGE_FOREACH,
        .OP_RESUME,
        .OP_YIELD,
        .OP_RESOLVE,
        .OP_TRY_END,
        .OP_GET_ENUM_CASE_FROM_VALUE,
        .OP_TYPEOF,
        .OP_HOTSPOT_CALL,
        .OP_CLONE,
        .OP_CLOSE_UPVALUE,
        .OP_RETURN,
        .OP_RANGE,
        .OP_FIBER,
        => simpleInstruction(instruction, offset),

        .OP_SWAP => bytesInstruction(instruction, chunk, offset),

        .OP_DEFINE_GLOBAL,
        .OP_GET_GLOBAL,
        .OP_SET_GLOBAL,
        .OP_GET_LOCAL,
        .OP_SET_LOCAL,
        .OP_GET_UPVALUE,
        .OP_SET_UPVALUE,
        .OP_GET_ENUM_CASE,
        .OP_EXPORT,
        .OP_LIST_APPEND,
        .OP_SET_MAP,
        .OP_PROPERTY,
        .OP_OBJECT_DEFAULT,
        .OP_SET_OBJECT_PROPERTY,
        .OP_SET_INSTANCE_PROPERTY,
        .OP_GET_INSTANCE_PROPERTY,
        .OP_GET_INSTANCE_METHOD,
        .OP_GET_LIST_PROPERTY,
        .OP_GET_MAP_PROPERTY,
        .OP_GET_FIBER_PROPERTY,
        .OP_GET_RANGE_PROPERTY,
        .OP_GET_STRING_PROPERTY,
        .OP_GET_PATTERN_PROPERTY,
        .OP_GET_OBJECT_PROPERTY,
        .OP_GET_LIST_SUBSCRIPT,
        .OP_GET_STRING_SUBSCRIPT,
        .OP_GET_MAP_SUBSCRIPT,
        .OP_GET_FCONTAINER_INSTANCE_PROPERTY,
        .OP_SET_FCONTAINER_INSTANCE_PROPERTY,
        .OP_COPY,
        .OP_DBG_LOCAL_EXIT,
        => byteInstruction(instruction, chunk, offset),

        .OP_OBJECT,
        .OP_LIST,
        .OP_MAP,
        .OP_GET_PROTOCOL_METHOD,
        .OP_CONSTANT,
        => constantInstruction(instruction, chunk, offset),

        .OP_JUMP,
        .OP_JUMP_IF_FALSE,
        .OP_JUMP_IF_NOT_NULL,
        .OP_HOTSPOT,
        .OP_TRY,
        => jumpInstruction(instruction, chunk, true, offset),

        .OP_LOOP => jumpInstruction(instruction, chunk, false, offset),

        .OP_CALL_INSTANCE_PROPERTY,
        .OP_TAIL_CALL_INSTANCE_PROPERTY,
        .OP_INSTANCE_INVOKE,
        .OP_INSTANCE_TAIL_INVOKE,
        .OP_STRING_INVOKE,
        .OP_PATTERN_INVOKE,
        .OP_FIBER_INVOKE,
        .OP_LIST_INVOKE,
        .OP_MAP_INVOKE,
        .OP_RANGE_INVOKE,
        => invokeInstruction(instruction, chunk, offset),

        .OP_PROTOCOL_INVOKE,
        .OP_PROTOCOL_TAIL_INVOKE,
        => namedInvokeInstruction(instruction, chunk, offset),

        .OP_CALL,
        .OP_TAIL_CALL,
        => triInstruction(instruction, chunk, offset),

        .OP_DBG_GLOBAL_DEFINE => {
            const slot = chunk.code.items[offset + 1];
            const constant: u24 = @intCast(chunk.code.items[offset + 2]);
            const value_str = chunk.constants.items[constant].toStringAlloc(global_allocator) catch @panic("Out of memory");
            defer global_allocator.free(value_str);

            print(
                "OP_DBG_GLOBAL_DEFINE\t{} {} {s}",
                .{
                    slot,
                    constant,
                    value_str,
                },
            );

            return offset + 3;
        },

        .OP_DBG_LOCAL_ENTER => {
            const arg_instruction = chunk.code.items[offset + 1];
            const slot: u8 = @intCast(arg_instruction >> 24);
            const constant: u24 = @intCast(0x00ffffff & arg_instruction);
            const value_str = chunk.constants.items[constant].toStringAlloc(global_allocator) catch @panic("Out of memory");
            defer global_allocator.free(value_str);

            print(
                "OP_DBG_LOCAL_ENTER\t{} {} {s}",
                .{
                    slot,
                    constant,
                    value_str,
                },
            );

            return offset + 2;
        },

        .OP_CLOSURE => closure: {
            const constant: u24 = arg;
            var off_offset: usize = offset + 1;

            var value_str = chunk.constants.items[constant].toStringAlloc(global_allocator) catch @panic("Out of memory");
            defer global_allocator.free(value_str);

            print(
                "{s}\t{} {s}",
                .{
                    @tagName(instruction),
                    constant,
                    value_str[0..@min(value_str.len, 100)],
                },
            );

            const function: *obj.ObjFunction = obj.ObjFunction.cast(chunk.constants.items[constant].obj()).?;
            var i: u8 = 0;
            while (i < function.upvalue_count) : (i += 1) {
                const is_local: bool = chunk.code.items[off_offset] == 1;
                off_offset += 1;
                const index: u8 = @intCast(chunk.code.items[off_offset]);
                off_offset += 1;
                print(
                    "\n{:0>3} |                         \t{s} {}\n",
                    .{
                        off_offset - 2,
                        if (is_local) "local  " else "upvalue",
                        index,
                    },
                );
            }

            break :closure off_offset;
        },
    };
}

pub const DumpState = struct {
    const Self = @This();

    vm: *VM,
    seen: std.AutoHashMapUnmanaged(*obj.Obj, void) = .empty,
    depth: usize = 0,
    tab: usize = 0,

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        self.seen.deinit(allocator);
    }

    pub fn valueDump(state: *DumpState, value: Value, out: anytype, same_line: bool) void {
        if (state.depth > 50) {
            out.print("...", .{}) catch unreachable;
            return;
        }

        state.depth += 1;

        if (!same_line) {
            for (0..state.tab) |_| {
                out.writeAll("    ") catch unreachable;
            }
        }

        if (value.isNull()) {
            out.print("null", .{}) catch unreachable;
        } else if (!value.isObj() or state.seen.get(value.obj()) != null) {
            const string = value.toStringAlloc(state.vm.gc.allocator) catch unreachable;
            defer state.vm.gc.allocator.free(string);

            out.print("{s}", .{string}) catch unreachable;
        } else {
            state.seen.put(state.vm.gc.allocator, value.obj(), {}) catch unreachable;

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
                    const string = value.toStringAlloc(state.vm.gc.allocator) catch unreachable;
                    defer state.vm.gc.allocator.free(string);

                    out.print("{s}", .{string}) catch unreachable;
                },

                .UpValue => {
                    const upvalue = obj.ObjUpValue.cast(value.obj()).?;

                    state.valueDump(
                        if (upvalue.closed != null)
                            upvalue.closed.?
                        else
                            upvalue.location.*,
                        out,
                        false,
                    );
                },

                .String => {
                    const string = obj.ObjString.cast(value.obj()).?;

                    out.print("\"{s}\"", .{string.string}) catch unreachable;
                },

                .Pattern => {
                    const pattern = obj.ObjPattern.cast(value.obj()).?;

                    out.print("$\"{s}\"", .{pattern.source}) catch unreachable;
                },

                .Range => {
                    const range = obj.ObjRange.cast(value.obj()).?;

                    out.print("{}..{}", .{ range.low, range.high }) catch unreachable;
                },

                .List => {
                    const list = obj.ObjList.cast(value.obj()).?;

                    out.print(
                        "{s}[{s}",
                        .{
                            if (list.type_def.resolved_type.?.List.mutable)
                                "mut "
                            else
                                "",
                            if (list.items.items.len > 0)
                                "\n"
                            else
                                "",
                        },
                    ) catch unreachable;
                    state.tab += 1;
                    for (list.items.items) |item| {
                        state.valueDump(
                            item,
                            out,
                            false,
                        );
                        out.print(",\n", .{}) catch unreachable;
                    }
                    state.tab -= 1;
                    out.print("]", .{}) catch unreachable;
                },

                .Map => {
                    const map = obj.ObjMap.cast(value.obj()).?;

                    out.print(
                        "{s}{{{s}",
                        .{
                            if (map.type_def.resolved_type.?.Map.mutable)
                                "mut "
                            else
                                "",
                            if (map.map.count() > 0)
                                "\n"
                            else
                                "",
                        },
                    ) catch unreachable;
                    state.tab += 1;
                    var it = map.map.iterator();
                    while (it.next()) |kv| {
                        const key = kv.key_ptr.*;

                        state.valueDump(
                            key,
                            out,
                            false,
                        );
                        out.writeAll(": ") catch unreachable;
                        state.valueDump(
                            kv.value_ptr.*,
                            out,
                            true,
                        );
                        out.writeAll(",\n") catch unreachable;
                    }
                    state.tab -= 1;
                    out.print("}}", .{}) catch unreachable;
                },

                .Enum => {
                    const enumeration = obj.ObjEnum.cast(value.obj()).?;
                    const enum_type_def = enumeration.type_def.resolved_type.?.Enum;
                    const enum_value_type_def = enumeration.type_def.resolved_type.?.Enum.enum_type.toStringAlloc(state.vm.gc.allocator, true) catch unreachable;
                    defer state.vm.gc.allocator.free(enum_value_type_def);

                    out.print(
                        "enum<{s}> {s} {{\n",
                        .{
                            enum_value_type_def,
                            enum_type_def.name.string,
                        },
                    ) catch unreachable;
                    state.tab += 1;
                    for (enum_type_def.cases, 0..) |case, i| {
                        out.print("    {s} -> ", .{case}) catch unreachable;
                        state.valueDump(
                            enumeration.cases[i],
                            out,
                            true,
                        );
                        out.writeAll(",\n") catch unreachable;
                    }
                    state.tab -= 1;
                    out.print("}}", .{}) catch unreachable;
                },

                .Object => {
                    const object = obj.ObjObject.cast(value.obj()).?;
                    const object_def = object.type_def.resolved_type.?.Object;

                    out.print("object", .{}) catch unreachable;
                    if (object_def.conforms_to.count() > 0) {
                        out.print("<", .{}) catch unreachable;
                        var it = object_def.conforms_to.iterator();
                        while (it.next()) |kv| {
                            out.print("{s}, ", .{kv.key_ptr.*.resolved_type.?.Protocol.name.string}) catch unreachable;
                        }
                        out.print(">", .{}) catch unreachable;
                    }

                    out.print(" {s} {{\n", .{object_def.name.string}) catch unreachable;
                    state.tab += 1;

                    var it = object_def.fields.iterator();
                    while (it.next()) |kv| {
                        const field = kv.value_ptr.*;
                        const field_type_str = field.type_def.toStringAlloc(state.vm.gc.allocator, true) catch unreachable;
                        defer state.vm.gc.allocator.free(field_type_str);

                        if (!field.method) {
                            out.print(
                                "    {s}{s}{s}: {s}",
                                .{
                                    if (kv.value_ptr.*.static) "static " else "",
                                    if (kv.value_ptr.*.final) "final " else "",
                                    kv.key_ptr.*,
                                    field_type_str,
                                },
                            ) catch unreachable;

                            if (if (field.static)
                                object.fields[field.index]
                            else if (field.has_default)
                                object.defaults[field.index]
                            else
                                null) |v|
                            {
                                out.print(" = ", .{}) catch unreachable;
                                state.valueDump(
                                    v,
                                    out,
                                    true,
                                );
                            }

                            out.print(",\n", .{}) catch unreachable;
                        } else {
                            out.print(
                                "    {s}{s}{s}\n",
                                .{
                                    if (field.static) "static " else "",
                                    if (field.mutable) "mut " else "",
                                    field_type_str,
                                },
                            ) catch unreachable;
                        }
                    }

                    state.tab -= 1;
                    out.print("}}", .{}) catch unreachable;
                },

                .ObjectInstance => {
                    const object_instance = obj.ObjObjectInstance.cast(value.obj()).?;
                    const fields = object_instance.type_def.resolved_type.?.ObjectInstance.of
                        .resolved_type.?.Object
                        .fields;

                    out.print(
                        "{s}{s}{{\n",
                        .{
                            if (object_instance.type_def.resolved_type.?.ObjectInstance.mutable)
                                "mut "
                            else
                                "",
                            if (object_instance.object) |object|
                                object.type_def.resolved_type.?.Object.name.string
                            else
                                ".",
                        },
                    ) catch unreachable;
                    state.tab += 1;
                    for (object_instance.fields, 0..) |val, idx| {
                        out.print(
                            "    {s} = ",
                            .{
                                fields.keys()[idx],
                            },
                        ) catch unreachable;
                        state.valueDump(
                            val,
                            out,
                            true,
                        );
                        out.print(",\n", .{}) catch unreachable;
                    }
                    state.tab -= 1;
                    out.print("}}", .{}) catch unreachable;
                },

                .ForeignContainer => {
                    const foreign = obj.ObjForeignContainer.cast(value.obj()).?;
                    const foreign_def = foreign.type_def.resolved_type.?.ForeignContainer;

                    out.print(
                        "{s}{{\n",
                        .{foreign_def.name.string},
                    ) catch unreachable;

                    var it = foreign_def.fields.iterator();
                    while (it.next()) |kv| {
                        out.print("    {s} = ", .{kv.key_ptr.*}) catch unreachable;
                        state.valueDump(
                            kv.value_ptr.*.getter(
                                state.vm,
                                foreign.data.ptr,
                            ),
                            out,
                            true,
                        );
                        out.print(",\n", .{}) catch unreachable;
                    }
                    out.print("}}", .{}) catch unreachable;
                },
            }

            _ = state.seen.remove(value.obj());
        }

        state.depth -= 1;
    }
};
