const std = @import("std");
const print = std.debug.print;
const Chunk = @import("Chunk.zig");
const Value = @import("value.zig").Value;
const _obj = @import("obj.zig");
const _vm = @import("vm.zig");
const global_allocator = @import("buzz_api.zig").allocator;

const VM = _vm.VM;
const OpCode = Chunk.OpCode;
const ObjFunction = _obj.ObjFunction;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) !void {
    print("\u{001b}[2m", .{}); // Dimmed
    print("=== {s} ===\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = try disassembleInstruction(chunk, offset);
    }
    print("\u{001b}[0m", .{});
}

fn invokeInstruction(code: OpCode, chunk: *Chunk, offset: usize) !usize {
    const constant: u24 = @intCast(0x00ffffff & chunk.code.items[offset]);
    const arg_count: u8 = @intCast(chunk.code.items[offset + 1] >> 24);
    const catch_count: u24 = @intCast(0x00ffffff & chunk.code.items[offset + 1]);

    var value_str = try chunk.constants.items[constant].toStringAlloc(global_allocator);
    defer value_str.deinit();

    print("{s}\t{s}({} args, {} catches)", .{
        @tagName(code),
        value_str.items[0..@min(value_str.items.len, 100)],
        arg_count,
        catch_count,
    });

    return offset + 2;
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

fn constantInstruction(code: OpCode, chunk: *Chunk, offset: usize) !usize {
    const constant: u24 = @intCast(0x00ffffff & chunk.code.items[offset]);
    var value_str = try chunk.constants.items[constant].toStringAlloc(global_allocator);
    defer value_str.deinit();

    print(
        "{s}\t{} {s}",
        .{
            @tagName(code),
            constant,
            value_str.items[0..@min(value_str.items.len, 100)],
        },
    );

    return offset + 1;
}

fn jumpInstruction(code: OpCode, chunk: *Chunk, direction: bool, offset: usize) !usize {
    const jump: u24 = @intCast(0x00ffffff & chunk.code.items[offset]);

    if (direction) {
        print(
            "{s}\t{} -> {}",
            .{
                @tagName(code),
                offset,
                offset + 1 + 1 * jump,
            },
        );
    } else {
        print(
            "{s}\t{} -> {}",
            .{
                @tagName(code),
                offset,
                offset + 1 - 1 * jump,
            },
        );
    }

    return offset + 1;
}

fn tryInstruction(code: OpCode, chunk: *Chunk, offset: usize) !usize {
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
    while (@intFromPtr(value) < @intFromPtr(vm.current_fiber.stack_top)) {
        var value_str = value[0].toStringAlloc(global_allocator) catch unreachable;
        defer value_str.deinit();

        if (vm.currentFrame().?.slots == value) {
            print(
                "{} {} {s} frame\n ",
                .{
                    @intFromPtr(value),
                    value[0].val,
                    value_str.items[0..@min(value_str.items.len, 100)],
                },
            );
        } else {
            print(
                "{} {} {s}\n ",
                .{
                    @intFromPtr(value),
                    value[0].val,
                    value_str.items[0..@min(value_str.items.len, 100)],
                },
            );
        }

        value += 1;
    }
    print("{} top\n", .{@intFromPtr(vm.current_fiber.stack_top)});

    print("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n\n\n", .{});
    print("\u{001b}[0m", .{});
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) !usize {
    print("\n{:0>3} ", .{offset});
    const lines = chunk.ast.tokens.items(.line);

    if (offset > 0 and lines[chunk.lines.items[offset]] == lines[chunk.lines.items[offset - 1]]) {
        print("|   ", .{});
    } else {
        print("{:0>3} ", .{lines[chunk.lines.items[offset]]});
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
        .OP_SUBTRACT,
        .OP_MULTIPLY,
        .OP_DIVIDE,
        .OP_NOT,
        .OP_NEGATE,
        .OP_BAND,
        .OP_BOR,
        .OP_XOR,
        .OP_BNOT,
        .OP_SHL,
        .OP_SHR,
        .OP_MOD,
        .OP_UNWRAP,
        .OP_GET_ENUM_CASE_VALUE,
        .OP_LIST_APPEND,
        .OP_SET_MAP,
        .OP_GET_LIST_SUBSCRIPT,
        .OP_GET_MAP_SUBSCRIPT,
        .OP_GET_STRING_SUBSCRIPT,
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
        .OP_RESUME,
        .OP_YIELD,
        .OP_RESOLVE,
        .OP_TRY_END,
        .OP_GET_ENUM_CASE_FROM_VALUE,
        .OP_TYPEOF,
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
        .OP_MAP,
        .OP_EXPORT,
        .OP_COPY,
        .OP_CLONE,
        .OP_CLOSE_UPVALUE,
        .OP_RETURN,
        => byteInstruction(instruction, chunk, offset),

        .OP_OBJECT,
        .OP_LIST,
        .OP_RANGE,
        .OP_METHOD,
        .OP_PROPERTY,
        .OP_GET_OBJECT_PROPERTY,
        .OP_GET_INSTANCE_PROPERTY,
        .OP_GET_FCONTAINER_INSTANCE_PROPERTY,
        .OP_GET_LIST_PROPERTY,
        .OP_GET_MAP_PROPERTY,
        .OP_GET_STRING_PROPERTY,
        .OP_GET_PATTERN_PROPERTY,
        .OP_GET_FIBER_PROPERTY,
        .OP_SET_OBJECT_PROPERTY,
        .OP_SET_INSTANCE_PROPERTY,
        .OP_SET_FCONTAINER_INSTANCE_PROPERTY,
        .OP_CONSTANT,
        => try constantInstruction(instruction, chunk, offset),

        .OP_JUMP,
        .OP_JUMP_IF_FALSE,
        .OP_JUMP_IF_NOT_NULL,
        => jumpInstruction(instruction, chunk, true, offset),

        .OP_TRY => tryInstruction(instruction, chunk, offset),

        .OP_LOOP => jumpInstruction(instruction, chunk, false, offset),

        .OP_INSTANCE_INVOKE,
        .OP_INSTANCE_TAIL_INVOKE,
        .OP_STRING_INVOKE,
        .OP_PATTERN_INVOKE,
        .OP_FIBER_INVOKE,
        .OP_LIST_INVOKE,
        .OP_MAP_INVOKE,
        => try invokeInstruction(instruction, chunk, offset),

        .OP_CALL,
        .OP_TAIL_CALL,
        .OP_FIBER,
        .OP_INVOKE_FIBER,
        => triInstruction(instruction, chunk, offset),

        .OP_CLOSURE => closure: {
            const constant: u24 = arg;
            var off_offset: usize = offset + 1;

            var value_str = try chunk.constants.items[constant].toStringAlloc(global_allocator);
            defer value_str.deinit();

            print(
                "{s}\t{} {s}",
                .{
                    @tagName(instruction),
                    constant,
                    value_str.items[0..@min(value_str.items.len, 100)],
                },
            );

            const function: *ObjFunction = ObjFunction.cast(chunk.constants.items[constant].obj()).?;
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
