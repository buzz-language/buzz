const std = @import("std");
const print = std.debug.print;
const _chunk = @import("./chunk.zig");
const _value = @import("./value.zig");
const _obj = @import("./obj.zig");
const _vm = @import("./vm.zig");

const VM = _vm.VM;
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;
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
    const constant: u24 = @intCast(u24, 0x00ffffff & chunk.code.items[offset]);
    const arg_count: u8 = @intCast(u8, chunk.code.items[offset + 1] >> 24);
    const catch_count: u24 = @intCast(u8, 0x00ffffff & chunk.code.items[offset + 1]);

    var value_str: []const u8 = try _value.valueToString(std.heap.c_allocator, chunk.constants.items[constant]);
    defer std.heap.c_allocator.free(value_str);

    print("{}\t{s}({} args, {} catches)", .{ code, value_str, arg_count, catch_count });

    return offset + 2;
}

fn simpleInstruction(code: OpCode, offset: usize) usize {
    print("{}\t", .{code});

    return offset + 1;
}

fn byteInstruction(code: OpCode, chunk: *Chunk, offset: usize) usize {
    const slot: u24 = @intCast(u24, 0x00ffffff & chunk.code.items[offset]);
    print("{}\t{}", .{ code, slot });
    return offset + 1;
}

fn bytesInstruction(code: OpCode, chunk: *Chunk, offset: usize) usize {
    const a: u24 = @intCast(u24, 0x00ffffff & chunk.code.items[offset]);
    const b: u24 = @intCast(u24, chunk.code.items[offset + 1]);

    print("{}\t{} {}", .{ code, a, b });
    return offset + 2;
}

fn triInstruction(code: OpCode, chunk: *Chunk, offset: usize) usize {
    const full_instruction: u32 = chunk.code.items[offset];

    const a: u8 = @intCast(u8, (0x00ffffff & full_instruction) >> 16);
    const b: u16 = @intCast(u16, 0x0000ffff & full_instruction);

    print("{}\t{} {}", .{ code, a, b });
    return offset + 1;
}

fn constantInstruction(code: OpCode, chunk: *Chunk, offset: usize) !usize {
    const constant: u24 = @intCast(u24, 0x00ffffff & chunk.code.items[offset]);
    var value_str: []const u8 = try _value.valueToString(std.heap.c_allocator, chunk.constants.items[constant]);
    defer std.heap.c_allocator.free(value_str);

    print("{}\t{} {s}", .{ code, constant, value_str });

    return offset + 1;
}

fn jumpInstruction(code: OpCode, chunk: *Chunk, direction: bool, offset: usize) !usize {
    const jump: u24 = @intCast(u24, 0x00ffffff & chunk.code.items[offset]);

    if (direction) {
        print("{}\t{} -> {}", .{ code, offset, offset + 1 + 1 * jump });
    } else {
        print("{}\t{} -> {}", .{ code, offset, offset + 1 - 1 * jump });
    }

    return offset + 1;
}

pub fn dumpStack(vm: *VM) !void {
    print("\u{001b}[2m", .{}); // Dimmed
    print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n", .{});

    var value = @ptrCast([*]_value.Value, vm.stack[0..]);
    while (@ptrToInt(value) < @ptrToInt(vm.stack_top)) {
        var value_str: []const u8 = try _value.valueToString(std.heap.c_allocator, value[0]);
        defer std.heap.c_allocator.free(value_str);

        if (vm.currentFrame().?.slots == value) {
            print("{*} {s} frame\n ", .{ value, value_str });
        } else {
            print("{*} {s}\n ", .{ value, value_str });
        }

        value += 1;
    }
    print("{*} top\n", .{vm.stack_top});

    print("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n\n\n", .{});
    print("\u{001b}[0m", .{});
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) !usize {
    print("\n{:0>3} ", .{offset});

    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        print("|   ", .{});
    } else {
        print("{:0>3} ", .{chunk.lines.items[offset]});
    }

    const full_instruction: u32 = chunk.code.items[offset];
    const instruction: OpCode = @intToEnum(OpCode, @intCast(u8, full_instruction >> 24));
    const arg: u24 = @intCast(u24, 0x00ffffff & full_instruction);
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
        .OP_ADD,
        .OP_SUBTRACT,
        .OP_MULTIPLY,
        .OP_DIVIDE,
        .OP_NOT,
        .OP_NEGATE,
        .OP_MOD,
        .OP_SHL,
        .OP_SHR,
        .OP_UNWRAP,
        .OP_NULL_OR,
        .OP_ENUM_CASE,
        .OP_GET_ENUM_CASE_VALUE,
        .OP_LIST_APPEND,
        .OP_SET_MAP,
        .OP_GET_SUBSCRIPT,
        .OP_SET_SUBSCRIPT,
        .OP_THROW,
        .OP_IMPORT,
        .OP_TO_STRING,
        .OP_INSTANCE,
        .OP_FOREACH,
        .OP_GET_SUPER,
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
        .OP_CLOSE_UPVALUE,
        .OP_RETURN,
        => byteInstruction(instruction, chunk, offset),

        .OP_OBJECT,
        .OP_ENUM,
        .OP_LIST,
        .OP_CLASS,
        .OP_METHOD,
        .OP_PROPERTY,
        .OP_GET_PROPERTY,
        .OP_SET_PROPERTY,
        .OP_INHERIT,
        .OP_CONSTANT,
        => try constantInstruction(instruction, chunk, offset),

        .OP_JUMP, .OP_JUMP_IF_FALSE => jumpInstruction(instruction, chunk, true, offset),

        .OP_LOOP => jumpInstruction(instruction, chunk, false, offset),

        .OP_SUPER_INVOKE, .OP_INVOKE => try invokeInstruction(instruction, chunk, offset),
        .OP_CALL => triInstruction(instruction, chunk, offset),

        .OP_CLOSURE => closure: {
            var constant: u24 = arg;
            var off_offset: usize = offset + 1;

            var value_str: []const u8 = try _value.valueToString(std.heap.c_allocator, chunk.constants.items[constant]);
            defer std.heap.c_allocator.free(value_str);

            print("{}\t{} {s}", .{ instruction, constant, value_str });

            var function: *ObjFunction = ObjFunction.cast(chunk.constants.items[constant].Obj).?;
            var i: u8 = 0;
            while (i < function.upvalue_count) : (i += 1) {
                var is_local: bool = chunk.code.items[off_offset] == 1;
                off_offset += 1;
                var index: u8 = @intCast(u8, chunk.code.items[off_offset]);
                off_offset += 1;
                print("\n{:0>3} |                         \t{s} {}\n", .{ off_offset - 2, if (is_local) "local  " else "upvalue", index });
            }

            break :closure off_offset;
        },

        .OP_PRINT => simpleInstruction(instruction, offset),
    };
}
