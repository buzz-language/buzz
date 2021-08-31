const std = @import("std");
const _value = @import("./value.zig");
const _obj = @import("./obj.zig");
const _chunk = @import("./chunk.zig");
const disassembler = @import("./disassembler.zig");
const Allocator = std.mem.Allocator;
const Value = _value.Value;
const ValueType = _value.ValueType;
const ObjClosure = _obj.ObjClosure;
const ObjFunction = _obj.ObjFunction;
const ObjUpValue = _obj.ObjUpValue;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjString = _obj.ObjString;
const Obj = _obj.Obj;
const OpCode = _chunk.OpCode;

pub const CallFrame = struct {
    closure: *ObjClosure,
    // Index into closure's chunk
    ip: usize,
    slots: [*]Value,
};

pub const VM = struct {
    const Self = @This();

    pub const init_string: []const u8 = "init";
    pub const this_string: []const u8 = "this";
    pub const empty_string: []const u8 = "";
    pub const script_string: []const u8 = "<script>";

    pub const InterpretResult = enum {
        Ok,
        CompileError,
        RuntimeError,
    };

    allocator: *Allocator,

    frames: std.ArrayList(CallFrame),

    // TODO: put ta limit somewhere
    stack: [1000000]Value,
    stack_top: usize = 0,
    // Interned strings
    strings: std.StringHashMap(*ObjString),
    // Interned typedef, find a better way of hashing a key (won't accept float so we use toString)
    type_defs: std.StringHashMap(*ObjTypeDef),
    open_upvalues: ?*ObjUpValue,

    bytes_allocated: usize = 0,
    next_gc: usize = 1024 * 1024,
    // TODO: replace with SinglyLinkedList(*Obj)
    objects: ?*Obj = null,
    gray_stack: std.ArrayList(*Obj),

    pub fn init(allocator: *Allocator) Self {
        var self: Self = .{
            .allocator = allocator,
            .stack = [_]Value { .{ .Null = null } } ** 1000000,
            .frames = std.ArrayList(CallFrame).init(allocator),
            .strings = std.StringHashMap(*ObjString).init(allocator),
            .type_defs = std.StringHashMap(*ObjTypeDef).init(allocator),
            .open_upvalues = null,
            .gray_stack = std.ArrayList(*Obj).init(allocator),
        };

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.frames.deinit();
        self.strings.deinit();

        // TODO: key are strings on the heap so free them, does this work?
        var it = self.type_defs.iterator();
        while (it.next()) |kv| {
            self.allocator.free(kv.key_ptr.*);
        }

        self.type_defs.deinit();
        
        while (self.open_upvalues) |upvalue| {
            self.open_upvalues = upvalue.next;

            self.allocator.destroy(upvalue);
        }

        self.gray_stack.deinit();
    }

    pub fn getTypeDef(self: *Self, type_def: ObjTypeDef) !*ObjTypeDef {
        var type_def_str: []const u8 = try type_def.toString(self.allocator);

        if (self.type_defs.get(type_def_str)) |type_def_ptr| {
            return type_def_ptr;
        }

        var type_def_ptr: *ObjTypeDef = ObjTypeDef.cast(try _obj.allocateObject(self, .Type)).?;
        type_def_ptr.* = type_def;

        _ = try self.type_defs.put(type_def_str, type_def_ptr);

        return type_def_ptr;
    }

    pub fn push(self: *Self, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn pop(self: *Self) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    pub fn peek(self: *Self, distance: u32) Value {
        return self.stack[self.stack_top - 1 - distance];
    }

    pub fn interpret(self: *Self, function: *ObjFunction) !?InterpretResult {
        self.push(.{
            .Obj = function.toObj()
        });

        var closure: *ObjClosure = try self.allocator.create(ObjClosure);
        closure.* = try ObjClosure.init(self.allocator, function);

        _ = self.pop();

        self.push(.{
            .Obj = closure.toObj()
        });

        _ = try self.call(closure, 0);

        return try self.run();
    }

    inline fn readByte(frame: *CallFrame) u8 {
        // TODO: measure if [*]OpCode[0] is faster
        var byte: u8 = frame.closure.function.chunk.code.items[frame.ip];

        frame.ip += 1;

        return byte;
    }

    inline fn readOpCode(frame: *CallFrame) OpCode {
        // TODO: measure if [*]OpCode[0] is faster
        var opcode: OpCode = @intToEnum(OpCode, frame.closure.function.chunk.code.items[frame.ip]);

        frame.ip += 1;

        return opcode;
    }

    inline fn readConstant(frame: *CallFrame) Value {
        return frame.closure.function.chunk.constants.items[readByte(frame)];
    }

    inline fn readString(frame: *CallFrame) *ObjString {
        return ObjString.cast(readConstant(frame).Obj);
    }

    fn run(self: *Self) !InterpretResult {
        var frame: *CallFrame = &self.frames.items[self.frames.items.len - 1];

        try disassembler.disassembleChunk(
            &frame.closure.function.chunk,
            frame.closure.function.name.string
        );

        while (true) {
            var instruction: OpCode = readOpCode(frame);
            switch(instruction) {
                .OP_NULL         => self.push(Value { .Null = null }),
                .OP_TRUE         => self.push(Value { .Boolean = true }),
                .OP_FALSE        => self.push(Value { .Boolean = false }),
                .OP_POP          => _ = self.pop(),
                .OP_NOT          => self.push(Value { .Boolean = isFalse(self.pop()) }),
                .OP_GET_LOCAL    => self.push(frame.slots[readByte(frame)]),
                .OP_SET_LOCAL    => frame.slots[readByte(frame)] = self.peek(0),
                .OP_GET_UPVALUE  => self.push(frame.closure.upvalues.items[readByte(frame)].location.*),
                .OP_SET_UPVALUE  => frame.closure.upvalues.items[readByte(frame)].location.* = self.peek(0),
                .OP_CONSTANT     => self.push(readConstant(frame)),
                .OP_NEGATE       => {
                    if (@as(ValueType, self.peek(0)) != .Number) {
                        runtimeError("Operand must be a number.");

                        return .RuntimeError;
                    }

                    self.push(Value{ .Number = -self.pop().Number });
                },
                .OP_CLOSURE      => {
                    var function: *ObjFunction = ObjFunction.cast(readConstant(frame).Obj).?;
                    var closure: *ObjClosure = ObjClosure.cast(try _obj.allocateObject(self, .Closure)).?;
                    closure.* = try ObjClosure.init(self.allocator, function);

                    self.push(Value{ .Obj = closure.toObj() });

                    for (closure.upvalues.items) |_, i| {
                        var is_local: bool = readByte(frame) == 1;
                        var index: u8 = readByte(frame);

                        if (is_local) {
                            closure.upvalues.items[i] = try self.captureUpvalue(&(frame.slots + index)[0]);
                        } else {
                            closure.upvalues.items[i] = frame.closure.upvalues.items[index];
                        }
                    }
                },

                // TODO: for now, used to debug
                .OP_RETURN => {
                    var value: []const u8 = try _value.valueToString(std.heap.c_allocator, self.pop());
                    defer std.heap.c_allocator.free(value);

                    std.debug.warn("\nReturned with: {s}\n", .{ value });

                    std.os.exit(1);
                },

                else => {
                    std.debug.warn("{} not yet implemented\n", .{ instruction });

                    std.os.exit(1);
                }
            }
        }

        return InterpretResult.Ok;
    }

    fn call(self: *Self, closure: *ObjClosure, arg_count: u8) !bool {
        if (arg_count != closure.function.parameters.count()) {
            // TODO: runtimeError("Expected %d arguments but got %d.", closure->function->arity, argCount);
            
            return false;
        }
        
        // TODO: do we check for stack overflow

        try self.frames.append(CallFrame {
            .closure = closure,
            .ip = 0,
            .slots = @ptrCast([*]Value, self.stack[(self.stack_top - arg_count - 1)..]),
        });

        return true;
    }

    fn captureUpvalue(self: *Self, local: *Value) !*ObjUpValue {
        var prev_upvalue: ?*ObjUpValue = null;
        var upvalue: ?*ObjUpValue = self.open_upvalues;
        while (upvalue != null and @ptrToInt(upvalue.?.location) > @ptrToInt(local)) {
            prev_upvalue = upvalue;
            upvalue = upvalue.?.next;
        }

        if (upvalue != null and upvalue.?.location == local) {
            return upvalue.?;
        }

        var created_upvalue: *ObjUpValue = ObjUpValue.cast(try _obj.allocateObject(self, .UpValue)).?;
        created_upvalue.* = ObjUpValue.init(local);
        created_upvalue.next = upvalue;

        if (prev_upvalue) |uprev_upvalue| {
            uprev_upvalue.next = created_upvalue;
        } else {
            self.open_upvalues = created_upvalue;
        }

        return created_upvalue;
    }

    fn isFalse(value: Value) bool {
        if (@as(ValueType, value) != .Boolean) {
            runtimeError("Expected boolean but got ...");
        }

        return value.Boolean == false;
    }

    fn runtimeError(error_message: []const u8) void {
        // TODO
        std.debug.warn("\u{001b}[31m{s}\u{001b}[0m\n", .{ error_message });

        std.os.exit(1);
    }
};
