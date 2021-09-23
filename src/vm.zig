// zig fmt: off
const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
usingnamespace @import("./value.zig");
usingnamespace @import("./obj.zig");
usingnamespace @import("./chunk.zig");
usingnamespace @import("./disassembler.zig");
const Allocator = std.mem.Allocator;
// TODO: usingnamespace ?
usingnamespace @import("./obj.zig");
const Config = @import("./config.zig").Config;

fn invertedList(comptime T: type, list: std.ArrayList(T)) !std.ArrayList(T) {
    var inverted = try std.ArrayList(T).initCapacity(list.allocator, list.items.len);

    for (list.items) |item| {
        try inverted.insert(0, item);
    }

    return inverted;
}

pub const CallFrame = struct {
    closure: *ObjClosure,
    // Index into closure's chunk
    ip: usize,
    slots: [*]Value,

    // Line in source code where the call occured
    call_site: ?usize,
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
    frame_count: u64 = 0,
    current_frame: ?*CallFrame = null,

    // TODO: put ta limit somewhere
    stack: []Value,
    stack_top: [*]Value,
    globals: std.ArrayList(Value),
    // Interned strings
    strings: *std.StringHashMap(*ObjString),
    open_upvalues: ?*ObjUpValue,

    bytes_allocated: usize = 0,
    next_gc: usize = if (Config.debug_gc) 1024 else 1024 * 1024,
    // TODO: replace with SinglyLinkedList(*Obj)
    objects: ?*Obj = null,
    gray_stack: std.ArrayList(*Obj),

    pub fn init(allocator: *Allocator, strings: *std.StringHashMap(*ObjString)) !Self {
        var self: Self = .{
            .allocator = allocator,
            .stack = try allocator.alloc(Value, 1000000),
            .stack_top = undefined,
            .globals = std.ArrayList(Value).init(allocator),
            .frames = std.ArrayList(CallFrame).init(allocator),
            .strings = strings,
            .open_upvalues = null,
            .gray_stack = std.ArrayList(*Obj).init(allocator),
        };

        self.stack_top = @ptrCast([*]Value, self.stack[0..]);

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.stack);

        self.frames.deinit();
        
        // TODO: free all objects except exported ones (be careful of indirected exported stuff like object of objectinstance)

        self.gray_stack.deinit();
        self.globals.deinit();
    }

    pub fn push(self: *Self, value: Value) void {
        self.stack_top[0] = value;
        self.stack_top += 1;
    }

    pub fn pop(self: *Self) Value {
        self.stack_top -= 1;
        return self.stack_top[0];
    }

    pub fn peek(self: *Self, distance: u32) Value {
        return (self.stack_top - 1 - distance)[0];
    }

    inline fn swap(self: *Self, from: u8, to: u8) void {
        var temp: Value = (self.stack_top - to - 1)[0];
        (self.stack_top - to - 1)[0] = (self.stack_top - from - 1)[0];
        (self.stack_top - from - 1)[0] = temp;
    }

    pub fn interpret(self: *Self, function: *ObjFunction) !?InterpretResult {        
        self.push(.{
            .Obj = function.toObj()
        });

        var closure: *ObjClosure = try allocateObject(self, ObjClosure, try ObjClosure.init(self.allocator, function));

        _ = self.pop();

        self.push(.{
            .Obj = closure.toObj()
        });

        _ = try self.call(closure, 0);

        return try self.run();
    }

    inline fn readByte(self: *Self) u8 {
        // TODO: measure if [*]OpCode[0] is faster
        var byte: u8 = self.current_frame.?.closure.function.chunk.code.items[self.current_frame.?.ip];

        self.current_frame.?.ip += 1;

        return byte;
    }

    inline fn readShort(self: *Self) u16 {
        self.current_frame.?.ip += 2;

        const byte1: u16 = @intCast(u16, self.current_frame.?.closure.function.chunk.code.items[self.current_frame.?.ip - 2]);
        const byte2: u16 = @intCast(u16, self.current_frame.?.closure.function.chunk.code.items[self.current_frame.?.ip - 1]);

        return @intCast(u16, (byte1 << 8) | byte2);
    }

    inline fn readOpCode(self: *Self) OpCode {
        // TODO: measure if [*]OpCode[0] is faster
        var opcode: OpCode = @intToEnum(OpCode, self.current_frame.?.closure.function.chunk.code.items[self.current_frame.?.ip]);

        self.current_frame.?.ip += 1;

        return opcode;
    }

    inline fn readConstant(self: *Self) Value {
        return self.current_frame.?.closure.function.chunk.constants.items[self.readByte()];
    }

    inline fn readString(self: *Self) *ObjString {
        return ObjString.cast(self.readConstant().Obj).?;
    }

    fn run(self: *Self) !InterpretResult {
        self.current_frame = &self.frames.items[self.frame_count - 1];

        while (true) {
            var instruction: OpCode = self.readOpCode();
            switch(instruction) {
                .OP_NULL => self.push(Value { .Null = null }),
                .OP_TRUE => self.push(Value { .Boolean = true }),
                .OP_FALSE => self.push(Value { .Boolean = false }),
                .OP_POP => _ = self.pop(),
                .OP_SWAP => self.swap(self.readByte(), self.readByte()),
                .OP_DEFINE_GLOBAL => {
                    const slot: u8 = self.readByte();
                    try self.globals.ensureTotalCapacity(slot + 1);
                    self.globals.expandToCapacity();
                    self.globals.items[slot] = self.peek(0);
                    _ = self.pop();
                },
                .OP_GET_GLOBAL => self.push(self.globals.items[self.readByte()]),
                .OP_SET_GLOBAL => self.globals.items[self.readByte()] = self.peek(0),
                .OP_GET_LOCAL => self.push(self.current_frame.?.slots[self.readByte()]),
                .OP_SET_LOCAL => self.current_frame.?.slots[self.readByte()] = self.peek(0),
                .OP_GET_UPVALUE => self.push(self.current_frame.?.closure.upvalues.items[self.readByte()].location.*),
                .OP_SET_UPVALUE => self.current_frame.?.closure.upvalues.items[self.readByte()].location.* = self.peek(0),
                .OP_CONSTANT => self.push(self.readConstant()),
                .OP_NEGATE => {
                    if (@as(ValueType, self.peek(0)) != .Number) {
                        try self.runtimeError("Operand must be a number.", null);

                        return .RuntimeError;
                    }

                    self.push(Value{ .Number = -self.pop().Number });
                },
                .OP_CLOSURE => {
                    var function: *ObjFunction = ObjFunction.cast(self.readConstant().Obj).?;
                    var closure: *ObjClosure = try allocateObject(self, ObjClosure, try ObjClosure.init(self.allocator, function));

                    self.push(Value{ .Obj = closure.toObj() });

                    var i: usize = 0;
                    while (i < function.upvalue_count) : (i += 1) {
                        var is_local: bool = self.readByte() == 1;
                        var index: u8 = self.readByte();

                        if (is_local) {
                            try closure.upvalues.append(try self.captureUpvalue(&(self.current_frame.?.slots + index)[0]));
                        } else {
                            try closure.upvalues.append(self.current_frame.?.closure.upvalues.items[index]);
                        }
                    }
                },
                .OP_CALL => {
                    var arg_count: u8 = self.readByte();
                    if (!(try self.callValue(self.peek(arg_count), arg_count))) {
                        return .RuntimeError;
                    }

                    self.current_frame.? = &self.frames.items[self.frame_count - 1];
                },

                .OP_INVOKE => {
                    var method: *ObjString = self.readString();
                    var arg_count: u8 = self.readByte();
                    if (!try self.invoke(method, arg_count)) {
                        return .RuntimeError;
                    }

                    self.current_frame.? = &self.frames.items[self.frame_count - 1];
                },

                .OP_RETURN => {
                    if (try self.returnFrame()) {
                        return .Ok;
                    }
                },

                .OP_EXPORT => {
                    self.push(Value{ .Number = @intToFloat(f64, self.readByte()) });
                    return  .Ok;
                },

                .OP_IMPORT => {
                    try self.import(self.peek(0));
                },

                .OP_THROW => {
                    var message: *ObjString = ObjString.cast(self.peek(0).Obj).?;

                    try self.runtimeError(message.string, null);
                },

                .OP_CATCH => {
                    var try_closure: *ObjClosure = ObjClosure.cast(self.peek(1).Obj).?;
                    var catch_closure: *ObjClosure = ObjClosure.cast(self.peek(0).Obj).?;

                    try_closure.catch_closure = catch_closure;

                    _ = self.pop(); // Pop catch closure
                },

                .OP_LIST => {
                    var list: *ObjList = try allocateObject(self, ObjList, ObjList.init(self.allocator, ObjTypeDef.cast(self.readConstant().Obj).?));

                    self.push(Value{ .Obj = list.toObj() });
                },

                .OP_LIST_APPEND => try self.appendToList(),

                .OP_MAP => {
                    var map: *ObjMap = try allocateObject(self, ObjMap, ObjMap.init(
                        self.allocator,
                        ObjTypeDef.cast(self.readConstant().Obj).?,
                        ObjTypeDef.cast(self.readConstant().Obj).?
                    ));

                    self.push(Value{ .Obj = map.toObj() });
                },

                .OP_SET_MAP => {
                    var map: *ObjMap = ObjMap.cast(self.peek(2).Obj).?;
                    var key: Value = self.peek(1);
                    var value: Value = self.peek(0);

                    try map.map.put(valueToHashable(key), value);

                    _ = self.pop();
                    _ = self.pop();
                },

                .OP_GET_SUBSCRIPT => {
                    try self.subscript();
                },
                
                .OP_SET_SUBSCRIPT => {
                    try self.setSubscript();
                },

                .OP_ENUM => {
                    var enum_: *ObjEnum = try allocateObject(self, ObjEnum, ObjEnum.init(self.allocator, ObjTypeDef.cast(self.readConstant().Obj).?));

                    self.push(Value{ .Obj = enum_.toObj() });
                },

                .OP_ENUM_CASE => try self.defineEnumCase(),

                .OP_GET_ENUM_CASE => {
                    var enum_: *ObjEnum = ObjEnum.cast(self.peek(0).Obj).?;

                    _ = self.pop();
                    
                    var enum_case: *ObjEnumInstance = try allocateObject(self, ObjEnumInstance, ObjEnumInstance{
                        .enum_ref = enum_,
                        .case = self.readByte(),
                    });

                    self.push(Value{ .Obj = enum_case.toObj() });
                },

                .OP_GET_ENUM_CASE_VALUE => {
                    var enum_case: *ObjEnumInstance = ObjEnumInstance.cast(self.peek(0).Obj).?;

                    _ = self.pop();
                    self.push(enum_case.enum_ref.cases.items[enum_case.case]);
                },

                .OP_OBJECT => {
                    var object: *ObjObject = try allocateObject(self, ObjObject, ObjObject.init(self.allocator, ObjString.cast(self.readConstant().Obj).?));

                    self.push(Value{ .Obj = object.toObj() });
                },

                .OP_METHOD => {
                    try self.defineMethod(self.readString());
                },

                .OP_PROPERTY => {
                    try self.definePropertyDefaultValue(self.readString());
                },
                
                .OP_GET_PROPERTY => {
                    var obj: *Obj = self.peek(0).Obj;

                    switch (obj.obj_type) {
                        .ObjectInstance => instance: {
                            var instance: *ObjObjectInstance = ObjObjectInstance.cast(obj).?;
                            var name: *ObjString = self.readString();

                            if (instance.fields.get(name.string)) |field| {
                                _ = self.pop(); // Pop instance
                                self.push(field);
                                break :instance;
                            }
                            
                            if (instance.object.methods.get(name.string)) |method| {
                                try self.bindMethod(method);
                            }
                        },
                        .Enum => {
                            unreachable;
                        },
                        .List => list: {
                            var list = ObjList.cast(obj).?;
                            var name: *ObjString = self.readString();

                            if (try list.member(self, name.string)) |member| {
                                // We don't pop the list, it'll be the first argument
                                self.push(Value{ .Obj = member.toObj() });
                                break :list;
                            } else {
                                try self.runtimeError("Property doesn't exists", null);
                            }
                        },
                        else => unreachable
                    }
                },

                .OP_SET_PROPERTY => {
                    var instance: *ObjObjectInstance = ObjObjectInstance.cast(self.peek(1).Obj).?;
                    var name: *ObjString = self.readString();

                    // Set new value
                    try instance.fields.put(name.string, self.peek(0));

                    // Get the new value from stack, pop the instance and push value again
                    var value: Value = self.pop();
                    _ = self.pop();
                    self.push(value);
                },

                // TODO: remove
                .OP_PRINT => {
                    var value_str: []const u8 = try valueToString(self.allocator, self.pop());
                    defer self.allocator.free(value_str);

                    std.debug.print("{s}\n", .{ value_str });
                },

                .OP_NOT => {
                    self.push(Value{ .Boolean = !self.pop().Boolean });
                },

                .OP_GREATER => {
                    const left: f64 = self.pop().Number;
                    const right: f64 = self.pop().Number;

                    self.push(Value{ .Boolean = right > left });
                },


                .OP_LESS => {
                    const left: f64 = self.pop().Number;
                    const right: f64 = self.pop().Number;

                    self.push(Value{ .Boolean = right < left });
                },

                .OP_ADD,
                .OP_SUBTRACT,
                .OP_MULTIPLY,
                .OP_DIVIDE,
                .OP_MOD => try self.binary(instruction),

                .OP_EQUAL => self.push(Value{ .Boolean = valueEql(self.pop(), self.pop()) }),

                .OP_JUMP => {
                    const jump = self.readShort();

                    self.current_frame.?.ip += jump;
                },

                .OP_JUMP_IF_FALSE => {
                    const jump: u16 = self.readShort();

                    if (!self.peek(0).Boolean) {
                        self.current_frame.?.ip += jump;
                    }
                },

                .OP_LOOP => {
                    const jump = self.readShort();

                    self.current_frame.?.ip -= jump;
                },

                else => {
                    std.debug.warn("{} not yet implemented\n", .{ instruction });

                    std.os.exit(1);
                }
            }

            // if (builtin.mode == .Debug) {
            //     std.debug.warn("frame: {s}, code: {}\n", .{self.current_frame.?.closure.function.name.string, instruction});
            //     try dumpStack(self);
            // }
        }

        return InterpretResult.Ok;
    }

    // result_count > 0 when the return is `export`
    fn returnFrame(self: *Self) !bool {
        var result = self.pop();

        self.closeUpValues(&self.current_frame.?.slots[0]);

        self.frame_count -= 1;
        _ = self.frames.pop();
        if (self.frame_count == 0) {
            _ = self.pop();
            return true;
        }

        self.stack_top = self.current_frame.?.slots;

        self.push(result);

        self.current_frame.? = &self.frames.items[self.frame_count - 1];

        return false;
    }

    fn import(self: *Self, value: Value) anyerror!void {
        var function: *ObjFunction = ObjFunction.cast(value.Obj).?;

        var vm = try VM.init(self.allocator, self.strings);
        defer vm.deinit();

        if (((vm.interpret(function) catch null) orelse .RuntimeError) == .Ok) {
            // Top of stack is how many export we got
            var exported_count: u8 = @floatToInt(u8, vm.peek(0).Number);

            // Copy them to this vm globals
            if (exported_count > 0) {
                var i: u8 = exported_count;
                while (i > 0) : (i -= 1) {
                    try self.globals.append(vm.peek(i));
                }
            }
        } else {
            try self.runtimeError("Error while executing import", null);
        }
    }

    fn runtimeError(self: *Self, message: []const u8, call_stack: ?std.ArrayList(CallFrame)) anyerror!void {
        var stack = call_stack orelse std.ArrayList(CallFrame).init(self.allocator);

        var frame: *CallFrame = self.current_frame.?;
        try stack.append(frame.*);

        // Pop frame
        self.closeUpValues(&self.current_frame.?.slots[0]);
        self.frame_count -= 1;
        _ = self.frames.pop();
        if (self.frame_count == 0) {
            // No more frames, the error is uncaught.
            _ = self.pop();
            
            // Raise the runtime error
            std.debug.warn("\n\u{001b}[31mError: {s}\u{001b}[0m\n", .{ message });

            for (stack.items) |stack_frame| {
                std.debug.warn("\tat {s}", .{ stack_frame.closure.function.name.string });
                if (stack_frame.call_site) |call_site| {
                    std.debug.warn(":{}\n", .{ call_site });
                } else {
                    std.debug.warn("\n", .{});
                }
            }

            std.os.exit(1);
        }

        self.stack_top = self.current_frame.?.slots;

        // We don't care about a return value but call assumes it when setting frame.slots
        self.push(Value { .Null = null });

        self.current_frame.? = &self.frames.items[self.frame_count - 1];

        // Call catch closure or continue unwinding frames to find one
        if (frame.closure.catch_closure) |catch_closure| {
            stack.deinit();
            // TODO: Push ObjError as first argument
            if (!try self.call(catch_closure, 0)) {
                try self.runtimeError("Error while executing catch clause.", null);
            }

            self.current_frame.? = &self.frames.items[self.frame_count - 1];
        } else {
            return try self.runtimeError(message, stack);
        }
    }

    fn binary(self: *Self, code: OpCode) !void {
        const left: Value = self.pop();
        const right: Value = self.pop();

        const right_f: ?f64 = if (right == .Number) right.Number else null;
        const left_f: ?f64 = if (left == .Number) left.Number else null;

        const right_s: ?*ObjString = if (right == .Obj) ObjString.cast(right.Obj).? else null;
        const left_s: ?*ObjString = if (left == .Obj) ObjString.cast(left.Obj).? else null;

        switch (code) {
            .OP_ADD => add: {
                if (right_s != null) {
                    self.push(Value{
                        .Obj = (try right_s.?.concat(self, left_s.?)).toObj()
                    });
                    break :add;
                }

                self.push(Value{
                    .Number = right_f.? + left_f.?
                });
            },

            .OP_SUBTRACT => {
                self.push(Value{
                    .Number = right_f.? - left_f.?
                });
            },

            .OP_MULTIPLY => {
                self.push(Value{
                    .Number = right_f.? * left_f.?
                });
            },

            .OP_DIVIDE => {
                self.push(Value{
                    .Number = right_f.? / left_f.?
                });
            },

            .OP_MOD => {
                self.push(Value{
                    .Number = @mod(right_f.?, left_f.?)
                });
            },

            else => unreachable
        }
    }

    fn call(self: *Self, closure: *ObjClosure, arg_count: u8) !bool {
        // We don't type check or check arity because it was done at comptime
        
        // TODO: do we check for stack overflow

        var frame = CallFrame {
            .closure = closure,
            .ip = 0,
            // -1 is because we reserve slot 0 for this
            .slots = self.stack_top - arg_count - 1,
            .call_site = if (self.current_frame) |current_frame| current_frame.closure.function.chunk.lines.items[self.current_frame.?.ip] else null
        };

        if (self.frames.items.len <= self.frame_count) {
            try self.frames.append(frame);
        } else {
            self.frames.items[self.frame_count] = frame;
        }

        self.frame_count += 1;

        if (builtin.mode == .Debug) {
            try disassembleChunk(
                &frame.closure.function.chunk,
                frame.closure.function.name.string
            );
            std.debug.print("\n\n", .{});
        }

        return true;
    }

    fn callNative(self: *Self, native: *ObjNative, arg_count: u8) !bool {
        var result: Value = try native.native(self);

        self.stack_top = self.stack_top - arg_count - 1;
        self.push(result);

        return true;
    }

    fn bindMethod(self: *Self, method: *ObjClosure) !void {
        var bound: *ObjBoundMethod = try allocateObject(self, ObjBoundMethod, .{
            .receiver = self.peek(0),
            .closure = method,
        });

        _ = self.pop(); // Pop instane
        self.push(Value{ .Obj = bound.toObj() });
    }

    fn callValue(self: *Self, callee: Value, arg_count: u8) !bool {
        var obj: *Obj = callee.Obj;
        switch (obj.obj_type) {
            .Bound => {
                unreachable;
            },
            .Object => {
                var object: *ObjObject = ObjObject.cast(obj).?;
                
                return try self.instanciateObject(object, arg_count);
            },
            .Closure => {
                return try self.call(ObjClosure.cast(obj).?, arg_count);
            },
            .Native => {
                return try self.callNative(ObjNative.cast(obj).?, arg_count);
            },
            else => {}
        }

        return false;
    }

    fn instanciateObject(self: *Self, object: *ObjObject, arg_count: u8) !bool {
        var instance: *ObjObjectInstance = try allocateObject(self, ObjObjectInstance, ObjObjectInstance.init(self.allocator, object));

        // Put new instance as first local of the constructor
        // We do it right now so it doesn't get collected
        (self.stack_top - arg_count - 1)[0] = Value { .Obj = instance.toObj() };

        // Set instance fields with default values
        var it = object.fields.iterator();
        while (it.next()) |kv| {
            try instance.fields.put(kv.key_ptr.*, kv.value_ptr.*);
        }

        // TODO: init should always exists. Default one provided by compiler asks for all fields.
        var initializer: ?*ObjClosure = object.methods.get("init");
        if (initializer) |uinit| {
            return try self.call(uinit, arg_count);
        } else if (arg_count != 0) {
            try self.runtimeError("Expected 0 arguments.", null);
            return false;
        }

        return true;
    }

    fn invokeFromObject(self: *Self, object: *ObjObject, name: *ObjString, arg_count: u8) !bool {
        if (object.methods.get(name.string)) |method| {
            return self.call(method, arg_count);
        } else {
            try self.runtimeError("Undefined property.", null);
            return false;
        }
    }

    fn invoke(self: *Self, name: *ObjString, arg_count: u8) !bool {
        var receiver: Value = self.peek(arg_count);

        var obj: *Obj = receiver.Obj;
        switch (obj.obj_type) {
            .ObjectInstance => {
                var instance: *ObjObjectInstance = ObjObjectInstance.cast(obj).?;

                if (instance.fields.get(name.string)) |field| {
                    (self.stack_top - arg_count - 1)[0] = field;

                    return try self.callValue(field, arg_count);
                }

                return try self.invokeFromObject(instance.object, name, arg_count);
            },
            else => return false
        }

        return false;
    }

    fn closeUpValues(self: *Self, last: *Value) void {
        while (self.open_upvalues != null and @ptrToInt(self.open_upvalues.?.location) >= @ptrToInt(last)) {
            var upvalue: *ObjUpValue = self.open_upvalues.?;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed.?;
            self.open_upvalues = upvalue.next;
        }
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

        var created_upvalue: *ObjUpValue = try allocateObject(self, ObjUpValue, ObjUpValue.init(local));
        created_upvalue.next = upvalue;

        if (prev_upvalue) |uprev_upvalue| {
            uprev_upvalue.next = created_upvalue;
        } else {
            self.open_upvalues = created_upvalue;
        }

        return created_upvalue;
    }

    fn appendToList(self: *Self) !void {
        var list: *ObjList = ObjList.cast(self.peek(1).Obj).?;
        var list_value: Value = self.peek(0);

        try list.items.append(list_value);

        _ = self.pop();
    }

    fn defineEnumCase(self: *Self) !void {
        var enum_: *ObjEnum = ObjEnum.cast(self.peek(1).Obj).?;
        var enum_value: Value = self.peek(0);

        try enum_.cases.append(enum_value);

        _ = self.pop();
    }

    fn defineMethod(self: *Self, name: *ObjString) !void {
        var method: Value = self.peek(0);
        var object: *ObjObject = ObjObject.cast(self.peek(1).Obj).?;

        try object.methods.put(name.string, ObjClosure.cast(method.Obj).?);

        _ = self.pop();
    }

    fn definePropertyDefaultValue(self: *Self, name: *ObjString) !void {
        var property: Value = self.peek(0);
        var object: *ObjObject = ObjObject.cast(self.peek(1).Obj).?;

        try object.fields.put(name.string, property);

        _ = self.pop();
    }

    fn subscript(self: *Self) !void {
        var list_or_map: *Obj = self.peek(1).Obj;
        var index: Value = self.peek(0);

        if (list_or_map.obj_type == .List) {
            var list: *ObjList = ObjList.cast(list_or_map).?;

            if (index.Number < 0) {
                try self.runtimeError("Out of bound list access.", null);
            }

            const list_index: usize = @floatToInt(usize, index.Number);
            
            if (list_index < list.items.items.len) {
                var list_item: Value = list.items.items[list_index];
                
                // Pop list and index
                _ = self.pop();
                _ = self.pop();

                // Push value
                self.push(list_item);
            } else {
                try self.runtimeError("Out of bound list access.", null);
            }
        } else {
            var map: *ObjMap = ObjMap.cast(list_or_map).?;

            // Pop map and key
            _ = self.pop();
            _ = self.pop();

            if (map.map.get(valueToHashable(index))) |value| {
                // Push value
                self.push(value);
            } else {
                self.push(Value{ .Null = null });
            }
        }
    }

    fn setSubscript(self: *Self) !void {
        var list_or_map: *Obj = self.peek(2).Obj;
        var index: Value = self.peek(1);
        var value: Value = self.peek(0);

        if (list_or_map.obj_type == .List) {
            var list: *ObjList = ObjList.cast(list_or_map).?;

            if (index.Number < 0) {
                try self.runtimeError("Out of bound list access.", null);
            }

            const list_index: usize = @floatToInt(usize, index.Number);
            
            if (list_index < list.items.items.len) {
                list.items.items[list_index] = value;

                // Pop everyting
                _ = self.pop();
                _ = self.pop();
                _ = self.pop();

                // Push the value
                self.push(value);
            } else {
                try self.runtimeError("Out of bound list access.", null);
            }
        } else {
            var map: *ObjMap = ObjMap.cast(list_or_map).?;

            try map.map.put(valueToHashable(index), value);

            // Pop everyting
            _ = self.pop();
            _ = self.pop();
            _ = self.pop();

            // Push the value
            self.push(value);
        }
    }
};