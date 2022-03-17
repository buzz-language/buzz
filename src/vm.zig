// zig fmt: off
const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const _value = @import("./value.zig");
const _chunk = @import("./chunk.zig");
const _disassembler = @import("./disassembler.zig");
const _obj = @import("./obj.zig");
const Allocator = std.mem.Allocator;
const Config = @import("./config.zig").Config;

const Value = _value.Value;
const HashableValue = _value.HashableValue;
const ValueType = _value.ValueType;
const valueToHashable = _value.valueToHashable;
const hashableToValue = _value.hashableToValue;
const valueToString = _value.valueToString;
const valueEql = _value.valueEql;
const valueIs = _value.valueIs;
const ObjType = _obj.ObjType;
const Obj = _obj.Obj;
const ObjNative = _obj.ObjNative;
const ObjError = _obj.ObjError;
const ObjString = _obj.ObjString;
const ObjUpValue = _obj.ObjUpValue;
const ObjClosure = _obj.ObjClosure;
const ObjFunction = _obj.ObjFunction;
const ObjObjectInstance = _obj.ObjObjectInstance;
const ObjObject = _obj.ObjObject;
const ObjectDef = _obj.ObjectDef;
const ObjList = _obj.ObjList;
const ObjMap = _obj.ObjMap;
const ObjEnum = _obj.ObjEnum;
const ObjEnumInstance = _obj.ObjEnumInstance;
const ObjBoundMethod = _obj.ObjBoundMethod;
const ObjTypeDef = _obj.ObjTypeDef;
const allocateObject = _obj.allocateObject;
const allocateString = _obj.allocateString;
const OpCode = _chunk.OpCode;
const Chunk = _chunk.Chunk;
const disassembleChunk = _disassembler.disassembleChunk;
const dumpStack = _disassembler.dumpStack;

pub const CallFrame = struct {
    const Self = @This();

    closure: *ObjClosure,
    // Index into closure's chunk
    ip: usize,
    // Frame
    slots: [*]Value,

    // Default value in case of error
    error_value: ?Value = null,

    // Error handlers
    error_handlers: std.ArrayList(*ObjClosure),

    // The ip at which the current try block started
    try_start_ip: ?usize = null,

    // Catch jump set when entering a try block, and reset when exiting it or where actually jumping to that ip
    // Relative to try_start_ip
    error_catch_jmp: ?usize = null,

    // Line in source code where the call occured
    call_site: ?usize,
};

pub const VM = struct {
    const Self = @This();

    pub const Error = error {
        UnwrappedNull,
        OutOfBound,
        NumberOverflow,
        Custom, // TODO: remove when user can use this set directly in buzz code
    } || Allocator.Error || std.fmt.BufPrintError;

    allocator: Allocator,

    frames: std.ArrayList(CallFrame),
    frame_count: u64 = 0,

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

    pub fn init(allocator: Allocator, strings: *std.StringHashMap(*ObjString)) !Self {
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
        // TODO: we can't free this because exported closure refer to it
        // self.globals.deinit();
    }

    pub fn pushArgs(self: *Self, args: ?[][:0]u8) !void {
        // TODO: 3 steps to do this is horrible -> helper functions please
        var list_def: ObjList.ListDef = ObjList.ListDef.init(
            self.allocator,
            try allocateObject(
                self,
                ObjTypeDef,
                ObjTypeDef{
                    .def_type = .String
                }
            )
        );

        var list_def_union: ObjTypeDef.TypeUnion = .{
            .List = list_def,
        };

        var list_def_type: *ObjTypeDef = try allocateObject(self, ObjTypeDef, ObjTypeDef{
            .def_type = .List,
            .optional = false,
            .resolved_type = list_def_union,
        });

        var list: *ObjList = try allocateObject(
            self,
            ObjList,
            ObjList.init(
                self.allocator,
                // TODO: get instance that already exists
                list_def_type
            )
        );

        // Args is the first local like `this` which replace the closure itself in the stack
        (self.stack_top - 1)[0] = list.toValue();

        if (args) |uargs| {
            for (uargs) |arg| {
                try list.items.append(
                    Value{
                        .Obj = (try _obj.copyString(self, std.mem.sliceTo(arg, 0))).toObj()
                    }
                );
            }
        }
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

    pub fn copy(self: *Self, n: u24) void {
        if (n == 0) {
            self.push(self.peek(0));
            return;
        }

        var i = n - 1;
        while (i >= 0) : (i -= 1) {
            self.push(self.peek(i));

            if (i == 0) {
                break;
            }
        }
    }

    fn swap(self: *Self, from: u8, to: u8) void {
        var temp: Value = (self.stack_top - to - 1)[0];
        (self.stack_top - to - 1)[0] = (self.stack_top - from - 1)[0];
        (self.stack_top - from - 1)[0] = temp;
    }

    pub inline fn currentFrame(self: *Self) ?*CallFrame {
        if (self.frame_count == 0) {
            return null;
        }

        return &self.frames.items[self.frame_count - 1];
    }

    pub inline fn currentGlobals(self: *Self) *std.ArrayList(Value) {
        return self.currentFrame().?.closure.globals;
    }

    pub fn interpret(self: *Self, function: *ObjFunction, args: ?[][:0]u8) Error!void {        
        self.push(.{
            .Obj = function.toObj()
        });

        var closure: *ObjClosure = try allocateObject(
            self,
            ObjClosure,
            try ObjClosure.init(self.allocator, self, function)
        );

        _ = self.pop();

        self.push(.{
            .Obj = closure.toObj()
        });

        // Command line arguments are the first local
        try self.pushArgs(args);

        _ = try self.call(closure, 0, null);

        return try self.run();
    }

    fn readInstruction(self: *Self) u32 {
        const current_frame: *CallFrame = self.currentFrame().?;
        var instruction: u32 = current_frame.closure.function.chunk.code.items[current_frame.ip];

        current_frame.ip += 1;

        return instruction;
    }

    inline fn getCode(instruction: u32) OpCode {
        return @intToEnum(OpCode, @intCast(u8, instruction >> 24));
    }

    inline fn getArg(instruction: u32) u24 {
        return @intCast(u24, 0x00ffffff & instruction);
    }

    inline fn readByte(self: *Self) u8 {
        return @intCast(u8, self.readInstruction());
    }

    inline fn readOpCode(self: *Self) OpCode {
        // TODO: measure if [*]OpCode[0] is faster
        var opcode: OpCode = @intToEnum(OpCode, self.currentFrame().?.closure.function.chunk.code.items[self.currentFrame().?.ip]);

        self.currentFrame().?.ip += 1;

        return opcode;
    }

    inline fn readConstant(self: *Self, arg: u24) Value {
        return self.currentFrame().?.closure.function.chunk.constants.items[arg];
    }

    inline fn readString(self: *Self, arg: u24) *ObjString {
        return ObjString.cast(self.readConstant(arg).Obj).?;
    }

    fn run(self: *Self) Error!void {
        while (true) {
            const current_frame: *CallFrame = self.currentFrame().?;
            var full_instruction: u32 = self.readInstruction();
            var instruction: OpCode = getCode(full_instruction);
            var arg: u24 = getArg(full_instruction);
            if (Config.debug_current_instruction) {
                std.debug.print("{}: {}\n", .{current_frame.ip, instruction});
            }
            switch(instruction) {
                .OP_NULL => self.push(Value { .Null = null }),
                .OP_VOID => self.push(Value { .Void = null }),
                .OP_TRUE => self.push(Value { .Boolean = true }),
                .OP_FALSE => self.push(Value { .Boolean = false }),
                .OP_POP => _ = self.pop(),
                .OP_COPY => self.copy(arg),
                .OP_SWAP => self.swap(@intCast(u8, arg), self.readByte()),
                .OP_DEFINE_GLOBAL => {
                    try self.globals.ensureTotalCapacity(arg + 1);
                    self.globals.expandToCapacity();
                    self.globals.items[arg] = self.peek(0);
                    _ = self.pop();
                },
                .OP_GET_GLOBAL => self.push(self.currentGlobals().items[arg]),
                .OP_SET_GLOBAL => self.currentGlobals().items[arg] = self.peek(0),
                .OP_GET_LOCAL => self.push(current_frame.slots[arg]),
                .OP_SET_LOCAL => current_frame.slots[arg] = self.peek(0),
                .OP_GET_UPVALUE => self.push(current_frame.closure.upvalues.items[arg].location.*),
                .OP_SET_UPVALUE => current_frame.closure.upvalues.items[arg].location.* = self.peek(0),
                .OP_CONSTANT => self.push(self.readConstant(arg)),
                .OP_TO_STRING => self.push(
                        Value{
                            .Obj = (try _obj.copyString(
                                self,
                                try valueToString(self.allocator, self.pop())
                            )).toObj()
                        }
                    ),
                .OP_NEGATE => self.push(Value{ .Number = -self.pop().Number }),
                .OP_CLOSURE => {
                    var function: *ObjFunction = ObjFunction.cast(self.readConstant(arg).Obj).?;
                    var closure: *ObjClosure = try allocateObject(self, ObjClosure, try ObjClosure.init(self.allocator, self, function));

                    self.push(Value{ .Obj = closure.toObj() });

                    var i: usize = 0;
                    while (i < function.upvalue_count) : (i += 1) {
                        var is_local: bool = self.readByte() == 1;
                        var index: u8 = self.readByte();

                        if (is_local) {
                            try closure.upvalues.append(try self.captureUpvalue(&(current_frame.slots[index])));
                        } else {
                            try closure.upvalues.append(current_frame.closure.upvalues.items[index]);
                        }
                    }
                },
                .OP_CLOSE_UPVALUE => {
                    self.closeUpValues(@ptrCast(*Value, self.stack_top - 1));
                    _ = self.pop();
                },
                .OP_CALL => {
                    const arg_count: u8 = @intCast(u8, (0x00ffffff & full_instruction) >> 16);
                    const catch_count: u16 = @intCast(u16, 0x0000ffff & full_instruction);

                    var catch_values = std.ArrayList(Value).init(self.allocator);
                    defer catch_values.deinit();
                    var i: u16 = 0;
                    while (i < catch_count) : (i += 1) {
                        try catch_values.append(self.pop());
                    }

                    try self.callValue(self.peek(arg_count), arg_count, catch_values);
                },

                .OP_INVOKE => {
                    const method: *ObjString = self.readString(arg);
                    const arg_instruction: u32 = self.readInstruction();
                    const arg_count: u8 = @intCast(u8, arg_instruction >> 24);
                    const catch_count: u24 = @intCast(u8, 0x00ffffff & arg_instruction);

                    var catch_values = std.ArrayList(Value).init(self.allocator);
                    defer catch_values.deinit();
                    var i: u16 = 0;
                    while (i < catch_count) : (i += 1) {
                        try catch_values.append(self.pop());
                    }

                    try self.invoke(method, arg_count, catch_values);
                },

                .OP_SUPER_INVOKE => {
                    const method: *ObjString = self.readString(arg);
                    
                    const arg_instruction: u32 = self.readInstruction();
                    const arg_count: u8 = @intCast(u8, arg_instruction >> 24);
                    const catch_count: u24 = @intCast(u8, 0x00ffffff & arg_instruction);

                    var catch_values = std.ArrayList(Value).init(self.allocator);
                    defer catch_values.deinit();
                    var i: u16 = 0;
                    while (i < catch_count) : (i += 1) {
                        try catch_values.append(self.pop());
                    }

                    const super_class: *ObjObject = ObjObject.cast(self.pop().Obj).?;
                    try self.invokeFromObject(super_class, method, arg_count, catch_values);
                },

                .OP_RETURN => {
                    if (self.returnFrame()) {
                        return;
                    }
                },

                .OP_EXPORT => {
                    self.push(Value{ .Number = @intToFloat(f64, arg) });
                    return;
                },

                .OP_IMPORT => try self.import(self.peek(0)),

                .OP_THROW => try self.throw(Error.Custom, self.pop()),

                .OP_TRY => {
                    const frame: ?*CallFrame = self.currentFrame();

                    frame.?.try_start_ip = frame.?.ip;
                    frame.?.error_catch_jmp = arg;
                },

                .OP_TRY_END => {
                    const frame: ?*CallFrame = self.currentFrame();

                    frame.?.try_start_ip = null;
                    frame.?.error_catch_jmp = null;
                },

                .OP_LIST => {
                    var list: *ObjList = try allocateObject(
                        self,
                        ObjList,
                        ObjList.init(
                            self.allocator,
                            ObjTypeDef.cast(self.readConstant(arg).Obj).?
                        )
                    );

                    self.push(Value{ .Obj = list.toObj() });
                },

                .OP_LIST_APPEND => try self.appendToList(),

                .OP_MAP => {
                    var map: *ObjMap = try allocateObject(self, ObjMap, ObjMap.init(
                        self.allocator,
                        ObjTypeDef.cast(self.readConstant(arg).Obj).?,
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

                .OP_GET_SUBSCRIPT => try self.subscript(),
                
                .OP_SET_SUBSCRIPT => try self.setSubscript(),

                .OP_ENUM => {
                    var enum_: *ObjEnum = try allocateObject(self, ObjEnum, ObjEnum.init(self.allocator, ObjTypeDef.cast(self.readConstant(arg).Obj).?));

                    self.push(Value{ .Obj = enum_.toObj() });
                },

                .OP_ENUM_CASE => try self.defineEnumCase(),

                .OP_GET_ENUM_CASE => {
                    var enum_: *ObjEnum = ObjEnum.cast(self.peek(0).Obj).?;

                    _ = self.pop();
                    
                    var enum_case: *ObjEnumInstance = try allocateObject(self, ObjEnumInstance, ObjEnumInstance{
                        .enum_ref = enum_,
                        .case = @intCast(u8, arg),
                    });

                    self.push(Value{ .Obj = enum_case.toObj() });
                },

                .OP_GET_ENUM_CASE_VALUE => {
                    var enum_case: *ObjEnumInstance = ObjEnumInstance.cast(self.peek(0).Obj).?;

                    _ = self.pop();
                    self.push(enum_case.enum_ref.cases.items[enum_case.case]);
                },

                .OP_OBJECT => {
                    var object: *ObjObject = try allocateObject(
                        self,
                        ObjObject,
                        ObjObject.init(
                            self.allocator,
                            ObjString.cast(self.readConstant(arg).Obj).?,
                            ObjTypeDef.cast(self.readConstant(@intCast(u24, self.readInstruction())).Obj).?
                        )
                    );

                    self.push(Value{ .Obj = object.toObj() });
                },

                .OP_INHERIT => {
                    ObjObject.cast(self.pop().Obj).?.super = ObjObject.cast(self.currentGlobals().items[arg].Obj).?;
                },

                .OP_GET_SUPER => {
                    const name: *ObjString = self.readString(arg);
                    const super_class: *ObjObject = ObjObject.cast(self.pop().Obj).?;

                    try self.bindMethod(super_class.methods.get(name.string).?, null);
                },

                .OP_INSTANCE => try self.instanciateObject(ObjObject.cast(self.pop().Obj).?),

                .OP_METHOD => try self.defineMethod(self.readString(arg)),

                // Like OP_SET_PROPERTY but pops the value and leaves the instance on the stack
                .OP_PROPERTY => try self.setObjectFieldDefaultValue(self.readString(arg)),
                
                .OP_GET_PROPERTY => {
                    var obj: *Obj = self.peek(0).Obj;

                    switch (obj.obj_type) {
                        .Object => {
                            const object: *ObjObject = ObjObject.cast(obj).?;
                            const name: *ObjString = self.readString(arg);

                            _ = self.pop(); // Pop instance
                            self.push(object.static_fields.get(name.string).?);
                        },
                        .ObjectInstance => {
                            const instance: *ObjObjectInstance = ObjObjectInstance.cast(obj).?;
                            const name: *ObjString = self.readString(arg);

                            if (instance.fields.get(name.string)) |field| {
                                _ = self.pop(); // Pop instance
                                self.push(field);
                            } else if (instance.object.methods.get(name.string)) |method| {
                                try self.bindMethod(method, null);
                            } else if (instance.object.super) |super| {
                                try self.getSuperField(name.string, super);
                            } else {
                                unreachable;
                            }
                        },
                        .Enum => {
                            unreachable;
                        },
                        .List => {
                            const list = ObjList.cast(obj).?;
                            const name: *ObjString = self.readString(arg);

                            if (try list.member(self, name.string)) |member| {
                                try self.bindMethod(null, member);
                            } else {
                                unreachable;
                            }
                        },
                        else => unreachable
                    }
                },

                .OP_SET_PROPERTY => {
                    var obj: *Obj = self.peek(1).Obj;

                    switch (obj.obj_type) {
                        .ObjectInstance => {
                            const instance: *ObjObjectInstance = ObjObjectInstance.cast(obj).?;
                            const name: *ObjString = self.readString(arg);

                            // Set new value
                            try instance.fields.put(name.string, self.peek(0));

                            // Get the new value from stack, pop the instance and push value again
                            const value: Value = self.pop();
                            _ = self.pop();
                            self.push(value);
                        },
                        .Object => {
                            const object: *ObjObject = ObjObject.cast(obj).?;
                            const name: *ObjString = self.readString(arg);

                            // Set new value
                            try object.static_fields.put(name.string, self.peek(0));

                            // Get the new value from stack, pop the object and push value again
                            const value: Value = self.pop();
                            _ = self.pop();
                            self.push(value);
                        },
                        else => unreachable
                    }
                },

                // TODO: remove
                .OP_PRINT => {
                    var value_str: []const u8 = try valueToString(self.allocator, self.pop());
                    defer self.allocator.free(value_str);

                    std.debug.print("{s}\n", .{ value_str });
                },

                .OP_NOT => self.push(Value{ .Boolean = !self.pop().Boolean }),

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

                .OP_IS => self.push(Value{ .Boolean = valueIs(self.pop(), self.pop()) }),

                .OP_JUMP => current_frame.ip += arg,

                .OP_JUMP_IF_FALSE => {
                    if (!self.peek(0).Boolean) {
                        current_frame.ip += arg;
                    }
                },

                .OP_LOOP => current_frame.ip -= arg,

                .OP_FOREACH => try self.foreach(),

                .OP_UNWRAP => {
                    if (self.peek(0) == .Null) {
                        try self.throw(Error.UnwrappedNull, (try _obj.copyString(self, "Force unwrapped optional is null")).toValue());
                    }
                },

                .OP_NULL_OR => {
                    if (self.peek(1) == .Null) {
                        var else_: Value = self.peek(0);
                        // Pop operands
                        _ = self.pop();
                        _ = self.pop();
                        // Push left operand
                        self.push(else_);
                    } else {
                        _ = self.pop(); // Pop right operand
                    }
                },

                else => {
                    std.debug.print("{} not yet implemented\n", .{ instruction });

                    std.os.exit(1);
                }
            }

            if (Config.debug_stack) {
                std.debug.print("frame: {s}, code: {}\n", .{current_frame.closure.function.name.string, instruction});
                try dumpStack(self);
            }
        }

        return true;
    }

    fn foreach(self: *Self) !void {
        var iterable_value: Value = self.peek(0);
        var iterable: *Obj = iterable_value.Obj;
        switch (iterable.obj_type) {
            .List => {
                var key_slot: *Value = @ptrCast(*Value, self.stack_top - 3);
                var value_slot: *Value = @ptrCast(*Value, self.stack_top - 2);
                var list: *ObjList = ObjList.cast(iterable).?;

                // Get next index
                key_slot.* = if (try list.rawNext(self, if (key_slot.* == .Null) null else key_slot.Number)) |new_index|
                    Value{ .Number = new_index }
                    else Value{ .Null = null };
                
                // Set new value
                if (key_slot.* != .Null) {
                    value_slot.* = list.items.items[@floatToInt(usize, key_slot.Number)];
                }
            },
            .Enum => {
                var value_slot: *Value = @ptrCast(*Value, self.stack_top - 2);
                var enum_case: ?*ObjEnumInstance = if (value_slot.* == .Null) null else ObjEnumInstance.cast(value_slot.Obj).?;
                var enum_: *ObjEnum = ObjEnum.cast(iterable).?;

                // Get next enum case
                var next_case: ?*ObjEnumInstance = try enum_.rawNext(self, enum_case);
                value_slot.* = (if (next_case) |new_case| Value{ .Obj = new_case.toObj() }
                    else Value{ .Null = null });
            },
            .Map => {
                var key_slot: *Value = @ptrCast(*Value, self.stack_top - 3);
                var value_slot: *Value = @ptrCast(*Value, self.stack_top - 2);
                var map: *ObjMap = ObjMap.cast(iterable).?;
                var current_key: ?HashableValue = if (key_slot.* != .Null) valueToHashable(key_slot.*) else null;

                var next_key: ?HashableValue = map.rawNext(current_key);
                key_slot.* = if (next_key) |unext_key| hashableToValue(unext_key) else Value{ .Null = null };

                if (next_key) |unext_key| {
                    value_slot.* = map.map.get(unext_key) orelse Value{ .Null = null };
                }
            },
            else => unreachable,
        }
    }

    // result_count > 0 when the return is `export`
    fn returnFrame(self: *Self) bool {
        var result = self.pop();

        const frame: *CallFrame = self.currentFrame().?;

        self.closeUpValues(&frame.slots[0]);

        self.frame_count -= 1;
        _ = self.frames.pop();
        if (self.frame_count == 0) {
            _ = self.pop();
            return true;
        }

        self.stack_top = frame.slots;

        self.push(result);

        return false;
    }

    fn import(self: *Self, value: Value) Error!void {
        var function: *ObjFunction = ObjFunction.cast(value.Obj).?;

        var vm = try self.allocator.create(VM);
        vm.* = try VM.init(self.allocator, self.strings);
        // TODO: we can't free this because exported closure refer to it
        // defer vm.deinit();

        try vm.interpret(function, null);

        // Top of stack is how many export we got
        var exported_count: u8 = @floatToInt(u8, vm.peek(0).Number);

        // Copy them to this vm globals
        if (exported_count > 0) {
            var i: u8 = exported_count;
            while (i > 0) : (i -= 1) {
                try self.globals.append(vm.peek(i));
            }
        }

        _ = self.pop();
    }

    // Jump to first try clause if we're in a try block
    fn jumpToCatch(self: *Self, payload: Value) bool {
        const current_frame: *CallFrame = self.currentFrame().?;
        if (current_frame.error_catch_jmp) |error_catch_jmp| {
            assert(current_frame.try_start_ip != null);

            // Push error payload
            self.push(payload);

            // Jump to it
            current_frame.ip = current_frame.try_start_ip.? + error_catch_jmp;

            return true;
        }

        return false;
    }
    
    pub fn throw(self: *Self, code: Error, payload: Value) Error!void {
        // Are we in a try block ?
        if (self.jumpToCatch(payload)) {
            return;
        }

        var stack = std.ArrayList(CallFrame).init(self.allocator);

        while (self.frame_count > 0) {
            var frame: *CallFrame = self.currentFrame().?;
            try stack.append(frame.*);

            // Pop frame
            self.closeUpValues(&frame.slots[0]);
            self.frame_count -= 1;
            _ = self.frames.pop();
            if (self.frame_count == 0) {
                // No more frames, the error is uncaught.
                _ = self.pop();
                
                // Raise the runtime error
                std.debug.print("\n\u{001b}[31mError: {s}\u{001b}[0m\n", .{ try valueToString(self.allocator, payload) });

                for (stack.items) |stack_frame| {
                    std.debug.print("\tat {s}", .{ stack_frame.closure.function.name.string });
                    if (stack_frame.call_site) |call_site| {
                        std.debug.print(":{}\n", .{ call_site });
                    } else {
                        std.debug.print("\n", .{});
                    }
                }

                return code;
            }

            self.stack_top = frame.slots;

            if (frame.error_value) |error_value| {
                // Push error_value as failed function return value
                self.push(error_value);

                return;
            } else {
                if (self.jumpToCatch(payload)) {
                    return;
                } else {
                    // Are we in a try function?
                    // TODO: we can accept inline catch to be functions but not the try block
                    // Call catch closure or continue unwinding frames to find one
                    for (frame.error_handlers.items) |handler| {
                        const parameters: std.StringArrayHashMap(*ObjTypeDef) = handler.function.type_def.resolved_type.?.Function.parameters;
                        if (parameters.count() == 0 or _value.valueTypeEql(payload, parameters.get(parameters.keys()[0]).?)) {
                            stack.deinit();

                            // In a normal frame, the slots 0 is either the function or a `this` value
                            self.push(Value { .Null = null });

                            // Push error payload
                            self.push(payload);

                            // Call handler, it's return value is the result of the frame we just closed
                            try self.call(handler, 1, null);

                            return;
                        }
                    }
                }
            }
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

    fn call(self: *Self, closure: *ObjClosure, arg_count: u8, catch_values: ?std.ArrayList(Value)) !void {
        // We don't type check or check arity because it was done at comptime
        
        // TODO: do we check for stack overflow

        var frame = CallFrame {
            .closure = closure,
            .ip = 0,
            // -1 is because we reserve slot 0 for this
            .slots = self.stack_top - arg_count - 1,
            .call_site = if (self.currentFrame()) |current_frame|
                current_frame.closure.function.chunk.lines.items[current_frame.ip - 1]
                else null,
            .error_handlers = std.ArrayList(*ObjClosure).init(self.allocator),
        };

        if (catch_values != null) {
            for (catch_values.?.items) |catch_value| {
                if (catch_value == .Obj
                    and ObjClosure.cast(catch_value.Obj) != null
                    and ObjClosure.cast(catch_value.Obj).?.function.type_def.resolved_type.?.Function.function_type == .Catch) {
                    try frame.error_handlers.append(ObjClosure.cast(catch_value.Obj).?);
                } else {
                    assert(catch_values.?.items.len == 1);

                    frame.error_value = catch_value;
                }
            }
        }

        if (self.frames.items.len <= self.frame_count) {
            try self.frames.append(frame);
        } else {
            self.frames.items[self.frame_count] = frame;
        }

        self.frame_count += 1;

        if (Config.debug) {
            try disassembleChunk(
                &frame.closure.function.chunk,
                frame.closure.function.name.string
            );
            std.debug.print("\n\n", .{});
        }
    }

    fn callNative(self: *Self, native: *ObjNative, arg_count: u8, _: ?std.ArrayList(Value)) !void {
        // TODO: how to use catch_values with a native call?
        var result: Value = Value { .Null = null };
        if (native.native(self)) {
            result = self.pop();
        }

        self.stack_top = self.stack_top - arg_count - 1;
        self.push(result);
    }

    fn bindMethod(self: *Self, method: ?*ObjClosure, native: ?*ObjNative) !void {
        var bound: *ObjBoundMethod = try allocateObject(self, ObjBoundMethod, .{
            .receiver = self.peek(0),
            .closure = method,
            .native = native,
        });

        _ = self.pop(); // Pop instane
        self.push(Value{ .Obj = bound.toObj() });
    }

    fn callValue(self: *Self, callee: Value, arg_count: u8, catch_values: ?std.ArrayList(Value)) !void {
        var obj: *Obj = callee.Obj;
        switch (obj.obj_type) {
            .Bound => {
                var bound: *ObjBoundMethod = ObjBoundMethod.cast(obj).?;
                (self.stack_top - arg_count - 1)[0] = bound.receiver;

                if (bound.closure) |closure| {
                    return try self.call(closure, arg_count, catch_values);
                } else {
                    assert(bound.native != null);
                    return try self.callNative(bound.native.?, arg_count, catch_values);
                }
            },
            .Closure => {
                return try self.call(ObjClosure.cast(obj).?, arg_count, catch_values);
            },
            .Native => {
                return try self.callNative(ObjNative.cast(obj).?, arg_count, catch_values);
            },
            else => unreachable
        }
    }

    fn instanciateObject(self: *Self, object: *ObjObject) !void {
        var instance: *ObjObjectInstance = try allocateObject(self, ObjObjectInstance, ObjObjectInstance.init(self.allocator, object));

        // Set instance fields with super classes default values
        if (object.super) |super| {
            try self.superDefaults(instance, super);
        }

        // Set instance fields with default values
        var it = object.fields.iterator();
        while (it.next()) |kv| {
            try instance.fields.put(kv.key_ptr.*, kv.value_ptr.*);
        }

        self.push(instance.toValue());
    }

    // TODO: superDefaults and getSuperField could be replaced by specialized opcodes to avoid having to walk up the chain of inheritance

    fn superDefaults(self: *Self, instance: *ObjObjectInstance, super: *ObjObject) Allocator.Error!void {
        if (super.super) |super_super| {
            try self.superDefaults(instance, super_super);
        }

        var it = super.fields.iterator();
        while (it.next()) |kv| {
            try instance.fields.put(kv.key_ptr.*, kv.value_ptr.*);
        }
    }

    fn getSuperField(self: *Self, name: []const u8, super: *ObjObject) Allocator.Error!void {
        if (super.static_fields.get(name)) |static| {
            _ = self.pop(); // Pop instance
            self.push(static);
        } if (super.methods.get(name)) |method| {
            try self.bindMethod(method, null);
        } else  if (super.super) |super_super| {
            try self.getSuperField(name, super_super);
        }
    }

    fn invokeFromObject(self: *Self, object: *ObjObject, name: *ObjString, arg_count: u8, catch_values: ?std.ArrayList(Value)) !void {
        if (object.methods.get(name.string)) |method| {
            return self.call(method, arg_count, catch_values);
        } else {
            unreachable;
        }
    }

    fn invoke(self: *Self, name: *ObjString, arg_count: u8, catch_values: ?std.ArrayList(Value)) !void {
        var receiver: Value = self.peek(arg_count);

        var obj: *Obj = receiver.Obj;
        switch (obj.obj_type) {
            .ObjectInstance => {
                var instance: *ObjObjectInstance = ObjObjectInstance.cast(obj).?;

                if (instance.fields.get(name.string)) |field| {
                    (self.stack_top - arg_count - 1)[0] = field;

                    return try self.callValue(field, arg_count, catch_values);
                }

                try self.invokeFromObject(instance.object, name, arg_count, catch_values);
            },
            .List => {
                var list: *ObjList = ObjList.cast(obj).?;

                if (try list.member(self, name.string)) |member| {
                    var member_value: Value = Value { .Obj = member.toObj() };
                    (self.stack_top - arg_count - 1)[0] = member_value;

                    return try self.callValue(member_value, arg_count, catch_values);
                }
                
                unreachable;
            },
            else => unreachable
        }
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

    fn setObjectFieldDefaultValue(self: *Self, name: *ObjString) !void {
        var property: Value = self.peek(0);
        var object: *ObjObject = ObjObject.cast(self.peek(1).Obj).?;

        if (object.type_def.resolved_type.?.Object.fields.contains(name.string)) {
            try object.fields.put(name.string, property);
        } else {
            assert(object.type_def.resolved_type.?.Object.static_fields.contains(name.string));
            try object.static_fields.put(name.string, property);
        }

        _ = self.pop();
    }

    fn subscript(self: *Self) !void {
        var list_or_map: *Obj = self.peek(1).Obj;
        var index: Value = self.peek(0);

        if (list_or_map.obj_type == .List) {
            var list: *ObjList = ObjList.cast(list_or_map).?;

            if (index.Number < 0) {
                try self.throw(Error.OutOfBound, (try _obj.copyString(self, "Out of bound list access.")).toValue());
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
                try self.throw(Error.OutOfBound, (try _obj.copyString(self, "Out of bound list access.")).toValue());
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
                try self.throw(Error.OutOfBound, (try _obj.copyString(self, "Out of bound list access.")).toValue());
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
                try self.throw(Error.OutOfBound, (try _obj.copyString(self, "Out of bound list access.")).toValue());
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