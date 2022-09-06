const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const _value = @import("./value.zig");
const _chunk = @import("./chunk.zig");
const _disassembler = @import("./disassembler.zig");
const _obj = @import("./obj.zig");
const Allocator = std.mem.Allocator;
const Config = @import("./config.zig").Config;
const _memory = @import("./memory.zig");
const GarbageCollector = _memory.GarbageCollector;
const TypeRegistry = _memory.TypeRegistry;

const Value = _value.Value;
const HashableValue = _value.HashableValue;
const ValueType = _value.ValueType;
const valueToHashable = _value.valueToHashable;
const hashableToValue = _value.hashableToValue;
const floatToInteger = _value.floatToInteger;
const valueToString = _value.valueToString;
const valueToStringAlloc = _value.valueToStringAlloc;
const valueEql = _value.valueEql;
const valueIs = _value.valueIs;
const ObjType = _obj.ObjType;
const Obj = _obj.Obj;
const ObjNative = _obj.ObjNative;
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
const ObjFiber = _obj.ObjFiber;
const ObjEnumInstance = _obj.ObjEnumInstance;
const ObjBoundMethod = _obj.ObjBoundMethod;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjPattern = _obj.ObjPattern;
const cloneObject = _obj.cloneObject;
const OpCode = _chunk.OpCode;
const Chunk = _chunk.Chunk;
const disassembleChunk = _disassembler.disassembleChunk;
const dumpStack = _disassembler.dumpStack;

pub const ImportRegistry = std.AutoHashMap(*ObjString, std.ArrayList(Value));

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

    // Line in source code where the call occured
    call_site: ?usize,

    // Offset at which error can be handled (means we're in a try block)
    try_ip: ?usize = null,
};

pub const Fiber = struct {
    const Self = @This();

    pub const Status = enum {
        // Just created, never started
        Instanciated,
        // Currently running
        Running,
        // Yielded an expected value
        Yielded,
        // Reached return statement
        Over,
    };

    allocator: Allocator,

    parent_fiber: ?*Fiber,

    call_type: OpCode,
    arg_count: u8,
    catch_count: u8,
    method: ?*ObjString,

    frames: std.ArrayList(CallFrame),
    frame_count: u64 = 0,

    stack: []Value,
    stack_top: [*]Value,
    open_upvalues: ?*ObjUpValue,

    status: Status = .Instanciated,
    // true: we did `resolve fiber`, false: we did `resume fiber`
    resolved: bool = false,

    pub fn init(
        allocator: Allocator,
        parent_fiber: ?*Fiber,
        stack_slice: ?[]Value,
        call_type: OpCode,
        arg_count: u8,
        catch_count: u8,
        method: ?*ObjString,
    ) !Self {
        var self: Self = .{
            .allocator = allocator,
            .parent_fiber = parent_fiber,
            .stack = try allocator.alloc(Value, 100000),
            .stack_top = undefined,
            .frames = std.ArrayList(CallFrame).init(allocator),
            .open_upvalues = null,
            .call_type = call_type,
            .arg_count = arg_count,
            .catch_count = catch_count,
            .method = method,
        };

        if (stack_slice != null) {
            std.mem.copy(Value, self.stack, stack_slice.?);

            self.stack_top = @ptrCast([*]Value, self.stack[stack_slice.?.len..]);
        } else {
            self.stack_top = @ptrCast([*]Value, self.stack[0..]);
        }

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.stack);

        self.frames.deinit();
    }

    pub fn start(self: *Self, vm: *VM) !void {
        assert(self.status == .Instanciated);

        vm.current_fiber = self;

        switch (self.call_type) {
            .OP_ROUTINE => { // | closure | ...args | ...catch |
                var catch_values = std.ArrayList(Value).init(self.allocator);
                defer catch_values.deinit();
                var i: u16 = 0;
                while (i < self.catch_count) : (i += 1) {
                    try catch_values.append(vm.pop());
                }

                try vm.callValue(vm.peek(self.arg_count), self.arg_count, catch_values);
            },
            .OP_INVOKE_ROUTINE => { // | receiver | ...args | ...catch |
                var catch_values = std.ArrayList(Value).init(self.allocator);
                defer catch_values.deinit();
                var i: u16 = 0;
                while (i < self.catch_count) : (i += 1) {
                    try catch_values.append(vm.pop());
                }

                try vm.invoke(self.method.?, self.arg_count, catch_values);
            },
            .OP_SUPER_INVOKE_ROUTINE => { // | receiver | super | ...args | ...catch |
                var catch_values = std.ArrayList(Value).init(self.allocator);
                defer catch_values.deinit();
                var i: u16 = 0;
                while (i < self.catch_count) : (i += 1) {
                    try catch_values.append(vm.pop());
                }

                const super_class: *ObjObject = ObjObject.cast(vm.pop().Obj).?;
                try vm.invokeFromObject(super_class, self.method.?, self.arg_count, catch_values);
            },
            else => unreachable,
        }

        self.status = .Running;
    }

    pub fn yield(self: *Self, vm: *VM) void {
        assert(self.status == .Running);

        // If resolved or not run in a fiber, dismiss yielded value and keep running
        if (self.resolved or self.parent_fiber == null) {
            return;
        }

        // Was resumed, so push the yielded value on the parent fiber and give control back to parent fiber
        const top = vm.peek(0);

        vm.current_fiber = self.parent_fiber.?;
        vm.push(top);

        self.status = .Yielded;

        // Do we need to finish OP_CODE that triggered the yield?
        const full_instruction = vm.readPreviousInstruction();
        if (full_instruction) |ufull_instruction| {
            const instruction = VM.getCode(ufull_instruction);
            switch (instruction) {
                .OP_FOREACH => {
                    _ = vm.pop();

                    var value_slot: *Value = @ptrCast(*Value, vm.current_fiber.stack_top - 2);

                    value_slot.* = top;
                },
                else => {},
            }
        }
    }

    pub fn resume_(self: *Self, vm: *VM) !void {
        switch (self.status) {
            .Instanciated => {
                // No yet started, do so
                try self.start(vm);
            },
            .Yielded => {
                // Give control to fiber
                self.parent_fiber = vm.current_fiber;
                vm.current_fiber = self;

                self.status = .Running;
            },
            .Over => {
                // User should check fiber.over() before doing `resume`
                try vm.throw(VM.Error.FiberOver, (try vm.gc.copyString("Fiber is over")).toValue());
            },
            .Running => unreachable,
        }
    }

    pub fn resolve_(self: *Self, vm: *VM) !void {
        self.resolved = true;

        switch (self.status) {
            .Instanciated => try self.start(vm),
            .Yielded => try self.resume_(vm),
            .Over => {
                // Already over, just take the top value
                const parent_fiber = vm.current_fiber;
                vm.current_fiber = self;

                const top = vm.peek(0);

                vm.current_fiber = parent_fiber;
                vm.push(top);

                // FIXME: but this means we can do several `resolve fiber`
            },
            .Running => unreachable,
        }
    }

    pub fn finish(self: *Self, vm: *VM, result: Value) !void {
        // Fiber is now over
        self.status = .Over;
        const resolved = self.resolved;

        // Put the result back stack so `resolve` can pick it up
        vm.push(result);

        // Go back to parent fiber
        vm.current_fiber = self.parent_fiber.?;

        if (resolved) {
            // We did `resolve fiber` we want the returned value
            vm.push(result);
        } else {
            // We did `resume fiber` and hit return
            // We don't yet care about that value;
            vm.push(Value{ .Null = {} });
        }

        // Do we need to finish OP_CODE that triggered the yield?
        const full_instruction = vm.readPreviousInstruction();
        if (full_instruction) |ufull_instruction| {
            const instruction = VM.getCode(ufull_instruction);
            switch (instruction) {
                .OP_FOREACH => {
                    // We don't care about the returned value
                    _ = vm.pop();

                    var value_slot: *Value = @ptrCast(*Value, vm.current_fiber.stack_top - 2);

                    value_slot.* = Value{ .Null = {} };
                },
                else => {},
            }
        }
    }
};

pub const VM = struct {
    const Self = @This();

    pub const Error = error{
        UnwrappedNull,
        OutOfBound,
        NumberOverflow,
        NotInFiber,
        FiberOver,
        BadNumber,
        Custom, // TODO: remove when user can use this set directly in buzz code
    } || Allocator.Error || std.fmt.BufPrintError;

    gc: *GarbageCollector,
    current_fiber: *Fiber,
    globals: std.ArrayList(Value),
    import_registry: *ImportRegistry,

    pub fn init(gc: *GarbageCollector, import_registry: *ImportRegistry) !Self {
        var self: Self = .{
            .gc = gc,
            .import_registry = import_registry,
            .globals = std.ArrayList(Value).init(gc.allocator),
            .current_fiber = try gc.allocator.create(Fiber),
        };

        return self;
    }

    pub fn deinit(_: *Self) void {
        // TODO: we can't free this because exported closure refer to it
        // self.globals.deinit();
    }

    pub fn cliArgs(self: *Self, args: ?[][:0]u8) !*ObjList {
        var list_def: ObjList.ListDef = ObjList.ListDef.init(
            self.gc.allocator,
            try self.gc.allocateObject(
                ObjTypeDef,
                ObjTypeDef{ .def_type = .String },
            ),
        );

        var list_def_union: ObjTypeDef.TypeUnion = .{
            .List = list_def,
        };

        var list_def_type: *ObjTypeDef = try self.gc.allocateObject(
            ObjTypeDef,
            ObjTypeDef{
                .def_type = .List,
                .optional = false,
                .resolved_type = list_def_union,
            },
        );

        var arg_list = try self.gc.allocateObject(
            ObjList,
            ObjList.init(
                self.gc.allocator,
                // TODO: get instance that already exists
                list_def_type,
            ),
        );

        // Prevent gc
        self.push(arg_list.toValue());

        if (args) |uargs| {
            for (uargs) |arg, index| {
                // We can't have more than 255 arguments to a function
                // TODO: should we silently ignore them or should we raise an error?
                if (index >= 255) {
                    break;
                }

                try arg_list.items.append(
                    Value{
                        .Obj = (try self.gc.copyString(std.mem.sliceTo(arg, 0))).toObj(),
                    },
                );
            }
        }

        _ = self.pop();

        return arg_list;
    }

    pub fn push(self: *Self, value: Value) void {
        self.current_fiber.stack_top[0] = value;
        self.current_fiber.stack_top += 1;
    }

    pub fn pop(self: *Self) Value {
        self.current_fiber.stack_top -= 1;
        return self.current_fiber.stack_top[0];
    }

    pub fn peek(self: *Self, distance: u32) Value {
        return (self.current_fiber.stack_top - 1 - distance)[0];
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

    fn cloneValue(self: *Self, value: Value) !Value {
        return switch (value) {
            .Boolean,
            .Integer,
            .Float,
            .Null,
            .Void,
            => value,
            .Obj => try cloneObject(value.Obj, self),
        };
    }

    fn clone(self: *Self) !void {
        self.push(try self.cloneValue(self.pop()));
    }

    fn swap(self: *Self, from: u8, to: u8) void {
        var temp: Value = (self.current_fiber.stack_top - to - 1)[0];
        (self.current_fiber.stack_top - to - 1)[0] = (self.current_fiber.stack_top - from - 1)[0];
        (self.current_fiber.stack_top - from - 1)[0] = temp;
    }

    pub inline fn currentFrame(self: *Self) ?*CallFrame {
        if (self.current_fiber.frame_count == 0) {
            return null;
        }

        return &self.current_fiber.frames.items[self.current_fiber.frame_count - 1];
    }

    pub inline fn currentGlobals(self: *Self) *std.ArrayList(Value) {
        return self.currentFrame().?.closure.globals;
    }

    pub fn interpret(self: *Self, function: *ObjFunction, args: ?[][:0]u8) Error!void {
        self.current_fiber.* = try Fiber.init(
            self.gc.allocator,
            null, // parent fiber
            null, // stack_slice
            .OP_CALL, // call_type
            1, // arg_count
            0, // catch_count
            null, // method/member
        );

        self.push((try self.gc.allocateObject(
            ObjClosure,
            try ObjClosure.init(self.gc.allocator, self, function),
        )).toValue());

        self.push((try self.cliArgs(args)).toValue());

        try self.gc.registerVM(self);
        defer self.gc.unregisterVM(self);

        try self.callValue(self.peek(1), 0, null);

        self.current_fiber.status = .Running;

        return try self.run();
    }

    fn readPreviousInstruction(self: *Self) ?u32 {
        const current_frame: *CallFrame = self.currentFrame().?;

        if (current_frame.ip > 0) {
            return current_frame.closure.function.chunk.code.items[current_frame.ip - 1];
        }

        return null;
    }

    fn readInstruction(self: *Self) u32 {
        const current_frame: *CallFrame = self.currentFrame().?;
        var instruction: u32 = current_frame.closure.function.chunk.code.items[current_frame.ip];

        current_frame.ip += 1;

        return instruction;
    }

    pub inline fn getCode(instruction: u32) OpCode {
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
        var opcode: OpCode = @intToEnum(
            OpCode,
            self.currentFrame().?.closure.function.chunk.code.items[self.currentFrame().?.ip],
        );

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
            const full_instruction: u32 = self.readInstruction();
            const instruction: OpCode = getCode(full_instruction);
            const arg: u24 = getArg(full_instruction);
            if (Config.debug_current_instruction) {
                std.debug.print(
                    "{}: {}\n",
                    .{
                        current_frame.ip,
                        instruction,
                    },
                );
            }
            switch (instruction) {
                .OP_NULL => self.push(Value{ .Null = {} }),
                .OP_VOID => self.push(Value{ .Void = {} }),
                .OP_TRUE => self.push(Value{ .Boolean = true }),
                .OP_FALSE => self.push(Value{ .Boolean = false }),
                .OP_POP => _ = self.pop(),
                .OP_COPY => self.copy(arg),
                .OP_CLONE => try self.clone(),
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
                .OP_TO_STRING => {
                    var str = try valueToStringAlloc(self.gc.allocator, self.pop());
                    defer self.gc.allocator.free(str);
                    self.push(
                        Value{
                            .Obj = (try self.gc.copyString(str)).toObj(),
                        },
                    );
                },
                .OP_NEGATE => {
                    const value = self.pop();

                    if (value == .Integer) {
                        self.push(Value{ .Integer = -value.Integer });
                    } else {
                        self.push(Value{ .Float = -value.Float });
                    }
                },
                .OP_CLOSURE => {
                    var function: *ObjFunction = ObjFunction.cast(self.readConstant(arg).Obj).?;
                    var closure: *ObjClosure = try self.gc.allocateObject(
                        ObjClosure,
                        try ObjClosure.init(self.gc.allocator, self, function),
                    );

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
                    self.closeUpValues(@ptrCast(*Value, self.current_fiber.stack_top - 1));
                    _ = self.pop();
                },

                .OP_ROUTINE => {
                    const arg_count: u8 = @intCast(u8, (0x00ffffff & full_instruction) >> 16);
                    const catch_count: u16 = @intCast(u16, 0x0000ffff & full_instruction);

                    const stack_ptr = self.current_fiber.stack_top - arg_count - catch_count - 1;
                    const stack_len = arg_count + catch_count + 1;
                    const stack_slice = stack_ptr[0..stack_len];

                    var fiber = try self.gc.allocator.create(Fiber);
                    fiber.* = try Fiber.init(
                        self.gc.allocator,
                        self.current_fiber,
                        stack_slice,
                        instruction,
                        arg_count,
                        @intCast(u8, catch_count),
                        null,
                    );

                    // Pop arguments and catch clauses
                    self.current_fiber.stack_top = self.current_fiber.stack_top - stack_len;

                    const type_def = ObjTypeDef.cast(self.pop().Obj).?;

                    // Put new fiber on the stack
                    var obj_fiber = try self.gc.allocateObject(ObjFiber, ObjFiber{
                        .fiber = fiber,
                        .type_def = type_def,
                    });

                    self.push(obj_fiber.toValue());
                },

                .OP_INVOKE_ROUTINE,
                .OP_SUPER_INVOKE_ROUTINE,
                => {
                    const method: *ObjString = self.readString(arg);
                    const arg_instruction: u32 = self.readInstruction();
                    const arg_count: u8 = @intCast(u8, arg_instruction >> 24);
                    const catch_count: u24 = @intCast(u8, 0x00ffffff & arg_instruction);

                    // - 2 because of super
                    const extra: usize = if (instruction == .OP_SUPER_INVOKE_ROUTINE) 1 else 0;
                    const stack_ptr = self.current_fiber.stack_top - arg_count - catch_count - 1 - extra;
                    const stack_len = arg_count + catch_count + 1 + extra;
                    const stack_slice = stack_ptr[0..stack_len];

                    var fiber = try self.gc.allocator.create(Fiber);
                    fiber.* = try Fiber.init(
                        self.gc.allocator,
                        self.current_fiber,
                        stack_slice,
                        instruction,
                        arg_count,
                        @intCast(u8, catch_count),
                        method,
                    );

                    // Pop arguments and catch clauses
                    self.current_fiber.stack_top = self.current_fiber.stack_top - stack_len;

                    const type_def = ObjTypeDef.cast(self.pop().Obj).?;

                    // Push new fiber on the stack
                    var obj_fiber = try self.gc.allocateObject(ObjFiber, ObjFiber{
                        .fiber = fiber,
                        .type_def = type_def,
                    });

                    self.push(obj_fiber.toValue());
                },

                .OP_RESUME => {
                    const obj_fiber = ObjFiber.cast(self.pop().Obj).?;

                    try obj_fiber.fiber.resume_(self);
                },

                .OP_RESOLVE => {
                    const obj_fiber = ObjFiber.cast(self.pop().Obj).?;

                    try obj_fiber.fiber.resolve_(self);
                },

                .OP_YIELD => {
                    self.current_fiber.yield(self);
                },

                .OP_CALL => {
                    const arg_count: u8 = @intCast(u8, (0x00ffffff & full_instruction) >> 16);
                    const catch_count: u16 = @intCast(u16, 0x0000ffff & full_instruction);

                    var catch_values = std.ArrayList(Value).init(self.gc.allocator);
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

                    var catch_values = std.ArrayList(Value).init(self.gc.allocator);
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

                    var catch_values = std.ArrayList(Value).init(self.gc.allocator);
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
                    self.push(Value{ .Integer = @intCast(i64, arg) });
                    return;
                },

                .OP_IMPORT => try self.import(self.peek(1), self.peek(0)),

                .OP_TRY => self.currentFrame().?.try_ip = @intCast(usize, arg),
                .OP_TRY_END => self.currentFrame().?.try_ip = null,

                .OP_THROW => try self.throw(Error.Custom, self.pop()),

                .OP_LIST => {
                    var list: *ObjList = try self.gc.allocateObject(
                        ObjList,
                        ObjList.init(self.gc.allocator, ObjTypeDef.cast(self.readConstant(arg).Obj).?),
                    );

                    self.push(Value{ .Obj = list.toObj() });
                },

                .OP_LIST_APPEND => try self.appendToList(),

                .OP_MAP => {
                    var map: *ObjMap = try self.gc.allocateObject(ObjMap, ObjMap.init(
                        self.gc.allocator,
                        ObjTypeDef.cast(self.readConstant(arg).Obj).?,
                    ));

                    self.push(Value{ .Obj = map.toObj() });
                },

                .OP_SET_MAP => {
                    var map: *ObjMap = ObjMap.cast(self.peek(2).Obj).?;
                    var key: Value = self.peek(1);
                    var value: Value = self.peek(0);

                    try map.set(self.gc, key, value);

                    _ = self.pop();
                    _ = self.pop();
                },

                .OP_GET_SUBSCRIPT => try self.subscript(),

                .OP_SET_SUBSCRIPT => try self.setSubscript(),

                .OP_ENUM => {
                    var enum_: *ObjEnum = try self.gc.allocateObject(
                        ObjEnum,
                        ObjEnum.init(self.gc.allocator, ObjTypeDef.cast(self.readConstant(arg).Obj).?),
                    );

                    self.push(Value{ .Obj = enum_.toObj() });
                },

                .OP_ENUM_CASE => try self.defineEnumCase(),

                .OP_GET_ENUM_CASE => {
                    var enum_: *ObjEnum = ObjEnum.cast(self.peek(0).Obj).?;

                    _ = self.pop();

                    var enum_case: *ObjEnumInstance = try self.gc.allocateObject(ObjEnumInstance, ObjEnumInstance{
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
                    var object: *ObjObject = try self.gc.allocateObject(
                        ObjObject,
                        ObjObject.init(
                            self.gc.allocator,
                            ObjString.cast(self.readConstant(arg).Obj).?,
                            ObjTypeDef.cast(self.readConstant(@intCast(u24, self.readInstruction())).Obj).?,
                        ),
                    );

                    self.push(Value{ .Obj = object.toObj() });
                },

                .OP_INHERIT => {
                    const obj = self.pop().Obj;
                    ObjObject.cast(obj).?.super = ObjObject.cast(self.currentGlobals().items[arg].Obj).?;
                    try self.gc.markObjDirty(obj);
                },

                .OP_GET_SUPER => {
                    const name: *ObjString = self.readString(arg);
                    const super_class: *ObjObject = ObjObject.cast(self.pop().Obj).?;

                    try self.bindMethod(super_class.methods.get(name).?, null);
                },

                .OP_INSTANCE => try self.instanciateObject(self.pop().Obj),

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
                            self.push(object.static_fields.get(name).?);
                        },
                        .ObjectInstance => {
                            const instance: *ObjObjectInstance = ObjObjectInstance.cast(obj).?;
                            const name: *ObjString = self.readString(arg);

                            if (instance.fields.get(name)) |field| {
                                _ = self.pop(); // Pop instance
                                self.push(field);
                            } else if (instance.object) |object| {
                                if (object.methods.get(name)) |method| {
                                    try self.bindMethod(method, null);
                                } else if (object.super) |super| {
                                    try self.getSuperField(name, super);
                                } else {
                                    unreachable;
                                }
                            }
                        },
                        .Enum => {
                            unreachable;
                        },
                        .List => {
                            const list = ObjList.cast(obj).?;
                            const name: *ObjString = self.readString(arg);

                            if (try list.member(self, name)) |member| {
                                try self.bindMethod(null, member);
                            } else {
                                unreachable;
                            }
                        },
                        .Map => {
                            const map = ObjMap.cast(obj).?;
                            const name: *ObjString = self.readString(arg);

                            if (try map.member(self, name)) |member| {
                                try self.bindMethod(null, member);
                            } else {
                                unreachable;
                            }
                        },
                        .String => {
                            const name: *ObjString = self.readString(arg);

                            if (try ObjString.member(self, name)) |member| {
                                try self.bindMethod(null, member);
                            } else {
                                unreachable;
                            }
                        },
                        .Pattern => {
                            const name: *ObjString = self.readString(arg);

                            if (try ObjPattern.member(self, name)) |member| {
                                try self.bindMethod(null, member);
                            } else {
                                unreachable;
                            }
                        },
                        .Fiber => {
                            const name: *ObjString = self.readString(arg);

                            if (try ObjFiber.member(self, name)) |member| {
                                try self.bindMethod(null, member);
                            }

                            unreachable;
                        },
                        else => unreachable,
                    }
                },

                .OP_SET_PROPERTY => {
                    var obj: *Obj = self.peek(1).Obj;

                    switch (obj.obj_type) {
                        .ObjectInstance => {
                            const instance: *ObjObjectInstance = ObjObjectInstance.cast(obj).?;
                            const name: *ObjString = self.readString(arg);

                            // Set new value
                            try instance.setField(self.gc, name, self.peek(0));

                            // Get the new value from stack, pop the instance and push value again
                            const value: Value = self.pop();
                            _ = self.pop();
                            self.push(value);
                        },
                        .Object => {
                            const object: *ObjObject = ObjObject.cast(obj).?;
                            const name: *ObjString = self.readString(arg);

                            // Set new value
                            try object.setStaticField(self.gc, name, self.peek(0));

                            // Get the new value from stack, pop the object and push value again
                            const value: Value = self.pop();
                            _ = self.pop();
                            self.push(value);
                        },
                        else => unreachable,
                    }
                },

                .OP_NOT => self.push(Value{ .Boolean = !self.pop().Boolean }),

                .OP_BNOT => {
                    const value = self.pop();

                    self.push(Value{ .Integer = ~(if (value == .Integer) value.Integer else @floatToInt(i64, value.Float)) });
                },

                .OP_GREATER => {
                    const right_value = floatToInteger(self.pop());
                    const left_value = floatToInteger(self.pop());

                    const left_f: ?f64 = if (left_value == .Float) left_value.Float else null;
                    const left_i: ?i64 = if (left_value == .Integer) left_value.Integer else null;
                    const right_f: ?f64 = if (right_value == .Float) right_value.Float else null;
                    const right_i: ?i64 = if (right_value == .Integer) right_value.Integer else null;

                    if (left_f) |lf| {
                        if (right_f) |rf| {
                            self.push(Value{ .Boolean = lf > rf });
                        } else {
                            self.push(Value{ .Boolean = lf > @intToFloat(f64, right_i.?) });
                        }
                    } else {
                        if (right_f) |rf| {
                            self.push(Value{ .Boolean = @intToFloat(f64, left_i.?) > rf });
                        } else {
                            self.push(Value{ .Boolean = left_i.? > right_i.? });
                        }
                    }
                },

                .OP_LESS => {
                    const right_value = floatToInteger(self.pop());
                    const left_value = floatToInteger(self.pop());

                    const left_f: ?f64 = if (left_value == .Float) left_value.Float else null;
                    const left_i: ?i64 = if (left_value == .Integer) left_value.Integer else null;
                    const right_f: ?f64 = if (right_value == .Float) right_value.Float else null;
                    const right_i: ?i64 = if (right_value == .Integer) right_value.Integer else null;

                    if (left_f) |lf| {
                        if (right_f) |rf| {
                            self.push(Value{ .Boolean = lf < rf });
                        } else {
                            self.push(Value{ .Boolean = lf < @intToFloat(f64, right_i.?) });
                        }
                    } else {
                        if (right_f) |rf| {
                            self.push(Value{ .Boolean = @intToFloat(f64, left_i.?) < rf });
                        } else {
                            self.push(Value{ .Boolean = left_i.? < right_i.? });
                        }
                    }
                },

                .OP_ADD,
                .OP_SUBTRACT,
                .OP_MULTIPLY,
                .OP_DIVIDE,
                .OP_MOD,
                .OP_BAND,
                .OP_BOR,
                .OP_XOR,
                .OP_SHL,
                .OP_SHR,
                => try self.binary(instruction),

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
                        try self.throw(Error.UnwrappedNull, (try self.gc.copyString("Force unwrapped optional is null")).toValue());
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
            }

            if (Config.debug_stack) {
                std.debug.print(
                    "frame: {s} {*}, code: {}\n",
                    .{
                        current_frame.closure.function.name.string,
                        current_frame.slots,
                        instruction,
                    },
                );
                try dumpStack(self);
            }
        }

        return true;
    }

    fn foreach(self: *Self) !void {
        var iterable_value: Value = self.peek(0);
        var iterable: *Obj = iterable_value.Obj;
        switch (iterable.obj_type) {
            .String => {
                var key_slot: *Value = @ptrCast(*Value, self.current_fiber.stack_top - 3);
                var value_slot: *Value = @ptrCast(*Value, self.current_fiber.stack_top - 2);
                var str: *ObjString = ObjString.cast(iterable).?;

                key_slot.* = if (try str.next(self, if (key_slot.* == .Null) null else key_slot.Integer)) |new_index|
                    Value{ .Integer = new_index }
                else
                    Value{ .Null = {} };

                // Set new value
                if (key_slot.* != .Null) {
                    value_slot.* = (try self.gc.copyString(&([_]u8{str.string[@intCast(usize, key_slot.Integer)]}))).toValue();
                }
            },
            .List => {
                var key_slot: *Value = @ptrCast(*Value, self.current_fiber.stack_top - 3);
                var value_slot: *Value = @ptrCast(*Value, self.current_fiber.stack_top - 2);
                var list: *ObjList = ObjList.cast(iterable).?;

                // Get next index
                key_slot.* = if (try list.rawNext(self, if (key_slot.* == .Null) null else key_slot.Integer)) |new_index|
                    Value{ .Integer = new_index }
                else
                    Value{ .Null = {} };

                // Set new value
                if (key_slot.* != .Null) {
                    value_slot.* = list.items.items[@intCast(usize, key_slot.Integer)];
                }
            },
            .Enum => {
                var value_slot: *Value = @ptrCast(*Value, self.current_fiber.stack_top - 2);
                var enum_case: ?*ObjEnumInstance = if (value_slot.* == .Null) null else ObjEnumInstance.cast(value_slot.Obj).?;
                var enum_: *ObjEnum = ObjEnum.cast(iterable).?;

                // Get next enum case
                var next_case: ?*ObjEnumInstance = try enum_.rawNext(self, enum_case);
                value_slot.* = (if (next_case) |new_case| Value{ .Obj = new_case.toObj() } else Value{ .Null = {} });
            },
            .Map => {
                var key_slot: *Value = @ptrCast(*Value, self.current_fiber.stack_top - 3);
                var value_slot: *Value = @ptrCast(*Value, self.current_fiber.stack_top - 2);
                var map: *ObjMap = ObjMap.cast(iterable).?;
                var current_key: ?HashableValue = if (key_slot.* != .Null) valueToHashable(key_slot.*) else null;

                var next_key: ?HashableValue = map.rawNext(current_key);
                key_slot.* = if (next_key) |unext_key| hashableToValue(unext_key) else Value{ .Null = {} };

                if (next_key) |unext_key| {
                    value_slot.* = map.map.get(unext_key) orelse Value{ .Null = {} };
                }
            },
            .Fiber => {
                var value_slot: *Value = @ptrCast(*Value, self.current_fiber.stack_top - 2);
                var fiber = ObjFiber.cast(iterable).?;

                if (fiber.fiber.status == .Over) {
                    value_slot.* = Value{ .Null = {} };
                } else {
                    try fiber.fiber.resume_(self);
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

        self.current_fiber.frame_count -= 1;
        _ = self.current_fiber.frames.pop();

        // We popped the last frame
        if (self.current_fiber.frame_count == 0) {
            // We're in a fiber
            if (self.current_fiber.parent_fiber != null) {
                try self.current_fiber.finish(self, result);

                // Don't stop the VM
                return false;
            }

            // We're not in a fiber, the program is over
            _ = self.pop();
            return true;
        }

        // Normal return, set the stack back and push the result
        self.current_fiber.stack_top = frame.slots;

        self.push(result);

        return false;
    }

    fn import(self: *Self, path: Value, value: Value) Error!void {
        const fullpath = ObjString.cast(path.Obj).?;
        const closure = ObjClosure.cast(value.Obj).?;

        if (self.import_registry.get(fullpath)) |globals| {
            for (globals.items) |global| {
                try self.globals.append(global);
            }
        } else {
            var vm = try self.gc.allocator.create(VM);
            vm.* = try VM.init(self.gc, self.import_registry);
            // TODO: how to free this since we copy things to new vm, also fails anyway
            // {
            //     defer vm.deinit();
            //     defer gn.deinit();
            // }

            try vm.interpret(closure.function, null);

            // Top of stack is how many export we got
            var exported_count: u8 = @intCast(u8, vm.peek(0).Integer);

            // Copy them to this vm globals
            var import_cache = std.ArrayList(Value).init(self.gc.allocator);
            if (exported_count > 0) {
                var i: u8 = exported_count;
                while (i > 0) : (i -= 1) {
                    const global = vm.peek(i);
                    try self.globals.append(global);
                    try import_cache.append(global);
                }
            }

            try self.import_registry.put(fullpath, import_cache);
        }

        // Pop path and closure
        _ = self.pop();
        _ = self.pop();
    }

    pub fn throw(self: *Self, code: Error, payload: Value) Error!void {
        var stack = std.ArrayList(CallFrame).init(self.gc.allocator);

        while (self.current_fiber.frame_count > 0) {
            var frame: *CallFrame = self.currentFrame().?;
            try stack.append(frame.*);

            // Are we in a try-catch ?
            if (frame.try_ip) |try_ip| {
                // Push error and jump to start of the catch clauses
                self.push(payload);

                frame.ip = try_ip;

                // As soon as we step into catch clauses, we're not in a try-catch block anymore
                frame.try_ip = null;

                return;
            }

            // Pop frame
            self.closeUpValues(&frame.slots[0]);
            self.current_fiber.frame_count -= 1;
            _ = self.current_fiber.frames.pop();
            if (self.current_fiber.frame_count == 0 and self.current_fiber.parent_fiber == null) {
                // No more frames, the error is uncaught.
                _ = self.pop();

                // Raise the runtime error
                std.debug.print("\n\u{001b}[31mError: {s}\u{001b}[0m\n", .{try valueToStringAlloc(self.gc.allocator, payload)});

                for (stack.items) |stack_frame| {
                    std.debug.print("\tat {s}", .{stack_frame.closure.function.name.string});
                    if (stack_frame.call_site) |call_site| {
                        std.debug.print(":{}\n", .{call_site});
                    } else {
                        std.debug.print("\n", .{});
                    }
                }

                return code;
            } else if (self.current_fiber.frame_count == 0) {
                // Error raised inside a fiber, forward it to parent fiber
                self.current_fiber = self.current_fiber.parent_fiber.?;

                try self.throw(code, payload);

                return;
            }

            self.current_fiber.stack_top = frame.slots;

            if (frame.error_value) |error_value| {
                // Push error_value as failed function return value
                self.push(error_value);

                return;
            } else if (try self.handleError(payload, frame.error_handlers)) {
                // Error was handled, stop unwinding frames
                stack.deinit();
                break;
            }
        }
    }

    // Returns true if error was handled
    fn handleError(self: *Self, error_payload: Value, handlers: std.ArrayList(*ObjClosure)) !bool {
        for (handlers.items) |handler| {
            const parameters = handler.function.type_def.resolved_type.?.Function.parameters;
            if (parameters.count() == 0 or _value.valueTypeEql(error_payload, parameters.get(parameters.keys()[0]).?)) {
                // In a normal frame, the slots 0 is either the function or a `this` value
                self.push(Value{ .Null = {} });

                // Push error payload
                self.push(error_payload);

                // Call handler, it's return value is the result of the frame we just closed
                try self.call(handler, 1, null);

                return true;
            }
        }

        return false;
    }

    fn binary(self: *Self, code: OpCode) !void {
        const right: Value = floatToInteger(self.pop());
        const left: Value = floatToInteger(self.pop());

        const right_f: ?f64 = if (right == .Float) right.Float else null;
        const left_f: ?f64 = if (left == .Float) left.Float else null;
        const right_i: ?i64 = if (right == .Integer) right.Integer else null;
        const left_i: ?i64 = if (left == .Integer) left.Integer else null;

        const right_s: ?*ObjString = if (right == .Obj) ObjString.cast(right.Obj) else null;
        const left_s: ?*ObjString = if (left == .Obj) ObjString.cast(left.Obj) else null;

        const right_l: ?*ObjList = if (right == .Obj) ObjList.cast(right.Obj) else null;
        const left_l: ?*ObjList = if (left == .Obj) ObjList.cast(left.Obj) else null;

        const right_m: ?*ObjMap = if (right == .Obj) ObjMap.cast(right.Obj) else null;
        const left_m: ?*ObjMap = if (left == .Obj) ObjMap.cast(left.Obj) else null;

        switch (code) {
            .OP_BAND => {
                self.push(Value{
                    .Integer = (left_i orelse @floatToInt(i64, left_f.?)) & (right_i orelse @floatToInt(i64, right_f.?)),
                });
            },
            .OP_BOR => {
                self.push(Value{
                    .Integer = (left_i orelse @floatToInt(i64, left_f.?)) | (right_i orelse @floatToInt(i64, right_f.?)),
                });
            },
            .OP_XOR => {
                self.push(Value{
                    .Integer = (left_i orelse @floatToInt(i64, left_f.?)) ^ (right_i orelse @floatToInt(i64, right_f.?)),
                });
            },
            .OP_SHL => {
                const b = right_i orelse @floatToInt(i64, right_f.?);

                if (b < 0) {
                    if (b * -1 > std.math.maxInt(u6)) {
                        self.push(Value{ .Integer = 0 });
                    } else {
                        self.push(Value{
                            .Integer = (left_i orelse @floatToInt(i64, left_f.?)) >> @truncate(u6, @intCast(u64, b * -1)),
                        });
                    }
                } else {
                    if (b > std.math.maxInt(u6)) {
                        self.push(Value{ .Integer = 0 });
                    } else {
                        self.push(Value{
                            .Integer = (left_i orelse @floatToInt(i64, left_f.?)) << @truncate(u6, @intCast(u64, b)),
                        });
                    }
                }
            },
            .OP_SHR => {
                const b = right_i orelse @floatToInt(i64, right_f.?);

                if (b < 0) {
                    if (b * -1 > std.math.maxInt(u6)) {
                        self.push(Value{ .Integer = 0 });
                    } else {
                        self.push(Value{
                            .Integer = (left_i orelse @floatToInt(i64, left_f.?)) << @truncate(u6, @intCast(u64, b * -1)),
                        });
                    }
                } else {
                    if (b > std.math.maxInt(u6)) {
                        self.push(Value{ .Integer = 0 });
                    } else {
                        self.push(Value{
                            .Integer = (left_i orelse @floatToInt(i64, left_f.?)) >> @truncate(u6, @intCast(u64, b)),
                        });
                    }
                }
            },
            .OP_ADD => add: {
                if (right_s != null) {
                    self.push(Value{ .Obj = (try left_s.?.concat(self, right_s.?)).toObj() });
                    break :add;
                } else if (right_f != null or left_f != null) {
                    self.push(Value{
                        .Float = (left_f orelse @intToFloat(f64, left_i.?)) + (right_f orelse @intToFloat(f64, right_i.?)),
                    });

                    break :add;
                } else if (right_i != null or left_i != null) {
                    self.push(Value{
                        .Integer = left_i.? + right_i.?,
                    });

                    break :add;
                } else if (right_l != null) {
                    var new_list = std.ArrayList(Value).init(self.gc.allocator);
                    try new_list.appendSlice(left_l.?.items.items);
                    try new_list.appendSlice(right_l.?.items.items);

                    self.push(
                        (try self.gc.allocateObject(ObjList, ObjList{
                            .type_def = left_l.?.type_def,
                            .methods = left_l.?.methods,
                            .items = new_list,
                        })).toValue(),
                    );

                    break :add;
                }

                // map
                var new_map = try left_m.?.map.clone();
                var it = right_m.?.map.iterator();
                while (it.next()) |entry| {
                    try new_map.put(entry.key_ptr.*, entry.value_ptr.*);
                }

                self.push(
                    (try self.gc.allocateObject(ObjMap, ObjMap{
                        .type_def = left_m.?.type_def,
                        .methods = left_m.?.methods,
                        .map = new_map,
                    })).toValue(),
                );
            },

            .OP_SUBTRACT => {
                if (right_f != null or left_f != null) {
                    self.push(Value{ .Float = (left_f orelse @intToFloat(f64, left_i.?)) - (right_f orelse @intToFloat(f64, right_i.?)) });
                } else {
                    self.push(Value{ .Integer = left_i.? - right_i.? });
                }
            },

            .OP_MULTIPLY => {
                if (right_f != null or left_f != null) {
                    self.push(Value{ .Float = (left_f orelse @intToFloat(f64, left_i.?)) * (right_f orelse @intToFloat(f64, right_i.?)) });
                } else {
                    self.push(Value{ .Integer = left_i.? * right_i.? });
                }
            },

            .OP_DIVIDE => {
                self.push(
                    Value{
                        .Float = (left_f orelse @intToFloat(f64, left_i.?)) / (right_f orelse @intToFloat(f64, right_i.?)),
                    },
                );
            },

            .OP_MOD => {
                if (right_f != null or left_f != null) {
                    self.push(Value{ .Float = @mod((left_f orelse @intToFloat(f64, left_i.?)), (right_f orelse @intToFloat(f64, right_i.?))) });
                } else {
                    self.push(Value{ .Integer = @mod(left_i.?, right_i.?) });
                }
            },

            else => unreachable,
        }
    }

    // FIXME: catch_values should be on the stack like arguments
    fn call(self: *Self, closure: *ObjClosure, arg_count: u8, catch_values: ?std.ArrayList(Value)) !void {
        // TODO: check for stack overflow
        var frame = CallFrame{
            .closure = closure,
            .ip = 0,
            // -1 is because we reserve slot 0 for this
            .slots = self.current_fiber.stack_top - arg_count - 1,
            .call_site = if (self.currentFrame()) |current_frame|
                current_frame.closure.function.chunk.lines.items[current_frame.ip - 1]
            else
                null,
            .error_handlers = std.ArrayList(*ObjClosure).init(self.gc.allocator),
        };

        if (catch_values != null) {
            for (catch_values.?.items) |catch_value| {
                if (catch_value == .Obj and ObjClosure.cast(catch_value.Obj) != null and ObjClosure.cast(catch_value.Obj).?.function.type_def.resolved_type.?.Function.function_type == .Catch) {
                    try frame.error_handlers.append(ObjClosure.cast(catch_value.Obj).?);
                } else {
                    assert(catch_values.?.items.len == 1);

                    frame.error_value = catch_value;
                }
            }
        }

        if (self.current_fiber.frames.items.len <= self.current_fiber.frame_count) {
            try self.current_fiber.frames.append(frame);
        } else {
            self.current_fiber.frames.items[self.current_fiber.frame_count] = frame;
        }

        self.current_fiber.frame_count += 1;
    }

    fn callNative(self: *Self, native: *ObjNative, arg_count: u8, catch_values: ?std.ArrayList(Value)) !void {
        // TODO: how to use catch_values with a native call?
        var result: Value = Value{ .Null = {} };
        const native_return = native.native(self);
        if (native_return == 1 or native_return == 0) {
            if (native_return == 1) {
                result = self.pop();
            }

            self.current_fiber.stack_top = self.current_fiber.stack_top - arg_count - 1;
            self.push(result);
        } else {
            // An error occured within the native function -> call error handlers
            if (catch_values != null) {
                var handlers = std.ArrayList(*ObjClosure).init(self.gc.allocator);
                defer handlers.deinit();
                for (catch_values.?.items) |catch_value| {
                    if (catch_value == .Obj and ObjClosure.cast(catch_value.Obj) != null and ObjClosure.cast(catch_value.Obj).?.function.type_def.resolved_type.?.Function.function_type == .Catch) {
                        try handlers.append(ObjClosure.cast(catch_value.Obj).?);
                    } else {
                        assert(catch_values.?.items.len == 1);

                        // We discard the error
                        _ = self.pop();

                        // Default value in case of error
                        self.current_fiber.stack_top = self.current_fiber.stack_top - arg_count - 1;
                        self.push(catch_value);
                        return;
                    }
                }

                // We have some error handlers to try
                if (try self.handleError(self.peek(0), handlers)) {
                    return;
                }
            }

            // Error was not handled are we in a try-catch ?
            var frame = self.currentFrame().?;
            if (frame.try_ip) |try_ip| {
                frame.ip = try_ip;

                // As soon as we step into catch clauses, we're not in a try-catch block anymore
                frame.try_ip = null;
            } else {
                // No error handler or default value was triggered so forward the error
                try self.throw(Error.Custom, self.peek(0));
            }
        }
    }

    fn bindMethod(self: *Self, method: ?*ObjClosure, native: ?*ObjNative) !void {
        var bound: *ObjBoundMethod = try self.gc.allocateObject(ObjBoundMethod, .{
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
                (self.current_fiber.stack_top - arg_count - 1)[0] = bound.receiver;

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
            else => {
                std.debug.print("unexpected @{} {}\n", .{ @ptrToInt(obj), obj.obj_type });
                unreachable;
            },
        }
    }

    fn instanciateObject(self: *Self, object_or_type: *Obj) !void {
        var instance: *ObjObjectInstance = try self.gc.allocateObject(
            ObjObjectInstance,
            ObjObjectInstance.init(
                self.gc.allocator,
                ObjObject.cast(object_or_type),
                ObjTypeDef.cast(object_or_type),
            ),
        );

        // If not anonymous, set default fields
        if (ObjObject.cast(object_or_type)) |object| {
            // Set instance fields with super classes default values
            if (object.super) |super| {
                try self.superDefaults(instance, super);
            }

            // Set instance fields with default values
            var it = object.fields.iterator();
            while (it.next()) |kv| {
                try instance.setField(self.gc, kv.key_ptr.*, try self.cloneValue(kv.value_ptr.*));
            }
        }

        self.push(instance.toValue());
    }

    // TODO: superDefaults and getSuperField could be replaced by specialized opcodes to avoid having to walk up the chain of inheritance

    fn superDefaults(self: *Self, instance: *ObjObjectInstance, super: *ObjObject) VM.Error!void {
        if (super.super) |super_super| {
            try self.superDefaults(instance, super_super);
        }

        var it = super.fields.iterator();
        while (it.next()) |kv| {
            try instance.setField(self.gc, kv.key_ptr.*, try self.cloneValue(kv.value_ptr.*));
        }
    }

    fn getSuperField(self: *Self, name: *ObjString, super: *ObjObject) VM.Error!void {
        if (super.static_fields.get(name)) |static| {
            _ = self.pop(); // Pop instance
            self.push(static);
        }
        if (super.methods.get(name)) |method| {
            try self.bindMethod(method, null);
        } else if (super.super) |super_super| {
            try self.getSuperField(name, super_super);
        }
    }

    fn invokeFromObject(self: *Self, object: *ObjObject, name: *ObjString, arg_count: u8, catch_values: ?std.ArrayList(Value)) !void {
        if (object.methods.get(name)) |method| {
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

                assert(instance.object != null);

                if (instance.fields.get(name)) |field| {
                    (self.current_fiber.stack_top - arg_count - 1)[0] = field;

                    return try self.callValue(field, arg_count, catch_values);
                }

                try self.invokeFromObject(instance.object.?, name, arg_count, catch_values);
            },
            .String => {
                if (try ObjString.member(self, name)) |member| {
                    var member_value: Value = Value{ .Obj = member.toObj() };
                    (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;

                    return try self.callValue(member_value, arg_count, catch_values);
                }

                unreachable;
            },
            .Pattern => {
                if (try ObjPattern.member(self, name)) |member| {
                    var member_value: Value = Value{ .Obj = member.toObj() };
                    (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;

                    return try self.callValue(member_value, arg_count, catch_values);
                }

                unreachable;
            },
            .Fiber => {
                if (try ObjFiber.member(self, name)) |member| {
                    var member_value: Value = Value{ .Obj = member.toObj() };
                    (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;

                    return try self.callValue(member_value, arg_count, catch_values);
                }

                unreachable;
            },
            .List => {
                var list: *ObjList = ObjList.cast(obj).?;

                if (try list.member(self, name)) |member| {
                    var member_value: Value = Value{ .Obj = member.toObj() };
                    (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;

                    return try self.callValue(member_value, arg_count, catch_values);
                }

                unreachable;
            },
            .Map => {
                var map: *ObjMap = ObjMap.cast(obj).?;

                if (try map.member(self, name)) |member| {
                    var member_value: Value = Value{ .Obj = member.toObj() };
                    (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;

                    return try self.callValue(member_value, arg_count, catch_values);
                }

                unreachable;
            },
            else => unreachable,
        }
    }

    fn closeUpValues(self: *Self, last: *Value) void {
        while (self.current_fiber.open_upvalues != null and @ptrToInt(self.current_fiber.open_upvalues.?.location) >= @ptrToInt(last)) {
            var upvalue: *ObjUpValue = self.current_fiber.open_upvalues.?;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed.?;
            self.current_fiber.open_upvalues = upvalue.next;
        }
    }

    fn captureUpvalue(self: *Self, local: *Value) !*ObjUpValue {
        var prev_upvalue: ?*ObjUpValue = null;
        var upvalue: ?*ObjUpValue = self.current_fiber.open_upvalues;
        while (upvalue != null and @ptrToInt(upvalue.?.location) > @ptrToInt(local)) {
            prev_upvalue = upvalue;
            upvalue = upvalue.?.next;
        }

        if (upvalue != null and upvalue.?.location == local) {
            return upvalue.?;
        }

        var created_upvalue: *ObjUpValue = try self.gc.allocateObject(ObjUpValue, ObjUpValue.init(local));
        created_upvalue.next = upvalue;

        if (prev_upvalue) |uprev_upvalue| {
            uprev_upvalue.next = created_upvalue;
        } else {
            self.current_fiber.open_upvalues = created_upvalue;
        }

        return created_upvalue;
    }

    fn appendToList(self: *Self) !void {
        var list: *ObjList = ObjList.cast(self.peek(1).Obj).?;
        var list_value: Value = self.peek(0);

        try list.rawAppend(self.gc, list_value);

        _ = self.pop();
    }

    fn defineEnumCase(self: *Self) !void {
        var enum_: *ObjEnum = ObjEnum.cast(self.peek(1).Obj).?;
        var enum_value: Value = self.peek(0);

        try enum_.cases.append(enum_value);
        try self.gc.markObjDirty(&enum_.obj);

        _ = self.pop();
    }

    fn defineMethod(self: *Self, name: *ObjString) !void {
        var method: Value = self.peek(0);
        var object: *ObjObject = ObjObject.cast(self.peek(1).Obj).?;

        try object.methods.put(name, ObjClosure.cast(method.Obj).?);

        _ = self.pop();
    }

    fn setObjectFieldDefaultValue(self: *Self, name: *ObjString) !void {
        var property: Value = self.peek(0);
        var object: *ObjObject = ObjObject.cast(self.peek(1).Obj).?;

        if (object.type_def.resolved_type.?.Object.fields.contains(name.string)) {
            try object.setField(self.gc, name, property);
        } else {
            assert(object.type_def.resolved_type.?.Object.static_fields.contains(name.string));
            try object.setStaticField(self.gc, name, property);
        }

        _ = self.pop();
    }

    fn subscript(self: *Self) !void {
        var subscriptable: *Obj = self.peek(1).Obj;
        var index: Value = floatToInteger(self.peek(0));

        switch (subscriptable.obj_type) {
            .List => {
                var list: *ObjList = ObjList.cast(subscriptable).?;

                if (index != .Integer or index.Integer < 0) {
                    try self.throw(Error.OutOfBound, (try self.gc.copyString("Out of bound list access.")).toValue());
                }

                const list_index: usize = @intCast(usize, index.Integer);

                if (list_index < list.items.items.len) {
                    var list_item: Value = list.items.items[list_index];

                    // Pop list and index
                    _ = self.pop();
                    _ = self.pop();

                    // Push value
                    self.push(list_item);
                } else {
                    try self.throw(Error.OutOfBound, (try self.gc.copyString("Out of bound list access.")).toValue());
                }
            },
            .Map => {
                var map: *ObjMap = ObjMap.cast(subscriptable).?;

                // Pop map and key
                _ = self.pop();
                _ = self.pop();

                if (map.map.get(valueToHashable(index))) |value| {
                    // Push value
                    self.push(value);
                } else {
                    self.push(Value{ .Null = {} });
                }
            },
            .String => {
                var str: *ObjString = ObjString.cast(subscriptable).?;

                if (index != .Integer or index.Integer < 0) {
                    try self.throw(Error.OutOfBound, (try self.gc.copyString("Out of bound string access.")).toValue());
                }

                const str_index: usize = @intCast(usize, index.Integer);

                if (str_index < str.string.len) {
                    var str_item: Value = (try self.gc.copyString(&([_]u8{str.string[str_index]}))).toValue();

                    // Pop str and index
                    _ = self.pop();
                    _ = self.pop();

                    // Push value
                    self.push(str_item);
                } else {
                    try self.throw(Error.OutOfBound, (try self.gc.copyString("Out of bound str access.")).toValue());
                }
            },
            else => unreachable,
        }
    }

    fn setSubscript(self: *Self) !void {
        var list_or_map: *Obj = self.peek(2).Obj;
        var index: Value = self.peek(1);
        var value: Value = self.peek(0);

        if (list_or_map.obj_type == .List) {
            var list: *ObjList = ObjList.cast(list_or_map).?;

            if (index != .Integer or index.Integer < 0) {
                try self.throw(Error.OutOfBound, (try self.gc.copyString("Out of bound list access.")).toValue());
            }

            const list_index: usize = @intCast(usize, index.Integer);

            if (list_index < list.items.items.len) {
                try list.set(self.gc, list_index, value);

                // Pop everyting
                _ = self.pop();
                _ = self.pop();
                _ = self.pop();

                // Push the value
                self.push(value);
            } else {
                try self.throw(Error.OutOfBound, (try self.gc.copyString("Out of bound list access.")).toValue());
            }
        } else {
            var map: *ObjMap = ObjMap.cast(list_or_map).?;

            try map.set(self.gc, index, value);

            // Pop everyting
            _ = self.pop();
            _ = self.pop();
            _ = self.pop();

            // Push the value
            self.push(value);
        }
    }
};
