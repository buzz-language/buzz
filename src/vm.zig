const std = @import("std");
const _value = @import("./value.zig");
const _obj = @import("./obj.zig");
const _chunk = @import("./chunk.zig");
const disassembler = @import("./disassembler.zig");
const Allocator = std.mem.Allocator;
const Value = _value.Value;
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
    globals: std.StringHashMap(Value),
    // Interned strings
    strings: std.StringHashMap(*ObjString),
    // Interned typedef, find a better way of hashing a key (won't accept float so we use toString)
    type_defs: std.StringHashMap(*ObjTypeDef),
    open_upvalues: std.ArrayList(*ObjUpValue),

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
            .globals = std.StringHashMap(Value).init(allocator),
            .strings = std.StringHashMap(*ObjString).init(allocator),
            .type_defs = std.StringHashMap(*ObjTypeDef).init(allocator),
            .open_upvalues = std.ArrayList(*ObjUpValue).init(allocator),
            .gray_stack = std.ArrayList(*Obj).init(allocator),
        };

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.frames.deinit();
        self.globals.deinit();
        self.strings.deinit();
        self.type_defs.deinit();
        self.open_upvalues.deinit();
        self.gray_stack.deinit();
    }

    pub fn getTypeDef(self: *Self, type_def: ObjTypeDef) !*ObjTypeDef {
        var type_def_str: []const u8 = try type_def.toString(self.allocator);
        defer self.allocator.free(type_def_str);

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

    fn readByte(frame: *CallFrame) callconv(.Inline) u8 {
        // TODO: measure if [*]OpCode[0] is faster
        var byte: u8 = frame.closure.function.chunk.code.items[frame.ip];

        frame.ip += 1;

        return byte;
    }

    fn readOpCode(frame: *CallFrame) callconv(.Inline) OpCode {
        // TODO: measure if [*]OpCode[0] is faster
        var opcode: OpCode = @intToEnum(OpCode, frame.closure.function.chunk.code.items[frame.ip]);

        frame.ip += 1;

        return opcode;
    }

    fn run(self: *Self) !InterpretResult {
        var frame: *CallFrame = &self.frames.items[self.frames.items.len - 1];

        // while (frame.ip < frame.closure.function.chunk.code.items.len) { // while (true) {
        //     switch(self.readByte(frame)) {
        //     }
        // }

        try disassembler.disassembleChunk(&frame.closure.function.chunk, frame.closure.function.name.string);

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
};