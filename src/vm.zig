const std = @import("std");
const _value = @import("./value.zig");
const _obj = @import("./obj.zig");
const _chunk = @import("./chunk.zig");
const Allocator = std.mem.Allocator;
const Value = _value.Value;
const ObjClosure = _obj.ObjClosure;
const ObjFunction = _obj.ObjFunction;
const ObjUpValue = _obj.ObjUpValue;
const ObjTypeDef = _obj.ObjTypeDef;
const OpCode = _chunk.OpCode;

pub const CallFrame = struct {
    closure: *ObjClosure,
    // Index into closure's chunk
    ip: usize,
    slots: [*]Value,
};

const init_string: [4]u8 = "init".*;

pub const VM = struct {
    const Self = @This();

    allocator: *Allocator,

    frames: std.ArrayList(CallFrame),

    // TODO: put ta limit somewhere
    stack: [1000000]Value,
    stack_top: usize,
    globals: std.StringArrayHashMap(Value),
    // Interned strings
    strings: std.StringArrayHashMap(*ObjString),
    // Interned typedef
    type_defs: std.AutoHashMap(ObjTypeDef, *ObjTypeDef),
    open_upvalues: std.ArrayList(*ObjUpValue),

    bytes_allocated: usize = 0,
    next_gc: usize = 0,
    // TODO: replace with SinglyLinkedList(*Obj)
    objects: ?*Obj = null,
    gray_stack: std.ArrayList(*Obj),

    pub fn init(allocator: *Allocator) Self {
        return .{
            .allocator = allocator,
            .frames = std.ArrayList(CallFrame).init(allocator),
            .stack = std.ArrayList(Value).init(allocator),
            .globals = std.StringArrayHashMap(Value).init(allocator),
            .strings = std.StringArrayHashMap(*ObjString).init(allocator),
            .type_defs = std.AutoHashMap(ObjTypeDef, *ObjTypeDef).init(allocator),
            .open_upvalues = std.ArrayList(*ObjUpValue).init(allocator),
            .gray_stack = std.ArrayList(*Obj).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.frames.deinit();
        self.stack.deinit();
        self.globals.deinit();
        self.strings.deinit();
        self.type_defs.deinit();
        self.open_upvalues.deinit();
        self.gray_stack.deinit();
    }

    pub fn getTypeDef(self: *Self, type_def: ObjTypeDef) !*ObjTypeDef {
        if (self.type_defs.get(type_def)) |type_def_ptr| {
            return type_def_ptr;
        }

        var type_def_ptr: *ObjTypeDef = try _obj.allocateObject(self, .Type);
        type_def_ptr.* = type_def;

        self.type_defs.put(type_def, type_def_ptr);

        return type_def_ptr;
    }

    fn push(self: *Self, value: Value) void {
        self.stack[self.stack_top] = Value;
        self.stack_top += 1;
    }

    fn pop(self: *Self) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    pub fn interpret(self: *Self, function: *ObjFunction) !?Value {
        self.push(.{
            .Obj = function.toObj()
        });

        var closure: *ObjClosure = try allocator.create(ObjClosure);
        closure.* = ObjClosure.init(allocator);

        _ = self.pop();

        self.push(.{
            .Obj = closure.toObj()
        });

        _ = try self.call(closure, 0);

        return try run();
    }

    fn readByte(self: *Self, frame: *CallFrame) callconv(.Inline) OpCode {
        // TODO: measure if [*]OpCode[0] is faster
        var opcode: OpCode = frame.closure.function.chunk.code.items[frame.ip];

        frame.ip += 1;

        return opcode;
    }

    fn run(self: *Self) !void {
        var frame: *CallFrame = &self.frames.items[self.frames.items.len - 1];

        while (frame.ip < frame.closure.function.chunk.code.items.len) { // while (true) {
            // switch(self.readByte(frame)) {
            // }

            std.debug.print("\n\t{}\t{}\n", .{ frame.ip, self.readByte(frame) });
        }
    }

    fn call(self: *Self, closure: *ObjClosure, arg_count: u8) bool {
        if (arg_count != closure.function.parameters.count()) {
            // TODO: runtimeError("Expected %d arguments but got %d.", closure->function->arity, argCount);
            
            return false;
        }
        
        // TODO: do we check for stack overflow

        var frame: *CallFrame = self.frames.add(.{
            .closure = closure,
            .ip = 0,
            .slots = self.stack[(self.stack_top - arg_count - 1)..]
        });

        return true;
    }
};