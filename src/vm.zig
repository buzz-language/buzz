const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const Value = @import("value.zig").Value;
const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;
const Ast = @import("Ast.zig");
const _disassembler = @import("disassembler.zig");
const _obj = @import("obj.zig");
const Allocator = std.mem.Allocator;
const BuildOptions = if (!is_wasm) @import("build_options") else @import("wasm.zig").BuildOptions;
const _memory = @import("memory.zig");
const GarbageCollector = _memory.GarbageCollector;
const TypeRegistry = _memory.TypeRegistry;
const is_wasm = builtin.cpu.arch.isWasm();
const JIT = if (!is_wasm) @import("Jit.zig") else void;
const Token = @import("Token.zig");
const Reporter = @import("Reporter.zig");
const FFI = if (!is_wasm) @import("FFI.zig") else void;
const dispatch_call_modifier: std.builtin.CallModifier = if (!is_wasm) .always_tail else .auto;

const ObjType = _obj.ObjType;
const Obj = _obj.Obj;
const ObjNative = _obj.ObjNative;
const NativeFn = _obj.NativeFn;
const Native = _obj.Native;
const NativeCtx = _obj.NativeCtx;
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
const ObjForeignContainer = _obj.ObjForeignContainer;
const cloneObject = _obj.cloneObject;
const disassembleChunk = _disassembler.disassembleChunk;
const dumpStack = _disassembler.dumpStack;
const jmp = if (!is_wasm) @import("jmp.zig").jmp else void;

pub const ImportRegistry = std.AutoHashMap(*ObjString, std.ArrayList(Value));

pub const RunFlavor = enum {
    Run,
    Test,
    Check,
    Fmt,
    Ast,
    Repl,

    pub inline fn resolveImports(self: RunFlavor) bool {
        return self == .Run or self == .Test or self == .Repl;
    }
};

pub const CallFrame = struct {
    const Self = @This();

    closure: *ObjClosure,
    // Index into closure's chunk
    ip: usize,
    // Frame
    slots: [*]Value,

    // Default value in case of error
    error_value: ?Value = null,

    // Line in source code where the call occured
    call_site: ?Ast.TokenIndex,

    // Offset at which error can be handled (means we're in a try block)
    try_ip: ?usize = null,
    // Top when try block started
    try_top: ?[*]Value = null,

    // True if a native function is being called, we need this because a native function can also
    // call buzz code and we need to know how to stop interpreting once we get back to native code
    in_native_call: bool = false,
    native_call_error_value: ?Value = null,
};

pub const TryCtx = if (!is_wasm)
    extern struct {
        previous: ?*TryCtx,
        env: jmp.jmp_buf = undefined,
        // FIXME: remember top here
    }
else
    void;

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
    has_catch_value: bool,
    method: ?*ObjString,

    frames: std.ArrayList(CallFrame),
    frame_count: usize = 0,
    recursive_count: u32 = 0,
    current_compiled_function: ?*ObjFunction = null,

    stack: []Value,
    stack_top: [*]Value,
    open_upvalues: ?*ObjUpValue,

    status: Status = .Instanciated,
    // true: we did `resolve fiber`, false: we did `resume fiber`
    resolved: bool = false,

    // When within a try catch in a JIT compiled function
    try_context: ?*TryCtx = null,

    type_def: *ObjTypeDef,

    pub fn init(
        allocator: Allocator,
        type_def: *ObjTypeDef,
        parent_fiber: ?*Fiber,
        stack_slice: ?[]Value,
        call_type: OpCode,
        arg_count: u8,
        has_catch_value: bool,
        method: ?*ObjString,
    ) !Self {
        var self: Self = .{
            .allocator = allocator,
            .type_def = type_def,
            .parent_fiber = parent_fiber,
            .stack = try allocator.alloc(Value, BuildOptions.stack_size),
            .stack_top = undefined,
            .frames = std.ArrayList(CallFrame).init(allocator),
            .open_upvalues = null,
            .call_type = call_type,
            .arg_count = arg_count,
            .has_catch_value = has_catch_value,
            .method = method,
        };

        if (stack_slice != null) {
            std.mem.copyForwards(Value, self.stack, stack_slice.?);

            self.stack_top = @as([*]Value, @ptrCast(self.stack[stack_slice.?.len..]));
        } else {
            self.stack_top = @as([*]Value, @ptrCast(self.stack[0..]));
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
            .OP_FIBER => { // | closure | ...args | ?catch |
                try vm.callValue(
                    vm.peek(self.arg_count),
                    self.arg_count,
                    if (self.has_catch_value) vm.pop() else null,
                    true,
                );
            },
            .OP_INVOKE_FIBER => { // | receiver | ...args | ?catch |
                _ = try vm.invoke(
                    self.method.?,
                    self.arg_count,
                    if (self.has_catch_value) vm.pop() else null,
                    true,
                    false,
                );
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
                .OP_FIBER_FOREACH => {
                    _ = vm.pop();

                    const value_slot: *Value = @ptrCast(vm.current_fiber.stack_top - 2);

                    value_slot.* = top;
                },
                else => {},
            }
        }
    }

    pub fn @"resume"(self: *Self, vm: *VM) !void {
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
                try vm.throw(
                    VM.Error.FiberOver,
                    (try vm.gc.copyString("Fiber is over")).toValue(),
                    null,
                    null,
                );
            },
            .Running => unreachable,
        }
    }

    pub fn resolve_(self: *Self, vm: *VM) !void {
        self.resolved = true;

        switch (self.status) {
            .Instanciated => try self.start(vm),
            .Yielded => try self.@"resume"(vm),
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
            vm.push(Value.Null);
        }

        // Do we need to finish OP_CODE that triggered the yield?
        const full_instruction = vm.readPreviousInstruction();
        if (full_instruction) |ufull_instruction| {
            const instruction = VM.getCode(ufull_instruction);
            switch (instruction) {
                .OP_FIBER_FOREACH => {
                    // We don't care about the returned value
                    _ = vm.pop();

                    const value_slot: *Value = @ptrCast(vm.current_fiber.stack_top - 2);

                    value_slot.* = Value.Null;
                },
                else => {},
            }
        }
    }
};

pub const VM = struct {
    const Self = @This();

    var cycles: u128 = 0;

    pub const Error = error{
        RuntimeError, // Thrown in wasm build because we can't stop the program
        CantCompile,
        UnwrappedNull,
        OutOfBound,
        NumberOverflow,
        NotInFiber,
        FiberOver,
        BadNumber,
        ReachedMaximumMemoryUsage,
        ReachedMaximumCPUUsage,
        ReachedMaximumRecursiveCall,
        Custom, // TODO: remove when user can use this set directly in buzz code
    } || Allocator.Error || std.fmt.BufPrintError;

    gc: *GarbageCollector,
    current_fiber: *Fiber,
    current_ast: Ast,
    main_fiber: *Fiber,
    globals: std.ArrayList(Value),
    globals_count: usize = 0,
    import_registry: *ImportRegistry,
    jit: ?JIT = null,
    flavor: RunFlavor,
    reporter: Reporter,
    ffi: FFI,

    pub fn init(gc: *GarbageCollector, import_registry: *ImportRegistry, flavor: RunFlavor) !Self {
        return .{
            .gc = gc,
            .import_registry = import_registry,
            .globals = std.ArrayList(Value).init(gc.allocator),
            .current_ast = undefined,
            .current_fiber = undefined,
            .main_fiber = undefined,
            .flavor = flavor,
            .reporter = Reporter{
                .allocator = gc.allocator,
            },
            .ffi = if (!is_wasm) FFI.init(gc) else {},
        };
    }

    pub fn deinit(self: *Self) void {
        // TODO: we can't free this because exported closure refer to it
        // self.globals.deinit();
        self.ffi.deinit();
    }

    pub fn cliArgs(self: *Self, args: ?[][:0]u8) !*ObjList {
        const list_def = ObjList.ListDef.init(
            self.gc.allocator,
            try self.gc.allocateObject(
                ObjTypeDef,
                ObjTypeDef{ .def_type = .String },
            ),
        );

        const list_def_union: ObjTypeDef.TypeUnion = .{
            .List = list_def,
        };

        const list_def_type: *ObjTypeDef = try self.gc.allocateObject(
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
            for (uargs, 0..) |arg, index| {
                // We can't have more than 255 arguments to a function
                // TODO: should we silently ignore them or should we raise an error?
                if (index >= 255) {
                    break;
                }

                try arg_list.items.append(
                    Value.fromObj((try self.gc.copyString(std.mem.sliceTo(arg, 0))).toObj()),
                );
            }
        }

        _ = self.pop();

        return arg_list;
    }

    pub fn push(self: *Self, value: Value) void {
        // FIXME: check overflow, can't we do it at compile time?

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

    pub inline fn cloneValue(self: *Self, value: Value) !Value {
        return if (value.isObj()) try cloneObject(value.obj(), self) else value;
    }

    inline fn clone(self: *Self) !void {
        self.push(try self.cloneValue(self.pop()));
    }

    inline fn swap(self: *Self, from: u8, to: u8) void {
        const temp: Value = (self.current_fiber.stack_top - to - 1)[0];
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

    pub fn interpret(self: *Self, ast: Ast, function: *ObjFunction, args: ?[][:0]u8) Error!void {
        const fiber_def = ObjFiber.FiberDef{
            .return_type = try self.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
            .yield_type = try self.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
        };

        const resolved_type = ObjTypeDef.TypeUnion{
            .Fiber = fiber_def,
        };

        const type_def = try self.gc.type_registry.getTypeDef(ObjTypeDef{
            .optional = false,
            .def_type = .Fiber,
            .resolved_type = resolved_type,
        });

        self.current_ast = ast;
        self.current_fiber = try self.gc.allocator.create(Fiber);
        self.current_fiber.* = try Fiber.init(
            self.gc.allocator,
            type_def,
            null, // parent fiber
            null, // stack_slice
            .OP_CALL, // call_type
            1, // arg_count
            false, // catch_count
            null, // method/member
        );

        self.push((try self.gc.allocateObject(
            ObjClosure,
            try ObjClosure.init(self.gc.allocator, self, function),
        )).toValue());

        self.push((try self.cliArgs(args)).toValue());

        try self.gc.registerVM(self);
        defer self.gc.unregisterVM(self);

        try self.callValue(self.peek(1), 0, null, false);

        self.current_fiber.status = .Running;

        return self.run();
    }

    fn readPreviousInstruction(self: *Self) ?u32 {
        const current_frame: *CallFrame = self.currentFrame().?;

        if (current_frame.ip > 0) {
            return current_frame.closure.function.chunk.code.items[current_frame.ip - 1];
        }

        return null;
    }

    inline fn readInstruction(self: *Self) u32 {
        const current_frame = self.currentFrame().?;
        const instruction = current_frame.closure.function.chunk.code.items[current_frame.ip];

        current_frame.ip += 1;

        return instruction;
    }

    pub inline fn getCode(instruction: u32) OpCode {
        return @enumFromInt(@as(u8, @intCast(instruction >> 24)));
    }

    inline fn getArg(instruction: u32) u24 {
        return @as(u24, @intCast(0x00ffffff & instruction));
    }

    inline fn readByte(self: *Self) u8 {
        return @as(u8, @intCast(self.readInstruction()));
    }

    inline fn readConstant(self: *Self, arg: u24) Value {
        return self.currentFrame().?.closure.function.chunk.constants.items[arg];
    }

    inline fn readString(self: *Self, arg: u24) *ObjString {
        return self.readConstant(arg).obj().access(
            ObjString,
            .String,
            self.gc,
        ).?;
    }

    const OpFn = *const fn (*Self, *CallFrame, u32, OpCode, u24) void;

    // WARNING: same order as OpCode enum
    const op_table = [_]OpFn{
        OP_CONSTANT,
        OP_NULL,
        OP_VOID,
        OP_TRUE,
        OP_FALSE,
        OP_POP,
        OP_COPY,
        OP_CLONE,

        OP_DEFINE_GLOBAL,
        OP_GET_GLOBAL,
        OP_SET_GLOBAL,
        OP_GET_LOCAL,
        OP_SET_LOCAL,
        OP_GET_UPVALUE,
        OP_SET_UPVALUE,
        OP_GET_LIST_SUBSCRIPT,
        OP_GET_MAP_SUBSCRIPT,
        OP_GET_STRING_SUBSCRIPT,
        OP_SET_LIST_SUBSCRIPT,
        OP_SET_MAP_SUBSCRIPT,

        OP_EQUAL,
        OP_IS,
        OP_GREATER,
        OP_LESS,
        OP_ADD_F,
        OP_ADD_I,
        OP_ADD_STRING,
        OP_ADD_LIST,
        OP_ADD_MAP,
        OP_SUBTRACT,
        OP_MULTIPLY,
        OP_DIVIDE,
        OP_MOD,
        OP_BNOT,
        OP_BAND,
        OP_BOR,
        OP_XOR,
        OP_SHL,
        OP_SHR,

        OP_UNWRAP,

        OP_NOT,
        OP_NEGATE,

        OP_SWAP,
        OP_JUMP,
        OP_JUMP_IF_FALSE,
        OP_JUMP_IF_NOT_NULL,
        OP_LOOP,
        OP_STRING_FOREACH,
        OP_LIST_FOREACH,
        OP_ENUM_FOREACH,
        OP_MAP_FOREACH,
        OP_FIBER_FOREACH,

        OP_CALL,
        OP_TAIL_CALL,
        OP_INSTANCE_INVOKE,
        OP_INSTANCE_TAIL_INVOKE,
        OP_STRING_INVOKE,
        OP_PATTERN_INVOKE,
        OP_FIBER_INVOKE,
        OP_LIST_INVOKE,
        OP_MAP_INVOKE,

        OP_CLOSURE,
        OP_CLOSE_UPVALUE,

        OP_FIBER,
        OP_INVOKE_FIBER,
        OP_RESUME,
        OP_RESOLVE,
        OP_YIELD,

        OP_TRY,
        OP_TRY_END,
        OP_THROW,

        OP_RETURN,

        OP_OBJECT,
        OP_INSTANCE,
        OP_FCONTAINER_INSTANCE,
        OP_METHOD,
        OP_PROPERTY,
        OP_GET_OBJECT_PROPERTY,
        OP_GET_INSTANCE_PROPERTY,
        OP_GET_FCONTAINER_INSTANCE_PROPERTY,
        OP_GET_LIST_PROPERTY,
        OP_GET_MAP_PROPERTY,
        OP_GET_STRING_PROPERTY,
        OP_GET_PATTERN_PROPERTY,
        OP_GET_FIBER_PROPERTY,
        OP_SET_OBJECT_PROPERTY,
        OP_SET_INSTANCE_PROPERTY,
        OP_SET_FCONTAINER_INSTANCE_PROPERTY,

        OP_GET_ENUM_CASE,
        OP_GET_ENUM_CASE_VALUE,
        OP_GET_ENUM_CASE_FROM_VALUE,

        OP_LIST,
        OP_RANGE,
        OP_LIST_APPEND,

        OP_MAP,
        OP_SET_MAP,

        OP_EXPORT,
        OP_IMPORT,

        OP_TO_STRING,

        OP_TYPEOF,
    };

    fn dispatch(self: *Self, current_frame: *CallFrame, full_instruction: u32, instruction: OpCode, arg: u24) void {
        if (BuildOptions.debug_stack) {
            dumpStack(self);
        }

        if (BuildOptions.debug_current_instruction or BuildOptions.debug_stack) {
            std.debug.print(
                "{}: {}\n",
                .{
                    current_frame.ip,
                    instruction,
                },
            );
        }

        // We're at the start of catch clauses because an error was thrown
        // We must close the try block scope
        if (current_frame.try_ip == current_frame.ip - 1) {
            assert(current_frame.try_top != null);
            const err = self.pop();

            // Close scope
            self.closeUpValues(@as(*Value, @ptrCast(current_frame.try_top.?)));
            self.current_fiber.stack_top = current_frame.try_top.?;

            // Put error back on stack
            self.push(err);

            // As soon as we step into catch clauses, we're not in a try-catch block anymore
            current_frame.try_ip = null;
            current_frame.try_top = null;
        }

        if (BuildOptions.gc_debug_access) {
            self.gc.where = current_frame.closure.function.chunk.lines.items[current_frame.ip - 1];
        }

        if (BuildOptions.cycle_limit) |limit| {
            cycles += 1;

            if (cycles > limit * 1000) {
                self.throw(
                    Error.ReachedMaximumCPUUsage,
                    (self.gc.copyString("Maximum CPU usage reached") catch @panic("Maximum CPU usage reached")).toValue(),
                    null,
                    null,
                ) catch @panic("Maximum CPU usage reached");

                return;
            }
        }

        // Tail call
        @call(
            dispatch_call_modifier,
            op_table[@intFromEnum(instruction)],
            .{
                self,
                current_frame,
                full_instruction,
                instruction,
                arg,
            },
        );
    }

    inline fn panic(e: anytype) void {
        std.debug.print("{}\n", .{e});
        if (!is_wasm) {
            std.os.exit(1);
        }
    }

    fn OP_NULL(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        self.push(Value.Null);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_VOID(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        self.push(Value.Void);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_TRUE(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        self.push(Value.True);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_FALSE(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        self.push(Value.False);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_POP(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        _ = self.pop();

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_COPY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        self.copy(arg);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_CLONE(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        self.clone() catch |e| {
            panic(e);
            unreachable;
        };

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_SWAP(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        self.swap(@as(u8, @intCast(arg)), self.readByte());

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_DEFINE_GLOBAL(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        self.globals.ensureTotalCapacity(arg + 1) catch |e| {
            panic(e);
            unreachable;
        };
        self.globals.expandToCapacity();
        self.globals.items[arg] = self.peek(0);
        self.globals_count = @max(self.globals_count, arg);
        _ = self.pop();

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_GLOBAL(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        self.push(self.currentGlobals().items[arg]);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_SET_GLOBAL(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        self.currentGlobals().items[arg] = self.peek(0);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_LOCAL(self: *Self, current_frame: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        self.push(current_frame.slots[arg]);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_SET_LOCAL(self: *Self, current_frame: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        current_frame.slots[arg] = self.peek(0);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_UPVALUE(self: *Self, current_frame: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        self.push(current_frame.closure.upvalues.items[arg].location.*);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_SET_UPVALUE(self: *Self, current_frame: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        current_frame.closure.upvalues.items[arg].location.* = self.peek(0);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_CONSTANT(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        self.push(self.readConstant(arg));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_TO_STRING(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const str = self.pop().toStringAlloc(self.gc.allocator) catch |e| {
            panic(e);
            unreachable;
        };
        self.push(
            Value.fromObj(
                (self.gc.copyString(str.items) catch |e| {
                    panic(e);
                    unreachable;
                }).toObj(),
            ),
        );
        str.deinit();

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_NEGATE(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const value = self.pop();

        if (value.isInteger()) {
            self.push(Value.fromInteger(-%value.integer()));
        } else {
            self.push(Value.fromFloat(-value.float()));
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_CLOSURE(self: *Self, current_frame: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const function: *ObjFunction = self.readConstant(arg).obj().access(ObjFunction, .Function, self.gc).?;
        var closure: *ObjClosure = self.gc.allocateObject(
            ObjClosure,
            ObjClosure.init(self.gc.allocator, self, function) catch |e| {
                panic(e);
                unreachable;
            },
        ) catch |e| {
            panic(e);
            unreachable;
        };

        self.push(closure.toValue());

        var i: usize = 0;
        while (i < function.upvalue_count) : (i += 1) {
            const is_local: bool = self.readByte() == 1;
            const index: u8 = self.readByte();

            if (is_local) {
                closure.upvalues.append(self.captureUpvalue(&(current_frame.slots[index])) catch |e| {
                    panic(e);
                    unreachable;
                }) catch |e| {
                    panic(e);
                    unreachable;
                };
            } else {
                closure.upvalues.append(current_frame.closure.upvalues.items[index]) catch |e| {
                    panic(e);
                    unreachable;
                };
            }
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_CLOSE_UPVALUE(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        self.closeUpValues(@ptrCast(self.current_fiber.stack_top - 1));
        _ = self.pop();

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_FIBER(self: *Self, _: *CallFrame, full_instruction: u32, instruction: OpCode, _: u24) void {
        const arg_count: u8 = @intCast((0x00ffffff & full_instruction) >> 16);
        const catch_count: u16 = @intCast(0x0000ffff & full_instruction);

        const stack_ptr = self.current_fiber.stack_top - arg_count - catch_count - 1;
        const stack_len = arg_count + catch_count + 1;
        const stack_slice = stack_ptr[0..stack_len];

        var fiber = self.gc.allocator.create(Fiber) catch |e| {
            panic(e);
            unreachable;
        };
        fiber.* = Fiber.init(
            self.gc.allocator,
            undefined,
            self.current_fiber,
            stack_slice,
            instruction,
            arg_count,
            catch_count > 0,
            null,
        ) catch |e| {
            panic(e);
            unreachable;
        };

        // Pop arguments and catch clauses
        self.current_fiber.stack_top = self.current_fiber.stack_top - stack_len;

        fiber.type_def = self.pop().obj().access(ObjTypeDef, .Type, self.gc).?;

        // Put new fiber on the stack
        var obj_fiber = self.gc.allocateObject(ObjFiber, ObjFiber{
            .fiber = fiber,
        }) catch |e| {
            panic(e);
            unreachable;
        };

        self.push(obj_fiber.toValue());

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_INVOKE_FIBER(self: *Self, _: *CallFrame, _: u32, instruction: OpCode, arg: u24) void {
        const method: *ObjString = self.readString(arg);
        const arg_instruction: u32 = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);

        const stack_ptr = self.current_fiber.stack_top - arg_count - catch_count - 1;
        const stack_len = arg_count + catch_count + 1;
        const stack_slice = stack_ptr[0..stack_len];

        var fiber = self.gc.allocator.create(Fiber) catch |e| {
            panic(e);
            unreachable;
        };
        fiber.* = Fiber.init(
            self.gc.allocator,
            undefined,
            self.current_fiber,
            stack_slice,
            instruction,
            arg_count,
            catch_count > 0,
            method,
        ) catch |e| {
            panic(e);
            unreachable;
        };

        // Pop arguments and catch clauses
        self.current_fiber.stack_top = self.current_fiber.stack_top - stack_len;

        fiber.type_def = self.pop().obj().access(ObjTypeDef, .Type, self.gc).?;

        // Push new fiber on the stack
        var obj_fiber = self.gc.allocateObject(ObjFiber, ObjFiber{
            .fiber = fiber,
        }) catch |e| {
            panic(e);
            unreachable;
        };

        self.push(obj_fiber.toValue());

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_RESUME(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const obj_fiber = self.pop().obj().access(ObjFiber, .Fiber, self.gc).?;
        obj_fiber.fiber.@"resume"(self) catch |e| {
            panic(e);
            unreachable;
        };

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_RESOLVE(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const obj_fiber = self.pop().obj().access(ObjFiber, .Fiber, self.gc).?;
        obj_fiber.fiber.resolve_(self) catch |e| {
            panic(e);
            unreachable;
        };

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_YIELD(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        self.current_fiber.yield(self);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_CALL(self: *Self, _: *CallFrame, full_instruction: u32, _: OpCode, _: u24) void {
        const arg_count: u8 = @intCast((0x00ffffff & full_instruction) >> 16);
        const catch_count: u16 = @intCast(0x0000ffff & full_instruction);

        // FIXME: no reason to take the catch value off the stack
        const catch_value = if (catch_count > 0) self.pop() else null;

        self.callValue(
            self.peek(arg_count),
            arg_count,
            catch_value,
            false,
        ) catch |e| {
            panic(e);
            unreachable;
        };

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_TAIL_CALL(self: *Self, _: *CallFrame, full_instruction: u32, _: OpCode, _: u24) void {
        const arg_count: u8 = @intCast((0x00ffffff & full_instruction) >> 16);
        const catch_count: u16 = @intCast(0x0000ffff & full_instruction);

        // FIXME: no reason to take the catch value off the stack
        const catch_value = if (catch_count > 0) self.pop() else null;

        self.tailCall(
            self.peek(arg_count),
            arg_count,
            catch_value,
            false,
        ) catch |e| {
            panic(e);
            unreachable;
        };

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_INSTANCE_INVOKE(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const method: *ObjString = self.readString(arg);
        const arg_instruction: u32 = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const instance: *ObjObjectInstance = self.peek(arg_count).obj().access(ObjObjectInstance, .ObjectInstance, self.gc).?;

        assert(instance.object != null);

        if (instance.fields.get(method)) |field| {
            (self.current_fiber.stack_top - arg_count - 1)[0] = field;

            self.callValue(field, arg_count, catch_value, false) catch |e| {
                panic(e);
                unreachable;
            };
        } else {
            _ = self.invokeFromObject(
                instance.object.?,
                method,
                arg_count,
                catch_value,
                false,
                false,
            ) catch |e| {
                panic(e);
                unreachable;
            };
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_INSTANCE_TAIL_INVOKE(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const method: *ObjString = self.readString(arg);
        const arg_instruction: u32 = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const instance: *ObjObjectInstance = self.peek(arg_count).obj().access(ObjObjectInstance, .ObjectInstance, self.gc).?;

        assert(instance.object != null);

        if (instance.fields.get(method)) |field| {
            (self.current_fiber.stack_top - arg_count - 1)[0] = field;

            self.tailCall(field, arg_count, catch_value, false) catch |e| {
                panic(e);
                unreachable;
            };
        } else {
            _ = self.invokeFromObject(
                instance.object.?,
                method,
                arg_count,
                catch_value,
                false,
                true,
            ) catch |e| {
                panic(e);
                unreachable;
            };
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_STRING_INVOKE(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const method: *ObjString = self.readString(arg);
        const arg_instruction: u32 = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const member = (ObjString.member(self, method) catch |e| {
            panic(e);
            unreachable;
        }).?;
        const member_value: Value = member.toValue();
        (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;

        self.callValue(member_value, arg_count, catch_value, false) catch |e| {
            panic(e);
            unreachable;
        };

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_PATTERN_INVOKE(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const method: *ObjString = self.readString(arg);
        const arg_instruction: u32 = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const member = (ObjPattern.member(self, method) catch |e| {
            panic(e);
            unreachable;
        }).?;
        const member_value: Value = member.toValue();
        (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;

        self.callValue(member_value, arg_count, catch_value, false) catch |e| {
            panic(e);
            unreachable;
        };

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_FIBER_INVOKE(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const method: *ObjString = self.readString(arg);
        const arg_instruction: u32 = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const member = (ObjFiber.member(self, method) catch |e| {
            panic(e);
            unreachable;
        }).?;
        const member_value: Value = member.toValue();
        (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;
        self.callValue(member_value, arg_count, catch_value, false) catch |e| {
            panic(e);
            unreachable;
        };

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_LIST_INVOKE(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const method: *ObjString = self.readString(arg);
        const arg_instruction: u32 = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const list = self.peek(arg_count).obj().access(ObjList, .List, self.gc).?;
        const member = (list.member(self, method) catch |e| {
            panic(e);
            unreachable;
        }).?;

        const member_value: Value = member.toValue();
        (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;
        self.callValue(member_value, arg_count, catch_value, false) catch |e| {
            panic(e);
            unreachable;
        };

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_MAP_INVOKE(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const method: *ObjString = self.readString(arg);
        const arg_instruction: u32 = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const map = self.peek(arg_count).obj().access(ObjMap, .Map, self.gc).?;
        const member = (map.member(self, method) catch |e| {
            panic(e);
            unreachable;
        }).?;

        const member_value: Value = member.toValue();
        (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;
        self.callValue(member_value, arg_count, catch_value, false) catch |e| {
            panic(e);
            unreachable;
        };

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    // result_count > 0 when the return is `export`
    inline fn returnFrame(self: *Self) bool {
        const result = self.pop();

        const frame: *CallFrame = self.currentFrame().?;

        self.closeUpValues(&frame.slots[0]);

        self.current_fiber.frame_count -= 1;
        _ = self.current_fiber.frames.pop();

        // We popped the last frame
        if (self.current_fiber.frame_count == 0) {
            // We're in a fiber
            if (self.current_fiber.parent_fiber != null) {
                self.current_fiber.finish(self, result) catch |e| {
                    panic(e);
                    unreachable;
                };

                // Don't stop the VM
                return false;
            }

            // We're not in a fiber, the program is over
            if (self.flavor != .Repl) {
                _ = self.pop();
            }
            return true;
        }

        // Normal return, set the stack back and push the result
        self.current_fiber.stack_top = frame.slots;

        self.push(result);

        return false;
    }

    inline fn repurposeFrame(self: *Self, closure: *ObjClosure, arg_count: u8, catch_value: ?Value, in_fiber: bool) Error!void {
        // Is or will be JIT compiled, call and stop there
        if (!is_wasm and !in_fiber and try self.compileAndCall(closure, arg_count, catch_value)) {
            return;
        }

        const frame = self.currentFrame().?;
        const call_site = self.getSite();

        // Close upvalues
        self.closeUpValues(&frame.slots[0]);

        // Shift new call arguments at start of current frame
        std.mem.copyForwards(
            Value,
            frame.slots[0..(arg_count + 1)],
            (self.current_fiber.stack_top - arg_count - 1)[0..(arg_count + 1)],
        );

        // Reposition stack top
        self.current_fiber.stack_top = frame.slots + arg_count + 1;

        // Repurpose frame
        frame.closure = closure;
        frame.ip = 0;
        frame.call_site = call_site;
        frame.error_value = catch_value;
    }

    fn OP_RETURN(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        if (self.returnFrame() or self.currentFrame().?.in_native_call) {
            return;
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_EXPORT(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        self.push(Value.fromInteger(@as(i32, @intCast(arg))));

        // Ends program, so we don't call dispatch
    }

    fn OP_IMPORT(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const fullpath = self.peek(1).obj().access(ObjString, .String, self.gc).?;
        const closure = self.peek(0).obj().access(ObjClosure, .Closure, self.gc).?;

        if (self.import_registry.get(fullpath)) |globals| {
            for (globals.items) |global| {
                self.globals.append(global) catch |e| {
                    panic(e);
                    unreachable;
                };
            }
        } else {
            var vm = self.gc.allocator.create(VM) catch |e| {
                panic(e);
                unreachable;
            };
            // FIXME: give reference to JIT?
            vm.* = VM.init(self.gc, self.import_registry, self.flavor) catch |e| {
                panic(e);
                unreachable;
            };
            // TODO: how to free this since we copy things to new vm, also fails anyway
            // {
            //     defer vm.deinit();
            //     defer gn.deinit();
            // }

            vm.interpret(self.current_ast, closure.function, null) catch |e| {
                panic(e);
                unreachable;
            };

            // Top of stack is how many export we got
            const exported_count = vm.peek(0).integer();

            // Copy them to this vm globals
            var import_cache = std.ArrayList(Value).init(self.gc.allocator);
            if (exported_count > 0) {
                var i = exported_count;
                while (i > 0) : (i -= 1) {
                    const global = vm.peek(@intCast(i));
                    self.globals.append(global) catch |e| {
                        panic(e);
                        unreachable;
                    };
                    import_cache.append(global) catch |e| {
                        panic(e);
                        unreachable;
                    };
                }
            }

            self.import_registry.put(fullpath, import_cache) catch |e| {
                panic(e);
                unreachable;
            };
        }

        // Pop path and closure
        _ = self.pop();
        _ = self.pop();

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_TRY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        self.currentFrame().?.try_ip = @as(usize, @intCast(arg));
        // We will close scope up to this top if an error is thrown
        self.currentFrame().?.try_top = self.current_fiber.stack_top;

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_TRY_END(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        self.currentFrame().?.try_ip = null;
        self.currentFrame().?.try_top = null;

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_THROW(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        self.throw(
            Error.Custom,
            self.pop(),
            null,
            null,
        ) catch |e| {
            panic(e);
            unreachable;
        };

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_LIST(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        var list: *ObjList = self.gc.allocateObject(
            ObjList,
            ObjList.init(self.gc.allocator, self.readConstant(arg).obj().access(ObjTypeDef, .Type, self.gc).?),
        ) catch |e| {
            panic(e);
            unreachable;
        };

        self.push(Value.fromObj(list.toObj()));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_RANGE(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const high = self.pop().integer();
        const low = self.pop().integer();

        var list: *ObjList = self.gc.allocateObject(
            ObjList,
            ObjList.init(
                self.gc.allocator,
                self.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Integer,
                    },
                ) catch @panic("Could not instanciate list"),
            ),
        ) catch |e| {
            panic(e);
            unreachable;
        };

        self.push(Value.fromObj(list.toObj()));

        if (low < high) {
            var i: i32 = low;
            while (i < high) : (i += 1) {
                list.rawAppend(self.gc, Value.fromInteger(i)) catch @panic("Could not append to list");
            }
        } else {
            var i: i32 = low;
            while (i > high) : (i -= 1) {
                list.rawAppend(self.gc, Value.fromInteger(i)) catch @panic("Could not append to list");
            }
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_LIST_APPEND(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        var list = self.peek(1).obj().access(ObjList, .List, self.gc).?;
        const list_value = self.peek(0);

        list.rawAppend(self.gc, list_value) catch |e| {
            panic(e);
            unreachable;
        };

        _ = self.pop();

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_MAP(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        var map: *ObjMap = self.gc.allocateObject(ObjMap, ObjMap.init(
            self.gc.allocator,
            self.readConstant(arg).obj().access(ObjTypeDef, .Type, self.gc).?,
        )) catch |e| {
            panic(e);
            unreachable;
        };

        self.push(Value.fromObj(map.toObj()));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_SET_MAP(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        var map = self.peek(2).obj().access(ObjMap, .Map, self.gc).?;
        const key = self.peek(1);
        const value = self.peek(0);

        map.set(self.gc, key, value) catch |e| {
            panic(e);
            unreachable;
        };

        _ = self.pop();
        _ = self.pop();

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_LIST_SUBSCRIPT(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const list = self.peek(1).obj().access(ObjList, .List, self.gc).?;
        const index = self.peek(0).integer();

        if (index < 0) {
            self.throw(
                Error.OutOfBound,
                (self.gc.copyString("Out of bound list access.") catch |e| {
                    panic(e);
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |e| {
                panic(e);
                unreachable;
            };
        }

        const list_index: usize = @intCast(index);

        if (list_index >= list.items.items.len) {
            self.throw(
                Error.OutOfBound,
                (self.gc.copyString("Out of bound list access.") catch |e| {
                    panic(e);
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |e| {
                panic(e);
                unreachable;
            };

            return;
        }

        const list_item = list.items.items[list_index];

        // Pop list and index
        _ = self.pop();
        _ = self.pop();

        // Push value
        self.push(list_item);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_MAP_SUBSCRIPT(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        var map = self.peek(1).obj().access(ObjMap, .Map, self.gc).?;
        const index = self.peek(0);

        // Pop map and key
        _ = self.pop();
        _ = self.pop();

        if (map.map.get(index)) |value| {
            // Push value
            self.push(value);
        } else {
            self.push(Value.Null);
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_STRING_SUBSCRIPT(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const str = self.peek(1).obj().access(ObjString, .String, self.gc).?;
        const index = self.peek(0).integer();

        if (index < 0) {
            self.throw(
                Error.OutOfBound,
                (self.gc.copyString("Out of bound string access.") catch |e| {
                    panic(e);
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |e| {
                panic(e);
                unreachable;
            };
        }

        const str_index: usize = @intCast(index);

        if (str_index < str.string.len) {
            const str_item = (self.gc.copyString(&([_]u8{str.string[str_index]})) catch |e| {
                panic(e);
                unreachable;
            }).toValue();

            // Pop str and index
            _ = self.pop();
            _ = self.pop();

            // Push value
            self.push(str_item);
        } else {
            self.throw(
                Error.OutOfBound,
                (self.gc.copyString("Out of bound str access.") catch |e| {
                    panic(e);
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |e| {
                panic(e);
                unreachable;
            };
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_SET_LIST_SUBSCRIPT(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        var list = self.peek(2).obj().access(ObjList, .List, self.gc).?;
        const index = self.peek(1);
        const value = self.peek(0);

        if (index.integer() < 0) {
            self.throw(
                Error.OutOfBound,
                (self.gc.copyString("Out of bound list access.") catch |e| {
                    panic(e);
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |e| {
                panic(e);
                unreachable;
            };
        }

        const list_index: usize = @intCast(index.integer());

        if (list_index < list.items.items.len) {
            list.set(self.gc, list_index, value) catch |e| {
                panic(e);
                unreachable;
            };

            // Pop everyting
            _ = self.pop();
            _ = self.pop();
            _ = self.pop();

            // Push the value
            self.push(value);
        } else {
            self.throw(
                Error.OutOfBound,
                (self.gc.copyString("Out of bound list access.") catch |e| {
                    panic(e);
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |e| {
                panic(e);
                unreachable;
            };
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_SET_MAP_SUBSCRIPT(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        var map: *ObjMap = self.peek(2).obj().access(ObjMap, .Map, self.gc).?;
        const index = self.peek(1);
        const value = self.peek(0);

        map.set(self.gc, index, value) catch |e| {
            panic(e);
            unreachable;
        };

        // Pop everyting
        _ = self.pop();
        _ = self.pop();
        _ = self.pop();

        // Push the value
        self.push(value);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_ENUM_CASE(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const enum_ = self.peek(0).obj().access(ObjEnum, .Enum, self.gc).?;

        _ = self.pop();

        var enum_case: *ObjEnumInstance = self.gc.allocateObject(
            ObjEnumInstance,
            ObjEnumInstance{
                .enum_ref = enum_,
                .case = @intCast(arg),
            },
        ) catch |e| {
            panic(e);
            unreachable;
        };

        self.push(Value.fromObj(enum_case.toObj()));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_ENUM_CASE_VALUE(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const enum_case = self.peek(0).obj().access(ObjEnumInstance, .EnumInstance, self.gc).?;

        _ = self.pop();
        self.push(enum_case.enum_ref.cases[enum_case.case]);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_ENUM_CASE_FROM_VALUE(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const case_value = self.pop();
        const enum_ = self.pop().obj().access(ObjEnum, .Enum, self.gc).?;

        var found = false;
        for (enum_.cases, 0..) |case, index| {
            if (case.eql(case_value)) {
                var enum_case: *ObjEnumInstance = self.gc.allocateObject(ObjEnumInstance, ObjEnumInstance{
                    .enum_ref = enum_,
                    .case = @intCast(index),
                }) catch |e| {
                    panic(e);
                    unreachable;
                };

                self.push(Value.fromObj(enum_case.toObj()));
                found = true;

                break;
            }
        }

        if (!found) {
            self.push(Value.Null);
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_OBJECT(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        var object: *ObjObject = self.gc.allocateObject(
            ObjObject,
            ObjObject.init(
                self.gc.allocator,
                self.readConstant(arg).obj().access(ObjString, .String, self.gc).?,
                self.readConstant(@as(u24, @intCast(self.readInstruction()))).obj().access(ObjTypeDef, .Type, self.gc).?,
            ),
        ) catch |e| {
            panic(e);
            unreachable;
        };

        self.push(Value.fromObj(object.toObj()));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_FCONTAINER_INSTANCE(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const typedef = self.pop().obj().access(ObjTypeDef, .Type, self.gc);
        const instance = (self.gc.allocateObject(
            ObjForeignContainer,
            ObjForeignContainer.init(
                self,
                typedef.?,
            ) catch @panic("Out of memory"),
        ) catch @panic("Out of memory")).toValue();

        self.push(instance);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_INSTANCE(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const typedef = self.pop().obj().access(ObjTypeDef, .Type, self.gc).?;
        const object_or_null = self.pop();
        const object = if (object_or_null.isObj())
            object_or_null.obj().access(ObjObject, .Object, self.gc).?
        else
            null;

        var obj_instance: *ObjObjectInstance = self.gc.allocateObject(
            ObjObjectInstance,
            ObjObjectInstance.init(
                self,
                object,
                typedef,
            ),
        ) catch @panic("Out of memory");

        // If not anonymous, set default fields
        if (object) |obj| {
            // Set instance fields with default values
            var it = obj.fields.iterator();
            while (it.next()) |kv| {
                obj_instance.setField(
                    self.gc,
                    kv.key_ptr.*,
                    self.cloneValue(kv.value_ptr.*) catch |e| {
                        panic(e);
                        unreachable;
                    },
                ) catch |e| {
                    panic(e);
                    unreachable;
                };
            }
        }

        self.push(obj_instance.toValue());

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_METHOD(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const name = self.readString(arg);
        var method: Value = self.peek(0);
        var object: *ObjObject = self.peek(1).obj().access(ObjObject, .Object, self.gc).?;

        object.methods.put(
            name,
            method.obj().access(ObjClosure, .Closure, self.gc).?,
        ) catch |e| {
            panic(e);
            unreachable;
        };

        _ = self.pop();

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const name = self.readString(arg);
        const property = self.peek(0);
        var object = self.peek(1).obj().access(ObjObject, .Object, self.gc).?;

        if (object.type_def.resolved_type.?.Object.fields.contains(name.string)) {
            object.setField(self.gc, name, property) catch |e| {
                panic(e);
                unreachable;
            };
        } else {
            assert(object.type_def.resolved_type.?.Object.static_fields.contains(name.string));
            object.setStaticField(self.gc, name, property) catch |e| {
                panic(e);
                unreachable;
            };
        }

        _ = self.pop();

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_OBJECT_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const object: *ObjObject = self.peek(0).obj().access(ObjObject, .Object, self.gc).?;
        const name: *ObjString = self.readString(arg);

        _ = self.pop(); // Pop instance
        self.push(object.static_fields.get(name).?);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_FCONTAINER_INSTANCE_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const instance_value = self.peek(0);
        const name: *ObjString = self.readString(arg);

        const struct_instance = instance_value.obj().access(
            ObjForeignContainer,
            .ForeignContainer,
            self.gc,
        ).?;

        _ = self.pop(); // Pop instance
        self.push(struct_instance.getField(self, name.string));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_INSTANCE_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const instance_value = self.peek(0);
        const name: *ObjString = self.readString(arg);
        const obj_instance = instance_value.obj().access(ObjObjectInstance, .ObjectInstance, self.gc).?;

        if (obj_instance.fields.get(name)) |field| {
            _ = self.pop(); // Pop instance
            self.push(field);
        } else if (obj_instance.object) |object| {
            if (object.methods.get(name)) |method| {
                self.bindMethod(method, null) catch |e| {
                    panic(e);
                    unreachable;
                };
            } else {
                unreachable;
            }
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_LIST_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const list = self.peek(0).obj().access(ObjList, .List, self.gc).?;
        const name: *ObjString = self.readString(arg);

        if (list.member(self, name) catch |e| {
            panic(e);
            unreachable;
        }) |member| {
            self.bindMethod(null, member) catch |e| {
                panic(e);
                unreachable;
            };
        } else {
            unreachable;
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }
    fn OP_GET_MAP_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const map = self.peek(0).obj().access(ObjMap, .Map, self.gc).?;
        const name: *ObjString = self.readString(arg);

        if (map.member(self, name) catch |e| {
            panic(e);
            unreachable;
        }) |member| {
            self.bindMethod(null, member) catch |e| {
                panic(e);
                unreachable;
            };
        } else {
            unreachable;
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_STRING_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const name: *ObjString = self.readString(arg);

        if (ObjString.member(self, name) catch |e| {
            panic(e);
            unreachable;
        }) |member| {
            self.bindMethod(null, member) catch |e| {
                panic(e);
                unreachable;
            };
        } else {
            unreachable;
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_PATTERN_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const name: *ObjString = self.readString(arg);

        if (ObjPattern.member(self, name) catch |e| {
            panic(e);
            unreachable;
        }) |member| {
            self.bindMethod(null, member) catch |e| {
                panic(e);
                unreachable;
            };
        } else {
            unreachable;
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GET_FIBER_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const name: *ObjString = self.readString(arg);

        if (ObjFiber.member(self, name) catch |e| {
            panic(e);
            unreachable;
        }) |member| {
            self.bindMethod(null, member) catch |e| {
                panic(e);
                unreachable;
            };
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_SET_OBJECT_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const object: *ObjObject = self.peek(1).obj().access(ObjObject, .Object, self.gc).?;
        const name: *ObjString = self.readString(arg);

        // Set new value
        object.setStaticField(self.gc, name, self.peek(0)) catch |e| {
            panic(e);
            unreachable;
        };

        // Get the new value from stack, pop the object and push value again
        const value: Value = self.pop();
        _ = self.pop();
        self.push(value);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_SET_FCONTAINER_INSTANCE_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const instance_value = self.peek(1);
        const name: *ObjString = self.readString(arg);

        const struct_instance = instance_value.obj().access(
            ObjForeignContainer,
            .ForeignContainer,
            self.gc,
        ).?;

        struct_instance.setField(
            self,
            name.string,
            self.peek(0),
        ) catch @panic("Out of memory");

        // Get the new value from stack, pop the instance and push value again
        const value: Value = self.pop();
        _ = self.pop();
        self.push(value);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_SET_INSTANCE_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        const instance_value = self.peek(1);
        const name: *ObjString = self.readString(arg);

        // Set new value
        instance_value.obj().access(
            ObjObjectInstance,
            .ObjectInstance,
            self.gc,
        ).?.setField(
            self.gc,
            name,
            self.peek(0),
        ) catch @panic("Out of memory");

        // Get the new value from stack, pop the instance and push value again
        const value: Value = self.pop();
        _ = self.pop();
        self.push(value);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_NOT(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        self.push(Value.fromBoolean(!self.pop().boolean()));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_BNOT(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const value = self.pop();

        self.push(Value.fromInteger(~(if (value.isInteger()) value.integer() else @as(i32, @intFromFloat(value.float())))));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_GREATER(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right_value = self.pop();
        const left_value = self.pop();

        const left_f: ?f64 = if (left_value.isFloat()) left_value.float() else null;
        const left_i: ?i32 = if (left_value.isInteger()) left_value.integer() else null;
        const right_f: ?f64 = if (right_value.isFloat()) right_value.float() else null;
        const right_i: ?i32 = if (right_value.isInteger()) right_value.integer() else null;

        if (left_f) |lf| {
            if (right_f) |rf| {
                self.push(Value.fromBoolean(lf > rf));
            } else {
                self.push(Value.fromBoolean(lf > @as(f64, @floatFromInt(right_i.?))));
            }
        } else {
            if (right_f) |rf| {
                self.push(Value.fromBoolean(@as(f64, @floatFromInt(left_i.?)) > rf));
            } else {
                self.push(Value.fromBoolean(left_i.? > right_i.?));
            }
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_LESS(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right_value = self.pop();
        const left_value = self.pop();

        const left_f: ?f64 = if (left_value.isFloat()) left_value.float() else null;
        const left_i: ?i32 = if (left_value.isInteger()) left_value.integer() else null;
        const right_f: ?f64 = if (right_value.isFloat()) right_value.float() else null;
        const right_i: ?i32 = if (right_value.isInteger()) right_value.integer() else null;

        if (left_f) |lf| {
            if (right_f) |rf| {
                self.push(Value.fromBoolean(lf < rf));
            } else {
                self.push(Value.fromBoolean(lf < @as(f64, @floatFromInt(right_i.?))));
            }
        } else {
            if (right_f) |rf| {
                self.push(Value.fromBoolean(@as(f64, @floatFromInt(left_i.?)) < rf));
            } else {
                self.push(Value.fromBoolean(left_i.? < right_i.?));
            }
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_ADD_STRING(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right: *ObjString = self.pop().obj().access(ObjString, .String, self.gc).?;
        const left: *ObjString = self.pop().obj().access(ObjString, .String, self.gc).?;

        self.push(Value.fromObj((left.concat(self, right) catch |e| {
            panic(e);
            unreachable;
        }).toObj()));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_ADD_LIST(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right: *ObjList = self.pop().obj().access(ObjList, .List, self.gc).?;
        const left: *ObjList = self.pop().obj().access(ObjList, .List, self.gc).?;

        var new_list = std.ArrayList(Value).init(self.gc.allocator);
        new_list.appendSlice(left.items.items) catch |e| {
            panic(e);
            unreachable;
        };
        new_list.appendSlice(right.items.items) catch |e| {
            panic(e);
            unreachable;
        };

        self.push(
            (self.gc.allocateObject(ObjList, ObjList{
                .type_def = left.type_def,
                .methods = left.methods,
                .items = new_list,
            }) catch |e| {
                panic(e);
                unreachable;
            }).toValue(),
        );

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_ADD_MAP(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right: *ObjMap = self.pop().obj().access(ObjMap, .Map, self.gc).?;
        const left: *ObjMap = self.pop().obj().access(ObjMap, .Map, self.gc).?;

        var new_map = left.map.clone() catch |e| {
            panic(e);
            unreachable;
        };
        var it = right.map.iterator();
        while (it.next()) |entry| {
            new_map.put(entry.key_ptr.*, entry.value_ptr.*) catch |e| {
                panic(e);
                unreachable;
            };
        }

        self.push(
            (self.gc.allocateObject(ObjMap, ObjMap{
                .type_def = left.type_def,
                .methods = left.methods,
                .map = new_map,
            }) catch |e| {
                panic(e);
                unreachable;
            }).toValue(),
        );

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_ADD_I(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right = self.pop().integer();
        const left = self.pop().integer();

        self.push(Value.fromInteger(left +% right));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_ADD_F(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right = self.pop().float();
        const left = self.pop().float();

        self.push(Value.fromFloat(left + right));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_SUBTRACT(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right: Value = self.pop();
        const left: Value = self.pop();

        const right_f: ?f64 = if (right.isFloat()) right.float() else null;
        const left_f: ?f64 = if (left.isFloat()) left.float() else null;
        const right_i: ?i32 = if (right.isInteger()) right.integer() else null;
        const left_i: ?i32 = if (left.isInteger()) left.integer() else null;

        if (right_f != null or left_f != null) {
            self.push(Value.fromFloat((left_f orelse @as(f64, @floatFromInt(left_i.?))) - (right_f orelse @as(f64, @floatFromInt(right_i.?)))));
        } else {
            self.push(Value.fromInteger(left_i.? -% right_i.?));
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_MULTIPLY(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right: Value = self.pop();
        const left: Value = self.pop();

        const right_f: ?f64 = if (right.isFloat()) right.float() else null;
        const left_f: ?f64 = if (left.isFloat()) left.float() else null;
        const right_i: ?i32 = if (right.isInteger()) right.integer() else null;
        const left_i: ?i32 = if (left.isInteger()) left.integer() else null;

        if (right_f != null or left_f != null) {
            self.push(Value.fromFloat((left_f orelse @as(f64, @floatFromInt(left_i.?))) * (right_f orelse @as(f64, @floatFromInt(right_i.?)))));
        } else {
            self.push(Value.fromInteger(left_i.? *% right_i.?));
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_DIVIDE(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right: Value = self.pop();
        const left: Value = self.pop();

        const right_f: ?f64 = if (right.isFloat()) right.float() else null;
        const left_f: ?f64 = if (left.isFloat()) left.float() else null;
        const right_i: ?i32 = if (right.isInteger()) right.integer() else null;
        const left_i: ?i32 = if (left.isInteger()) left.integer() else null;

        if (left_f != null or right_f != null) {
            self.push(
                Value.fromFloat(
                    (left_f orelse @as(f64, @floatFromInt(left_i.?))) / (right_f orelse @as(f64, @floatFromInt(right_i.?))),
                ),
            );
        } else {
            self.push(
                Value.fromInteger(@divTrunc(left_i.?, right_i.?)),
            );
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_MOD(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right: Value = self.pop();
        const left: Value = self.pop();

        const right_f: ?f64 = if (right.isFloat()) right.float() else null;
        const left_f: ?f64 = if (left.isFloat()) left.float() else null;
        const right_i: ?i32 = if (right.isInteger()) right.integer() else null;
        const left_i: ?i32 = if (left.isInteger()) left.integer() else null;

        if (right_f != null or left_f != null) {
            self.push(Value.fromFloat(@mod((left_f orelse @as(f64, @floatFromInt(left_i.?))), (right_f orelse @as(f64, @floatFromInt(right_i.?))))));
        } else {
            self.push(Value.fromInteger(@mod(left_i.?, right_i.?)));
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_BAND(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right: Value = self.pop();
        const left: Value = self.pop();

        const right_f: ?f64 = if (right.isFloat()) right.float() else null;
        const left_f: ?f64 = if (left.isFloat()) left.float() else null;
        const right_i: ?i32 = if (right.isInteger()) right.integer() else null;
        const left_i: ?i32 = if (left.isInteger()) left.integer() else null;

        self.push(Value.fromInteger((left_i orelse @as(i32, @intFromFloat(left_f.?))) & (right_i orelse @as(i32, @intFromFloat(right_f.?)))));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_BOR(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right: Value = self.pop();
        const left: Value = self.pop();

        const right_f: ?f64 = if (right.isFloat()) right.float() else null;
        const left_f: ?f64 = if (left.isFloat()) left.float() else null;
        const right_i: ?i32 = if (right.isInteger()) right.integer() else null;
        const left_i: ?i32 = if (left.isInteger()) left.integer() else null;

        self.push(Value.fromInteger((left_i orelse @as(i32, @intFromFloat(left_f.?))) | (right_i orelse @as(i32, @intFromFloat(right_f.?)))));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_XOR(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right: Value = self.pop();
        const left: Value = self.pop();

        const right_f: ?f64 = if (right.isFloat()) right.float() else null;
        const left_f: ?f64 = if (left.isFloat()) left.float() else null;
        const right_i: ?i32 = if (right.isInteger()) right.integer() else null;
        const left_i: ?i32 = if (left.isInteger()) left.integer() else null;
        self.push(Value.fromInteger((left_i orelse @as(i32, @intFromFloat(left_f.?))) ^ (right_i orelse @as(i32, @intFromFloat(right_f.?)))));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_SHL(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right: Value = self.pop();
        const left: Value = self.pop();

        const right_f: ?f64 = if (right.isFloat()) right.float() else null;
        const left_f: ?f64 = if (left.isFloat()) left.float() else null;
        const right_i: ?i32 = if (right.isInteger()) right.integer() else null;
        const left_i: ?i32 = if (left.isInteger()) left.integer() else null;
        const b: i32 = right_i orelse @intFromFloat(right_f.?);

        if (b < 0) {
            if (b * -1 > std.math.maxInt(u5)) {
                self.push(Value.fromInteger(0));
            } else {
                self.push(Value.fromInteger((left_i orelse @as(i32, @intFromFloat(left_f.?))) >> @as(u5, @truncate(@as(u64, @intCast(b * -1))))));
            }
        } else {
            if (b > std.math.maxInt(u5)) {
                self.push(Value.fromInteger(0));
            } else {
                self.push(Value.fromInteger((left_i orelse @as(i32, @intFromFloat(left_f.?))) << @as(u5, @truncate(@as(u64, @intCast(b))))));
            }
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_SHR(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const right: Value = self.pop();
        const left: Value = self.pop();

        const right_f: ?f64 = if (right.isFloat()) right.float() else null;
        const left_f: ?f64 = if (left.isFloat()) left.float() else null;
        const right_i: ?i32 = if (right.isInteger()) right.integer() else null;
        const left_i: ?i32 = if (left.isInteger()) left.integer() else null;
        const b: i32 = right_i orelse @intFromFloat(right_f.?);

        if (b < 0) {
            if (b * -1 > std.math.maxInt(u5)) {
                self.push(Value.fromInteger(0));
            } else {
                self.push(Value.fromInteger((left_i orelse @as(i32, @intFromFloat(left_f.?))) << @as(u5, @truncate(@as(u64, @intCast(b * -1))))));
            }
        } else {
            if (b > std.math.maxInt(u5)) {
                self.push(Value.fromInteger(0));
            } else {
                self.push(Value.fromInteger((left_i orelse @as(i32, @intFromFloat(left_f.?))) >> @as(u5, @truncate(@as(u64, @intCast(b))))));
            }
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_EQUAL(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        self.push(Value.fromBoolean(self.pop().eql(self.pop())));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_IS(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        self.push(Value.fromBoolean(self.pop().is(self.pop())));

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_JUMP(self: *Self, current_frame: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        current_frame.ip += arg;

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_JUMP_IF_FALSE(self: *Self, current_frame: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        if (!self.peek(0).boolean()) {
            current_frame.ip += arg;
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_JUMP_IF_NOT_NULL(self: *Self, current_frame: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        if (!self.peek(0).isNull()) {
            current_frame.ip += arg;
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_LOOP(self: *Self, current_frame: *CallFrame, _: u32, _: OpCode, arg: u24) void {
        current_frame.ip -= arg;

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_STRING_FOREACH(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const key_slot: *Value = @ptrCast(self.current_fiber.stack_top - 3);
        const value_slot: *Value = @ptrCast(self.current_fiber.stack_top - 2);
        const str: *ObjString = self.peek(0).obj().access(ObjString, .String, self.gc).?;

        key_slot.* = if (str.next(self, if (key_slot.*.isNull()) null else key_slot.integer()) catch |e| {
            panic(e);
            unreachable;
        }) |new_index|
            Value.fromInteger(new_index)
        else
            Value.Null;

        // Set new value
        if (key_slot.*.isInteger()) {
            value_slot.* = (self.gc.copyString(&([_]u8{str.string[@as(usize, @intCast(key_slot.integer()))]})) catch |e| {
                panic(e);
                unreachable;
            }).toValue();
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_LIST_FOREACH(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        var key_slot: *Value = @ptrCast(self.current_fiber.stack_top - 3);
        const value_slot: *Value = @ptrCast(self.current_fiber.stack_top - 2);
        var list = self.peek(0).obj().access(ObjList, .List, self.gc).?;

        // Get next index
        key_slot.* = if (list.rawNext(
            self,
            if (key_slot.*.isNull()) null else key_slot.integer(),
        ) catch |e| {
            panic(e);
            unreachable;
        }) |new_index|
            Value.fromInteger(new_index)
        else
            Value.Null;

        // Set new value
        if (key_slot.*.isInteger()) {
            value_slot.* = list.items.items[@as(usize, @intCast(key_slot.integer()))];
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_ENUM_FOREACH(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        var value_slot: *Value = @ptrCast(self.current_fiber.stack_top - 2);
        const enum_case = if (value_slot.*.isNull())
            null
        else
            value_slot.obj().access(ObjEnumInstance, .EnumInstance, self.gc).?;
        var enum_: *ObjEnum = self.peek(0).obj().access(ObjEnum, .Enum, self.gc).?;

        // Get next enum case
        const next_case = enum_.rawNext(self, enum_case) catch |e| {
            panic(e);
            unreachable;
        };
        value_slot.* = (if (next_case) |new_case| Value.fromObj(new_case.toObj()) else Value.Null);

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_MAP_FOREACH(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const key_slot: *Value = @ptrCast(self.current_fiber.stack_top - 3);
        const value_slot: *Value = @ptrCast(self.current_fiber.stack_top - 2);
        var map: *ObjMap = self.peek(0).obj().access(ObjMap, .Map, self.gc).?;
        const current_key = if (!key_slot.*.isNull()) key_slot.* else null;

        const next_key = map.rawNext(current_key);
        key_slot.* = if (next_key) |unext_key| unext_key else Value.Null;

        if (next_key) |unext_key| {
            value_slot.* = map.map.get(unext_key) orelse Value.Null;
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_FIBER_FOREACH(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        const value_slot: *Value = @ptrCast(self.current_fiber.stack_top - 2);
        var fiber = self.peek(0).obj().access(ObjFiber, .Fiber, self.gc).?;

        if (fiber.fiber.status == .Over) {
            value_slot.* = Value.Null;
        } else {
            fiber.fiber.@"resume"(self) catch |e| {
                panic(e);
                unreachable;
            };
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_UNWRAP(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        if (self.peek(0).isNull()) {
            // TODO: Should we throw or @panic?
            self.throw(
                Error.UnwrappedNull,
                (self.gc.copyString("Force unwrapped optional is null") catch |e| {
                    panic(e);
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |e| {
                panic(e);
                unreachable;
            };
        }

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    fn OP_TYPEOF(self: *Self, _: *CallFrame, _: u32, _: OpCode, _: u24) void {
        self.push((Value.typeOf(self.pop(), self.gc) catch @panic("Out of memory")).toValue());

        const next_full_instruction: u32 = self.readInstruction();
        @call(
            dispatch_call_modifier,
            dispatch,
            .{
                self,
                self.currentFrame().?,
                next_full_instruction,
                getCode(next_full_instruction),
                getArg(next_full_instruction),
            },
        );
    }

    pub fn run(self: *Self) void {
        const next_current_frame: *CallFrame = self.currentFrame().?;
        const next_full_instruction: u32 = self.readInstruction();
        const next_instruction: OpCode = getCode(next_full_instruction);
        const next_arg: u24 = getArg(next_full_instruction);

        if (BuildOptions.debug_current_instruction) {
            std.debug.print(
                "{}: {}\n",
                .{
                    next_current_frame.ip,
                    next_instruction,
                },
            );
        }

        if (BuildOptions.gc_debug) {
            self.gc.where = if (self.currentFrame()) |current_frame|
                current_frame.closure.function.chunk.lines.items[current_frame.ip - 1]
            else
                null;
        }

        op_table[@intFromEnum(next_instruction)](
            self,
            next_current_frame,
            next_full_instruction,
            next_instruction,
            next_arg,
        );
    }

    pub fn throw(self: *Self, code: Error, payload: Value, previous_stack: ?*std.ArrayList(CallFrame), previous_error_site: ?Ast.TokenIndex) Error!void {
        var stack = if (previous_stack) |pstack|
            pstack.*
        else
            std.ArrayList(CallFrame).init(self.gc.allocator);
        defer {
            if (previous_stack == null) {
                stack.deinit();
            }
        }

        const error_site = if (previous_error_site) |perror_site|
            perror_site
        else if (self.currentFrame()) |current_frame|
            current_frame.closure.function.chunk.lines.items[current_frame.ip - 1]
        else
            null;

        while (self.current_fiber.frame_count > 0 or self.current_fiber.parent_fiber != null) {
            const frame = self.currentFrame();
            if (self.current_fiber.frame_count > 0) {
                const function_type = frame.?.closure.function.type_def.resolved_type.?.Function.function_type;
                if (function_type != .ScriptEntryPoint and function_type != .Repl) {
                    try stack.append(frame.?.*);
                }

                // Are we in a try-catch?
                if (frame.?.try_ip) |try_ip| {
                    // Push error and jump to start of the catch clauses
                    self.push(payload);

                    frame.?.ip = try_ip;

                    return;
                }

                // Pop frame
                self.closeUpValues(&frame.?.slots[0]);
                self.current_fiber.frame_count -= 1;
                _ = self.current_fiber.frames.pop();
            }

            if (self.current_fiber.frame_count == 0 and self.current_fiber.parent_fiber == null) {
                // No more frames, the error is uncaught.
                _ = self.pop();

                // Raise the runtime error
                // If object instance, does it have a str `message` field ?
                var processed_payload = payload;
                if (payload.isObj()) {
                    if (payload.obj().access(ObjObjectInstance, .ObjectInstance, self.gc)) |instance| {
                        processed_payload = instance.fields.get(try self.gc.copyString("message")) orelse payload;
                    }
                }

                const value_str = try processed_payload.toStringAlloc(self.gc.allocator);
                defer value_str.deinit();

                self.reportRuntimeError(
                    value_str.items,
                    error_site,
                    stack.items,
                );

                if (!is_wasm) {
                    std.os.exit(1);
                } else {
                    return Error.RuntimeError;
                }
            } else if (self.current_fiber.frame_count == 0) {
                // Error raised inside a fiber, forward it to parent fiber
                self.current_fiber = self.current_fiber.parent_fiber.?;

                try self.throw(
                    code,
                    payload,
                    &stack,
                    error_site,
                );

                return;
            }

            if (frame != null) {
                self.current_fiber.stack_top = frame.?.slots;

                if (frame.?.error_value) |error_value| {
                    // Push error_value as failed function return value
                    self.push(error_value);

                    return;
                }
            }
        }
    }

    fn reportRuntimeError(self: *Self, message: []const u8, error_site: ?Ast.TokenIndex, stack: []const CallFrame) void {
        var notes = std.ArrayList(Reporter.Note).init(self.gc.allocator);
        defer {
            for (notes.items) |note| {
                self.gc.allocator.free(note.message);
            }
            notes.deinit();
        }

        for (stack, 0..) |frame, i| {
            const next = if (i < stack.len - 1) stack[i + 1] else null;
            var msg = std.ArrayList(u8).init(self.gc.allocator);
            defer msg.shrinkAndFree(msg.items.len);
            var writer = msg.writer();

            if (next) |unext| {
                writer.print(
                    "\t{s} in \x1b[36m{s}\x1b[0m at {s}",
                    .{
                        if (i == 0)
                            "╰─┬─"
                        else if (i < stack.len - 1)
                            "  ├─"
                        else
                            "  ╰─",
                        if (unext.closure.function.type_def.resolved_type.?.Function.function_type == .Test)
                            unext.closure.function.name.string[(std.mem.indexOfScalar(u8, unext.closure.function.name.string, ' ').? + 1)..]
                        else
                            unext.closure.function.name.string,
                        if (frame.call_site) |call_site|
                            self.current_ast.tokens.items(.script_name)[call_site]
                        else
                            frame.closure.function.type_def.resolved_type.?.Function.script_name.string,
                    },
                ) catch @panic("Could not report error");
            } else {
                writer.print(
                    "\t{s} in {s}",
                    .{
                        if (i == 0)
                            "╰─┬─"
                        else if (i < stack.len - 1)
                            "  ├─"
                        else
                            "  ╰─",
                        if (frame.call_site) |call_site|
                            self.current_ast.tokens.items(.script_name)[call_site]
                        else
                            frame.closure.function.type_def.resolved_type.?.Function.script_name.string,
                    },
                ) catch @panic("Could not report error");
            }

            if (frame.call_site) |call_site| {
                writer.print(
                    ":{d}:{d}",
                    .{
                        self.current_ast.tokens.items(.line)[call_site] + 1,
                        self.current_ast.tokens.items(.column)[call_site],
                    },
                ) catch @panic("Could not report error");
            }

            notes.append(
                Reporter.Note{
                    .message = msg.items,
                    .show_prefix = false,
                },
            ) catch @panic("Could not report error");
        }

        var err_report = Reporter.Report{
            .message = message,
            .error_type = .runtime,
            .items = &[_]Reporter.ReportItem{
                .{
                    .location = self.current_ast.tokens.get(error_site orelse stack[0].call_site.?),
                    .kind = .@"error",
                    .message = message,
                },
            },
            .notes = notes.items,
        };

        err_report.reportStderr(&self.reporter) catch @panic("Could not report error");
    }

    fn compileAndCall(self: *Self, closure: *ObjClosure, arg_count: u8, catch_value: ?Value) Error!bool {
        var native = closure.function.native;
        if (self.jit) |*jit| {
            jit.call_count += 1;
            // Do we need to jit the function?
            // TODO: figure out threshold strategy
            if (self.shouldCompileFunction(closure)) {
                var timer = if (!is_wasm) std.time.Timer.start() catch unreachable else {};

                var success = true;
                jit.compileFunction(self.current_ast, closure) catch |err| {
                    if (err == Error.CantCompile) {
                        success = false;
                    } else {
                        return err;
                    }
                };

                if (BuildOptions.jit_debug and success) {
                    std.debug.print(
                        "Compiled function `{s}` in {d} ms\n",
                        .{
                            closure.function.type_def.resolved_type.?.Function.name.string,
                            @as(f64, @floatFromInt(timer.read())) / 1000000,
                        },
                    );
                }

                jit.jit_time += timer.read();

                if (success) {
                    native = closure.function.native;
                }
            }
        }

        // Is there a compiled version of it?
        if (native != null) {
            if (BuildOptions.jit_debug) {
                std.debug.print("Calling compiled version of function `{s}`\n", .{closure.function.name.string});
            }

            try self.callCompiled(
                closure,
                @ptrCast(@alignCast(native.?)),
                arg_count,
                catch_value,
            );

            return true;
        }

        return false;
    }

    fn call(self: *Self, closure: *ObjClosure, arg_count: u8, catch_value: ?Value, in_fiber: bool) Error!void {
        closure.function.call_count += 1;

        if (BuildOptions.recursive_call_limit) |recursive_call_limit| {
            // If recursive call, update counter
            self.current_fiber.recursive_count = if (self.currentFrame() != null and self.currentFrame().?.closure.function == closure.function)
                self.current_fiber.recursive_count + 1
            else
                0;

            if (self.current_fiber.recursive_count > recursive_call_limit) {
                try self.throw(
                    VM.Error.ReachedMaximumRecursiveCall,
                    (try self.gc.copyString("Maximum recursive call reached")).toValue(),
                    null,
                    null,
                );

                return;
            }
        }

        // Is or will be JIT compiled, call and stop there
        if (!is_wasm and !in_fiber and try self.compileAndCall(closure, arg_count, catch_value)) {
            return;
        }

        if (self.flavor == .Test and closure.function.type_def.resolved_type.?.Function.function_type == .Test) {
            std.debug.print(
                "\x1b[33m▶ Test: {s}\x1b[0m\n",
                .{
                    self.current_ast.tokens.items(.lexeme)[self.current_ast.nodes.items(.components)[closure.function.node].Function.test_message.?],
                },
            );
        }

        const frame = CallFrame{
            .closure = closure,
            .ip = 0,
            // -1 is because we reserve slot 0 for this
            .slots = self.current_fiber.stack_top - arg_count - 1,
            .call_site = self.getSite(),
            .error_value = catch_value,
        };

        if (self.current_fiber.frames.items.len <= self.current_fiber.frame_count) {
            try self.current_fiber.frames.append(frame);
        } else {
            self.current_fiber.frames.items[self.current_fiber.frame_count] = frame;
        }

        self.current_fiber.frame_count += 1;

        if (BuildOptions.jit_debug) {
            std.debug.print("Calling uncompiled version of function `{s}`\n", .{closure.function.name.string});
        }
    }

    fn getSite(self: *Self) ?Ast.TokenIndex {
        return if (self.currentFrame()) |current_frame|
            current_frame.closure.function.chunk.lines.items[@max(1, current_frame.ip) - 1]
        else if (self.current_fiber.parent_fiber) |parent_fiber| parent: {
            const parent_frame = parent_fiber.frames.items[parent_fiber.frame_count - 1];

            break :parent parent_frame.closure.function.chunk.lines.items[@max(1, parent_frame.ip - 1)];
        } else null;
    }

    fn callNative(self: *Self, closure: ?*ObjClosure, native: NativeFn, arg_count: u8, catch_value: ?Value) !void {
        const was_in_native_call = self.currentFrame().?.in_native_call;
        self.currentFrame().?.in_native_call = true;
        self.currentFrame().?.native_call_error_value = catch_value;

        var result: Value = Value.Null;
        var ctx = NativeCtx{
            .vm = self,
            .globals = if (closure) |uclosure| uclosure.globals.items.ptr else &[_]Value{},
            .upvalues = if (closure) |uclosure| uclosure.upvalues.items.ptr else &[_]*ObjUpValue{},
            .base = self.current_fiber.stack_top - arg_count - 1,
            .stack_top = &self.current_fiber.stack_top,
        };
        const native_return = native(&ctx);

        self.currentFrame().?.in_native_call = was_in_native_call;
        self.currentFrame().?.native_call_error_value = null;

        if (native_return == 1 or native_return == 0) {
            if (native_return == 1) {
                result = self.pop();
            }

            self.current_fiber.stack_top = self.current_fiber.stack_top - arg_count - 1;
            self.push(result);
        } else {
            // An error occured within the native function -> call error handlers
            if (catch_value != null) {
                // We discard the error
                _ = self.pop();

                // Default value in case of error
                self.current_fiber.stack_top = self.current_fiber.stack_top - arg_count - 1;
                self.push(catch_value.?);
                return;
            }

            // Error was not handled are we in a try-catch ?
            var frame = self.currentFrame().?;
            if (frame.try_ip) |try_ip| {
                frame.ip = try_ip;
            } else {
                // No error handler or default value was triggered so forward the error
                try self.throw(
                    Error.Custom,
                    self.peek(0),
                    null,
                    null,
                );
            }
        }
    }

    // A JIT compiled function pops its stack on its own
    fn callCompiled(self: *Self, closure: *ObjClosure, native: NativeFn, arg_count: u8, catch_value: ?Value) !void {
        const was_in_native_call = self.currentFrame() != null and self.currentFrame().?.in_native_call;
        if (self.currentFrame()) |frame| {
            frame.in_native_call = true;
            frame.native_call_error_value = catch_value;
        }

        var ctx = NativeCtx{
            .vm = self,
            .globals = closure.globals.items.ptr,
            .upvalues = closure.upvalues.items.ptr,
            .base = self.current_fiber.stack_top - arg_count - 1,
            .stack_top = &self.current_fiber.stack_top,
        };
        const native_return = native(&ctx);

        if (self.currentFrame()) |frame| {
            frame.in_native_call = was_in_native_call;
            frame.native_call_error_value = null;
        }

        if (native_return == -1) {
            // An error occured within the native function -> call error handlers
            if (catch_value != null) {
                // We discard the error
                _ = self.pop();

                // Default value in case of error
                self.push(catch_value.?);
                return;
            }

            // Error was not handled are we in a try-catch ?
            if (self.currentFrame() != null and self.currentFrame().?.try_ip != null) {
                self.currentFrame().?.ip = self.currentFrame().?.try_ip.?;
            } else {
                // No error handler or default value was triggered so forward the error
                try self.throw(
                    Error.Custom,
                    self.peek(0),
                    null,
                    null,
                );
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
        self.push(Value.fromObj(bound.toObj()));
    }

    pub fn callValue(self: *Self, callee: Value, arg_count: u8, catch_value: ?Value, in_fiber: bool) Error!void {
        var obj: *Obj = callee.obj();
        switch (obj.obj_type) {
            .Bound => {
                const bound = obj.access(ObjBoundMethod, .Bound, self.gc).?;
                (self.current_fiber.stack_top - arg_count - 1)[0] = bound.receiver;

                if (bound.closure) |closure| {
                    return try self.call(
                        closure,
                        arg_count,
                        catch_value,
                        in_fiber,
                    );
                } else {
                    assert(bound.native != null);
                    return try self.callNative(
                        null,
                        @ptrCast(@alignCast(bound.native.?.native)),
                        arg_count,
                        catch_value,
                    );
                }
            },
            .Closure => {
                return try self.call(
                    obj.access(ObjClosure, .Closure, self.gc).?,
                    arg_count,
                    catch_value,
                    in_fiber,
                );
            },
            .Native => {
                return try self.callNative(
                    null,
                    @ptrCast(@alignCast(obj.access(ObjNative, .Native, self.gc).?.native)),
                    arg_count,
                    catch_value,
                );
            },
            else => {
                unreachable;
            },
        }
    }

    fn tailCall(self: *Self, callee: Value, arg_count: u8, catch_value: ?Value, in_fiber: bool) Error!void {
        var obj: *Obj = callee.obj();
        switch (obj.obj_type) {
            .Bound => {
                const bound = obj.access(ObjBoundMethod, .Bound, self.gc).?;
                (self.current_fiber.stack_top - arg_count - 1)[0] = bound.receiver;

                if (bound.closure) |closure| {
                    return try self.repurposeFrame(
                        closure,
                        arg_count,
                        catch_value,
                        in_fiber,
                    );
                } else {
                    assert(bound.native != null);
                    return try self.callNative(
                        null,
                        @ptrCast(@alignCast(bound.native.?.native)),
                        arg_count,
                        catch_value,
                    );
                }
            },
            .Closure => {
                return try self.repurposeFrame(
                    obj.access(ObjClosure, .Closure, self.gc).?,
                    arg_count,
                    catch_value,
                    in_fiber,
                );
            },
            .Native => {
                return try self.callNative(
                    null,
                    @ptrCast(@alignCast(obj.access(ObjNative, .Native, self.gc).?.native)),
                    arg_count,
                    catch_value,
                );
            },
            else => {
                unreachable;
            },
        }
    }

    fn invokeFromObject(
        self: *Self,
        object: *ObjObject,
        name: *ObjString,
        arg_count: u8,
        catch_value: ?Value,
        in_fiber: bool,
        tail_call: bool,
    ) !Value {
        if (object.methods.get(name)) |method| {
            if (tail_call)
                try self.tailCall(method.toValue(), arg_count, catch_value, in_fiber)
            else
                try self.call(method, arg_count, catch_value, in_fiber);

            return method.toValue();
        }

        unreachable;
    }

    // FIXME: find way to remove
    pub fn invoke(
        self: *Self,
        name: *ObjString,
        arg_count: u8,
        catch_value: ?Value,
        in_fiber: bool,
        tail_call: bool,
    ) !Value {
        var receiver: Value = self.peek(arg_count);

        var obj: *Obj = receiver.obj();
        switch (obj.obj_type) {
            .ObjectInstance => {
                const instance = obj.access(ObjObjectInstance, .ObjectInstance, self.gc).?;

                assert(instance.object != null);

                if (instance.fields.get(name)) |field| {
                    (self.current_fiber.stack_top - arg_count - 1)[0] = field;

                    if (tail_call)
                        try self.tailCall(field, arg_count, catch_value, in_fiber)
                    else
                        try self.callValue(field, arg_count, catch_value, in_fiber);

                    return field;
                }

                return try self.invokeFromObject(
                    instance.object.?,
                    name,
                    arg_count,
                    catch_value,
                    in_fiber,
                    tail_call,
                );
            },
            .String => {
                if (try ObjString.member(self, name)) |member| {
                    const member_value = member.toValue();
                    (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;

                    try self.callValue(member_value, arg_count, catch_value, in_fiber);

                    return member_value;
                }

                unreachable;
            },
            .Pattern => {
                if (try ObjPattern.member(self, name)) |member| {
                    const member_value = member.toValue();
                    (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;

                    try self.callValue(member_value, arg_count, catch_value, in_fiber);

                    return member_value;
                }

                unreachable;
            },
            .Fiber => {
                if (try ObjFiber.member(self, name)) |member| {
                    const member_value = member.toValue();
                    (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;

                    try self.callValue(member_value, arg_count, catch_value, in_fiber);

                    return member_value;
                }

                unreachable;
            },
            .List => {
                const list = obj.access(ObjList, .List, self.gc).?;

                if (try list.member(self, name)) |member| {
                    const member_value = member.toValue();
                    (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;

                    try self.callValue(member_value, arg_count, catch_value, in_fiber);

                    return member_value;
                }

                unreachable;
            },
            .Map => {
                const map = obj.access(ObjMap, .Map, self.gc).?;

                if (try map.member(self, name)) |member| {
                    const member_value = member.toValue();
                    (self.current_fiber.stack_top - arg_count - 1)[0] = member_value;

                    try self.callValue(member_value, arg_count, catch_value, in_fiber);

                    return member_value;
                }

                unreachable;
            },
            else => unreachable,
        }
    }

    pub fn closeUpValues(self: *Self, last: *Value) void {
        while (self.current_fiber.open_upvalues != null and @intFromPtr(self.current_fiber.open_upvalues.?.location) >= @intFromPtr(last)) {
            var upvalue: *ObjUpValue = self.current_fiber.open_upvalues.?;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed.?;
            self.current_fiber.open_upvalues = upvalue.next;
        }
    }

    pub fn captureUpvalue(self: *Self, local: *Value) !*ObjUpValue {
        var prev_upvalue: ?*ObjUpValue = null;
        var upvalue: ?*ObjUpValue = self.current_fiber.open_upvalues;
        while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
            prev_upvalue = upvalue;
            upvalue = upvalue.?.next;
        }

        if (upvalue != null and upvalue.?.location == local) {
            return upvalue.?;
        }

        var created_upvalue: *ObjUpValue = try self.gc.allocateObject(
            ObjUpValue,
            ObjUpValue.init(local),
        );
        created_upvalue.next = upvalue;

        if (prev_upvalue) |uprev_upvalue| {
            uprev_upvalue.next = created_upvalue;
        } else {
            self.current_fiber.open_upvalues = created_upvalue;
        }

        return created_upvalue;
    }

    fn shouldCompileFunction(self: *Self, closure: *ObjClosure) bool {
        const function_type = closure.function.type_def.resolved_type.?.Function.function_type;

        if (function_type == .Extern or function_type == .Script or function_type == .ScriptEntryPoint or function_type == .EntryPoint or function_type == .Repl) {
            return false;
        }

        if (self.jit != null and (self.jit.?.compiled_closures.get(closure) != null or self.jit.?.blacklisted_closures.get(closure) != null)) {
            return false;
        }

        const user_hot = if (self.current_ast.nodes.items(.components)[closure.function.node].Function.docblock) |docblock|
            std.mem.indexOf(u8, self.current_ast.tokens.items(.lexeme)[docblock], "@hot") != null
        else
            false;

        return if (BuildOptions.jit_always_on)
            return true
        else
            user_hot or
                (closure.function.call_count > 10 and
                (@as(f128, @floatFromInt(closure.function.call_count)) / @as(f128, @floatFromInt(self.jit.?.call_count))) > BuildOptions.jit_prof_threshold);
    }
};
