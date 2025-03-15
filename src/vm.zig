const std = @import("std");
const builtin = @import("builtin");
const v = @import("value.zig");
const Value = v.Value;
const Chunk = @import("Chunk.zig");
const Ast = @import("Ast.zig");
const disassembler = @import("disassembler.zig");
const obj = @import("obj.zig");
const BuildOptions = @import("build_options");
const memory = @import("memory.zig");
const is_wasm = builtin.cpu.arch.isWasm();
const JIT = if (!is_wasm) @import("Jit.zig") else void;
const Token = @import("Token.zig");
const Reporter = @import("Reporter.zig");
const FFI = if (!is_wasm) @import("FFI.zig") else void;
const dispatch_call_modifier: std.builtin.CallModifier = if (!is_wasm) .always_tail else .auto;
const io = @import("io.zig");

const dumpStack = disassembler.dumpStack;
const jmp = if (!is_wasm) @import("jmp.zig") else void;

pub const ImportRegistry = std.AutoHashMapUnmanaged(*obj.ObjString, []const Value);

pub const RunFlavor = enum {
    Run,
    Test,
    Check,
    Fmt,
    Ast,
    Repl,

    pub inline fn resolveImports(self: RunFlavor) bool {
        return switch (self) {
            .Check, .Fmt => false,
            else => true,
        };
    }
};

pub const CallFrame = struct {
    const Self = @This();

    closure: *obj.ObjClosure,
    /// Index into closure's chunk
    ip: usize,
    // Frame
    slots: [*]Value,

    /// Default value in case of error
    error_value: ?Value = null,

    /// Line in source code where the call occured
    call_site: ?Ast.TokenIndex,

    /// Offset at which error can be handled (means we're in a try block)
    try_ip: ?usize = null,
    /// Top when try block started
    try_top: ?[*]Value = null,

    /// True if a native function is being called, we need this because a native function can also
    /// call buzz code and we need to know how to stop interpreting once we get back to native code
    in_native_call: bool = false,
    native_call_error_value: ?Value = null,
};

pub const TryCtx = if (!is_wasm)
    extern struct {
        previous: ?*TryCtx,
        env: jmp.jmp_buf = undefined,
    }
else
    void;

pub const Fiber = struct {
    const Self = @This();

    pub const Status = enum {
        /// Just created, never started
        Instanciated,
        /// Currently running
        Running,
        /// Yielded an expected value
        Yielded,
        /// Reached return statement
        Over,
    };

    allocator: std.mem.Allocator,

    parent_fiber: ?*Fiber,

    /// Instruction(s) that triggered the fiber
    instruction: u32,
    extra_instruction: ?u32,

    frames: std.ArrayListUnmanaged(CallFrame),
    // FIXME: this is useless since we actually pop items from the frames list
    frame_count: usize = 0,
    recursive_count: u32 = 0,
    current_compiled_function: ?*obj.ObjFunction = null,

    stack: []Value,
    stack_top: [*]Value,
    open_upvalues: ?*obj.ObjUpValue,

    status: Status = .Instanciated,
    /// true: we did `resolve fiber`, false: we did `resume fiber`
    resolved: bool = false,

    /// When within a try catch in a JIT compiled function
    try_context: ?*TryCtx = null,

    type_def: *obj.ObjTypeDef,

    pub fn init(
        allocator: std.mem.Allocator,
        type_def: *obj.ObjTypeDef,
        parent_fiber: ?*Fiber,
        stack_slice: ?[]Value,
        instruction: u32,
        extra_instruction: ?u32,
    ) !Self {
        var self: Self = .{
            .allocator = allocator,
            .type_def = type_def,
            .parent_fiber = parent_fiber,
            .stack = try allocator.alloc(Value, BuildOptions.stack_size),
            .stack_top = undefined,
            .frames = std.ArrayListUnmanaged(CallFrame){},
            .open_upvalues = null,
            .instruction = instruction,
            .extra_instruction = extra_instruction,
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

        self.frames.deinit(self.allocator);
    }

    // FIXME: we replicate here what opcodes do, would be easier to call the opcodes themselves but they assume
    // there's a current frame with an active chunk
    pub fn start(self: *Self, vm: *VM) !void {
        std.debug.assert(self.status == .Instanciated);

        vm.current_fiber = self;

        const arg_count: u8 = if (self.extra_instruction) |extra|
            @intCast(extra >> 24)
        else
            @intCast((0x00ffffff & self.instruction) >> 16);

        const catch_count: u16 = if (self.extra_instruction) |extra|
            @intCast(0x00ffffff & extra)
        else
            @intCast(0x0000ffff & self.instruction);

        const catch_value = if (catch_count > 1)
            vm.pop()
        else
            null;

        switch (VM.getCode(self.instruction)) {
            .OP_TAIL_CALL, .OP_CALL => try vm.callValue(
                vm.peek(arg_count),
                arg_count,
                catch_value,
            ),
            .OP_CALL_INSTANCE_PROPERTY, .OP_TAIL_CALL_INSTANCE_PROPERTY => vm.callInstanceProperty(
                self.extra_instruction.?,
                VM.getArg(self.instruction),
                false,
            ),
            .OP_INSTANCE_INVOKE, .OP_INSTANCE_TAIL_INVOKE => {
                const instance: *obj.ObjObjectInstance = vm.peek(arg_count).obj()
                    .access(obj.ObjObjectInstance, .ObjectInstance, vm.gc).?;

                std.debug.assert(instance.object != null);

                _ = try vm.invokeFromObject(
                    instance.object.?,
                    VM.getArg(self.instruction),
                    arg_count,
                    catch_value,
                    false,
                );
            },
            .OP_PROTOCOL_INVOKE, .OP_PROTOCOL_TAIL_INVOKE => {
                const name = vm.readConstant(VM.getArg(self.instruction))
                    .obj().access(obj.ObjString, .String, vm.gc).?
                    .string;

                const instance = vm.peek(arg_count).obj()
                    .access(obj.ObjObjectInstance, .ObjectInstance, vm.gc).?;

                std.debug.assert(instance.object != null);

                // Find the actual field
                const property_idx = instance.type_def.resolved_type.?.ObjectInstance.of
                    .resolved_type.?.Object
                    .fields.get(name).?.index;

                _ = try vm.invokeFromObject(
                    instance.object.?,
                    property_idx,
                    arg_count,
                    catch_value,
                    false,
                );
            },
            .OP_MAP_INVOKE => {
                const map = vm.peek(arg_count).obj().access(obj.ObjMap, .Map, vm.gc).?;
                const member = try map.member(vm, VM.getArg(self.instruction));

                (self.stack_top - arg_count - 1)[0] = member;
                try vm.callValue(
                    member,
                    arg_count,
                    catch_value,
                );
            },
            .OP_LIST_INVOKE => {
                const list = vm.peek(arg_count).obj().access(obj.ObjList, .List, vm.gc).?;
                const member = try list.member(vm, VM.getArg(self.instruction));

                (self.stack_top - arg_count - 1)[0] = member;
                try vm.callValue(
                    member,
                    arg_count,
                    catch_value,
                );
            },
            .OP_RANGE_INVOKE => {
                const member = try obj.ObjRange.member(vm, VM.getArg(self.instruction));

                (self.stack_top - arg_count - 1)[0] = member;
                try vm.callValue(
                    member,
                    arg_count,
                    catch_value,
                );
            },
            .OP_STRING_INVOKE => {
                const member = try obj.ObjString.member(vm, VM.getArg(self.instruction));

                (self.stack_top - arg_count - 1)[0] = member;
                try vm.callValue(
                    member,
                    arg_count,
                    catch_value,
                );
            },
            .OP_PATTERN_INVOKE => {
                const member = try obj.ObjPattern.member(vm, VM.getArg(self.instruction));

                (self.stack_top - arg_count - 1)[0] = member;
                try vm.callValue(
                    member,
                    arg_count,
                    catch_value,
                );
            },
            else => unreachable,
        }

        self.status = .Running;
    }

    pub fn yield(self: *Self, vm: *VM) void {
        std.debug.assert(self.status == .Running);

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

    pub fn resolve(self: *Self, vm: *VM) !void {
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

    pub fn finish(self: *Self, vm: *VM, result: Value) void {
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
    } || std.mem.Allocator.Error || std.fmt.BufPrintError;

    gc: *memory.GarbageCollector,
    current_fiber: *Fiber,
    current_ast: Ast.Slice,
    globals: std.ArrayListUnmanaged(Value) = .{},
    global_names: std.AutoArrayHashMap(u24, []const u8),
    // FIXME: remove
    globals_count: usize = 0,
    import_registry: *ImportRegistry,
    jit: ?JIT = null,
    hotspots_count: u128 = 0,
    flavor: RunFlavor,
    reporter: Reporter,
    ffi: FFI,

    pub fn init(gc: *memory.GarbageCollector, import_registry: *ImportRegistry, flavor: RunFlavor) !Self {
        return .{
            .gc = gc,
            .global_names = std.AutoArrayHashMap(u24, []const u8).init(gc.allocator),
            .import_registry = import_registry,
            .current_ast = undefined,
            .current_fiber = undefined,
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
        // self.global_names.deinit();
        if (!is_wasm) {
            self.ffi.deinit();
        }
    }

    pub fn cliArgs(self: *Self, args: ?[]const []const u8) !*obj.ObjList {
        var arg_list = try self.gc.allocateObject(
            obj.ObjList,
            try obj.ObjList.init(
                self.gc.allocator,
                // TODO: get instance that already exists
                try self.gc.allocateObject(
                    obj.ObjTypeDef,
                    .{
                        .def_type = .List,
                        .optional = false,
                        .resolved_type = .{
                            .List = obj.ObjList.ListDef.init(
                                self.gc.type_registry.str_type,
                                false,
                            ),
                        },
                    },
                ),
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
                    self.gc.allocator,
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

    pub fn discard(self: *Self, n: usize) void {
        self.current_fiber.stack_top -= n;
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
        return if (value.isObj())
            try obj.cloneObject(value.obj(), self)
        else
            value;
    }

    pub inline fn currentFrame(self: *Self) ?*CallFrame {
        if (self.current_fiber.frame_count == 0) {
            return null;
        }

        return &self.current_fiber.frames.items[self.current_fiber.frame_count - 1];
    }

    pub inline fn currentGlobals(self: *Self) *std.ArrayListUnmanaged(Value) {
        return self.currentFrame().?.closure.globals;
    }

    pub fn interpret(self: *Self, ast: Ast.Slice, function: *obj.ObjFunction, args: ?[]const []const u8) Error!void {
        self.current_ast = ast;

        self.current_fiber = try self.gc.allocator.create(Fiber);
        self.current_fiber.* = try Fiber.init(
            self.gc.allocator,
            try self.gc.type_registry.getTypeDef(
                .{
                    .optional = false,
                    .def_type = .Fiber,
                    .resolved_type = .{
                        .Fiber = .{
                            .return_type = self.gc.type_registry.void_type,
                            .yield_type = self.gc.type_registry.void_type,
                        },
                    },
                },
            ),
            null,
            null,
            // Those don't matter for the main fiber
            undefined,
            null,
        );

        self.push((try self.gc.allocateObject(
            obj.ObjClosure,
            try obj.ObjClosure.init(self.gc.allocator, self, function),
        )).toValue());

        self.push((try self.cliArgs(args)).toValue());

        try self.gc.registerVM(self);
        defer self.gc.unregisterVM(self);

        try self.callValue(
            self.peek(1),
            0,
            null,
        );

        self.current_fiber.status = .Running;

        return self.run();
    }

    fn readPreviousInstruction(self: *Self) ?u32 {
        const current_frame = self.currentFrame().?;

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

    pub inline fn getCode(instruction: u32) Chunk.OpCode {
        return @enumFromInt(@as(u8, @intCast(instruction >> 24)));
    }

    inline fn replaceCode(instruction: u32, new_code: Chunk.OpCode) u32 {
        return (@as(u32, @intCast(@intFromEnum(new_code))) << 24) | @as(u32, @intCast(getArg(instruction)));
    }

    inline fn getArg(instruction: u32) u24 {
        return @as(u24, @intCast(0x00ffffff & instruction));
    }

    inline fn readByte(self: *Self) u8 {
        return @as(u8, @intCast(self.readInstruction()));
    }

    pub inline fn readConstant(self: *Self, arg: u24) Value {
        return self.currentFrame().?.closure.function.chunk.constants.items[arg];
    }

    inline fn readString(self: *Self, arg: u24) *obj.ObjString {
        return self.readConstant(arg).obj().access(
            obj.ObjString,
            .String,
            self.gc,
        ).?;
    }

    const OpFn = *const fn (*Self, *CallFrame, u32, Chunk.OpCode, u24) void;

    // WARNING: same order as Chunk.OpCode enum
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
        OP_SUBTRACT_I,
        OP_SUBTRACT_F,
        OP_MULTIPLY_I,
        OP_MULTIPLY_F,
        OP_DIVIDE_I,
        OP_DIVIDE_F,
        OP_MOD_I,
        OP_MOD_F,
        OP_BNOT,
        OP_BAND,
        OP_BOR,
        OP_XOR,
        OP_SHL,
        OP_SHR,

        OP_UNWRAP,

        OP_NOT,
        OP_NEGATE_I,
        OP_NEGATE_F,

        OP_SWAP,
        OP_JUMP,
        OP_JUMP_IF_FALSE,
        OP_JUMP_IF_NOT_NULL,
        OP_LOOP,
        OP_STRING_FOREACH,
        OP_LIST_FOREACH,
        OP_RANGE_FOREACH,
        OP_ENUM_FOREACH,
        OP_MAP_FOREACH,
        OP_FIBER_FOREACH,

        OP_CALL,
        OP_TAIL_CALL,
        OP_CALL_INSTANCE_PROPERTY,
        OP_TAIL_CALL_INSTANCE_PROPERTY,
        OP_INSTANCE_INVOKE,
        OP_INSTANCE_TAIL_INVOKE,
        OP_PROTOCOL_INVOKE,
        OP_PROTOCOL_TAIL_INVOKE,
        OP_STRING_INVOKE,
        OP_PATTERN_INVOKE,
        OP_FIBER_INVOKE,
        OP_LIST_INVOKE,
        OP_MAP_INVOKE,
        OP_RANGE_INVOKE,

        OP_CLOSURE,
        OP_CLOSE_UPVALUE,

        OP_FIBER,
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
        OP_PROPERTY,
        OP_OBJECT_DEFAULT,
        OP_GET_OBJECT_PROPERTY,
        OP_GET_INSTANCE_PROPERTY,
        OP_GET_INSTANCE_METHOD,
        OP_GET_PROTOCOL_METHOD,
        OP_GET_FCONTAINER_INSTANCE_PROPERTY,
        OP_GET_LIST_PROPERTY,
        OP_GET_MAP_PROPERTY,
        OP_GET_STRING_PROPERTY,
        OP_GET_PATTERN_PROPERTY,
        OP_GET_FIBER_PROPERTY,
        OP_GET_RANGE_PROPERTY,
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

        OP_HOTSPOT,
        OP_HOTSPOT_CALL,
    };

    fn dispatch(self: *Self, current_frame: *CallFrame, full_instruction: u32, instruction: Chunk.OpCode, arg: u24) void {
        if (BuildOptions.debug_stack) {
            dumpStack(self);
        }

        if (BuildOptions.debug_current_instruction or BuildOptions.debug_stack) {
            var report = Reporter.Report{
                .message = @tagName(instruction),
                .error_type = .no_error,
                .options = .{
                    .surrounding_lines = 1,
                },
                .items = &[_]Reporter.ReportItem{
                    .{
                        .location = self.current_ast.tokens.get(current_frame.closure.function.chunk.lines.items[current_frame.ip - 1]),
                        .kind = .hint,
                        .message = @tagName(instruction),
                    },
                },
            };

            report.reportStderr(&self.reporter) catch unreachable;
        }

        // We're at the start of catch clauses because an error was thrown
        // We must close the try block scope
        if (current_frame.try_ip == current_frame.ip - 1) {
            std.debug.assert(current_frame.try_top != null);
            const err = self.pop();

            // Close scope
            self.closeUpValues(@ptrCast(current_frame.try_top.?));
            self.current_fiber.stack_top = current_frame.try_top.?;

            // Put error back on stack
            self.push(err);

            // As soon as we step into catch clauses, we're not in a try-catch block anymore
            current_frame.try_ip = null;
            current_frame.try_top = null;
        }

        if (BuildOptions.gc_debug_access) {
            self.gc.where = self.current_ast.tokens.get(
                current_frame.closure.function.chunk.lines.items[current_frame.ip - 1],
            );
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

    pub fn panic(self: *Self, msg: []const u8) void {
        @branchHint(.cold);

        self.reportRuntimeErrorWithCurrentStack(msg);

        unreachable;
    }

    fn OP_NULL(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.push(Value.Null);

        const next_full_instruction = self.readInstruction();
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

    fn OP_VOID(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.push(Value.Void);

        const next_full_instruction = self.readInstruction();
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

    fn OP_TRUE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.push(Value.True);

        const next_full_instruction = self.readInstruction();
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

    fn OP_FALSE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.push(Value.False);

        const next_full_instruction = self.readInstruction();
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

    fn OP_POP(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        _ = self.pop();

        const next_full_instruction = self.readInstruction();
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

    fn OP_COPY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        self.copy(arg);

        const next_full_instruction = self.readInstruction();
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

    fn OP_CLONE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.push(
            self.cloneValue(self.pop()) catch {
                self.panic("Out of memory");
                unreachable;
            },
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_SWAP(self: *Self, _: *CallFrame, full_instruction: u32, _: Chunk.OpCode, _: u24) void {
        const from: u8 = @intCast((0x00ffffff & full_instruction) >> 16);
        const to: u8 = @intCast(0x0000ffff & full_instruction);

        const temp = (self.current_fiber.stack_top - to - 1)[0];
        (self.current_fiber.stack_top - to - 1)[0] = (self.current_fiber.stack_top - from - 1)[0];
        (self.current_fiber.stack_top - from - 1)[0] = temp;

        const next_full_instruction = self.readInstruction();
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

    fn OP_DEFINE_GLOBAL(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        const new_len = @max(arg + 1, self.globals.items.len);

        self.globals.ensureTotalCapacity(self.gc.allocator, new_len) catch {
            self.panic("Out of memory");
            unreachable;
        };

        const name = self.pop().obj()
            .access(obj.ObjString, .String, self.gc).?
            .string;
        self.global_names.put(arg, name) catch {
            self.panic("Out of memory");
            unreachable;
        };
        // const values = self.global_names.values();
        // std.log.debug("self.global_names: keys: {any}", .{self.global_names.keys()});
        // for (values) |value| {
        //     std.log.debug("{s}", .{value});
        // }
        // std.log.debug("------------\n", .{});

        // We don't always define a new global at the end of the list
        self.globals.items.len = new_len;
        self.globals.items[arg] = self.pop();
        self.globals_count = @max(self.globals_count, arg);

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_GLOBAL(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        self.push(self.currentGlobals().items[arg]);

        const next_full_instruction = self.readInstruction();
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

    fn OP_SET_GLOBAL(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        self.currentGlobals().items[arg] = self.peek(0);

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_LOCAL(self: *Self, current_frame: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        self.push(current_frame.slots[arg]);

        const next_full_instruction = self.readInstruction();
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

    fn OP_SET_LOCAL(self: *Self, current_frame: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        current_frame.slots[arg] = self.peek(0);

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_UPVALUE(self: *Self, current_frame: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        self.push(current_frame.closure.upvalues[arg].location.*);

        const next_full_instruction = self.readInstruction();
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

    fn OP_SET_UPVALUE(self: *Self, current_frame: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        current_frame.closure.upvalues[arg].location.* = self.peek(0);

        const next_full_instruction = self.readInstruction();
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

    fn OP_CONSTANT(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        self.push(self.readConstant(arg));

        const next_full_instruction = self.readInstruction();
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

    fn OP_TO_STRING(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const str = self.pop().toStringAlloc(self.gc.allocator) catch {
            self.panic("Out of memory");
            unreachable;
        };
        self.push(
            Value.fromObj(
                (self.gc.copyString(str) catch {
                    self.panic("Out of memory");
                    unreachable;
                }).toObj(),
            ),
        );
        self.gc.allocator.free(str);

        const next_full_instruction = self.readInstruction();
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

    fn OP_NEGATE_I(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.push(
            Value.fromInteger(
                -%self.pop().integer(),
            ),
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_NEGATE_F(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.push(
            Value.fromDouble(
                -self.pop().double(),
            ),
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_CLOSURE(self: *Self, current_frame: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        const function = self.readConstant(arg).obj().access(
            obj.ObjFunction,
            .Function,
            self.gc,
        ).?;

        var closure = self.gc.allocateObject(
            obj.ObjClosure,
            obj.ObjClosure.init(self.gc.allocator, self, function) catch {
                self.panic("Out of memory");
                unreachable;
            },
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        self.push(closure.toValue());

        var i: usize = 0;
        while (i < function.upvalue_count) : (i += 1) {
            const is_local = self.readByte() == 1;
            const index = self.readByte();

            if (is_local) {
                closure.upvalues[i] = self.captureUpvalue(&(current_frame.slots[index])) catch {
                    self.panic("Out of memory");
                    unreachable;
                };
            } else {
                closure.upvalues[i] = current_frame.closure.upvalues[index];
            }
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_CLOSE_UPVALUE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.closeUpValues(@ptrCast(self.current_fiber.stack_top - 1));
        _ = self.pop();

        const next_full_instruction = self.readInstruction();
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

    fn OP_FIBER(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        // We read the next instruction to know about the call we want to wrap in the fiber
        const instruction = self.readInstruction();
        // Some opcodes need an extra instruction
        const extra_instruction = switch (getCode(instruction)) {
            .OP_CALL_INSTANCE_PROPERTY,
            .OP_TAIL_CALL_INSTANCE_PROPERTY,
            .OP_MAP_INVOKE,
            .OP_LIST_INVOKE,
            .OP_STRING_INVOKE,
            .OP_PATTERN_INVOKE,
            .OP_FIBER_INVOKE,
            .OP_PROTOCOL_INVOKE,
            .OP_PROTOCOL_TAIL_INVOKE,
            .OP_RANGE_INVOKE,
            .OP_INSTANCE_INVOKE,
            => self.readInstruction(),
            else => null,
        };

        // FIXME: in the case of PROTOCOL_INVOKE, it will try to read the constant from the other fiber's chunk and fail?

        const arg_count: u8 = if (extra_instruction) |extra|
            @intCast(extra >> 24)
        else
            @intCast((0x00ffffff & instruction) >> 16);

        const catch_count: u24 = if (extra_instruction) |extra|
            @intCast(0x00ffffff & extra)
        else
            @intCast(0x0000ffff & instruction);

        const stack_ptr = self.current_fiber.stack_top - arg_count - catch_count - 1;
        const stack_len = arg_count + catch_count + 1;
        const stack_slice = stack_ptr[0..stack_len];

        var fiber = self.gc.allocator.create(Fiber) catch {
            self.panic("Out of memory");
            unreachable;
        };
        fiber.* = Fiber.init(
            self.gc.allocator,
            self.gc.type_registry.getTypeDef(
                .{
                    .optional = false,
                    .def_type = .Fiber,
                    .resolved_type = .{
                        .Fiber = .{
                            .return_type = self.gc.type_registry.void_type,
                            .yield_type = self.gc.type_registry.void_type,
                        },
                    },
                },
            ) catch @panic("Out of memory"),
            self.current_fiber,
            stack_slice,
            instruction,
            extra_instruction,
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        // Pop arguments and catch clauses
        self.current_fiber.stack_top = self.current_fiber.stack_top - stack_len;

        fiber.type_def = self.pop().obj().access(obj.ObjTypeDef, .Type, self.gc).?;

        // Put new fiber on the stack
        var obj_fiber = self.gc.allocateObject(obj.ObjFiber, obj.ObjFiber{
            .fiber = fiber,
        }) catch {
            self.panic("Out of memory");
            unreachable;
        };

        self.push(obj_fiber.toValue());

        const next_full_instruction = self.readInstruction();
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

    fn OP_RESUME(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.pop().obj()
            .access(obj.ObjFiber, .Fiber, self.gc).?
            .fiber.@"resume"(self) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_RESOLVE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.pop().obj()
            .access(obj.ObjFiber, .Fiber, self.gc).?
            .fiber.resolve(self) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_YIELD(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.current_fiber.yield(self);

        const next_full_instruction = self.readInstruction();
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

    fn OP_CALL(self: *Self, _: *CallFrame, full_instruction: u32, _: Chunk.OpCode, _: u24) void {
        const arg_count: u8 = @intCast((0x00ffffff & full_instruction) >> 16);
        const catch_count: u16 = @intCast(0x0000ffff & full_instruction);

        // FIXME: no reason to take the catch value off the stack
        const catch_value = if (catch_count > 0) self.pop() else null;

        self.callValue(
            self.peek(arg_count),
            arg_count,
            catch_value,
        ) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_TAIL_CALL(self: *Self, _: *CallFrame, full_instruction: u32, _: Chunk.OpCode, _: u24) void {
        const arg_count: u8 = @intCast((0x00ffffff & full_instruction) >> 16);
        const catch_count: u16 = @intCast(0x0000ffff & full_instruction);

        // FIXME: no reason to take the catch value off the stack
        const catch_value = if (catch_count > 0) self.pop() else null;

        self.tailCall(
            self.peek(arg_count),
            arg_count,
            catch_value,
        ) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn callInstanceProperty(self: *Self, arg_instruction: u32, property_idx: u24, tail_call: bool) void {
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const instance = self.peek(arg_count).obj()
            .access(obj.ObjObjectInstance, .ObjectInstance, self.gc).?;

        std.debug.assert(instance.object != null);

        const property = instance.fields[property_idx];

        (self.current_fiber.stack_top - arg_count - 1)[0] = property;

        if (tail_call) {
            self.tailCall(
                property,
                arg_count,
                catch_value,
            ) catch |err| {
                switch (err) {
                    Error.RuntimeError => return,
                    else => {
                        self.panic("Out of memory");
                        unreachable;
                    },
                }
            };
        } else {
            self.callValue(
                property,
                arg_count,
                catch_value,
            ) catch |err| {
                switch (err) {
                    Error.RuntimeError => return,
                    else => {
                        self.panic("Out of memory");
                        unreachable;
                    },
                }
            };
        }
    }

    fn OP_CALL_INSTANCE_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, property_idx: u24) void {
        self.callInstanceProperty(
            self.readInstruction(),
            property_idx,
            false,
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_TAIL_CALL_INSTANCE_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, property_idx: u24) void {
        self.callInstanceProperty(
            self.readInstruction(),
            property_idx,
            true,
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_INSTANCE_INVOKE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, property_idx: u24) void {
        const arg_instruction: u32 = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const instance = self.peek(arg_count).obj()
            .access(obj.ObjObjectInstance, .ObjectInstance, self.gc).?;

        std.debug.assert(instance.object != null);

        _ = self.invokeFromObject(
            instance.object.?,
            property_idx,
            arg_count,
            catch_value,
            false,
        ) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_PROTOCOL_INVOKE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, name_constant: u24) void {
        const name = self.readConstant(name_constant)
            .obj().access(obj.ObjString, .String, self.gc).?
            .string;

        const arg_instruction = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const instance = self.peek(arg_count).obj()
            .access(obj.ObjObjectInstance, .ObjectInstance, self.gc).?;

        std.debug.assert(instance.object != null);

        // Find the actual field
        const property_idx = instance.type_def.resolved_type.?.ObjectInstance.of
            .resolved_type.?.Object
            .fields.get(name).?.index;

        _ = self.invokeFromObject(
            instance.object.?,
            property_idx,
            arg_count,
            catch_value,
            false,
        ) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_INSTANCE_TAIL_INVOKE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, property_idx: u24) void {
        const arg_instruction = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const instance = self.peek(arg_count).obj()
            .access(obj.ObjObjectInstance, .ObjectInstance, self.gc).?;

        std.debug.assert(instance.object != null);

        _ = self.invokeFromObject(
            instance.object.?,
            property_idx,
            arg_count,
            catch_value,
            false,
        ) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_PROTOCOL_TAIL_INVOKE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, name_constant: u24) void {
        const name = self.readConstant(name_constant)
            .obj().access(obj.ObjString, .String, self.gc).?
            .string;

        const arg_instruction = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const instance = self.peek(arg_count).obj()
            .access(obj.ObjObjectInstance, .ObjectInstance, self.gc).?;

        std.debug.assert(instance.object != null);

        // Find the actual field
        const property_idx = instance.type_def.resolved_type.?.ObjectInstance.of
            .resolved_type.?.Object
            .fields.get(name).?.index;

        _ = self.invokeFromObject(
            instance.object.?,
            property_idx,
            arg_count,
            catch_value,
            false,
        ) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_STRING_INVOKE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, method_idx: u24) void {
        const arg_instruction = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const member = obj.ObjString.member(self, method_idx) catch {
            self.panic("Out of memory");
            unreachable;
        };

        (self.current_fiber.stack_top - arg_count - 1)[0] = member;

        self.callValue(
            member,
            arg_count,
            catch_value,
        ) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_RANGE_INVOKE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, method_idx: u24) void {
        const arg_instruction = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const member = obj.ObjRange.member(self, method_idx) catch {
            self.panic("Out of memory");
            unreachable;
        };

        (self.current_fiber.stack_top - arg_count - 1)[0] = member;

        self.callValue(
            member,
            arg_count,
            catch_value,
        ) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_PATTERN_INVOKE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, method_idx: u24) void {
        const arg_instruction = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const member = obj.ObjPattern.member(self, method_idx) catch {
            self.panic("Out of memory");
            unreachable;
        };

        (self.current_fiber.stack_top - arg_count - 1)[0] = member;

        self.callValue(
            member,
            arg_count,
            catch_value,
        ) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_FIBER_INVOKE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, method_idx: u24) void {
        const arg_instruction = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const member = obj.ObjFiber.member(self, method_idx) catch {
            self.panic("Out of memory");
            unreachable;
        };

        (self.current_fiber.stack_top - arg_count - 1)[0] = member;
        self.callValue(
            member,
            arg_count,
            catch_value,
        ) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_LIST_INVOKE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, method_idx: u24) void {
        const arg_instruction = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const list = self.peek(arg_count).obj().access(obj.ObjList, .List, self.gc).?;
        const member = list.member(self, method_idx) catch {
            self.panic("Out of memory");
            unreachable;
        };

        (self.current_fiber.stack_top - arg_count - 1)[0] = member;
        self.callValue(
            member,
            arg_count,
            catch_value,
        ) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_MAP_INVOKE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, method_idx: u24) void {
        const arg_instruction = self.readInstruction();
        const arg_count: u8 = @intCast(arg_instruction >> 24);
        const catch_count: u24 = @intCast(0x00ffffff & arg_instruction);
        const catch_value = if (catch_count > 0) self.pop() else null;

        const map = self.peek(arg_count).obj().access(obj.ObjMap, .Map, self.gc).?;
        const member = map.member(self, method_idx) catch {
            self.panic("Out of memory");
            unreachable;
        };

        (self.current_fiber.stack_top - arg_count - 1)[0] = member;
        self.callValue(
            member,
            arg_count,
            catch_value,
        ) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

        const frame = self.currentFrame().?;

        self.closeUpValues(&frame.slots[0]);

        self.current_fiber.frame_count -= 1;
        _ = self.current_fiber.frames.pop();

        // We popped the last frame
        if (self.current_fiber.frame_count == 0) {
            // We're in a fiber
            if (self.current_fiber.parent_fiber != null) {
                self.current_fiber.finish(self, result);

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

    fn repurposeFrame(self: *Self, closure: *obj.ObjClosure, arg_count: u8, catch_value: ?Value) Error!void {
        // Is or will be JIT compiled, call and stop there
        if (!is_wasm and self.current_fiber.parent_fiber == null and
            try self.compileAndCall(
                closure,
                arg_count,
                catch_value,
            ))
        {
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

    fn OP_RETURN(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        if (self.returnFrame() or self.currentFrame().?.in_native_call) {
            return;
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_EXPORT(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        self.push(Value.fromInteger(@as(v.Integer, @intCast(arg))));

        // Ends program, so we don't call dispatch
    }

    fn OP_IMPORT(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const fullpath = self.peek(1).obj().access(obj.ObjString, .String, self.gc).?;
        const closure = self.peek(0).obj().access(obj.ObjClosure, .Closure, self.gc).?;

        if (self.import_registry.get(fullpath)) |globals| {
            self.globals.appendSlice(self.gc.allocator, globals) catch {
                self.panic("Out of memory");
                unreachable;
            };
        } else {
            var vm = self.gc.allocator.create(VM) catch {
                self.panic("Out of memory");
                unreachable;
            };
            // FIXME: give reference to JIT?
            vm.* = VM.init(self.gc, self.import_registry, self.flavor) catch {
                self.panic("Out of memory");
                unreachable;
            };
            // TODO: how to free this since we copy things to new vm, also fails anyway
            // {
            //     defer vm.deinit();
            //     defer gn.deinit();
            // }

            vm.interpret(self.current_ast, closure.function, null) catch |err| {
                switch (err) {
                    Error.RuntimeError => return,
                    else => {
                        self.panic("Out of memory");
                        unreachable;
                    },
                }
            };

            // Top of stack is how many export we got
            const exported_count = vm.peek(0).integer();

            // Copy them to this vm globals
            var import_cache = std.ArrayList(Value).init(self.gc.allocator);

            if (exported_count > 0) {
                var i = exported_count;
                while (i > 0) : (i -= 1) {
                    const global = vm.peek(@intCast(i));
                    self.globals.append(self.gc.allocator, global) catch {
                        self.panic("Out of memory");
                        unreachable;
                    };

                    import_cache.append(global) catch {
                        self.panic("Out of memory");
                        unreachable;
                    };
                }
            }

            self.import_registry.put(
                self.gc.allocator,
                fullpath,
                import_cache.toOwnedSlice() catch {
                    self.panic("Out of memory");
                    unreachable;
                },
            ) catch {
                self.panic("Out of memory");
                unreachable;
            };
        }

        // Pop path and closure
        self.discard(2);

        const next_full_instruction = self.readInstruction();
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

    fn OP_TRY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        self.currentFrame().?.try_ip = @as(usize, @intCast(arg));
        // We will close scope up to this top if an error is thrown
        self.currentFrame().?.try_top = self.current_fiber.stack_top;

        const next_full_instruction = self.readInstruction();
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

    fn OP_TRY_END(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.currentFrame().?.try_ip = null;
        self.currentFrame().?.try_top = null;

        const next_full_instruction = self.readInstruction();
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

    fn OP_THROW(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.throw(
            Error.Custom,
            self.pop(),
            null,
            null,
        ) catch |err| {
            switch (err) {
                Error.RuntimeError => return,
                else => {
                    self.panic("Out of memory");
                    unreachable;
                },
            }
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_LIST(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        var list = self.gc.allocateObject(
            obj.ObjList,
            obj.ObjList.init(
                self.gc.allocator,
                self.readConstant(arg).obj().access(
                    obj.ObjTypeDef,
                    .Type,
                    self.gc,
                ).?,
            ) catch {
                self.panic("Out of memory");
                unreachable;
            },
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        self.push(Value.fromObj(list.toObj()));

        const next_full_instruction = self.readInstruction();
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

    fn OP_RANGE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const high = self.pop().integer();
        const low = self.pop().integer();

        self.push(
            Value.fromObj((self.gc.allocateObject(
                obj.ObjRange,
                obj.ObjRange{
                    .high = high,
                    .low = low,
                },
            ) catch {
                self.panic("Out of memory");
                unreachable;
            }).toObj()),
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_LIST_APPEND(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, item_count: u24) void {
        var list = self.peek(item_count).obj().access(obj.ObjList, .List, self.gc).?;

        var distance: i64 = @intCast(item_count - 1);
        while (distance >= 0) : (distance -= 1) {
            const item = self.peek(@intCast(distance));
            list.rawAppend(self.gc, item) catch {
                self.panic("Out of memory");
                unreachable;
            };
        }

        // Pop items all at once
        self.current_fiber.stack_top -= item_count;

        const next_full_instruction = self.readInstruction();
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

    fn OP_MAP(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        var map = self.gc.allocateObject(
            obj.ObjMap,
            obj.ObjMap.init(
                self.gc.allocator,
                self.readConstant(arg).obj().access(obj.ObjTypeDef, .Type, self.gc).?,
            ) catch {
                self.panic("Out of memory");
                unreachable;
            },
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        self.push(Value.fromObj(map.toObj()));

        const next_full_instruction = self.readInstruction();
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

    fn OP_SET_MAP(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, entries_count: u24) void {
        var map = self.peek(entries_count * 2).obj()
            .access(obj.ObjMap, .Map, self.gc).?;

        var distance: i64 = @intCast((entries_count * 2) - 1);
        while (distance >= 0) : (distance -= 2) {
            const key = self.peek(@intCast(distance));
            const value = self.peek(@intCast(distance - 1));

            map.set(self.gc, key, value) catch {
                self.panic("Out of memory");
                unreachable;
            };
        }

        // Pop entries all at once
        self.current_fiber.stack_top -= entries_count * 2;

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_LIST_SUBSCRIPT(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, checked: u24) void {
        const list = self.peek(1).obj()
            .access(obj.ObjList, .List, self.gc).?;
        const index = self.peek(0).integer();

        if (checked == 0 and index < 0) {
            self.throw(
                Error.OutOfBound,
                (self.gc.copyString("Out of bound list access.") catch {
                    self.panic("Out of memory");
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |err| {
                switch (err) {
                    Error.RuntimeError => return,
                    else => {
                        self.panic("Out of memory");
                        unreachable;
                    },
                }
            };
        }

        const list_index: usize = @intCast(index);

        if (checked == 0 and list_index >= list.items.items.len) {
            self.throw(
                Error.OutOfBound,
                (self.gc.copyString("Out of bound list access.") catch {
                    self.panic("Out of memory");
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |err| {
                switch (err) {
                    Error.RuntimeError => return,
                    else => {
                        self.panic("Out of memory");
                        unreachable;
                    },
                }
            };

            return;
        }

        const list_item = if (list_index >= list.items.items.len or list_index < 0)
            Value.Null
        else
            list.items.items[list_index];

        // Pop list and index
        self.discard(2);

        // Push value
        self.push(list_item);

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_MAP_SUBSCRIPT(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        var map = self.peek(1).obj().access(obj.ObjMap, .Map, self.gc).?;
        const index = self.peek(0);

        // Pop map and key
        self.discard(2);

        if (map.map.get(index)) |value| {
            // Push value
            self.push(value);
        } else {
            self.push(Value.Null);
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_STRING_SUBSCRIPT(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, checked: u24) void {
        const str = self.peek(1).obj().access(obj.ObjString, .String, self.gc).?;
        const index = self.peek(0).integer();

        if (checked == 0 and index < 0) {
            self.throw(
                Error.OutOfBound,
                (self.gc.copyString("Out of bound string access.") catch {
                    self.panic("Out of memory");
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |err| {
                switch (err) {
                    Error.RuntimeError => return,
                    else => {
                        self.panic("Out of memory");
                        unreachable;
                    },
                }
            };
        }

        const str_index: usize = @intCast(index);

        if (checked == 0 and str_index >= str.string.len) {
            self.throw(
                Error.OutOfBound,
                (self.gc.copyString("Out of bound str access.") catch {
                    self.panic("Out of memory");
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |err| {
                switch (err) {
                    Error.RuntimeError => return,
                    else => {
                        self.panic("Out of memory");
                        unreachable;
                    },
                }
            };
        } else {
            const str_item = if (str_index < 0 or str_index >= str.string.len)
                Value.Null
            else
                (self.gc.copyString(&([_]u8{str.string[str_index]})) catch {
                    self.panic("Out of memory");
                    unreachable;
                }).toValue();

            // Pop str and index
            self.discard(2);

            // Push value
            self.push(str_item);
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_SET_LIST_SUBSCRIPT(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        var list = self.peek(2).obj().access(obj.ObjList, .List, self.gc).?;
        const index = self.peek(1);
        const value = self.peek(0);

        if (index.integer() < 0) {
            self.throw(
                Error.OutOfBound,
                (self.gc.copyString("Out of bound list access.") catch {
                    self.panic("Out of memory");
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |err| {
                switch (err) {
                    Error.RuntimeError => return,
                    else => {
                        self.panic("Out of memory");
                        unreachable;
                    },
                }
            };
        }

        const list_index: usize = @intCast(index.integer());

        if (list_index < list.items.items.len) {
            list.set(self.gc, list_index, value) catch {
                self.panic("Out of memory");
                unreachable;
            };

            // Pop everyting
            self.discard(3);

            // Push the value
            self.push(value);
        } else {
            self.throw(
                Error.OutOfBound,
                (self.gc.copyString("Out of bound list access.") catch {
                    self.panic("Out of memory");
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |err| {
                switch (err) {
                    Error.RuntimeError => return,
                    else => {
                        self.panic("Out of memory");
                        unreachable;
                    },
                }
            };
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_SET_MAP_SUBSCRIPT(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        var map: *obj.ObjMap = self.peek(2).obj().access(obj.ObjMap, .Map, self.gc).?;
        const index = self.peek(1);
        const value = self.peek(0);

        map.set(self.gc, index, value) catch {
            self.panic("Out of memory");
            unreachable;
        };

        // Pop everyting
        self.discard(3);

        // Push the value
        self.push(value);

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_ENUM_CASE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        const enum_ = self.peek(0).obj().access(obj.ObjEnum, .Enum, self.gc).?;

        var enum_case: *obj.ObjEnumInstance = self.gc.allocateObject(
            obj.ObjEnumInstance,
            obj.ObjEnumInstance{
                .enum_ref = enum_,
                .case = arg,
            },
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        _ = self.pop();
        self.push(Value.fromObj(enum_case.toObj()));

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_ENUM_CASE_VALUE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const enum_case = self.peek(0).obj().access(obj.ObjEnumInstance, .EnumInstance, self.gc).?;

        _ = self.pop();
        self.push(enum_case.enum_ref.cases[enum_case.case]);

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_ENUM_CASE_FROM_VALUE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const case_value = self.pop();
        const enum_ = self.pop().obj().access(obj.ObjEnum, .Enum, self.gc).?;

        var found = false;
        for (enum_.cases, 0..) |case, index| {
            if (case.eql(case_value)) {
                var enum_case = self.gc.allocateObject(obj.ObjEnumInstance, obj.ObjEnumInstance{
                    .enum_ref = enum_,
                    .case = @intCast(index),
                }) catch {
                    self.panic("Out of memory");
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

        const next_full_instruction = self.readInstruction();
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

    fn OP_OBJECT(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, type_def_constant: u24) void {
        var object = self.gc.allocateObject(
            obj.ObjObject,
            obj.ObjObject.init(
                self.gc.allocator,
                self.readConstant(type_def_constant)
                    .obj().access(obj.ObjTypeDef, .Type, self.gc).?,
            ) catch {
                self.panic("Out of memory");
                unreachable;
            },
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        self.push(Value.fromObj(object.toObj()));

        const next_full_instruction = self.readInstruction();
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

    fn OP_FCONTAINER_INSTANCE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const typedef = self.pop().obj().access(obj.ObjTypeDef, .Type, self.gc);
        const instance = (self.gc.allocateObject(
            obj.ObjForeignContainer,
            obj.ObjForeignContainer.init(
                self,
                typedef.?,
            ) catch {
                self.panic("Out of memory");
                unreachable;
            },
        ) catch {
            self.panic("Out of memory");
            unreachable;
        }).toValue();

        self.push(instance);

        const next_full_instruction = self.readInstruction();
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

    fn OP_INSTANCE(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const typedef = self.pop().obj().access(obj.ObjTypeDef, .Type, self.gc).?;
        const object_or_null = self.pop();
        const object = if (object_or_null.isObj())
            object_or_null.obj().access(obj.ObjObject, .Object, self.gc).?
        else
            null;

        var obj_instance = self.gc.allocateObject(
            obj.ObjObjectInstance,
            obj.ObjObjectInstance.init(
                self,
                object,
                typedef,
                self.gc,
            ) catch {
                self.panic("Out of memory");
                unreachable;
            },
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        // If not anonymous, set default fields
        if (object) |uobject| {
            // Set instance fields with default values
            for (uobject.defaults, 0..) |default_opt, i| {
                if (default_opt) |default| {
                    obj_instance.setField(
                        self.gc,
                        i,
                        self.cloneValue(default) catch {
                            self.panic("Out of memory");
                            unreachable;
                        },
                    ) catch {
                        self.panic("Out of memory");
                        unreachable;
                    };
                }
            }
        }

        self.push(obj_instance.toValue());

        const next_full_instruction = self.readInstruction();
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

    fn OP_OBJECT_DEFAULT(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, property_idx: u24) void {
        self.peek(1).obj()
            .access(obj.ObjObject, .Object, self.gc).?
            .setPropertyDefaultValue(
            self.gc,
            property_idx,
            self.peek(0),
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        _ = self.pop();

        const next_full_instruction = self.readInstruction();
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

    fn OP_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, property_idx: u24) void {
        self.peek(1).obj()
            .access(obj.ObjObject, .Object, self.gc).?
            .setField(
            self.gc,
            property_idx,
            self.peek(0),
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        _ = self.pop();

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_OBJECT_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, property_idx: u24) void {
        const object = self.peek(0).obj().access(obj.ObjObject, .Object, self.gc).?;

        _ = self.pop(); // Pop instance
        self.push(object.fields[property_idx]);

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_FCONTAINER_INSTANCE_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, field_idx: u24) void {
        const instance_value = self.peek(0);

        const struct_instance = instance_value.obj().access(
            obj.ObjForeignContainer,
            .ForeignContainer,
            self.gc,
        ).?;

        _ = self.pop(); // Pop instance
        self.push(struct_instance.getField(self, field_idx));

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_INSTANCE_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, property_idx: u24) void {
        const instance_value = self.peek(0);

        _ = self.pop(); // Pop instance
        self.push(
            instance_value.obj()
                .access(obj.ObjObjectInstance, .ObjectInstance, self.gc).?
                .fields[property_idx],
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_INSTANCE_METHOD(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, method_idx: u24) void {
        const instance_value = self.peek(0);

        self.bindMethod(
            instance_value
                .obj()
                .access(obj.ObjObjectInstance, .ObjectInstance, self.gc).?
                .object.?
                .fields[method_idx]
                .obj()
                .access(obj.ObjClosure, .Closure, self.gc),
            null,
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_PROTOCOL_METHOD(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, name_constant: u24) void {
        const instance: *obj.ObjObjectInstance = self.peek(0).obj()
            .access(obj.ObjObjectInstance, .ObjectInstance, self.gc).?;

        const name = self.readConstant(name_constant).obj()
            .access(obj.ObjString, .String, self.gc).?
            .string;

        // Find the actual field
        const method_idx = instance.type_def.resolved_type.?.ObjectInstance.of
            .resolved_type.?.Object
            .fields.get(name).?.index;

        self.bindMethod(
            instance
                .object.?
                .fields[method_idx]
                .obj()
                .access(obj.ObjClosure, .Closure, self.gc),
            null,
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_LIST_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, method_idx: u24) void {
        self.bindMethod(
            null,
            (self.peek(0).obj().access(obj.ObjList, .List, self.gc).?
                .member(self, method_idx) catch {
                self.panic("Out of memory");
                unreachable;
            }).obj().access(obj.ObjNative, .Native, self.gc).?,
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        const next_full_instruction = self.readInstruction();
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
    fn OP_GET_MAP_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, method_idx: u24) void {
        self.bindMethod(
            null,
            (self.peek(0).obj().access(obj.ObjMap, .Map, self.gc).?
                .member(self, method_idx) catch {
                self.panic("Out of memory");
                unreachable;
            }).obj().access(obj.ObjNative, .Native, self.gc),
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_STRING_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, method_idx: u24) void {
        self.bindMethod(
            null,
            (obj.ObjString
                .member(self, method_idx) catch {
                self.panic("Out of memory");
                unreachable;
            }).obj().access(obj.ObjNative, .Native, self.gc),
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_PATTERN_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, method_idx: u24) void {
        self.bindMethod(
            null,
            (obj.ObjPattern
                .member(self, method_idx) catch {
                self.panic("Out of memory");
                unreachable;
            }).obj().access(obj.ObjNative, .Native, self.gc),
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_FIBER_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, method_idx: u24) void {
        self.bindMethod(
            null,
            (obj.ObjFiber
                .member(self, method_idx) catch {
                self.panic("Out of memory");
                unreachable;
            }).obj().access(obj.ObjNative, .Native, self.gc),
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_GET_RANGE_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, method_idx: u24) void {
        self.bindMethod(
            null,
            (obj.ObjRange.member(self, method_idx) catch {
                self.panic("Out of memory");
                unreachable;
            }).obj().access(obj.ObjNative, .Native, self.gc),
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        const next_full_instruction = self.readInstruction();
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

    fn OP_SET_OBJECT_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, property_idx: u24) void {
        const object = self.peek(1).obj().access(obj.ObjObject, .Object, self.gc).?;

        // Set new value
        object.setField(
            self.gc,
            property_idx,
            self.peek(0),
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        // Get the new value from stack, pop the object and push value again
        const value = self.pop();
        _ = self.pop();
        self.push(value);

        const next_full_instruction = self.readInstruction();
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

    fn OP_SET_FCONTAINER_INSTANCE_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, field_idx: u24) void {
        const instance_value = self.peek(1);

        const struct_instance = instance_value.obj().access(
            obj.ObjForeignContainer,
            .ForeignContainer,
            self.gc,
        ).?;

        struct_instance.setField(
            self,
            field_idx,
            self.peek(0),
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        // Get the new value from stack, pop the instance and push value again
        const value = self.pop();
        _ = self.pop();
        self.push(value);

        const next_full_instruction = self.readInstruction();
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

    fn OP_SET_INSTANCE_PROPERTY(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, property_idx: u24) void {
        const instance_value = self.peek(1);

        // Set new value
        instance_value.obj().access(
            obj.ObjObjectInstance,
            .ObjectInstance,
            self.gc,
        ).?.setField(
            self.gc,
            property_idx,
            self.peek(0),
        ) catch {
            self.panic("Out of memory");
            unreachable;
        };

        // Get the new value from stack, pop the instance and push value again
        const value = self.pop();
        _ = self.pop();
        self.push(value);

        const next_full_instruction = self.readInstruction();
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

    fn OP_NOT(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.push(Value.fromBoolean(!self.pop().boolean()));

        const next_full_instruction = self.readInstruction();
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

    fn OP_BNOT(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const value = self.pop();

        self.push(Value.fromInteger(~value.integer()));

        const next_full_instruction = self.readInstruction();
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

    fn OP_GREATER(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right_value = self.pop();
        const left_value = self.pop();

        const left_f = if (left_value.isDouble()) left_value.double() else null;
        const left_i = if (left_value.isInteger()) left_value.integer() else null;
        const right_f = if (right_value.isDouble()) right_value.double() else null;
        const right_i = if (right_value.isInteger()) right_value.integer() else null;

        if (left_f) |lf| {
            if (right_f) |rf| {
                self.push(Value.fromBoolean(lf > rf));
            } else {
                self.push(Value.fromBoolean(lf > @as(v.Double, @floatFromInt(right_i.?))));
            }
        } else {
            if (right_f) |rf| {
                self.push(Value.fromBoolean(@as(v.Double, @floatFromInt(left_i.?)) > rf));
            } else {
                self.push(Value.fromBoolean(left_i.? > right_i.?));
            }
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_LESS(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right_value = self.pop();
        const left_value = self.pop();

        const left_f = if (left_value.isDouble()) left_value.double() else null;
        const left_i = if (left_value.isInteger()) left_value.integer() else null;
        const right_f = if (right_value.isDouble()) right_value.double() else null;
        const right_i = if (right_value.isInteger()) right_value.integer() else null;

        if (left_f) |lf| {
            if (right_f) |rf| {
                self.push(Value.fromBoolean(lf < rf));
            } else {
                self.push(Value.fromBoolean(lf < @as(v.Double, @floatFromInt(right_i.?))));
            }
        } else {
            if (right_f) |rf| {
                self.push(Value.fromBoolean(@as(v.Double, @floatFromInt(left_i.?)) < rf));
            } else {
                self.push(Value.fromBoolean(left_i.? < right_i.?));
            }
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_ADD_STRING(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop().obj().access(obj.ObjString, .String, self.gc).?;
        const left = self.pop().obj().access(obj.ObjString, .String, self.gc).?;

        self.push(
            Value.fromObj(
                (left.concat(self, right) catch {
                    self.panic("Out of memory");
                    unreachable;
                }).toObj(),
            ),
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_ADD_LIST(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop().obj().access(obj.ObjList, .List, self.gc).?;
        const left = self.pop().obj().access(obj.ObjList, .List, self.gc).?;

        var new_list = std.ArrayListUnmanaged(Value){};
        new_list.appendSlice(self.gc.allocator, left.items.items) catch {
            self.panic("Out of memory");
            unreachable;
        };
        new_list.appendSlice(self.gc.allocator, right.items.items) catch {
            self.panic("Out of memory");
            unreachable;
        };

        self.push(
            (self.gc.allocateObject(obj.ObjList, obj.ObjList{
                .type_def = left.type_def,
                .methods = left.methods,
                .items = new_list,
            }) catch {
                self.panic("Out of memory");
                unreachable;
            }).toValue(),
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_ADD_MAP(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop().obj().access(obj.ObjMap, .Map, self.gc).?;
        const left = self.pop().obj().access(obj.ObjMap, .Map, self.gc).?;

        var new_map = left.map.clone(self.gc.allocator) catch {
            self.panic("Out of memory");
            unreachable;
        };
        var it = right.map.iterator();
        while (it.next()) |entry| {
            new_map.put(
                self.gc.allocator,
                entry.key_ptr.*,
                entry.value_ptr.*,
            ) catch {
                self.panic("Out of memory");
                unreachable;
            };
        }

        self.push(
            (self.gc.allocateObject(obj.ObjMap, obj.ObjMap{
                .type_def = left.type_def,
                .methods = left.methods,
                .map = new_map,
            }) catch {
                self.panic("Out of memory");
                unreachable;
            }).toValue(),
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_ADD_I(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop().integer();
        const left = self.pop().integer();

        self.push(Value.fromInteger(left +% right));

        const next_full_instruction = self.readInstruction();
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

    fn OP_ADD_F(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop().double();
        const left = self.pop().double();

        self.push(Value.fromDouble(left + right));

        const next_full_instruction = self.readInstruction();
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

    fn OP_SUBTRACT_I(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop();
        const left = self.pop();

        self.push(Value.fromInteger(left.integer() -% right.integer()));

        const next_full_instruction = self.readInstruction();
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

    fn OP_SUBTRACT_F(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop();
        const left = self.pop();

        self.push(Value.fromDouble(left.double() - right.double()));

        const next_full_instruction = self.readInstruction();
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

    fn OP_MULTIPLY_I(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop();
        const left = self.pop();

        self.push(Value.fromInteger(left.integer() *% right.integer()));

        const next_full_instruction = self.readInstruction();
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

    fn OP_MULTIPLY_F(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop();
        const left = self.pop();

        self.push(Value.fromDouble(left.double() * right.double()));

        const next_full_instruction = self.readInstruction();
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

    fn OP_DIVIDE_I(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop();
        const left = self.pop();

        self.push(
            Value.fromInteger(@divTrunc(left.integer(), right.integer())),
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_DIVIDE_F(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop();
        const left = self.pop();

        self.push(
            Value.fromDouble(
                left.double() / right.double(),
            ),
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_MOD_I(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop();
        const left = self.pop();

        self.push(
            Value.fromInteger(
                @mod(left.integer(), right.integer()),
            ),
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_MOD_F(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop();
        const left = self.pop();

        self.push(
            Value.fromDouble(
                @mod(left.double(), right.double()),
            ),
        );

        const next_full_instruction = self.readInstruction();
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

    fn OP_BAND(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right: Value = self.pop();
        const left: Value = self.pop();

        self.push(Value.fromInteger(left.integer() & right.integer()));

        const next_full_instruction = self.readInstruction();
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

    fn OP_BOR(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right: Value = self.pop();
        const left: Value = self.pop();

        self.push(Value.fromInteger(left.integer() | right.integer()));

        const next_full_instruction = self.readInstruction();
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

    fn OP_XOR(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right: Value = self.pop();
        const left: Value = self.pop();

        self.push(Value.fromInteger(left.integer() ^ right.integer()));

        const next_full_instruction = self.readInstruction();
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

    fn OP_SHL(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop().integer();
        const left = self.pop().integer();

        if (right < 0) {
            if (right * -1 > std.math.maxInt(u5)) {
                self.push(Value.fromInteger(0));
            } else {
                self.push(Value.fromInteger(left >> @intCast(right * -1)));
            }
        } else {
            if (right > std.math.maxInt(u5)) {
                self.push(Value.fromInteger(0));
            } else {
                self.push(Value.fromInteger(left << @as(u5, @truncate(@as(u64, @intCast(right))))));
            }
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_SHR(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const right = self.pop().integer();
        const left = self.pop().integer();

        if (right < 0) {
            if (right * -1 > std.math.maxInt(u5)) {
                self.push(Value.fromInteger(0));
            } else {
                self.push(Value.fromInteger(left << @as(u5, @truncate(@as(u64, @intCast(right * -1))))));
            }
        } else {
            if (right > std.math.maxInt(u5)) {
                self.push(Value.fromInteger(0));
            } else {
                self.push(Value.fromInteger(left >> @as(u5, @truncate(@as(u64, @intCast(right))))));
            }
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_EQUAL(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.push(Value.fromBoolean(self.pop().eql(self.pop())));

        const next_full_instruction = self.readInstruction();
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

    fn OP_IS(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.push(Value.fromBoolean(self.pop().is(self.pop())));

        const next_full_instruction = self.readInstruction();
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

    fn OP_JUMP(self: *Self, current_frame: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        current_frame.ip += arg;

        const next_full_instruction = self.readInstruction();
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

    fn OP_JUMP_IF_FALSE(self: *Self, current_frame: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        if (!self.peek(0).boolean()) {
            current_frame.ip += arg;
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_JUMP_IF_NOT_NULL(self: *Self, current_frame: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        if (!self.peek(0).isNull()) {
            current_frame.ip += arg;
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_LOOP(self: *Self, current_frame: *CallFrame, _: u32, _: Chunk.OpCode, arg: u24) void {
        current_frame.ip -= arg;

        const next_full_instruction = self.readInstruction();
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

    fn OP_STRING_FOREACH(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const key_slot: *Value = @ptrCast(self.current_fiber.stack_top - 3);
        const value_slot: *Value = @ptrCast(self.current_fiber.stack_top - 2);
        const str = self.peek(0).obj().access(obj.ObjString, .String, self.gc).?;

        key_slot.* = if (str.next(
            self,
            if (key_slot.*.isNull())
                null
            else
                key_slot.integer(),
        ) catch {
            self.panic("Out of memory");
            unreachable;
        }) |new_index|
            Value.fromInteger(new_index)
        else
            Value.Null;

        // Set new value
        if (key_slot.*.isInteger()) {
            value_slot.* = (self.gc.copyString(
                &([_]u8{
                    str.string[@as(usize, @intCast(key_slot.integer()))],
                }),
            ) catch {
                self.panic("Out of memory");
                unreachable;
            }).toValue();
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_LIST_FOREACH(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        var key_slot: *Value = @ptrCast(self.current_fiber.stack_top - 3);
        const value_slot: *Value = @ptrCast(self.current_fiber.stack_top - 2);
        var list = self.peek(0).obj().access(obj.ObjList, .List, self.gc).?;

        // Get next index
        key_slot.* = if (list.rawNext(
            self,
            if (key_slot.*.isNull()) null else key_slot.integer(),
        ) catch {
            self.panic("Out of memory");
            unreachable;
        }) |new_index|
            Value.fromInteger(new_index)
        else
            Value.Null;

        // Set new value
        if (key_slot.*.isInteger()) {
            value_slot.* = list.items.items[@as(usize, @intCast(key_slot.integer()))];
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_RANGE_FOREACH(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const value_slot: *Value = @ptrCast(self.current_fiber.stack_top - 2);
        const range = self.peek(0).obj().access(obj.ObjRange, .Range, self.gc).?;

        if (value_slot.integerOrNull()) |index| {
            if (range.low < range.high) {
                value_slot.* = if (index + 1 >= range.high)
                    Value.Null
                else
                    Value.fromInteger(index + 1);
            } else {
                value_slot.* = if (index - 1 <= range.high)
                    Value.Null
                else
                    Value.fromInteger(index - 1);
            }
        } else {
            value_slot.* = Value.fromInteger(range.low);
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_ENUM_FOREACH(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        var value_slot: *Value = @ptrCast(self.current_fiber.stack_top - 2);
        const enum_case = if (value_slot.*.isNull())
            null
        else
            value_slot.obj().access(obj.ObjEnumInstance, .EnumInstance, self.gc).?;
        var enum_: *obj.ObjEnum = self.peek(0).obj().access(obj.ObjEnum, .Enum, self.gc).?;

        // Get next enum case
        const next_case = enum_.rawNext(self, enum_case) catch {
            self.panic("Out of memory");
            unreachable;
        };
        value_slot.* = (if (next_case) |new_case| Value.fromObj(new_case.toObj()) else Value.Null);

        const next_full_instruction = self.readInstruction();
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

    fn OP_MAP_FOREACH(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const key_slot: *Value = @ptrCast(self.current_fiber.stack_top - 3);
        const value_slot: *Value = @ptrCast(self.current_fiber.stack_top - 2);
        var map: *obj.ObjMap = self.peek(0).obj().access(obj.ObjMap, .Map, self.gc).?;
        const current_key = if (!key_slot.*.isNull()) key_slot.* else null;

        const next_key = map.rawNext(current_key);
        key_slot.* = if (next_key) |unext_key| unext_key else Value.Null;

        if (next_key) |unext_key| {
            value_slot.* = map.map.get(unext_key) orelse Value.Null;
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_FIBER_FOREACH(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        const value_slot: *Value = @ptrCast(self.current_fiber.stack_top - 2);
        var fiber = self.peek(0).obj().access(obj.ObjFiber, .Fiber, self.gc).?;

        if (fiber.fiber.status == .Over) {
            value_slot.* = Value.Null;
        } else {
            fiber.fiber.@"resume"(self) catch {
                self.panic("Out of memory");
                unreachable;
            };
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_UNWRAP(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        if (self.peek(0).isNull()) {
            // TODO: Should we throw or @panic?
            self.throw(
                Error.UnwrappedNull,
                (self.gc.copyString("Force unwrapped optional is null") catch {
                    self.panic("Out of memory");
                    unreachable;
                }).toValue(),
                null,
                null,
            ) catch |err| {
                switch (err) {
                    Error.RuntimeError => return,
                    else => {
                        self.panic("Out of memory");
                        unreachable;
                    },
                }
            };
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_TYPEOF(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        self.push((Value.typeOf(self.pop(), self.gc) catch {
            self.panic("Out of memory");
            unreachable;
        }).toValue());

        const next_full_instruction = self.readInstruction();
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

    // Never generated if jit is disabled
    fn OP_HOTSPOT(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, end_ip: u24) void {
        if (is_wasm) unreachable;

        const node = self.readInstruction();

        self.current_ast.nodes.items(.count)[node] += 1;

        if (self.shouldCompileHotspot(node)) {
            var timer = std.time.Timer.start() catch unreachable;

            if (self.jit.?.compileHotSpot(
                self.current_ast,
                self.currentFrame().?.closure,
                node,
            ) catch null) |native| {
                const obj_native = self.gc.allocateObject(
                    obj.ObjNative,
                    .{
                        .native = native,
                    },
                ) catch {
                    self.panic("Out of memory");
                    unreachable;
                };

                // Prevent collection
                self.gc.markObj(obj_native.toObj()) catch {
                    self.panic("Out of memory");
                    unreachable;
                };
                obj_native.mark(self.gc);

                if (BuildOptions.jit_debug) {
                    io.print(
                        "Compiled hotspot {s} in function `{s}` in {d} ms\n",
                        .{
                            @tagName(self.current_ast.nodes.items(.tag)[node]),
                            self.currentFrame().?.closure.function.type_def.resolved_type.?.Function.name.string,
                            @as(v.Double, @floatFromInt(timer.read())) / 1000000,
                        },
                    );
                }

                // The now compile hotspot must be a new constant for the current function
                self.currentFrame().?.closure.function.chunk.constants.append(
                    self.currentFrame().?.closure.function.chunk.allocator,
                    obj_native.toValue(),
                ) catch {
                    self.panic("Out of memory");
                    unreachable;
                };

                // Patch bytecode to replace hotspot with function call
                self.patchHotspot(
                    self.current_ast.nodes.items(.location)[node],
                    self.currentFrame().?.closure.function.chunk.constants.items.len - 1,
                    end_ip,
                ) catch {
                    self.panic("Out of memory");
                    unreachable;
                };
            } else {
                // Blacklist the node
                self.jit.?.blacklisted_nodes.put(self.gc.allocator, node, {}) catch {
                    self.panic("Out of memory");
                    unreachable;
                };
            }

            self.jit.?.jit_time += timer.read();
        }

        const next_full_instruction = self.readInstruction();
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

    fn OP_HOTSPOT_CALL(self: *Self, _: *CallFrame, _: u32, _: Chunk.OpCode, _: u24) void {
        if (is_wasm) unreachable;

        if (self.callHotspot(
            @ptrCast(
                @alignCast(
                    self.pop().obj().access(obj.ObjNative, .Native, self.gc).?.native,
                ),
            ),
        ) and self.returnFrame()) {
            return;
        }

        const next_full_instruction = self.readInstruction();
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
        const next_full_instruction = self.readInstruction();
        const next_instruction: Chunk.OpCode = getCode(next_full_instruction);
        const next_arg: u24 = getArg(next_full_instruction);

        if (BuildOptions.debug_current_instruction) {
            io.print(
                "{}: {}\n",
                .{
                    next_current_frame.ip,
                    next_instruction,
                },
            );
        }

        if (BuildOptions.gc_debug) {
            self.gc.where = if (self.currentFrame()) |current_frame|
                self.current_ast.tokens.get(current_frame.closure.function.chunk.lines.items[current_frame.ip - 1])
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
        var initial_stack = if (previous_stack == null)
            std.ArrayList(CallFrame).init(self.gc.allocator)
        else
            null;

        var stack = if (previous_stack) |pstack|
            pstack
        else
            &initial_stack.?;
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
                const processed_payload =
                    if (payload.isObj()) payload: {
                        if (payload.obj().access(obj.ObjObjectInstance, .ObjectInstance, self.gc)) |instance| {
                            const object_def = instance.type_def.resolved_type.?.ObjectInstance.of
                                .resolved_type.?.Object;

                            if (object_def.fields.get("message")) |field| {
                                if (!field.method and !field.static) {
                                    break :payload instance.fields[field.index];
                                }
                            }
                        }

                        break :payload payload;
                    } else payload;

                const value_str = try processed_payload.toStringAlloc(self.gc.allocator);
                defer self.gc.allocator.free(value_str);

                self.reportRuntimeError(
                    value_str,
                    error_site,
                    stack.items,
                );

                if (!is_wasm and self.flavor != .Repl) {
                    std.process.exit(1);
                } else {
                    return Error.RuntimeError;
                }
            } else if (self.current_fiber.frame_count == 0) {
                // Error raised inside a fiber, forward it to parent fiber
                self.current_fiber = self.current_fiber.parent_fiber.?;

                try self.throw(
                    code,
                    payload,
                    stack,
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

    inline fn reportRuntimeErrorWithCurrentStack(self: *Self, message: []const u8) void {
        self.reportRuntimeError(
            message,
            if (self.currentFrame()) |frame|
                frame.closure.function.chunk.lines.items[frame.ip - 1]
            else
                null,
            self.current_fiber.frames.items,
        );
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
            var writer = msg.writer();

            if (next) |unext| {
                const function_name = unext.closure.function.type_def.resolved_type.?.Function.name.string;
                writer.print(
                    if (builtin.os.tag != .windows)
                        "\t{s} in \x1b[36m{s}\x1b[0m at {s}"
                    else
                        "\t{s} in {s} at {s}",
                    .{
                        if (i == 0)
                            "╰─┬─"
                        else if (i < stack.len - 1)
                            "  ├─"
                        else
                            "  ╰─",
                        if (unext.closure.function.type_def.resolved_type.?.Function.function_type == .Test)
                            function_name //[(std.mem.indexOfScalar(u8, function_name, ' ') orelse 0 + 1)..]
                        else
                            function_name,
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
                if (frame.closure.function.type_def.resolved_type.?.Function.function_type != .ScriptEntryPoint) {
                    writer.print(
                        ":{d}:{d}",
                        .{
                            self.current_ast.tokens.items(.line)[call_site] + 1,
                            self.current_ast.tokens.items(.column)[call_site],
                        },
                    ) catch @panic("Could not report error");
                }
            }

            notes.append(
                Reporter.Note{
                    .message = msg.toOwnedSlice() catch @panic("Could not report error"),
                    .show_prefix = false,
                },
            ) catch @panic("Could not report error");
        }

        const location = self.current_ast.tokens.get(
            error_site orelse stack[0].call_site.?,
        );
        var err_report = Reporter.Report{
            .message = message,
            .error_type = .runtime,
            .items = &[_]Reporter.ReportItem{
                .{
                    .location = location,
                    .end_location = location,
                    .kind = .@"error",
                    .message = message,
                },
            },
            .notes = notes.items,
        };

        err_report.reportStderr(&self.reporter) catch @panic("Could not report error");
    }

    fn compileAndCall(self: *Self, closure: *obj.ObjClosure, arg_count: u8, catch_value: ?Value) Error!bool {
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
                    io.print(
                        "Compiled function `{s}` in {d} ms\n",
                        .{
                            closure.function.type_def.resolved_type.?.Function.name.string,
                            @as(v.Double, @floatFromInt(timer.read())) / 1000000,
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
                io.print(
                    "Calling compiled version of function `{s}.{}.n{}`\n",
                    .{
                        closure.function.type_def.resolved_type.?.Function.name.string,
                        self.current_ast.nodes.items(.components)[closure.function.node].Function.id,
                        closure.function.node,
                    },
                );
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

    fn call(self: *Self, closure: *obj.ObjClosure, arg_count: u8, catch_value: ?Value) Error!void {
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
        if (!is_wasm and
            self.current_fiber.parent_fiber == null and
            try self.compileAndCall(
                closure,
                arg_count,
                catch_value,
            ))
        {
            return;
        }

        if (self.flavor == .Test and closure.function.type_def.resolved_type.?.Function.function_type == .Test) {
            io.print(
                if (builtin.os.tag != .windows)
                    "\x1b[33m▶ Test: {s}\x1b[0m\n"
                else
                    "▶ Test: {s}\n",
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
            try self.current_fiber.frames.append(self.gc.allocator, frame);
        } else {
            self.current_fiber.frames.items[self.current_fiber.frame_count] = frame;
        }

        self.current_fiber.frame_count += 1;

        if (BuildOptions.jit_debug) {
            io.print(
                "Calling uncompiled version of function `{s}.{}.n{}`\n",
                .{
                    closure.function.type_def.resolved_type.?.Function.name.string,
                    self.current_ast.nodes.items(.components)[closure.function.node].Function.id,
                    closure.function.node,
                },
            );
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

    fn callHotspot(self: *Self, native: obj.NativeFn) bool {
        if (BuildOptions.jit_debug) {
            io.print("Calling hotspot {*}\n", .{native});
        }

        const was_in_native_call = self.currentFrame().?.in_native_call;
        self.currentFrame().?.in_native_call = true;
        defer self.currentFrame().?.in_native_call = was_in_native_call;

        var ctx = obj.NativeCtx{
            .vm = self,
            .globals = self.currentFrame().?.closure.globals.items.ptr,
            .upvalues = self.currentFrame().?.closure.upvalues.ptr,
            .base = self.currentFrame().?.slots,
            .stack_top = &self.current_fiber.stack_top,
        };

        // If native returns 1 here, we know there was an early return in the hotspot
        return native(&ctx) == 1;
    }

    fn callNative(self: *Self, native: obj.NativeFn, arg_count: u8, catch_value: ?Value) !void {
        const was_in_native_call = self.currentFrame().?.in_native_call;
        self.currentFrame().?.in_native_call = true;
        self.currentFrame().?.native_call_error_value = catch_value;

        var result: Value = Value.Null;
        var ctx = obj.NativeCtx{
            .vm = self,
            .globals = &[_]Value{},
            .upvalues = &[_]*obj.ObjUpValue{},
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
    fn callCompiled(self: *Self, closure: *obj.ObjClosure, native: obj.NativeFn, arg_count: u8, catch_value: ?Value) !void {
        const was_in_native_call = self.currentFrame() != null and self.currentFrame().?.in_native_call;
        if (self.currentFrame()) |frame| {
            frame.in_native_call = true;
            frame.native_call_error_value = catch_value;
        }

        var ctx = obj.NativeCtx{
            .vm = self,
            .globals = closure.globals.items.ptr,
            .upvalues = closure.upvalues.ptr,
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

    fn bindMethod(self: *Self, method: ?*obj.ObjClosure, native: ?*obj.ObjNative) !void {
        var bound = try self.gc.allocateObject(
            obj.ObjBoundMethod,
            .{
                .receiver = self.peek(0),
                .closure = method,
                .native = native,
            },
        );

        _ = self.pop(); // Pop instane
        self.push(Value.fromObj(bound.toObj()));
    }

    pub fn callValue(self: *Self, callee: Value, arg_count: u8, catch_value: ?Value) Error!void {
        var object = callee.obj();
        switch (object.obj_type) {
            .Bound => {
                const bound = object.access(obj.ObjBoundMethod, .Bound, self.gc).?;
                (self.current_fiber.stack_top - arg_count - 1)[0] = bound.receiver;

                if (bound.closure) |closure| {
                    return try self.call(
                        closure,
                        arg_count,
                        catch_value,
                    );
                } else {
                    std.debug.assert(bound.native != null);
                    return try self.callNative(
                        @ptrCast(@alignCast(bound.native.?.native)),
                        arg_count,
                        catch_value,
                    );
                }
            },
            .Closure => {
                return try self.call(
                    object.access(obj.ObjClosure, .Closure, self.gc).?,
                    arg_count,
                    catch_value,
                );
            },
            .Native => {
                return try self.callNative(
                    @ptrCast(@alignCast(object.access(obj.ObjNative, .Native, self.gc).?.native)),
                    arg_count,
                    catch_value,
                );
            },
            else => {
                unreachable;
            },
        }
    }

    fn tailCall(self: *Self, callee: Value, arg_count: u8, catch_value: ?Value) Error!void {
        var object = callee.obj();
        switch (object.obj_type) {
            .Bound => {
                const bound = object.access(obj.ObjBoundMethod, .Bound, self.gc).?;
                (self.current_fiber.stack_top - arg_count - 1)[0] = bound.receiver;

                if (bound.closure) |closure| {
                    return try self.repurposeFrame(
                        closure,
                        arg_count,
                        catch_value,
                    );
                } else {
                    std.debug.assert(bound.native != null);
                    return try self.callNative(
                        @ptrCast(@alignCast(bound.native.?.native)),
                        arg_count,
                        catch_value,
                    );
                }
            },
            .Closure => {
                return try self.repurposeFrame(
                    object.access(obj.ObjClosure, .Closure, self.gc).?,
                    arg_count,
                    catch_value,
                );
            },
            .Native => {
                return try self.callNative(
                    @ptrCast(@alignCast(object.access(obj.ObjNative, .Native, self.gc).?.native)),
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
        object: *obj.ObjObject,
        method_idx: usize,
        arg_count: u8,
        catch_value: ?Value,
        tail_call: bool,
    ) !Value {
        const method = object.fields[method_idx];

        if (tail_call)
            try self.tailCall(
                method,
                arg_count,
                catch_value,
            )
        else
            try self.call(
                method.obj().access(obj.ObjClosure, .Closure, self.gc).?,
                arg_count,
                catch_value,
            );

        return method;
    }

    // Used by bz_invoke
    pub fn invoke(
        self: *Self,
        is_property_call: bool,
        member_idx: usize,
        arg_count: u8,
        catch_value: ?Value,
        tail_call: bool,
    ) !Value {
        var receiver: Value = self.peek(arg_count);

        var object = receiver.obj();
        switch (object.obj_type) {
            .ObjectInstance => {
                const instance = object.access(obj.ObjObjectInstance, .ObjectInstance, self.gc).?;

                std.debug.assert(instance.object != null);

                if (is_property_call) {
                    const property = instance.fields[member_idx];

                    (self.current_fiber.stack_top - arg_count - 1)[0] = property;

                    if (tail_call)
                        try self.tailCall(
                            property,
                            arg_count,
                            catch_value,
                        )
                    else
                        try self.callValue(
                            property,
                            arg_count,
                            catch_value,
                        );

                    return property;
                }

                return try self.invokeFromObject(
                    instance.object.?,
                    member_idx,
                    arg_count,
                    catch_value,
                    tail_call,
                );
            },
            .String, .Pattern, .Fiber, .List, .Map => {
                const member = switch (object.obj_type) {
                    .String => try obj.ObjString.member(self, member_idx),
                    .Pattern => try obj.ObjPattern.member(self, member_idx),
                    .Fiber => try obj.ObjFiber.member(self, member_idx),
                    .List => try object.access(obj.ObjList, .List, self.gc).?.member(self, member_idx),
                    .Map => try object.access(obj.ObjMap, .Map, self.gc).?.member(self, member_idx),
                    else => unreachable,
                };

                (self.current_fiber.stack_top - arg_count - 1)[0] = member;

                try self.callValue(
                    member,
                    arg_count,
                    catch_value,
                );

                return member;
            },
            else => unreachable,
        }
    }

    pub fn closeUpValues(self: *Self, last: *Value) void {
        while (self.current_fiber.open_upvalues != null and @intFromPtr(self.current_fiber.open_upvalues.?.location) >= @intFromPtr(last)) {
            var upvalue: *obj.ObjUpValue = self.current_fiber.open_upvalues.?;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed.?;
            self.current_fiber.open_upvalues = upvalue.next;
        }
    }

    pub fn captureUpvalue(self: *Self, local: *Value) !*obj.ObjUpValue {
        var prev_upvalue: ?*obj.ObjUpValue = null;
        var upvalue: ?*obj.ObjUpValue = self.current_fiber.open_upvalues;
        while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
            prev_upvalue = upvalue;
            upvalue = upvalue.?.next;
        }

        if (upvalue != null and upvalue.?.location == local) {
            return upvalue.?;
        }

        var created_upvalue: *obj.ObjUpValue = try self.gc.allocateObject(
            obj.ObjUpValue,
            obj.ObjUpValue.init(local),
        );
        created_upvalue.next = upvalue;

        if (prev_upvalue) |uprev_upvalue| {
            uprev_upvalue.next = created_upvalue;
        } else {
            self.current_fiber.open_upvalues = created_upvalue;
        }

        return created_upvalue;
    }

    fn shouldCompileFunction(self: *Self, closure: *obj.ObjClosure) bool {
        const function_type = closure.function.type_def.resolved_type.?.Function.function_type;

        switch (function_type) {
            .Extern,
            .Script,
            .ScriptEntryPoint,
            .EntryPoint,
            .Repl,
            => return false,
            else => {},
        }

        if (
        // Marked as compilable
        self.current_ast.nodes.items(.compilable)[closure.function.node] and
            self.jit != null and
            (
                // Always on
                BuildOptions.jit_always_on or
                    // Threshold reached
                    (closure.function.call_count > 10 and (@as(f128, @floatFromInt(closure.function.call_count)) / @as(f128, @floatFromInt(self.jit.?.call_count))) > BuildOptions.jit_prof_threshold)))
        {
            // Not blacklisted or already compiled
            self.current_ast.nodes.items(.compilable)[closure.function.node] =
                self.jit.?.compiled_nodes.get(closure.function.node) == null and
                self.jit.?.blacklisted_nodes.get(closure.function.node) == null;

            return self.current_ast.nodes.items(.compilable)[closure.function.node];
        }

        return false;
    }

    fn shouldCompileHotspot(self: *Self, node: Ast.Node.Index) bool {
        const count = self.current_ast.nodes.items(.count)[node];

        if (
        // Marked as compilable
        self.current_ast.nodes.items(.compilable)[node] and
            self.jit != null and
            BuildOptions.jit_hotspot_on and
            // JIT compile all the thing?
            (
                // Always compile
                BuildOptions.jit_always_on or BuildOptions.jit_hotspot_always_on or
                    // Threshold reached
                    (count > 10 and (@as(f128, @floatFromInt(count)) / @as(f128, @floatFromInt(self.hotspots_count))) > BuildOptions.jit_prof_threshold)))
        {
            // It's not already done or blacklisted
            self.current_ast.nodes.items(.compilable)[node] = (self.jit.?.compiled_nodes.get(node) == null and self.jit.?.blacklisted_nodes.get(node) == null);

            return self.current_ast.nodes.items(.compilable)[node];
        }

        return false;
    }

    fn patchHotspot(
        self: *Self,
        location: Ast.TokenIndex,
        constant: usize,
        to: usize,
    ) !void {
        const chunk = &self.currentFrame().?.closure.function.chunk;

        // In order to not fuck up any other ip absolute instructions (like OP_JUMP, etc.), we only put the revelant
        // new bytecode at the end of the range and jump to it.
        const hotspot_call = [_]u32{
            (@as(u32, @intCast(@intFromEnum(Chunk.OpCode.OP_CONSTANT))) << 24) | @as(u32, @intCast(constant)),
            (@as(u32, @intCast(@intFromEnum(Chunk.OpCode.OP_HOTSPOT_CALL))) << 24),
        };

        try chunk.code.replaceRange(
            chunk.allocator,
            to - hotspot_call.len,
            hotspot_call.len,
            &hotspot_call,
        );

        try chunk.lines.replaceRange(
            chunk.allocator,
            to - hotspot_call.len,
            hotspot_call.len,
            &[_]Ast.TokenIndex{
                location,
                location,
            },
        );

        const hotspot_call_start = to - hotspot_call.len;

        // In the event that we are in a nested loop, we put a jump instruction in place of OP_HOTSPOT
        chunk.code.items[self.currentFrame().?.ip - 2] = (@as(u32, @intCast(@intFromEnum(Chunk.OpCode.OP_JUMP))) << 24) | @as(
            u32,
            @intCast(
                hotspot_call_start - (self.currentFrame().?.ip - 2) - 1, // -2 because OP_HOTSPOT has one more instruction for the node index
            ),
        );
        // To avoid the disassembler being lost we replace the OP_HOTSPOT's node index instruction with OP_VOID
        chunk.code.items[self.currentFrame().?.ip - 1] = (@as(u32, @intCast(@intFromEnum(Chunk.OpCode.OP_VOID))) << 24);

        // Jump to it
        self.currentFrame().?.ip = hotspot_call_start;

        if (BuildOptions.debug) {
            disassembler.disassembleChunk(
                chunk,
                self.currentFrame().?.closure.function.type_def.resolved_type.?.Function.name.string,
            );
        }
    }
};
