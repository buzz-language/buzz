const std = @import("std");
const assert = std.debug.assert;
const builtin = @import("builtin");
const BuildOptions = @import("build_options");
const jmp = @import("./jmp.zig").jmp;

const r = @import("./vm.zig");
const VM = r.VM;
const m = @import("./mir.zig");
const n = @import("./node.zig");
const o = @import("./obj.zig");
const v = @import("./value.zig");
const api = @import("./lib/buzz_api.zig");

pub const Error = error{CantCompile} || VM.Error;

const OptJump = struct {
    current_insn: std.ArrayList(m.MIR_insn_t),
    alloca: m.MIR_reg_t,

    pub fn deinit(self: OptJump) void {
        self.current_insn.deinit();
    }
};

const GenState = struct {
    module: m.MIR_module_t,
    prototypes: std.AutoHashMap(ExternApi, m.MIR_item_t),
    // Root closure (not necessarily the one being compiled)
    closure: *o.ObjClosure,
    opt_jump: ?OptJump = null,

    // Frame related stuff, since we compile one function at a time, we don't stack frames while compiling

    function_node: *n.FunctionNode,
    return_counts: bool = false,
    return_emitted: bool = false,

    try_should_handle: ?std.AutoHashMap(*o.ObjTypeDef, void) = null,

    function: ?m.MIR_item_t = null,

    function_native: ?m.MIR_item_t = null,
    function_native_proto: ?m.MIR_item_t = null,

    // Convenience registers
    ctx_reg: ?m.MIR_reg_t = null,
    vm_reg: ?m.MIR_reg_t = null,

    // Avoid register name collisions
    registers: std.AutoHashMap([*:0]const u8, usize),

    // Label to jump to when breaking a loop
    break_label: m.MIR_insn_t = null,
    // Label to jump to when continuing a loop
    continue_label: m.MIR_insn_t = null,

    pub fn deinit(self: *GenState) void {
        self.prototypes.deinit();
        self.registers.deinit();
        if (self.try_should_handle) |*try_should_handle| {
            try_should_handle.deinit();
        }
    }
};

const Self = @This();

vm: *VM,
ctx: m.MIR_context_t,
state: ?GenState = null,
// List of closures being or already compiled
compiled_closures: std.AutoHashMap(*o.ObjClosure, void),
// Closure we can't compile (containing async call, or yield)
blacklisted_closures: std.AutoHashMap(*o.ObjClosure, void),
// MIR doesn't allow generating multiple functions at once, so we keep a set of function to compile
// Once compiled, the value is set to an array of the native and raw native func_items
functions_queue: std.AutoHashMap(*n.FunctionNode, ?[2]m.MIR_item_t),
// ObjClosures for which we later compiled the function and need to set it's native and native_raw fields
objclosures_queue: std.AutoHashMap(*o.ObjClosure, void),
// External api to link
required_ext_api: std.AutoHashMap(ExternApi, void),
// Modules to load when linking/generating
modules: std.ArrayList(m.MIR_module_t),
// Call count of all functions
call_count: u128 = 0,
// Keeps track of time spent in the JIT
jit_time: usize = 0,

pub fn init(vm: *VM) Self {
    return .{
        .vm = vm,
        .ctx = m.MIR_init(),
        .compiled_closures = std.AutoHashMap(*o.ObjClosure, void).init(vm.gc.allocator),
        .blacklisted_closures = std.AutoHashMap(*o.ObjClosure, void).init(vm.gc.allocator),
        .functions_queue = std.AutoHashMap(*n.FunctionNode, ?[2]m.MIR_item_t).init(vm.gc.allocator),
        .objclosures_queue = std.AutoHashMap(*o.ObjClosure, void).init(vm.gc.allocator),
        .required_ext_api = std.AutoHashMap(ExternApi, void).init(vm.gc.allocator),
        .modules = std.ArrayList(m.MIR_module_t).init(vm.gc.allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.compiled_closures.deinit();
    self.blacklisted_closures.deinit();
    // assert(self.functions_queue.count() == 0);
    self.functions_queue.deinit();
    // assert(self.objclosures_queue.count() == 0);
    self.objclosures_queue.deinit();
    self.modules.deinit();
    self.required_ext_api.deinit();
    m.MIR_finish(self.ctx);
}

fn buildFunction(self: *Self, closure: ?*o.ObjClosure, function_node: *n.FunctionNode) Error!void {
    const qualified_name = try self.getFunctionQualifiedName(
        function_node,
        false,
    );
    defer qualified_name.deinit();
    const raw_qualified_name = try self.getFunctionQualifiedName(
        function_node,
        true,
    );
    defer raw_qualified_name.deinit();

    const module = m.MIR_new_module(self.ctx, @ptrCast([*:0]u8, qualified_name.items.ptr));
    defer m.MIR_finish_module(self.ctx);

    try self.modules.append(module);

    self.state = .{
        .module = module,
        .prototypes = std.AutoHashMap(ExternApi, m.MIR_item_t).init(self.vm.gc.allocator),
        .function_node = function_node,
        .registers = std.AutoHashMap([*:0]const u8, usize).init(self.vm.gc.allocator),
        .closure = closure orelse self.state.?.closure,
    };

    if (closure) |uclosure| {
        try self.compiled_closures.put(uclosure, {});

        if (BuildOptions.jit_debug) {
            std.debug.print(
                "Compiling function `{s}` because it was called {}/{} times\n",
                .{
                    qualified_name.items,
                    uclosure.function.call_count,
                    self.call_count,
                },
            );
        }
    } else {
        if (BuildOptions.jit_debug) {
            std.debug.print(
                "Compiling closure `{s}`\n",
                .{
                    qualified_name.items,
                },
            );
        }
    }

    _ = self.generateNode(function_node.toNode()) catch |err| {
        if (err == Error.CantCompile) {
            if (BuildOptions.jit_debug) {
                std.debug.print("Not compiling `{s}`, likely because it uses a fiber\n", .{qualified_name.items});
            }

            m.MIR_finish_func(self.ctx);

            _ = self.functions_queue.remove(function_node);
            if (closure) |uclosure| {
                _ = self.objclosures_queue.remove(uclosure);
                try self.blacklisted_closures.put(uclosure, {});
            }
        }

        return err;
    };

    // Export generated function so it can be linked
    _ = m.MIR_new_export(self.ctx, @ptrCast([*:0]u8, raw_qualified_name.items.ptr));
    _ = m.MIR_new_export(self.ctx, @ptrCast([*:0]u8, qualified_name.items.ptr));

    if (BuildOptions.jit_debug) {
        var debug_path = std.ArrayList(u8).init(self.vm.gc.allocator);
        defer debug_path.deinit();
        debug_path.writer().print("./dist/gen/{s}.mod.mir\u{0}", .{qualified_name.items}) catch unreachable;

        const debug_file = std.c.fopen(@ptrCast([*:0]const u8, debug_path.items.ptr), "w").?;
        defer _ = std.c.fclose(debug_file);

        m.MIR_output_module(self.ctx, debug_file, module);
    }
}

pub fn compileFunction(self: *Self, closure: *o.ObjClosure) Error!void {
    const function = closure.function;
    const function_node = @ptrCast(*n.FunctionNode, @alignCast(@alignOf(n.FunctionNode), function.node));

    // Remember we need to set this functions fields
    try self.objclosures_queue.put(closure, {});

    // Build the function
    try self.buildFunction(closure, function_node);

    // Did we encountered other functions that need to be compiled?
    var it = self.functions_queue.iterator();
    while (it.next()) |kv| {
        const node = kv.key_ptr.*;

        if (kv.value_ptr.* == null) {
            // Does it have an associated closure?
            var it2 = self.objclosures_queue.iterator();
            var sub_closure: ?*o.ObjClosure = null;
            while (it2.next()) |kv2| {
                if (kv2.key_ptr.*.function.node == @ptrCast(*anyopaque, node)) {
                    sub_closure = kv2.key_ptr.*;
                    break;
                }
            }
            try self.buildFunction(sub_closure, node);

            // Building a new function might have added functions in the queue, so we reset the iterator
            it = self.functions_queue.iterator();
        }
    }

    // Load modules
    for (self.modules.items) |module| {
        m.MIR_load_module(self.ctx, module);
    }

    // Load external functions
    var it_ext = self.required_ext_api.iterator();
    while (it_ext.next()) |kv| {
        switch (kv.key_ptr.*) {
            // TODO: don't mix those with actual api functions
            .rawfn, .nativefn => {},
            else => m.MIR_load_external(
                self.ctx,
                kv.key_ptr.*.name(),
                kv.key_ptr.*.ptr(),
            ),
        }
    }

    // Link everything together
    m.MIR_link(self.ctx, m.MIR_set_lazy_gen_interface, null);

    m.MIR_gen_init(self.ctx, 1);
    defer m.MIR_gen_finish(self.ctx);

    // Generate all needed functions and set them in corresponding ObjFunctions
    var it2 = self.functions_queue.iterator();
    while (it2.next()) |kv| {
        const node = kv.key_ptr.*;
        const items = kv.value_ptr.*.?;

        const native = m.MIR_gen(self.ctx, 0, items[0]);
        const native_raw = m.MIR_gen(self.ctx, 0, items[1]);

        // Find out if we need to set it in a ObjFunction
        var it3 = self.objclosures_queue.iterator();
        while (it3.next()) |kv2| {
            if (kv2.key_ptr.*.function.node == @ptrCast(*anyopaque, node)) {
                kv2.key_ptr.*.function.native = native;
                kv2.key_ptr.*.function.native_raw = native_raw;
                break;
            }
        }
    }

    // Ensure queues are empty for future use
    self.functions_queue.clearAndFree();
    self.objclosures_queue.clearAndFree();
    self.required_ext_api.clearAndFree();
    self.modules.clearAndFree();

    self.state.?.deinit();
    self.state = null;
}

fn closeScope(self: *Self, node: *n.ParseNode) !void {
    if (node.ends_scope) |closing| {
        for (closing.items) |op| {
            if (op == .OP_CLOSE_UPVALUE) {
                try self.buildCloseUpValues();
            } else if (op == .OP_POP) {
                try self.buildPop(null);
            } else {
                unreachable;
            }
        }
    }
}

fn buildCloseUpValues(self: *Self) !void {
    const ctx_reg = self.state.?.ctx_reg.?;
    const stack_top_ptr_base = try self.REG("stack_top_ptr_base", m.MIR_T_I64);
    const stack_top_base = try self.REG("stack_top_base", m.MIR_T_I64);
    const index = try self.REG("index", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    // *[*]Value
    const stack_top_ptr = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        @offsetOf(o.NativeCtx, "stack_top"),
        ctx_reg,
        index,
        1,
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_ptr_base),
        stack_top_ptr,
    );

    // [*]Value
    const stack_top = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        0,
        stack_top_ptr_base,
        index,
        1,
    );

    // Store value on stack top
    self.SUB(
        m.MIR_new_reg_op(self.ctx, stack_top_base),
        stack_top,
        m.MIR_new_uint_op(self.ctx, @sizeOf(u64)),
    );

    try self.buildExternApiCall(
        .bz_closeUpValues,
        null,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            m.MIR_new_reg_op(self.ctx, stack_top_base),
        },
    );
}

fn buildStackPtr(self: *Self, distance: usize) !m.MIR_op_t {
    const ctx_reg = self.state.?.ctx_reg.?;
    const stack_top_ptr_base = try self.REG("stack_top_ptr_base", m.MIR_T_I64);
    const stack_top_base = try self.REG("stack_top_base", m.MIR_T_I64);
    const index = try self.REG("index", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    // *[*]Value
    const stack_top_ptr = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        @offsetOf(o.NativeCtx, "stack_top"),
        ctx_reg,
        index,
        1,
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_ptr_base),
        stack_top_ptr,
    );

    // [*]Value
    const stack_top = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        0,
        stack_top_ptr_base,
        index,
        1,
    );

    // Store value on stack top
    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_base),
        stack_top,
    );

    const ptr = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("ptr", m.MIR_T_I64),
    );

    self.SUB(
        ptr,
        stack_top,
        m.MIR_new_uint_op(self.ctx, (1 + distance) * @sizeOf(u64)),
    );

    return ptr;
}

fn buildPush(self: *Self, value: m.MIR_op_t) !void {
    const ctx_reg = self.state.?.ctx_reg.?;
    const stack_top_ptr_base = try self.REG("stack_top_ptr_base", m.MIR_T_I64);
    const stack_top_base = try self.REG("stack_top_base", m.MIR_T_I64);
    const index = try self.REG("index", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    // *[*]Value
    const stack_top_ptr = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        @offsetOf(o.NativeCtx, "stack_top"),
        ctx_reg,
        index,
        1,
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_ptr_base),
        stack_top_ptr,
    );

    // [*]Value
    const stack_top = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        0,
        stack_top_ptr_base,
        index,
        1,
    );

    // Store value on stack top
    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_base),
        stack_top,
    );

    const top = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        0,
        stack_top_base,
        index,
        1,
    );

    self.MOV(
        top,
        value,
    );

    // Increment stack top
    self.ADD(
        stack_top,
        m.MIR_new_reg_op(self.ctx, stack_top_base),
        m.MIR_new_uint_op(self.ctx, @sizeOf(u64)),
    );
}

fn buildPop(self: *Self, dest: ?m.MIR_op_t) !void {
    const ctx_reg = self.state.?.ctx_reg.?;
    const stack_top_ptr_base = try self.REG("stack_top_ptr_base", m.MIR_T_I64);
    const stack_top_base = try self.REG("stack_top_base", m.MIR_T_I64);
    const index = try self.REG("index", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    // *[*]Value
    const stack_top_ptr = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        @offsetOf(o.NativeCtx, "stack_top"),
        ctx_reg,
        index,
        1,
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_ptr_base),
        stack_top_ptr,
    );

    // [*]Value
    const stack_top = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        0,
        stack_top_ptr_base,
        index,
        1,
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_base),
        stack_top,
    );

    // Decrement stack top
    self.SUB(
        stack_top,
        m.MIR_new_reg_op(self.ctx, stack_top_base),
        m.MIR_new_uint_op(self.ctx, @sizeOf(u64)),
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_base),
        stack_top,
    );

    // Store new top in result reg
    const top = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        0,
        stack_top_base,
        index,
        1,
    );

    self.MOV(
        dest orelse m.MIR_new_reg_op(
            self.ctx,
            try self.REG("dismiss", m.MIR_T_I64),
        ),
        top,
    );
}

fn buildPeek(self: *Self, distance: u32, dest: m.MIR_op_t) !void {
    const ctx_reg = self.state.?.ctx_reg.?;
    const stack_top_ptr_base = try self.REG("stack_top_ptr_base", m.MIR_T_I64);
    const stack_top_base = try self.REG("stack_top_base", m.MIR_T_I64);
    const index = try self.REG("index", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    // *[*]Value
    const stack_top_ptr = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        @offsetOf(o.NativeCtx, "stack_top"),
        ctx_reg,
        index,
        1,
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_ptr_base),
        stack_top_ptr,
    );

    // [*]Value
    const stack_top = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        0,
        stack_top_ptr_base,
        index,
        1,
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_base),
        stack_top,
    );

    const top = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        (-1 - @intCast(i32, distance)) * @sizeOf(u64),
        stack_top_base,
        index,
        1,
    );

    self.MOV(dest, top);
}

fn buildGetLocal(self: *Self, slot: usize) !m.MIR_op_t {
    const ctx_reg = self.state.?.ctx_reg.?;
    const index = try self.REG("index", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    const base = try self.REG("base", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, base),
        m.MIR_new_mem_op(
            self.ctx,
            m.MIR_T_U64,
            @offsetOf(o.NativeCtx, "base"),
            ctx_reg,
            index,
            @sizeOf(u64),
        ),
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, slot),
    );

    return m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_U64,
        0,
        base,
        index,
        @sizeOf(u64),
    );
}

fn buildSetLocal(self: *Self, slot: usize, value: m.MIR_op_t) !void {
    const ctx_reg = self.state.?.ctx_reg.?;
    const index = try self.REG("index", m.MIR_T_I64);
    const base = try self.REG("base", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, base),
        m.MIR_new_mem_op(
            self.ctx,
            m.MIR_T_U64,
            @offsetOf(o.NativeCtx, "base"),
            ctx_reg,
            index,
            0,
        ),
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, slot),
    );

    const local = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_U64,
        0,
        base,
        index,
        @sizeOf(u64),
    );

    self.MOV(local, value);
}

fn buildGetGlobal(self: *Self, slot: usize) !m.MIR_op_t {
    const ctx_reg = self.state.?.ctx_reg.?;
    const index = try self.REG("index", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    const globals = try self.REG("globals", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, globals),
        m.MIR_new_mem_op(
            self.ctx,
            m.MIR_T_P,
            @offsetOf(o.NativeCtx, "globals"),
            ctx_reg,
            index,
            @sizeOf(u64),
        ),
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, slot),
    );

    return m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_U64,
        0,
        globals,
        index,
        @sizeOf(u64),
    );
}

fn buildSetGlobal(self: *Self, slot: usize, value: m.MIR_op_t) !void {
    const ctx_reg = self.state.?.ctx_reg.?;
    const index = try self.REG("index", m.MIR_T_I64);

    const globals = try self.REG("globals", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, globals),
        m.MIR_new_mem_op(
            self.ctx,
            m.MIR_T_P,
            @offsetOf(o.NativeCtx, "globals"),
            ctx_reg,
            index,
            @sizeOf(u64),
        ),
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, slot),
    );

    const local = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        0,
        globals,
        index,
        @sizeOf(u64),
    );

    self.MOV(local, value);
}

fn buildValueToBoolean(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) void {
    self.EQ(
        dest,
        value,
        m.MIR_new_uint_op(self.ctx, v.TrueMask),
    );
}

fn buildValueFromBoolean(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) void {
    const true_label = m.MIR_new_label(self.ctx);
    const out_label = m.MIR_new_label(self.ctx);

    self.BEQ(
        m.MIR_new_label_op(self.ctx, true_label),
        value,
        m.MIR_new_uint_op(self.ctx, 1),
    );

    self.MOV(
        dest,
        m.MIR_new_uint_op(self.ctx, v.Value.False.val),
    );

    self.JMP(out_label);

    self.append(true_label);

    self.MOV(
        dest,
        m.MIR_new_uint_op(self.ctx, v.Value.True.val),
    );

    self.append(out_label);
}

fn buildValueToInteger(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) void {
    self.ANDS(
        dest,
        value,
        m.MIR_new_uint_op(self.ctx, 0xffffffff),
    );
}

fn buildValueFromInteger(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) void {
    self.OR(
        dest,
        m.MIR_new_uint_op(self.ctx, v.IntegerMask),
        value,
    );
}

fn buildValueToObj(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) void {
    self.NOT(dest, m.MIR_new_uint_op(self.ctx, v.PointerMask));
    self.AND(dest, value, dest);
}

fn buildValueFromObj(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) void {
    self.OR(
        dest,
        m.MIR_new_uint_op(self.ctx, v.PointerMask),
        value,
    );
}

// Unwrap buzz value to its raw mir Value
fn unwrap(self: *Self, def_type: o.ObjTypeDef.Type, value: m.MIR_op_t, dest: m.MIR_op_t) void {
    return switch (def_type) {
        .Bool => self.buildValueToBoolean(value, dest),
        .Integer => self.buildValueToInteger(value, dest),
        .Float => {
            // Allocate memory
            const addr = self.REG("cast", m.MIR_T_I64) catch unreachable;
            self.ALLOCA(addr, @sizeOf(u64));

            // Put the value in it as u64
            self.MOV(
                m.MIR_new_mem_op(
                    self.ctx,
                    m.MIR_T_U64,
                    0,
                    addr,
                    0,
                    0,
                ),
                value,
            );

            // Take it out as double
            self.DMOV(
                dest,
                m.MIR_new_mem_op(
                    self.ctx,
                    m.MIR_T_D,
                    0,
                    addr,
                    0,
                    0,
                ),
            );
        },
        .Void => self.MOV(dest, value),
        .String,
        .Pattern,
        .ObjectInstance,
        .Object,
        .Protocol,
        .ProtocolInstance,
        .Enum,
        .EnumInstance,
        .List,
        .Map,
        .Function,
        .Type,
        .Fiber,
        .UserData,
        => self.buildValueToObj(value, dest),
        .Placeholder,
        .Generic,
        => unreachable,
    };
}

// Wrap mir value to buzz Value
fn wrap(self: *Self, def_type: o.ObjTypeDef.Type, value: m.MIR_op_t, dest: m.MIR_op_t) void {
    return switch (def_type) {
        .Bool => self.buildValueFromBoolean(value, dest),
        .Integer => self.buildValueFromInteger(value, dest),
        .Float => {
            // Allocate memory
            const addr = self.REG("cast", m.MIR_T_I64) catch unreachable;
            self.ALLOCA(addr, @sizeOf(u64));

            // Put the value in it as double
            self.DMOV(
                m.MIR_new_mem_op(
                    self.ctx,
                    m.MIR_T_D,
                    0,
                    addr,
                    0,
                    0,
                ),
                value,
            );

            // Take it out as u64
            self.MOV(
                dest,
                m.MIR_new_mem_op(
                    self.ctx,
                    m.MIR_T_U64,
                    0,
                    addr,
                    0,
                    0,
                ),
            );
        },
        .Void => self.MOV(dest, m.MIR_new_uint_op(self.ctx, v.Value.Void.val)),
        .String,
        .Pattern,
        .ObjectInstance,
        .Object,
        .Protocol,
        .ProtocolInstance,
        .Enum,
        .EnumInstance,
        .List,
        .Map,
        .Function,
        .Type,
        .Fiber,
        .UserData,
        => self.buildValueFromObj(value, dest),
        .Placeholder,
        .Generic,
        => unreachable,
    };
}

fn buildExternApiCall(self: *Self, method: ExternApi, dest: ?m.MIR_op_t, args: []const m.MIR_op_t) !void {
    var full_args = std.ArrayList(m.MIR_op_t).init(self.vm.gc.allocator);
    defer full_args.deinit();

    try full_args.append(m.MIR_new_ref_op(self.ctx, try method.declare(self)));
    try full_args.append(m.MIR_new_ref_op(self.ctx, m.MIR_new_import(self.ctx, method.name())));
    if (dest) |udest| {
        try full_args.append(udest);
    }
    try full_args.appendSlice(args);

    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            m.MIR_CALL,
            full_args.items.len,
            full_args.items.ptr,
        ),
    );
}

fn generateNode(self: *Self, node: *n.ParseNode) Error!?m.MIR_op_t {
    var value = if (node.isConstant(node) and node.node_type != .List and node.node_type != .Map)
        m.MIR_new_uint_op(
            self.ctx,
            (node.toValue(node, self.vm.gc) catch return VM.Error.Custom).val,
        )
    else switch (node.node_type) {
        .Boolean => m.MIR_new_uint_op(
            self.ctx,
            v.Value.fromBoolean(n.BooleanNode.cast(node).?.constant).val,
        ),
        .Float => m.MIR_new_double_op(
            self.ctx,
            n.FloatNode.cast(node).?.float_constant,
        ),
        .Integer => m.MIR_new_uint_op(
            self.ctx,
            v.Value.fromInteger(n.IntegerNode.cast(node).?.integer_constant).val,
        ),
        .StringLiteral => m.MIR_new_uint_op(
            self.ctx,
            n.StringLiteralNode.cast(node).?.constant.toValue().val,
        ),
        .Null => m.MIR_new_uint_op(
            self.ctx,
            v.Value.Null.val,
        ),
        .Void => m.MIR_new_uint_op(
            self.ctx,
            v.Value.Void.val,
        ),
        .String => try self.generateString(n.StringNode.cast(node).?),
        .Expression => try self.generateNode(n.ExpressionNode.cast(node).?.expression),
        .Grouping => try self.generateNode(n.GroupingNode.cast(node).?.expression),
        .Function => try self.generateFunction(n.FunctionNode.cast(node).?),
        .FunDeclaration => try self.generateFunDeclaration(n.FunDeclarationNode.cast(node).?),
        .VarDeclaration => try self.generateVarDeclaration(n.VarDeclarationNode.cast(node).?),
        .Block => try self.generateBlock(n.BlockNode.cast(node).?),
        .Call => try self.generateCall(n.CallNode.cast(node).?),
        .NamedVariable => try self.generateNamedVariable(n.NamedVariableNode.cast(node).?),
        .Return => try self.generateReturn(n.ReturnNode.cast(node).?),
        .If => try self.generateIf(n.IfNode.cast(node).?),
        .Binary => try self.generateBinary(n.BinaryNode.cast(node).?),
        .While => try self.generateWhile(n.WhileNode.cast(node).?),
        .DoUntil => try self.generateDoUntil(n.DoUntilNode.cast(node).?),
        .For => try self.generateFor(n.ForNode.cast(node).?),
        .Break => try self.generateBreak(node),
        .Continue => try self.generateContinue(node),
        .List => try self.generateList(n.ListNode.cast(node).?),
        .Dot => try self.generateDot(n.DotNode.cast(node).?),
        .Subscript => try self.generateSubscript(n.SubscriptNode.cast(node).?),
        .Map => try self.generateMap(n.MapNode.cast(node).?),
        .Is => try self.generateIs(n.IsNode.cast(node).?),
        .Try => try self.generateTry(n.TryNode.cast(node).?),
        .Throw => try self.generateThrow(n.ThrowNode.cast(node).?),
        .Unwrap => try self.generateUnwrap(n.UnwrapNode.cast(node).?),
        .ObjectInit => try self.generateObjectInit(n.ObjectInitNode.cast(node).?),
        .ForceUnwrap => try self.generateForceUnwrap(n.ForceUnwrapNode.cast(node).?),
        .Unary => try self.generateUnary(n.UnaryNode.cast(node).?),
        .Pattern => try self.generatePattern(n.PatternNode.cast(node).?),
        .ForEach => try self.generateForEach(n.ForEachNode.cast(node).?),
        .InlineIf => try self.generateInlineIf(n.InlineIfNode.cast(node).?),
        .AsyncCall,
        .Resume,
        .Resolve,
        .Yield,
        => return Error.CantCompile,

        else => {
            std.debug.print("{} NYI\n", .{node.node_type});
            unreachable;
        },
    };

    if (node.node_type != .Break and node.node_type != .Continue) {
        // Patch opt jumps if needed
        if (node.patch_opt_jumps) {
            assert(self.state.?.opt_jump != null);

            const out_label = m.MIR_new_label(self.ctx);

            // We reached here, means nothing was null, set the alloca with the value and use it has the node return value
            self.MOV(
                m.MIR_new_reg_op(self.ctx, self.state.?.opt_jump.?.alloca),
                value.?,
            );

            self.JMP(out_label);

            // Patch opt blocks with the branching
            for (self.state.?.opt_jump.?.current_insn.items) |current_insn| {
                m.MIR_insert_insn_after(
                    self.ctx,
                    self.state.?.function.?,
                    current_insn,
                    m.MIR_new_insn(
                        self.ctx,
                        m.MIR_BEQ,
                        m.MIR_new_label_op(self.ctx, out_label),
                        m.MIR_new_reg_op(self.ctx, self.state.?.opt_jump.?.alloca),
                        m.MIR_new_uint_op(self.ctx, v.Value.Null.val),
                    ),
                );
            }

            self.append(out_label);

            value = m.MIR_new_reg_op(self.ctx, self.state.?.opt_jump.?.alloca);

            self.state.?.opt_jump.?.deinit();
            self.state.?.opt_jump = null;
        }

        // Close scope if needed
        try self.closeScope(node);
    }

    return value;
}

fn generateString(self: *Self, string_node: *n.StringNode) Error!?m.MIR_op_t {
    if (string_node.elements.len == 0) {
        return m.MIR_new_uint_op(
            self.ctx,
            self.state.?.closure.function.chunk.constants.items[0].val,
        ); // Constant 0 is the empty string
    }

    var previous: ?m.MIR_op_t = null;
    for (string_node.elements) |element| {
        var value = (try self.generateNode(element)).?;

        if (element.type_def.?.def_type != .String or element.type_def.?.optional) {
            const dest = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("result", m.MIR_T_I64),
            );

            try self.buildExternApiCall(
                .bz_toString,
                dest,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    value,
                },
            );

            value = dest;
        }

        if (previous) |uprevious| {
            const dest = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("result", m.MIR_T_I64),
            );

            try self.buildExternApiCall(
                .bz_objStringConcat,
                dest,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    uprevious,
                    value,
                },
            );

            value = dest;
        }

        if (previous != null) {
            try self.buildPop(null);
        }

        previous = value;

        try self.buildPush(previous.?);
    }

    try self.buildPop(null);

    return previous.?;
}

fn generateNamedVariable(self: *Self, named_variable_node: *n.NamedVariableNode) Error!?m.MIR_op_t {
    const function_type: ?o.ObjFunction.FunctionType = if (named_variable_node.node.type_def.?.def_type == .Function)
        named_variable_node.node.type_def.?.resolved_type.?.Function.function_type
    else
        null;
    const is_constant_fn = function_type != null and function_type.? != .Extern and function_type.? != .Anonymous;

    const name = try self.vm.gc.allocator.dupeZ(u8, named_variable_node.identifier.lexeme);
    defer self.vm.gc.allocator.free(name);

    switch (named_variable_node.slot_type) {
        .Global => {
            if (named_variable_node.value) |value| {
                assert(!is_constant_fn);

                try self.buildSetGlobal(named_variable_node.slot, (try self.generateNode(value)).?);

                return null;
            } else if (is_constant_fn) {
                // Get the actual Value as it is right now (which is correct since a function doesn't change)
                const closure = o.ObjClosure.cast(self.state.?.closure.globals.items[named_variable_node.slot].obj()).?;

                // Does it need to be compiled?
                if (self.compiled_closures.get(closure) == null) {
                    if (self.blacklisted_closures.get(closure) != null) {
                        return Error.CantCompile;
                    }

                    // Remember we need to set native fields of this ObjFunction later
                    try self.objclosures_queue.put(closure, {});

                    // Remember that we need to compile this function later
                    try self.functions_queue.put(
                        @ptrCast(
                            *n.FunctionNode,
                            @alignCast(@alignOf(n.FunctionNode), closure.function.node),
                        ),
                        null,
                    );
                }

                return m.MIR_new_uint_op(self.ctx, closure.toValue().val);
            } else {
                return try self.buildGetGlobal(named_variable_node.slot);
            }
        },
        .Local => {
            if (named_variable_node.value) |value| {
                try self.buildSetLocal(named_variable_node.slot, (try self.generateNode(value)).?);

                return null;
            }

            return try self.buildGetLocal(named_variable_node.slot);
        },
        .UpValue => {
            if (named_variable_node.value) |value| {
                try self.buildExternApiCall(
                    .bz_setUpValue,
                    null,
                    &[_]m.MIR_op_t{
                        m.MIR_new_reg_op(self.ctx, self.state.?.ctx_reg.?),
                        m.MIR_new_uint_op(self.ctx, named_variable_node.slot),
                        (try self.generateNode(value)).?,
                    },
                );

                return null;
            }

            const upvalue = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("upvalue", m.MIR_T_I64),
            );
            try self.buildExternApiCall(
                .bz_getUpValue,
                upvalue,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.ctx_reg.?),
                    m.MIR_new_uint_op(self.ctx, named_variable_node.slot),
                },
            );

            return upvalue;
        },
    }
}

fn generateCall(self: *Self, call_node: *n.CallNode) Error!?m.MIR_op_t {
    // This is not a call but an Enum(value)
    if (call_node.callee.type_def.?.def_type == .Enum) {
        const value = call_node.arguments.get(call_node.arguments.keys()[0]).?;
        const result_reg = try self.REG("enum_case", m.MIR_T_I64);

        try self.buildExternApiCall(
            .bz_getEnumCaseFromValue,
            m.MIR_new_reg_op(self.ctx, result_reg),
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                (try self.generateNode(call_node.callee)).?,
                (try self.generateNode(value)).?,
            },
        );

        return m.MIR_new_reg_op(self.ctx, result_reg);
    }

    // Find out if call is invoke or regular call
    const dot = n.DotNode.cast(call_node.callee);
    const invoked_on = if (call_node.callee.node_type == .Dot)
        dot.?.callee.type_def.?.def_type
    else
        null;

    const subject = if (invoked_on != null) try self.generateNode(dot.?.callee) else null;
    const callee_reg = try self.REG("callee", m.MIR_T_I64);
    const callee = m.MIR_new_reg_op(self.ctx, callee_reg);
    if (invoked_on != null) {
        switch (invoked_on.?) {
            .Object => try self.buildExternApiCall(
                .bz_getObjectField,
                callee,
                &[_]m.MIR_op_t{
                    subject.?,
                    m.MIR_new_uint_op(
                        self.ctx,
                        (try self.vm.gc.copyString(n.DotNode.cast(call_node.callee).?.identifier.lexeme)).toValue().val,
                    ),
                },
            ),
            .ObjectInstance,
            .ProtocolInstance,
            .String,
            .Pattern,
            .Fiber,
            .List,
            .Map,
            => try self.buildExternApiCall(
                switch (invoked_on.?) {
                    .ObjectInstance, .ProtocolInstance => .bz_getInstanceField,
                    .String => .bz_getStringField,
                    .Pattern => .bz_getPatternField,
                    .Fiber => .bz_getFiberField,
                    .List => .bz_getListField,
                    .Map => .bz_getMapField,
                    else => unreachable,
                },
                callee,
                &[_]m.MIR_op_t{
                    // vm
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    // subject
                    subject.?,
                    // member
                    m.MIR_new_uint_op(
                        self.ctx,
                        (try self.vm.gc.copyString(n.DotNode.cast(call_node.callee).?.identifier.lexeme)).toValue().val,
                    ),
                    // bound
                    m.MIR_new_uint_op(self.ctx, 0),
                },
            ),
            else => unreachable,
        }
    } else {
        self.MOV(callee, (try self.generateNode(call_node.callee)).?);
    }

    const callee_type = switch (call_node.callee.node_type) {
        .Dot => n.DotNode.cast(call_node.callee).?.member_type_def,
        else => call_node.callee.type_def,
    };

    const function_type_def: *o.ObjTypeDef = try callee_type.?.populateGenerics(
        callee_type.?.resolved_type.?.Function.id,
        call_node.resolved_generics,
        &self.vm.gc.type_registry,
        null,
    );
    const function_type = function_type_def.resolved_type.?.Function.function_type;

    const error_types = function_type_def.resolved_type.?.Function.error_types;
    const has_catch_clause = call_node.catch_default != null and error_types != null and error_types.?.len > 0 and function_type != .Extern;

    // If we have a catch value, create alloca for return value so we can replace it when error is raised
    const return_alloca = if (has_catch_clause)
        try self.REG("return_value", m.MIR_T_I64)
    else
        null;

    if (return_alloca) |alloca| {
        self.ALLOCA(alloca, @sizeOf(u64));
    }

    const post_call_label = if (has_catch_clause)
        m.MIR_new_label(self.ctx)
    else
        null;

    const catch_value = if (call_node.catch_default) |value|
        (try self.generateNode(value)).?
    else
        null;

    if (has_catch_clause) {
        const catch_label = m.MIR_new_label(self.ctx);
        const continue_label = m.MIR_new_label(self.ctx);

        // setjmp to catch any error bubbling up here
        const try_ctx = m.MIR_new_reg_op(
            self.ctx,
            try self.REG("try_ctx", m.MIR_T_I64),
        );

        try self.buildExternApiCall(
            .bz_setTryCtx,
            try_ctx,
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            },
        );

        const env = m.MIR_new_reg_op(
            self.ctx,
            try self.REG("env", m.MIR_T_I64),
        );
        self.ADD(
            env,
            try_ctx,
            m.MIR_new_uint_op(self.ctx, @offsetOf(r.TryCtx, "env")),
        );

        const status = try self.REG("status", m.MIR_T_I64);
        try self.buildExternApiCall(
            .setjmp,
            m.MIR_new_reg_op(self.ctx, status),
            &[_]m.MIR_op_t{env},
        );

        self.BEQ(
            m.MIR_new_label_op(self.ctx, catch_label),
            m.MIR_new_reg_op(self.ctx, status),
            m.MIR_new_uint_op(self.ctx, 1),
        );

        self.JMP(continue_label);

        self.append(catch_label);

        // on error set return alloca with catch_value
        // FIXME: probably not how you use the alloca, maybe use it in a mem_op
        self.MOV(
            m.MIR_new_reg_op(self.ctx, return_alloca.?),
            catch_value.?,
        );

        self.JMP(post_call_label.?);

        // else continue
        self.append(continue_label);
    }

    // This is an async call, create a fiber
    if (call_node.async_call) {
        // TODO: fiber
        unreachable;
    }

    // Arguments

    // if invoked, first arg is `this`
    if (invoked_on != null) {
        _ = try self.buildPush(subject.?);
    } else {
        _ = try self.buildPush(m.MIR_new_uint_op(self.ctx, v.Value.Void.val));
    }

    const args: std.AutoArrayHashMap(*o.ObjString, *o.ObjTypeDef) = function_type_def.resolved_type.?.Function.parameters;
    const defaults = function_type_def.resolved_type.?.Function.defaults;
    const arg_keys = args.keys();

    var arguments = std.AutoArrayHashMap(*o.ObjString, m.MIR_op_t).init(self.vm.gc.allocator);
    defer arguments.deinit();

    // Evaluate arguments
    for (call_node.arguments.keys(), 0..) |arg_key, index| {
        const argument = call_node.arguments.get(arg_key).?;
        const actual_arg_key = if (index == 0 and std.mem.eql(u8, arg_key.string, "$")) arg_keys[0] else arg_key;

        try arguments.put(actual_arg_key, (try self.generateNode(argument)).?);
    }

    // Push them in order on the stack with default value if missing argument
    for (arg_keys) |key| {
        if (arguments.get(key)) |arg| {
            try self.buildPush(arg);
        } else {
            var value = defaults.get(key).?;
            value = if (value.isObj()) try o.cloneObject(value.obj(), self.vm) else value;

            // Push clone of default
            const clone = try self.REG("clone", m.MIR_T_I64);
            try self.buildExternApiCall(
                .bz_clone,
                m.MIR_new_reg_op(self.ctx, clone),
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    m.MIR_new_uint_op(self.ctx, value.val),
                },
            );

            try self.buildPush(m.MIR_new_reg_op(self.ctx, clone));
        }
    }

    const new_ctx = try self.REG("new_ctx", m.MIR_T_I64);
    self.ALLOCA(new_ctx, @sizeOf(o.NativeCtx));
    try self.buildExternApiCall(
        .bz_context,
        callee,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.ctx_reg.?),
            callee,
            m.MIR_new_reg_op(self.ctx, new_ctx),
            m.MIR_new_uint_op(self.ctx, arg_keys.len),
        },
    );

    // Regular function, just call it
    const result = try self.REG("result", m.MIR_T_I64);
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            m.MIR_CALL,
            4,
            &[_]m.MIR_op_t{
                m.MIR_new_ref_op(
                    self.ctx,
                    if (function_type == .Extern)
                        try ExternApi.nativefn.declare(self)
                    else
                        try ExternApi.rawfn.declare(self),
                ),
                callee,
                m.MIR_new_reg_op(self.ctx, result),
                m.MIR_new_reg_op(self.ctx, new_ctx),
            },
        ),
    );

    if (post_call_label) |label| {
        self.append(label);

        self.MOV(
            m.MIR_new_reg_op(self.ctx, result),
            m.MIR_new_reg_op(self.ctx, return_alloca.?),
        );
    }

    if (function_type == .Extern) {
        return try self.generateHandleExternReturn(
            function_type_def.resolved_type.?.Function.error_types != null,
            function_type_def.resolved_type.?.Function.return_type.def_type != .Void,
            m.MIR_new_reg_op(self.ctx, result),
            function_type_def.resolved_type.?.Function.parameters.count(),
            catch_value,
        );
    }

    return m.MIR_new_reg_op(self.ctx, result);
}

// Handle Extern call like VM.callNative does
fn generateHandleExternReturn(
    self: *Self,
    can_fail: bool,
    should_return: bool,
    return_code: m.MIR_op_t,
    arg_count: usize,
    catch_value: ?m.MIR_op_t,
) !m.MIR_op_t {
    if (can_fail) {
        const continue_label = m.MIR_new_label(self.ctx);

        self.BNE(
            continue_label,
            return_code,
            m.MIR_new_int_op(self.ctx, -1),
        );

        if (catch_value) |value| {
            // Pop error
            const discard = try self.REG("discard", m.MIR_T_I64);
            try self.buildPop(m.MIR_new_reg_op(self.ctx, discard));

            // Push catch value
            try self.buildPush(value);
        } else {
            try self.buildExternApiCall(
                .bz_rethrow,
                null,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                },
            );

            try self.buildExternApiCall(
                .exit,
                null,
                &[_]m.MIR_op_t{m.MIR_new_uint_op(self.ctx, 1)},
            );
        }

        self.append(continue_label);
    }

    const result = try self.REG("result", m.MIR_T_I64);
    if (should_return) {
        try self.buildPop(m.MIR_new_reg_op(self.ctx, result));
    } else {
        self.MOV(
            m.MIR_new_reg_op(self.ctx, result),
            m.MIR_new_uint_op(self.ctx, v.Value.Void.val),
        );
    }

    const ctx_reg = self.state.?.ctx_reg.?;
    const stack_top_ptr_base = try self.REG("stack_top_ptr_base", m.MIR_T_I64);
    const stack_top_base = try self.REG("stack_top_base", m.MIR_T_I64);
    const index = try self.REG("index", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    // *[*]Value
    const stack_top_ptr = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        @offsetOf(o.NativeCtx, "stack_top"),
        ctx_reg,
        index,
        1,
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_ptr_base),
        stack_top_ptr,
    );

    // [*]Value
    const stack_top = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        0,
        stack_top_ptr_base,
        index,
        1,
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_base),
        stack_top,
    );

    // Reset stack
    self.SUB(
        stack_top,
        m.MIR_new_reg_op(self.ctx, stack_top_base),
        m.MIR_new_uint_op(self.ctx, (arg_count + 1) * @sizeOf(u64)),
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_base),
        stack_top,
    );

    return m.MIR_new_reg_op(self.ctx, result);
}

fn buildReturn(self: *Self, value: m.MIR_op_t) !void {
    const ctx_reg = self.state.?.ctx_reg.?;
    const index = try self.REG("index", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    // Get base
    // [*]Value
    const base = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        @offsetOf(o.NativeCtx, "base"),
        ctx_reg,
        index,
        1,
    );

    try self.buildExternApiCall(
        .bz_closeUpValues,
        null,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            base,
        },
    );

    // *[*]Value
    const stack_top_ptr = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        @offsetOf(o.NativeCtx, "stack_top"),
        ctx_reg,
        index,
        1,
    );

    // Reset stack_top to base
    self.MOV(try self.LOAD(stack_top_ptr), base);

    // Do return
    self.append(
        m.MIR_new_ret_insn(
            self.ctx,
            1,
            value,
        ),
    );
}

fn generateReturn(self: *Self, return_node: *n.ReturnNode) Error!?m.MIR_op_t {
    if (return_node.unconditional) {
        self.state.?.return_emitted = true;
    }

    try self.buildReturn(
        if (return_node.value) |value|
            (try self.generateNode(value)).?
        else
            m.MIR_new_uint_op(self.ctx, v.Value.Void.val),
    );

    return null;
}

fn generateIf(self: *Self, if_node: *n.IfNode) Error!?m.MIR_op_t {
    const constant_condition = if (if_node.condition.isConstant(if_node.condition) and if_node.unwrapped_identifier == null and if_node.casted_type == null)
        if_node.condition.toValue(if_node.condition, self.vm.gc) catch unreachable
    else
        null;

    // Generate condition
    const condition_value = if (constant_condition == null)
        (try self.generateNode(if_node.condition)).?
    else
        null;
    const condition = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("condition", m.MIR_T_I64),
    );

    // Is it `if (opt -> unwrapped)`?
    if (if_node.unwrapped_identifier != null) {
        try self.buildExternApiCall(
            .bz_valueEqual,
            condition,
            &[_]m.MIR_op_t{
                condition_value.?,
                m.MIR_new_uint_op(self.ctx, v.Value.Null.val),
            },
        );

        const true_label = m.MIR_new_label(self.ctx);
        const out_label = m.MIR_new_label(self.ctx);

        self.BEQ(
            m.MIR_new_label_op(self.ctx, true_label),
            condition,
            m.MIR_new_uint_op(self.ctx, v.Value.True.val),
        );

        self.MOV(
            condition,
            m.MIR_new_uint_op(self.ctx, 1),
        );

        self.JMP(out_label);

        self.append(true_label);

        self.MOV(
            condition,
            m.MIR_new_uint_op(self.ctx, 0),
        );

        self.append(out_label);
    } else if (if_node.casted_type) |casted_type| {
        try self.buildExternApiCall(
            .bz_valueIs,
            condition,
            &[_]m.MIR_op_t{
                condition_value.?,
                m.MIR_new_uint_op(self.ctx, casted_type.toValue().val),
            },
        );

        self.unwrap(
            .Bool,
            condition,
            condition,
        );
    } else if (constant_condition == null) {
        self.unwrap(
            .Bool,
            condition_value.?,
            condition,
        );
    }

    const out_label = m.MIR_new_label(self.ctx);
    const then_label = m.MIR_new_label(self.ctx);
    const else_label = if (if_node.else_branch != null)
        m.MIR_new_label(self.ctx)
    else
        null;

    if (constant_condition != null) {
        self.JMP(
            if (constant_condition.?.boolean())
                then_label
            else if (if_node.else_branch != null)
                else_label.?
            else
                out_label,
        );
    } else {
        self.BEQ(
            m.MIR_new_label_op(self.ctx, then_label),
            condition,
            m.MIR_new_uint_op(self.ctx, 1),
        );

        self.JMP(
            if (if_node.else_branch != null)
                else_label.?
            else
                out_label,
        );
    }

    if (constant_condition == null or constant_condition.?.boolean()) {
        self.append(then_label);

        // Push unwrapped value as local of the then block
        if (if_node.unwrapped_identifier != null or if_node.casted_type != null) {
            try self.buildPush(condition_value.?);
        }

        _ = try self.generateNode(if_node.body);

        self.JMP(out_label);
    }

    if (constant_condition == null or !constant_condition.?.boolean()) {
        if (if_node.else_branch) |else_branch| {
            self.append(else_label);

            _ = try self.generateNode(else_branch);
        }
    }

    self.append(out_label);

    return null;
}

fn generateInlineIf(self: *Self, inline_if_node: *n.InlineIfNode) Error!?m.MIR_op_t {
    const constant_condition = if (inline_if_node.condition.isConstant(inline_if_node.condition))
        inline_if_node.condition.toValue(inline_if_node.condition, self.vm.gc) catch unreachable
    else
        null;

    // Generate condition
    const condition = (try self.generateNode(inline_if_node.condition)).?;

    const resolved = try self.REG("resolved", m.MIR_T_I64);

    const out_label = m.MIR_new_label(self.ctx);
    const then_label = m.MIR_new_label(self.ctx);
    const else_label = m.MIR_new_label(self.ctx);

    if (constant_condition != null) {
        self.JMP(
            if (constant_condition.?.boolean())
                then_label
            else
                else_label,
        );
    } else {
        const unwrapped_condition = m.MIR_new_reg_op(
            self.ctx,
            try self.REG("ucond", m.MIR_T_I64),
        );

        self.unwrap(
            .Bool,
            condition,
            unwrapped_condition,
        );

        self.BEQ(
            m.MIR_new_label_op(self.ctx, then_label),
            unwrapped_condition,
            m.MIR_new_uint_op(self.ctx, 1),
        );

        self.JMP(else_label);
    }

    if (constant_condition == null or constant_condition.?.boolean()) {
        self.append(
            then_label,
        );

        self.MOV(
            m.MIR_new_reg_op(self.ctx, resolved),
            (try self.generateNode(inline_if_node.body)).?,
        );

        self.JMP(out_label);
    }

    if (constant_condition == null or !constant_condition.?.boolean()) {
        self.append(
            else_label,
        );

        self.MOV(
            m.MIR_new_reg_op(self.ctx, resolved),
            (try self.generateNode(inline_if_node.else_branch)).?,
        );
    }

    self.append(out_label);

    return m.MIR_new_reg_op(self.ctx, resolved);
}

fn generateBinary(self: *Self, binary_node: *n.BinaryNode) Error!?m.MIR_op_t {
    const left_type_def = binary_node.left.type_def.?.def_type;
    const right_type_def = binary_node.right.type_def.?.def_type;

    return switch (binary_node.operator) {
        .Ampersand,
        .Bor,
        .Xor,
        .ShiftLeft,
        .ShiftRight,
        => try self.generateBitwise(binary_node),
        .QuestionQuestion,
        .And,
        .Or,
        => try self.genereateConditional(binary_node),
        else => {
            const left_value = (try self.generateNode(binary_node.left)).?;
            const right_value = (try self.generateNode(binary_node.right)).?;

            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );
            var left = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("left", if (left_type_def == .Float) m.MIR_T_D else m.MIR_T_I64),
            );
            var right = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("right", if (right_type_def == .Float) m.MIR_T_D else m.MIR_T_I64),
            );

            if (left_type_def == .Integer) {
                self.unwrap(.Integer, left_value, left);
            } else if (left_type_def == .Float) {
                self.unwrap(.Float, left_value, left);
            } else {
                self.MOV(left, left_value);
            }

            if (right_type_def == .Integer) {
                self.unwrap(.Integer, right_value, right);
            } else if (right_type_def == .Float) {
                self.unwrap(.Float, right_value, right);
            } else {
                self.MOV(right, right_value);
            }

            // Avoid collection
            if (left_type_def != .Integer and left_type_def != .Float) {
                try self.buildPush(left_value);
            }

            if (right_type_def != .Integer and right_type_def != .Float) {
                try self.buildPush(right_value);
            }

            switch (binary_node.operator) {
                .EqualEqual => try self.buildExternApiCall(
                    .bz_valueEqual,
                    res,
                    &[_]m.MIR_op_t{
                        left_value,
                        right_value,
                    },
                ),
                .BangEqual => {
                    try self.buildExternApiCall(
                        .bz_valueEqual,
                        res,
                        &[_]m.MIR_op_t{
                            left_value,
                            right_value,
                        },
                    );

                    self.unwrap(.Bool, res, res);

                    const true_label = m.MIR_new_label(self.ctx);
                    const out_label = m.MIR_new_label(self.ctx);

                    self.BEQ(
                        m.MIR_new_label_op(self.ctx, true_label),
                        res,
                        m.MIR_new_uint_op(self.ctx, 1),
                    );

                    self.MOV(
                        res,
                        m.MIR_new_uint_op(self.ctx, v.Value.True.val),
                    );

                    self.JMP(out_label);

                    self.append(true_label);

                    self.MOV(
                        res,
                        m.MIR_new_uint_op(self.ctx, v.Value.False.val),
                    );

                    self.append(out_label);
                },
                .Greater, .Less, .GreaterEqual, .LessEqual => {
                    if (left_type_def == .Float or right_type_def == .Float) {
                        if (left_type_def == .Integer) {
                            const left_f = m.MIR_new_reg_op(
                                self.ctx,
                                try self.REG("left_float", m.MIR_T_D),
                            );
                            self.I2D(left_f, left);
                            left = left_f;
                        }

                        if (right_type_def == .Integer) {
                            const right_f = m.MIR_new_reg_op(
                                self.ctx,
                                try self.REG("right_float", m.MIR_T_D),
                            );
                            self.I2D(right_f, right);
                            right = right_f;
                        }

                        switch (binary_node.operator) {
                            .Greater => self.DGT(res, left, right),
                            .Less => self.DLT(res, left, right),
                            .GreaterEqual => self.DGE(res, left, right),
                            .LessEqual => self.DLE(res, left, right),
                            else => unreachable,
                        }

                        self.wrap(.Bool, res, res);
                    } else {
                        switch (binary_node.operator) {
                            .Greater => self.GTS(res, left, right),
                            .Less => self.LTS(res, left, right),
                            .GreaterEqual => self.GES(res, left, right),
                            .LessEqual => self.LES(res, left, right),
                            else => unreachable,
                        }

                        self.wrap(.Bool, res, res);
                    }
                },
                .Plus => {
                    switch (binary_node.left.type_def.?.def_type) {
                        .Integer, .Float => {
                            if (left_type_def == .Float or right_type_def == .Float) {
                                if (left_type_def == .Integer) {
                                    const left_f = m.MIR_new_reg_op(
                                        self.ctx,
                                        try self.REG("left_float", m.MIR_T_D),
                                    );
                                    self.I2D(left_f, left);
                                    left = left_f;
                                }

                                if (right_type_def == .Integer) {
                                    const right_f = m.MIR_new_reg_op(
                                        self.ctx,
                                        try self.REG("right_float", m.MIR_T_D),
                                    );
                                    self.I2D(right_f, right);
                                    right = right_f;
                                }

                                const f_res = m.MIR_new_reg_op(
                                    self.ctx,
                                    try self.REG("f_res", m.MIR_T_D),
                                );
                                self.DADD(f_res, left, right);

                                self.wrap(.Float, f_res, res);
                            } else {
                                self.ADDS(res, left, right);

                                self.wrap(.Integer, res, res);
                            }
                        },
                        .String => {
                            try self.buildExternApiCall(
                                .bz_objStringConcat,
                                res,
                                &[_]m.MIR_op_t{
                                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                                    left,
                                    right,
                                },
                            );
                        },
                        .List => {
                            try self.buildExternApiCall(
                                .bz_listConcat,
                                res,
                                &[_]m.MIR_op_t{
                                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                                    left,
                                    right,
                                },
                            );
                        },
                        .Map => {
                            try self.buildExternApiCall(
                                .bz_mapConcat,
                                res,
                                &[_]m.MIR_op_t{
                                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                                    left,
                                    right,
                                },
                            );
                        },
                        else => unreachable,
                    }
                },
                .Minus => {
                    if (left_type_def == .Float or right_type_def == .Float) {
                        if (left_type_def == .Integer) {
                            const left_f = m.MIR_new_reg_op(
                                self.ctx,
                                try self.REG("left_float", m.MIR_T_D),
                            );
                            self.I2D(left_f, left);
                            left = left_f;
                        }

                        if (right_type_def == .Integer) {
                            const right_f = m.MIR_new_reg_op(
                                self.ctx,
                                try self.REG("right_float", m.MIR_T_D),
                            );
                            self.I2D(right_f, right);
                            right = right_f;
                        }

                        const f_res = m.MIR_new_reg_op(
                            self.ctx,
                            try self.REG("f_res", m.MIR_T_D),
                        );
                        self.DSUB(f_res, left, right);

                        self.wrap(.Float, f_res, res);
                    } else {
                        self.SUBS(res, left, right);

                        self.wrap(.Integer, res, res);
                    }
                },
                .Star => {
                    if (left_type_def == .Float or right_type_def == .Float) {
                        if (left_type_def == .Integer) {
                            const left_f = m.MIR_new_reg_op(
                                self.ctx,
                                try self.REG("left_float", m.MIR_T_D),
                            );
                            self.I2D(left_f, left);
                            left = left_f;
                        }

                        if (right_type_def == .Integer) {
                            const right_f = m.MIR_new_reg_op(
                                self.ctx,
                                try self.REG("right_float", m.MIR_T_D),
                            );
                            self.I2D(right_f, right);
                            right = right_f;
                        }

                        const f_res = m.MIR_new_reg_op(
                            self.ctx,
                            try self.REG("f_res", m.MIR_T_D),
                        );
                        self.DMUL(f_res, left, right);

                        self.wrap(.Float, f_res, res);
                    } else {
                        self.MULS(res, left, right);

                        self.wrap(.Integer, res, res);
                    }
                },
                .Slash => {
                    if (left_type_def == .Float or right_type_def == .Float) {
                        if (left_type_def == .Integer) {
                            const left_f = m.MIR_new_reg_op(
                                self.ctx,
                                try self.REG("left_float", m.MIR_T_D),
                            );
                            self.I2D(left_f, left);
                            left = left_f;
                        }

                        if (right_type_def == .Integer) {
                            const right_f = m.MIR_new_reg_op(
                                self.ctx,
                                try self.REG("right_float", m.MIR_T_D),
                            );
                            self.I2D(right_f, right);
                            right = right_f;
                        }

                        const f_res = m.MIR_new_reg_op(
                            self.ctx,
                            try self.REG("f_res", m.MIR_T_D),
                        );
                        self.DDIV(f_res, left, right);

                        self.wrap(.Float, f_res, res);
                    } else {
                        self.DIVS(res, left, right);

                        self.wrap(.Integer, res, res);
                    }
                },
                .Percent => {
                    if (left_type_def == .Float or right_type_def == .Float) {
                        // FIXME: mir doesn't seem to have a mod/rem for floats?
                        unreachable;
                    } else {
                        self.MODS(res, left, right);

                        self.wrap(.Integer, res, res);
                    }
                },
                else => unreachable,
            }

            if (left_type_def != .Integer and left_type_def != .Float) {
                try self.buildPop(null);
            }

            if (right_type_def != .Integer and right_type_def != .Float) {
                try self.buildPop(null);
            }

            return res;
        },
    };
}

fn genereateConditional(self: *Self, binary_node: *n.BinaryNode) Error!?m.MIR_op_t {
    const value = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("value", m.MIR_T_I64),
    );

    self.MOV(
        value,
        (try self.generateNode(binary_node.left)).?,
    );

    const out_label = m.MIR_new_label(self.ctx);

    self.BNE(
        out_label,
        value,
        m.MIR_new_uint_op(
            self.ctx,
            switch (binary_node.operator) {
                .QuestionQuestion => v.Value.Null.val,
                .And => v.Value.True.val,
                .Or => v.Value.False.val,
                else => unreachable,
            },
        ),
    );

    self.MOV(
        value,
        (try self.generateNode(binary_node.right)).?,
    );

    self.append(out_label);

    return value;
}

fn generateBitwise(self: *Self, binary_node: *n.BinaryNode) Error!?m.MIR_op_t {
    const res = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("res", m.MIR_T_I64),
    );
    const left = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("left", m.MIR_T_I64),
    );
    const right = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("right", m.MIR_T_I64),
    );

    self.unwrap(
        .Integer,
        (try self.generateNode(binary_node.left)).?,
        left,
    );
    self.unwrap(
        .Integer,
        (try self.generateNode(binary_node.right)).?,
        right,
    );

    switch (binary_node.operator) {
        .Ampersand => self.AND(res, left, right),
        .Bor => self.OR(res, left, right),
        .Xor => self.XOR(res, left, right),
        .ShiftLeft => self.SHL(res, left, right),
        .ShiftRight => self.SHR(res, left, right),
        else => unreachable,
    }

    self.wrap(.Integer, res, res);

    return res;
}

fn generateWhile(self: *Self, while_node: *n.WhileNode) Error!?m.MIR_op_t {
    if (while_node.condition.isConstant(while_node.condition) and !(while_node.condition.toValue(while_node.condition, self.vm.gc) catch @panic("Could not fold while loop")).boolean()) {
        return null;
    }

    const cond_label = m.MIR_new_label(self.ctx);
    const out_label = m.MIR_new_label(self.ctx);

    const previous_out_label = self.state.?.break_label;
    self.state.?.break_label = out_label;
    const previous_continue_label = self.state.?.continue_label;
    self.state.?.continue_label = cond_label;

    self.append(cond_label);

    self.BEQ(
        m.MIR_new_label_op(self.ctx, out_label),
        (try self.generateNode(while_node.condition)).?,
        m.MIR_new_uint_op(self.ctx, v.Value.False.val),
    );

    _ = try self.generateNode(while_node.block);

    self.JMP(cond_label);

    self.append(out_label);

    self.state.?.break_label = previous_out_label;
    self.state.?.continue_label = previous_continue_label;

    return null;
}

fn generateDoUntil(self: *Self, do_until_node: *n.DoUntilNode) Error!?m.MIR_op_t {
    const out_label = m.MIR_new_label(self.ctx);
    const loop_label = m.MIR_new_label(self.ctx);

    const previous_out_label = self.state.?.break_label;
    self.state.?.break_label = out_label;
    const previous_continue_label = self.state.?.continue_label;
    self.state.?.continue_label = loop_label;

    self.append(loop_label);

    _ = try self.generateNode(do_until_node.block);

    self.BEQ(
        m.MIR_new_label_op(self.ctx, loop_label),
        (try self.generateNode(do_until_node.condition)).?,
        m.MIR_new_uint_op(self.ctx, v.Value.False.val),
    );

    self.append(out_label);

    self.state.?.break_label = previous_out_label;
    self.state.?.continue_label = previous_continue_label;

    return null;
}

fn generateFor(self: *Self, for_node: *n.ForNode) Error!?m.MIR_op_t {
    if (for_node.condition.isConstant(for_node.condition) and !(for_node.condition.toValue(for_node.condition, self.vm.gc) catch @panic("Could not fold for loop")).boolean()) {
        return null;
    }

    const cond_label = m.MIR_new_label(self.ctx);
    const out_label = m.MIR_new_label(self.ctx);
    const previous_out_label = self.state.?.break_label;
    self.state.?.break_label = out_label;
    const previous_continue_label = self.state.?.continue_label;
    self.state.?.continue_label = cond_label;

    // Init expressions
    for (for_node.init_declarations.items) |expr| {
        _ = try self.generateNode(&expr.node);
    }

    // Condition
    self.append(cond_label);

    self.BEQ(
        m.MIR_new_label_op(self.ctx, out_label),
        (try self.generateNode(for_node.condition)).?,
        m.MIR_new_uint_op(self.ctx, v.Value.False.val),
    );

    _ = try self.generateNode(for_node.body);

    // Post loop
    for (for_node.post_loop.items) |expr| {
        _ = try self.generateNode(expr);
    }

    self.JMP(cond_label);

    self.append(out_label);

    self.state.?.break_label = previous_out_label;
    self.state.?.continue_label = previous_continue_label;

    return null;
}

fn generateBreak(self: *Self, break_node: *n.ParseNode) Error!?m.MIR_op_t {
    try self.closeScope(break_node);

    self.JMP(self.state.?.break_label.?);

    return null;
}

fn generateContinue(self: *Self, continue_node: *n.ParseNode) Error!?m.MIR_op_t {
    try self.closeScope(continue_node);

    self.JMP(self.state.?.continue_label.?);

    return null;
}

fn generateList(self: *Self, list_node: *n.ListNode) Error!?m.MIR_op_t {
    const new_list = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("new_list", m.MIR_T_I64),
    );

    try self.buildExternApiCall(
        .bz_newList,
        new_list,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            m.MIR_new_uint_op(self.ctx, list_node.node.type_def.?.resolved_type.?.List.item_type.toValue().val),
        },
    );

    // Prevent collection
    try self.buildPush(new_list);

    for (list_node.items) |item| {
        try self.buildExternApiCall(
            .bz_listAppend,
            null,
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                new_list,
                (try self.generateNode(item)).?,
            },
        );
    }

    try self.buildPop(null);

    return new_list;
}

fn generateMap(self: *Self, map_node: *n.MapNode) Error!?m.MIR_op_t {
    const new_map = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("new_map", m.MIR_T_I64),
    );

    try self.buildExternApiCall(
        .bz_newMap,
        new_map,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            m.MIR_new_uint_op(self.ctx, map_node.node.type_def.?.toValue().val),
        },
    );

    // Prevent collection
    try self.buildPush(new_map);

    for (map_node.keys, 0..) |key, index| {
        try self.buildExternApiCall(
            .bz_mapSet,
            null,
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                new_map,
                (try self.generateNode(key)).?,
                (try self.generateNode(map_node.values[index])).?,
            },
        );
    }

    try self.buildPop(null);

    return new_map;
}

fn generateDot(self: *Self, dot_node: *n.DotNode) Error!?m.MIR_op_t {
    const callee_type = dot_node.callee.type_def.?;

    switch (callee_type.def_type) {
        .Fiber => {
            if (dot_node.call) |call| {
                return try self.generateCall(call);
            }

            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );
            try self.buildExternApiCall(
                .bz_getFiberField,
                res,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    (try self.generateNode(dot_node.callee)).?,
                    m.MIR_new_uint_op(self.ctx, (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val),
                    m.MIR_new_uint_op(self.ctx, 1),
                },
            );

            return res;
        },

        .Pattern => {
            if (dot_node.call) |call| {
                return try self.generateCall(call);
            }

            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );
            try self.buildExternApiCall(
                .bz_getPatternField,
                res,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    (try self.generateNode(dot_node.callee)).?,
                    m.MIR_new_uint_op(self.ctx, (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val),
                    m.MIR_new_uint_op(self.ctx, 1),
                },
            );

            return res;
        },

        .String => {
            if (dot_node.call) |call| {
                return try self.generateCall(call);
            }

            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );
            try self.buildExternApiCall(
                .bz_getStringField,
                res,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    (try self.generateNode(dot_node.callee)).?,
                    m.MIR_new_uint_op(self.ctx, (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val),
                    m.MIR_new_uint_op(self.ctx, 1),
                },
            );

            return res;
        },

        .Object => {
            if (dot_node.call) |call| {
                return try self.generateCall(call);
            }

            if (dot_node.value) |value| {
                const gen_value = (try self.generateNode(value)).?;

                try self.buildExternApiCall(
                    .bz_setObjectField,
                    null,
                    &[_]m.MIR_op_t{
                        m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        (try self.generateNode(dot_node.callee)).?,
                        m.MIR_new_uint_op(self.ctx, (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val),
                        gen_value,
                    },
                );

                return gen_value;
            }

            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );
            try self.buildExternApiCall(
                .bz_getObjectField,
                res,
                &[_]m.MIR_op_t{
                    (try self.generateNode(dot_node.callee)).?,
                    m.MIR_new_uint_op(self.ctx, (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val),
                },
            );

            return res;
        },

        .ObjectInstance, .ProtocolInstance => {
            if (dot_node.call) |call| {
                return try self.generateCall(call);
            }

            if (dot_node.value) |value| {
                const gen_value = (try self.generateNode(value)).?;

                try self.buildExternApiCall(
                    .bz_setInstanceField,
                    null,
                    &[_]m.MIR_op_t{
                        m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        (try self.generateNode(dot_node.callee)).?,
                        m.MIR_new_uint_op(self.ctx, (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val),
                        gen_value,
                    },
                );

                return gen_value;
            }

            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );
            try self.buildExternApiCall(
                .bz_getInstanceField,
                res,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    (try self.generateNode(dot_node.callee)).?,
                    m.MIR_new_uint_op(self.ctx, (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val),
                    m.MIR_new_uint_op(self.ctx, 1),
                },
            );

            return res;
        },

        .Enum => {
            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );
            try self.buildExternApiCall(
                .bz_getEnumCase,
                res,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    (try self.generateNode(dot_node.callee)).?,
                    m.MIR_new_uint_op(self.ctx, (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val),
                },
            );

            return res;
        },

        .EnumInstance => {
            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );
            try self.buildExternApiCall(
                .bz_getEnumCaseValue,
                res,
                &[_]m.MIR_op_t{
                    (try self.generateNode(dot_node.callee)).?,
                },
            );

            return res;
        },

        .List => {
            if (dot_node.call) |call| {
                return try self.generateCall(call);
            }

            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );
            try self.buildExternApiCall(
                .bz_getListField,
                res,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    (try self.generateNode(dot_node.callee)).?,
                    m.MIR_new_uint_op(self.ctx, (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val),
                    m.MIR_new_uint_op(self.ctx, 1),
                },
            );

            return res;
        },

        .Map => {
            if (dot_node.call) |call| {
                return try self.generateCall(call);
            }

            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );
            try self.buildExternApiCall(
                .bz_getMapField,
                res,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    (try self.generateNode(dot_node.callee)).?,
                    m.MIR_new_uint_op(self.ctx, (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val),
                    m.MIR_new_uint_op(self.ctx, 1),
                },
            );

            return res;
        },

        else => unreachable,
    }
}

fn generateSubscript(self: *Self, subscript_node: *n.SubscriptNode) Error!?m.MIR_op_t {
    const subscripted = (try self.generateNode(subscript_node.subscripted)).?;
    const index_val = (try self.generateNode(subscript_node.index)).?;
    const value = if (subscript_node.value) |val| (try self.generateNode(val)).? else null;

    switch (subscript_node.subscripted.type_def.?.def_type) {
        .List => {
            const index = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("index", m.MIR_T_I64),
            );

            self.unwrap(.Integer, index_val, index);

            if (value) |val| {
                try self.buildExternApiCall(
                    .bz_listSet,
                    null,
                    &[_]m.MIR_op_t{
                        m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        subscripted,
                        index,
                        val,
                    },
                );

                return subscripted;
            }

            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );

            try self.buildExternApiCall(
                .bz_listGet,
                res,
                &[_]m.MIR_op_t{
                    subscripted,
                    index,
                },
            );

            return res;
        },
        .String => {
            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );

            try self.buildExternApiCall(
                .bz_objStringSubscript,
                res,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    subscripted,
                    index_val,
                },
            );

            return res;
        },
        .Map => {
            if (value) |val| {
                try self.buildExternApiCall(
                    .bz_mapSet,
                    null,
                    &[_]m.MIR_op_t{
                        m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        subscripted,
                        index_val,
                        val,
                    },
                );

                return subscripted;
            }

            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );

            try self.buildExternApiCall(
                .bz_mapGet,
                res,
                &[_]m.MIR_op_t{
                    subscripted,
                    index_val,
                },
            );

            return res;
        },
        else => unreachable,
    }
}

fn generateIs(self: *Self, is_node: *n.IsNode) Error!?m.MIR_op_t {
    const res = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("res", m.MIR_T_I64),
    );

    try self.buildExternApiCall(
        .bz_valueIs,
        res,
        &[_]m.MIR_op_t{
            (try self.generateNode(is_node.left)).?,
            m.MIR_new_uint_op(self.ctx, is_node.constant.val),
        },
    );

    return res;
}

fn generateTry(self: *Self, try_node: *n.TryNode) Error!?m.MIR_op_t {
    const raise_label = m.MIR_new_label(self.ctx);
    const out_label = m.MIR_new_label(self.ctx);
    const catch_label = m.MIR_new_label(self.ctx);
    var clause_labels = std.ArrayList(m.MIR_insn_t).init(self.vm.gc.allocator);
    defer clause_labels.deinit();

    for (try_node.clauses.keys()) |_| {
        try clause_labels.append(
            m.MIR_new_label(self.ctx),
        );
    }

    const unconditional_label = if (try_node.unconditional_clause != null)
        m.MIR_new_label(self.ctx)
    else
        null;

    const index = try self.REG("index", m.MIR_T_I64);
    const stack_top_ptr_base = try self.REG("stack_top_ptr_base", m.MIR_T_I64);

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    // *[*]Value
    const stack_top_ptr = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_P,
        @offsetOf(o.NativeCtx, "stack_top"),
        self.state.?.ctx_reg.?,
        index,
        1,
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, stack_top_ptr_base),
        stack_top_ptr,
    );

    // [*]Value
    const stack_top = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("stack_top", m.MIR_T_I64),
    );

    self.MOV(
        stack_top,
        m.MIR_new_mem_op(
            self.ctx,
            m.MIR_T_P,
            0,
            stack_top_ptr_base,
            index,
            1,
        ),
    );

    // Set it as current jump env
    const try_ctx = m.MIR_new_reg_op(self.ctx, try self.REG("try_ctx", m.MIR_T_I64));
    try self.buildExternApiCall(
        .bz_setTryCtx,
        try_ctx,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
        },
    );

    const env = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("env", m.MIR_T_I64),
    );
    self.ADD(
        env,
        try_ctx,
        m.MIR_new_uint_op(self.ctx, @offsetOf(r.TryCtx, "env")),
    );

    const status = try self.REG("status", m.MIR_T_I64);
    try self.buildExternApiCall(
        .setjmp,
        m.MIR_new_reg_op(self.ctx, status),
        &[_]m.MIR_op_t{env},
    );

    self.BEQ(
        m.MIR_new_label_op(self.ctx, catch_label),
        m.MIR_new_reg_op(self.ctx, status),
        m.MIR_new_int_op(self.ctx, 1),
    );

    _ = try self.generateNode(try_node.body);

    self.JMP(out_label);

    self.append(catch_label);

    const payload = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("payload", m.MIR_T_I64),
    );
    try self.buildPop(payload);

    // Get stack top as it was before try block
    // Close upvalues up to it
    try self.buildExternApiCall(
        .bz_closeUpValues,
        null,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            stack_top,
        },
    );

    // Restore stack top as it was before the try block
    self.MOV(
        try self.LOAD(stack_top_ptr),
        stack_top,
    );

    // Put error back on stack
    try self.buildPush(payload);

    self.JMP(if (clause_labels.items.len > 0)
        clause_labels.items[0]
    else
        unconditional_label.?);

    for (try_node.clauses.keys(), 0..) |type_def, idx| {
        const label = clause_labels.items[idx];
        const clause = try_node.clauses.get(type_def).?;

        self.append(label);

        // Get error payload from stack
        const err_payload = try self.REG("err_paylaod", m.MIR_T_I64);
        try self.buildPeek(
            0,
            m.MIR_new_reg_op(self.ctx, err_payload),
        );

        const matches = try self.REG("matches", m.MIR_T_I64);
        try self.buildExternApiCall(
            .bz_valueIs,
            m.MIR_new_reg_op(self.ctx, matches),
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, err_payload),
                m.MIR_new_uint_op(self.ctx, type_def.toValue().val),
            },
        );

        self.unwrap(
            .Bool,
            m.MIR_new_reg_op(self.ctx, matches),
            m.MIR_new_reg_op(self.ctx, matches),
        );

        self.BEQ(
            m.MIR_new_label_op(
                self.ctx,
                if (index < try_node.clauses.keys().len - 1)
                    clause_labels.items[index + 1]
                else if (unconditional_label) |unconditional|
                    unconditional
                else
                    raise_label,
            ),
            m.MIR_new_reg_op(self.ctx, matches),
            m.MIR_new_uint_op(self.ctx, 0),
        );

        // Unwind TryCtx
        try self.buildExternApiCall(
            .bz_popTryCtx,
            null,
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            },
        );

        _ = try self.generateNode(clause);

        self.JMP(out_label);
    }

    if (unconditional_label) |label| {
        self.append(label);

        // Unwind TryCtx
        try self.buildExternApiCall(
            .bz_popTryCtx,
            null,
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            },
        );

        _ = try self.generateNode(try_node.unconditional_clause.?);

        self.JMP(out_label);
    }

    self.append(raise_label);

    // Unwind TryCtx
    try self.buildExternApiCall(
        .bz_popTryCtx,
        null,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
        },
    );

    // Raise error again
    try self.buildExternApiCall(
        .bz_rethrow,
        null,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
        },
    );

    self.append(out_label);

    // Unwind TryCtx
    try self.buildExternApiCall(
        .bz_popTryCtx,
        null,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
        },
    );

    return null;
}

fn generateThrow(self: *Self, throw_node: *n.ThrowNode) Error!?m.MIR_op_t {
    try self.buildExternApiCall(
        .bz_throw,
        null,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            (try self.generateNode(throw_node.error_value)).?,
        },
    );

    return null;
}

fn generateUnwrap(self: *Self, unwrap_node: *n.UnwrapNode) Error!?m.MIR_op_t {
    const value = (try self.generateNode(unwrap_node.unwrapped)).?;

    // Remember that we need to had a terminator to this block that will jump at the end of the optionals chain
    if (self.state.?.opt_jump == null) {
        self.state.?.opt_jump = .{
            // Store the value on the stack, that spot will be overwritten with the final value of the optional chain
            .alloca = try self.REG("opt", m.MIR_T_I64),
            .current_insn = std.ArrayList(m.MIR_insn_t).init(self.vm.gc.allocator),
        };
    }

    const current_insn = m.MIR_new_insn(
        self.ctx,
        m.MIR_MOV,
        m.MIR_new_reg_op(self.ctx, self.state.?.opt_jump.?.alloca),
        value,
    );

    self.append(
        current_insn,
    );

    try self.state.?.opt_jump.?.current_insn.append(current_insn);

    return value;
}

fn generateObjectInit(self: *Self, object_init_node: *n.ObjectInitNode) Error!?m.MIR_op_t {
    const object = if (object_init_node.object) |node|
        (try self.generateNode(node)).?
    else
        m.MIR_new_uint_op(self.ctx, v.Value.Null.val);

    const typedef = if (object_init_node.object == null)
        m.MIR_new_uint_op(self.ctx, object_init_node.node.type_def.?.toValue().val)
    else
        m.MIR_new_uint_op(self.ctx, v.Value.Null.val);

    const instance = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("instance", m.MIR_T_I64),
    );
    try self.buildExternApiCall(
        .bz_instance,
        instance,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            object,
            typedef,
        },
    );

    // Push to prevent collection
    try self.buildPush(instance);

    for (object_init_node.properties.keys()) |property_name| {
        const value = object_init_node.properties.get(property_name).?;

        try self.buildExternApiCall(
            .bz_setInstanceField,
            null,
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                instance,
                m.MIR_new_uint_op(self.ctx, (try self.vm.gc.copyString(property_name)).toValue().val),
                (try self.generateNode(value)).?,
            },
        );
    }

    try self.buildPop(instance);

    return instance;
}

fn generateForceUnwrap(self: *Self, force_unwrap_node: *n.ForceUnwrapNode) Error!?m.MIR_op_t {
    const expr = (try self.generateNode(force_unwrap_node.unwrapped)).?;

    const out_label = m.MIR_new_label(self.ctx);

    self.BNE(
        out_label,
        expr,
        m.MIR_new_uint_op(self.ctx, v.Value.Null.val),
    );

    try self.buildExternApiCall(
        .bz_throw,
        null,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            m.MIR_new_uint_op(self.ctx, (try self.vm.gc.copyString("Force unwrapped optional is null")).toValue().val),
        },
    );

    self.append(out_label);

    return expr;
}

fn generateUnary(self: *Self, unary_node: *n.UnaryNode) Error!?m.MIR_op_t {
    const left = (try self.generateNode(unary_node.left)).?;
    const result = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("result", m.MIR_T_I64),
    );

    switch (unary_node.operator) {
        .Bnot => {
            self.unwrap(.Integer, left, result);
            self.NOTS(result, result);
            self.wrap(.Integer, result, result);
        },
        .Bang => {
            self.unwrap(.Bool, left, result);

            const true_label = m.MIR_new_label(self.ctx);
            const out_label = m.MIR_new_label(self.ctx);

            self.BEQ(
                m.MIR_new_label_op(self.ctx, out_label),
                result,
                m.MIR_new_uint_op(self.ctx, 1),
            );

            self.MOV(
                result,
                m.MIR_new_uint_op(self.ctx, v.Value.True.val),
            );

            self.JMP(out_label);

            self.append(true_label);

            self.MOV(
                result,
                m.MIR_new_uint_op(self.ctx, v.Value.False.val),
            );

            self.append(out_label);
        },
        .Minus => {
            self.unwrap(.Integer, left, result);

            if (unary_node.left.type_def.?.def_type == .Integer) {
                self.NEGS(result, result);
            } else {
                self.DNEG(result, result);
            }

            self.wrap(
                unary_node.left.type_def.?.def_type,
                result,
                result,
            );
        },
        else => unreachable,
    }

    return result;
}

fn generatePattern(self: *Self, pattern_node: *n.PatternNode) Error!?m.MIR_op_t {
    return m.MIR_new_uint_op(self.ctx, pattern_node.constant.toValue().val);
}

fn generateForEach(self: *Self, foreach_node: *n.ForEachNode) Error!?m.MIR_op_t {
    // If iteratble is empty constant, skip the node
    if (foreach_node.iterable.isConstant(foreach_node.iterable)) {
        const iterable = (foreach_node.iterable.toValue(foreach_node.iterable, self.vm.gc) catch @panic("Could not compile foreach loop")).obj();

        if (switch (iterable.obj_type) {
            .List => o.ObjList.cast(iterable).?.items.items.len == 0,
            .Map => o.ObjMap.cast(iterable).?.map.count() == 0,
            .String => o.ObjString.cast(iterable).?.string.len == 0,
            .Enum => o.ObjEnum.cast(iterable).?.cases.items.len == 0,
            else => unreachable,
        }) {
            return null;
        }
    }

    // key, value and iterable are locals of the foreach scope
    if (foreach_node.key) |key| {
        // var declaration so will push value on stack
        _ = try self.generateNode(&key.node);
    }
    // var declaration so will push value on stack
    _ = try self.generateNode(&foreach_node.value.node);
    const iterable = (try self.generateNode(foreach_node.iterable)).?;
    try self.buildPush(iterable);

    const key_ptr = try self.buildStackPtr(2);
    const value_ptr = try self.buildStackPtr(1);

    const cond_label = m.MIR_new_label(self.ctx);
    const out_label = m.MIR_new_label(self.ctx);

    const previous_out_label = self.state.?.break_label;
    self.state.?.break_label = out_label;
    const previous_continue_label = self.state.?.continue_label;
    self.state.?.continue_label = cond_label;

    self.append(cond_label);

    // Call appropriate `next` method
    if (foreach_node.iterable.type_def.?.def_type == .Fiber) {
        // TODO: fiber foreach (tricky, need to complete foreach op after it has yielded)
        unreachable;
    } else if (foreach_node.iterable.type_def.?.def_type == .Enum) {
        try self.buildExternApiCall(
            .bz_enumNext,
            try self.LOAD(value_ptr),
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                iterable,
                try self.LOAD(value_ptr),
            },
        );

        // If next key is null stop, otherwise do loop
        self.BEQ(
            m.MIR_new_label_op(self.ctx, out_label),
            try self.LOAD(value_ptr),
            m.MIR_new_uint_op(self.ctx, v.Value.Null.val),
        );
    } else {
        // The `next` method will store the new key in the key local
        try self.buildExternApiCall(
            switch (foreach_node.iterable.type_def.?.def_type) {
                .String => .bz_stringNext,
                .List => .bz_listNext,
                .Map => .bz_mapNext,
                else => unreachable,
            },
            try self.LOAD(value_ptr),
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                iterable,
                // Pass ptr so the method can put he new key in it
                key_ptr,
            },
        );

        // If next key is null stop, otherwise loop
        self.BEQ(
            m.MIR_new_label_op(self.ctx, out_label),
            try self.LOAD(key_ptr),
            m.MIR_new_uint_op(self.ctx, v.Value.Null.val),
        );
    }

    _ = try self.generateNode(foreach_node.block);

    self.JMP(cond_label);

    self.append(out_label);

    self.state.?.break_label = previous_out_label;
    self.state.?.continue_label = previous_continue_label;

    return null;
}

fn generateBlock(self: *Self, block_node: *n.BlockNode) Error!?m.MIR_op_t {
    for (block_node.statements.items) |statement| {
        _ = try self.generateNode(statement);
    }

    return null;
}

fn generateFunDeclaration(self: *Self, fun_declaration_node: *n.FunDeclarationNode) Error!?m.MIR_op_t {
    return try self.generateFunction(fun_declaration_node.function);
}

fn generateVarDeclaration(self: *Self, var_declaration_node: *n.VarDeclarationNode) Error!?m.MIR_op_t {
    // We should only declare locals
    assert(var_declaration_node.slot_type == .Local);

    try self.buildPush(
        if (var_declaration_node.value) |value|
            (try self.generateNode(value)).?
        else
            m.MIR_new_uint_op(
                self.ctx,
                v.Value.Null.val,
            ),
    );

    return null;
}

fn generateFunction(self: *Self, function_node: *n.FunctionNode) Error!?m.MIR_op_t {
    const root_node = @ptrCast(*n.FunctionNode, @alignCast(@alignOf(n.FunctionNode), self.state.?.function_node));

    const function_def = function_node.node.type_def.?.resolved_type.?.Function;
    const function_type = function_def.function_type;

    // Those are not allowed to be compiled
    assert(function_type != .Extern and function_type != .Script and function_type != .ScriptEntryPoint);

    // Get fully qualified name of function
    var qualified_name = try self.getFunctionQualifiedName(
        function_node,
        true,
    );
    defer qualified_name.deinit();

    // If this is not the root function, we need to compile this later
    if (root_node != function_node) {
        var nativefn_qualified_name = try self.getFunctionQualifiedName(function_node, false);
        defer nativefn_qualified_name.deinit();

        // Remember that we need to compile this function later
        try self.functions_queue.put(
            @ptrCast(
                *n.FunctionNode,
                @alignCast(@alignOf(n.FunctionNode), function_node),
            ),
            null,
        );

        // For now declare it
        const native_raw = m.MIR_new_import(self.ctx, @ptrCast([*:0]u8, qualified_name.items.ptr));
        const native = m.MIR_new_import(self.ctx, @ptrCast([*:0]u8, nativefn_qualified_name.items.ptr));

        // Call bz_closure
        const dest = m.MIR_new_reg_op(
            self.ctx,
            try self.REG("result", m.MIR_T_I64),
        );

        try self.buildExternApiCall(
            .bz_closure,
            dest,
            &[_]m.MIR_op_t{
                // ctx
                m.MIR_new_reg_op(self.ctx, self.state.?.ctx_reg.?),
                // function_node
                m.MIR_new_uint_op(self.ctx, @ptrToInt(function_node)),
                m.MIR_new_ref_op(self.ctx, native),
                m.MIR_new_ref_op(self.ctx, native_raw),
            },
        );

        return dest;
    }

    // FIXME: FIME: I don't get why we need this: a simple constant becomes rubbish as soon as we enter MIR_new_func_arr if we don't
    var ctx_name = std.ArrayList(u8).init(self.vm.gc.allocator);
    try ctx_name.writer().print("{s}\u{0}", .{"ctx"});
    defer ctx_name.deinit();
    const function = m.MIR_new_func_arr(
        self.ctx,
        @ptrCast([*:0]u8, qualified_name.items.ptr),
        1,
        &[_]m.MIR_type_t{m.MIR_T_U64},
        1,
        &[_]m.MIR_var_t{
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast([*:0]u8, ctx_name.items.ptr),
                .size = undefined,
            },
        },
    );

    self.state.?.function = function;

    // Build ref to ctx arg and vm
    self.state.?.ctx_reg = m.MIR_reg(self.ctx, "ctx", function.u.func);
    self.state.?.vm_reg = m.MIR_new_func_reg(self.ctx, function.u.func, m.MIR_T_I64, "vm");

    const index = try self.REG("index", m.MIR_T_I64);
    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );
    self.MOV(
        m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
        m.MIR_new_mem_op(
            self.ctx,
            m.MIR_T_P,
            @offsetOf(o.NativeCtx, "vm"),
            self.state.?.ctx_reg.?,
            index,
            0,
        ),
    );

    if (function_node.arrow_expr) |arrow_expr| {
        try self.buildReturn((try self.generateNode(arrow_expr)).?);

        self.state.?.return_emitted = true;
    } else {
        _ = try self.generateNode(function_node.body.?.toNode());
    }

    if (self.state.?.function_node.node.type_def.?.resolved_type.?.Function.return_type.def_type == .Void and !self.state.?.return_emitted) {
        try self.buildReturn(m.MIR_new_uint_op(self.ctx, v.Value.Void.val));
    }

    m.MIR_finish_func(self.ctx);

    // Add the NativeFn version of the function
    const native_fn = try self.generateNativeFn(function_node, function);

    try self.functions_queue.put(
        function_node,
        [_]m.MIR_item_t{
            native_fn,
            self.state.?.function.?,
        },
    );

    return m.MIR_new_ref_op(self.ctx, function);
}

fn generateNativeFn(self: *Self, function_node: *n.FunctionNode, raw_fn: m.MIR_item_t) !m.MIR_item_t {
    const function_def = function_node.node.type_def.?.resolved_type.?.Function;
    const function_type = function_def.function_type;

    assert(function_type != .Extern);

    var nativefn_qualified_name = try self.getFunctionQualifiedName(function_node, false);
    defer nativefn_qualified_name.deinit();

    // FIXME: I don't get why we need this: a simple constant becomes rubbish as soon as we enter MIR_new_func_arr if we don't
    var ctx_name = std.ArrayList(u8).init(self.vm.gc.allocator);
    try ctx_name.writer().print("{s}\u{0}", .{"ctx"});
    defer ctx_name.deinit();
    const function = m.MIR_new_func_arr(
        self.ctx,
        @ptrCast([*:0]u8, nativefn_qualified_name.items.ptr),
        1,
        &[_]m.MIR_type_t{m.MIR_T_I64},
        1,
        &[_]m.MIR_var_t{
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast([*:0]u8, ctx_name.items.ptr),
                .size = undefined,
            },
        },
    );

    const previous = self.state.?.function;
    self.state.?.function = function;
    defer self.state.?.function = previous;

    const ctx_reg = m.MIR_reg(self.ctx, "ctx", function.u.func);
    if (function_type == .Test or (function_def.error_types != null and function_def.error_types.?.len > 0)) {
        const index = try self.REG("index", m.MIR_T_I64);
        self.MOV(
            m.MIR_new_reg_op(self.ctx, index),
            m.MIR_new_uint_op(self.ctx, 0),
        );

        const vm_reg = try self.REG("vm", m.MIR_T_I64);
        self.MOV(
            m.MIR_new_reg_op(self.ctx, vm_reg),
            m.MIR_new_mem_op(
                self.ctx,
                m.MIR_T_P,
                @offsetOf(o.NativeCtx, "vm"),
                ctx_reg,
                index,
                0,
            ),
        );

        // Catch any error to forward them as a buzz error (push payload + return -1)
        // Set it as current jump env
        const try_ctx = m.MIR_new_reg_op(
            self.ctx,
            try self.REG("try_ctx", m.MIR_T_I64),
        );

        try self.buildExternApiCall(
            .bz_setTryCtx,
            try_ctx,
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, vm_reg),
            },
        );

        const env = m.MIR_new_reg_op(
            self.ctx,
            try self.REG("env", m.MIR_T_I64),
        );

        self.ADD(
            env,
            try_ctx,
            m.MIR_new_uint_op(
                self.ctx,
                @offsetOf(r.TryCtx, "env"),
            ),
        );

        // setjmp
        const status = m.MIR_new_reg_op(
            self.ctx,
            try self.REG("status", m.MIR_T_I64),
        );
        try self.buildExternApiCall(
            .setjmp,
            status,
            &[_]m.MIR_op_t{env},
        );

        const fun_label = m.MIR_new_label(self.ctx);

        // If status is 0, go to body, else go to catch clauses
        self.BEQ(
            m.MIR_new_label_op(self.ctx, fun_label),
            status,
            m.MIR_new_uint_op(self.ctx, 0),
        );

        // Payload already on stack so juste return -1;
        self.append(
            m.MIR_new_ret_insn(
                self.ctx,
                1,
                m.MIR_new_int_op(self.ctx, -1),
            ),
        );

        self.append(fun_label);
    }

    // Call the raw function
    const result = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("result", m.MIR_T_I64),
    );
    m.MIR_append_insn(
        self.ctx,
        function,
        m.MIR_new_insn_arr(
            self.ctx,
            m.MIR_CALL,
            4,
            &[_]m.MIR_op_t{
                m.MIR_new_ref_op(self.ctx, try ExternApi.rawfn.declare(self)),
                m.MIR_new_ref_op(self.ctx, raw_fn),
                result,
                m.MIR_new_reg_op(self.ctx, ctx_reg),
            },
        ),
    );

    const should_return = function_def.return_type.def_type != .Void;

    // Push its result back into the VM
    if (should_return) {
        try self.buildPush(result);
    }

    self.RET(
        m.MIR_new_int_op(
            self.ctx,
            if (should_return) 1 else 0,
        ),
    );

    m.MIR_finish_func(self.ctx);

    return function;
}

fn getFunctionQualifiedName(self: *Self, function_node: *n.FunctionNode, raw: bool) !std.ArrayList(u8) {
    const function_def = function_node.node.type_def.?.resolved_type.?.Function;
    const function_type = function_def.function_type;
    const name = function_def.name.string;

    var qualified_name = std.ArrayList(u8).init(self.vm.gc.allocator);

    try qualified_name.appendSlice(name);

    // Main and script are not allowed to be compiled
    assert(function_type != .ScriptEntryPoint and function_type != .Script);

    // Don't qualify extern functions
    if (function_type != .Extern) {
        try qualified_name.append('.');
        try qualified_name.writer().print("{}", .{function_node.id});
    }
    if (function_type != .Extern and raw) {
        try qualified_name.appendSlice(".raw");
    }
    try qualified_name.append(0);

    return qualified_name;
}

// MIR helper functions
inline fn LOAD(self: *Self, ptr: m.MIR_op_t) !m.MIR_op_t {
    const reg = if (ptr.mode == m.MIR_OP_REG)
        ptr.u.reg
    else
        try self.REG("ptr", m.MIR_T_I64);

    if (ptr.mode != m.MIR_OP_REG) {
        self.MOV(
            m.MIR_new_reg_op(self.ctx, reg),
            ptr,
        );
    }

    return m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_U64,
        0,
        reg,
        0,
        0,
    );
}

inline fn append(self: *Self, inst: m.MIR_insn_t) void {
    m.MIR_append_insn(
        self.ctx,
        self.state.?.function.?,
        inst,
    );
}

inline fn MOV(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_MOV,
            dest,
            value,
        ),
    );
}

inline fn DMOV(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DMOV,
            dest,
            value,
        ),
    );
}

inline fn EQ(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_EQ,
            dest,
            left,
            right,
        ),
    );
}

inline fn EQS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_EQS,
            dest,
            left,
            right,
        ),
    );
}

inline fn DEQ(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DEQ,
            dest,
            left,
            right,
        ),
    );
}

inline fn GT(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_GT,
            dest,
            left,
            right,
        ),
    );
}

inline fn GTS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_GTS,
            dest,
            left,
            right,
        ),
    );
}

inline fn DGT(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DGT,
            dest,
            left,
            right,
        ),
    );
}

inline fn LT(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_LT,
            dest,
            left,
            right,
        ),
    );
}

inline fn LTS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_LTS,
            dest,
            left,
            right,
        ),
    );
}

inline fn DLT(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DLT,
            dest,
            left,
            right,
        ),
    );
}

inline fn GE(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_GE,
            dest,
            left,
            right,
        ),
    );
}

inline fn GES(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_GES,
            dest,
            left,
            right,
        ),
    );
}

inline fn DGE(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DGE,
            dest,
            left,
            right,
        ),
    );
}

inline fn LE(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_LE,
            dest,
            left,
            right,
        ),
    );
}

inline fn LES(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_LES,
            dest,
            left,
            right,
        ),
    );
}

inline fn DLE(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DLE,
            dest,
            left,
            right,
        ),
    );
}

inline fn NE(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_NE,
            dest,
            left,
            right,
        ),
    );
}

inline fn NES(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_NES,
            dest,
            left,
            right,
        ),
    );
}

inline fn DNE(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DNE,
            dest,
            left,
            right,
        ),
    );
}

inline fn BEQ(self: *Self, label: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_BEQ,
            label,
            left,
            right,
        ),
    );
}

inline fn BNE(self: *Self, label: m.MIR_insn_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_BNE,
            m.MIR_new_label_op(self.ctx, label),
            left,
            right,
        ),
    );
}

inline fn JMP(self: *Self, label: m.MIR_insn_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_JMP,
            m.MIR_new_label_op(self.ctx, label),
        ),
    );
}

inline fn ADD(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_ADD,
            dest,
            left,
            right,
        ),
    );
}

inline fn DADD(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DADD,
            dest,
            left,
            right,
        ),
    );
}

inline fn ADDS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_ADDS,
            dest,
            left,
            right,
        ),
    );
}

inline fn SUB(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_SUB,
            dest,
            left,
            right,
        ),
    );
}

inline fn SUBS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_SUBS,
            dest,
            left,
            right,
        ),
    );
}

inline fn DSUB(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DSUB,
            dest,
            left,
            right,
        ),
    );
}

inline fn MUL(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_MUL,
            dest,
            left,
            right,
        ),
    );
}

inline fn MULS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_MULS,
            dest,
            left,
            right,
        ),
    );
}

inline fn DMUL(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DMUL,
            dest,
            left,
            right,
        ),
    );
}

inline fn DIV(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DIV,
            dest,
            left,
            right,
        ),
    );
}

inline fn DIVS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DIVS,
            dest,
            left,
            right,
        ),
    );
}

inline fn DDIV(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DDIV,
            dest,
            left,
            right,
        ),
    );
}

inline fn MOD(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_MOD,
            dest,
            left,
            right,
        ),
    );
}

inline fn MODS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_MODS,
            dest,
            left,
            right,
        ),
    );
}

inline fn AND(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_AND,
            dest,
            left,
            right,
        ),
    );
}

inline fn ANDS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_ANDS,
            dest,
            left,
            right,
        ),
    );
}

inline fn OR(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_OR,
            dest,
            left,
            right,
        ),
    );
}

inline fn ORS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_ORS,
            dest,
            left,
            right,
        ),
    );
}

inline fn XOR(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_XOR,
            dest,
            left,
            right,
        ),
    );
}

inline fn SHL(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_LSH,
            dest,
            left,
            right,
        ),
    );
}

inline fn SHR(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_RSH,
            dest,
            left,
            right,
        ),
    );
}

inline fn NOT(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_XOR,
            dest,
            value,
            m.MIR_new_uint_op(self.ctx, std.math.maxInt(u64)),
        ),
    );
}

inline fn NOTS(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_XORS,
            dest,
            value,
            m.MIR_new_uint_op(self.ctx, std.math.maxInt(u64)),
        ),
    );
}

inline fn I2D(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_I2D,
            dest,
            value,
        ),
    );
}

inline fn D2I(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_D2I,
            dest,
            value,
        ),
    );
}

inline fn NEGS(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_NEGS,
            dest,
            value,
        ),
    );
}

inline fn DNEG(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_DNEG,
            dest,
            value,
        ),
    );
}

inline fn RET(self: *Self, return_value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_ret_insn(
            self.ctx,
            1,
            return_value,
        ),
    );
}

inline fn ALLOCA(self: *Self, reg: m.MIR_reg_t, size: usize) void {
    self.append(
        m.MIR_new_insn(
            self.ctx,
            m.MIR_ALLOCA,
            m.MIR_new_reg_op(self.ctx, reg),
            m.MIR_new_uint_op(self.ctx, size),
        ),
    );
}

fn REG(self: *Self, name: [*:0]const u8, reg_type: m.MIR_type_t) !m.MIR_reg_t {
    var actual_name = std.ArrayList(u8).init(self.vm.gc.allocator);
    defer actual_name.deinit();

    const count = self.state.?.registers.get(name) orelse 0;
    if (count > 0) {
        try actual_name.writer().print("{s}{d}\u{0}", .{ name, count + 1 });
    } else {
        try actual_name.writer().print("{s}\u{0}", .{name});
    }

    const reg = m.MIR_new_func_reg(
        self.ctx,
        self.state.?.function.?.u.func,
        reg_type,
        @ptrCast([*:0]u8, actual_name.items.ptr),
    );

    try self.state.?.registers.put(name, count + 1);

    return reg;
}

extern fn exit(c_int) void;

pub const ExternApi = enum {
    nativefn,
    rawfn,

    bz_objStringConcat,
    bz_objStringSubscript,
    bz_toString,
    bz_newList,
    bz_listAppend,
    bz_listGet,
    bz_listSet,
    bz_valueEqual,
    bz_listConcat,
    bz_newMap,
    bz_mapSet,
    bz_mapGet,
    bz_mapConcat,
    bz_valueIs,
    bz_setTryCtx,
    bz_popTryCtx,
    bz_rethrow,
    bz_throw,
    bz_closeUpValues,
    bz_getUpValue,
    bz_setUpValue,
    bz_closure,
    bz_context,
    bz_instance,
    bz_setInstanceField,
    bz_getInstanceField,
    bz_getObjectField,
    bz_setObjectField,
    bz_getStringField,
    bz_getPatternField,
    bz_getFiberField,
    bz_getEnumCase,
    bz_getEnumCaseValue,
    bz_getListField,
    bz_getMapField,
    bz_getEnumCaseFromValue,
    bz_bindMethod,
    bz_stringNext,
    bz_listNext,
    bz_mapNext,
    bz_enumNext,
    bz_clone,

    bz_dumpStack,

    // https://opensource.apple.com/source/libplatform/libplatform-161/include/setjmp.h.auto.html
    setjmp,
    // libc exit: https://man7.org/linux/man-pages/man3/exit.3.html
    exit,

    dumpInt,

    pub fn declare(self: ExternApi, jit: *Self) !m.MIR_item_t {
        const prototype = jit.state.?.prototypes.get(self) orelse self.proto(jit.ctx);

        try jit.required_ext_api.put(self, {});
        try jit.state.?.prototypes.put(
            self,
            prototype,
        );

        return prototype;
    }

    fn proto(self: ExternApi, ctx: m.MIR_context_t) m.MIR_item_t {
        return switch (self) {
            .bz_objStringSubscript => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "obj_string",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "index_value",
                        .size = undefined,
                    },
                },
            ),
            .bz_closure => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "ctx",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "function_node",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "native",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "native_raw",
                        .size = undefined,
                    },
                },
            ),
            .bz_toString, .bz_newList, .bz_newMap => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_listAppend => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_listGet, .bz_mapGet => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "index",
                        .size = undefined,
                    },
                },
            ),
            .bz_listSet, .bz_mapSet => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "index",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_valueEqual => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "other",
                        .size = undefined,
                    },
                },
            ),
            .bz_listConcat,
            .bz_mapConcat,
            .bz_objStringConcat,
            .bz_instance,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "other_list",
                        .size = undefined,
                    },
                },
            ),
            .bz_getEnumCaseFromValue, .bz_getEnumCase => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "enum",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_getEnumCaseValue => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "enum_instance",
                        .size = undefined,
                    },
                },
            ),
            .bz_getObjectField, .bz_valueIs => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "subject",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field",
                        .size = undefined,
                    },
                },
            ),
            .bz_getListField,
            .bz_getMapField,
            .bz_getStringField,
            .bz_getPatternField,
            .bz_getFiberField,
            .bz_getInstanceField,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "subject",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U8,
                        .name = "bind",
                        .size = undefined,
                    },
                },
            ),
            .bz_setInstanceField,
            .bz_setObjectField,
            .bz_bindMethod,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "instance",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_getUpValue => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "native_ctx",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "slot",
                        .size = undefined,
                    },
                },
            ),
            .bz_setUpValue => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "native_ctx",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "slot",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_closeUpValues => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "last",
                        .size = undefined,
                    },
                },
            ),
            .bz_setTryCtx => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                // *TryContext
                &[_]m.MIR_type_t{m.MIR_T_P},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                },
            ),
            .bz_popTryCtx, .bz_rethrow => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                },
            ),
            .bz_throw => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "payload",
                        .size = undefined,
                    },
                },
            ),
            .bz_context => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_P},
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "native_ctx",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "function",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "new_native_ctx",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "arg_count",
                        .size = undefined,
                    },
                },
            ),
            .setjmp => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "jmp_buf",
                        .size = undefined,
                    },
                },
            ),
            .bz_clone => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_dumpStack => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "ctx",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "offset",
                        .size = undefined,
                    },
                },
            ),
            .exit => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U8,
                        .name = "status",
                        .size = undefined,
                    },
                },
            ),
            .bz_stringNext,
            .bz_listNext,
            .bz_mapNext,
            .bz_enumNext,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "iterable",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "key",
                        .size = undefined,
                    },
                },
            ),
            .rawfn => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "ctx",
                        .size = undefined,
                    },
                },
            ),
            .nativefn => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_I16},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "ctx",
                        .size = undefined,
                    },
                },
            ),

            .dumpInt => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
        };
    }

    pub fn ptr(self: ExternApi) *anyopaque {
        return switch (self) {
            .bz_toString => @intToPtr(*anyopaque, @ptrToInt(&api.ObjString.bz_toString)),
            .bz_objStringConcat => @intToPtr(*anyopaque, @ptrToInt(&api.ObjString.bz_objStringConcat)),
            .bz_objStringSubscript => @intToPtr(*anyopaque, @ptrToInt(&api.ObjString.bz_objStringSubscript)),
            .bz_stringNext => @intToPtr(*anyopaque, @ptrToInt(&api.ObjString.bz_stringNext)),
            .bz_newList => @intToPtr(*anyopaque, @ptrToInt(&api.ObjList.bz_newList)),
            .bz_listAppend => @intToPtr(*anyopaque, @ptrToInt(&api.ObjList.bz_listAppend)),
            .bz_listGet => @intToPtr(*anyopaque, @ptrToInt(&api.ObjList.bz_listGet)),
            .bz_listSet => @intToPtr(*anyopaque, @ptrToInt(&api.ObjList.bz_listSet)),
            .bz_listConcat => @intToPtr(*anyopaque, @ptrToInt(&api.ObjList.bz_listConcat)),
            .bz_listNext => @intToPtr(*anyopaque, @ptrToInt(&api.ObjList.bz_listNext)),
            .bz_newMap => @intToPtr(*anyopaque, @ptrToInt(&api.ObjMap.bz_newMap)),
            .bz_mapGet => @intToPtr(*anyopaque, @ptrToInt(&api.ObjMap.bz_mapGet)),
            .bz_mapSet => @intToPtr(*anyopaque, @ptrToInt(&api.ObjMap.bz_mapSet)),
            .bz_mapNext => @intToPtr(*anyopaque, @ptrToInt(&api.ObjMap.bz_mapNext)),
            .bz_mapConcat => @intToPtr(*anyopaque, @ptrToInt(&api.ObjMap.bz_mapConcat)),
            .bz_valueEqual => @intToPtr(*anyopaque, @ptrToInt(&api.Value.bz_valueEqual)),
            .bz_valueIs => @intToPtr(*anyopaque, @ptrToInt(&api.Value.bz_valueIs)),
            .bz_closure => @intToPtr(*anyopaque, @ptrToInt(&api.VM.bz_closure)),
            .bz_context => @intToPtr(*anyopaque, @ptrToInt(&api.VM.bz_context)),
            .bz_instance => @intToPtr(*anyopaque, @ptrToInt(&api.ObjObject.bz_instance)),
            .bz_setInstanceField => @intToPtr(*anyopaque, @ptrToInt(&api.ObjObject.bz_setInstanceField)),
            .bz_getInstanceField => @intToPtr(*anyopaque, @ptrToInt(&api.ObjObject.bz_getInstanceField)),
            .bz_rethrow => @intToPtr(*anyopaque, @ptrToInt(&api.VM.bz_rethrow)),
            .bz_throw => @intToPtr(*anyopaque, @ptrToInt(&api.VM.bz_throw)),
            .bz_bindMethod => @intToPtr(*anyopaque, @ptrToInt(&api.VM.bz_bindMethod)),
            .bz_getUpValue => @intToPtr(*anyopaque, @ptrToInt(&api.VM.bz_getUpValue)),
            .bz_setUpValue => @intToPtr(*anyopaque, @ptrToInt(&api.VM.bz_setUpValue)),
            .bz_closeUpValues => @intToPtr(*anyopaque, @ptrToInt(&api.VM.bz_closeUpValues)),
            .bz_clone => @intToPtr(*anyopaque, @ptrToInt(&api.VM.bz_clone)),
            .bz_dumpStack => @intToPtr(*anyopaque, @ptrToInt(&api.VM.bz_dumpStack)),
            .bz_getEnumCaseFromValue => @intToPtr(*anyopaque, @ptrToInt(&api.ObjEnum.bz_getEnumCaseFromValue)),
            .bz_getEnumCase => @intToPtr(*anyopaque, @ptrToInt(&api.ObjEnum.bz_getEnumCase)),
            .bz_enumNext => @intToPtr(*anyopaque, @ptrToInt(&api.ObjEnum.bz_enumNext)),
            .bz_getEnumCaseValue => @intToPtr(*anyopaque, @ptrToInt(&api.ObjEnumInstance.bz_getEnumCaseValue)),
            .bz_setObjectField => @intToPtr(*anyopaque, @ptrToInt(&api.ObjObject.bz_setObjectField)),
            .bz_getObjectField => @intToPtr(*anyopaque, @ptrToInt(&api.ObjObject.bz_getObjectField)),
            .bz_getListField => @intToPtr(*anyopaque, @ptrToInt(&api.ObjList.bz_getListField)),
            .bz_getMapField => @intToPtr(*anyopaque, @ptrToInt(&api.ObjMap.bz_getMapField)),
            .bz_getStringField => @intToPtr(*anyopaque, @ptrToInt(&api.ObjString.bz_getStringField)),
            .bz_getPatternField => @intToPtr(*anyopaque, @ptrToInt(&api.ObjPattern.bz_getPatternField)),
            .bz_getFiberField => @intToPtr(*anyopaque, @ptrToInt(&api.ObjFiber.bz_getFiberField)),
            .bz_setTryCtx => @intToPtr(*anyopaque, @ptrToInt(&api.VM.bz_setTryCtx)),
            .bz_popTryCtx => @intToPtr(*anyopaque, @ptrToInt(&api.VM.bz_popTryCtx)),
            .setjmp => @intToPtr(
                *anyopaque,
                @ptrToInt(&(if (builtin.os.tag == .macos or builtin.os.tag == .linux) jmp._setjmp else jmp.setjmp)),
            ),
            .exit => @intToPtr(*anyopaque, @ptrToInt(&exit)),

            .dumpInt => @intToPtr(*anyopaque, @ptrToInt(&api.dumpInt)),
            else => {
                std.debug.print("{s}\n", .{self.name()});
                unreachable;
            },
        };
    }

    pub fn name(self: ExternApi) [*:0]const u8 {
        return switch (self) {
            .nativefn => "NativeFn",
            .rawfn => "RawFn",

            .bz_objStringConcat => "bz_objStringConcat",
            .bz_objStringSubscript => "bz_objStringSubscript",
            .bz_toString => "bz_toString",
            .bz_newList => "bz_newList",
            .bz_listAppend => "bz_listAppend",
            .bz_listGet => "bz_listGet",
            .bz_listSet => "bz_listSet",
            .bz_valueEqual => "bz_valueEqual",
            .bz_listConcat => "bz_listConcat",
            .bz_newMap => "bz_newMap",
            .bz_mapSet => "bz_mapSet",
            .bz_mapGet => "bz_mapGet",
            .bz_mapConcat => "bz_mapConcat",
            .bz_valueIs => "bz_valueIs",
            .bz_setTryCtx => "bz_setTryCtx",
            .bz_popTryCtx => "bz_popTryCtx",
            .bz_rethrow => "bz_rethrow",
            .bz_throw => "bz_throw",
            .bz_getUpValue => "bz_getUpValue",
            .bz_setUpValue => "bz_setUpValue",
            .bz_closeUpValues => "bz_closeUpValues",
            .bz_closure => "bz_closure",
            .bz_context => "bz_context",
            .bz_instance => "bz_instance",
            .bz_setInstanceField => "bz_setInstanceField",
            .bz_getInstanceField => "bz_getInstanceField",
            .bz_setObjectField => "bz_setObjectField",
            .bz_getObjectField => "bz_getObjectField",
            .bz_getStringField => "bz_getStringField",
            .bz_getPatternField => "bz_getPatternField",
            .bz_getFiberField => "bz_getFiberField",
            .bz_getEnumCase => "bz_getEnumCase",
            .bz_getEnumCaseValue => "bz_getEnumCaseValue",
            .bz_getListField => "bz_getListField",
            .bz_getMapField => "bz_getMapField",
            .bz_getEnumCaseFromValue => "bz_getEnumCaseFromValue",
            .bz_bindMethod => "bz_bindMethod",
            .bz_stringNext => "bz_stringNext",
            .bz_listNext => "bz_listNext",
            .bz_mapNext => "bz_mapNext",
            .bz_enumNext => "bz_enumNext",
            .bz_clone => "bz_clone",

            .setjmp => if (builtin.os.tag == .macos or builtin.os.tag == .linux) "_setjmp" else "setjmp",
            .exit => "exit",

            .bz_dumpStack => "bz_dumpStack",

            .dumpInt => "dumpInt",
        };
    }

    pub fn pname(self: ExternApi) [*:0]const u8 {
        return switch (self) {
            .nativefn => "p_NativeFn",
            .rawfn => "p_RawFn",

            .bz_objStringConcat => "p_bz_objStringConcat",
            .bz_objStringSubscript => "p_bz_objStringSubscript",
            .bz_toString => "p_bz_toString",
            .bz_newList => "p_bz_newList",
            .bz_listAppend => "p_bz_listAppend",
            .bz_listGet => "p_bz_listGet",
            .bz_listSet => "p_bz_listSet",
            .bz_valueEqual => "p_bz_valueEqual",
            .bz_listConcat => "p_bz_listConcat",
            .bz_newMap => "p_bz_newMap",
            .bz_mapSet => "p_bz_mapSet",
            .bz_mapGet => "p_bz_mapGet",
            .bz_mapConcat => "p_bz_mapConcat",
            .bz_valueIs => "p_bz_valueIs",
            .bz_setTryCtx => "p_bz_setTryCtx",
            .bz_popTryCtx => "p_bz_popTryCtx",
            .bz_rethrow => "p_bz_rethrow",
            .bz_throw => "p_bz_throw",
            .bz_getUpValue => "p_bz_getUpValue",
            .bz_setUpValue => "p_bz_setUpValue",
            .bz_closeUpValues => "p_bz_closeUpValues",
            .bz_closure => "p_bz_closure",
            .bz_context => "p_bz_context",
            .bz_instance => "p_bz_instance",
            .bz_setInstanceField => "p_bz_setInstanceField",
            .bz_getInstanceField => "p_bz_getInstanceField",
            .bz_setObjectField => "p_bz_setObjectField",
            .bz_getObjectField => "p_bz_getObjectField",
            .bz_getStringField => "p_bz_getStringField",
            .bz_getPatternField => "p_bz_getPatternField",
            .bz_getFiberField => "p_bz_getFiberField",
            .bz_getEnumCase => "p_bz_getEnumCase",
            .bz_getEnumCaseValue => "p_bz_getEnumCaseValue",
            .bz_getListField => "p_bz_getListField",
            .bz_getMapField => "p_bz_getMapField",
            .bz_getEnumCaseFromValue => "p_bz_getEnumCaseFromValue",
            .bz_bindMethod => "p_bz_bindMethod",
            .bz_stringNext => "p_bz_stringNext",
            .bz_listNext => "p_bz_listNext",
            .bz_mapNext => "p_bz_mapNext",
            .bz_enumNext => "p_bz_enumNext",
            .bz_clone => "p_bz_clone",

            .setjmp => if (builtin.os.tag == .macos or builtin.os.tag == .linux) "p__setjmp" else "p_setjmp",
            .exit => "p_exit",

            .bz_dumpStack => "p_bz_dumpStack",

            .dumpInt => "p_dumpInt",
        };
    }
};
