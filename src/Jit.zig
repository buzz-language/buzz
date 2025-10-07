const std = @import("std");
const Ast = @import("Ast.zig");
const o = @import("obj.zig");
const v = @import("value.zig");
const Value = v.Value;
const m = @import("mir.zig");
const builtin = @import("builtin");
const BuildOptions = @import("build_options");
const r = @import("vm.zig");
const VM = r.VM;
const ZigType = @import("zigtypes.zig").Type;
const ExternApi = @import("jit_extern_api.zig").ExternApi;
const api = @import("lib/buzz_api.zig");
const io = @import("io.zig");
const Chunk = @import("Chunk.zig");
const Token = @import("Token.zig");

pub const Error = error{
    CantCompile,
    UnwrappedNull,
    OutOfBound,
    ReachedMaximumMemoryUsage,
} || std.mem.Allocator.Error || std.fmt.BufPrintError;

const OptJump = struct {
    current_insn: std.ArrayList(m.MIR_insn_t),
    alloca: m.MIR_reg_t,

    pub fn deinit(self: *OptJump, allocator: std.mem.Allocator) void {
        self.current_insn.deinit(allocator);
    }
};

const Break = struct {
    break_label: m.MIR_insn_t,
    continue_label: m.MIR_insn_t,
    node: Ast.Node.Index,
};

const Breaks = std.ArrayList(Break);

const GenState = struct {
    module: m.MIR_module_t,
    prototypes: std.AutoHashMapUnmanaged(ExternApi, m.MIR_item_t) = .empty,

    /// Root closure (not necessarily the one being compiled)
    closure: *o.ObjClosure,
    opt_jumps: std.ArrayList(OptJump) = .empty,

    // Frame related stuff, since we compile one function at a time, we don't stack frames while compiling
    ast: Ast.Slice,
    ast_node: Ast.Node.Index,
    return_counts: bool = false,
    return_emitted: bool = false,

    try_should_handle: ?std.AutoHashMapUnmanaged(*o.ObjTypeDef, void) = null,

    function: ?m.MIR_item_t = null,
    function_native: ?m.MIR_item_t = null,
    function_native_proto: ?m.MIR_item_t = null,

    /// Convenience registers
    ctx_reg: ?m.MIR_reg_t = null,
    vm_reg: ?m.MIR_reg_t = null,

    /// Avoid register name collisions
    registers: std.AutoHashMapUnmanaged([*:0]const u8, usize) = .empty,

    /// Label to jump to when breaking a loop without a label
    break_label: m.MIR_insn_t = null,
    /// Label to jump to when continuing a loop whithout a label
    continue_label: m.MIR_insn_t = null,

    breaks_label: Breaks = .empty,

    pub fn deinit(self: *GenState, allocator: std.mem.Allocator) void {
        self.prototypes.deinit(allocator);
        self.registers.deinit(allocator);
        if (self.try_should_handle) |*try_should_handle| {
            try_should_handle.deinit(allocator);
        }
        self.breaks_label.deinit(allocator);
        for (self.opt_jumps.items) |*opt_jump| {
            opt_jump.deinit(allocator);
        }
        self.opt_jumps.deinit(allocator);
    }
};

const CompiledFunction = struct {
    native: *anyopaque,
    native_raw: *anyopaque,
};

const Self = @This();

vm: *VM,
ctx: m.MIR_context_t,
state: ?GenState = null,
/// Set of closures or hotspots being or already compiled
compiled_nodes: std.AutoHashMapUnmanaged(Ast.Node.Index, void) = .empty,
/// Closures or hotspots we can't compile (containing async call, or yield)
blacklisted_nodes: std.AutoHashMapUnmanaged(Ast.Node.Index, void) = .empty,
/// MIR doesn't allow generating multiple functions at once, so we keep a set of function to compile
/// Once compiled, the value is set to an array of the native and raw native func_items
functions_queue: std.AutoHashMapUnmanaged(Ast.Node.Index, ?[2]?m.MIR_item_t) = .empty,
/// ObjClosures for which we later compiled the function and need to set it's native and native_raw fields
objclosures_queue: std.AutoHashMapUnmanaged(*o.ObjClosure, void) = .empty,
/// External api to link
required_ext_api: std.AutoHashMapUnmanaged(ExternApi, void) = .empty,
/// Modules to load when linking/generating
modules: std.ArrayList(m.MIR_module_t) = .empty,
/// Call count of all functions
call_count: u128 = 0,
/// Keeps track of time spent in the JIT
jit_time: usize = 0,
/// Closures already compiled (hash is bytecode list), useful to compile once a function
compiled_functions_bodies: Chunk.HashMap(CompiledFunction) = .empty,

args_buffer: [255]m.MIR_op_t = undefined,

pub fn init(vm: *VM) Self {
    return .{
        .vm = vm,
        .ctx = m.MIR_init(),
    };
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    self.compiled_nodes.deinit(allocator);
    self.blacklisted_nodes.deinit(allocator);
    // std.debug.assert(self.functions_queue.count() == 0);
    self.functions_queue.deinit(allocator);
    // std.debug.assert(self.objclosures_queue.count() == 0);
    self.objclosures_queue.deinit(allocator);
    self.modules.deinit(allocator);
    self.required_ext_api.deinit(allocator);
    self.compiled_functions_bodies.deinit(allocator);
    m.MIR_finish(self.ctx);
}

// Ensure queues are empty for future use
fn reset(self: *Self, allocator: std.mem.Allocator) void {
    self.functions_queue.clearAndFree(allocator);
    self.objclosures_queue.clearAndFree(allocator);
    self.required_ext_api.clearAndFree(allocator);
    self.modules.clearAndFree(allocator);

    self.state.?.deinit(allocator);
    self.state = null;
}

pub fn compileFunction(self: *Self, ast: Ast.Slice, closure: *o.ObjClosure) Error!void {
    const function = closure.function;

    // Did we already compile a function with the same body?
    if (self.compiled_functions_bodies.get(function.chunk)) |compiled| {
        function.native = compiled.native;
        function.native_raw = compiled.native_raw;

        if (BuildOptions.jit_debug) {
            io.print("Reusing previous compilation\n", .{});
        }

        return;
    }

    const ast_node = function.node;

    if (try ast.usesFiber(
        self.vm.gc.allocator,
        ast_node,
    )) {
        if (BuildOptions.jit_debug) {
            io.print(
                "Not compiling node {s}#{}, likely because it uses a fiber\n",
                .{
                    @tagName(ast.nodes.items(.tag)[ast_node]),
                    ast_node,
                },
            );
        }
        _ = self.functions_queue.remove(ast_node);
        _ = self.objclosures_queue.remove(closure);
        try self.blacklisted_nodes.put(self.vm.gc.allocator, closure.function.node, {});

        return error.CantCompile;
    }

    // Remember we need to set this functions fields
    try self.objclosures_queue.put(self.vm.gc.allocator, closure, {});

    // Build the function
    try self.buildFunction(ast, closure, ast_node);

    // Did we encounter other functions to compile?
    try self.buildCollateralFunctions(ast);

    // Load modules
    for (self.modules.items) |module| {
        m.MIR_load_module(self.ctx, module);
    }

    // Load external functions
    var it_ext = self.required_ext_api.iterator();
    while (it_ext.next()) |kv| {
        switch (kv.key_ptr.*) {
            // TODO: don't mix those with actual api functions
            .RawFn, .NativeFn => {},
            else => m.MIR_load_external(
                self.ctx,
                kv.key_ptr.*.name(),
                kv.key_ptr.*.ptr(),
            ),
        }
    }

    // Link everything together
    m.MIR_link(self.ctx, m.MIR_set_lazy_gen_interface, null);

    m.MIR_gen_init(self.ctx);
    defer m.MIR_gen_finish(self.ctx);

    // Generate all needed functions and set them in corresponding ObjFunctions
    var it2 = self.functions_queue.iterator();
    while (it2.next()) |kv| {
        const node = kv.key_ptr.*;
        const items = kv.value_ptr.*.?;

        const native = if (items[0]) |item| m.MIR_gen(self.ctx, item) else null;
        const native_raw = if (items[1]) |item| m.MIR_gen(self.ctx, item) else null;

        // Find out if we need to set it in a ObjFunction
        var it3 = self.objclosures_queue.iterator();
        while (it3.next()) |kv2| {
            if (kv2.key_ptr.*.function.node == node) {
                kv2.key_ptr.*.function.native = native;
                kv2.key_ptr.*.function.native_raw = native_raw;

                try self.compiled_functions_bodies.put(
                    self.vm.gc.allocator,
                    kv2.key_ptr.*.function.chunk,
                    .{
                        .native = native.?,
                        .native_raw = native_raw.?,
                    },
                );
                break;
            }
        }
    }

    self.reset(self.vm.gc.allocator);
}

pub fn compileHotSpot(self: *Self, ast: Ast.Slice, closure: *o.ObjClosure, hotspot_node: Ast.Node.Index) Error!*anyopaque {
    if (try ast.usesFiber(
        self.vm.gc.allocator,
        hotspot_node,
    )) {
        if (BuildOptions.jit_debug) {
            io.print(
                "Not compiling node {s}#{}, likely because it uses a fiber\n",
                .{
                    @tagName(ast.nodes.items(.tag)[hotspot_node]),
                    hotspot_node,
                },
            );
        }

        try self.blacklisted_nodes.put(self.vm.gc.allocator, hotspot_node, {});

        return error.CantCompile;
    }

    // Build function surrounding the node
    try self.buildFunction(
        ast,
        closure,
        hotspot_node,
    );

    // Did we encounter other functions to compile?
    try self.buildCollateralFunctions(ast);

    // Load modules
    for (self.modules.items) |module| {
        m.MIR_load_module(self.ctx, module);
    }

    // Load external functions
    var it_ext = self.required_ext_api.iterator();
    while (it_ext.next()) |kv| {
        switch (kv.key_ptr.*) {
            // TODO: don't mix those with actual api functions
            .RawFn, .NativeFn => {},
            else => m.MIR_load_external(
                self.ctx,
                kv.key_ptr.*.name(),
                kv.key_ptr.*.ptr(),
            ),
        }
    }

    // Link everything together
    m.MIR_link(self.ctx, m.MIR_set_lazy_gen_interface, null);

    m.MIR_gen_init(self.ctx);
    defer m.MIR_gen_finish(self.ctx);

    // Generate all needed functions and set them in corresponding ObjFunctions
    var it2 = self.functions_queue.iterator();
    var hotspot_native: ?*anyopaque = null;
    while (it2.next()) |kv| {
        const node = kv.key_ptr.*;
        const items = kv.value_ptr.*.?;

        const native = if (items[0]) |item| m.MIR_gen(self.ctx, item) else null;
        const native_raw = if (items[1]) |item| m.MIR_gen(self.ctx, item) else null;

        // Find out if we need to set it in a ObjFunction
        var it3 = self.objclosures_queue.iterator();
        while (it3.next()) |kv2| {
            if (kv2.key_ptr.*.function.node == node) {
                kv2.key_ptr.*.function.native = native;
                kv2.key_ptr.*.function.native_raw = native_raw;
                break;
            }
        }

        // If its the hotspot, return the NativeFn pointer
        if (node == hotspot_node) {
            hotspot_native = native_raw;
        }
    }

    self.reset(self.vm.gc.allocator);

    return hotspot_native orelse Error.CantCompile;
}

fn buildCollateralFunctions(self: *Self, ast: Ast.Slice) Error!void {
    var it = self.functions_queue.iterator();
    while (it.next()) |kv| {
        const node = kv.key_ptr.*;

        if (kv.value_ptr.* == null) {
            // Does it have an associated closure?
            var it2 = self.objclosures_queue.iterator();
            var sub_closure: ?*o.ObjClosure = null;
            while (it2.next()) |kv2| {
                if (kv2.key_ptr.*.function.node == node) {
                    sub_closure = kv2.key_ptr.*;
                    break;
                }
            }
            try self.buildFunction(ast, sub_closure, node);

            // Building a new function might have added functions in the queue, so we reset the iterator
            it = self.functions_queue.iterator();
        }
    }
}

fn buildFunction(self: *Self, ast: Ast.Slice, closure: ?*o.ObjClosure, ast_node: Ast.Node.Index) Error!void {
    self.state = .{
        .ast = ast,
        .module = undefined,
        .ast_node = ast_node,
        .closure = closure orelse self.state.?.closure,
    };

    const tag = self.state.?.ast.nodes.items(.tag)[ast_node];
    const qualified_name = try self.getQualifiedName(
        ast_node,
        false,
    );
    defer self.vm.gc.allocator.free(qualified_name);
    const raw_qualified_name = try self.getQualifiedName(
        ast_node,
        true,
    );
    defer self.vm.gc.allocator.free(raw_qualified_name);

    const module = m.MIR_new_module(self.ctx, @ptrCast(qualified_name));
    defer m.MIR_finish_module(self.ctx);

    try self.modules.append(self.vm.gc.allocator, module);

    self.state.?.module = module;

    if (closure) |uclosure| {
        try self.compiled_nodes.put(self.vm.gc.allocator, uclosure.function.node, {});

        if (BuildOptions.jit_debug) {
            io.print(
                "Compiling function `{s}` because it was called {}/{} times\n",
                .{
                    qualified_name.items,
                    uclosure.function.call_count,
                    self.call_count,
                },
            );
        }
    } else {
        try self.compiled_nodes.put(self.vm.gc.allocator, ast_node, {});

        if (BuildOptions.jit_debug) {
            if (tag.isHotspot()) {
                io.print(
                    "Compiling hotspot for node {s} {}\n",
                    .{
                        @tagName(self.state.?.ast.nodes.items(.tag)[ast_node]),
                        ast_node,
                    },
                );
            } else {
                io.print(
                    "Compiling closure `{s}`\n",
                    .{
                        qualified_name.items,
                    },
                );
            }
        }
    }

    _ = (if (tag.isHotspot())
        self.generateHotspotFunction(ast_node)
    else
        self.generateNode(ast_node)) catch |err| {
        if (err == Error.CantCompile) {
            if (BuildOptions.jit_debug) {
                io.print("Not compiling `{s}`, likely because it uses a fiber\n", .{qualified_name.items});
            }

            m.MIR_finish_func(self.ctx);

            _ = self.functions_queue.remove(ast_node);
            if (closure) |uclosure| {
                _ = self.objclosures_queue.remove(uclosure);
                try self.blacklisted_nodes.put(self.vm.gc.allocator, uclosure.function.node, {});
            }
        }

        return err;
    };

    // Export generated function so it can be linked
    _ = m.MIR_new_export(self.ctx, @ptrCast(raw_qualified_name));
    _ = m.MIR_new_export(self.ctx, @ptrCast(qualified_name));

    if (BuildOptions.jit_debug) {
        _ = std.mem.replace(
            u8,
            qualified_name.items,
            "/",
            ".",
            qualified_name.items,
        );

        self.outputModule(qualified_name.items, module);
    }
}

fn generateNode(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components);
    const tag = self.state.?.ast.nodes.items(.tag)[node];
    const constant = self.state.?.ast.nodes.items(.value)[node] orelse if (try self.state.?.ast.isConstant(self.vm.gc.allocator, node))
        try self.state.?.ast.toValue(node, self.vm.gc)
    else
        null;

    var value = if (constant != null)
        m.MIR_new_uint_op(self.ctx, constant.?.val)
    else switch (tag) {
        .Boolean => m.MIR_new_uint_op(
            self.ctx,
            Value.fromBoolean(components[node].Boolean).val,
        ),
        .Double => m.MIR_new_double_op(
            self.ctx,
            components[node].Double,
        ),
        .Integer => m.MIR_new_uint_op(
            self.ctx,
            Value.fromInteger(components[node].Integer).val,
        ),
        .StringLiteral => m.MIR_new_uint_op(
            self.ctx,
            components[node].StringLiteral.literal.toValue().val,
        ),
        .Null => m.MIR_new_uint_op(
            self.ctx,
            Value.Null.val,
        ),
        .Void => m.MIR_new_uint_op(
            self.ctx,
            Value.Void.val,
        ),
        .String => try self.generateString(node),
        .Expression => try self.generateNode(components[node].Expression),
        .GenericResolve => try self.generateNode(components[node].GenericResolve.expression),
        .Grouping => try self.generateNode(components[node].Grouping),
        .Function => try self.generateFunction(node),
        .FunDeclaration => try self.generateFunDeclaration(node),
        .VarDeclaration => try self.generateVarDeclaration(node),
        .Block => try self.generateBlock(node),
        .BlockExpression => try self.generateBlockExpression(node),
        .Out => try self.generateNode(components[node].Out),
        .Call => try self.generateCall(node),
        .NamedVariable => try self.generateNamedVariable(node),
        .Return => try self.generateReturn(node),
        .If => try self.generateIf(node),
        .Binary => try self.generateBinary(node),
        .While => try self.generateWhile(node),
        .DoUntil => try self.generateDoUntil(node),
        .For => try self.generateFor(node),
        .Break => try self.generateBreak(node),
        .Continue => try self.generateContinue(node),
        .List => try self.generateList(node),
        .Range => try self.generateRange(node),
        .Dot => try self.generateDot(node),
        .Subscript => try self.generateSubscript(node),
        .Map => try self.generateMap(node),
        .Is => try self.generateIs(node),
        .As => try self.generateAs(node),
        .Try => try self.generateTry(node),
        .Throw => try self.generateThrow(node),
        .Unwrap => try self.generateUnwrap(node),
        .ObjectInit => try self.generateObjectInit(node),
        .ForceUnwrap => try self.generateForceUnwrap(node),
        .Unary => try self.generateUnary(node),
        .Pattern => try self.generatePattern(node),
        .ForEach => try self.generateForEach(node),
        .TypeExpression => try self.generateTypeExpression(node),
        .TypeOfExpression => try self.generateTypeOfExpression(node),
        .AsyncCall,
        .Resume,
        .Resolve,
        .Yield,
        => return Error.CantCompile,

        else => {
            io.print("{} NYI\n", .{tag});
            unreachable;
        },
    };

    if (tag != .Break and tag != .Continue) {
        // Patch opt jumps if needed
        if (self.state.?.ast.nodes.items(.patch_opt_jumps)[node]) {
            std.debug.assert(self.state.?.opt_jumps.items.len > 0);

            var opt_jump = self.state.?.opt_jumps.pop().?;
            const out_label = m.MIR_new_label(self.ctx);

            // We reached here, means nothing was null, set the alloca with the value and use it has the node return value
            self.MOV(
                m.MIR_new_reg_op(self.ctx, opt_jump.alloca),
                value.?,
            );

            self.JMP(out_label);

            // Patch opt blocks with the branching
            for (opt_jump.current_insn.items) |current_insn| {
                m.MIR_insert_insn_after(
                    self.ctx,
                    self.state.?.function.?,
                    current_insn,
                    m.MIR_new_insn_arr(
                        self.ctx,
                        @intFromEnum(m.MIR_Instruction.BEQ),
                        3,
                        &[_]m.MIR_op_t{
                            m.MIR_new_label_op(self.ctx, out_label),
                            m.MIR_new_reg_op(self.ctx, opt_jump.alloca),
                            m.MIR_new_uint_op(self.ctx, Value.Null.val),
                        },
                    ),
                );
            }

            self.append(out_label);

            value = m.MIR_new_reg_op(self.ctx, opt_jump.alloca);

            opt_jump.deinit(self.vm.gc.allocator);
        }
        // Close scope if needed
        if (constant == null or tag != .Range) { // Range creates locals for its limits, but we don't push anything if its constant
            try self.closeScope(node);
        }
    }

    return value;
}

fn closeScope(self: *Self, node: Ast.Node.Index) !void {
    if (self.state.?.ast.nodes.items(.ends_scope)[node]) |closing| {
        for (closing) |op| {
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

    // Avoid intertwining the push and its value expression
    const value_reg = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("value", m.MIR_T_I64),
    );
    self.MOV(value_reg, value);

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
        value_reg,
    );

    // Increment stack top
    self.ADD(
        stack_top,
        m.MIR_new_reg_op(self.ctx, stack_top_base),
        m.MIR_new_uint_op(self.ctx, @sizeOf(u64)),
    );
}

// FIXME: we should not need 3 MOV to get to the value?
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
    if (dest) |into| {
        const top = m.MIR_new_mem_op(
            self.ctx,
            m.MIR_T_P,
            0,
            stack_top_base,
            index,
            1,
        );

        self.MOV(
            into,
            top,
        );
    }
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
        (-1 - @as(i32, @intCast(distance))) * @sizeOf(u64),
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

    // Avoid intertwining the get local and its value expression
    const value_reg = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("value", m.MIR_T_I64),
    );
    self.MOV(
        value_reg,
        m.MIR_new_mem_op(
            self.ctx,
            m.MIR_T_U64,
            0,
            base,
            index,
            @sizeOf(u64),
        ),
    );

    return value_reg;
}

fn buildSetLocal(self: *Self, slot: usize, value: m.MIR_op_t) !void {
    const ctx_reg = self.state.?.ctx_reg.?;
    const index = try self.REG("index", m.MIR_T_I64);
    const base = try self.REG("base", m.MIR_T_I64);

    // Avoid intertwining the set local and its value expression
    const value_reg = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("value", m.MIR_T_I64),
    );
    self.MOV(value_reg, value);

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

    self.MOV(local, value_reg);
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
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, globals),
        m.MIR_new_mem_op(
            self.ctx,
            m.MIR_T_U64,
            @offsetOf(o.NativeCtx, "globals"),
            ctx_reg,
            index,
            0,
        ),
    );

    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, slot),
    );

    const global = m.MIR_new_mem_op(
        self.ctx,
        m.MIR_T_U64,
        0,
        globals,
        index,
        @sizeOf(u64),
    );

    self.MOV(global, value);
}

fn buildValueToBoolean(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) void {
    self.EQ(
        dest,
        value,
        m.MIR_new_uint_op(self.ctx, Value.TrueMask),
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
        m.MIR_new_uint_op(self.ctx, Value.False.val),
    );

    self.JMP(out_label);

    self.append(true_label);

    self.MOV(
        dest,
        m.MIR_new_uint_op(self.ctx, Value.True.val),
    );

    self.append(out_label);
}

fn buildValueToInteger(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    const inter = m.MIR_new_reg_op(
        self.ctx,
        try self.REG(
            "inter",
            m.MIR_T_I64,
        ),
    );

    // Extract i48
    self.AND(
        inter,
        value,
        m.MIR_new_uint_op(self.ctx, 0xffffffffffff),
    );

    const is_neg = m.MIR_new_reg_op(
        self.ctx,
        try self.REG(
            "is_neg",
            m.MIR_T_I64,
        ),
    );

    const out_label = m.MIR_new_label(self.ctx);
    self.AND(
        is_neg,
        inter,
        m.MIR_new_uint_op(self.ctx, 1 << 47),
    );

    // If 48th bit is 1, means the i48 is negative
    self.BEQ(
        m.MIR_new_label_op(self.ctx, out_label),
        is_neg,
        m.MIR_new_uint_op(self.ctx, 0),
    );

    // Extend sign by filling upper bits with 1s
    self.OR(
        inter,
        inter,
        m.MIR_new_uint_op(self.ctx, @bitCast(~@as(i64, 0xFFFFFFFFFFFF))),
    );

    self.append(out_label);

    self.MOV(dest, inter);
}

fn buildValueFromInteger(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) void {
    // Get rid of upper bits
    self.AND(
        dest,
        value,
        m.MIR_new_uint_op(self.ctx, @bitCast(@as(i64, 0xFFFFFFFFFFFF))),
    );

    self.OR(
        dest,
        m.MIR_new_uint_op(self.ctx, Value.IntegerMask),
        dest,
    );
}

fn buildValueToObj(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) void {
    self.NOT(dest, m.MIR_new_uint_op(self.ctx, Value.PointerMask));
    self.AND(dest, value, dest);
}

fn buildValueFromObj(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) void {
    self.OR(
        dest,
        m.MIR_new_uint_op(self.ctx, Value.PointerMask),
        value,
    );
}

fn buildValueToCString(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    try self.buildExternApiCall(
        .bz_valueToCString,
        dest,
        &[_]m.MIR_op_t{value},
    );
}

fn buildValueToOptionalCString(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    const null_label = m.MIR_new_label(self.ctx);

    self.MOV(
        dest,
        m.MIR_new_uint_op(self.ctx, 0),
    );

    self.BEQ(
        m.MIR_new_label_op(self.ctx, null_label),
        value,
        m.MIR_new_uint_op(self.ctx, Value.Null.val),
    );

    try self.buildExternApiCall(
        .bz_valueToCString,
        dest,
        &[_]m.MIR_op_t{value},
    );

    self.append(null_label);
}

fn buildValueFromCString(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    try self.buildExternApiCall(
        .bz_stringToValueZ,
        dest,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            value,
        },
    );
}

fn buildValueFromOptionalCString(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    const null_label = m.MIR_new_label(self.ctx);

    self.MOV(
        dest,
        m.MIR_new_uint_op(self.ctx, Value.Null.val),
    );

    self.BEQ(
        m.MIR_new_label_op(self.ctx, null_label),
        value,
        m.MIR_new_uint_op(self.ctx, 0),
    );

    try self.buildExternApiCall(
        .bz_stringToValueZ,
        dest,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            value,
        },
    );

    self.append(null_label);
}

fn buildValueToUserData(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    try self.buildExternApiCall(
        .bz_getUserDataPtr,
        dest,
        &[_]m.MIR_op_t{value},
    );
}

fn buildValueToForeignContainerPtr(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    try self.buildExternApiCall(
        .bz_valueToForeignContainerPtr,
        dest,
        &[_]m.MIR_op_t{value},
    );
}

fn buildValueFromForeignContainerPtr(self: *Self, type_def: *o.ObjTypeDef, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    try self.buildExternApiCall(
        .bz_newForeignContainerFromSlice,
        dest,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            m.MIR_new_uint_op(self.ctx, @intFromPtr(type_def)),
            value,
            m.MIR_new_uint_op(self.ctx, type_def.resolved_type.?.ForeignContainer.zig_type.size()),
        },
    );
}

fn buildValueFromOptionalForeignContainerPtr(self: *Self, type_def: *o.ObjTypeDef, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    const null_label = m.MIR_new_label(self.ctx);

    self.MOV(
        dest,
        m.MIR_new_uint_op(self.ctx, Value.Null.val),
    );

    self.BEQ(
        m.MIR_new_label_op(self.ctx, null_label),
        value,
        m.MIR_new_uint_op(self.ctx, 0),
    );

    try self.buildExternApiCall(
        .bz_newForeignContainerFromSlice,
        dest,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            m.MIR_new_uint_op(self.ctx, @intFromPtr(type_def)),
            value,
            m.MIR_new_uint_op(self.ctx, type_def.resolved_type.?.ForeignContainer.zig_type.size()),
        },
    );

    self.append(null_label);
}

fn buildValueToOptionalForeignContainerPtr(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    const null_label = m.MIR_new_label(self.ctx);

    self.MOV(
        dest,
        m.MIR_new_uint_op(self.ctx, 0),
    );

    self.BEQ(
        m.MIR_new_label_op(self.ctx, null_label),
        value,
        m.MIR_new_uint_op(self.ctx, Value.Null.val),
    );

    try self.buildExternApiCall(
        .bz_valueToForeignContainerPtr,
        dest,
        &[_]m.MIR_op_t{value},
    );

    self.append(null_label);
}

fn buildValueFromUserData(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    try self.buildExternApiCall(
        .bz_newUserData,
        dest,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            value,
        },
    );
}

fn buildValueFromOptionalUserData(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    const null_label = m.MIR_new_label(self.ctx);

    self.MOV(
        dest,
        m.MIR_new_uint_op(self.ctx, Value.Null.val),
    );

    self.BEQ(
        m.MIR_new_label_op(self.ctx, null_label),
        value,
        m.MIR_new_uint_op(self.ctx, 0),
    );

    try self.buildExternApiCall(
        .bz_newUserData,
        dest,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            value,
        },
    );

    self.append(null_label);
}

fn buildValueToOptionalUserData(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    const null_label = m.MIR_new_label(self.ctx);

    self.MOV(
        dest,
        m.MIR_new_uint_op(self.ctx, 0),
    );

    self.BEQ(
        m.MIR_new_label_op(self.ctx, null_label),
        value,
        m.MIR_new_uint_op(self.ctx, Value.Null.val),
    );

    try self.buildExternApiCall(
        .bz_getUserDataPtr,
        dest,
        &[_]m.MIR_op_t{value},
    );

    self.append(null_label);
}

fn buildValueFromFloat(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    // Allocate memory
    const block = try self.REG("call_block", m.MIR_T_I64);
    self.BSTART(block);
    defer self.BEND(block); // release alloca after result is put in dest registry
    const addr = self.REG("cast", m.MIR_T_I64) catch unreachable;
    self.ALLOCA(addr, @sizeOf(u64));

    // Put the value in it as double
    self.FMOV(
        m.MIR_new_mem_op(
            self.ctx,
            m.MIR_T_F,
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
}

fn buildValueFromDouble(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    // Allocate memory
    const block = try self.REG("call_block", m.MIR_T_I64);
    self.BSTART(block);
    defer self.BEND(block); // release alloca after result is put in dest registry
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
}

fn buildValueToDouble(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    // Allocate memory
    const block = try self.REG("call_block", m.MIR_T_I64);
    self.BSTART(block);
    defer self.BEND(block); // release alloca after result is put in dest registry
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
}

fn buildValueToFloat(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    // Allocate memory
    const block = try self.REG("call_block", m.MIR_T_I64);
    self.BSTART(block);
    defer self.BEND(block); // release alloca after result is put in dest registry
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
    self.FMOV(
        dest,
        m.MIR_new_mem_op(
            self.ctx,
            m.MIR_T_F,
            0,
            addr,
            0,
            0,
        ),
    );
}

fn buildValueToForeignContainer(self: *Self, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    const index = try self.REG("index", m.MIR_T_I64);
    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    const foreign = try self.REG("foreign", m.MIR_T_P);
    try self.buildExternApiCall(
        .bz_valueToForeignContainerPtr,
        m.MIR_new_reg_op(self.ctx, foreign),
        &[_]m.MIR_op_t{value},
    );

    self.MOV(
        dest,
        m.MIR_new_mem_op(
            self.ctx,
            m.MIR_T_P,
            0,
            foreign,
            index,
            0,
        ),
    );
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

    // If return whithin hotspot, push value, return 1 so the return is forwarded to caller
    if (self.state.?.ast.nodes.items(.tag)[self.state.?.ast_node].isHotspot()) {
        try self.buildPush(value);

        self.RET(m.MIR_new_int_op(self.ctx, 1));

        return;
    }

    // Do return
    self.RET(value);
}

// Unwrap buzz value to its raw mir Value
fn unwrap(self: *Self, def_type: o.ObjTypeDef.Type, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    return switch (def_type) {
        .Bool => self.buildValueToBoolean(value, dest),
        .Integer => self.buildValueToInteger(value, dest),
        .Double => try self.buildValueToDouble(value, dest),
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
        .Range,
        => self.buildValueToObj(value, dest),
        .ForeignContainer => try self.buildValueToForeignContainer(value, dest),
        .Placeholder,
        .Generic,
        .Any,
        => unreachable,
    };
}

// Wrap mir value to buzz Value
fn wrap(self: *Self, def_type: o.ObjTypeDef.Type, value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    return switch (def_type) {
        .Bool => self.buildValueFromBoolean(value, dest),
        .Integer => self.buildValueFromInteger(value, dest),
        .Double => try self.buildValueFromDouble(value, dest),
        .Void => self.MOV(dest, m.MIR_new_uint_op(self.ctx, Value.Void.val)),
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
        .Range,
        => self.buildValueFromObj(value, dest),
        .ForeignContainer,
        .Placeholder,
        .Generic,
        .Any,
        => unreachable,
    };
}

fn buildExternApiCall(self: *Self, method: ExternApi, dest: ?m.MIR_op_t, args: []const m.MIR_op_t) !void {
    var full_args = std.ArrayList(m.MIR_op_t){};
    defer full_args.deinit(self.vm.gc.allocator);

    try full_args.append(self.vm.gc.allocator, m.MIR_new_ref_op(self.ctx, try method.declare(self)));
    try full_args.append(self.vm.gc.allocator, m.MIR_new_ref_op(self.ctx, m.MIR_new_import(self.ctx, method.name())));
    if (dest) |udest| {
        try full_args.append(self.vm.gc.allocator, udest);
    }
    try full_args.appendSlice(self.vm.gc.allocator, args);

    self.args_buffer[0] = m.MIR_new_ref_op(self.ctx, try method.declare(self));
    self.args_buffer[1] = m.MIR_new_ref_op(self.ctx, m.MIR_new_import(self.ctx, method.name()));
    if (dest) |udest| {
        self.args_buffer[2] = udest;
    }
    const off: usize = if (dest != null) 3 else 2;
    std.mem.copyForwards(
        m.MIR_op_t,
        self.args_buffer[off..],
        args,
    );

    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.CALL),
            off + args.len,
            &self.args_buffer,
        ),
    );
}

fn generateString(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const elements = self.state.?.ast.nodes.items(.components)[node].String;
    const type_defs = self.state.?.ast.nodes.items(.type_def);

    if (elements.len == 0) {
        return m.MIR_new_uint_op(
            self.ctx,
            self.state.?.closure.function.chunk.constants.items[0].val,
        ); // Constant 0 is the empty string
    }

    var previous: ?m.MIR_op_t = null;
    for (elements) |element| {
        var value = (try self.generateNode(element)).?;

        if (type_defs[element].?.def_type != .String or type_defs[element].?.optional) {
            const dest = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("result", m.MIR_T_I64),
            );

            try self.buildExternApiCall(
                .bz_valueCastToString,
                dest,
                &[_]m.MIR_op_t{
                    value,
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
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
                .bz_stringConcat,
                dest,
                &[_]m.MIR_op_t{
                    uprevious,
                    value,
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
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

fn generateNamedVariable(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].NamedVariable;
    const type_def = self.state.?.ast.nodes.items(.type_def)[node];
    const tags = self.state.?.ast.tokens.items(.tag);

    const function_type = if (type_def.?.def_type == .Function)
        type_def.?.resolved_type.?.Function.function_type
    else
        null;
    const is_constant_fn = function_type != null and function_type.? != .Extern and function_type.? != .Anonymous;

    const value = if (components.value) |val|
        try self.generateNode(val)
    else
        null;

    switch (components.slot_type) {
        .Global => {
            if (value) |val| {
                std.debug.assert(!is_constant_fn);

                if (tags[components.assign_token.?] != .Equal) {
                    // buildGetGlobal returns the actual address of the global so no need to buildSetGlobal after
                    const global = try self.buildGetGlobal(components.slot);
                    try self.buildBinary(
                        tags[components.assign_token.?],
                        type_def.?.def_type,
                        global,
                        val,
                        global,
                    );
                } else {
                    try self.buildSetGlobal(components.slot, val);
                }

                return null;
            } else if (is_constant_fn) {
                // Get the actual Value as it is right now (which is correct since a function doesn't change)
                const closure = o.ObjClosure.cast(self.state.?.closure.globals.items[components.slot].obj()).?;

                // Does it need to be compiled?
                if (self.compiled_nodes.get(closure.function.node) == null) {
                    if (self.blacklisted_nodes.get(closure.function.node) != null) {
                        return Error.CantCompile;
                    }

                    // Remember we need to set native fields of this ObjFunction later
                    try self.objclosures_queue.put(self.vm.gc.allocator, closure, {});

                    // Remember that we need to compile this function later
                    try self.functions_queue.put(self.vm.gc.allocator, closure.function.node, null);
                }

                return m.MIR_new_uint_op(self.ctx, closure.toValue().val);
            } else {
                return try self.buildGetGlobal(components.slot);
            }
        },
        .Local => {
            if (value) |val| {
                if (tags[components.assign_token.?] != .Equal) {
                    const local = try self.buildGetLocal(components.slot);

                    try self.buildBinary(
                        tags[components.assign_token.?],
                        type_def.?.def_type,
                        local,
                        val,
                        local,
                    );

                    try self.buildSetLocal(components.slot, local);
                } else {
                    try self.buildSetLocal(components.slot, val);
                }

                return null;
            }

            return try self.buildGetLocal(components.slot);
        },
        .UpValue => {
            if (value) |val| {
                if (tags[components.assign_token.?] != .Equal) {
                    const upvalue = m.MIR_new_reg_op(
                        self.ctx,
                        try self.REG("upvalue", m.MIR_T_I64),
                    );

                    try self.buildExternApiCall(
                        .bz_getUpValue,
                        upvalue,
                        &[_]m.MIR_op_t{
                            m.MIR_new_reg_op(self.ctx, self.state.?.ctx_reg.?),
                            m.MIR_new_uint_op(self.ctx, components.slot),
                        },
                    );

                    try self.buildBinary(
                        tags[components.assign_token.?],
                        type_def.?.def_type,
                        upvalue,
                        val,
                        upvalue,
                    );
                } else {
                    try self.buildExternApiCall(
                        .bz_setUpValue,
                        null,
                        &[_]m.MIR_op_t{
                            m.MIR_new_reg_op(self.ctx, self.state.?.ctx_reg.?),
                            m.MIR_new_uint_op(self.ctx, components.slot),
                            val,
                        },
                    );
                }

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
                    m.MIR_new_uint_op(self.ctx, components.slot),
                },
            );

            return upvalue;
        },
    }
}

fn generateCall(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const node_types = self.state.?.ast.nodes.items(.tag);
    const node_components = self.state.?.ast.nodes.items(.components);
    const components = node_components[node].Call;
    const type_defs = self.state.?.ast.nodes.items(.type_def);
    const lexemes = self.state.?.ast.tokens.items(.lexeme);

    // This is not a call but an Enum(value)
    if (type_defs[components.callee].?.def_type == .Enum) {
        const result_reg = try self.REG("enum_case", m.MIR_T_I64);

        try self.buildExternApiCall(
            .bz_getEnumCaseFromValue,
            m.MIR_new_reg_op(self.ctx, result_reg),
            &[_]m.MIR_op_t{
                (try self.generateNode(components.callee)).?,
                (try self.generateNode(components.arguments[0].value)).?,
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            },
        );

        return m.MIR_new_reg_op(self.ctx, result_reg);
    }

    // Find out if call is invoke or regular call
    const dot = if (node_types[components.callee] == .Dot)
        components.callee
    else
        null;
    const invoked_on = if (dot != null)
        type_defs[node_components[dot.?].Dot.callee].?.def_type
    else
        null;

    const subject = if (invoked_on != null)
        try self.generateNode(node_components[dot.?].Dot.callee)
    else
        null;
    const callee_reg = try self.REG("callee", m.MIR_T_I64);
    const callee = m.MIR_new_reg_op(self.ctx, callee_reg);
    if (invoked_on != null) {
        const member_lexeme = lexemes[node_components[components.callee].Dot.identifier];

        switch (invoked_on.?) {
            .Object => try self.buildExternApiCall(
                .bz_getObjectField,
                callee,
                &[_]m.MIR_op_t{
                    subject.?,
                    m.MIR_new_uint_op(
                        self.ctx,
                        type_defs[node_components[components.callee].Dot.callee].?
                            .resolved_type.?.Object
                            .fields.get(member_lexeme).?
                            .index,
                    ),
                },
            ),
            .ObjectInstance => instance: {
                const field = type_defs[node_components[components.callee].Dot.callee].?
                    .resolved_type.?.ObjectInstance.of
                    .resolved_type.?.Object
                    .fields.get(member_lexeme).?;

                break :instance try self.buildExternApiCall(
                    if (field.method)
                        .bz_getObjectInstanceMethod
                    else
                        .bz_getObjectInstanceProperty,
                    callee,
                    if (field.method)
                        &[_]m.MIR_op_t{
                            // subject
                            subject.?,
                            // member
                            m.MIR_new_uint_op(self.ctx, field.index),
                            // bound
                            m.MIR_new_uint_op(self.ctx, 0),
                            // vm
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        }
                    else
                        &[_]m.MIR_op_t{
                            // subject
                            subject.?,
                            // member
                            m.MIR_new_uint_op(self.ctx, field.index),
                        },
                );
            },
            .ProtocolInstance,
            .String,
            .Pattern,
            .Fiber,
            .List,
            .Map,
            .Range,
            => try self.buildExternApiCall(
                switch (invoked_on.?) {
                    .ProtocolInstance => .bz_getProtocolMethod,
                    .String => .bz_getStringProperty,
                    .Pattern => .bz_getPatternProperty,
                    .Fiber => .bz_getFiberProperty,
                    .List => .bz_getListProperty,
                    .Map => .bz_getMapProperty,
                    .Range => .bz_getRangeProperty,
                    else => unreachable,
                },
                callee,
                if (invoked_on.? != .ProtocolInstance)
                    &[_]m.MIR_op_t{
                        // vm
                        m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        // subject
                        subject.?,
                        // member
                        m.MIR_new_uint_op(
                            self.ctx,
                            switch (invoked_on.?) {
                                .String => o.ObjString.members_name.get(member_lexeme).?,
                                .Pattern => o.ObjPattern.members_name.get(member_lexeme).?,
                                .Fiber => o.ObjFiber.members_name.get(member_lexeme).?,
                                .Range => o.ObjRange.members_name.get(member_lexeme).?,
                                .List => o.ObjList.members_name.get(member_lexeme).?,
                                .Map => o.ObjMap.members_name.get(member_lexeme).?,
                                else => unreachable,
                            },
                        ),
                        // bound
                        m.MIR_new_uint_op(self.ctx, 0),
                    }
                else
                    &[_]m.MIR_op_t{
                        // subject
                        subject.?,
                        // member
                        m.MIR_new_uint_op(
                            self.ctx,
                            (try self.vm.gc.copyString(
                                self.state.?.ast.tokens.items(.lexeme)[node_components[dot.?].Dot.identifier],
                            )).toValue().val,
                        ),
                        // vm
                        m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    },
            ),
            else => unreachable,
        }
    } else {
        self.MOV(callee, (try self.generateNode(components.callee)).?);
    }

    const callee_type = if (dot != null)
        node_components[dot.?].Dot.member_type_def
    else
        type_defs[components.callee];

    const function_type_def = callee_type.?;
    const function_type = function_type_def.resolved_type.?.Function.function_type;

    const error_types = function_type_def.resolved_type.?.Function.error_types;
    const has_catch_clause = components.catch_default != null and error_types != null and error_types.?.len > 0 and function_type != .Extern;

    // We store the callee NativeCtx and eventual catch value in an alloca
    // Since the alloca is declared outside of the callee, it will outlive it until the enclosing function returns
    // So we use BSTART/BEND so the new_ctx is released once the callee is done
    const block = try self.REG("call_block", m.MIR_T_I64);
    self.BSTART(block);

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

    const catch_value = if (components.catch_default) |value|
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
    if (components.is_async) {
        // TODO: fiber
        unreachable;
    }

    // Arguments

    // if invoked, first arg is `this`
    if (invoked_on != null) {
        _ = try self.buildPush(subject.?);
    } else {
        _ = try self.buildPush(m.MIR_new_uint_op(self.ctx, Value.Void.val));
    }

    const args = function_type_def.resolved_type.?.Function.parameters;
    const defaults = function_type_def.resolved_type.?.Function.defaults;
    const arg_keys = args.keys();

    var arguments = std.AutoArrayHashMapUnmanaged(*o.ObjString, m.MIR_op_t).empty;
    defer arguments.deinit(self.vm.gc.allocator);

    // Evaluate arguments
    for (components.arguments, 0..) |argument, index| {
        const actual_arg_key = if (index == 0 and argument.name == null)
            arg_keys[0]
        else
            (try self.vm.gc.copyString(lexemes[argument.name.?]));

        try arguments.put(
            self.vm.gc.allocator,
            actual_arg_key,
            (try self.generateNode(argument.value)).?,
        );
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
            @intFromEnum(m.MIR_Instruction.CALL),
            4,
            &[_]m.MIR_op_t{
                m.MIR_new_ref_op(
                    self.ctx,
                    if (function_type == .Extern)
                        try ExternApi.NativeFn.declare(self)
                    else
                        try ExternApi.RawFn.declare(self),
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

    self.BEND(block);

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
            m.MIR_new_uint_op(self.ctx, Value.Void.val),
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

fn generateReturn(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].Return;

    if (components.unconditional) {
        self.state.?.return_emitted = true;
    }

    try self.buildReturn(
        if (components.value) |value|
            (try self.generateNode(value)).?
        else
            m.MIR_new_uint_op(self.ctx, Value.Void.val),
    );

    return null;
}

fn generateIf(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const values = self.state.?.ast.nodes.items(.value);
    const type_defs = self.state.?.ast.nodes.items(.type_def);
    const components = self.state.?.ast.nodes.items(.components)[node].If;
    // We assume that if condition is const, the codegen will already have generated the Value
    const constant_condition = if (components.unwrapped_identifier == null and components.casted_type == null)
        values[components.condition]
    else
        null;

    // Generate condition
    const condition_value = if (constant_condition == null)
        (try self.generateNode(components.condition)).?
    else
        null;
    const condition = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("condition", m.MIR_T_I64),
    );

    const resolved = if (!components.is_statement)
        try self.REG("resolved", m.MIR_T_I64)
    else
        null;

    if (components.casted_type) |casted_type| {
        try self.buildExternApiCall(
            .bz_valueIs,
            condition,
            &[_]m.MIR_op_t{
                condition_value.?,
                m.MIR_new_uint_op(
                    self.ctx,
                    @constCast(type_defs[casted_type].?).toValue().val,
                ),
            },
        );

        try self.unwrap(
            .Bool,
            condition,
            condition,
        );
    } else if (components.unwrapped_identifier != null) {
        try self.buildExternApiCall(
            .bz_valueEqual,
            condition,
            &[_]m.MIR_op_t{
                condition_value.?,
                m.MIR_new_uint_op(self.ctx, Value.Null.val),
            },
        );

        // TODO: replace with condition ^ (MIR_OR, MIR_XOR?) 1
        const true_label = m.MIR_new_label(self.ctx);
        const out_label = m.MIR_new_label(self.ctx);

        self.BEQ(
            m.MIR_new_label_op(self.ctx, true_label),
            condition,
            m.MIR_new_uint_op(self.ctx, Value.True.val),
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
    } else if (constant_condition == null) {
        try self.unwrap(
            .Bool,
            condition_value.?,
            condition,
        );
    }

    const out_label = m.MIR_new_label(self.ctx);
    const then_label = m.MIR_new_label(self.ctx);
    const else_label = if (components.else_branch != null)
        m.MIR_new_label(self.ctx)
    else
        null;

    if (constant_condition != null) {
        self.JMP(
            if (constant_condition.?.boolean())
                then_label
            else if (components.else_branch != null)
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
            if (components.else_branch != null)
                else_label.?
            else
                out_label,
        );
    }

    if (constant_condition == null or constant_condition.?.boolean()) {
        self.append(then_label);

        // Push unwrapped value as local of the then block
        if (components.unwrapped_identifier != null or components.casted_type != null) {
            try self.buildPush(
                if (constant_condition) |constant|
                    m.MIR_new_uint_op(self.ctx, constant.val)
                else
                    condition_value.?,
            );
        }

        if (components.is_statement) {
            _ = try self.generateNode(components.body);
        } else {
            self.MOV(
                m.MIR_new_reg_op(self.ctx, resolved.?),
                (try self.generateNode(components.body)).?,
            );
        }

        self.JMP(out_label);
    }

    if (constant_condition == null or !constant_condition.?.boolean()) {
        if (components.else_branch) |else_branch| {
            self.append(else_label);

            if (components.is_statement) {
                _ = try self.generateNode(else_branch);
            } else {
                self.MOV(
                    m.MIR_new_reg_op(self.ctx, resolved.?),
                    (try self.generateNode(else_branch)).?,
                );
            }
        }
    }

    self.append(out_label);

    return if (components.is_statement)
        null
    else
        m.MIR_new_reg_op(self.ctx, resolved.?);
}

fn generateTypeExpression(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const type_expression = self.state.?.ast.nodes.items(.components)[node].TypeExpression;
    return m.MIR_new_uint_op(
        self.ctx,
        @constCast(self.state.?.ast.nodes.items(.type_def)[type_expression].?).toValue().val,
    );
}

fn generateTypeOfExpression(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const value = (try self.generateNode(self.state.?.ast.nodes.items(.components)[node].TypeOfExpression)).?;
    const result = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("typeof", m.MIR_T_I64),
    );

    try self.buildExternApiCall(
        .bz_valueTypeOf,
        result,
        &[_]m.MIR_op_t{
            value,
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
        },
    );

    return result;
}

fn buildBinary(
    self: *Self,
    operator: Token.Type,
    def_type: o.ObjTypeDef.Type,
    left_value: m.MIR_op_t,
    right_value: m.MIR_op_t,
    dest: m.MIR_op_t,
) Error!void {
    const left = m.MIR_new_reg_op(
        self.ctx,
        try self.REG(
            "left",
            if (def_type == .Double) m.MIR_T_D else m.MIR_T_I64,
        ),
    );
    const right = m.MIR_new_reg_op(
        self.ctx,
        try self.REG(
            "right",
            if (def_type == .Double) m.MIR_T_D else m.MIR_T_I64,
        ),
    );

    if (def_type == .Integer) {
        try self.unwrap(.Integer, left_value, left);
        try self.unwrap(.Integer, right_value, right);
    } else if (def_type == .Double) {
        try self.unwrap(.Double, left_value, left);
        try self.unwrap(.Double, right_value, right);
    } else {
        self.MOV(left, left_value);
        self.MOV(right, right_value);
    }

    // Avoid collection
    if (def_type != .Integer and def_type != .Double) {
        try self.buildPush(left_value);
        try self.buildPush(right_value);
    }

    switch (operator) {
        .Plus, .PlusEqual => {
            switch (def_type) {
                .Integer => {
                    self.ADD(dest, left, right);
                    try self.wrap(.Integer, dest, dest);
                },
                .Double => {
                    self.DADD(left, left, right);
                    try self.wrap(.Double, left, dest);
                },
                .String => {
                    try self.buildExternApiCall(
                        .bz_stringConcat,
                        dest,
                        &[_]m.MIR_op_t{
                            left_value,
                            right_value,
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        },
                    );
                },
                .List => {
                    try self.buildExternApiCall(
                        .bz_listConcat,
                        dest,
                        &[_]m.MIR_op_t{
                            left_value,
                            right_value,
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        },
                    );
                },
                .Map => {
                    try self.buildExternApiCall(
                        .bz_mapConcat,
                        dest,
                        &[_]m.MIR_op_t{
                            left_value,
                            right_value,
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        },
                    );
                },
                else => unreachable,
            }
        },
        .Minus, .MinusEqual => {
            switch (def_type) {
                .Integer => {
                    self.SUB(dest, left, right);
                    try self.wrap(.Integer, dest, dest);
                },
                .Double => {
                    self.DSUB(left, left, right);
                    try self.wrap(.Double, left, dest);
                },
                else => unreachable,
            }
        },
        .Star, .StarEqual => {
            switch (def_type) {
                .Integer => {
                    self.MUL(dest, left, right);
                    try self.wrap(.Integer, dest, dest);
                },
                .Double => {
                    self.DMUL(left, left, right);
                    try self.wrap(.Double, left, dest);
                },
                else => unreachable,
            }
        },
        .Slash, .SlashEqual => {
            switch (def_type) {
                .Integer => {
                    self.DIV(dest, left, right);
                    try self.wrap(.Integer, dest, dest);
                },
                .Double => {
                    self.DDIV(left, left, right);
                    try self.wrap(.Double, left, dest);
                },
                else => unreachable,
            }
        },
        .Percent, .PercentEqual => {
            switch (def_type) {
                .Integer => {
                    self.MOD(dest, left, right);
                    try self.wrap(.Integer, dest, dest);
                },
                .Double => {
                    try self.buildExternApiCall(
                        .fmod,
                        dest,
                        &[_]m.MIR_op_t{
                            left,
                            right,
                        },
                    );
                },
                else => unreachable,
            }
        },
        .Ampersand, .AmpersandEqual => {
            self.AND(dest, left, right);
            try self.wrap(.Integer, dest, dest);
        },
        .Bor, .BorEqual => {
            self.OR(dest, left, right);
            try self.wrap(.Integer, dest, dest);
        },
        .Xor, .XorEqual => {
            self.XOR(dest, left, right);
            try self.wrap(.Integer, dest, dest);
        },
        .ShiftLeft, .ShiftLeftEqual => {
            self.SHL(dest, left, right);
            try self.wrap(.Integer, dest, dest);
        },
        .ShiftRight, .ShiftRightEqual => {
            self.SHR(dest, left, right);
            try self.wrap(.Integer, dest, dest);
        },
        else => unreachable,
    }

    if (def_type != .Integer and def_type != .Double) {
        try self.buildPop(null);
        try self.buildPop(null);
    }
}

fn generateBinary(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const node_components = self.state.?.ast.nodes.items(.components);
    const components = node_components[node].Binary;

    return switch (components.operator) {
        .QuestionQuestion,
        .And,
        .Or,
        => try self.generateConditional(components),
        .Less,
        .Greater,
        .GreaterEqual,
        .LessEqual,
        .EqualEqual,
        .BangEqual,
        => try self.generateComparison(components),
        else => bin: {
            const type_defs = self.state.?.ast.nodes.items(.type_def);
            const type_def = type_defs[components.left].?.def_type;

            const left_value = (try self.generateNode(components.left)).?;
            const right_value = (try self.generateNode(components.right)).?;

            const res = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("res", m.MIR_T_I64),
            );

            try self.buildBinary(
                components.operator,
                type_def,
                left_value,
                right_value,
                res,
            );

            break :bin res;
        },
    };
}

fn generateComparison(self: *Self, components: Ast.Binary) Error!?m.MIR_op_t {
    const type_defs = self.state.?.ast.nodes.items(.type_def);

    const left_type_def = type_defs[components.left].?.def_type;
    const right_type_def = type_defs[components.right].?.def_type;

    const left_value = (try self.generateNode(components.left)).?;
    const right_value = (try self.generateNode(components.right)).?;

    const res = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("res", m.MIR_T_I64),
    );

    var left = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("left", if (left_type_def == .Double) m.MIR_T_D else m.MIR_T_I64),
    );
    var right = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("right", if (right_type_def == .Double) m.MIR_T_D else m.MIR_T_I64),
    );

    if (left_type_def == .Integer) {
        try self.unwrap(.Integer, left_value, left);
    } else if (left_type_def == .Double) {
        try self.unwrap(.Double, left_value, left);
    } else {
        self.MOV(left, left_value);
    }

    if (right_type_def == .Integer) {
        try self.unwrap(.Integer, right_value, right);
    } else if (right_type_def == .Double) {
        try self.unwrap(.Double, right_value, right);
    } else {
        self.MOV(right, right_value);
    }

    // Avoid collection
    if (left_type_def != .Integer and left_type_def != .Double) {
        try self.buildPush(left_value);
    }

    if (right_type_def != .Integer and right_type_def != .Double) {
        try self.buildPush(right_value);
    }

    switch (components.operator) {
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

            try self.unwrap(.Bool, res, res);

            const true_label = m.MIR_new_label(self.ctx);
            const out_label = m.MIR_new_label(self.ctx);

            self.BEQ(
                m.MIR_new_label_op(self.ctx, true_label),
                res,
                m.MIR_new_uint_op(self.ctx, 1),
            );

            self.MOV(
                res,
                m.MIR_new_uint_op(self.ctx, Value.True.val),
            );

            self.JMP(out_label);

            self.append(true_label);

            self.MOV(
                res,
                m.MIR_new_uint_op(self.ctx, Value.False.val),
            );

            self.append(out_label);
        },
        .Greater, .Less, .GreaterEqual, .LessEqual => {
            if (left_type_def == .Double or right_type_def == .Double) {
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

                switch (components.operator) {
                    .Greater => self.DGT(res, left, right),
                    .Less => self.DLT(res, left, right),
                    .GreaterEqual => self.DGE(res, left, right),
                    .LessEqual => self.DLE(res, left, right),
                    else => unreachable,
                }

                try self.wrap(.Bool, res, res);
            } else {
                switch (components.operator) {
                    .Greater => self.GT(res, left, right),
                    .Less => self.LT(res, left, right),
                    .GreaterEqual => self.GE(res, left, right),
                    .LessEqual => self.LE(res, left, right),
                    else => unreachable,
                }

                try self.wrap(.Bool, res, res);
            }
        },
        else => {},
    }

    if (left_type_def != .Integer and left_type_def != .Double) {
        try self.buildPop(null);
    }

    if (right_type_def != .Integer and right_type_def != .Double) {
        try self.buildPop(null);
    }

    return res;
}

fn generateConditional(self: *Self, binary: Ast.Binary) Error!?m.MIR_op_t {
    const value = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("value", m.MIR_T_I64),
    );

    self.MOV(
        value,
        (try self.generateNode(binary.left)).?,
    );

    const out_label = m.MIR_new_label(self.ctx);

    self.BNE(
        out_label,
        value,
        m.MIR_new_uint_op(
            self.ctx,
            switch (binary.operator) {
                .QuestionQuestion => Value.Null.val,
                .And => Value.True.val,
                .Or => Value.False.val,
                else => unreachable,
            },
        ),
    );

    self.MOV(
        value,
        (try self.generateNode(binary.right)).?,
    );

    self.append(out_label);

    return value;
}

fn generateBitwise(self: *Self, binary: Ast.Binary) Error!?m.MIR_op_t {
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

    try self.unwrap(
        .Integer,
        (try self.generateNode(binary.left)).?,
        left,
    );
    try self.unwrap(
        .Integer,
        (try self.generateNode(binary.right)).?,
        right,
    );

    switch (binary.operator) {
        .Ampersand => self.AND(res, left, right),
        .Bor => self.OR(res, left, right),
        .Xor => self.XOR(res, left, right),
        .ShiftLeft => self.SHL(res, left, right),
        .ShiftRight => self.SHR(res, left, right),
        else => unreachable,
    }

    try self.wrap(.Integer, res, res);

    return res;
}

fn generateWhile(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].While;
    const condition_value = self.state.?.ast.nodes.items(.value)[components.condition];

    if (condition_value != null and !condition_value.?.boolean()) {
        return null;
    }

    const cond_label = m.MIR_new_label(self.ctx);
    const out_label = m.MIR_new_label(self.ctx);

    const previous_out_label = self.state.?.break_label;
    self.state.?.break_label = out_label;
    const previous_continue_label = self.state.?.continue_label;
    self.state.?.continue_label = cond_label;

    if (components.label != null) {
        try self.state.?.breaks_label.append(
            self.vm.gc.allocator,
            .{
                .node = node,
                .break_label = out_label,
                .continue_label = cond_label,
            },
        );
    }

    self.append(cond_label);

    self.BEQ(
        m.MIR_new_label_op(self.ctx, out_label),
        (try self.generateNode(components.condition)).?,
        m.MIR_new_uint_op(self.ctx, Value.False.val),
    );

    _ = try self.generateNode(components.body);

    self.JMP(cond_label);

    self.append(out_label);

    self.state.?.break_label = previous_out_label;
    self.state.?.continue_label = previous_continue_label;

    if (components.label != null) {
        _ = self.state.?.breaks_label.pop();
    }

    return null;
}

fn generateDoUntil(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].DoUntil;

    const out_label = m.MIR_new_label(self.ctx);
    const loop_label = m.MIR_new_label(self.ctx);

    const previous_out_label = self.state.?.break_label;
    self.state.?.break_label = out_label;
    const previous_continue_label = self.state.?.continue_label;
    self.state.?.continue_label = loop_label;

    if (components.label != null) {
        try self.state.?.breaks_label.append(
            self.vm.gc.allocator,
            .{
                .node = node,
                .break_label = out_label,
                .continue_label = loop_label,
            },
        );
    }

    self.append(loop_label);

    _ = try self.generateNode(components.body);

    self.BEQ(
        m.MIR_new_label_op(self.ctx, loop_label),
        (try self.generateNode(components.condition)).?,
        m.MIR_new_uint_op(self.ctx, Value.False.val),
    );

    self.append(out_label);

    self.state.?.break_label = previous_out_label;
    self.state.?.continue_label = previous_continue_label;

    if (components.label != null) {
        _ = self.state.?.breaks_label.pop();
    }

    return null;
}

fn generateFor(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].For;
    const condition_value = self.state.?.ast.nodes.items(.value)[components.condition];

    if (condition_value != null and !condition_value.?.boolean()) {
        return null;
    }

    const cond_label = m.MIR_new_label(self.ctx);
    const out_label = m.MIR_new_label(self.ctx);
    const previous_out_label = self.state.?.break_label;
    self.state.?.break_label = out_label;
    const previous_continue_label = self.state.?.continue_label;
    self.state.?.continue_label = cond_label;

    if (components.label != null) {
        try self.state.?.breaks_label.append(
            self.vm.gc.allocator,
            .{
                .node = node,
                .break_label = out_label,
                .continue_label = cond_label,
            },
        );
    }

    if (self.state.?.ast_node != node) {
        // Init expressions (if not hotspot)
        for (components.init_declarations) |expr| {
            _ = try self.generateNode(expr);
        }
    }

    // Condition
    self.append(cond_label);

    self.BEQ(
        m.MIR_new_label_op(self.ctx, out_label),
        (try self.generateNode(components.condition)).?,
        m.MIR_new_uint_op(self.ctx, Value.False.val),
    );

    _ = try self.generateNode(components.body);

    // Post loop
    for (components.post_loop) |expr| {
        _ = try self.generateNode(expr);
    }

    self.JMP(cond_label);

    self.append(out_label);

    self.state.?.break_label = previous_out_label;
    self.state.?.continue_label = previous_continue_label;

    if (components.label != null) {
        _ = self.state.?.breaks_label.pop();
    }

    return null;
}

fn findBreakLabel(self: *Self, node: Ast.Node.Index) Break {
    var i = self.state.?.breaks_label.items.len - 1;
    while (i >= 0) : (i -= 1) {
        const brk = self.state.?.breaks_label.items[i];

        if (brk.node == node) {
            return brk;
        }
    }

    // Should not happen: searched when parsing
    unreachable;
}

fn generateBreak(self: *Self, break_node: Ast.Node.Index) Error!?m.MIR_op_t {
    try self.closeScope(break_node);

    if (self.state.?.ast.nodes.items(.components)[break_node].Break.destination) |label_node| {
        self.JMP(self.findBreakLabel(label_node).break_label);
    } else {
        self.JMP(self.state.?.break_label.?);
    }

    return null;
}

fn generateContinue(self: *Self, continue_node: Ast.Node.Index) Error!?m.MIR_op_t {
    try self.closeScope(continue_node);

    if (self.state.?.ast.nodes.items(.components)[continue_node].Continue.destination) |label_node| {
        self.JMP(self.findBreakLabel(label_node).continue_label);
    } else {
        self.JMP(self.state.?.continue_label.?);
    }

    return null;
}

fn generateList(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].List;
    const type_def = self.state.?.ast.nodes.items(.type_def)[node];

    const new_list = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("new_list", m.MIR_T_I64),
    );

    try self.buildExternApiCall(
        .bz_newList,
        new_list,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            m.MIR_new_uint_op(self.ctx, type_def.?.toValue().val),
        },
    );

    // Prevent collection
    try self.buildPush(new_list);

    for (components.items) |item| {
        try self.buildExternApiCall(
            .bz_listAppend,
            null,
            &[_]m.MIR_op_t{
                new_list,
                (try self.generateNode(item)).?,
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            },
        );
    }

    try self.buildPop(null);

    return new_list;
}

fn generateRange(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].Range;

    const new_range = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("new_range", m.MIR_T_I64),
    );

    try self.buildExternApiCall(
        .bz_newRange,
        new_range,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            (try self.generateNode(components.low)).?,
            (try self.generateNode(components.high)).?,
        },
    );

    return new_range;
}

fn generateMap(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].Map;
    const type_def = self.state.?.ast.nodes.items(.type_def)[node];

    const new_map = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("new_map", m.MIR_T_I64),
    );

    try self.buildExternApiCall(
        .bz_newMap,
        new_map,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            m.MIR_new_uint_op(self.ctx, @constCast(type_def.?).toValue().val),
        },
    );

    // Prevent collection
    try self.buildPush(new_map);

    for (components.entries) |entry| {
        try self.buildExternApiCall(
            .bz_mapSet,
            null,
            &[_]m.MIR_op_t{
                new_map,
                (try self.generateNode(entry.key)).?,
                (try self.generateNode(entry.value)).?,
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            },
        );
    }

    try self.buildPop(null);

    return new_map;
}

fn generateDot(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].Dot;
    const type_defs = self.state.?.ast.nodes.items(.type_def);
    const tags = self.state.?.ast.tokens.items(.tag);

    const callee_type = type_defs[components.callee].?;
    const member_lexeme = self.state.?.ast.tokens.items(.lexeme)[components.identifier];
    const member_identifier = (try self.vm.gc.copyString(member_lexeme)).toValue().val;

    switch (callee_type.def_type) {
        .Fiber => {
            switch (components.member_kind) {
                .Call => return try self.generateCall(components.value_or_call_or_enum.Call),
                else => {
                    const res = m.MIR_new_reg_op(
                        self.ctx,
                        try self.REG("res", m.MIR_T_I64),
                    );
                    try self.buildExternApiCall(
                        .bz_getFiberProperty,
                        res,
                        &[_]m.MIR_op_t{
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(
                                self.ctx,
                                o.ObjFiber.members_name.get(member_lexeme).?,
                            ),
                            m.MIR_new_uint_op(self.ctx, 1),
                        },
                    );

                    return res;
                },
            }
        },

        .Pattern => {
            switch (components.member_kind) {
                .Call => return try self.generateCall(components.value_or_call_or_enum.Call),
                else => {
                    const res = m.MIR_new_reg_op(
                        self.ctx,
                        try self.REG("res", m.MIR_T_I64),
                    );
                    try self.buildExternApiCall(
                        .bz_getPatternProperty,
                        res,
                        &[_]m.MIR_op_t{
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(
                                self.ctx,
                                o.ObjPattern.members_name.get(member_lexeme).?,
                            ),
                            m.MIR_new_uint_op(self.ctx, 1),
                        },
                    );

                    return res;
                },
            }
        },

        .String => {
            switch (components.member_kind) {
                .Call => return try self.generateCall(components.value_or_call_or_enum.Call),
                else => {
                    const res = m.MIR_new_reg_op(
                        self.ctx,
                        try self.REG("res", m.MIR_T_I64),
                    );
                    try self.buildExternApiCall(
                        .bz_getStringProperty,
                        res,
                        &[_]m.MIR_op_t{
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(
                                self.ctx,
                                o.ObjString.members_name.get(member_lexeme).?,
                            ),
                            m.MIR_new_uint_op(self.ctx, 1),
                        },
                    );

                    return res;
                },
            }
        },

        .Range => {
            switch (components.member_kind) {
                .Call => return try self.generateCall(components.value_or_call_or_enum.Call),
                else => {
                    const res = m.MIR_new_reg_op(
                        self.ctx,
                        try self.REG("res", m.MIR_T_I64),
                    );
                    try self.buildExternApiCall(
                        .bz_getRangeProperty,
                        res,
                        &[_]m.MIR_op_t{
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(
                                self.ctx,
                                o.ObjRange.members_name.get(member_lexeme).?,
                            ),
                            m.MIR_new_uint_op(self.ctx, 1),
                        },
                    );

                    return res;
                },
            }
        },

        .Object => {
            switch (components.member_kind) {
                .Call => return try self.generateCall(components.value_or_call_or_enum.Call),
                .Value => {
                    const field = callee_type.resolved_type.?.Object.fields
                        .get(member_lexeme).?;

                    const assign_token = tags[components.value_or_call_or_enum.Value.assign_token];
                    const gen_value = (try self.generateNode(components.value_or_call_or_enum.Value.value)).?;

                    if (assign_token == .Equal) {
                        try self.buildExternApiCall(
                            .bz_setObjectField,
                            null,
                            &[_]m.MIR_op_t{
                                (try self.generateNode(components.callee)).?,
                                m.MIR_new_uint_op(self.ctx, field.index),
                                gen_value,
                                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
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
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(self.ctx, field.index),
                        },
                    );

                    try self.buildBinary(
                        assign_token,
                        field.type_def.def_type,
                        res,
                        gen_value,
                        res,
                    );

                    try self.buildExternApiCall(
                        .bz_setObjectField,
                        null,
                        &[_]m.MIR_op_t{
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(self.ctx, field.index),
                            res,
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        },
                    );

                    return res;
                },
                else => {
                    const field = callee_type.resolved_type.?.Object.fields
                        .get(member_lexeme).?;

                    const res = m.MIR_new_reg_op(
                        self.ctx,
                        try self.REG("res", m.MIR_T_I64),
                    );
                    try self.buildExternApiCall(
                        .bz_getObjectField,
                        res,
                        &[_]m.MIR_op_t{
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(self.ctx, field.index),
                        },
                    );

                    return res;
                },
            }
        },

        .ObjectInstance, .ProtocolInstance => {
            switch (components.member_kind) {
                .Call => return try self.generateCall(components.value_or_call_or_enum.Call),
                .Value => {
                    std.debug.assert(callee_type.def_type == .ObjectInstance);
                    const field = callee_type.resolved_type.?.ObjectInstance.of
                        .resolved_type.?.Object.fields
                        .get(member_lexeme).?;

                    const assign_token = tags[components.value_or_call_or_enum.Value.assign_token];
                    const gen_value = (try self.generateNode(components.value_or_call_or_enum.Value.value)).?;

                    if (assign_token == .Equal) {
                        try self.buildExternApiCall(
                            .bz_setObjectInstanceProperty,
                            null,
                            &[_]m.MIR_op_t{
                                (try self.generateNode(components.callee)).?,
                                m.MIR_new_uint_op(
                                    self.ctx,
                                    field.index,
                                ),
                                gen_value,
                                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                            },
                        );

                        return gen_value;
                    }

                    const res = m.MIR_new_reg_op(
                        self.ctx,
                        try self.REG("res", m.MIR_T_I64),
                    );

                    try self.buildExternApiCall(
                        .bz_getObjectInstanceProperty,
                        res,
                        &[_]m.MIR_op_t{
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(self.ctx, field.index),
                        },
                    );

                    try self.buildBinary(
                        assign_token,
                        field.type_def.def_type,
                        res,
                        gen_value,
                        res,
                    );

                    try self.buildExternApiCall(
                        .bz_setObjectInstanceProperty,
                        null,
                        &[_]m.MIR_op_t{
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(
                                self.ctx,
                                field.index,
                            ),
                            res,
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        },
                    );

                    return res;
                },
                else => {
                    const field = if (callee_type.def_type == .ObjectInstance)
                        callee_type.resolved_type.?.ObjectInstance.of
                            .resolved_type.?.Object.fields
                            .get(member_lexeme)
                    else
                        null;

                    const res = m.MIR_new_reg_op(
                        self.ctx,
                        try self.REG("res", m.MIR_T_I64),
                    );

                    if (field) |f| {
                        if (f.method) {
                            try self.buildExternApiCall(
                                .bz_getObjectInstanceMethod,
                                res,
                                &[_]m.MIR_op_t{
                                    (try self.generateNode(components.callee)).?,
                                    m.MIR_new_uint_op(self.ctx, f.index),
                                    m.MIR_new_uint_op(self.ctx, 1),
                                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                                },
                            );
                        } else {
                            try self.buildExternApiCall(
                                .bz_getObjectInstanceProperty,
                                res,
                                &[_]m.MIR_op_t{
                                    (try self.generateNode(components.callee)).?,
                                    m.MIR_new_uint_op(self.ctx, f.index),
                                },
                            );
                        }
                    } else {
                        try self.buildExternApiCall(
                            .bz_getProtocolMethod,
                            res,
                            &[_]m.MIR_op_t{
                                (try self.generateNode(components.callee)).?,
                                m.MIR_new_uint_op(self.ctx, member_identifier),
                                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                            },
                        );
                    }

                    return res;
                },
            }
        },

        .ForeignContainer => {
            switch (components.member_kind) {
                .Call => return try self.generateCall(components.value_or_call_or_enum.Call),
                .Value => {
                    const field_type_def = callee_type.resolved_type.?.ForeignContainer
                        .buzz_type
                        .get(member_lexeme).?;
                    const field_index = callee_type.resolved_type.?.ForeignContainer
                        .fields
                        .getIndex(member_lexeme).?;

                    const assign_token = tags[components.value_or_call_or_enum.Value.assign_token];
                    const gen_value = (try self.generateNode(components.value_or_call_or_enum.Value.value)).?;

                    if (assign_token == .Equal) {
                        try self.buildExternApiCall(
                            .bz_foreignContainerSet,
                            null,
                            &[_]m.MIR_op_t{
                                (try self.generateNode(components.callee)).?,
                                m.MIR_new_uint_op(
                                    self.ctx,
                                    field_index,
                                ),
                                gen_value,
                                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                            },
                        );

                        return gen_value;
                    }

                    const res = m.MIR_new_reg_op(
                        self.ctx,
                        try self.REG("res", m.MIR_T_I64),
                    );

                    try self.buildExternApiCall(
                        .bz_foreignContainerGet,
                        res,
                        &[_]m.MIR_op_t{
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(
                                self.ctx,
                                field_index,
                            ),
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        },
                    );

                    try self.buildBinary(
                        assign_token,
                        field_type_def.def_type,
                        res,
                        gen_value,
                        res,
                    );

                    try self.buildExternApiCall(
                        .bz_foreignContainerSet,
                        null,
                        &[_]m.MIR_op_t{
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(
                                self.ctx,
                                field_index,
                            ),
                            res,
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        },
                    );

                    return res;
                },
                else => {
                    const res = m.MIR_new_reg_op(
                        self.ctx,
                        try self.REG("res", m.MIR_T_I64),
                    );

                    try self.buildExternApiCall(
                        .bz_foreignContainerGet,
                        res,
                        &[_]m.MIR_op_t{
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(
                                self.ctx,
                                callee_type.resolved_type.?.ForeignContainer
                                    .fields
                                    .getIndex(member_lexeme).?,
                            ),
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                        },
                    );

                    return res;
                },
            }
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
                    (try self.generateNode(components.callee)).?,
                    m.MIR_new_uint_op(self.ctx, member_identifier),
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
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
                .bz_getEnumInstanceValue,
                res,
                &[_]m.MIR_op_t{
                    (try self.generateNode(components.callee)).?,
                },
            );

            return res;
        },

        .List => {
            switch (components.member_kind) {
                .Call => return try self.generateCall(components.value_or_call_or_enum.Call),
                else => {
                    const res = m.MIR_new_reg_op(
                        self.ctx,
                        try self.REG("res", m.MIR_T_I64),
                    );
                    try self.buildExternApiCall(
                        .bz_getListProperty,
                        res,
                        &[_]m.MIR_op_t{
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(self.ctx, o.ObjList.members_name.get(member_lexeme).?),
                            m.MIR_new_uint_op(self.ctx, 1),
                        },
                    );

                    return res;
                },
            }
        },

        .Map => {
            switch (components.member_kind) {
                .Call => return try self.generateCall(components.value_or_call_or_enum.Call),
                else => {
                    const res = m.MIR_new_reg_op(
                        self.ctx,
                        try self.REG("res", m.MIR_T_I64),
                    );
                    try self.buildExternApiCall(
                        .bz_getMapProperty,
                        res,
                        &[_]m.MIR_op_t{
                            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                            (try self.generateNode(components.callee)).?,
                            m.MIR_new_uint_op(self.ctx, o.ObjMap.members_name.get(member_lexeme).?),
                            m.MIR_new_uint_op(self.ctx, 1),
                        },
                    );

                    return res;
                },
            }
        },

        else => unreachable,
    }
}

fn generateSubscript(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].Subscript;
    const type_defs = self.state.?.ast.nodes.items(.type_def);

    const subscripted = (try self.generateNode(components.subscripted)).?;
    const index_val = (try self.generateNode(components.index)).?;
    const value = if (components.value) |val| (try self.generateNode(val)).? else null;

    switch (type_defs[components.subscripted].?.def_type) {
        .List => {
            const index = m.MIR_new_reg_op(
                self.ctx,
                try self.REG("index", m.MIR_T_I64),
            );

            try self.unwrap(.Integer, index_val, index);

            if (value) |val| {
                try self.buildExternApiCall(
                    .bz_listSet,
                    null,
                    &[_]m.MIR_op_t{
                        subscripted,
                        index,
                        val,
                        m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
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
                    m.MIR_new_uint_op(
                        self.ctx,
                        if (components.checked) 1 else 0,
                    ),
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
                .bz_stringSubscript,
                res,
                &[_]m.MIR_op_t{
                    subscripted,
                    index_val,
                    m.MIR_new_uint_op(
                        self.ctx,
                        if (components.checked) 1 else 0,
                    ),
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
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
                        subscripted,
                        index_val,
                        val,
                        m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
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

fn generateIs(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].Is;

    const res = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("res", m.MIR_T_I64),
    );

    try self.buildExternApiCall(
        .bz_valueIs,
        res,
        &[_]m.MIR_op_t{
            (try self.generateNode(components.left)).?,
            m.MIR_new_uint_op(
                self.ctx,
                self.state.?.ast.nodes.items(.value)[components.constant].?.val,
            ),
        },
    );

    return res;
}

fn generateAs(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].As;

    const left = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("left", m.MIR_T_I64),
    );
    self.MOV(
        left,
        (try self.generateNode(components.left)).?,
    );

    const res = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("res", m.MIR_T_I64),
    );

    try self.buildExternApiCall(
        .bz_valueIs,
        res,
        &[_]m.MIR_op_t{
            left,
            m.MIR_new_uint_op(
                self.ctx,
                self.state.?.ast.nodes.items(.value)[components.constant].?.val,
            ),
        },
    );

    const casted_label = m.MIR_new_label(self.ctx);

    self.BEQ(
        m.MIR_new_label_op(self.ctx, casted_label),
        res,
        m.MIR_new_uint_op(self.ctx, Value.True.val),
    );

    self.MOV(
        left,
        m.MIR_new_uint_op(self.ctx, Value.Null.val),
    );

    self.append(casted_label);

    return left;
}

fn generateTry(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].Try;
    const type_defs = self.state.?.ast.nodes.items(.type_def);

    const raise_label = m.MIR_new_label(self.ctx);
    const out_label = m.MIR_new_label(self.ctx);
    const catch_label = m.MIR_new_label(self.ctx);
    var clause_labels = std.ArrayList(m.MIR_insn_t){};
    defer clause_labels.deinit(self.vm.gc.allocator);

    for (components.clauses) |_| {
        try clause_labels.append(
            self.vm.gc.allocator,
            m.MIR_new_label(self.ctx),
        );
    }

    const unconditional_label = if (components.unconditional_clause != null)
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

    _ = try self.generateNode(components.body);

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

    for (components.clauses, 0..) |clause, idx| {
        const label = clause_labels.items[idx];

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
                m.MIR_new_uint_op(self.ctx, @constCast(type_defs[clause.type_def].?).toValue().val),
            },
        );

        try self.unwrap(
            .Bool,
            m.MIR_new_reg_op(self.ctx, matches),
            m.MIR_new_reg_op(self.ctx, matches),
        );

        self.BEQ(
            m.MIR_new_label_op(
                self.ctx,
                if (idx < components.clauses.len - 1)
                    clause_labels.items[idx + 1]
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

        _ = try self.generateNode(clause.body);

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

        _ = try self.generateNode(components.unconditional_clause.?);

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

fn generateThrow(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].Throw;

    if (components.unconditional) {
        self.state.?.return_emitted = true;
    }

    try self.buildExternApiCall(
        .bz_throw,
        null,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            (try self.generateNode(components.expression)).?,
        },
    );

    return null;
}

fn generateUnwrap(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].Unwrap;

    const value = (try self.generateNode(components.unwrapped)).?;

    // Remember that we need to had a terminator to this block that will jump at the end of the optionals chain
    if (self.state.?.opt_jumps.items.len == 0 or components.start_opt_jumps) {
        try self.state.?.opt_jumps.append(
            self.vm.gc.allocator,
            .{
                // Store the value on the stack, that spot will be overwritten with the final value of the optional chain
                .alloca = try self.REG("opt", m.MIR_T_I64),
                .current_insn = .{},
            },
        );
    } else if (self.state.?.opt_jumps.items.len == 0) {
        @panic("Unwrap node not marked as starting opt_jumps but not ongoing opt_jumps");
    }

    const current_insn = m.MIR_new_insn_arr(
        self.ctx,
        @intFromEnum(m.MIR_Instruction.MOV),
        2,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(
                self.ctx,
                self.state.?.opt_jumps.items[@intCast(self.state.?.opt_jumps.items.len - 1)].alloca,
            ),
            value,
        },
    );

    self.append(
        current_insn,
    );

    try self.state.?.opt_jumps.items[@intCast(self.state.?.opt_jumps.items.len - 1)]
        .current_insn
        .append(self.vm.gc.allocator, current_insn);

    return value;
}

fn generateObjectInit(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const lexemes = self.state.?.ast.tokens.items(.lexeme);
    const components = self.state.?.ast.nodes.items(.components)[node].ObjectInit;
    const type_defs = self.state.?.ast.nodes.items(.type_def);
    const type_def = type_defs[node];

    if (type_def.?.def_type == .ForeignContainer) {
        return self.generateForeignContainerInit(node);
    }

    const object = if (components.object != null and type_defs[components.object.?].?.def_type == .Object)
        (try self.generateNode(components.object.?)).?
    else
        m.MIR_new_uint_op(self.ctx, Value.Null.val);

    const typedef = m.MIR_new_uint_op(
        self.ctx,
        @constCast(type_def.?).toValue().val,
    );

    const instance = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("instance", m.MIR_T_I64),
    );
    try self.buildExternApiCall(
        .bz_newObjectInstance,
        instance,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            object,
            typedef,
        },
    );

    // Push to prevent collection
    try self.buildPush(instance);

    for (components.properties) |property| {
        try self.buildExternApiCall(
            .bz_setObjectInstanceProperty,
            null,
            &[_]m.MIR_op_t{
                instance,
                m.MIR_new_uint_op(
                    self.ctx,
                    type_def.?.resolved_type.?.ObjectInstance.of
                        .resolved_type.?.Object.fields
                        .get(lexemes[property.name]).?.index,
                ),
                (try self.generateNode(property.value)).?,
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            },
        );
    }

    try self.buildPop(instance);

    return instance;
}

fn generateForeignContainerInit(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].ObjectInit;
    const type_defs = self.state.?.ast.nodes.items(.type_def);
    const type_def = type_defs[node];
    const lexemes = self.state.?.ast.tokens.items(.lexeme);

    const instance = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("instance", m.MIR_T_I64),
    );
    try self.buildExternApiCall(
        .bz_newForeignContainerInstance,
        instance,
        &[_]m.MIR_op_t{
            m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            m.MIR_new_uint_op(
                self.ctx,
                @constCast(type_def.?).toValue().val,
            ),
        },
    );

    for (components.properties) |property| {
        try self.buildExternApiCall(
            .bz_foreignContainerSet,
            null,
            &[_]m.MIR_op_t{
                instance,
                m.MIR_new_uint_op(
                    self.ctx,
                    type_def.?.resolved_type.?.ForeignContainer
                        .fields
                        .getIndex(lexemes[property.name]).?,
                ),
                (try self.generateNode(property.value)).?,
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            },
        );
    }

    return instance;
}

fn generateForceUnwrap(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].ForceUnwrap;

    const expr = (try self.generateNode(components.unwrapped)).?;

    const out_label = m.MIR_new_label(self.ctx);

    self.BNE(
        out_label,
        expr,
        m.MIR_new_uint_op(self.ctx, Value.Null.val),
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

fn generateUnary(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].Unary;
    const left_type_def = self.state.?.ast.nodes.items(.type_def)[components.expression];

    const left = (try self.generateNode(components.expression)).?;
    const result = m.MIR_new_reg_op(
        self.ctx,
        try self.REG("result", m.MIR_T_I64),
    );

    switch (components.operator) {
        .Bnot => {
            try self.unwrap(.Integer, left, result);
            self.NOT(result, result);
            try self.wrap(.Integer, result, result);
        },
        .Bang => {
            try self.unwrap(.Bool, left, result);

            const true_label = m.MIR_new_label(self.ctx);
            const out_label = m.MIR_new_label(self.ctx);

            self.BEQ(
                m.MIR_new_label_op(self.ctx, out_label),
                result,
                m.MIR_new_uint_op(self.ctx, 1),
            );

            self.MOV(
                result,
                m.MIR_new_uint_op(self.ctx, Value.True.val),
            );

            self.JMP(out_label);

            self.append(true_label);

            self.MOV(
                result,
                m.MIR_new_uint_op(self.ctx, Value.False.val),
            );

            self.append(out_label);
        },
        .Minus => {
            try self.unwrap(.Integer, left, result);

            if (left_type_def.?.def_type == .Integer) {
                self.NEG(result, result);
            } else {
                self.DNEG(result, result);
            }

            try self.wrap(
                left_type_def.?.def_type,
                result,
                result,
            );
        },
        else => unreachable,
    }

    return result;
}

fn generatePattern(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    return m.MIR_new_uint_op(
        self.ctx,
        self.state.?.ast.nodes.items(.components)[node].Pattern.toValue().val,
    );
}

fn generateForEach(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].ForEach;
    const iterable_type_def = self.state.?.ast.nodes.items(.type_def)[components.iterable];

    // If iterable is empty constant, skip the node
    if (self.state.?.ast.nodes.items(.value)[components.iterable]) |iterable| {
        if (switch (iterable.obj().obj_type) {
            .List => o.ObjList.cast(iterable.obj()).?.items.items.len == 0,
            .Map => o.ObjMap.cast(iterable.obj()).?.map.count() == 0,
            .String => o.ObjString.cast(iterable.obj()).?.string.len == 0,
            .Enum => o.ObjEnum.cast(iterable.obj()).?.cases.len == 0,
            .Range => o.ObjRange.cast(iterable.obj()).?.high == o.ObjRange.cast(iterable.obj()).?.low,
            else => unreachable,
        }) {
            return null;
        }
    }

    const iterable = if (self.state.?.ast_node != node) regular: {
        // key, value and iterable are locals of the foreach scope
        // var declaration so will push value on stack
        _ = try self.generateNode(components.key);
        // var declaration so will push value on stack
        _ = try self.generateNode(components.value);
        const iterable = (try self.generateNode(components.iterable)).?;
        try self.buildPush(iterable);

        break :regular iterable;
    } else hotspot: {
        // When the loop is a hotspot, the foreach setup has already been done and the iterable is at the top of the stack
        const iterable = m.MIR_new_reg_op(
            self.ctx,
            try self.REG("iterable", m.MIR_T_I64),
        );

        try self.buildPeek(
            0,
            iterable,
        );

        break :hotspot iterable;
    };

    const key_ptr = try self.buildStackPtr(2);
    const value_ptr = try self.buildStackPtr(1);

    const cond_label = m.MIR_new_label(self.ctx);
    const out_label = m.MIR_new_label(self.ctx);

    const previous_out_label = self.state.?.break_label;
    self.state.?.break_label = out_label;
    const previous_continue_label = self.state.?.continue_label;
    self.state.?.continue_label = cond_label;

    if (components.label != null) {
        try self.state.?.breaks_label.append(
            self.vm.gc.allocator,
            .{
                .node = node,
                .break_label = out_label,
                .continue_label = cond_label,
            },
        );
    }

    self.append(cond_label);

    // Call appropriate `next` method
    if (iterable_type_def.?.def_type == .Fiber) {
        // TODO: fiber foreach (tricky, need to complete foreach op after it has yielded)
        return Error.CantCompile;
    } else if (iterable_type_def.?.def_type == .Enum) {
        try self.buildExternApiCall(
            .bz_enumNext,
            try self.LOAD(value_ptr),
            &[_]m.MIR_op_t{
                iterable,
                try self.LOAD(value_ptr),
                m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
            },
        );

        // If next key is null stop, otherwise do loop
        self.BEQ(
            m.MIR_new_label_op(self.ctx, out_label),
            try self.LOAD(value_ptr),
            m.MIR_new_uint_op(self.ctx, Value.Null.val),
        );
    } else if (iterable_type_def.?.def_type == .Range) {
        try self.buildExternApiCall(
            .bz_rangeNext,
            try self.LOAD(value_ptr),
            &[_]m.MIR_op_t{
                iterable,
                try self.LOAD(value_ptr),
            },
        );

        // If next key is null stop, otherwise do loop
        self.BEQ(
            m.MIR_new_label_op(self.ctx, out_label),
            try self.LOAD(value_ptr),
            m.MIR_new_uint_op(self.ctx, Value.Null.val),
        );
    } else {
        // The `next` method will store the new key in the key local
        try self.buildExternApiCall(
            switch (iterable_type_def.?.def_type) {
                .String => .bz_stringNext,
                .List => .bz_listNext,
                .Map => .bz_mapNext,
                else => unreachable,
            },
            try self.LOAD(value_ptr),
            if (iterable_type_def.?.def_type == .Map)
                &[_]m.MIR_op_t{
                    iterable,
                    // Pass ptr so the method can put he new key in it
                    key_ptr,
                }
            else
                &[_]m.MIR_op_t{
                    iterable,
                    // Pass ptr so the method can put he new key in it
                    key_ptr,
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                },
        );

        // If next key is null stop, otherwise loop
        self.BEQ(
            m.MIR_new_label_op(self.ctx, out_label),
            try self.LOAD(key_ptr),
            m.MIR_new_uint_op(self.ctx, Value.Null.val),
        );
    }

    _ = try self.generateNode(components.body);

    self.JMP(cond_label);

    self.append(out_label);

    self.state.?.break_label = previous_out_label;
    self.state.?.continue_label = previous_continue_label;

    if (components.label != null) {
        _ = self.state.?.breaks_label.pop();
    }

    return null;
}

fn generateBlock(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    for (self.state.?.ast.nodes.items(.components)[node].Block) |statement| {
        _ = try self.generateNode(statement);
    }

    return null;
}

fn generateBlockExpression(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const statements = self.state.?.ast.nodes.items(.components)[node].BlockExpression;

    var out_statement: ?m.MIR_op_t = null;
    for (statements) |statement| {
        out_statement = try self.generateNode(statement);
    }

    return if (statements.len > 0 and self.state.?.ast.nodes.items(.tag)[statements[statements.len - 1]] == .Out)
        out_statement.?
    else
        null;
}

fn generateFunDeclaration(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    return try self.generateFunction(
        self.state.?.ast.nodes.items(.components)[node].FunDeclaration.function,
    );
}

fn generateVarDeclaration(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].VarDeclaration;

    // We should only declare locals
    std.debug.assert(components.slot_type == .Local);

    try self.buildPush(
        if (components.value) |value|
            (try self.generateNode(value)).?
        else
            m.MIR_new_uint_op(
                self.ctx,
                Value.Null.val,
            ),
    );

    return null;
}

fn generateFunction(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const components = self.state.?.ast.nodes.items(.components)[node].Function;
    const function_signature = self.state.?.ast.nodes.items(.components)[components.function_signature.?].FunctionType;
    const type_defs = self.state.?.ast.nodes.items(.type_def);

    const function_def = type_defs[node].?.resolved_type.?.Function;
    const function_type = function_def.function_type;

    // Those are not allowed to be compiled
    std.debug.assert(function_type != .Extern and function_type != .Script and function_type != .ScriptEntryPoint);

    // Get fully qualified name of function
    const qualified_name = try self.getQualifiedName(node, true);
    defer self.vm.gc.allocator.free(qualified_name);

    // If this is not the root function, we need to compile this later
    if (self.state.?.ast_node != node) {
        const nativefn_qualified_name = try self.getQualifiedName(node, false);
        defer self.vm.gc.allocator.free(nativefn_qualified_name);

        // Remember that we need to compile this function later
        try self.functions_queue.put(self.vm.gc.allocator, node, null);

        // For now declare it
        const native_raw = m.MIR_new_import(self.ctx, @ptrCast(qualified_name));
        const native = m.MIR_new_import(self.ctx, @ptrCast(nativefn_qualified_name));

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
                m.MIR_new_uint_op(self.ctx, node),
                m.MIR_new_ref_op(self.ctx, native),
                m.MIR_new_ref_op(self.ctx, native_raw),
            },
        );

        return dest;
    }

    // FIXME: I don't get why we need this: a simple constant becomes rubbish as soon as we enter MIR_new_func_arr if we don't
    const ctx_name = self.vm.gc.allocator.dupeZ(u8, "ctx") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(ctx_name);
    const function = m.MIR_new_func_arr(
        self.ctx,
        @ptrCast(qualified_name),
        1,
        &[_]m.MIR_type_t{m.MIR_T_U64},
        1,
        &[_]m.MIR_var_t{
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast(ctx_name.ptr),
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

    if (function_signature.lambda) {
        try self.buildReturn(
            (try self.generateNode(components.body.?)) orelse m.MIR_new_uint_op(
                self.ctx,
                Value.Void.val,
            ),
        );

        self.state.?.return_emitted = true;
    } else {
        _ = try self.generateNode(components.body.?);
    }

    if (type_defs[self.state.?.ast_node].?.resolved_type.?.Function.return_type.def_type == .Void and !self.state.?.return_emitted) {
        try self.buildReturn(m.MIR_new_uint_op(self.ctx, Value.Void.val));
    }

    m.MIR_finish_func(self.ctx);

    // Add the NativeFn version of the function
    const native_fn = try self.generateNativeFn(node, function);

    try self.functions_queue.put(
        self.vm.gc.allocator,
        node,
        [_]m.MIR_item_t{
            native_fn,
            self.state.?.function.?,
        },
    );

    return m.MIR_new_ref_op(self.ctx, function);
}

fn generateHotspotFunction(self: *Self, node: Ast.Node.Index) Error!?m.MIR_op_t {
    const tag = self.state.?.ast.nodes.items(.tag)[node];

    std.debug.assert(tag.isHotspot());

    const qualified_name = try self.getQualifiedName(node, false);
    defer self.vm.gc.allocator.free(qualified_name);

    const ctx_name = self.vm.gc.allocator.dupeZ(u8, "ctx") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(ctx_name);
    const function = m.MIR_new_func_arr(
        self.ctx,
        @ptrCast(qualified_name),
        1,
        &[_]m.MIR_type_t{m.MIR_T_U64},
        1,
        &[_]m.MIR_var_t{
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast(ctx_name.ptr),
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

    _ = self.generateNode(node) catch |err| {
        if (err == error.CantCompile) {
            if (BuildOptions.jit_debug) {
                io.print(
                    "Not compiling node {s}#{}, likely because it uses a fiber\n",
                    .{
                        @tagName(self.state.?.ast.nodes.items(.tag)[self.state.?.ast_node]),
                        self.state.?.ast_node,
                    },
                );
            }

            m.MIR_finish_func(self.ctx);

            try self.blacklisted_nodes.put(self.vm.gc.allocator, self.state.?.ast_node, {});
        }

        return err;
    };

    // If we reach here, return 0 meaning there was no early return in the hotspot
    self.RET(m.MIR_new_int_op(self.ctx, 0));

    m.MIR_finish_func(self.ctx);

    try self.functions_queue.put(
        self.vm.gc.allocator,
        node,
        [_]?m.MIR_item_t{
            null,
            self.state.?.function.?,
        },
    );

    return m.MIR_new_ref_op(self.ctx, function);
}

fn generateNativeFn(self: *Self, node: Ast.Node.Index, raw_fn: m.MIR_item_t) !m.MIR_item_t {
    const type_defs = self.state.?.ast.nodes.items(.type_def);

    const function_def = type_defs[node].?.resolved_type.?.Function;
    const function_type = function_def.function_type;

    std.debug.assert(function_type != .Extern);

    const nativefn_qualified_name = try self.getQualifiedName(node, false);
    defer self.vm.gc.allocator.free(nativefn_qualified_name);

    // FIXME: I don't get why we need this: a simple constant becomes rubbish as soon as we enter MIR_new_func_arr if we don't
    const ctx_name = self.vm.gc.allocator.dupeZ(u8, "ctx") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(ctx_name);
    const function = m.MIR_new_func_arr(
        self.ctx,
        @ptrCast(nativefn_qualified_name),
        1,
        &[_]m.MIR_type_t{m.MIR_T_I64},
        1,
        &[_]m.MIR_var_t{
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast(ctx_name.ptr),
                .size = undefined,
            },
        },
    );

    const previous = self.state.?.function;
    self.state.?.function = function;
    defer self.state.?.function = previous;

    const ctx_reg = m.MIR_reg(self.ctx, "ctx", function.u.func);
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

    const should_try = function_type == .Test or (function_def.error_types != null and function_def.error_types.?.len > 0);
    if (should_try) {
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

        // Unwind TryCtx
        try self.buildExternApiCall(
            .bz_popTryCtx,
            null,
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, vm_reg),
            },
        );

        // Payload already on stack so juste return -1;
        self.RET(m.MIR_new_int_op(self.ctx, -1));

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
            @intFromEnum(m.MIR_Instruction.CALL),
            4,
            &[_]m.MIR_op_t{
                m.MIR_new_ref_op(self.ctx, try ExternApi.RawFn.declare(self)),
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
    } else {
        try self.buildPush(m.MIR_new_uint_op(self.ctx, Value.Void.val));
    }

    if (should_try) {
        // Unwind TryCtx
        try self.buildExternApiCall(
            .bz_popTryCtx,
            null,
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, vm_reg),
            },
        );
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

fn getQualifiedName(self: *Self, node: Ast.Node.Index, raw: bool) ![]const u8 {
    const tag = self.state.?.ast.nodes.items(.tag)[node];

    switch (tag) {
        .Function => {
            const components = self.state.?.ast.nodes.items(.components)[node].Function;
            const type_defs = self.state.?.ast.nodes.items(.type_def);

            const function_def = type_defs[node].?.resolved_type.?.Function;
            const function_type = function_def.function_type;
            const name = function_def.name.string;

            var qualified_name = std.ArrayList(u8){};

            try qualified_name.appendSlice(self.vm.gc.allocator, name);

            // Main and script are not allowed to be compiled
            std.debug.assert(function_type != .ScriptEntryPoint and function_type != .Script);

            // Don't qualify extern functions
            if (function_type != .Extern) {
                try qualified_name.writer(self.vm.gc.allocator).print(
                    ".{}.n{}",
                    .{
                        components.id,
                        node,
                    },
                );
            }
            if (function_type != .Extern and raw) {
                try qualified_name.appendSlice(self.vm.gc.allocator, ".raw");
            }
            try qualified_name.append(self.vm.gc.allocator, 0);

            return qualified_name.toOwnedSlice(self.vm.gc.allocator);
        },

        .For,
        .ForEach,
        .While,
        => {
            var qualified_name = std.ArrayList(u8){};

            try qualified_name.writer(self.vm.gc.allocator).print(
                "{s}#{d}\x00",
                .{
                    @tagName(tag),
                    node,
                },
            );

            return qualified_name.toOwnedSlice(self.vm.gc.allocator);
        },

        else => {
            if (BuildOptions.debug) {
                io.print(
                    "Ast {s} node are not valid hotspots",
                    .{
                        @tagName(tag),
                    },
                );
            }

            unreachable;
        },
    }
}

pub fn compileZdefContainer(self: *Self, ast: Ast.Slice, zdef_element: Ast.Zdef.ZdefElement) Error!void {
    var wrapper_name = std.ArrayList(u8){};
    defer wrapper_name.deinit(self.vm.gc.allocator);

    try wrapper_name.writer(self.vm.gc.allocator).print("zdef_{s}\x00", .{zdef_element.zdef.name});

    const module = m.MIR_new_module(self.ctx, @ptrCast(wrapper_name.items.ptr));
    defer m.MIR_finish_module(self.ctx);

    if (BuildOptions.jit_debug) {
        io.print(
            "Compiling zdef struct getters/setters for `{s}` of type `{s}`\n",
            .{
                zdef_element.zdef.name,
                zdef_element.zdef.type_def.toStringAlloc(self.vm.gc.allocator) catch unreachable,
            },
        );
    }

    // FIXME: Not everything applies to a zdef, maybe split the two states?
    self.state = .{
        .ast = ast,
        .module = module,
        .prototypes = .{},
        .ast_node = undefined,
        .registers = .{},
        .closure = undefined,
        .breaks_label = .{},
    };
    defer self.reset(self.vm.gc.allocator);

    const foreign_def = zdef_element.zdef.type_def.resolved_type.?.ForeignContainer;

    var getters = std.ArrayList(m.MIR_item_t).empty;
    defer getters.deinit(self.vm.gc.allocator);
    var setters = std.ArrayList(m.MIR_item_t).empty;
    defer setters.deinit(self.vm.gc.allocator);

    switch (foreign_def.zig_type) {
        .Struct => {
            for (foreign_def.zig_type.Struct.fields) |field| {
                const container_field = foreign_def.fields.getEntry(field.name).?;

                try getters.append(
                    self.vm.gc.allocator,
                    try self.buildZdefContainerGetter(
                        container_field.value_ptr.*.offset,
                        foreign_def.name.string,
                        field.name,
                        foreign_def.buzz_type.get(field.name).?,
                        field.type,
                    ),
                );

                try setters.append(
                    self.vm.gc.allocator,
                    try self.buildZdefContainerSetter(
                        container_field.value_ptr.*.offset,
                        foreign_def.name.string,
                        field.name,
                        foreign_def.buzz_type.get(field.name).?,
                        field.type,
                    ),
                );
            }
        },

        .Union => {
            for (foreign_def.zig_type.Union.fields) |field| {
                const container_field = foreign_def.fields.getEntry(field.name).?;
                _ = container_field;

                try getters.append(
                    self.vm.gc.allocator,
                    try self.buildZdefUnionGetter(
                        foreign_def.name.string,
                        field.name,
                        foreign_def.buzz_type.get(field.name).?,
                        field.type,
                    ),
                );

                try setters.append(
                    self.vm.gc.allocator,
                    try self.buildZdefUnionSetter(
                        foreign_def.name.string,
                        field.name,
                        foreign_def.buzz_type.get(field.name).?,
                        field.type,
                    ),
                );
            }
        },

        else => unreachable,
    }

    if (BuildOptions.jit_debug) {
        self.outputModule(wrapper_name.items, module);
    }

    m.MIR_load_module(self.ctx, module);

    // Load external functions
    var it_ext = self.required_ext_api.iterator();
    while (it_ext.next()) |kv| {
        switch (kv.key_ptr.*) {
            // TODO: don't mix those with actual api functions
            .RawFn, .NativeFn => {},
            else => m.MIR_load_external(
                self.ctx,
                kv.key_ptr.*.name(),
                kv.key_ptr.*.ptr(),
            ),
        }
    }

    // Link everything together
    m.MIR_link(self.ctx, m.MIR_set_lazy_gen_interface, null);

    m.MIR_gen_init(self.ctx);
    defer m.MIR_gen_finish(self.ctx);

    // Gen getters/setters
    switch (foreign_def.zig_type) {
        .Struct => {
            for (foreign_def.zig_type.Struct.fields, 0..) |field, idx| {
                const struct_field = foreign_def.fields.getEntry(field.name).?;
                struct_field.value_ptr.*.getter = @ptrCast(@alignCast(m.MIR_gen(
                    self.ctx,
                    getters.items[idx],
                ) orelse unreachable));

                struct_field.value_ptr.*.setter = @ptrCast(@alignCast(m.MIR_gen(
                    self.ctx,
                    setters.items[idx],
                ) orelse unreachable));
            }
        },
        .Union => {
            for (foreign_def.zig_type.Union.fields, 0..) |field, idx| {
                const struct_field = foreign_def.fields.getEntry(field.name).?;
                struct_field.value_ptr.*.getter = @ptrCast(@alignCast(m.MIR_gen(
                    self.ctx,
                    getters.items[idx],
                ) orelse unreachable));

                struct_field.value_ptr.*.setter = @ptrCast(@alignCast(m.MIR_gen(
                    self.ctx,
                    setters.items[idx],
                ) orelse unreachable));
            }
        },
        else => unreachable,
    }
}

fn buildBuzzValueToZigValue(self: *Self, buzz_type: *o.ObjTypeDef, zig_type: ZigType, buzz_value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    switch (zig_type) {
        .Int => {
            if (buzz_type.def_type == .Double) {
                const tmp_float = m.MIR_new_reg_op(
                    self.ctx,
                    try self.REG("tmp_float", m.MIR_T_D),
                );

                // This is a int represented by a buzz double value
                try self.buildValueToDouble(buzz_value, tmp_float);

                // Convert it back to an int
                self.D2I(dest, tmp_float);
            } else {
                try self.buildValueToInteger(buzz_value, dest);
            }
        },
        // TODO: double can't be truncated like ints, we need a D2F instruction
        .Double => {
            if (zig_type.Double.bits == 64) {
                try self.buildValueToDouble(buzz_value, dest);
            } else {
                std.debug.assert(zig_type.Double.bits == 32);
                try self.buildValueToFloat(buzz_value, dest);
            }
        },
        .Bool => self.buildValueToBoolean(buzz_value, dest),
        .Pointer => {
            // Is it a [*:0]const u8
            if (zig_type.Pointer.child.* == .Int and
                zig_type.Pointer.child.Int.bits == 8 and
                zig_type.Pointer.child.Int.signedness == .unsigned)
            {
                try self.buildValueToCString(buzz_value, dest);
            } else if (zig_type.Pointer.child.* == .Struct or zig_type.Pointer.child.* == .Union) {
                try self.buildValueToForeignContainerPtr(buzz_value, dest);
            } else {
                try self.buildValueToUserData(buzz_value, dest);
            }
        },
        .Optional => {
            // Is it a [*:0]const u8
            if (zig_type.Optional.child.Pointer.child.* == .Int and
                zig_type.Optional.child.Pointer.child.Int.bits == 8 and
                zig_type.Optional.child.Pointer.child.Int.signedness == .unsigned)
            {
                try self.buildValueToOptionalCString(buzz_value, dest);
            } else if (zig_type.Optional.child.Pointer.child.* == .Struct) {
                try self.buildValueToOptionalForeignContainerPtr(buzz_value, dest);
            } else {
                try self.buildValueToOptionalUserData(buzz_value, dest);
            }
        },
        else => unreachable,
    }
}

fn buildZigValueToBuzzValue(self: *Self, buzz_type: *o.ObjTypeDef, zig_type: ZigType, zig_value: m.MIR_op_t, dest: m.MIR_op_t) !void {
    switch (zig_type) {
        .Int => {
            if (buzz_type.def_type == .Double) {
                // This is a int represented by a buzz double value
                const tmp_float = m.MIR_new_reg_op(
                    self.ctx,
                    try self.REG("tmp_float", m.MIR_T_D),
                );

                // Convert it back to an int
                if (zig_type.Int.signedness == .signed) {
                    self.I2D(tmp_float, zig_value);
                } else {
                    self.UI2D(tmp_float, zig_value);
                }

                // And to a buzz value
                try self.buildValueFromDouble(tmp_float, dest);
            } else {
                self.buildValueFromInteger(zig_value, dest);
            }
        },
        .Double => {
            if (zig_type.Double.bits == 64) {
                try self.buildValueFromDouble(zig_value, dest);
            } else {
                std.debug.assert(zig_type.Double.bits == 32);
                try self.buildValueFromFloat(zig_value, dest);
            }
        },
        .Bool => self.buildValueFromBoolean(zig_value, dest),
        .Void => self.MOV(dest, m.MIR_new_uint_op(self.ctx, Value.Void.val)),
        .Union, .Struct => unreachable, // FIXME: should call an api function build a ObjForeignContainer
        .Pointer => {
            // Is it a [*:0]const u8
            if (zig_type.Pointer.child.* == .Int and
                zig_type.Pointer.child.Int.bits == 8 and
                zig_type.Pointer.child.Int.signedness == .unsigned)
            {
                try self.buildValueFromCString(zig_value, dest);
            } else if (zig_type.Pointer.child.* == .Struct) {
                try self.buildValueFromForeignContainerPtr(buzz_type, zig_value, dest);
            } else {
                try self.buildValueFromUserData(zig_value, dest);
            }
        },
        .Optional => {
            // Is it a [*:0]const u8
            if (zig_type.Optional.child.Pointer.child.* == .Int and
                zig_type.Optional.child.Pointer.child.Int.bits == 8 and
                zig_type.Optional.child.Pointer.child.Int.signedness == .unsigned)
            {
                try self.buildValueFromOptionalCString(zig_value, dest);
            } else if (zig_type.Optional.child.Pointer.child.* == .Struct) {
                try self.buildValueFromOptionalForeignContainerPtr(buzz_type, zig_value, dest);
            } else {
                try self.buildValueFromOptionalUserData(zig_value, dest);
            }
        },
        else => unreachable,
    }
}

pub fn compileZdef(self: *Self, buzz_ast: Ast.Slice, zdef: Ast.Zdef.ZdefElement) Error!*o.ObjNative {
    var wrapper_name = std.ArrayList(u8){};
    defer wrapper_name.deinit(self.vm.gc.allocator);

    try wrapper_name.writer(self.vm.gc.allocator).print("zdef_{s}\x00", .{zdef.zdef.name});

    const dupped_symbol = self.vm.gc.allocator.dupeZ(u8, zdef.zdef.name) catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(dupped_symbol);

    const module = m.MIR_new_module(self.ctx, @ptrCast(wrapper_name.items.ptr));
    defer m.MIR_finish_module(self.ctx);

    if (BuildOptions.jit_debug) {
        io.print(
            "Compiling zdef wrapper for `{s}` of type `{s}`\n",
            .{
                zdef.zdef.name,
                zdef.zdef.type_def.toStringAlloc(self.vm.gc.allocator) catch unreachable,
            },
        );
    }

    // FIXME: Not everything applies to a zdef, maybe split the two states?
    self.state = .{
        .ast = buzz_ast,
        .module = module,
        .prototypes = .{},
        .ast_node = undefined,
        .registers = .{},
        .closure = undefined,
        .breaks_label = .{},
    };
    defer self.reset(self.vm.gc.allocator);

    // Build wrapper
    const wrapper_item = try self.buildZdefWrapper(zdef);

    _ = m.MIR_new_export(self.ctx, @ptrCast(wrapper_name.items.ptr));

    if (BuildOptions.jit_debug) {
        self.outputModule(zdef.zdef.name, module);
    }

    m.MIR_load_module(self.ctx, module);

    // Load function we're wrapping
    m.MIR_load_external(self.ctx, dupped_symbol, zdef.fn_ptr.?);

    // Load external functions
    var it_ext = self.required_ext_api.iterator();
    while (it_ext.next()) |kv| {
        switch (kv.key_ptr.*) {
            // TODO: don't mix those with actual api functions
            .RawFn, .NativeFn => {},
            else => m.MIR_load_external(
                self.ctx,
                kv.key_ptr.*.name(),
                kv.key_ptr.*.ptr(),
            ),
        }
    }

    // Link everything together
    m.MIR_link(self.ctx, m.MIR_set_lazy_gen_interface, null);

    m.MIR_gen_init(self.ctx);
    defer m.MIR_gen_finish(self.ctx);

    return try self.vm.gc.allocateObject(
        o.ObjNative{
            .native = m.MIR_gen(self.ctx, wrapper_item) orelse unreachable,
        },
    );
}

fn zigToMIRType(zig_type: ZigType) m.MIR_type_t {
    return switch (zig_type) {
        .Int => if (zig_type.Int.signedness == .signed)
            switch (zig_type.Int.bits) {
                8 => m.MIR_T_I8,
                16 => m.MIR_T_I16,
                32 => m.MIR_T_I32,
                64 => m.MIR_T_I64,
                else => unreachable,
            }
        else switch (zig_type.Int.bits) {
            8 => m.MIR_T_U8,
            16 => m.MIR_T_U16,
            32 => m.MIR_T_U32,
            64 => m.MIR_T_U64,
            else => unreachable,
        },
        .Double => switch (zig_type.Double.bits) {
            32 => m.MIR_T_F,
            64 => m.MIR_T_D,
            else => unreachable,
        },
        .Bool => m.MIR_T_U8,
        // Optional only allowed on pointers so its either 0 or an actual ptr
        .Pointer,
        .Optional,
        => m.MIR_T_I64,
        // See https://github.com/vnmakarov/mir/issues/332 passing struct by values will need some work
        .Struct => unreachable, //m.MIR_T_BLK,
        else => unreachable,
    };
}

fn zigToMIRRegType(zig_type: ZigType) m.MIR_type_t {
    return switch (zig_type) {
        .Int, .Bool, .Pointer => m.MIR_T_I64,
        .Double => switch (zig_type.Double.bits) {
            32 => m.MIR_T_F,
            64 => m.MIR_T_D,
            else => unreachable,
        },
        // See https://github.com/vnmakarov/mir/issues/332 passing struct by values will need some work
        .Struct => unreachable, //m.MIR_T_BLK,
        // Optional are only allowed on pointers
        .Optional => m.MIR_T_I64,
        else => {
            io.print("{}\n", .{zig_type});
            unreachable;
        },
    };
}

fn buildZdefWrapper(self: *Self, zdef_element: Ast.Zdef.ZdefElement) Error!m.MIR_item_t {
    var wrapper_name = std.ArrayList(u8){};
    defer wrapper_name.deinit(self.vm.gc.allocator);

    try wrapper_name.writer(self.vm.gc.allocator).print("zdef_{s}\x00", .{zdef_element.zdef.name});

    var wrapper_protocol_name = std.ArrayList(u8){};
    defer wrapper_protocol_name.deinit(self.vm.gc.allocator);

    try wrapper_protocol_name.writer(self.vm.gc.allocator).print("p_zdef_{s}\x00", .{zdef_element.zdef.name});

    const dupped_symbol = self.vm.gc.allocator.dupeZ(u8, zdef_element.zdef.name) catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(dupped_symbol);

    // FIXME: I don't get why we need this: a simple constant becomes rubbish as soon as we enter MIR_new_func_arr if we don't
    const ctx_name = self.vm.gc.allocator.dupeZ(u8, "ctx") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(ctx_name);
    const function = m.MIR_new_func_arr(
        self.ctx,
        @ptrCast(wrapper_name.items.ptr),
        1,
        &[_]m.MIR_type_t{m.MIR_T_U64},
        1,
        &[_]m.MIR_var_t{
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast(ctx_name.ptr),
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

    const function_def = zdef_element.zdef.type_def.resolved_type.?.Function;
    const zig_function_def = zdef_element.zdef.zig_type;

    // Get arguments from stack
    var full_args = std.ArrayList(m.MIR_op_t){};
    defer full_args.deinit(self.vm.gc.allocator);

    var arg_types = std.ArrayList(m.MIR_var_t){};
    defer arg_types.deinit(self.vm.gc.allocator);

    for (zig_function_def.Fn.params) |param| {
        try arg_types.append(
            self.vm.gc.allocator,
            .{
                .type = zigToMIRType(
                    if (param.type) |param_type|
                        param_type.*
                    else
                        .{ .Void = {} },
                ),
                .name = "param",
                .size = undefined,
            },
        );
    }

    const zig_return_type = if (zig_function_def.Fn.return_type) |return_type|
        return_type.*
    else
        ZigType{ .Void = {} };

    const buzz_return_type = function_def.return_type;

    const return_mir_type = if (zig_return_type != .Void)
        zigToMIRRegType(zig_return_type)
    else
        null;

    const result = if (return_mir_type) |rmt|
        m.MIR_new_reg_op(
            self.ctx,
            try self.REG(
                "result",
                rmt,
            ),
        )
    else
        null;

    const result_value = m.MIR_new_reg_op(
        self.ctx,
        try self.REG(
            "result_value",
            m.MIR_T_I64,
        ),
    );

    // proto
    try full_args.append(
        self.vm.gc.allocator,
        m.MIR_new_ref_op(
            self.ctx,
            m.MIR_new_proto_arr(
                self.ctx,
                @ptrCast(wrapper_protocol_name.items.ptr),
                if (return_mir_type != null) 1 else 0,
                if (return_mir_type) |rmt| &[_]m.MIR_type_t{rmt} else null,
                arg_types.items.len,
                arg_types.items.ptr,
            ),
        ),
    );
    // import
    try full_args.append(
        self.vm.gc.allocator,
        m.MIR_new_ref_op(
            self.ctx,
            m.MIR_new_import(self.ctx, @ptrCast(dupped_symbol)),
        ),
    );
    if (result) |res| {
        try full_args.append(self.vm.gc.allocator, res);
    }
    // actual args
    var it = function_def.parameters.iterator();
    var idx = function_def.parameters.count();
    var zidx: usize = 0;
    while (it.next() != null) : ({
        idx -= 1;
        zidx += 1;
    }) {
        const param_value = m.MIR_new_reg_op(
            self.ctx,
            try self.REG("param_value", m.MIR_T_I64),
        );
        const param = m.MIR_new_reg_op(
            self.ctx,
            try self.REG("param", zigToMIRRegType(zig_function_def.Fn.params[zidx].type.?.*)),
        );

        try self.buildPeek(@intCast(idx - 1), param_value);

        try self.buildBuzzValueToZigValue(
            function_def.parameters.get(function_def.parameters.keys()[zidx]).?,
            zig_function_def.Fn.params[zidx].type.?.*,
            param_value,
            param,
        );

        try full_args.append(self.vm.gc.allocator, param);
    }

    // Make the call
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.CALL),
            full_args.items.len,
            full_args.items.ptr,
        ),
    );

    // Push result on stack
    if (result) |res| {
        try self.buildZigValueToBuzzValue(
            buzz_return_type,
            zig_return_type,
            res,
            result_value,
        );
        try self.buildPush(result_value);
    }

    // Return -1/0/1
    self.RET(
        m.MIR_new_int_op(
            self.ctx,
            if (function_def.return_type.def_type != .Void)
                1
            else
                0,
        ),
    );

    m.MIR_finish_func(self.ctx);

    return function;
}

fn buildZdefUnionGetter(
    self: *Self,
    union_name: []const u8,
    field_name: []const u8,
    buzz_type: *o.ObjTypeDef,
    zig_type: *const ZigType,
) Error!m.MIR_item_t {
    var getter_name = std.ArrayList(u8){};
    defer getter_name.deinit(self.vm.gc.allocator);

    try getter_name.writer(self.vm.gc.allocator).print(
        "zdef_union_{s}_{s}_getter\x00",
        .{
            union_name,
            field_name,
        },
    );

    // FIXME: I don't get why we need this: a simple constant becomes rubbish as soon as we enter MIR_new_func_arr if we don't
    const vm_name = self.vm.gc.allocator.dupeZ(u8, "vm") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(vm_name);
    const data_name = self.vm.gc.allocator.dupeZ(u8, "data") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(data_name);
    const function = m.MIR_new_func_arr(
        self.ctx,
        @ptrCast(getter_name.items.ptr),
        1,
        &[_]m.MIR_type_t{m.MIR_T_U64},
        2,
        &[_]m.MIR_var_t{
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast(vm_name.ptr),
                .size = undefined,
            },
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast(data_name.ptr),
                .size = undefined,
            },
        },
    );

    self.state.?.function = function;
    self.state.?.vm_reg = m.MIR_reg(self.ctx, "vm", function.u.func);

    const result_value = m.MIR_new_reg_op(
        self.ctx,
        try self.REG(
            "result_value",
            m.MIR_T_I64,
        ),
    );

    const data_reg = m.MIR_reg(self.ctx, "data", function.u.func);

    // Getting an union field is essentialy casting it the concrete buzz type
    switch (zig_type.*) {
        .Struct, .Union => {
            try self.buildExternApiCall(
                .bz_newForeignContainerFromSlice,
                result_value,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, self.state.?.vm_reg.?),
                    m.MIR_new_uint_op(self.ctx, @intFromPtr(buzz_type)),
                    m.MIR_new_reg_op(self.ctx, data_reg),
                    m.MIR_new_uint_op(self.ctx, zig_type.size()),
                },
            );
        },
        else => {
            const index = try self.REG("index", m.MIR_T_I64);
            self.MOV(
                m.MIR_new_reg_op(self.ctx, index),
                m.MIR_new_uint_op(self.ctx, 0),
            );

            const field_ptr = m.MIR_new_mem_op(
                self.ctx,
                zigToMIRType(zig_type.*),
                0,
                data_reg,
                index,
                0,
            );

            try self.buildZigValueToBuzzValue(
                buzz_type,
                zig_type.*,
                field_ptr,
                result_value,
            );
        },
    }

    self.RET(result_value);

    m.MIR_finish_func(self.ctx);

    _ = m.MIR_new_export(self.ctx, @ptrCast(getter_name.items.ptr));

    return function;
}

fn buildZdefUnionSetter(
    self: *Self,
    union_name: []const u8,
    field_name: []const u8,
    buzz_type: *o.ObjTypeDef,
    zig_type: *const ZigType,
) Error!m.MIR_item_t {
    var setter_name = std.ArrayList(u8){};
    defer setter_name.deinit(self.vm.gc.allocator);

    try setter_name.writer(self.vm.gc.allocator).print(
        "zdef_union_{s}_{s}_setter\x00",
        .{
            union_name,
            field_name,
        },
    );

    // FIXME: I don't get why we need this: a simple constant becomes rubbish as soon as we enter MIR_new_func_arr if we don't
    const vm_name = self.vm.gc.allocator.dupeZ(u8, "vm") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(vm_name);
    const data_name = self.vm.gc.allocator.dupeZ(u8, "data") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(data_name);
    const new_value_name = self.vm.gc.allocator.dupeZ(u8, "new_value") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(new_value_name);
    const function = m.MIR_new_func_arr(
        self.ctx,
        @ptrCast(setter_name.items.ptr),
        0,
        null,
        3,
        &[_]m.MIR_var_t{
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast(vm_name.ptr),
                .size = undefined,
            },
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast(data_name.ptr),
                .size = undefined,
            },
            .{
                .type = m.MIR_T_U64,
                .name = @ptrCast(new_value_name.ptr),
                .size = undefined,
            },
        },
    );

    self.state.?.function = function;
    self.state.?.vm_reg = m.MIR_reg(self.ctx, "vm", function.u.func);

    const data_reg = m.MIR_reg(self.ctx, "data", function.u.func);
    const new_value_reg = m.MIR_new_reg_op(self.ctx, m.MIR_reg(self.ctx, "new_value", function.u.func));
    switch (zig_type.*) {
        .Struct, .Union => {
            const ptr_reg = m.MIR_new_reg_op(self.ctx, try self.REG("ptr_reg", m.MIR_T_I64));
            try self.buildValueToForeignContainerPtr(
                new_value_reg,
                ptr_reg,
            );

            try self.buildExternApiCall(
                .memcpy,
                null,
                &[_]m.MIR_op_t{
                    m.MIR_new_reg_op(self.ctx, data_reg),
                    m.MIR_new_uint_op(self.ctx, zig_type.size()),
                    ptr_reg,
                    m.MIR_new_uint_op(self.ctx, zig_type.size()),
                },
            );
        },
        else => {
            const index = try self.REG("index", m.MIR_T_I64);
            self.MOV(
                m.MIR_new_reg_op(self.ctx, index),
                m.MIR_new_uint_op(self.ctx, 0),
            );

            const field_ptr = m.MIR_new_mem_op(
                self.ctx,
                zigToMIRType(zig_type.*),
                0,
                data_reg,
                index,
                0,
            );

            try self.buildBuzzValueToZigValue(
                buzz_type,
                zig_type.*,
                new_value_reg,
                field_ptr,
            );
        },
    }

    m.MIR_finish_func(self.ctx);

    _ = m.MIR_new_export(self.ctx, @ptrCast(setter_name.items.ptr));

    return function;
}

fn buildZdefContainerGetter(
    self: *Self,
    offset: usize,
    struct_name: []const u8,
    field_name: []const u8,
    buzz_type: *o.ObjTypeDef,
    zig_type: *const ZigType,
) Error!m.MIR_item_t {
    var getter_name = std.ArrayList(u8){};
    defer getter_name.deinit(self.vm.gc.allocator);

    try getter_name.writer(self.vm.gc.allocator).print(
        "zdef_struct_{s}_{s}_getter\x00",
        .{
            struct_name,
            field_name,
        },
    );

    // FIXME: I don't get why we need this: a simple constant becomes rubbish as soon as we enter MIR_new_func_arr if we don't
    const vm_name = self.vm.gc.allocator.dupeZ(u8, "vm") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(vm_name);
    const data_name = self.vm.gc.allocator.dupeZ(u8, "data") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(data_name);
    const function = m.MIR_new_func_arr(
        self.ctx,
        @ptrCast(getter_name.items.ptr),
        1,
        &[_]m.MIR_type_t{m.MIR_T_U64},
        2,
        &[_]m.MIR_var_t{
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast(vm_name.ptr),
                .size = undefined,
            },
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast(data_name.ptr),
                .size = undefined,
            },
        },
    );

    self.state.?.function = function;
    self.state.?.vm_reg = m.MIR_reg(self.ctx, "vm", function.u.func);

    const data_reg = m.MIR_reg(self.ctx, "data", function.u.func);

    const index = try self.REG("index", m.MIR_T_I64);
    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    const field_ptr = m.MIR_new_mem_op(
        self.ctx,
        zigToMIRType(zig_type.*),
        @intCast(offset),
        data_reg,
        index,
        0,
    );

    const result_value = m.MIR_new_reg_op(
        self.ctx,
        try self.REG(
            "result_value",
            m.MIR_T_I64,
        ),
    );

    try self.buildZigValueToBuzzValue(
        buzz_type,
        zig_type.*,
        field_ptr,
        result_value,
    );

    self.RET(result_value);

    m.MIR_finish_func(self.ctx);

    _ = m.MIR_new_export(self.ctx, @ptrCast(getter_name.items.ptr));

    return function;
}

fn buildZdefContainerSetter(
    self: *Self,
    offset: usize,
    struct_name: []const u8,
    field_name: []const u8,
    buzz_type: *o.ObjTypeDef,
    zig_type: *const ZigType,
) Error!m.MIR_item_t {
    var setter_name = std.ArrayList(u8){};
    defer setter_name.deinit(self.vm.gc.allocator);

    try setter_name.writer(self.vm.gc.allocator).print(
        "zdef_struct_{s}_{s}_setter\x00",
        .{
            struct_name,
            field_name,
        },
    );

    // FIXME: I don't get why we need this: a simple constant becomes rubbish as soon as we enter MIR_new_func_arr if we don't
    const vm_name = self.vm.gc.allocator.dupeZ(u8, "vm") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(vm_name);
    const data_name = self.vm.gc.allocator.dupeZ(u8, "data") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(data_name);
    const new_value_name = self.vm.gc.allocator.dupeZ(u8, "new_value") catch {
        self.vm.panic("Out of memory");
        unreachable;
    };
    defer self.vm.gc.allocator.free(new_value_name);
    const function = m.MIR_new_func_arr(
        self.ctx,
        @ptrCast(setter_name.items.ptr),
        0,
        null,
        3,
        &[_]m.MIR_var_t{
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast(vm_name.ptr),
                .size = undefined,
            },
            .{
                .type = m.MIR_T_P,
                .name = @ptrCast(data_name.ptr),
                .size = undefined,
            },
            .{
                .type = m.MIR_T_U64,
                .name = @ptrCast(new_value_name.ptr),
                .size = undefined,
            },
        },
    );

    self.state.?.function = function;
    self.state.?.vm_reg = m.MIR_reg(self.ctx, "vm", function.u.func);

    const data_reg = m.MIR_reg(self.ctx, "data", function.u.func);
    const new_value_reg = m.MIR_reg(self.ctx, "new_value", function.u.func);

    const index = try self.REG("index", m.MIR_T_I64);
    self.MOV(
        m.MIR_new_reg_op(self.ctx, index),
        m.MIR_new_uint_op(self.ctx, 0),
    );

    const field_ptr = m.MIR_new_mem_op(
        self.ctx,
        zigToMIRType(zig_type.*),
        @intCast(offset),
        data_reg,
        index,
        0,
    );

    try self.buildBuzzValueToZigValue(
        buzz_type,
        zig_type.*,
        m.MIR_new_reg_op(self.ctx, new_value_reg),
        field_ptr,
    );

    m.MIR_finish_func(self.ctx);

    _ = m.MIR_new_export(self.ctx, @ptrCast(setter_name.items.ptr));

    return function;
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
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.MOV),
            2,
            &[_]m.MIR_op_t{
                dest,
                value,
            },
        ),
    );
}

inline fn DMOV(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DMOV),
            2,
            &[_]m.MIR_op_t{
                dest,
                value,
            },
        ),
    );
}

inline fn FMOV(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.FMOV),
            2,
            &[_]m.MIR_op_t{
                dest,
                value,
            },
        ),
    );
}

inline fn EQ(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.EQ),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn EQS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.EQS),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn DEQ(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DEQ),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn GT(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.GT),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn GTS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.GTS),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn DGT(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DGT),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn LT(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.LT),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn LTS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.LTS),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn DLT(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DLT),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn GE(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.GE),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn GES(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.GES),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn DGE(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DGE),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn LE(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.LE),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn LES(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.LES),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn DLE(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DLE),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn NE(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.NE),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn NES(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.NES),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn DNE(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DNE),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn BEQ(self: *Self, label: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.BEQ),
            3,
            &[_]m.MIR_op_t{
                label,
                left,
                right,
            },
        ),
    );
}

inline fn BLT(self: *Self, label: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.BLT),
            3,
            &[_]m.MIR_op_t{
                label,
                left,
                right,
            },
        ),
    );
}

inline fn BGT(self: *Self, label: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.BGT),
            3,
            &[_]m.MIR_op_t{
                label,
                left,
                right,
            },
        ),
    );
}

inline fn BNE(self: *Self, label: m.MIR_insn_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.BNE),
            3,
            &[_]m.MIR_op_t{
                m.MIR_new_label_op(self.ctx, label),
                left,
                right,
            },
        ),
    );
}

inline fn JMP(self: *Self, label: m.MIR_insn_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.JMP),
            1,
            &[_]m.MIR_op_t{
                m.MIR_new_label_op(self.ctx, label),
            },
        ),
    );
}

inline fn ADD(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.ADD),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn DADD(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DADD),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn ADDS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.ADDS),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn SUB(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.SUB),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn SUBS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.SUBS),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn DSUB(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DSUB),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn MUL(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.MUL),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn MULS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.MULS),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn DMUL(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DMUL),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn DIV(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DIV),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn DIVS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DIVS),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn DDIV(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DDIV),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn MOD(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.MOD),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn MODS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.MODS),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn AND(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.AND),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn ANDS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.ANDS),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn OR(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.OR),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn ORS(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.ORS),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn XOR(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.XOR),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn SHL(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.LSH),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn SHR(self: *Self, dest: m.MIR_op_t, left: m.MIR_op_t, right: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.RSH),
            3,
            &[_]m.MIR_op_t{
                dest,
                left,
                right,
            },
        ),
    );
}

inline fn NOT(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.XOR),
            3,
            &[_]m.MIR_op_t{
                dest,
                value,
                m.MIR_new_uint_op(self.ctx, std.math.maxInt(u64)),
            },
        ),
    );
}

inline fn NOTS(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.XORS),
            3,
            &[_]m.MIR_op_t{
                dest,
                value,
                m.MIR_new_uint_op(self.ctx, std.math.maxInt(u64)),
            },
        ),
    );
}

inline fn I2D(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.I2D),
            2,
            &[_]m.MIR_op_t{
                dest,
                value,
            },
        ),
    );
}

inline fn UI2D(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.UI2D),
            2,
            &[_]m.MIR_op_t{
                dest,
                value,
            },
        ),
    );
}

inline fn D2I(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.D2I),
            2,
            &[_]m.MIR_op_t{
                dest,
                value,
            },
        ),
    );
}

inline fn NEG(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.NEG),
            2,
            &[_]m.MIR_op_t{
                dest,
                value,
            },
        ),
    );
}

inline fn DNEG(self: *Self, dest: m.MIR_op_t, value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.DNEG),
            2,
            &[_]m.MIR_op_t{
                dest,
                value,
            },
        ),
    );
}

inline fn RET(self: *Self, return_value: m.MIR_op_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.RET),
            1,
            &[_]m.MIR_op_t{
                return_value,
            },
        ),
    );
}

inline fn ALLOCA(self: *Self, reg: m.MIR_reg_t, size: usize) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.ALLOCA),
            2,
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, reg),
                m.MIR_new_uint_op(self.ctx, size),
            },
        ),
    );
}

inline fn BSTART(self: *Self, into: m.MIR_reg_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.BSTART),
            1,
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, into),
            },
        ),
    );
}

inline fn BEND(self: *Self, from: m.MIR_reg_t) void {
    self.append(
        m.MIR_new_insn_arr(
            self.ctx,
            @intFromEnum(m.MIR_Instruction.BEND),
            1,
            &[_]m.MIR_op_t{
                m.MIR_new_reg_op(self.ctx, from),
            },
        ),
    );
}

fn REG(self: *Self, name: [*:0]const u8, reg_type: m.MIR_type_t) !m.MIR_reg_t {
    var actual_name = std.ArrayList(u8){};
    defer actual_name.deinit(self.vm.gc.allocator);

    const count = self.state.?.registers.get(name) orelse 0;
    if (count > 0) {
        try actual_name.writer(self.vm.gc.allocator).print("{s}{d}\u{0}", .{ name, count + 1 });
    } else {
        try actual_name.writer(self.vm.gc.allocator).print("{s}\u{0}", .{name});
    }

    const reg = m.MIR_new_func_reg(
        self.ctx,
        self.state.?.function.?.u.func,
        reg_type,
        @ptrCast(actual_name.items.ptr),
    );

    try self.state.?.registers.put(
        self.vm.gc.allocator,
        name,
        count + 1,
    );

    return reg;
}

fn outputModule(self: *Self, name: []const u8, module: m.MIR_module_t) void {
    // Output MIR code to .mir file
    var debug_path = std.ArrayList(u8){};
    defer debug_path.deinit(self.vm.gc.allocator);

    debug_path.writer(self.vm.gc.allocator).print(
        "./dist/gen/{s}.mod.mir\x00",
        .{
            name,
        },
    ) catch unreachable;

    const debug_file = std.c.fopen(
        @ptrCast(debug_path.items.ptr),
        "w",
    ).?;
    defer _ = std.c.fclose(debug_file);

    m.MIR_output_module(
        self.ctx,
        debug_file,
        module,
    );
}

pub fn fmod(lhs: v.Double, rhs: v.Double) Value {
    return Value.fromDouble(@mod(lhs, rhs));
}

pub fn dumpInt(value: u64) void {
    io.print("\nvalue: {} {b}\n", .{ value, value });
}
