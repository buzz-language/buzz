const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const BuildOptions = @import("build_options");
const _node = @import("./node.zig");
const ParseNode = _node.ParseNode;
const FunctionNode = _node.FunctionNode;
const BooleanNode = _node.BooleanNode;
const FloatNode = _node.FloatNode;
const IntegerNode = _node.IntegerNode;
const StringNode = _node.StringNode;
const VarDeclarationNode = _node.VarDeclarationNode;
const StringLiteralNode = _node.StringLiteralNode;
const FunDeclarationNode = _node.FunDeclarationNode;
const ExpressionNode = _node.ExpressionNode;
const CallNode = _node.CallNode;
const DotNode = _node.DotNode;
const BlockNode = _node.BlockNode;
const NamedVariableNode = _node.NamedVariableNode;
const _obj = @import("./obj.zig");
const _value = @import("./value.zig");
const Value = _value.Value;
const Obj = _obj.Obj;
const ObjString = _obj.ObjString;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjList = _obj.ObjList;
const ObjFunction = _obj.ObjFunction;
const ObjNative = _obj.ObjNative;
const NativeFn = _obj.NativeFn;
const PlaceholderDef = _obj.PlaceholderDef;
const ObjClosure = _obj.ObjClosure;
const llvm = @import("./llvm.zig");
const Token = @import("./token.zig").Token;
const disassembler = @import("./disassembler.zig");
const disassembleChunk = disassembler.disassembleChunk;
const VM = @import("./vm.zig").VM;

const GenState = struct {
    module: *llvm.OrcThreadSafeModule,
    context: *llvm.OrcThreadSafeContext,
    builder: *llvm.Builder,

    pub fn deinit(self: GenState) void {
        self.builder.dispose();
        self.context.dispose();
        // self.module ownership is taken by LLJIT
    }
};

pub const Frame = struct {
    enclosing: ?*Frame = null,
    function_node: *FunctionNode,
    return_counts: bool = false,
    return_emitted: bool = false,

    try_should_handle: ?std.AutoHashMap(*ObjTypeDef, void) = null,

    function: ?*llvm.Value = null,
    block: ?*llvm.BasicBlock = null,
};

pub const BuzzApiMethods = enum {
    nativefn,
    nativectx,
    value,
    bz_push,
    bz_peek,
    bz_valueToRawNativeFn,
    globals,

    pub fn name(self: BuzzApiMethods) []const u8 {
        return switch (self) {
            .bz_push => "bz_push",
            .bz_peek => "bz_peek",

            .nativefn => "NativeFn",
            .nativectx => "NativeCtx",
            .value => "Value",
            .globals => "globals",
            .bz_valueToRawNativeFn => "bz_valueToRawNativeFn",
        };
    }
};

pub const JIT = struct {
    const Self = @This();

    vm: *VM,

    // Closure being jitted right now
    closure: ?*ObjClosure = null,

    current: ?*Frame = null,
    state: GenState = undefined,

    vm_constant: ?*llvm.Value = null,

    api_lowered_types: std.AutoHashMap(BuzzApiMethods, *llvm.Type),

    orc_jit: *llvm.OrcLLJIT,

    pub fn init(vm: *VM) JIT {
        llvm.initializeLLVMTarget(builtin.target.cpu.arch);
        var builder = llvm.OrcLLJITBuilder.createBuilder();

        // TODO: LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator ?

        // Initialize LLJIT
        var orc_jit: *llvm.OrcLLJIT = undefined;
        if (llvm.OrcLLJITBuilder.createOrcLLJIT(&orc_jit, builder)) |orc_error| {
            std.debug.print("\n{s}\n", .{orc_error.getErrorMessage()});

            // Return error instead of panicking
            @panic("Could not create OrcJIT");
        }

        // Register host program symbols into the LLJIT
        var process_definition_generator: *llvm.OrcDefinitionGenerator = undefined;
        if (llvm.OrcDefinitionGenerator.createDynamicLibrarySearchGeneratorForProcess(
            &process_definition_generator,
            '_', // FIXME: adjust depending on the object format type?
            null,
            null,
        )) |orc_error| {
            std.debug.print("\n{s}\n", .{orc_error.getErrorMessage()});

            // Return error instead of panicking
            @panic("Could not create dynamic library searcher generator");
        }

        var main_jit_dylib = orc_jit.getMainJITDylib();
        main_jit_dylib.addGenerator(process_definition_generator);

        var self = Self{
            .vm = vm,
            .api_lowered_types = std.AutoHashMap(BuzzApiMethods, *llvm.Type).init(vm.gc.allocator),
            .orc_jit = orc_jit,
            .state = undefined,
        };

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.api_lowered_types.deinit();
        self.state.deinit();
    }

    // TODO: lowered types register like we do for ObjTypeDef
    fn lowerType(self: *Self, obj_typedef: *ObjTypeDef) VM.Error!*llvm.Type {
        return switch (obj_typedef.def_type) {
            .Bool => self.state.context.getContext().intType(8),
            .Integer => self.state.context.getContext().intType(64),
            .Float => self.state.context.getContext().doubleType(),
            .Void => self.state.context.getContext().voidType(),

            // Pointer to Obj, so we can continue to use the same data from the VM and LLVM IR
            // usize that will be converted to a pointer
            .String,
            .Pattern,
            .ObjectInstance,
            .Object,
            .Enum,
            .EnumInstance,
            .List,
            .Map,
            .Type,
            .UserData,
            => self.state.context.getContext().intType(64),

            .Function => function: {
                const function_type = obj_typedef.resolved_type.?.Function;

                const return_type = try self.lowerType(function_type.return_type);
                // TODO yield_type
                var param_types = std.ArrayList(*llvm.Type).init(self.vm.gc.allocator);
                defer param_types.deinit();

                try param_types.append((try self.lowerBuzzApiType(.nativectx)).pointerType(0));

                var it = function_type.parameters.iterator();
                while (it.next()) |kv| {
                    try param_types.append(try self.lowerType(kv.value_ptr.*));
                }

                break :function llvm.functionType(
                    return_type,
                    param_types.items.ptr,
                    @intCast(c_uint, param_types.items.len),
                    .False,
                );
            },

            // No runtime representation
            .Protocol,
            .ProtocolInstance,
            .Generic,
            .Placeholder,
            => unreachable,

            // TODO
            .Fiber => unreachable,
        };
    }

    inline fn lowerBuzzApiType(self: *Self, method: BuzzApiMethods) !*llvm.Type {
        if (self.api_lowered_types.get(method)) |lowered| {
            return lowered;
        }

        const ptr_type = self.state.context.getContext().pointerType(0);

        const lowered = switch (method) {
            .bz_peek => llvm.functionType(
                ptr_type,
                &[_]*llvm.Type{ ptr_type, self.state.context.getContext().intType(32) },
                2,
                .False,
            ),
            .bz_push => llvm.functionType(
                self.state.context.getContext().voidType(),
                &[_]*llvm.Type{ ptr_type, try self.lowerBuzzApiType(.value) },
                2,
                .False,
            ),
            .bz_valueToRawNativeFn => llvm.functionType(
                ptr_type,
                &[_]*llvm.Type{self.state.context.getContext().intType(64)},
                1,
                .False,
            ),
            .nativefn => llvm.functionType(
                self.state.context.getContext().intType(8),
                &[_]*llvm.Type{
                    (try self.lowerBuzzApiType(.nativectx)).pointerType(0),
                },
                1,
                .False,
            ),
            .value => self.state.context.getContext().intType(64),
            .nativectx => self.state.context.getContext().structCreateNamed(
                "NativeCtx",
                &[_]*llvm.Type{
                    // vm
                    ptr_type,
                    // globals
                    try self.lowerBuzzApiType(.globals),
                    // upvalues
                    ptr_type.pointerType(0),
                },
                3,
                .False,
            ),
            .globals => (try self.lowerBuzzApiType(.value)).pointerType(0),
        };

        try self.api_lowered_types.put(
            method,
            lowered,
        );

        return lowered;
    }

    fn declareBuzzApi(self: *Self) !void {
        for ([_]BuzzApiMethods{
            .bz_peek,
            .bz_push,
            .bz_valueToRawNativeFn,
        }) |method| {
            _ = self.state.module.addFunction(
                @ptrCast([*:0]const u8, method.name()),
                try self.lowerBuzzApiType(method),
            );
        }
    }

    inline fn vmConstant(self: *Self) *llvm.Value {
        self.vm_constant = self.vm_constant orelse self.state.builder.buildIntToPtr(
            self.state.context.getContext().intType(64).constInt(
                @ptrToInt(self.vm),
                .False,
            ),
            self.state.context.getContext().pointerType(0),
            "",
        );

        return self.vm_constant.?;
    }

    pub fn jitNative(self: *Self, native: *ObjNative) VM.Error!*anyopaque {
        const name = self.vm.gc.allocator.dupeZ(u8, native.name);
        defer self.vm.gc.allocator.free(name);

        self.state.module.addFunction(
            name.ptr,
            try self.lowerBuzzApiType(.nativefn),
        );

        var error_message: [*:0]const u8 = undefined;
        // verifyModule always allocs the error_message even if there is no error
        defer llvm.disposeMessage(error_message);

        if (self.state.module.verify(.ReturnStatus, &error_message).toBool()) {
            std.debug.print("\n{s}\n", .{error_message});

            @panic("LLVM module verification failed");
        }

        var fun_addr: u64 = undefined;
        if (self.orc_jit.lookup(&fun_addr, name.ptr)) |orc_error| {
            std.debug.print("\n{s}\n", .{orc_error.getErrorMessage()});

            @panic("Could find script symbol in module loaded in LLJIT");
        }

        return @intToPtr(*anyopaque, fun_addr);
    }

    pub fn jitFunction(self: *Self, closure: *ObjClosure) VM.Error![2]*anyopaque {
        var thread_safe_context = llvm.OrcThreadSafeContext.create();
        var module = llvm.Module.createWithName("buzz-jit", thread_safe_context.getContext());
        var thread_safe_module = llvm.OrcThreadSafeModule.createNewThreadSafeModule(
            module,
            thread_safe_context,
        );

        self.state = .{
            .module = thread_safe_module,
            .context = thread_safe_context,
            .builder = thread_safe_context.getContext().createBuilder(),
        };

        // TODO: do it once in its own module?
        self.declareBuzzApi() catch @panic("Could not declare buzz api into LLVM module");

        self.closure = closure;
        const function = closure.function;

        const function_node = @ptrCast(*FunctionNode, @alignCast(@alignOf(FunctionNode), function.node));

        var qualified_name = try self.getFunctionQualifiedName(
            function_node,
            false,
        );
        defer qualified_name.deinit();

        var qualified_name_raw = try self.getFunctionQualifiedName(
            function_node,
            true,
        );
        defer qualified_name_raw.deinit();

        if (BuildOptions.debug) {
            var out = std.ArrayList(u8).init(self.vm.gc.allocator);
            defer out.deinit();

            try function_node.node.toJson(&function_node.node, &out.writer());

            std.io.getStdOut().writer().print("\n{s}", .{out.items}) catch unreachable;
        }

        if (BuildOptions.jit_debug) {
            std.debug.print("JITting function `{s}`\n", .{qualified_name.items});
        }

        _ = try self.generateNode(function_node.toNode());

        var error_message: [*:0]const u8 = undefined;
        // verifyModule always allocs the error_message even if there is no error
        defer llvm.disposeMessage(error_message);

        if (self.state.module.verify(.ReturnStatus, &error_message).toBool()) {
            std.debug.print("\n{s}\n", .{error_message});

            if (BuildOptions.jit_debug) {
                _ = self.state.module.printModuleToFile("./out.bc", &error_message);
            }

            @panic("LLVM module verification failed");
        }

        if (BuildOptions.jit_debug) {
            _ = self.state.module.printModuleToFile("./out.bc", &error_message);
        }

        // Add module to LLJIT
        if (self.orc_jit.addLLVMIRModule(
            self.orc_jit.getMainJITDylib(),
            thread_safe_module,
        )) |orc_error| {
            std.debug.print("\n{s}\n", .{orc_error.getErrorMessage()});

            @panic("Could add IR module to OrcJIT");
        }

        var fun_addr: u64 = undefined;
        var fun_addr_raw: u64 = undefined;

        if (self.orc_jit.lookup(&fun_addr, @ptrCast([*:0]const u8, qualified_name.items))) |orc_error| {
            std.debug.print("\n{s}\n", .{orc_error.getErrorMessage()});

            @panic("Could find script symbol in module loaded in LLJIT");
        }

        if (self.orc_jit.lookup(&fun_addr_raw, @ptrCast([*:0]const u8, qualified_name_raw.items))) |orc_error| {
            std.debug.print("\n{s}\n", .{orc_error.getErrorMessage()});

            @panic("Could find script symbol in module loaded in LLJIT");
        }

        self.closure = null;

        return [_]*anyopaque{
            @intToPtr(*anyopaque, fun_addr),
            @intToPtr(*anyopaque, fun_addr_raw),
        };
    }

    fn generateNode(self: *Self, node: *ParseNode) VM.Error!?*llvm.Value {
        const lowered_type = if (node.type_def) |type_def| try self.lowerType(type_def) else null;

        return switch (node.node_type) {
            .Boolean => lowered_type.?.constInt(
                if (BooleanNode.cast(node).?.constant) 1 else 0,
                .False,
            ),
            .Float => lowered_type.?.constReal(
                FloatNode.cast(node).?.float_constant,
            ),
            .Integer => lowered_type.?.constInt(
                @intCast(c_ulonglong, IntegerNode.cast(node).?.integer_constant),
                .True,
            ),
            .StringLiteral => self.state.context.getContext().intType(64).constInt(
                StringLiteralNode.cast(node).?.constant.toValue().val,
                .False,
            ),

            .String => string: {
                const elements = StringNode.cast(node).?.elements;

                // TODO: only supports lone string literal for now
                assert(elements.len == 1 and elements[0].node_type == .StringLiteral);

                break :string try self.generateNode(elements[0]);
            },

            .Expression => try self.generateNode(ExpressionNode.cast(node).?.expression),
            .Function => try self.generateFunctionNode(FunctionNode.cast(node).?),
            .FunDeclaration => try self.generateFunDeclaration(FunDeclarationNode.cast(node).?),
            .VarDeclaration => try self.generateVarDeclaration(VarDeclarationNode.cast(node).?),
            .Block => try self.generateBlock(BlockNode.cast(node).?),
            .Call => try self.generateCall(CallNode.cast(node).?),
            .NamedVariable => try self.generateNamedVariable(NamedVariableNode.cast(node).?),

            else => {
                std.debug.print("{} NYI\n", .{node.node_type});
                unreachable;
            },
        };
    }

    fn generateNamedVariable(self: *Self, named_variable_node: *NamedVariableNode) VM.Error!?*llvm.Value {
        const function_type: ?ObjFunction.FunctionType = if (named_variable_node.node.type_def.?.def_type == .Function) named_variable_node.node.type_def.?.resolved_type.?.Function.function_type else null;
        const is_constant_fn = function_type != null and function_type.? != .Extern and function_type.? != .Anonymous;

        const name = try self.vm.gc.allocator.dupeZ(u8, named_variable_node.identifier.lexeme);
        defer self.vm.gc.allocator.free(name);

        return switch (named_variable_node.slot_type) {
            .Global => global: {
                if (named_variable_node.value) |value| {
                    assert(!is_constant_fn);

                    break :global try self.buildSetGlobal(
                        named_variable_node.slot,
                        (try self.generateNode(value)).?,
                    );
                } else if (is_constant_fn) {
                    // Get the actual Value as it is right now (which is correct since a function doesn't change)
                    const closure = ObjClosure.cast(self.closure.?.globals.items[named_variable_node.slot].obj()).?;

                    const qualified_name = try self.getFunctionQualifiedName(
                        @ptrCast(*FunctionNode, @alignCast(@alignOf(FunctionNode), closure.function.node)),
                        true,
                    );
                    defer qualified_name.deinit();

                    // Does it need to be compiled?
                    if (closure.function.native == null) {
                        const function_node = @ptrCast(*FunctionNode, @alignCast(@alignOf(FunctionNode), closure.function.node));

                        // save current state
                        const previous_current = self.current;
                        const previous_closure = self.closure;

                        self.current = null;
                        self.closure = closure;

                        // Compile function
                        _ = try self.generateFunctionNode(function_node);

                        // restore state
                        self.current = previous_current;
                        self.closure = previous_closure;
                        if (self.current != null and self.current.?.block != null) {
                            self.state.builder.positionBuilderAtEnd(self.current.?.block.?);
                        }
                    }

                    break :global self.state.module.getNamedFunction(
                        @ptrCast(
                            [*:0]const u8,
                            qualified_name.items.ptr,
                        ),
                    );
                } else {
                    break :global try self.buildGetGlobal(named_variable_node.slot);
                }
            },
            .Local => unreachable,
            .UpValue => unreachable,
        };
    }

    fn generateCall(self: *Self, call_node: *CallNode) VM.Error!?*llvm.Value {
        // This is not a call but an Enum(value)
        if (call_node.callee.type_def.?.def_type == .Enum) {
            // TODO

            unreachable;
        }

        // Find out if call is invoke or regular call
        var invoked = false;
        var invoked_on: ?ObjTypeDef.Type = null;

        if (call_node.callee.node_type == .Dot) {
            const dot = DotNode.cast(call_node.callee).?;
            const field_accessed = dot.callee.type_def;

            invoked = field_accessed.?.def_type != .Object;
            invoked_on = field_accessed.?.def_type;
        }

        // TODO
        // if (!invoked and invoked_on == null) {
        var callee = (try self.generateNode(call_node.callee)).?;

        const callee_type = switch (call_node.callee.node_type) {
            .Dot => DotNode.cast(call_node.callee).?.member_type_def,
            else => call_node.callee.type_def,
        };

        const function_type_def = try callee_type.?.populateGenerics(
            callee_type.?.resolved_type.?.Function.id,
            call_node.resolved_generics,
            &self.vm.gc.type_registry,
            null,
        );
        const function_type = function_type_def.resolved_type.?.Function.function_type;

        // TODO: arguments reordering

        const error_types = function_type_def.resolved_type.?.Function.error_types;
        if (call_node.catch_default) |_| {
            if (error_types != null and error_types.?.len > 0) {
                // TODO: catch clause
            }
        }

        // This is an async call, create a fiber
        if (call_node.async_call) {
            // TODO
            unreachable;
        }

        if (invoked) {
            // TODO
            unreachable;
        }

        var arguments = std.ArrayList(*llvm.Value).init(self.vm.gc.allocator);
        defer arguments.deinit();

        // first arg is ctx
        try arguments.append(self.current.?.function.?.getParam(0));

        var it = call_node.arguments.iterator();
        while (it.next()) |kv| {
            try arguments.append((try self.generateNode(kv.value_ptr.*)).?);
        }

        if (function_type == .Anonymous) {
            // TODO: bz_call
            unreachable;
        }

        // If extern, extract pointer to its raw function
        if (function_type == .Extern) {
            var error_message: [*:0]const u8 = undefined;
            _ = self.state.module.printModuleToFile("./out.bc", &error_message);

            callee = self.state.builder.buildCall(
                try self.lowerBuzzApiType(.bz_valueToRawNativeFn),
                self.state.module.getNamedFunction("bz_valueToRawNativeFn").?,
                &[_]*llvm.Value{callee},
                1,
                "",
            );
        }

        return self.state.builder.buildCall(
            try self.lowerType(function_type_def),
            callee,
            @ptrCast([*]*llvm.Value, arguments.items.ptr),
            @intCast(c_uint, arguments.items.len),
            "",
        );
    }

    fn generateBlock(self: *Self, block_node: *BlockNode) VM.Error!?*llvm.Value {
        var block_name = std.ArrayList(u8).init(self.vm.gc.allocator);
        try block_name.appendSlice(self.current.?.function_node.node.type_def.?.resolved_type.?.Function.name.string);
        try block_name.appendSlice(".block");
        try block_name.append(0);
        defer block_name.deinit();

        const block = self.state.context.getContext().appendBasicBlock(
            self.current.?.function.?,
            @ptrCast([*:0]const u8, block_name.items),
        );

        self.state.builder.positionBuilderAtEnd(block);
        self.current.?.block = block;

        for (block_node.statements.items) |statement| {
            _ = try self.generateNode(statement);
        }

        return null;
    }

    fn generateFunDeclaration(self: *Self, fun_declaration_node: *FunDeclarationNode) VM.Error!?*llvm.Value {
        return try self.generateFunctionNode(fun_declaration_node.function);
    }

    fn generateVarDeclaration(self: *Self, var_declaration_node: *VarDeclarationNode) VM.Error!?*llvm.Value {
        _ = try self.lowerType(var_declaration_node.type_def);

        // We should only declare locals
        assert(var_declaration_node.slot_type == .Local);

        unreachable;
    }

    // FIXME: multiple function can be defined at the same depth, so increment an id
    fn getFunctionQualifiedName(self: *Self, function_node: *FunctionNode, raw: bool) !std.ArrayList(u8) {
        const function_def = function_node.node.type_def.?.resolved_type.?.Function;
        const function_type = function_def.function_type;
        const name = function_def.name.string;

        var qualified_name = std.ArrayList(u8).init(self.vm.gc.allocator);

        try qualified_name.appendSlice(name);

        // Main and script are not allowed to be jitted
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

    // We create 2 function at the LLVM level: one with the NativeFn signature that will be called by buzz code,
    // and one with a signature reflecting the buzz signature that will be called by JITted functions
    fn generateFunctionNode(self: *Self, function_node: *FunctionNode) VM.Error!?*llvm.Value {
        const node = &function_node.node;

        var enclosing = self.current;
        self.current = try self.vm.gc.allocator.create(Frame);
        self.current.?.* = Frame{
            .enclosing = enclosing,
            .function_node = function_node,
        };

        const function_def = function_node.node.type_def.?.resolved_type.?.Function;
        const function_type = function_def.function_type;

        const ret_type = try self.lowerType(node.type_def.?);

        // Get fully qualified name of function
        var qualified_name = try self.getFunctionQualifiedName(function_node, true);
        defer qualified_name.deinit();

        var function = if (function_type != .Extern)
            self.state.module.addFunction(
                @ptrCast([*:0]const u8, qualified_name.items),
                ret_type,
            )
        else // FIXME: could be a local
            self.state.module.addGlobal(
                ret_type.pointerType(0),
                @ptrCast([*:0]const u8, qualified_name.items),
            );

        if (function_type != .Extern) {
            self.current.?.function = function;

            // Can't have both arrow expression and body
            assert((function_node.arrow_expr != null and function_node.body == null) or (function_node.arrow_expr == null and function_node.body != null));

            if (function_node.arrow_expr) |arrow_expr| {
                var block = self.state.context.getContext().appendBasicBlock(function, @ptrCast([*:0]const u8, qualified_name.items));
                self.state.builder.positionBuilderAtEnd(block);
                self.current.?.block = block;

                const arrow_value = try self.generateNode(arrow_expr);

                _ = self.state.builder.buildRet(arrow_value.?);
                self.current.?.return_emitted = true;
            } else {
                _ = try self.generateNode(function_node.body.?.toNode());
            }

            // Jitting main or the script function is not allowed
            assert(function_type != .Script and function_type != .ScriptEntryPoint);

            if (self.current.?.function_node.node.type_def.?.resolved_type.?.Function.return_type.def_type == .Void and !self.current.?.return_emitted) {
                // TODO: detect if some branches of the function body miss a return statement
                _ = self.state.builder.buildRetVoid();
            }

            // TODO: upvalues? closures?

            // Add the NativeFn version of the function
            try self.generateNativeFn(
                function_node,
                function,
                ret_type,
            );
        }

        self.current = self.current.?.enclosing;
        if (self.current != null and self.current.?.block != null) {
            self.state.builder.positionBuilderAtEnd(self.current.?.block.?);
        }

        return switch (function_type) {
            .Extern => ext: {
                function.setInitializer(
                    self.state.builder.buildIntToPtr(
                        self.state.context.getContext().intType(64).constInt(@ptrToInt(function_node.native.?.native), .False),
                        ret_type.pointerType(0),
                        "",
                    ),
                );

                break :ext function;
            },
            // TODO: closure
            else => function,
        };
    }

    /// Build instructions to get global at given index
    fn buildGetGlobal(self: *Self, slot: usize) !*llvm.Value {
        // Get ptr on NativeCtx `globals` field
        const globals_ptr = self.state.builder.buildStructGEP(
            try self.lowerBuzzApiType(.nativectx),
            self.current.?.function.?.getParam(0),
            1,
            "globals_ptr",
        );

        // Load globals ptr
        const globals = self.state.builder.buildLoad(
            (try self.lowerBuzzApiType(.globals)).pointerType(0),
            globals_ptr,
            "globals",
        );

        // Get element ptr at `slot`
        const value_ptr = self.state.builder.buildInBoundsGEP(
            try self.lowerBuzzApiType(.value),
            globals,
            &[_]*llvm.Value{
                self.state.context.getContext().intType(64).constInt(slot, .False),
            },
            1,
            "value_ptr",
        );

        // Load value
        return self.state.builder.buildLoad(
            try self.lowerBuzzApiType(.value),
            value_ptr,
            "value",
        );
    }

    /// Build instructions to set global at given index
    fn buildSetGlobal(self: *Self, slot: usize, value: *llvm.Value) !*llvm.Value {
        // Get ptr on NativeCtx `globals` field
        const globals_ptr = self.state.builder.buildStructGEP(
            try self.lowerBuzzApiType(.nativectx),
            self.current.?.function.?.getParam(0),
            1,
            "globals_ptr",
        );

        // Load globals ptr
        const globals = self.state.builder.buildLoad(
            (try self.lowerBuzzApiType(.globals)).pointerType(0),
            globals_ptr,
            "globals",
        );

        // Get element ptr at `slot`
        const value_ptr = self.state.builder.buildInBoundsGEP(
            try self.lowerBuzzApiType(.value),
            globals,
            &[_]*llvm.Value{
                self.state.context.getContext().intType(64).constInt(slot, .False),
            },
            1,
            "value_ptr",
        );

        // Store value
        return self.state.builder.buildStore(
            value,
            value_ptr,
        );
    }

    fn generateNativeFn(self: *Self, function_node: *FunctionNode, raw_fn: *llvm.Value, ret_type: *llvm.Type) !void {
        const function_def = function_node.node.type_def.?.resolved_type.?.Function;
        const function_type = function_def.function_type;

        assert(function_type != .Extern);

        var nativefn_qualified_name = try self.getFunctionQualifiedName(function_node, false);
        defer nativefn_qualified_name.deinit();

        var native_fn = self.state.module.addFunction(
            @ptrCast([*:0]const u8, nativefn_qualified_name.items),
            try self.lowerBuzzApiType(.nativefn),
        );

        // That version of the function takes argument from the stack and pushes the result of the raw version on the stack
        var block = self.state.context.getContext().appendBasicBlock(native_fn, @ptrCast([*:0]const u8, nativefn_qualified_name.items));
        self.state.builder.positionBuilderAtEnd(block);

        var arguments = std.ArrayList(*llvm.Value).init(self.vm.gc.allocator);
        defer arguments.deinit();
        const arg_count = function_def.parameters.count();

        // first arg is ctx
        try arguments.append(native_fn.getParam(0));

        if (arg_count > 0) {
            var i: i32 = @intCast(i32, arg_count - 1);
            // Each argument is a bz_peek(i) call
            while (i >= 0) : (i -= 1) {
                try arguments.append(
                    self.state.builder.buildCall(
                        try self.lowerBuzzApiType(.bz_peek),
                        self.state.module.getNamedFunction("bz_peek").?,
                        &[_]*llvm.Value{
                            self.vmConstant(),
                            self.state.context.getContext().intType(32).constInt(@intCast(c_ulonglong, i), .False),
                        },
                        2,
                        "",
                    ),
                );
            }
        }

        // Call the raw function
        const result = self.state.builder.buildCall(
            ret_type,
            raw_fn,
            @ptrCast([*]*llvm.Value, arguments.items.ptr),
            @intCast(c_uint, arguments.items.len),
            "",
        );

        const should_return = function_def.return_type.def_type != .Void;

        // Push its result back into the VM
        if (should_return) {
            _ = self.state.builder.buildCall(
                try self.lowerBuzzApiType(.bz_push),
                self.state.module.getNamedFunction("bz_push").?,
                &[_]*llvm.Value{
                    self.vmConstant(),
                    result,
                },
                2,
                "",
            );
        }

        // 1 = there's a return, 0 = no return, -1 = error
        // TODO: error ?
        _ = self.state.builder.buildRet(
            self.state.context.getContext().intType(8).constInt(
                if (should_return) 1 else 0,
                .True,
            ),
        );
    }
};
