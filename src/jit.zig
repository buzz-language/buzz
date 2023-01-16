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
const ReturnNode = _node.ReturnNode;
const IfNode = _node.IfNode;
const BinaryNode = _node.BinaryNode;
const WhileNode = _node.WhileNode;
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
const l = @import("./llvm.zig");
const Token = @import("./token.zig").Token;
const disassembler = @import("./disassembler.zig");
const disassembleChunk = disassembler.disassembleChunk;
const VM = @import("./vm.zig").VM;

const GenState = struct {
    module: *l.OrcThreadSafeModule,
    builder: *l.Builder,

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

    function: ?*l.Value = null,
    block: ?*l.BasicBlock = null,

    locals: std.ArrayList(*l.Value),
};

pub const BuzzApiMethods = enum {
    nativefn,
    nativectx,
    value,
    bz_push,
    bz_peek,
    bz_valueToRawNativeFn,
    bz_objStringConcat,
    bz_toString,
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
            .bz_objStringConcat => "bz_objStringConcat",
            .bz_toString => "bz_toString",
        };
    }

    pub fn namez(self: BuzzApiMethods) [*:0]const u8 {
        return switch (self) {
            .bz_push => "bz_push",
            .bz_peek => "bz_peek",

            .nativefn => "NativeFn",
            .nativectx => "NativeCtx",
            .value => "Value",
            .globals => "globals",
            .bz_valueToRawNativeFn => "bz_valueToRawNativeFn",
            .bz_objStringConcat => "bz_objStringConcat",
            .bz_toString => "bz_toString",
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

    vm_constant: ?*l.Value = null,

    api_lowered_types: std.AutoHashMap(BuzzApiMethods, *l.Type),
    lowered_types: std.AutoHashMap(*ObjTypeDef, *l.Type),

    orc_jit: *l.OrcLLJIT,
    context: *l.OrcThreadSafeContext,

    // Thresholds data

    // Call call of all functions
    call_count: u128 = 0,

    pub fn init(vm: *VM) JIT {
        l.initializeLLVMTarget(builtin.target.cpu.arch);
        var builder = l.OrcLLJITBuilder.createBuilder();

        // TODO: LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator ?

        // Initialize LLJIT
        var orc_jit: *l.OrcLLJIT = undefined;
        if (l.OrcLLJITBuilder.createOrcLLJIT(&orc_jit, builder)) |orc_error| {
            std.debug.print("\n{s}\n", .{orc_error.getErrorMessage()});

            // Return error instead of panicking
            @panic("Could not create OrcJIT");
        }

        // Register host program symbols into the LLJIT
        var process_definition_generator: *l.OrcDefinitionGenerator = undefined;
        if (l.OrcDefinitionGenerator.createDynamicLibrarySearchGeneratorForProcess(
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
            .api_lowered_types = std.AutoHashMap(BuzzApiMethods, *l.Type).init(vm.gc.allocator),
            .lowered_types = std.AutoHashMap(*ObjTypeDef, *l.Type).init(vm.gc.allocator),
            .orc_jit = orc_jit,
            .context = l.OrcThreadSafeContext.create(),
            .state = undefined,
        };

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.api_lowered_types.deinit();
        self.lowered_types.deinit();
        self.state.deinit();
    }

    fn lowerType(self: *Self, obj_typedef: *ObjTypeDef) VM.Error!*l.Type {
        var lowered = self.lowered_types.get(obj_typedef);

        if (lowered) |ulowered| {
            return ulowered;
        }

        lowered = switch (obj_typedef.def_type) {
            .Bool,
            .Integer,
            .Float,
            .Void,
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
            => self.context.getContext().intType(64),

            .Function => function: {
                const function_type = obj_typedef.resolved_type.?.Function;

                // TODO yield_type
                var param_types = std.ArrayList(*l.Type).init(self.vm.gc.allocator);
                defer param_types.deinit();

                try param_types.append((try self.lowerBuzzApiType(.nativectx)).pointerType(0));

                var it = function_type.parameters.iterator();
                while (it.next()) |kv| {
                    try param_types.append(try self.lowerType(kv.value_ptr.*));
                }

                break :function l.functionType(
                    try self.lowerBuzzApiType(.value),
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

        try self.lowered_types.put(obj_typedef, lowered.?);

        return lowered.?;
    }

    inline fn lowerBuzzApiType(self: *Self, method: BuzzApiMethods) !*l.Type {
        if (self.api_lowered_types.get(method)) |lowered| {
            return lowered;
        }

        const ptr_type = self.context.getContext().pointerType(0);

        const lowered = switch (method) {
            .bz_peek => l.functionType(
                try self.lowerBuzzApiType(.value),
                &[_]*l.Type{ ptr_type, self.context.getContext().intType(32) },
                2,
                .False,
            ),
            .bz_push => l.functionType(
                self.context.getContext().voidType(),
                &[_]*l.Type{ ptr_type, try self.lowerBuzzApiType(.value) },
                2,
                .False,
            ),
            .bz_valueToRawNativeFn => l.functionType(
                ptr_type,
                &[_]*l.Type{self.context.getContext().intType(64)},
                1,
                .False,
            ),
            .bz_objStringConcat => l.functionType(
                try self.lowerBuzzApiType(.value),
                &[_]*l.Type{
                    ptr_type,
                    try self.lowerBuzzApiType(.value),
                    try self.lowerBuzzApiType(.value),
                },
                3,
                .False,
            ),
            .bz_toString => l.functionType(
                try self.lowerBuzzApiType(.value),
                &[_]*l.Type{ ptr_type, try self.lowerBuzzApiType(.value) },
                2,
                .False,
            ),
            .nativefn => l.functionType(
                self.context.getContext().intType(8),
                &[_]*l.Type{
                    (try self.lowerBuzzApiType(.nativectx)).pointerType(0),
                },
                1,
                .False,
            ),
            .value => self.context.getContext().intType(64),
            .nativectx => self.context.getContext().structCreateNamed(
                "NativeCtx",
                &[_]*l.Type{
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
            .bz_objStringConcat,
            .bz_toString,
        }) |method| {
            _ = self.state.module.addFunction(
                @ptrCast([*:0]const u8, method.name()),
                try self.lowerBuzzApiType(method),
            );
        }
    }

    fn buildBuzzApiCall(self: *Self, method: BuzzApiMethods, args: []*l.Value) !*l.Value {
        return self.state.builder.buildCall(
            try self.lowerBuzzApiType(method),
            self.state.module.getNamedFunction(method.namez()).?,
            args.ptr,
            @intCast(c_uint, args.len),
            "",
        );
    }

    inline fn vmConstant(self: *Self) *l.Value {
        self.vm_constant = self.vm_constant orelse self.state.builder.buildIntToPtr(
            self.context.getContext().intType(64).constInt(
                @ptrToInt(self.vm),
                .False,
            ),
            self.context.getContext().pointerType(0),
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
        defer l.disposeMessage(error_message);

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

    pub fn shouldlJitFunction(self: *Self, closure: *ObjClosure) bool {
        const function_type = closure.function.type_def.resolved_type.?.Function.function_type;

        if (function_type == .Extern or function_type == .Script or function_type == .ScriptEntryPoint or function_type == .Anonymous or function_type == .EntryPoint) {
            return false;
        }

        return if (BuildOptions.jit_debug)
            return true
        else
            (closure.function.call_count / self.call_count) == BuildOptions.jit_prof_threshold;
    }

    pub fn jitFunction(self: *Self, closure: *ObjClosure) VM.Error![2]*anyopaque {
        var module = l.Module.createWithName("buzz-jit", self.context.getContext());
        var thread_safe_module = l.OrcThreadSafeModule.createNewThreadSafeModule(
            module,
            self.context,
        );

        self.state = .{
            .module = thread_safe_module,
            .builder = self.context.getContext().createBuilder(),
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
        defer l.disposeMessage(error_message);

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

    fn generateNode(self: *Self, node: *ParseNode) VM.Error!?*l.Value {
        const lowered_type = if (node.type_def) |type_def| try self.lowerType(type_def) else null;

        return switch (node.node_type) {
            .Boolean => lowered_type.?.constInt(
                Value.fromBoolean(BooleanNode.cast(node).?.constant).val,
                .False,
            ),
            .Float => lowered_type.?.constInt(
                Value.fromFloat(FloatNode.cast(node).?.float_constant).val,
                .False,
            ),
            .Integer => lowered_type.?.constInt(
                @intCast(c_ulonglong, Value.fromInteger(IntegerNode.cast(node).?.integer_constant).val),
                .False,
            ),
            .StringLiteral => self.context.getContext().intType(64).constInt(
                StringLiteralNode.cast(node).?.constant.toValue().val,
                .False,
            ),

            .String => try self.generateString(StringNode.cast(node).?),
            .Expression, .Grouping => try self.generateNode(ExpressionNode.cast(node).?.expression),
            .Function => try self.generateFunction(FunctionNode.cast(node).?),
            .FunDeclaration => try self.generateFunDeclaration(FunDeclarationNode.cast(node).?),
            .VarDeclaration => try self.generateVarDeclaration(VarDeclarationNode.cast(node).?),
            .Block => try self.generateBlock(BlockNode.cast(node).?),
            .Call => try self.generateCall(CallNode.cast(node).?),
            .NamedVariable => try self.generateNamedVariable(NamedVariableNode.cast(node).?),
            .Return => try self.generateReturn(ReturnNode.cast(node).?),
            .If => try self.generateIf(IfNode.cast(node).?),
            .Binary => try self.generateBinary(BinaryNode.cast(node).?),
            .While => try self.generateWhile(WhileNode.cast(node).?),

            else => {
                std.debug.print("{} NYI\n", .{node.node_type});
                unreachable;
            },
        };
    }

    inline fn readConstant(self: *Self, arg: u24) Value {
        return self.closure.?.function.chunk.constants.items[arg];
    }

    fn generateString(self: *Self, string_node: *StringNode) VM.Error!?*l.Value {
        if (string_node.elements.len == 0) {
            return (try self.lowerBuzzApiType(.value)).constInt(
                self.readConstant(0).val,
                .False,
            ); // Constant 0 is the empty string
        }

        var previous: ?*l.Value = null;
        for (string_node.elements) |element, index| {
            var value = (try self.generateNode(element)).?;

            if (element.type_def.?.def_type != .String or element.type_def.?.optional) {
                value = try self.buildBuzzApiCall(
                    .bz_toString,
                    &[_]*l.Value{
                        self.vmConstant(),
                        value,
                    },
                );
            }

            if (index >= 1) {
                value = try self.buildBuzzApiCall(
                    .bz_objStringConcat,
                    &[_]*l.Value{
                        self.vmConstant(),
                        previous.?,
                        value,
                    },
                );
            }

            previous = value;
        }

        return previous.?;
    }

    fn generateNamedVariable(self: *Self, named_variable_node: *NamedVariableNode) VM.Error!?*l.Value {
        const function_type: ?ObjFunction.FunctionType = if (named_variable_node.node.type_def.?.def_type == .Function)
            named_variable_node.node.type_def.?.resolved_type.?.Function.function_type
        else
            null;
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
                        _ = try self.generateFunction(function_node);

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
            .Local => local: {
                if (named_variable_node.value) |value| {
                    break :local try self.buildSetLocal(
                        named_variable_node.slot,
                        (try self.generateNode(value)).?,
                    );
                }

                break :local try self.buildGetLocal(named_variable_node.slot);
            },
            .UpValue => unreachable,
        };
    }

    fn generateCall(self: *Self, call_node: *CallNode) VM.Error!?*l.Value {
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

        var arguments = std.ArrayList(*l.Value).init(self.vm.gc.allocator);
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

            callee = try self.buildBuzzApiCall(
                .bz_valueToRawNativeFn,
                &[_]*l.Value{callee},
            );
        }

        return self.state.builder.buildCall(
            try self.lowerType(function_type_def),
            callee,
            @ptrCast([*]*l.Value, arguments.items.ptr),
            @intCast(c_uint, arguments.items.len),
            "",
        );
    }

    fn generateReturn(self: *Self, return_node: *ReturnNode) VM.Error!?*l.Value {
        if (return_node.unconditional) {
            self.current.?.return_emitted = true;
        }

        return self.state.builder.buildRet(
            if (return_node.value) |value|
                (try self.generateNode(value)).?
            else
                (try self.lowerBuzzApiType(.value)).constInt(Value.Void.val, .False),
        );
    }

    fn generateIf(self: *Self, if_node: *IfNode) VM.Error!?*l.Value {
        // Generate condition
        const condition_value = (try self.generateNode(if_node.condition)).?;

        // Get boolean from buzz value
        const condition = self.unwrap(.Bool, condition_value);

        if (if_node.unwrapped_identifier != null) {
            unreachable;
        } else if (if_node.casted_type != null) {
            unreachable;
        }

        // Continuation block
        var out_block = self.context.getContext().createBasicBlock("out");

        // If block
        var then_block = self.context.getContext().createBasicBlock("then");

        // Else block
        var else_block = if (if_node.else_branch != null)
            self.context.getContext().createBasicBlock("else")
        else
            null;

        _ = self.state.builder.buildCondBr(
            condition,
            then_block,
            if (if_node.else_branch != null) else_block.? else out_block,
        );

        self.current.?.function.?.appendExistingBasicBlock(then_block);
        self.state.builder.positionBuilderAtEnd(then_block);
        self.current.?.block = then_block;

        _ = try self.generateNode(if_node.body);

        // Jump after else
        _ = self.state.builder.buildBr(out_block);

        if (if_node.else_branch) |else_branch| {
            self.current.?.function.?.appendExistingBasicBlock(else_block.?);
            self.state.builder.positionBuilderAtEnd(else_block.?);
            self.current.?.block = else_block.?;

            _ = try self.generateNode(else_branch);

            // Jump after else
            _ = self.state.builder.buildBr(out_block);
        }

        self.current.?.function.?.appendExistingBasicBlock(out_block);

        // Continue writing after the if else
        self.current.?.block = out_block;
        self.state.builder.positionBuilderAtEnd(out_block);

        // Statement don't return values
        return null;
    }

    fn generateBinary(self: *Self, binary_node: *BinaryNode) VM.Error!?*l.Value {
        const left_type_def = binary_node.left.type_def.?.def_type;
        const right_type_def = binary_node.right.type_def.?.def_type;

        return switch (binary_node.operator) {
            .Ampersand => self.wrap(
                .Integer,
                self.state.builder.buildAnd(
                    (try self.generateNode(binary_node.left)).?,
                    (try self.generateNode(binary_node.right)).?,
                    "",
                ),
            ),
            .Bor => self.wrap(
                .Integer,
                self.state.builder.buildOr(
                    (try self.generateNode(binary_node.left)).?,
                    (try self.generateNode(binary_node.right)).?,
                    "",
                ),
            ),
            .Xor => self.wrap(
                .Integer,
                self.state.builder.buildXor(
                    (try self.generateNode(binary_node.left)).?,
                    (try self.generateNode(binary_node.right)).?,
                    "",
                ),
            ),
            .ShiftLeft => self.wrap(
                .Integer,
                self.state.builder.buildShl(
                    (try self.generateNode(binary_node.left)).?,
                    (try self.generateNode(binary_node.right)).?,
                    "",
                ),
            ),
            // ashr (https://llvm.org/docs/LangRef.html#ashr-instruction) ?
            .ShiftRight => self.wrap(
                .Integer,
                self.state.builder.buildLShr(
                    (try self.generateNode(binary_node.left)).?,
                    (try self.generateNode(binary_node.right)).?,
                    "",
                ),
            ),
            .QuestionQuestion, .And, .Or => cond: {
                const value = self.state.builder.buildAlloca(try self.lowerBuzzApiType(.value), "");
                const left = (try self.generateNode(binary_node.left)).?;

                _ = self.state.builder.buildStore(
                    left,
                    value,
                );

                const condition = self.state.builder.buildICmp(
                    .EQ,
                    left,
                    (try self.lowerBuzzApiType(.value)).constInt(
                        switch (binary_node.operator) {
                            .QuestionQuestion => Value.Null.val,
                            .And => Value.True.val,
                            .Or => Value.False.val,
                            else => unreachable,
                        },
                        .False,
                    ),
                    "cond",
                );

                // Continuation block
                const out_block = self.context.getContext().createBasicBlock("out");

                // If block
                const then_block = self.context.getContext().createBasicBlock("then");

                _ = self.state.builder.buildCondBr(
                    condition,
                    then_block,
                    out_block,
                );

                self.current.?.function.?.appendExistingBasicBlock(then_block);
                self.state.builder.positionBuilderAtEnd(then_block);
                self.current.?.block = then_block;

                _ = self.state.builder.buildStore(
                    (try self.generateNode(binary_node.right)).?,
                    value,
                );

                // Continue
                _ = self.state.builder.buildBr(out_block);

                // Continue writing after the if else
                self.current.?.function.?.appendExistingBasicBlock(out_block);
                self.current.?.block = out_block;
                self.state.builder.positionBuilderAtEnd(out_block);

                break :cond self.state.builder.buildLoad(
                    try self.lowerBuzzApiType(.value),
                    value,
                    "",
                );
            },
            else => bin: { // Operators where we can generate both operands everytime
                const left = try self.generateNode(binary_node.left);
                const right = try self.generateNode(binary_node.right);

                const left_i = if (left_type_def == .Integer) self.unwrap(.Integer, left.?) else null;
                const left_f = if (left_type_def == .Float) self.unwrap(.Float, left.?) else null;
                const right_i = if (right_type_def == .Integer) self.unwrap(.Integer, right.?) else null;
                const right_f = if (right_type_def == .Float) self.unwrap(.Float, right.?) else null;
                const left_s = if (left_type_def == .String) self.unwrap(.String, left.?) else null;
                const right_s = if (right_type_def == .String) self.unwrap(.String, right.?) else null;

                switch (binary_node.operator) {
                    .EqualEqual, .Greater, .Less, .GreaterEqual, .LessEqual, .BangEqual => {
                        if (left_s != null) {
                            return self.wrap(
                                .Bool,
                                self.state.builder.buildICmp(
                                    .EQ,
                                    left_s.?,
                                    right_s.?,
                                    "",
                                ),
                            );
                        }

                        if (left_f != null or right_f != null) {
                            return self.wrap(
                                .Bool,
                                self.state.builder.buildFCmp(
                                    switch (binary_node.operator) {
                                        .Greater => .OGT,
                                        .Less => .OLT,
                                        .GreaterEqual => .OGE,
                                        .LessEqual => .OLE,
                                        .BangEqual => .ONE,
                                        .EqualEqual => .OEQ,
                                        else => unreachable,
                                    },
                                    if (left_i) |i| self.state.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else left_f.?,
                                    if (right_i) |i| self.state.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else right_f.?,
                                    "",
                                ),
                            );
                        }

                        break :bin self.wrap(
                            .Bool,
                            self.state.builder.buildICmp(
                                switch (binary_node.operator) {
                                    .Greater => .SGT,
                                    .Less => .SLT,
                                    .GreaterEqual => .SGE,
                                    .LessEqual => .SLE,
                                    .BangEqual => .NE,
                                    .EqualEqual => .EQ,
                                    else => unreachable,
                                },
                                left_i.?,
                                right_i.?,
                                "",
                            ),
                        );
                    },
                    .Plus => {
                        switch (binary_node.left.type_def.?.def_type) {
                            .Integer, .Float => {
                                if (left_f != null or right_f != null) {
                                    break :bin self.wrap(
                                        .Float,
                                        self.state.builder.buildFAdd(
                                            if (left_i) |i| self.state.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else left_f.?,
                                            if (right_i) |i| self.state.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else right_f.?,
                                            "",
                                        ),
                                    );
                                }

                                break :bin self.wrap(
                                    .Integer,
                                    self.state.builder.buildAdd(
                                        left_i.?,
                                        right_i.?,
                                        "",
                                    ),
                                );
                            },
                            .String => break :bin try self.buildBuzzApiCall(
                                .bz_objStringConcat,
                                &[_]*l.Value{
                                    self.vmConstant(),
                                    self.wrap(.String, left_s.?),
                                    self.wrap(.String, right_s.?),
                                },
                            ),
                            .List => unreachable,
                            .Map => unreachable,
                            else => unreachable,
                        }
                    },
                    .Minus => {
                        if (left_f != null or right_f != null) {
                            break :bin self.wrap(
                                .Float,
                                self.state.builder.buildFSub(
                                    if (left_i) |i| self.state.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else left_f.?,
                                    if (right_i) |i| self.state.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else right_f.?,
                                    "",
                                ),
                            );
                        }

                        break :bin self.buildValueFromInteger(
                            self.state.builder.buildSub(
                                left_i orelse left_f.?,
                                right_i orelse right_f.?,
                                "",
                            ),
                        );
                    },
                    .Star => {
                        if (left_f != null or right_f != null) {
                            break :bin self.wrap(
                                .Float,
                                self.state.builder.buildFMul(
                                    if (left_i) |i| self.state.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else left_f.?,
                                    if (right_i) |i| self.state.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else right_f.?,
                                    "",
                                ),
                            );
                        }

                        break :bin self.wrap(
                            .Integer,
                            self.state.builder.buildMul(
                                left_i orelse left_f.?,
                                right_i orelse right_f.?,
                                "",
                            ),
                        );
                    },
                    .Slash => {
                        if (left_f != null or right_f != null) {
                            break :bin self.wrap(
                                .Float,
                                self.state.builder.buildFDiv(
                                    if (left_i) |i| self.state.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else left_f.?,
                                    if (right_i) |i| self.state.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else right_f.?,
                                    "",
                                ),
                            );
                        }

                        // Div result is always float
                        break :bin self.state.builder.buildSDiv(
                            left_i orelse left_f.?,
                            right_i orelse right_f.?,
                            "",
                        );
                    },
                    .Percent => {
                        if (left_f != null or right_f != null) {
                            break :bin self.wrap(
                                .Float,
                                self.state.builder.buildFRem(
                                    if (left_i) |i| self.state.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else left_f.?,
                                    if (right_i) |i| self.state.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else right_f.?,
                                    "",
                                ),
                            );
                        }

                        break :bin self.wrap(
                            .Integer,
                            self.state.builder.buildSRem(
                                left_i orelse left_f.?,
                                right_i orelse right_f.?,
                                "",
                            ),
                        );
                    },
                    else => unreachable,
                }
            },
        };
    }

    fn generateWhile(self: *Self, while_node: *WhileNode) VM.Error!?*l.Value {
        const cond_block = self.context.getContext().createBasicBlock("cond");
        const loop_block = self.context.getContext().createBasicBlock("loop");
        const out_block = self.context.getContext().createBasicBlock("out");

        _ = self.state.builder.buildBr(cond_block);

        self.current.?.function.?.appendExistingBasicBlock(cond_block);
        self.state.builder.positionBuilderAtEnd(cond_block);
        self.current.?.block = cond_block;

        const condition = self.state.builder.buildICmp(
            .EQ,
            (try self.generateNode(while_node.condition)).?,
            (try self.lowerBuzzApiType(.value)).constInt(
                Value.True.val,
                .False,
            ),
            "cond",
        );

        _ = self.state.builder.buildCondBr(
            condition,
            loop_block,
            out_block,
        );

        self.current.?.function.?.appendExistingBasicBlock(loop_block);
        self.state.builder.positionBuilderAtEnd(loop_block);
        self.current.?.block = loop_block;

        _ = try self.generateNode(while_node.block);

        _ = self.state.builder.buildBr(cond_block);

        self.current.?.function.?.appendExistingBasicBlock(out_block);
        self.state.builder.positionBuilderAtEnd(out_block);
        self.current.?.block = out_block;

        return null;
    }

    fn generateBlock(self: *Self, block_node: *BlockNode) VM.Error!?*l.Value {
        for (block_node.statements.items) |statement| {
            _ = try self.generateNode(statement);
        }

        return null;
    }

    fn generateFunDeclaration(self: *Self, fun_declaration_node: *FunDeclarationNode) VM.Error!?*l.Value {
        return try self.generateFunction(fun_declaration_node.function);
    }

    fn generateVarDeclaration(self: *Self, var_declaration_node: *VarDeclarationNode) VM.Error!?*l.Value {
        _ = try self.lowerType(var_declaration_node.type_def);

        // We should only declare locals
        assert(var_declaration_node.slot_type == .Local);

        return try self.buildSetLocal(
            var_declaration_node.slot,
            if (var_declaration_node.value) |value|
                (try self.generateNode(value)).?
            else
                (try self.lowerBuzzApiType(.value)).constInt(
                    Value.Null.val,
                    .False,
                ),
        );
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
    fn generateFunction(self: *Self, function_node: *FunctionNode) VM.Error!?*l.Value {
        const node = &function_node.node;

        var enclosing = self.current;
        self.current = try self.vm.gc.allocator.create(Frame);
        self.current.?.* = Frame{
            .enclosing = enclosing,
            .function_node = function_node,
            .locals = std.ArrayList(*l.Value).init(self.vm.gc.allocator),
        };

        const function_def = function_node.node.type_def.?.resolved_type.?.Function;
        const function_type = function_def.function_type;

        // Those are not allowed to be jitted
        assert(function_type != .Extern and function_type != .Anonymous and function_type != .Script and function_type != .ScriptEntryPoint);

        const ret_type = try self.lowerType(node.type_def.?);

        // Get fully qualified name of function
        var qualified_name = try self.getFunctionQualifiedName(function_node, true);
        defer qualified_name.deinit();

        var function = self.state.module.addFunction(
            @ptrCast([*:0]const u8, qualified_name.items),
            ret_type,
        );

        self.current.?.function = function;

        var block = self.context.getContext().appendBasicBlock(
            function,
            @ptrCast(
                [*:0]const u8,
                qualified_name.items,
            ),
        );
        self.state.builder.positionBuilderAtEnd(block);
        self.current.?.block = block;

        // First arg is reserved for an eventual `this` or cli arguments
        _ = switch (function_type) {
            .Method => unreachable, // this
            .Extern, .Anonymous, .EntryPoint, .ScriptEntryPoint => unreachable, // those are not allowed here
            else => try self.buildSetLocal(
                0,
                (try self.lowerBuzzApiType(.value)).constInt(Value.Void.val, .False),
            ),
        };

        // Put function arguments as locals
        var i: usize = 1;
        while (i <= function_def.parameters.count()) : (i += 1) {
            _ = try self.buildSetLocal(
                i,
                // Since actual function first arg is NativeCtx, no need to correct back with -1
                self.current.?.function.?.getParam(@intCast(c_uint, i)),
            );
        }

        if (function_node.arrow_expr) |arrow_expr| {
            const arrow_value = try self.generateNode(arrow_expr);

            _ = self.state.builder.buildRet(arrow_value.?);
            self.current.?.return_emitted = true;
        } else {
            _ = try self.generateBlock(function_node.body.?);
        }

        if (self.current.?.function_node.node.type_def.?.resolved_type.?.Function.return_type.def_type == .Void and !self.current.?.return_emitted) {
            // TODO: detect if some branches of the function body miss a return statement
            _ = self.state.builder.buildRet(
                (try self.lowerBuzzApiType(.value)).constInt(Value.Void.val, .False),
            );
        }

        // TODO: upvalues? closures?

        // Add the NativeFn version of the function
        try self.generateNativeFn(
            function_node,
            function,
            ret_type,
        );

        self.current.?.locals.deinit();
        self.current = self.current.?.enclosing;
        if (self.current != null and self.current.?.block != null) {
            self.state.builder.positionBuilderAtEnd(self.current.?.block.?);
        }

        return function;
    }

    /// Build instructions to get local at given index
    inline fn buildGetLocal(self: *Self, slot: usize) !*l.Value {
        assert(slot < self.current.?.locals.items.len);
        return self.state.builder.buildLoad(
            try self.lowerBuzzApiType(.value),
            self.current.?.locals.items[slot],
            "",
        );
    }

    /// Build instructinos to set local at given index
    fn buildSetLocal(self: *Self, slot: usize, value: *l.Value) !*l.Value {
        assert(self.current.?.locals.items.len >= slot);

        if (slot >= self.current.?.locals.items.len) {
            try self.current.?.locals.append(
                self.state.builder.buildAlloca(
                    try self.lowerBuzzApiType(.value),
                    "",
                ),
            );
        }

        return self.state.builder.buildStore(
            value,
            self.current.?.locals.items[slot],
        );
    }

    /// Build instructions to get global at given index
    fn buildGetGlobal(self: *Self, slot: usize) !*l.Value {
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
            &[_]*l.Value{
                self.context.getContext().intType(64).constInt(slot, .False),
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
    fn buildSetGlobal(self: *Self, slot: usize, value: *l.Value) !*l.Value {
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
            &[_]*l.Value{
                self.context.getContext().intType(64).constInt(slot, .False),
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

    fn buildValueToBoolean(self: *Self, value: *l.Value) *l.Value {
        return self.state.builder.buildICmp(
            .EQ,
            value,
            self.context.getContext().intType(64).constInt(_value.TrueMask, .False),
            "",
        );
    }

    fn buildValueFromBoolean(self: *Self, value: *l.Value) *l.Value {
        return self.state.builder.buildSelect(
            self.state.builder.buildICmp(
                .EQ,
                value,
                self.context.getContext().intType(1).constInt(1, .False),
                "",
            ),
            self.context.getContext().intType(64).constInt(Value.True.val, .False),
            self.context.getContext().intType(64).constInt(Value.False.val, .False),
            "",
        );
    }

    fn buildValueToInteger(self: *Self, value: *l.Value) *l.Value {
        return self.state.builder.buildAnd(
            value,
            self.context.getContext().intType(64).constInt(
                0xffffffff,
                .False,
            ),
            "",
        );
    }

    fn buildValueFromInteger(self: *Self, integer: *l.Value) *l.Value {
        return self.state.builder.buildOr(
            self.context.getContext().intType(64).constInt(
                _value.IntegerMask,
                .False,
            ),
            integer,
            "",
        );
    }

    fn buildValueToObj(self: *Self, value: *l.Value) *l.Value {
        return self.state.builder.buildAnd(
            value,
            self.state.builder.buildNot(
                self.context.getContext().intType(64).constInt(_value.PointerMask, .False),
                "",
            ),
            "",
        );
    }

    fn buildValueFromObj(self: *Self, value: *l.Value) *l.Value {
        return self.state.builder.buildOr(
            self.context.getContext().intType(64).constInt(
                _value.PointerMask,
                .False,
            ),
            value,
            "",
        );
    }

    // Unwrap buzz value to its raw llvm Value
    fn unwrap(self: *Self, def_type: ObjTypeDef.Type, value: *l.Value) *l.Value {
        return switch (def_type) {
            .Bool => self.buildValueToBoolean(value),
            .Integer => self.buildValueToInteger(value),
            .Float => self.state.builder.buildBitCast(
                value,
                self.context.getContext().doubleType(),
                "",
            ),
            .Void => value,
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
            => self.buildValueToObj(value),
            .Placeholder,
            .Generic,
            => unreachable,
        };
    }

    // Wrap llvm value to buzz Value
    fn wrap(self: *Self, def_type: ObjTypeDef.Type, value: *l.Value) *l.Value {
        return switch (def_type) {
            .Bool => self.buildValueFromBoolean(value),
            .Integer => self.buildValueFromInteger(value),
            .Float => self.state.builder.buildBitCast(
                value,
                self.context.getContext().intType(64),
                "",
            ),
            .Void => value,
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
            => self.buildValueFromObj(value),
            .Placeholder,
            .Generic,
            => unreachable,
        };
    }

    fn generateNativeFn(self: *Self, function_node: *FunctionNode, raw_fn: *l.Value, ret_type: *l.Type) !void {
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
        var block = self.context.getContext().appendBasicBlock(native_fn, @ptrCast([*:0]const u8, nativefn_qualified_name.items));
        self.state.builder.positionBuilderAtEnd(block);

        var arguments = std.ArrayList(*l.Value).init(self.vm.gc.allocator);
        defer arguments.deinit();
        const arg_count = function_def.parameters.count();

        // first arg is ctx
        try arguments.append(native_fn.getParam(0));

        if (arg_count > 0) {
            var i: i32 = @intCast(i32, arg_count - 1);
            // Each argument is a bz_peek(i) call
            while (i >= 0) : (i -= 1) {
                try arguments.append(
                    try self.buildBuzzApiCall(
                        .bz_peek,
                        &[_]*l.Value{
                            self.vmConstant(),
                            self.context.getContext().intType(32).constInt(
                                @intCast(c_ulonglong, i),
                                .False,
                            ),
                        },
                    ),
                );
            }
        }

        // Call the raw function
        const result = self.state.builder.buildCall(
            ret_type,
            raw_fn,
            @ptrCast([*]*l.Value, arguments.items.ptr),
            @intCast(c_uint, arguments.items.len),
            "",
        );

        const should_return = function_def.return_type.def_type != .Void;

        // Push its result back into the VM
        if (should_return) {
            _ = try self.buildBuzzApiCall(
                .bz_push,
                &[_]*l.Value{
                    self.vmConstant(),
                    result,
                },
            );
        }

        // 1 = there's a return, 0 = no return, -1 = error
        // TODO: error ?
        _ = self.state.builder.buildRet(
            self.context.getContext().intType(8).constInt(
                if (should_return) 1 else 0,
                .True,
            ),
        );
    }
};
