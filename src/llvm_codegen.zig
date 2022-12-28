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
const PlaceholderDef = _obj.PlaceholderDef;
const llvm = @import("./llvm.zig");
const Token = @import("./token.zig").Token;
const disassembler = @import("./disassembler.zig");
const disassembleChunk = disassembler.disassembleChunk;
const Parser = @import("./parser.zig").Parser;
const _memory = @import("./memory.zig");
const GarbageCollector = _memory.GarbageCollector;
const TypeRegistry = _memory.TypeRegistry;

fn toNullTerminatedCStr(allocator: Allocator, source: []const u8) ![]const u8 {
    var result = std.ArrayList(u8).init(allocator);
    try result.appendSlice(source);
    try result.append(0);

    return result.items;
}

fn lowerType(allocator: Allocator, obj_typedef: *ObjTypeDef, context: *llvm.Context) anyerror!*llvm.Type {
    return switch (obj_typedef.def_type) {
        .Bool => context.intType(8),
        .Integer => context.intType(64),
        .Float => context.doubleType(),
        .Void => context.voidType(),

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
        => context.pointerType(0),

        .Function => function: {
            const function_type = obj_typedef.resolved_type.?.Function;

            const return_type = try lowerType(allocator, function_type.return_type, context);
            // TODO yield_type
            var param_types = std.ArrayList(*llvm.Type).init(allocator);
            defer param_types.deinit(); // Can i free this?

            var it = function_type.parameters.iterator();
            while (it.next()) |kv| {
                try param_types.append(try lowerType(allocator, kv.value_ptr.*, context));
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

const GenState = struct {
    module: *llvm.Module,
    context: *llvm.Context,
    builder: *llvm.Builder,

    pub fn dispose(self: GenState) void {
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

pub const LLVMCodegen = struct {
    const Self = @This();

    parser: *Parser,
    gc: *GarbageCollector,

    current: ?*Frame = null,
    state: GenState = undefined,
    testing: bool,

    allocator: std.mem.Allocator,
    had_error: bool = false,
    panic_mode: bool = false,

    cli_args: ?*ObjList = null,

    // TODO: remove, this is only to test things out
    fn cliArgs(self: *Self, args: [][:0]u8) !*ObjList {
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

        for (args) |arg, index| {
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

        return arg_list;
    }

    fn executionEngine(_: *Self, module: *llvm.Module) *llvm.OrcLLJIT {
        llvm.initializeLLVMTarget(builtin.target.cpu.arch);
        var builder = llvm.OrcLLJITBuilder.createBuilder();

        // TODO: LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator ?

        var orc_jit: *llvm.OrcLLJIT = undefined;
        if (llvm.OrcLLJITBuilder.createOrcLLJIT(&orc_jit, builder)) |orc_error| {
            std.debug.print("\n{s}\n", .{orc_error.getErrorMessage()});

            // Return error instead of panicking
            @panic("Could not create OrcJIT");
        }

        if (orc_jit.addLLVMIRModule(
            orc_jit.getMainJITDylib(),
            llvm.OrcThreadSafeModule.createNewThreadSafeModule(
                module,
                llvm.OrcThreadSafeContext.create(),
            ),
        )) |orc_error| {
            std.debug.print("\n{s}\n", .{orc_error.getErrorMessage()});

            @panic("Could add IR module to OrcJIT");
        }

        return orc_jit;
    }

    pub fn execute(self: *Self, root: *FunctionNode, args: [][:0]u8) !void {
        self.cli_args = try self.cliArgs(args);

        if (try self.generate(root)) |module| {
            // TODO: Pass manager etc.

            var orc_jit = self.executionEngine(module);

            var qualified_name = try self.getFunctionQualifiedName(root);
            defer qualified_name.deinit();

            std.debug.print("Run script function `{s}`...\n", .{qualified_name.items});

            var fun_addr: u64 = undefined;

            if (orc_jit.lookup(&fun_addr, @ptrCast([*:0]const u8, qualified_name.items))) |orc_error| {
                std.debug.print("\n{s}\n", .{orc_error.getErrorMessage()});

                @panic("Could find script symbol in module loaded in LLJIT");
            }

            const fun_ptr = @intToPtr(*const fn () void, fun_addr);

            fun_ptr();
        }
    }

    fn generate(self: *Self, root: *FunctionNode) !?*llvm.Module {
        self.had_error = false;
        self.panic_mode = false;

        const name = root.node.type_def.?.resolved_type.?.Function.name.string;
        var namez = try toNullTerminatedCStr(self.gc.allocator, name);
        defer self.gc.allocator.free(namez);

        var context = llvm.Context.create();
        var module = llvm.Module.createWithName(@ptrCast([*:0]const u8, namez), context);

        self.state = .{
            .module = module,
            .context = context,
            .builder = context.createBuilder(),
        };

        if (BuildOptions.debug) {
            var out = std.ArrayList(u8).init(self.gc.allocator);
            defer out.deinit();

            try root.node.toJson(&root.node, &out.writer());

            try std.io.getStdOut().writer().print("\n{s}", .{out.items});
        }

        std.debug.print("Generate LLVM IR...\n", .{});

        _ = try self.generateNode(root.toNode());

        if (self.had_error) {
            return null;
        }

        var error_message: [*:0]const u8 = undefined;
        // verifyModule always allocs the error_message even if there is no error
        defer llvm.disposeMessage(error_message);

        std.debug.print("Verify LLVM module...\n", .{});

        if (module.verify(.ReturnStatus, &error_message).toBool()) {
            std.debug.print("\n{s}\n", .{error_message});

            _ = module.printModuleToFile("./out.bc", &error_message);

            @panic("LLVM module verification failed");
        }

        _ = module.printModuleToFile("./out.bc", &error_message);

        return module;
    }

    fn generateNode(self: *Self, node: *ParseNode) anyerror!?*llvm.Value {
        const lowered_type = if (node.type_def) |type_def| try lowerType(
            self.gc.allocator,
            type_def,
            self.state.context,
        ) else null;

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
            .StringLiteral => self.state.builder.buildIntToPtr(
                self.state.context.intType(64).constInt(
                    @ptrToInt(StringLiteralNode.cast(node).?.constant),
                    .False,
                ),
                lowered_type.?,
                "",
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

    fn generateNamedVariable(self: *Self, named_variable_node: *NamedVariableNode) anyerror!?*llvm.Value {
        const name = try toNullTerminatedCStr(self.gc.allocator, named_variable_node.identifier.lexeme);
        defer self.gc.allocator.free(name);

        const is_function = named_variable_node.node.type_def.?.def_type == .Function;
        const is_non_extern_function = is_function and named_variable_node.node.type_def.?.resolved_type.?.Function.function_type != .Extern;

        // TODO: qualified names!
        var variable = switch (named_variable_node.slot_type) {
            .Global => global: {
                if (is_non_extern_function) {
                    break :global self.state.module.getNamedFunction(@ptrCast([*:0]const u8, name)).?;
                }

                break :global self.state.module.getNamedGlobal(@ptrCast([*:0]const u8, name)).?;
            },
            .Local => unreachable,
            .UpValue => unreachable,
        };

        // Set
        if (named_variable_node.value) |value| {
            return switch (named_variable_node.slot_type) {
                .Global => self.state.builder.buildStore((try self.generateNode(value)).?, variable),
                .Local => unreachable,
                .UpValue => unreachable,
            };
        }

        return switch (named_variable_node.slot_type) {
            .Global => global: {
                if (is_non_extern_function) {
                    break :global variable;
                }

                const lowered = try lowerType(
                    self.gc.allocator,
                    named_variable_node.node.type_def.?,
                    self.state.context,
                );

                break :global self.state.builder.buildLoad(
                    if (is_function) lowered.pointerType(0) else lowered,
                    variable,
                    "",
                );
            },
            .Local => unreachable,
            .UpValue => unreachable,
        };
    }

    fn generateCall(self: *Self, call_node: *CallNode) anyerror!?*llvm.Value {
        if (call_node.callee.type_def == null or call_node.callee.type_def.?.def_type == .Placeholder) {
            try self.reportPlaceholder(call_node.callee.type_def.?.resolved_type.?.Placeholder);
        }

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
        const callee = (try self.generateNode(call_node.callee)).?;

        const callee_type = switch (call_node.callee.node_type) {
            .Dot => DotNode.cast(call_node.callee).?.member_type_def,
            else => call_node.callee.type_def,
        };

        const function_type = try callee_type.?.populateGenerics(
            callee_type.?.resolved_type.?.Function.id,
            call_node.resolved_generics,
            &self.gc.type_registry,
            null,
        );

        // TODO: arguments reordering

        const error_types = function_type.resolved_type.?.Function.error_types;
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

        var arguments = std.ArrayList(*llvm.Value).init(self.gc.allocator);
        defer arguments.deinit();

        var it = call_node.arguments.iterator();
        while (it.next()) |kv| {
            try arguments.append((try self.generateNode(kv.value_ptr.*)).?);
        }

        return self.state.builder.buildCall(
            try lowerType(
                self.gc.allocator,
                function_type,
                self.state.context,
            ),
            callee,
            @ptrCast([*]*llvm.Value, arguments.items.ptr),
            @intCast(c_uint, arguments.items.len),
            "",
        );
    }

    fn generateBlock(self: *Self, block_node: *BlockNode) anyerror!?*llvm.Value {
        var block_name = std.ArrayList(u8).init(self.gc.allocator);
        try block_name.appendSlice(self.current.?.function_node.node.type_def.?.resolved_type.?.Function.name.string);
        try block_name.appendSlice(".block");
        try block_name.append(0);
        defer block_name.deinit();

        const block = self.state.context.appendBasicBlock(
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

    fn generateFunDeclaration(self: *Self, fun_declaration_node: *FunDeclarationNode) anyerror!?*llvm.Value {
        return try self.generateFunctionNode(fun_declaration_node.function);
    }

    fn generateVarDeclaration(self: *Self, var_declaration_node: *VarDeclarationNode) anyerror!?*llvm.Value {
        var global: *llvm.Value = undefined;

        const lowered_type = try lowerType(self.gc.allocator, var_declaration_node.type_def, self.state.context);

        if (var_declaration_node.slot_type == .Global) {
            const var_name = try toNullTerminatedCStr(self.gc.allocator, var_declaration_node.name.lexeme);
            defer self.gc.allocator.free(var_name);

            // Create and store new value to global
            global = self.state.module.addGlobal(
                lowered_type,
                @ptrCast([*:0]const u8, var_name),
            );

            if (var_declaration_node.value == null) {
                // FIXME: need an initial value otherwise will be external ptr
                return global;
            }
        } else {
            // TODO: alloca
            unreachable;
        }

        if (var_declaration_node.value) |value| {
            const initial_value = try self.generateNode(value);

            if (var_declaration_node.slot_type == .Global) {
                const var_name = try toNullTerminatedCStr(self.gc.allocator, var_declaration_node.name.lexeme);
                defer self.gc.allocator.free(var_name);

                return self.state.builder.buildStore(initial_value.?, global);
            }

            return initial_value;
        }

        return null;
    }

    fn getFunctionQualifiedName(self: *Self, function_node: *FunctionNode) !std.ArrayList(u8) {
        const function_def = function_node.node.type_def.?.resolved_type.?.Function;
        const function_type = function_def.function_type;
        const name = function_node.node.type_def.?.resolved_type.?.Function.name.string;

        var qualified_name = std.ArrayList(u8).init(self.gc.allocator);

        try qualified_name.appendSlice(name);
        // Don't qualify extern functions or `main`
        if (function_type != .ScriptEntryPoint and function_type != .Extern and !std.mem.eql(u8, name, "main")) {
            var current = self.current;
            while (current) |frame| : (current = frame.enclosing) {
                try qualified_name.append('.');
                try qualified_name.appendSlice(frame.function_node.node.type_def.?.resolved_type.?.Function.name.string);
            }
        }
        try qualified_name.append(0);

        return qualified_name;
    }

    fn generateFunctionNode(self: *Self, function_node: *FunctionNode) anyerror!?*llvm.Value {
        const node = &function_node.node;

        var enclosing = self.current;
        self.current = try self.gc.allocator.create(Frame);
        self.current.?.* = Frame{
            .enclosing = enclosing,
            .function_node = function_node,
        };

        const function_def = function_node.node.type_def.?.resolved_type.?.Function;
        const function_type = function_def.function_type;

        const ret_type = try lowerType(
            self.gc.allocator,
            node.type_def.?,
            self.state.context,
        );

        // Get fully qualified name of function
        var qualified_name = try self.getFunctionQualifiedName(function_node);
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
                var block = self.state.context.appendBasicBlock(function, @ptrCast([*:0]const u8, qualified_name.items));
                self.state.builder.positionBuilderAtEnd(block);
                self.current.?.block = block;

                const arrow_value = try self.generateNode(arrow_expr);

                _ = self.state.builder.buildRet(arrow_value.?);
                self.current.?.return_emitted = true;
            } else {
                _ = try self.generateNode(function_node.body.?.toNode());
            }

            // If .Script, search for exported globals and return them in a map
            if (function_type == .Script or function_type == .ScriptEntryPoint) {
                // If top level, search `main` or `test` function(s) and call them
                // Then put any exported globals on the stack
                if (!self.testing and function_type == .ScriptEntryPoint) {
                    if (function_node.main_slot != null) {
                        if (self.state.module.getNamedFunction("main")) |main_function| {
                            _ = self.state.builder.buildCall(
                                try lowerType(
                                    self.gc.allocator,
                                    try self.parser.parseTypeDefFrom("Function main([str] args) > int"),
                                    self.state.context,
                                ), // FIXME: type of main
                                main_function,
                                &[_]*llvm.Value{
                                    self.state.builder.buildIntToPtr(
                                        self.state.context.intType(64).constInt(@ptrToInt(self.cli_args.?), .False),
                                        self.state.context.pointerType(0),
                                        "",
                                    ),
                                },
                                1,
                                "",
                            );
                        } else {
                            try self.reportError("Missing main function");
                        }
                    }
                } else if (self.testing and function_node.test_slots != null) {
                    // TODO: Create an entry point wich runs all `test`
                }

                // If we're being imported, put all globals on the stack
                if (function_node.import_root) {
                    // TODO: export globals?
                } else {
                    _ = self.state.builder.buildRetVoid();
                    self.current.?.return_emitted = true;
                }
            } else if (self.current.?.function_node.node.type_def.?.resolved_type.?.Function.return_type.def_type == .Void and !self.current.?.return_emitted) {
                // TODO: detect if some branches of the function body miss a return statement
                _ = self.state.builder.buildRetVoid();
            }

            // TODO: upvalues? closures?
        }

        self.current = self.current.?.enclosing;
        if (self.current != null and self.current.?.block != null) {
            self.state.builder.positionBuilderAtEnd(self.current.?.block.?);
        }

        return switch (function_type) {
            .Extern => ext: {
                function.setInitializer(
                    self.state.builder.buildIntToPtr(
                        self.state.context.intType(64).constInt(@ptrToInt(function_node.native.?.native), .False),
                        ret_type.pointerType(0),
                        "",
                    ),
                );

                break :ext null;
            },
            // TODO: closure
            else => function,
        };
    }

    fn report(self: *Self, location: Token, message: []const u8) !void {
        const lines: std.ArrayList([]const u8) = try location.getLines(self.gc.allocator, 3);
        defer lines.deinit();
        var report_line = std.ArrayList(u8).init(self.gc.allocator);
        defer report_line.deinit();
        var writer = report_line.writer();

        try writer.print("", .{});
        var l: usize = if (location.line > 0) location.line - 1 else 0;
        for (lines.items) |line| {
            if (l != location.line) {
                try writer.print("\u{001b}[2m", .{});
            }

            var prefix_len: usize = report_line.items.len;
            try writer.print(" {: >5} |", .{l + 1});
            prefix_len = report_line.items.len - prefix_len;
            try writer.print(" {s}\n\u{001b}[0m", .{line});

            if (l == location.line) {
                try writer.writeByteNTimes(' ', location.column + prefix_len);
                try writer.print("\u{001b}[31m^\u{001b}[0m\n", .{});
            }

            l += 1;
        }
        std.debug.print("{s}:{}:{}: \u{001b}[31mCompile error:\u{001b}[0m {s}\n{s}", .{
            location.script_name,
            location.line + 1,
            location.column + 1,
            message,
            report_line.items,
        });

        if (BuildOptions.stop_on_report) {
            unreachable;
        }
    }

    // Unlocated error, should not be used
    fn reportError(self: *Self, message: []const u8) !void {
        if (self.panic_mode) {
            return;
        }

        self.panic_mode = true;
        self.had_error = true;

        try self.report(
            Token{
                .token_type = .Error,
                .source = "",
                .script_name = "",
                .lexeme = "",
                .line = 0,
                .column = 0,
            },
            message,
        );
    }

    pub fn reportErrorAt(self: *Self, token: Token, message: []const u8) !void {
        if (self.panic_mode) {
            return;
        }

        self.panic_mode = true;
        self.had_error = true;

        try self.report(token, message);
    }

    pub fn reportErrorFmt(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) !void {
        var message = std.ArrayList(u8).init(self.gc.allocator);
        defer message.deinit();

        var writer = message.writer();
        try writer.print(fmt, args);

        try self.reportErrorAt(token, message.items);
    }

    pub fn reportTypeCheckAt(self: *Self, expected_type: *ObjTypeDef, actual_type: *ObjTypeDef, message: []const u8, at: Token) !void {
        var error_message = std.ArrayList(u8).init(self.gc.allocator);
        var writer = &error_message.writer();

        try writer.print("{s}: expected type `", .{message});
        try expected_type.toString(writer);
        try writer.writeAll("`, got `");
        try actual_type.toString(writer);
        try writer.writeAll("`");

        try self.reportErrorAt(at, error_message.items);
    }

    // Got to the root placeholder and report it
    pub fn reportPlaceholder(self: *Self, placeholder: PlaceholderDef) anyerror!void {
        if (placeholder.parent) |parent| {
            try self.reportPlaceholder(parent.resolved_type.?.Placeholder);
        } else {
            // Should be a root placeholder with a name
            assert(placeholder.name != null);
            try self.reportErrorFmt(placeholder.where, "`{s}` is not defined", .{placeholder.name.?.string});
        }
    }
};
