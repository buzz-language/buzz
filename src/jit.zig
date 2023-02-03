const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const BuildOptions = @import("build_options");
const _node = @import("./node.zig");
const BinaryNode = _node.BinaryNode;
const BlockNode = _node.BlockNode;
const BooleanNode = _node.BooleanNode;
const CallNode = _node.CallNode;
const DotNode = _node.DotNode;
const DoUntilNode = _node.DoUntilNode;
const ExpressionNode = _node.ExpressionNode;
const FloatNode = _node.FloatNode;
const ForNode = _node.ForNode;
const FunctionNode = _node.FunctionNode;
const FunDeclarationNode = _node.FunDeclarationNode;
const GroupingNode = _node.GroupingNode;
const IfNode = _node.IfNode;
const IntegerNode = _node.IntegerNode;
const IsNode = _node.IsNode;
const ListNode = _node.ListNode;
const MapNode = _node.MapNode;
const NamedVariableNode = _node.NamedVariableNode;
const ParseNode = _node.ParseNode;
const ReturnNode = _node.ReturnNode;
const StringLiteralNode = _node.StringLiteralNode;
const StringNode = _node.StringNode;
const SubscriptNode = _node.SubscriptNode;
const ThrowNode = _node.ThrowNode;
const TryNode = _node.TryNode;
const VarDeclarationNode = _node.VarDeclarationNode;
const WhileNode = _node.WhileNode;
const UnwrapNode = _node.UnwrapNode;
const ObjectInitNode = _node.ObjectInitNode;
const ForceUnwrapNode = _node.ForceUnwrapNode;
const UnaryNode = _node.UnaryNode;
const PatternNode = _node.PatternNode;
const ForEachNode = _node.ForEachNode;
const InlineIfNode = _node.InlineIfNode;
const _obj = @import("./obj.zig");
const _value = @import("./value.zig");
const Value = _value.Value;
const Obj = _obj.Obj;
const ObjString = _obj.ObjString;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjList = _obj.ObjList;
const ObjMap = _obj.ObjMap;
const ObjEnum = _obj.ObjEnum;
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
    // Closure being jitted right now
    closure: ?*ObjClosure = null,
    current: ?*Frame = null,
    opt_jump: ?OptJump = null,

    pub fn deinit(self: GenState) void {
        self.builder.dispose();
        // self.module ownership is taken by LLJIT
    }
};

const OptJump = struct {
    // FIXME: free those!
    current_blocks: std.ArrayList(*l.BasicBlock),
    next_expr_blocks: std.ArrayList(*l.BasicBlock),
    alloca: *l.Value,

    pub fn deinit(self: OptJump) void {
        self.next_expr_blocks.deinit();
        self.current_blocks.deinit();
    }
};

pub const Frame = struct {
    enclosing: ?*Frame = null,
    function_node: *FunctionNode,
    return_counts: bool = false,
    return_emitted: bool = false,

    try_should_handle: ?std.AutoHashMap(*ObjTypeDef, void) = null,

    function: ?*l.Value = null,
    // Current block in which we write IR
    block: ?*l.BasicBlock = null,
    // Block to jump to when breaking a loop
    break_block: ?*l.BasicBlock = null,
    // Block to jump to when continuing a loop
    continue_block: ?*l.BasicBlock = null,
};

pub const ExternApi = enum {
    nativefn,
    nativectx,
    value,
    tryctx,

    bz_push,
    bz_valueToExternNativeFn,
    bz_valueToRawNative,
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
    bz_getUpValues,
    bz_getGlobals,
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
    globals,

    bz_dumpStack,

    // https://opensource.apple.com/source/libplatform/libplatform-161/include/setjmp.h.auto.html
    jmp_buf,
    setjmp,

    // TODO: use comptime maps
    pub fn name(self: ExternApi) []const u8 {
        return switch (self) {
            .nativefn => "NativeFn",
            .nativectx => "NativeCtx",
            .tryctx => "TryCtx",
            .value => "Value",
            .globals => "globals",

            .bz_push => "bz_push",
            .bz_valueToExternNativeFn => "bz_valueToExternNativeFn",
            .bz_valueToRawNative => "bz_valueToRawNative",
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
            .bz_closeUpValues => "bz_closeUpValues",
            .bz_getUpValue => "bz_getUpValue",
            .bz_setUpValue => "bz_setUpValue",
            .bz_getUpValues => "bz_getUpValues",
            .bz_getGlobals => "bz_getGlobals",
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
            .setjmp => if (builtin.os.tag == .macos or builtin.os.tag == .linux) "_setjmp" else "setjmp",

            .jmp_buf => "jmp_buf",

            .bz_dumpStack => "bz_dumpStack",
        };
    }

    pub fn namez(self: ExternApi) [*:0]const u8 {
        return switch (self) {
            .nativefn => "NativeFn",
            .nativectx => "NativeCtx",
            .tryctx => "TryCtx",
            .value => "Value",
            .globals => "globals",

            .bz_push => "bz_push",
            .bz_valueToExternNativeFn => "bz_valueToExternNativeFn",
            .bz_valueToRawNative => "bz_valueToRawNative",
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
            .bz_getUpValues => "bz_getUpValues",
            .bz_getGlobals => "bz_getGlobals",
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
            .setjmp => if (builtin.os.tag == .macos or builtin.os.tag == .linux) "_setjmp" else "setjmp",

            .jmp_buf => "jmp_buf",

            .bz_dumpStack => "bz_dumpStack",
        };
    }

    pub fn lower(self: ExternApi, context: *l.Context) !*l.Type {
        return switch (self) {
            .bz_push => l.functionType(
                context.voidType(),
                &[_]*l.Type{
                    context.pointerType(0),
                    context.intType(64),
                },
                2,
                .False,
            ),
            .bz_valueToExternNativeFn, .bz_valueToRawNative => l.functionType(
                context.pointerType(0),
                &[_]*l.Type{context.intType(64)},
                1,
                .False,
            ),
            .bz_objStringConcat => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    context.pointerType(0),
                    try ExternApi.value.lower(context),
                    try ExternApi.value.lower(context),
                },
                3,
                .False,
            ),
            .bz_objStringSubscript => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    context.pointerType(0),
                    try ExternApi.value.lower(context),
                    try ExternApi.value.lower(context),
                },
                3,
                .False,
            ),
            .bz_toString => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{ context.pointerType(0), try ExternApi.value.lower(context) },
                2,
                .False,
            ),
            .bz_newList => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{ context.pointerType(0), try ExternApi.value.lower(context) },
                2,
                .False,
            ),
            .bz_listAppend => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    context.pointerType(0),
                    try ExternApi.value.lower(context),
                    try ExternApi.value.lower(context),
                },
                3,
                .False,
            ),
            .bz_listGet => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    try ExternApi.value.lower(context),
                    context.intType(32),
                },
                2,
                .False,
            ),
            .bz_listSet => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    context.pointerType(0),
                    try ExternApi.value.lower(context),
                    context.intType(32),
                    try ExternApi.value.lower(context),
                },
                4,
                .False,
            ),
            .bz_valueEqual => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    try ExternApi.value.lower(context),
                    try ExternApi.value.lower(context),
                },
                2,
                .False,
            ),
            .bz_listConcat => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    context.pointerType(0),
                    try ExternApi.value.lower(context),
                    try ExternApi.value.lower(context),
                },
                3,
                .False,
            ),
            .bz_newMap => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    context.pointerType(0),
                    try ExternApi.value.lower(context),
                },
                2,
                .False,
            ),
            .bz_mapSet => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    context.pointerType(0),
                    try ExternApi.value.lower(context),
                    try ExternApi.value.lower(context),
                    try ExternApi.value.lower(context),
                },
                4,
                .False,
            ),
            .bz_mapGet => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    try ExternApi.value.lower(context),
                    try ExternApi.value.lower(context),
                },
                2,
                .False,
            ),
            .bz_mapConcat => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    context.pointerType(0),
                    try ExternApi.value.lower(context),
                    try ExternApi.value.lower(context),
                },
                3,
                .False,
            ),
            .bz_valueIs => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    try ExternApi.value.lower(context),
                    try ExternApi.value.lower(context),
                },
                2,
                .False,
            ),
            .bz_setTryCtx => l.functionType(
                // *TryContext
                (try ExternApi.tryctx.lower(context)).pointerType(0),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                },
                1,
                .False,
            ),
            .bz_popTryCtx => l.functionType(
                context.voidType(),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                },
                1,
                .False,
            ),
            .bz_rethrow => l.functionType(
                context.voidType(),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                },
                1,
                .False,
            ),
            .bz_throw => l.functionType(
                context.voidType(),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                    // payload
                    try ExternApi.value.lower(context),
                },
                2,
                .False,
            ),
            .bz_getUpValue => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    (try ExternApi.nativectx.lower(context)).pointerType(0),
                    context.intType(64),
                },
                2,
                .False,
            ),
            .bz_setUpValue => l.functionType(
                context.voidType(),
                &[_]*l.Type{
                    (try ExternApi.nativectx.lower(context)).pointerType(0),
                    context.intType(64),
                    try ExternApi.value.lower(context),
                },
                3,
                .False,
            ),
            .bz_closeUpValues => l.functionType(
                context.voidType(),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                    // payload
                    (try ExternApi.value.lower(context)).pointerType(0),
                },
                2,
                .False,
            ),
            .bz_getUpValues => l.functionType(
                context.pointerType(0),
                &[_]*l.Type{
                    try ExternApi.value.lower(context),
                },
                1,
                .False,
            ),
            .bz_getGlobals => l.functionType(
                (try ExternApi.value.lower(context)).pointerType(0),
                &[_]*l.Type{
                    try ExternApi.value.lower(context),
                },
                1,
                .False,
            ),
            .bz_closure => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    // NativeCtx
                    (try ExternApi.nativectx.lower(context)).pointerType(0),
                    // function
                    context.pointerType(0),
                    // native
                    context.pointerType(0),
                    // native_raw
                    context.pointerType(0),
                },
                4,
                .False,
            ),
            .bz_context => l.functionType(
                // ptr to raw fn
                context.pointerType(0),
                &[_]*l.Type{
                    // NativeCtx
                    (try ExternApi.nativectx.lower(context)).pointerType(0),
                    // function
                    try ExternApi.value.lower(context),
                    // new NativeCtx
                    (try ExternApi.nativectx.lower(context)).pointerType(0),
                },
                3,
                .False,
            ),
            .bz_instance => l.functionType(
                // instance
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                    // object or Value.Null
                    try ExternApi.value.lower(context),
                    // typedef or Value.Null
                    try ExternApi.value.lower(context),
                },
                3,
                .False,
            ),
            .bz_setInstanceField, .bz_setObjectField => l.functionType(
                // instance
                context.voidType(),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                    // instance
                    try ExternApi.value.lower(context),
                    // field name
                    try ExternApi.value.lower(context),
                    // value
                    try ExternApi.value.lower(context),
                },
                4,
                .False,
            ),
            .bz_getInstanceField => l.functionType(
                // instance
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                    // field name
                    try ExternApi.value.lower(context),
                    // value
                    try ExternApi.value.lower(context),
                    // bind
                    context.intType(1),
                },
                4,
                .False,
            ),
            .bz_getObjectField => l.functionType(
                // instance
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    // field name
                    try ExternApi.value.lower(context),
                    // value
                    try ExternApi.value.lower(context),
                },
                2,
                .False,
            ),
            .bz_getEnumCase => l.functionType(
                // instance
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                    // enum
                    try ExternApi.value.lower(context),
                    // case name
                    try ExternApi.value.lower(context),
                },
                3,
                .False,
            ),
            .bz_getEnumCaseValue => l.functionType(
                // instance
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    // enum instance
                    try ExternApi.value.lower(context),
                },
                1,
                .False,
            ),
            .bz_getListField,
            .bz_getMapField,
            .bz_getStringField,
            .bz_getPatternField,
            .bz_getFiberField,
            => l.functionType(
                // instance
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                    // list/map/... instance
                    try ExternApi.value.lower(context),
                    // field
                    try ExternApi.value.lower(context),
                    // bind
                    context.intType(1),
                },
                4,
                .False,
            ),
            .bz_getEnumCaseFromValue => l.functionType(
                // instance
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                    // enum
                    try ExternApi.value.lower(context),
                    // value
                    try ExternApi.value.lower(context),
                },
                3,
                .False,
            ),
            .bz_bindMethod => l.functionType(
                // instance
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                    // receiver
                    try ExternApi.value.lower(context),
                    // closure
                    try ExternApi.value.lower(context),
                    // native
                    try ExternApi.value.lower(context),
                },
                4,
                .False,
            ),
            .setjmp => l.functionType(
                context.intType(@sizeOf(c_int)),
                &[_]*l.Type{
                    // vm
                    try ExternApi.jmp_buf.lower(context),
                },
                1,
                .False,
            ),
            .bz_dumpStack => l.functionType(
                context.voidType(),
                &[_]*l.Type{
                    (try ExternApi.nativectx.lower(context)).pointerType(0),
                    context.intType(64),
                },
                2,
                .False,
            ),
            .bz_stringNext,
            .bz_listNext,
            .bz_mapNext,
            => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                    // iterable
                    try ExternApi.value.lower(context),
                    // key/index
                    (try ExternApi.value.lower(context)).pointerType(0),
                },
                3,
                .False,
            ),
            .bz_enumNext => l.functionType(
                try ExternApi.value.lower(context),
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                    // iterable
                    try ExternApi.value.lower(context),
                    // key/index
                    try ExternApi.value.lower(context),
                },
                3,
                .False,
            ),
            .nativefn => l.functionType(
                context.intType(8),
                &[_]*l.Type{
                    (try ExternApi.nativectx.lower(context)).pointerType(0),
                },
                1,
                .False,
            ),
            .value => context.intType(64),
            .nativectx => context.structCreateNamed(
                "NativeCtx",
                &[_]*l.Type{
                    // vm
                    context.pointerType(0),
                    // globals
                    try ExternApi.globals.lower(context),
                    // upvalues
                    context.pointerType(0).pointerType(0),
                    // base,
                    (try ExternApi.value.lower(context)).pointerType(0),
                    // stack_top,
                    (try ExternApi.value.lower(context)).pointerType(0).pointerType(0),
                },
                5,
                .False,
            ),
            .tryctx => context.structCreateNamed(
                "TryCtx",
                &[_]*l.Type{
                    // *TryCtx, opaque to avoid infinite recursion
                    context.pointerType(0),
                    try ExternApi.jmp_buf.lower(context),
                },
                2,
                .False,
            ),
            .globals => (try ExternApi.value.lower(context)).pointerType(0),

            // TODO: Is it a c_int on all platforms?
            .jmp_buf => context.intType(@sizeOf(c_int)).pointerType(0),
        };
    }

    pub fn declare(self: ExternApi, context: *l.Context, module: *l.OrcThreadSafeModule) void {
        if (module.getNamedFunction(self.namez()) == null) {
            _ = module.addFunction(
                @ptrCast([*:0]const u8, self.name()),
                try self.lower(context),
            );
        }
    }
};

pub const JIT = struct {
    const Self = @This();

    vm: *VM,

    state: ?GenState = null,

    api_lowered_types: std.AutoHashMap(ExternApi, *l.Type),
    lowered_types: std.AutoHashMap(*ObjTypeDef, *l.Type),
    // List of closures being or already compiled
    compiled_closures: std.AutoHashMap(*ObjClosure, void),

    orc_jit: *l.OrcLLJIT,
    context: *l.OrcThreadSafeContext,

    // Thresholds data

    // Call call of all functions
    call_count: u128 = 0,

    // Keeps track of time spent in the JIT
    jit_time: usize = 0,

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
            .api_lowered_types = std.AutoHashMap(ExternApi, *l.Type).init(vm.gc.allocator),
            .lowered_types = std.AutoHashMap(*ObjTypeDef, *l.Type).init(vm.gc.allocator),
            .orc_jit = orc_jit,
            .context = l.OrcThreadSafeContext.create(),
            .compiled_closures = std.AutoHashMap(*ObjClosure, void).init(vm.gc.allocator),
        };

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.api_lowered_types.deinit();
        self.lowered_types.deinit();
        if (self.state) |state| {
            state.deinit();
        }
        self.compiled_closures.deinit();
    }

    // TODO: Kinda stupid, it's either Value or function type
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
            .Fiber,
            .Protocol,
            .ProtocolInstance,
            .Generic,
            .Placeholder,
            => self.context.getContext().intType(64),

            .Function => l.functionType(
                try self.lowerExternApi(.value),
                &[_]*l.Type{
                    (try self.lowerExternApi(.nativectx)).pointerType(0),
                },
                1,
                .False,
            ),
        };

        try self.lowered_types.put(obj_typedef, lowered.?);

        return lowered.?;
    }

    fn lowerExternApi(self: *Self, method: ExternApi) !*l.Type {
        if (self.api_lowered_types.get(method)) |lowered| {
            return lowered;
        }

        const lowered = try method.lower(self.context.getContext());

        try self.api_lowered_types.put(
            method,
            lowered,
        );

        return lowered;
    }

    fn buildExternApiCall(self: *Self, method: ExternApi, args: []*l.Value) !*l.Value {
        method.declare(self.context.getContext(), self.state.?.module);

        return self.state.?.builder.buildCall(
            try self.lowerExternApi(method),
            self.state.?.module.getNamedFunction(method.namez()).?,
            args.ptr,
            @intCast(c_uint, args.len),
            "",
        );
    }

    fn buildDumpStack(self: *Self, off: usize) !void {
        _ = try self.buildExternApiCall(
            .bz_dumpStack,
            &[_]*l.Value{
                self.state.?.current.?.function.?.getParam(0),
                self.context.getContext().intType(64).constInt(
                    off,
                    .False,
                ),
            },
        );
    }

    inline fn vmConstant(self: *Self) *l.Value {
        return self.state.?.builder.buildIntToPtr(
            self.context.getContext().intType(64).constInt(
                @ptrToInt(self.vm),
                .False,
            ),
            self.context.getContext().pointerType(0),
            "",
        );
    }

    pub fn compileNativeFn(self: *Self, native: *ObjNative) VM.Error!*anyopaque {
        const name = self.vm.gc.allocator.dupeZ(u8, native.name);
        defer self.vm.gc.allocator.free(name);

        self.state.?.module.addFunction(
            name.ptr,
            try self.lowerExternApi(.nativefn),
        );

        var error_message: [*:0]const u8 = undefined;
        // verifyModule always allocs the error_message even if there is no error
        defer l.disposeMessage(error_message);

        if (self.state.?.module.verify(.ReturnStatus, &error_message).toBool()) {
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

    pub fn shouldCompileFunction(self: *Self, closure: *ObjClosure) bool {
        const function_type = closure.function.type_def.resolved_type.?.Function.function_type;

        if (function_type == .Extern or function_type == .Script or function_type == .ScriptEntryPoint or function_type == .EntryPoint) {
            return false;
        }

        if (self.compiled_closures.get(closure) != null) {
            return false;
        }

        return if (BuildOptions.jit_debug)
            return true
        else
            (closure.function.call_count / self.call_count) == BuildOptions.jit_prof_threshold;
    }

    pub fn compileFunction(self: *Self, closure: *ObjClosure) VM.Error!void {
        const previous_state = self.state;

        var module = l.Module.createWithName("buzz-jit", self.context.getContext());
        var thread_safe_module = l.OrcThreadSafeModule.createNewThreadSafeModule(
            module,
            self.context,
        );

        self.state = .{
            .module = thread_safe_module,
            .builder = self.context.getContext().createBuilder(),
        };

        try self.compiled_closures.put(closure, {});
        self.state.?.closure = closure;
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

        if (BuildOptions.jit_debug) {
            std.debug.print("Compiling function `{s}`\n", .{qualified_name.items});
        }

        _ = try self.generateNode(function_node.toNode());

        var error_message: [*:0]const u8 = undefined;
        // verifyModule always allocs the error_message even if there is no error
        defer l.disposeMessage(error_message);

        if (self.state.?.module.verify(.ReturnStatus, &error_message).toBool()) {
            std.debug.print("\n{s}\n", .{error_message});

            if (BuildOptions.jit_debug) {
                const simple_name = function_node.node.type_def.?.resolved_type.?.Function.name.string;
                var filename = std.ArrayList(u8).init(self.vm.gc.allocator);
                defer filename.deinit();
                filename.writer().print("./dist/out-{s}.bc", .{simple_name}) catch unreachable;

                _ = self.state.?.module.printModuleToFile(
                    self.vm.gc.allocator.dupeZ(u8, filename.items) catch unreachable,
                    &error_message,
                );
            }

            @panic("LLVM module verification failed");
        }

        if (BuildOptions.jit_debug) {
            const simple_name = function_node.node.type_def.?.resolved_type.?.Function.name.string;
            var filename = std.ArrayList(u8).init(self.vm.gc.allocator);
            defer filename.deinit();
            filename.writer().print("./dist/out-{s}.bc", .{simple_name}) catch unreachable;

            _ = self.state.?.module.printModuleToFile(
                self.vm.gc.allocator.dupeZ(u8, filename.items) catch unreachable,
                &error_message,
            );
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

        closure.function.native = @intToPtr(*anyopaque, fun_addr);
        closure.function.native_raw = @intToPtr(*anyopaque, fun_addr_raw);

        // self.state.?.deinit();
        self.state = previous_state;
    }

    fn generateNode(self: *Self, node: *ParseNode) VM.Error!?*l.Value {
        const lowered_type = if (node.type_def) |type_def| try self.lowerType(type_def) else null;

        var value = if (node.isConstant(node))
            self.context.getContext().intType(64).constInt(
                (node.toValue(node, self.vm.gc) catch return VM.Error.Custom).val,
                .False,
            )
        else switch (node.node_type) {
            .Boolean => lowered_type.?.constInt(
                Value.fromBoolean(BooleanNode.cast(node).?.constant).val,
                .False,
            ),
            .Float => lowered_type.?.constInt(
                Value.fromFloat(FloatNode.cast(node).?.float_constant).val,
                .False,
            ),
            .Integer => lowered_type.?.constInt(
                Value.fromInteger(IntegerNode.cast(node).?.integer_constant).val,
                .False,
            ),
            .StringLiteral => self.context.getContext().intType(64).constInt(
                StringLiteralNode.cast(node).?.constant.toValue().val,
                .False,
            ),
            .Null => (try self.lowerExternApi(.value)).constInt(
                Value.Null.val,
                .False,
            ),
            .Void => (try self.lowerExternApi(.value)).constInt(
                Value.Void.val,
                .False,
            ),

            .String => try self.generateString(StringNode.cast(node).?),
            .Expression => try self.generateNode(ExpressionNode.cast(node).?.expression),
            .Grouping => try self.generateNode(GroupingNode.cast(node).?.expression),
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
            .DoUntil => try self.generateDoUntil(DoUntilNode.cast(node).?),
            .For => try self.generateFor(ForNode.cast(node).?),
            .Break => try self.generateBreak(),
            .Continue => try self.generateContinue(),
            .List => try self.generateList(ListNode.cast(node).?),
            .Dot => try self.generateDot(DotNode.cast(node).?),
            .Subscript => try self.generateSubscript(SubscriptNode.cast(node).?),
            .Map => try self.generateMap(MapNode.cast(node).?),
            .Is => try self.generateIs(IsNode.cast(node).?),
            .Try => try self.generateTry(TryNode.cast(node).?),
            .Throw => try self.generateThrow(ThrowNode.cast(node).?),
            .Unwrap => try self.generateUnwrap(UnwrapNode.cast(node).?),
            .ObjectInit => try self.generateObjectInit(ObjectInitNode.cast(node).?),
            .ForceUnwrap => try self.generateForceUnwrap(ForceUnwrapNode.cast(node).?),
            .Unary => try self.generateUnary(UnaryNode.cast(node).?),
            .Pattern => try self.generatePattern(PatternNode.cast(node).?),
            .ForEach => try self.generateForEach(ForEachNode.cast(node).?),
            .InlineIf => try self.generateInlineIf(InlineIfNode.cast(node).?),

            else => {
                std.debug.print("{} NYI\n", .{node.node_type});
                unreachable;
            },
        };

        // Patch opt jumps if needed
        if (node.patch_opt_jumps) {
            assert(self.state.?.opt_jump != null);

            const out_block = self.context.getContext().createBasicBlock("out");

            // We reached here, means nothing was null, set the alloca with the value and use it has the node return value
            _ = self.state.?.builder.buildStore(
                value.?,
                self.state.?.opt_jump.?.alloca,
            );

            // Continue
            _ = self.state.?.builder.buildBr(out_block);

            // Patch opt blocks with the branching
            for (self.state.?.opt_jump.?.current_blocks.items) |current_block, index| {
                const next_expr_block = self.state.?.opt_jump.?.next_expr_blocks.items[index];

                self.state.?.builder.positionBuilderAtEnd(current_block);
                self.state.?.current.?.block = current_block;

                const is_null = self.state.?.builder.buildICmp(
                    .EQ,
                    self.state.?.builder.buildLoad(
                        try self.lowerExternApi(.value),
                        self.state.?.opt_jump.?.alloca,
                        "unwrapped",
                    ),
                    (try self.lowerExternApi(.value)).constInt(
                        Value.Null.val,
                        .False,
                    ),
                    "is_null",
                );

                _ = self.state.?.builder.buildCondBr(
                    is_null,
                    out_block,
                    next_expr_block,
                );
            }

            self.state.?.current.?.function.?.appendExistingBasicBlock(out_block);
            self.state.?.builder.positionBuilderAtEnd(out_block);
            self.state.?.current.?.block = out_block;

            value = self.state.?.builder.buildLoad(
                try self.lowerExternApi(.value),
                self.state.?.opt_jump.?.alloca,
                "opt_resolved",
            );

            self.state.?.opt_jump.?.deinit();
            self.state.?.opt_jump = null;
        }

        // Close scope if needed
        if (node.ends_scope) |closing| {
            for (closing.items) |op| {
                if (op == .OP_CLOSE_UPVALUE) {
                    try self.buildCloseUpValues();
                } else if (op == .OP_POP) {
                    _ = try self.buildPop();
                } else {
                    unreachable;
                }
            }
        }

        return value;
    }

    inline fn readConstant(self: *Self, arg: u24) Value {
        return self.state.?.closure.?.function.chunk.constants.items[arg];
    }

    fn generateString(self: *Self, string_node: *StringNode) VM.Error!?*l.Value {
        if (string_node.elements.len == 0) {
            return (try self.lowerExternApi(.value)).constInt(
                self.readConstant(0).val,
                .False,
            ); // Constant 0 is the empty string
        }

        var previous: ?*l.Value = null;
        for (string_node.elements) |element, index| {
            var value = (try self.generateNode(element)).?;

            if (element.type_def.?.def_type != .String or element.type_def.?.optional) {
                value = try self.buildExternApiCall(
                    .bz_toString,
                    &[_]*l.Value{
                        self.vmConstant(),
                        value,
                    },
                );
            }

            if (index >= 1) {
                value = try self.buildExternApiCall(
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
                    const closure = ObjClosure.cast(self.state.?.closure.?.globals.items[named_variable_node.slot].obj()).?;

                    // Does it need to be compiled?
                    if (self.compiled_closures.get(closure) == null) {
                        const qualified_name = try self.getFunctionQualifiedName(
                            @ptrCast(
                                *FunctionNode,
                                @alignCast(@alignOf(FunctionNode), closure.function.node),
                            ),
                            true,
                        );
                        defer qualified_name.deinit();

                        try self.compileFunction(closure);

                        // Declare it in this module
                        _ = self.state.?.module.addFunction(
                            @ptrCast([*:0]const u8, qualified_name.items),
                            try self.lowerType(closure.function.type_def),
                        );
                    }

                    break :global self.context.getContext().intType(64).constInt(
                        closure.toValue().val,
                        .False,
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
            .UpValue => upvalue: {
                if (named_variable_node.value) |value| {
                    break :upvalue try self.buildExternApiCall(
                        .bz_setUpValue,
                        &[_]*l.Value{
                            self.state.?.current.?.function.?.getParam(0),
                            self.context.getContext().intType(64).constInt(
                                named_variable_node.slot,
                                .False,
                            ),
                            (try self.generateNode(value)).?,
                        },
                    );
                }

                break :upvalue try self.buildExternApiCall(
                    .bz_getUpValue,
                    &[_]*l.Value{
                        self.state.?.current.?.function.?.getParam(0),
                        self.context.getContext().intType(64).constInt(
                            named_variable_node.slot,
                            .False,
                        ),
                    },
                );
            },
        };
    }

    fn generateCall(self: *Self, call_node: *CallNode) VM.Error!?*l.Value {
        // This is not a call but an Enum(value)
        if (call_node.callee.type_def.?.def_type == .Enum) {
            const value = call_node.arguments.get(call_node.arguments.keys()[0]).?;

            return try self.buildExternApiCall(
                .bz_getEnumCaseFromValue,
                &[_]*l.Value{
                    self.vmConstant(),
                    (try self.generateNode(call_node.callee)).?,
                    (try self.generateNode(value)).?,
                },
            );
        }

        // Find out if call is invoke or regular call
        const dot = DotNode.cast(call_node.callee);
        const invoked_on = if (call_node.callee.node_type == .Dot)
            dot.?.callee.type_def.?.def_type
        else
            null;

        const subject = if (invoked_on != null) try self.generateNode(dot.?.callee) else null;
        var callee: *l.Value = if (invoked_on != null)
            (switch (invoked_on.?) {
                .Object => try self.buildExternApiCall(
                    .bz_getObjectField,
                    &[_]*l.Value{
                        subject.?,
                        self.context.getContext().intType(64).constInt(
                            (try self.vm.gc.copyString(DotNode.cast(call_node.callee).?.identifier.lexeme)).toValue().val,
                            .False,
                        ),
                    },
                ),
                .ObjectInstance => try self.buildExternApiCall(
                    .bz_getInstanceField,
                    &[_]*l.Value{
                        self.vmConstant(),
                        subject.?,
                        self.context.getContext().intType(64).constInt(
                            (try self.vm.gc.copyString(DotNode.cast(call_node.callee).?.identifier.lexeme)).toValue().val,
                            .False,
                        ),
                        self.context.getContext().intType(1).constInt(0, .False),
                    },
                ),
                .String => try self.buildExternApiCall(
                    .bz_getStringField,
                    &[_]*l.Value{
                        // vm
                        self.vmConstant(),
                        // string
                        subject.?,
                        // member name
                        self.context.getContext().intType(64).constInt(
                            (try self.vm.gc.copyString(DotNode.cast(call_node.callee).?.identifier.lexeme)).toValue().val,
                            .False,
                        ),
                        // unbound
                        self.context.getContext().intType(1).constInt(0, .False),
                    },
                ),
                .Pattern => try self.buildExternApiCall(
                    .bz_getPatternField,
                    &[_]*l.Value{
                        // vm
                        self.vmConstant(),
                        // pattern
                        subject.?,
                        // member name
                        self.context.getContext().intType(64).constInt(
                            (try self.vm.gc.copyString(DotNode.cast(call_node.callee).?.identifier.lexeme)).toValue().val,
                            .False,
                        ),
                        // unbound
                        self.context.getContext().intType(1).constInt(0, .False),
                    },
                ),
                .Fiber => try self.buildExternApiCall(
                    .bz_getFiberField,
                    &[_]*l.Value{
                        // vm
                        self.vmConstant(),
                        // fiber
                        subject.?,
                        // member name
                        self.context.getContext().intType(64).constInt(
                            (try self.vm.gc.copyString(DotNode.cast(call_node.callee).?.identifier.lexeme)).toValue().val,
                            .False,
                        ),
                        // unbound
                        self.context.getContext().intType(1).constInt(0, .False),
                    },
                ),
                .List => try self.buildExternApiCall(
                    .bz_getListField,
                    &[_]*l.Value{
                        // vm
                        self.vmConstant(),
                        // list
                        subject.?,
                        // member name
                        self.context.getContext().intType(64).constInt(
                            (try self.vm.gc.copyString(DotNode.cast(call_node.callee).?.identifier.lexeme)).toValue().val,
                            .False,
                        ),
                        // unbound
                        self.context.getContext().intType(1).constInt(0, .False),
                    },
                ),
                .Map => try self.buildExternApiCall(
                    .bz_getMapField,
                    &[_]*l.Value{
                        // vm
                        self.vmConstant(),
                        // map
                        subject.?,
                        // member name
                        self.context.getContext().intType(64).constInt(
                            (try self.vm.gc.copyString(DotNode.cast(call_node.callee).?.identifier.lexeme)).toValue().val,
                            .False,
                        ),
                        // unbound
                        self.context.getContext().intType(1).constInt(0, .False),
                    },
                ),
                else => unreachable,
            })
        else
            (try self.generateNode(call_node.callee)).?;

        const callee_type = switch (call_node.callee.node_type) {
            .Dot => DotNode.cast(call_node.callee).?.member_type_def,
            else => call_node.callee.type_def,
        };

        const function_type_def: *ObjTypeDef = try callee_type.?.populateGenerics(
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

        // Push args on the stack

        // if invoked, first arg is `this`
        if (invoked_on != null) {
            _ = try self.buildPush(subject.?);
        } else {
            _ = try self.buildPush(
                self.context.getContext().intType(64).constInt(
                    Value.Void.val,
                    .False,
                ),
            );
        }

        var it = call_node.arguments.iterator();
        while (it.next()) |kv| {
            _ = try self.buildPush((try self.generateNode(kv.value_ptr.*)).?);
        }

        var new_ctx: *l.Value = self.state.?.current.?.function.?.getParam(0);
        if (function_type != .Extern) {
            new_ctx = self.state.?.builder.buildAlloca(
                try self.lowerExternApi(.nativectx),
                "new_ctx",
            );

            callee = try self.buildExternApiCall(
                .bz_context,
                &[_]*l.Value{
                    self.state.?.current.?.function.?.getParam(0),
                    callee,
                    new_ctx,
                },
            );
        } else { // If extern, extract pointer to its raw function
            // TODO: declare it in LLVM and call that?
            callee = try self.buildExternApiCall(
                .bz_valueToExternNativeFn,
                &[_]*l.Value{callee},
            );
        }

        // Regular function, just call it
        const result = self.state.?.builder.buildCall(
            try self.lowerType(function_type_def),
            callee,
            &[_]*l.Value{new_ctx},
            1,
            "",
        );

        if (function_type == .Extern) {
            return try self.generateHandleExternReturn(
                function_type_def.resolved_type.?.Function.return_type.def_type != .Void,
                result,
                call_node.arguments.count(),
            );
        }

        return result;
    }

    // Handle Extern call like VM.callNative does
    fn generateHandleExternReturn(self: *Self, should_return: bool, return_code: *l.Value, arg_count: usize) !*l.Value {
        const has_error = self.state.?.builder.buildICmp(
            .EQ,
            return_code,
            self.context.getContext().intType(64).constInt(
                @bitCast(c_ulonglong, @as(i64, -1)),
                .True,
            ),
            "has_error",
        );

        const no_error_block = self.context.getContext().createBasicBlock("no_error");
        const error_block = self.context.getContext().createBasicBlock("error_block");

        _ = self.state.?.builder.buildCondBr(
            has_error,
            error_block,
            no_error_block,
        );

        self.state.?.current.?.function.?.appendExistingBasicBlock(error_block);
        self.state.?.builder.positionBuilderAtEnd(error_block);
        self.state.?.current.?.block = error_block;

        // if result == -1, handle error
        // TODO
        _ = self.state.?.builder.buildUnreachable();

        // If result == 1 or 0, reset stack
        self.state.?.current.?.function.?.appendExistingBasicBlock(no_error_block);
        self.state.?.builder.positionBuilderAtEnd(no_error_block);
        self.state.?.current.?.block = no_error_block;

        const result = if (should_return)
            try self.buildPop()
        else
            self.context.getContext().intType(64).constInt(Value.Void.val, .False);

        const stack_top_field_ptr = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.nativectx),
            self.state.?.current.?.function.?.getParam(0),
            4,
            "stack_top_field_ptr",
        );

        const stack_top_ptr = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0).pointerType(0),
            stack_top_field_ptr,
            "stack_top_ptr",
        );

        const stack_top = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0),
            stack_top_ptr,
            "stack_top",
        );

        // Reset stack
        _ = self.state.?.builder.buildStore(
            // Get element one alignment after stack_top
            self.state.?.builder.buildInBoundsGEP(
                try self.lowerExternApi(.value),
                stack_top,
                &[_]*l.Value{
                    self.context.getContext().intType(64).constInt(
                        @bitCast(c_ulonglong, -@intCast(i64, arg_count) - 1),
                        .True,
                    ),
                },
                1,
                "new_top",
            ),
            stack_top_ptr,
        );

        return result;
    }

    fn generateReturn(self: *Self, return_node: *ReturnNode) VM.Error!?*l.Value {
        if (return_node.unconditional) {
            self.state.?.current.?.return_emitted = true;
        }

        return try self.buildReturn(
            if (return_node.value) |value|
                (try self.generateNode(value)).?
            else
                (try self.lowerExternApi(.value)).constInt(Value.Void.val, .False),
        );
    }

    fn generateIf(self: *Self, if_node: *IfNode) VM.Error!?*l.Value {
        const constant_condition = if (if_node.condition.isConstant(if_node.condition) and if_node.unwrapped_identifier == null and if_node.casted_type == null)
            if_node.condition.toValue(if_node.condition, self.vm.gc) catch unreachable
        else
            null;

        // Generate condition
        var condition_value = if (constant_condition == null) (try self.generateNode(if_node.condition)).? else null;
        var condition: *l.Value = undefined;

        // Is it `if (opt -> unwrapped)`?
        if (if_node.unwrapped_identifier != null) {
            condition = self.state.?.builder.buildNot(
                self.unwrap(
                    .Bool,
                    // FIXME: no need to call bz_valueEqual to compare with Value.Null.val
                    try self.buildExternApiCall(
                        .bz_valueEqual,
                        &[_]*l.Value{
                            condition_value.?,
                            (try ExternApi.value.lower(self.context.getContext())).constInt(
                                Value.Null.val,
                                .False,
                            ),
                        },
                    ),
                ),
                "",
            );
        } else if (if_node.casted_type) |casted_type| {
            condition = self.unwrap(
                .Bool,
                try self.buildExternApiCall(
                    .bz_valueIs,
                    &[_]*l.Value{
                        condition_value.?,
                        (try ExternApi.value.lower(self.context.getContext())).constInt(
                            casted_type.toValue().val,
                            .False,
                        ),
                    },
                ),
            );
        } else if (constant_condition == null) {
            condition = self.unwrap(.Bool, condition_value.?);
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

        if (constant_condition != null) {
            _ = self.state.?.builder.buildBr(
                if (constant_condition.?.boolean())
                    then_block
                else if (if_node.else_branch != null)
                    else_block.?
                else
                    out_block,
            );
        } else {
            _ = self.state.?.builder.buildCondBr(
                condition,
                then_block,
                if (if_node.else_branch != null) else_block.? else out_block,
            );
        }

        if (constant_condition == null or constant_condition.?.boolean()) {
            self.state.?.current.?.function.?.appendExistingBasicBlock(then_block);
            self.state.?.builder.positionBuilderAtEnd(then_block);
            self.state.?.current.?.block = then_block;

            // Push unwrapped value as local of the then block
            if (if_node.unwrapped_identifier != null or if_node.casted_type != null) {
                _ = try self.buildPush(condition_value.?);
            }

            _ = try self.generateNode(if_node.body);

            // Jump after else
            self.buildBr(out_block);
        }

        if (constant_condition == null or !constant_condition.?.boolean()) {
            if (if_node.else_branch) |else_branch| {
                self.state.?.current.?.function.?.appendExistingBasicBlock(else_block.?);
                self.state.?.builder.positionBuilderAtEnd(else_block.?);
                self.state.?.current.?.block = else_block.?;

                _ = try self.generateNode(else_branch);

                // Jump after else
                self.buildBr(out_block);
            }
        }

        self.state.?.current.?.function.?.appendExistingBasicBlock(out_block);

        // Continue writing after the if else
        self.state.?.current.?.block = out_block;
        self.state.?.builder.positionBuilderAtEnd(out_block);

        // Statement don't return values
        return null;
    }

    fn generateInlineIf(self: *Self, inline_if_node: *InlineIfNode) VM.Error!?*l.Value {
        const constant_condition = if (inline_if_node.condition.isConstant(inline_if_node.condition))
            inline_if_node.condition.toValue(inline_if_node.condition, self.vm.gc) catch unreachable
        else
            null;

        // Generate condition
        const condition = (try self.generateNode(inline_if_node.condition)).?;

        // Alloca to hold resolved expression
        const resolved = self.state.?.builder.buildAlloca(
            (try ExternApi.value.lower(self.context.getContext())).pointerType(0),
            "inline_if_expression",
        );

        // Continuation block
        var out_block = self.context.getContext().createBasicBlock("out");

        // If block
        var then_block = self.context.getContext().createBasicBlock("then");

        // Else block
        var else_block = self.context.getContext().createBasicBlock("else");

        if (constant_condition != null) {
            _ = self.state.?.builder.buildBr(
                if (constant_condition.?.boolean())
                    then_block
                else
                    else_block,
            );
        } else {
            _ = self.state.?.builder.buildCondBr(
                self.unwrap(.Bool, condition),
                then_block,
                else_block,
            );
        }

        if (constant_condition == null or constant_condition.?.boolean()) {
            self.state.?.current.?.function.?.appendExistingBasicBlock(then_block);
            self.state.?.builder.positionBuilderAtEnd(then_block);
            self.state.?.current.?.block = then_block;

            _ = self.state.?.builder.buildStore(
                (try self.generateNode(inline_if_node.body)).?,
                resolved,
            );

            // Jump after else
            self.buildBr(out_block);
        }

        if (constant_condition == null or !constant_condition.?.boolean()) {
            self.state.?.current.?.function.?.appendExistingBasicBlock(else_block);
            self.state.?.builder.positionBuilderAtEnd(else_block);
            self.state.?.current.?.block = else_block;

            _ = self.state.?.builder.buildStore(
                (try self.generateNode(inline_if_node.else_branch)).?,
                resolved,
            );

            // Jump after else
            self.buildBr(out_block);
        }

        self.state.?.current.?.function.?.appendExistingBasicBlock(out_block);

        // Continue writing after the if else
        self.state.?.current.?.block = out_block;
        self.state.?.builder.positionBuilderAtEnd(out_block);

        // Return resolved value
        return self.state.?.builder.buildLoad(
            try ExternApi.value.lower(self.context.getContext()),
            resolved,
            "resolved_inline_if",
        );
    }

    fn generateBinary(self: *Self, binary_node: *BinaryNode) VM.Error!?*l.Value {
        const left_type_def = binary_node.left.type_def.?.def_type;
        const right_type_def = binary_node.right.type_def.?.def_type;

        return switch (binary_node.operator) {
            .Ampersand => self.wrap(
                .Integer,
                self.state.?.builder.buildAnd(
                    self.unwrap(.Integer, (try self.generateNode(binary_node.left)).?),
                    self.unwrap(.Integer, (try self.generateNode(binary_node.right)).?),
                    "",
                ),
            ),
            .Bor => self.wrap(
                .Integer,
                self.state.?.builder.buildOr(
                    self.unwrap(.Integer, (try self.generateNode(binary_node.left)).?),
                    self.unwrap(.Integer, (try self.generateNode(binary_node.right)).?),
                    "",
                ),
            ),
            .Xor => self.wrap(
                .Integer,
                self.state.?.builder.buildXor(
                    self.unwrap(.Integer, (try self.generateNode(binary_node.left)).?),
                    self.unwrap(.Integer, (try self.generateNode(binary_node.right)).?),
                    "",
                ),
            ),
            .ShiftLeft => self.wrap(
                .Integer,
                self.state.?.builder.buildShl(
                    self.unwrap(.Integer, (try self.generateNode(binary_node.left)).?),
                    self.unwrap(.Integer, (try self.generateNode(binary_node.right)).?),
                    "",
                ),
            ),
            // ashr (https://llvm.org/docs/LangRef.html#ashr-instruction) ?
            .ShiftRight => self.wrap(
                .Integer,
                self.state.?.builder.buildLShr(
                    self.unwrap(.Integer, (try self.generateNode(binary_node.left)).?),
                    self.unwrap(.Integer, (try self.generateNode(binary_node.right)).?),
                    "",
                ),
            ),
            .QuestionQuestion, .And, .Or => cond: {
                const value = self.state.?.builder.buildAlloca(try self.lowerExternApi(.value), "");
                const left = (try self.generateNode(binary_node.left)).?;

                _ = self.state.?.builder.buildStore(
                    left,
                    value,
                );

                const condition = self.state.?.builder.buildICmp(
                    .EQ,
                    left,
                    (try self.lowerExternApi(.value)).constInt(
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

                _ = self.state.?.builder.buildCondBr(
                    condition,
                    then_block,
                    out_block,
                );

                self.state.?.current.?.function.?.appendExistingBasicBlock(then_block);
                self.state.?.builder.positionBuilderAtEnd(then_block);
                self.state.?.current.?.block = then_block;

                _ = self.state.?.builder.buildStore(
                    (try self.generateNode(binary_node.right)).?,
                    value,
                );

                // Continue
                self.buildBr(out_block);

                // Continue writing after the if else
                self.state.?.current.?.function.?.appendExistingBasicBlock(out_block);
                self.state.?.current.?.block = out_block;
                self.state.?.builder.positionBuilderAtEnd(out_block);

                break :cond self.state.?.builder.buildLoad(
                    try self.lowerExternApi(.value),
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
                    .EqualEqual => {
                        return try self.buildExternApiCall(
                            .bz_valueEqual,
                            &[_]*l.Value{
                                left.?,
                                right.?,
                            },
                        );
                    },
                    .BangEqual => {
                        return self.wrap(
                            .Bool,
                            self.state.?.builder.buildNot(
                                self.unwrap(
                                    .Bool,
                                    try self.buildExternApiCall(
                                        .bz_valueEqual,
                                        &[_]*l.Value{
                                            left.?,
                                            right.?,
                                        },
                                    ),
                                ),
                                "",
                            ),
                        );
                    },
                    .Greater, .Less, .GreaterEqual, .LessEqual => {
                        if (left_f != null or right_f != null) {
                            return self.wrap(
                                .Bool,
                                self.state.?.builder.buildFCmp(
                                    switch (binary_node.operator) {
                                        .Greater => .OGT,
                                        .Less => .OLT,
                                        .GreaterEqual => .OGE,
                                        .LessEqual => .OLE,
                                        .BangEqual => .ONE,
                                        .EqualEqual => .OEQ,
                                        else => unreachable,
                                    },
                                    if (left_i) |i| self.state.?.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else left_f.?,
                                    if (right_i) |i| self.state.?.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else right_f.?,
                                    "",
                                ),
                            );
                        }

                        break :bin self.wrap(
                            .Bool,
                            self.state.?.builder.buildICmp(
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
                                        self.state.?.builder.buildFAdd(
                                            if (left_i) |i| self.state.?.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else left_f.?,
                                            if (right_i) |i| self.state.?.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else right_f.?,
                                            "",
                                        ),
                                    );
                                }

                                break :bin self.wrap(
                                    .Integer,
                                    self.state.?.builder.buildAdd(
                                        left_i.?,
                                        right_i.?,
                                        "",
                                    ),
                                );
                            },
                            .String => break :bin try self.buildExternApiCall(
                                .bz_objStringConcat,
                                &[_]*l.Value{
                                    self.vmConstant(),
                                    self.wrap(.String, left_s.?),
                                    self.wrap(.String, right_s.?),
                                },
                            ),
                            .List => break :bin try self.buildExternApiCall(
                                .bz_listConcat,
                                &[_]*l.Value{
                                    self.vmConstant(),
                                    left.?,
                                    right.?,
                                },
                            ),
                            .Map => break :bin try self.buildExternApiCall(
                                .bz_mapConcat,
                                &[_]*l.Value{
                                    self.vmConstant(),
                                    left.?,
                                    right.?,
                                },
                            ),
                            else => unreachable,
                        }
                    },
                    .Minus => {
                        if (left_f != null or right_f != null) {
                            break :bin self.wrap(
                                .Float,
                                self.state.?.builder.buildFSub(
                                    if (left_i) |i| self.state.?.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else left_f.?,
                                    if (right_i) |i| self.state.?.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else right_f.?,
                                    "",
                                ),
                            );
                        }

                        break :bin self.wrap(
                            .Integer,
                            self.state.?.builder.buildSub(
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
                                self.state.?.builder.buildFMul(
                                    if (left_i) |i| self.state.?.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else left_f.?,
                                    if (right_i) |i| self.state.?.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else right_f.?,
                                    "",
                                ),
                            );
                        }

                        break :bin self.wrap(
                            .Integer,
                            self.state.?.builder.buildMul(
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
                                self.state.?.builder.buildFDiv(
                                    if (left_i) |i| self.state.?.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else left_f.?,
                                    if (right_i) |i| self.state.?.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else right_f.?,
                                    "",
                                ),
                            );
                        }

                        // Div result is always float
                        break :bin self.wrap(
                            .Integer,
                            self.state.?.builder.buildSDiv(
                                left_i orelse left_f.?,
                                right_i orelse right_f.?,
                                "",
                            ),
                        );
                    },
                    .Percent => {
                        if (left_f != null or right_f != null) {
                            break :bin self.wrap(
                                .Float,
                                self.state.?.builder.buildFRem(
                                    if (left_i) |i| self.state.?.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else left_f.?,
                                    if (right_i) |i| self.state.?.builder.buildSIToFP(i, self.context.getContext().doubleType(), "") else right_f.?,
                                    "",
                                ),
                            );
                        }

                        break :bin self.wrap(
                            .Integer,
                            self.state.?.builder.buildSRem(
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
        const previous_out_block = self.state.?.current.?.break_block;
        self.state.?.current.?.break_block = out_block;
        const previous_continue_block = self.state.?.current.?.continue_block;
        self.state.?.current.?.continue_block = cond_block;

        self.buildBr(cond_block);

        self.state.?.current.?.function.?.appendExistingBasicBlock(cond_block);
        self.state.?.builder.positionBuilderAtEnd(cond_block);
        self.state.?.current.?.block = cond_block;

        const condition = self.state.?.builder.buildICmp(
            .EQ,
            (try self.generateNode(while_node.condition)).?,
            (try self.lowerExternApi(.value)).constInt(
                Value.True.val,
                .False,
            ),
            "cond",
        );

        _ = self.state.?.builder.buildCondBr(
            condition,
            loop_block,
            out_block,
        );

        self.state.?.current.?.function.?.appendExistingBasicBlock(loop_block);
        self.state.?.builder.positionBuilderAtEnd(loop_block);
        self.state.?.current.?.block = loop_block;

        _ = try self.generateNode(while_node.block);

        self.buildBr(cond_block);

        self.state.?.current.?.function.?.appendExistingBasicBlock(out_block);
        self.state.?.builder.positionBuilderAtEnd(out_block);
        self.state.?.current.?.block = out_block;

        self.state.?.current.?.break_block = previous_out_block;
        self.state.?.current.?.continue_block = previous_continue_block;

        return null;
    }

    fn generateDoUntil(self: *Self, do_until_node: *DoUntilNode) VM.Error!?*l.Value {
        const loop_block = self.context.getContext().createBasicBlock("loop");
        const out_block = self.context.getContext().createBasicBlock("out");
        const cond_block = self.context.getContext().createBasicBlock("cond");
        const previous_out_block = self.state.?.current.?.break_block;
        self.state.?.current.?.break_block = out_block;
        const previous_continue_block = self.state.?.current.?.continue_block;
        self.state.?.current.?.continue_block = cond_block;

        self.buildBr(loop_block);

        self.state.?.current.?.function.?.appendExistingBasicBlock(loop_block);
        self.state.?.builder.positionBuilderAtEnd(loop_block);
        self.state.?.current.?.block = loop_block;

        _ = try self.generateNode(do_until_node.block);

        self.buildBr(cond_block);

        self.state.?.current.?.function.?.appendExistingBasicBlock(cond_block);
        self.state.?.builder.positionBuilderAtEnd(cond_block);
        self.state.?.current.?.block = cond_block;

        const condition = self.state.?.builder.buildICmp(
            .EQ,
            (try self.generateNode(do_until_node.condition)).?,
            (try self.lowerExternApi(.value)).constInt(
                Value.True.val,
                .False,
            ),
            "cond",
        );

        _ = self.state.?.builder.buildCondBr(
            condition,
            out_block,
            loop_block,
        );

        self.state.?.current.?.function.?.appendExistingBasicBlock(out_block);
        self.state.?.builder.positionBuilderAtEnd(out_block);
        self.state.?.current.?.block = out_block;

        self.state.?.current.?.break_block = previous_out_block;
        self.state.?.current.?.continue_block = previous_continue_block;

        return null;
    }

    fn generateFor(self: *Self, for_node: *ForNode) VM.Error!?*l.Value {
        const cond_block = self.context.getContext().createBasicBlock("cond");
        const loop_block = self.context.getContext().createBasicBlock("loop");
        const out_block = self.context.getContext().createBasicBlock("out");
        const previous_out_block = self.state.?.current.?.break_block;
        self.state.?.current.?.break_block = out_block;
        const previous_continue_block = self.state.?.current.?.continue_block;
        self.state.?.current.?.continue_block = cond_block;

        // Init expressions
        for (for_node.init_declarations.items) |expr| {
            _ = try self.generateNode(&expr.node);
        }

        // Jump to condition
        self.buildBr(cond_block);

        // Condition
        self.state.?.current.?.function.?.appendExistingBasicBlock(cond_block);
        self.state.?.builder.positionBuilderAtEnd(cond_block);
        self.state.?.current.?.block = cond_block;

        const condition = self.state.?.builder.buildICmp(
            .EQ,
            (try self.generateNode(for_node.condition)).?,
            (try self.lowerExternApi(.value)).constInt(
                Value.True.val,
                .False,
            ),
            "cond",
        );

        _ = self.state.?.builder.buildCondBr(
            condition,
            loop_block,
            out_block,
        );

        // Body
        self.state.?.current.?.function.?.appendExistingBasicBlock(loop_block);
        self.state.?.builder.positionBuilderAtEnd(loop_block);
        self.state.?.current.?.block = loop_block;

        _ = try self.generateNode(for_node.body);

        // Post loop
        for (for_node.post_loop.items) |expr| {
            _ = try self.generateNode(expr);
        }

        self.buildBr(cond_block);

        self.state.?.current.?.function.?.appendExistingBasicBlock(out_block);
        self.state.?.builder.positionBuilderAtEnd(out_block);
        self.state.?.current.?.block = out_block;

        self.state.?.current.?.break_block = previous_out_block;
        self.state.?.current.?.continue_block = previous_continue_block;

        return null;
    }

    fn generateBreak(self: *Self) VM.Error!?*l.Value {
        self.buildBr(self.state.?.current.?.break_block.?);

        return null;
    }

    fn generateContinue(self: *Self) VM.Error!?*l.Value {
        self.buildBr(self.state.?.current.?.continue_block.?);

        return null;
    }

    fn generateList(self: *Self, list_node: *ListNode) VM.Error!?*l.Value {
        const item_type = (try self.lowerExternApi(.value)).constInt(
            list_node.node.type_def.?.resolved_type.?.List.item_type.toValue().val,
            .False,
        );

        // FIXME: should need the whole list type def not just item type
        const list = try self.buildExternApiCall(
            .bz_newList,
            &[_]*l.Value{
                self.vmConstant(),
                item_type,
            },
        );

        for (list_node.items) |item| {
            _ = try self.buildExternApiCall(
                .bz_listAppend,
                &[_]*l.Value{
                    self.vmConstant(),
                    list,
                    (try self.generateNode(item)).?,
                },
            );
        }

        return list;
    }

    fn generateMap(self: *Self, map_node: *MapNode) VM.Error!?*l.Value {
        const map = try self.buildExternApiCall(
            .bz_newMap,
            &[_]*l.Value{
                self.vmConstant(),
                (try self.lowerExternApi(.value)).constInt(
                    map_node.node.type_def.?.toValue().val,
                    .False,
                ),
            },
        );

        for (map_node.keys) |key, index| {
            _ = try self.buildExternApiCall(
                .bz_mapSet,
                &[_]*l.Value{
                    self.vmConstant(),
                    map,
                    (try self.generateNode(key)).?,
                    (try self.generateNode(map_node.values[index])).?,
                },
            );
        }

        return map;
    }

    fn generateDot(self: *Self, dot_node: *DotNode) VM.Error!?*l.Value {
        const callee_type = dot_node.callee.type_def.?;

        // TODO: not super readable...
        return switch (callee_type.def_type) {
            .Fiber => if (dot_node.call) |call|
                try self.generateCall(call)
            else
                try self.buildExternApiCall(
                    .bz_getFiberField,
                    &[_]*l.Value{
                        self.vmConstant(),
                        (try self.generateNode(dot_node.callee)).?,
                        self.context.getContext().intType(64).constInt(
                            (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val,
                            .False,
                        ),
                        self.context.getContext().intType(1).constInt(1, .False),
                    },
                ),

            .Pattern => if (dot_node.call) |call|
                try self.generateCall(call)
            else
                try self.buildExternApiCall(
                    .bz_getPatternField,
                    &[_]*l.Value{
                        self.vmConstant(),
                        (try self.generateNode(dot_node.callee)).?,
                        self.context.getContext().intType(64).constInt(
                            (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val,
                            .False,
                        ),
                        self.context.getContext().intType(1).constInt(1, .False),
                    },
                ),

            .String => if (dot_node.call) |call|
                try self.generateCall(call)
            else
                try self.buildExternApiCall(
                    .bz_getStringField,
                    &[_]*l.Value{
                        self.vmConstant(),
                        (try self.generateNode(dot_node.callee)).?,
                        self.context.getContext().intType(64).constInt(
                            (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val,
                            .False,
                        ),
                        self.context.getContext().intType(1).constInt(1, .False),
                    },
                ),

            .Object => obj: {
                if (dot_node.call) |call| {
                    break :obj try self.generateCall(call);
                } else if (dot_node.value) |value| {
                    break :obj try self.buildExternApiCall(
                        .bz_setObjectField,
                        &[_]*l.Value{
                            self.vmConstant(),
                            (try self.generateNode(dot_node.callee)).?,
                            self.context.getContext().intType(64).constInt(
                                (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val,
                                .False,
                            ),
                            (try self.generateNode(value)).?,
                        },
                    );
                } else {
                    break :obj try self.buildExternApiCall(
                        .bz_getObjectField,
                        &[_]*l.Value{
                            (try self.generateNode(dot_node.callee)).?,
                            self.context.getContext().intType(64).constInt(
                                (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val,
                                .False,
                            ),
                        },
                    );
                }
            },
            .ObjectInstance => inst: {
                if (dot_node.call) |call| {
                    break :inst try self.generateCall(call);
                } else if (dot_node.value) |value| {
                    break :inst try self.buildExternApiCall(
                        .bz_setInstanceField,
                        &[_]*l.Value{
                            self.vmConstant(),
                            (try self.generateNode(dot_node.callee)).?,
                            self.context.getContext().intType(64).constInt(
                                (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val,
                                .False,
                            ),
                            (try self.generateNode(value)).?,
                        },
                    );
                } else {
                    break :inst try self.buildExternApiCall(
                        .bz_getInstanceField,
                        &[_]*l.Value{
                            self.vmConstant(),
                            (try self.generateNode(dot_node.callee)).?,
                            self.context.getContext().intType(64).constInt(
                                (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val,
                                .False,
                            ),
                            self.context.getContext().intType(1).constInt(1, .False),
                        },
                    );
                }
            },
            .ProtocolInstance => if (dot_node.call) |call|
                try self.generateCall(call)
            else
                try self.buildExternApiCall(
                    .bz_getInstanceField,
                    &[_]*l.Value{
                        self.vmConstant(),
                        (try self.generateNode(dot_node.callee)).?,
                        self.context.getContext().intType(64).constInt(
                            (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val,
                            .False,
                        ),
                        self.context.getContext().intType(1).constInt(1, .False),
                    },
                ),
            .Enum => try self.buildExternApiCall(
                .bz_getEnumCase,
                &[_]*l.Value{
                    self.vmConstant(),
                    (try self.generateNode(dot_node.callee)).?,
                    self.context.getContext().intType(64).constInt(
                        (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val,
                        .False,
                    ),
                },
            ),
            .EnumInstance => try self.buildExternApiCall(
                .bz_getEnumCaseValue,
                &[_]*l.Value{
                    (try self.generateNode(dot_node.callee)).?,
                },
            ),
            .List => if (dot_node.call) |call|
                try self.generateCall(call)
            else
                try self.buildExternApiCall(
                    .bz_getListField,
                    &[_]*l.Value{
                        self.vmConstant(),
                        (try self.generateNode(dot_node.callee)).?,
                        self.context.getContext().intType(64).constInt(
                            (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val,
                            .False,
                        ),
                        self.context.getContext().intType(1).constInt(1, .False),
                    },
                ),
            .Map => if (dot_node.call) |call|
                try self.generateCall(call)
            else
                try self.buildExternApiCall(
                    .bz_getMapField,
                    &[_]*l.Value{
                        self.vmConstant(),
                        (try self.generateNode(dot_node.callee)).?,
                        self.context.getContext().intType(64).constInt(
                            (try self.vm.gc.copyString(dot_node.identifier.lexeme)).toValue().val,
                            .False,
                        ),
                        self.context.getContext().intType(1).constInt(1, .False),
                    },
                ),
            else => unreachable,
        };
    }

    fn generateSubscript(self: *Self, subscript_node: *SubscriptNode) VM.Error!?*l.Value {
        const subscripted = (try self.generateNode(subscript_node.subscripted)).?;
        const index = (try self.generateNode(subscript_node.index)).?;
        const value = if (subscript_node.value) |val| (try self.generateNode(val)).? else null;

        return switch (subscript_node.subscripted.type_def.?.def_type) {
            .List => list: {
                if (value) |val| {
                    _ = try self.buildExternApiCall(
                        .bz_listSet,
                        &[_]*l.Value{
                            self.vmConstant(),
                            subscripted,
                            self.unwrap(.Integer, index),
                            val,
                        },
                    );

                    break :list subscripted;
                } else {
                    break :list try self.buildExternApiCall(
                        .bz_listGet,
                        &[_]*l.Value{
                            subscripted,
                            self.unwrap(.Integer, index),
                        },
                    );
                }
            },
            .String => try self.buildExternApiCall(
                .bz_objStringSubscript,
                &[_]*l.Value{
                    self.vmConstant(),
                    subscripted,
                    index,
                },
            ),
            .Map => map: {
                if (value) |val| {
                    _ = try self.buildExternApiCall(
                        .bz_mapSet,
                        &[_]*l.Value{
                            self.vmConstant(),
                            subscripted,
                            index,
                            val,
                        },
                    );

                    break :map subscripted;
                } else {
                    break :map try self.buildExternApiCall(
                        .bz_mapGet,
                        &[_]*l.Value{
                            subscripted,
                            index,
                        },
                    );
                }
            },
            else => unreachable,
        };
    }

    fn generateIs(self: *Self, is_node: *IsNode) VM.Error!?*l.Value {
        return try self.buildExternApiCall(
            .bz_valueIs,
            &[_]*l.Value{
                (try self.generateNode(is_node.left)).?,
                (try self.lowerExternApi(.value)).constInt(
                    is_node.constant.val,
                    .False,
                ),
            },
        );
    }

    fn generateTry(self: *Self, try_node: *TryNode) VM.Error!?*l.Value {
        const body_block = self.context.getContext().createBasicBlock("body");
        const raise_block = self.context.getContext().createBasicBlock("raise");
        const out_block = self.context.getContext().createBasicBlock("out");
        const catch_block = self.context.getContext().createBasicBlock("catch");
        var clause_blocks = std.ArrayList(*l.BasicBlock).init(self.vm.gc.allocator);
        defer clause_blocks.deinit();

        for (try_node.clauses.keys()) |_| {
            try clause_blocks.append(
                self.context.getContext().createBasicBlock("clause"),
            );
        }
        const unconditional_block = if (try_node.unconditional_clause != null)
            self.context.getContext().createBasicBlock("unconditional")
        else
            null;

        // Remember stack top
        const top_alloca = self.state.?.builder.buildAlloca(
            (try ExternApi.value.lower(self.context.getContext())).pointerType(0),
            "try_top",
        );

        const stack_top_field_ptr = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.nativectx),
            self.state.?.current.?.function.?.getParam(0),
            4,
            "stack_top_field_ptr",
        );

        const stack_top_ptr = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0).pointerType(0),
            stack_top_field_ptr,
            "stack_top_ptr",
        );

        _ = self.state.?.builder.buildStore(
            stack_top_ptr,
            top_alloca,
        );

        // Set it as current jump env
        const try_ctx = try self.buildExternApiCall(
            .bz_setTryCtx,
            &[_]*l.Value{
                self.vmConstant(),
            },
        );

        const env = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.tryctx),
            try_ctx,
            1,
            "env",
        );

        // setjmp
        const status = try self.buildExternApiCall(
            .setjmp,
            &[_]*l.Value{env},
        );

        // If status is 0, go to body, else go to catch clauses
        const has_error = self.state.?.builder.buildICmp(
            .EQ,
            status,
            self.context.getContext().intType(@sizeOf(c_int)).constInt(1, .False),
            "has_error",
        );

        _ = self.state.?.builder.buildCondBr(
            has_error,
            catch_block,
            body_block,
        );

        self.state.?.current.?.function.?.appendExistingBasicBlock(body_block);
        self.state.?.builder.positionBuilderAtEnd(body_block);
        self.state.?.current.?.block = body_block;

        _ = try self.generateNode(try_node.body);

        _ = self.state.?.builder.buildBr(out_block);

        self.state.?.current.?.function.?.appendExistingBasicBlock(catch_block);
        self.state.?.builder.positionBuilderAtEnd(catch_block);
        self.state.?.current.?.block = catch_block;

        const payload = try self.buildPop();

        // Get stack top as it was before try block
        const try_stack_top_ptr = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0).pointerType(0),
            top_alloca,
            "try_stack_top_ptr",
        );

        // Close upvalues up to it
        _ = try self.buildExternApiCall(
            .bz_closeUpValues,
            &[_]*l.Value{
                self.vmConstant(),
                try_stack_top_ptr,
            },
        );

        // Restore stack top as it was before the try block
        _ = self.state.?.builder.buildStore(
            try_stack_top_ptr,
            stack_top_field_ptr,
        );

        // Put error back on stack
        _ = try self.buildPush(payload);

        _ = self.state.?.builder.buildBr(
            if (clause_blocks.items.len > 0) clause_blocks.items[0] else unconditional_block.?,
        );

        for (try_node.clauses.keys()) |type_def, index| {
            const block = clause_blocks.items[index];
            const clause = try_node.clauses.get(type_def).?;

            const clause_body_block = self.context.getContext().createBasicBlock("clause_body");

            self.state.?.current.?.function.?.appendExistingBasicBlock(block);
            self.state.?.builder.positionBuilderAtEnd(block);
            self.state.?.current.?.block = block;

            // Get error payload from stack
            const err_payload = try self.buildPeek(0);

            // Push payload
            _ = try self.buildPush(err_payload);

            const matches = self.unwrap(
                .Bool,
                try self.buildExternApiCall(
                    .bz_valueIs,
                    &[_]*l.Value{
                        err_payload,
                        (try self.lowerExternApi(.value)).constInt(
                            type_def.toValue().val,
                            .False,
                        ),
                    },
                ),
            );

            const cond = self.state.?.builder.buildICmp(
                .EQ,
                matches,
                self.context.getContext().intType(1).constInt(1, .False),
                "",
            );

            // If payload is of expected type jump to clause body otherwise jump to next catch clause if none jump to error propagation
            _ = self.state.?.builder.buildCondBr(
                cond,
                clause_body_block,
                if (index < try_node.clauses.keys().len - 1)
                    clause_blocks.items[index + 1]
                else if (unconditional_block) |unconditional|
                    unconditional
                else
                    raise_block,
            );

            self.state.?.current.?.function.?.appendExistingBasicBlock(clause_body_block);
            self.state.?.builder.positionBuilderAtEnd(clause_body_block);
            self.state.?.current.?.block = clause_body_block;

            _ = try self.generateNode(clause);

            _ = self.state.?.builder.buildBr(out_block);
        }

        if (unconditional_block) |block| {
            self.state.?.current.?.function.?.appendExistingBasicBlock(block);
            self.state.?.builder.positionBuilderAtEnd(block);
            self.state.?.current.?.block = block;

            _ = try self.generateNode(try_node.unconditional_clause.?);

            _ = self.state.?.builder.buildBr(out_block);
        }

        self.state.?.current.?.function.?.appendExistingBasicBlock(raise_block);
        self.state.?.builder.positionBuilderAtEnd(raise_block);
        self.state.?.current.?.block = raise_block;

        // Unwind TryCtx
        _ = try self.buildExternApiCall(
            .bz_popTryCtx,
            &[_]*l.Value{
                self.vmConstant(),
            },
        );

        // Raise error again
        _ = try self.buildExternApiCall(
            .bz_rethrow,
            &[_]*l.Value{
                self.vmConstant(),
            },
        );

        // Should not get here but need a terminator
        _ = self.state.?.builder.buildUnreachable();

        self.state.?.current.?.function.?.appendExistingBasicBlock(out_block);
        self.state.?.builder.positionBuilderAtEnd(out_block);
        self.state.?.current.?.block = out_block;

        // Unwind TryCtx
        _ = try self.buildExternApiCall(
            .bz_popTryCtx,
            &[_]*l.Value{
                self.vmConstant(),
            },
        );

        return null;
    }

    fn generateThrow(self: *Self, throw_node: *ThrowNode) VM.Error!?*l.Value {
        _ = try self.buildExternApiCall(
            .bz_throw,
            &[_]*l.Value{
                self.vmConstant(),
                (try self.generateNode(throw_node.error_value)).?,
            },
        );

        _ = self.state.?.builder.buildUnreachable();

        return null;
    }

    fn generateUnwrap(self: *Self, unwrap_node: *UnwrapNode) VM.Error!?*l.Value {
        const next_expr_block = self.context.getContext().createBasicBlock("next_expr");

        const value = (try self.generateNode(unwrap_node.unwrapped)).?;

        // Remember that we need to had a terminator to this block that will jump at the end of the optionals chain
        if (self.state.?.opt_jump == null) {
            // Store the value on the stack, that spot will be overwritten with the final value of the optional chain
            const value_ptr = self.state.?.builder.buildAlloca(
                try self.lowerExternApi(.value),
                "opt",
            );

            self.state.?.opt_jump = .{
                .alloca = value_ptr,
                .current_blocks = std.ArrayList(*l.BasicBlock).init(self.vm.gc.allocator),
                .next_expr_blocks = std.ArrayList(*l.BasicBlock).init(self.vm.gc.allocator),
            };
        }

        _ = self.state.?.builder.buildStore(
            value,
            self.state.?.opt_jump.?.alloca,
        );

        try self.state.?.opt_jump.?.current_blocks.append(self.state.?.current.?.block.?);
        try self.state.?.opt_jump.?.next_expr_blocks.append(next_expr_block);

        self.state.?.current.?.function.?.appendExistingBasicBlock(next_expr_block);
        self.state.?.builder.positionBuilderAtEnd(next_expr_block);
        self.state.?.current.?.block = next_expr_block;

        return value;
    }

    fn generateObjectInit(self: *Self, object_init_node: *ObjectInitNode) VM.Error!?*l.Value {
        const object = if (object_init_node.object) |node|
            (try self.generateNode(node)).?
        else
            self.context.getContext().intType(64).constInt(Value.Null.val, .False);

        const typedef = if (object_init_node.object == null)
            self.context.getContext().intType(64).constInt(
                object_init_node.node.type_def.?.toValue().val,
                .False,
            )
        else
            self.context.getContext().intType(64).constInt(
                Value.Null.val,
                .False,
            );

        const instance = try self.buildExternApiCall(
            .bz_instance,
            &[_]*l.Value{
                self.vmConstant(),
                object,
                typedef,
            },
        );

        // push to prevent collection
        _ = try self.buildPush(instance);

        for (object_init_node.properties.keys()) |property_name| {
            const value = object_init_node.properties.get(property_name).?;

            _ = try self.buildExternApiCall(
                .bz_setInstanceField,
                &[_]*l.Value{
                    self.vmConstant(),
                    instance,
                    self.context.getContext().intType(64).constInt(
                        (try self.vm.gc.copyString(property_name)).toValue().val,
                        .False,
                    ),
                    (try self.generateNode(value)).?,
                },
            );
        }

        return try self.buildPop();
    }

    fn generateForceUnwrap(self: *Self, force_unwrap_node: *ForceUnwrapNode) VM.Error!?*l.Value {
        const expr = (try self.generateNode(force_unwrap_node.unwrapped)).?;

        const is_null = self.state.?.builder.buildICmp(
            .EQ,
            expr,
            self.context.getContext().intType(64).constInt(
                Value.Null.val,
                .False,
            ),
            "is_null",
        );

        var out_block = self.context.getContext().createBasicBlock("out");
        var then_block = self.context.getContext().createBasicBlock("then");

        _ = self.state.?.builder.buildCondBr(
            is_null,
            then_block,
            out_block,
        );

        self.state.?.current.?.function.?.appendExistingBasicBlock(then_block);
        self.state.?.builder.positionBuilderAtEnd(then_block);
        self.state.?.current.?.block = then_block;

        // TODO: throw or panic?
        _ = try self.buildExternApiCall(
            .bz_throw,
            &[_]*l.Value{
                self.vmConstant(), self.context.getContext().intType(64).constInt(
                    (try self.vm.gc.copyString("Force unwrapped optional is null")).toValue().val,
                    .False,
                ),
            },
        );

        _ = self.state.?.builder.buildUnreachable();

        self.state.?.current.?.function.?.appendExistingBasicBlock(out_block);
        self.state.?.builder.positionBuilderAtEnd(out_block);
        self.state.?.current.?.block = out_block;

        return expr;
    }

    fn generateUnary(self: *Self, unary_node: *UnaryNode) VM.Error!?*l.Value {
        const left = (try self.generateNode(unary_node.left)).?;

        return switch (unary_node.operator) {
            .Bnot => self.wrap(
                .Integer,
                self.state.?.builder.buildNot(
                    self.unwrap(.Integer, left),
                    "",
                ),
            ),
            .Bang => self.wrap(
                .Bool,
                self.state.?.builder.buildNot(
                    self.unwrap(.Bool, left),
                    "",
                ),
            ),
            .Minus => self.wrap(
                unary_node.left.type_def.?.def_type,
                if (unary_node.left.type_def.?.def_type == .Integer)
                    self.state.?.builder.buildNeg(
                        self.unwrap(unary_node.left.type_def.?.def_type, left),
                        "",
                    )
                else
                    self.state.?.builder.buildFNeg(
                        self.unwrap(unary_node.left.type_def.?.def_type, left),
                        "",
                    ),
            ),
            else => unreachable,
        };
    }

    fn generatePattern(self: *Self, pattern_node: *PatternNode) VM.Error!?*l.Value {
        return self.context.getContext().intType(64).constInt(
            pattern_node.constant.toValue().val,
            .False,
        );
    }

    fn generateForEach(self: *Self, foreach_node: *ForEachNode) VM.Error!?*l.Value {
        // If iteratble is empty constant, skip the node
        if (foreach_node.iterable.isConstant(foreach_node.iterable)) {
            const iterable = (foreach_node.iterable.toValue(foreach_node.iterable, self.vm.gc) catch @panic("Could not compile foreach loop")).obj();

            if (switch (iterable.obj_type) {
                .List => ObjList.cast(iterable).?.items.items.len == 0,
                .Map => ObjMap.cast(iterable).?.map.count() == 0,
                .String => ObjString.cast(iterable).?.string.len == 0,
                .Enum => ObjEnum.cast(iterable).?.cases.items.len == 0,
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
        _ = try self.buildPush(iterable);

        const key_ptr = try self.buildStackPtr(2);
        const value_ptr = try self.buildStackPtr(1);

        const cond_block = self.context.getContext().createBasicBlock("cond");
        const loop_block = self.context.getContext().createBasicBlock("loop");
        const out_block = self.context.getContext().createBasicBlock("out");
        const previous_out_block = self.state.?.current.?.break_block;
        self.state.?.current.?.break_block = out_block;
        const previous_continue_block = self.state.?.current.?.continue_block;
        self.state.?.current.?.continue_block = cond_block;

        self.buildBr(cond_block);

        self.state.?.current.?.function.?.appendExistingBasicBlock(cond_block);
        self.state.?.builder.positionBuilderAtEnd(cond_block);
        self.state.?.current.?.block = cond_block;

        // Call appropriate `next` method
        if (foreach_node.iterable.type_def.?.def_type == .Fiber) {
            // TODO: fiber foreach (tricky, need to complete foreach op after it has yielded)
            unreachable;
        } else if (foreach_node.iterable.type_def.?.def_type == .Enum) {
            const next_case = try self.buildExternApiCall(
                .bz_enumNext,
                &[_]*l.Value{
                    self.vmConstant(),
                    iterable,
                    self.state.?.builder.buildLoad(try ExternApi.value.lower(self.context.getContext()), value_ptr, "enum_case"),
                },
            );

            // Store new value in value local
            _ = self.state.?.builder.buildStore(
                next_case,
                value_ptr,
            );

            // If next key is null stop, otherwise do loop
            _ = self.state.?.builder.buildCondBr(
                self.state.?.builder.buildICmp(
                    .EQ,
                    self.state.?.builder.buildLoad(try ExternApi.value.lower(self.context.getContext()), value_ptr, "current_case"),
                    (try ExternApi.value.lower(self.context.getContext())).constInt(
                        Value.Null.val,
                        .False,
                    ),
                    "next_case_exists",
                ),
                out_block,
                loop_block,
            );
        } else {
            // The `next` method will store the new key in the key local
            const next_value = try self.buildExternApiCall(
                switch (foreach_node.iterable.type_def.?.def_type) {
                    .String => .bz_stringNext,
                    .List => .bz_listNext,
                    .Map => .bz_mapNext,
                    else => unreachable,
                },
                &[_]*l.Value{
                    self.vmConstant(),
                    iterable,
                    // Pass ptr so the metho can put the new key in it
                    key_ptr,
                },
            );

            // Store new value in value local
            _ = self.state.?.builder.buildStore(
                next_value,
                value_ptr,
            );

            // If next key is null stop, otherwise do loop
            _ = self.state.?.builder.buildCondBr(
                self.state.?.builder.buildICmp(
                    .EQ,
                    self.state.?.builder.buildLoad(try ExternApi.value.lower(self.context.getContext()), key_ptr, "current_key"),
                    (try ExternApi.value.lower(self.context.getContext())).constInt(
                        Value.Null.val,
                        .False,
                    ),
                    "next_key_exists",
                ),
                out_block,
                loop_block,
            );
        }

        // Foreach body
        self.state.?.current.?.function.?.appendExistingBasicBlock(loop_block);
        self.state.?.builder.positionBuilderAtEnd(loop_block);
        self.state.?.current.?.block = loop_block;

        _ = try self.generateNode(foreach_node.block);

        self.buildBr(cond_block);

        self.state.?.current.?.function.?.appendExistingBasicBlock(out_block);
        self.state.?.builder.positionBuilderAtEnd(out_block);
        self.state.?.current.?.block = out_block;

        self.state.?.current.?.break_block = previous_out_block;
        self.state.?.current.?.continue_block = previous_continue_block;

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
        // We should only declare locals
        assert(var_declaration_node.slot_type == .Local);

        return try self.buildPush(
            if (var_declaration_node.value) |value|
                (try self.generateNode(value)).?
            else
                (try self.lowerExternApi(.value)).constInt(
                    Value.Null.val,
                    .False,
                ),
        );
    }

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

        const function_def = function_node.node.type_def.?.resolved_type.?.Function;
        const function_type = function_def.function_type;

        var enclosing = self.state.?.current;
        self.state.?.current = try self.vm.gc.allocator.create(Frame);
        self.state.?.current.?.* = Frame{
            .enclosing = enclosing,
            .function_node = function_node,
        };

        // Those are not allowed to be compiled
        assert(function_type != .Extern and function_type != .Script and function_type != .ScriptEntryPoint);

        const ret_type = try self.lowerType(node.type_def.?);

        // Get fully qualified name of function
        var qualified_name = try self.getFunctionQualifiedName(
            function_node,
            true,
        );
        defer qualified_name.deinit();

        var function = self.state.?.module.addFunction(
            @ptrCast([*:0]const u8, qualified_name.items),
            ret_type,
        );

        self.state.?.current.?.function = function;

        var block = self.context.getContext().appendBasicBlock(
            function,
            @ptrCast(
                [*:0]const u8,
                qualified_name.items,
            ),
        );
        self.state.?.builder.positionBuilderAtEnd(block);
        self.state.?.current.?.block = block;

        if (function_node.arrow_expr) |arrow_expr| {
            const arrow_value = try self.generateNode(arrow_expr);

            _ = try self.buildReturn(arrow_value.?);
            self.state.?.current.?.return_emitted = true;
        } else {
            _ = try self.generateNode(function_node.body.?.toNode());
        }

        if (self.state.?.current.?.function_node.node.type_def.?.resolved_type.?.Function.return_type.def_type == .Void and !self.state.?.current.?.return_emitted) {
            // TODO: detect if some branches of the function body miss a return statement
            _ = try self.buildReturn(
                (try self.lowerExternApi(.value)).constInt(Value.Void.val, .False),
            );
        }

        // After a `return` statement, we had a new block in case the return is not the end of the function
        // So we check if that block has a terminator
        if (self.state.?.current.?.block.?.getTerminator() == null) {
            _ = self.state.?.builder.buildUnreachable();
        }

        // Add the NativeFn version of the function
        const native_fn = try self.generateNativeFn(
            function_node,
            function,
            ret_type,
        );

        self.state.?.current = self.state.?.current.?.enclosing;
        if (self.state.?.current != null and self.state.?.current.?.block != null) {
            self.state.?.builder.positionBuilderAtEnd(self.state.?.current.?.block.?);
        }

        // Lambda function, need to create an ObjClosure
        // Regular function actually don't need closures since their *upvalues* are globals
        if (function_type == .Anonymous) {
            // Call bz_closure
            return try self.buildExternApiCall(
                .bz_closure,
                &[_]*l.Value{
                    self.state.?.current.?.function.?.getParam(0),
                    self.state.?.builder.buildIntToPtr(
                        self.context.getContext().intType(64).constInt(
                            @ptrToInt(function_node),
                            .False,
                        ),
                        self.context.getContext().pointerType(0),
                        "",
                    ),
                    native_fn,
                    function,
                },
            );
        }

        return function;
    }

    // Checks the current block hasn't already have a terminator
    fn buildBr(self: *Self, block: *l.BasicBlock) void {
        if (self.state.?.current.?.block.?.getTerminator() == null) {
            _ = self.state.?.builder.buildBr(block);
        }
    }

    fn buildPush(self: *Self, value: *l.Value) !*l.Value {
        const stack_top_field_ptr = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.nativectx),
            self.state.?.current.?.function.?.getParam(0),
            4,
            "stack_top_field_ptr",
        );

        const stack_top_ptr = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0).pointerType(0),
            stack_top_field_ptr,
            "stack_top_ptr",
        );

        const stack_top = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0),
            stack_top_ptr,
            "stack_top",
        );

        // Store value on top of stack
        _ = self.state.?.builder.buildStore(
            value,
            stack_top,
        );

        // FIXME: check overflow, can't we do it at compile time?

        // Increment top
        return self.state.?.builder.buildStore(
            // Get element one alignment after stack_top
            self.state.?.builder.buildInBoundsGEP(
                try self.lowerExternApi(.value),
                stack_top,
                &[_]*l.Value{
                    self.context.getContext().intType(64).constInt(1, .False),
                },
                1,
                "new_top",
            ),
            stack_top_ptr,
        );
    }

    fn buildPop(self: *Self) !*l.Value {
        const stack_top_field_ptr = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.nativectx),
            self.state.?.current.?.function.?.getParam(0),
            4,
            "stack_top_field_ptr",
        );

        const stack_top_ptr = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0).pointerType(0),
            stack_top_field_ptr,
            "stack_top_ptr",
        );

        const stack_top = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0),
            stack_top_ptr,
            "stack_top",
        );

        // Decrement top
        _ = self.state.?.builder.buildStore(
            // Get element one alignment after stack_top
            self.state.?.builder.buildInBoundsGEP(
                try self.lowerExternApi(.value),
                stack_top,
                &[_]*l.Value{
                    self.context.getContext().intType(64).constInt(
                        @bitCast(c_ulonglong, @as(i64, -1)),
                        .True,
                    ),
                },
                1,
                "new_top",
            ),
            stack_top_ptr,
        );

        // Return new top
        const new_stack_top = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0),
            stack_top_ptr,
            "new_stack_top",
        );

        return self.state.?.builder.buildLoad(
            try self.lowerExternApi(.value),
            self.state.?.builder.buildInBoundsGEP(
                try self.lowerExternApi(.value),
                new_stack_top,
                &[_]*l.Value{
                    self.context.getContext().intType(64).constInt(0, .True),
                },
                1,
                "popped_ptr",
            ),
            "popped",
        );
    }

    fn buildPeek(self: *Self, distance: usize) !*l.Value {
        const stack_top_field_ptr = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.nativectx),
            self.state.?.current.?.function.?.getParam(0),
            4,
            "stack_top_field_ptr",
        );

        const stack_top_ptr = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0).pointerType(0),
            stack_top_field_ptr,
            "stack_top_ptr",
        );

        const stack_top = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0),
            stack_top_ptr,
            "stack_top",
        );

        return self.state.?.builder.buildLoad(
            try self.lowerExternApi(.value),
            self.state.?.builder.buildInBoundsGEP(
                try self.lowerExternApi(.value),
                stack_top,
                &[_]*l.Value{
                    self.context.getContext().intType(64).constInt(
                        @bitCast(c_ulonglong, -1 - @intCast(i64, distance)),
                        .True,
                    ),
                },
                1,
                "peeked_ptr",
            ),
            "peeked",
        );
    }

    // Get ptr to element on stack
    fn buildStackPtr(self: *Self, distance: usize) !*l.Value {
        const stack_top_field_ptr = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.nativectx),
            self.state.?.current.?.function.?.getParam(0),
            4,
            "stack_top_field_ptr",
        );

        const stack_top_ptr = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0).pointerType(0),
            stack_top_field_ptr,
            "stack_top_ptr",
        );

        const stack_top = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0),
            stack_top_ptr,
            "stack_top",
        );

        return self.state.?.builder.buildInBoundsGEP(
            try self.lowerExternApi(.value),
            stack_top,
            &[_]*l.Value{
                self.context.getContext().intType(64).constInt(
                    @bitCast(c_ulonglong, -1 - @intCast(i64, distance)),
                    .True,
                ),
            },
            1,
            "local_from_top",
        );
    }

    fn buildReturn(self: *Self, value: *l.Value) !*l.Value {
        // Get base
        const base_field_ptr = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.nativectx),
            self.state.?.current.?.function.?.getParam(0),
            3,
            "base_field_ptr",
        );

        const base_ptr = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0),
            base_field_ptr,
            "base_ptr",
        );

        _ = try self.buildExternApiCall(
            .bz_closeUpValues,
            &[_]*l.Value{
                self.vmConstant(),
                base_ptr,
            },
        );

        // Get stack_top
        const stack_top_field_ptr = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.nativectx),
            self.state.?.current.?.function.?.getParam(0),
            4,
            "stack_top_field_ptr",
        );

        const stack_top_ptr = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0).pointerType(0),
            stack_top_field_ptr,
            "stack_top_ptr",
        );

        // Reset stack_top to base
        _ = self.state.?.builder.buildStore(
            base_ptr,
            stack_top_ptr,
        );

        // Do return
        const ret = self.state.?.builder.buildRet(value);

        // Function might not be over after this so create new block
        var block = self.context.getContext().appendBasicBlock(
            self.state.?.current.?.function.?,
            "continue",
        );
        self.state.?.builder.positionBuilderAtEnd(block);
        self.state.?.current.?.block = block;

        return ret;
    }

    /// Build instructions to get local at given index
    fn buildGetLocal(self: *Self, slot: usize) !*l.Value {
        const base_field_ptr = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.nativectx),
            self.state.?.current.?.function.?.getParam(0),
            3,
            "base_field_ptr",
        );

        const base_ptr = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0),
            base_field_ptr,
            "base_ptr",
        );

        const slot_ptr = self.state.?.builder.buildInBoundsGEP(
            try self.lowerExternApi(.value),
            base_ptr,
            &[_]*l.Value{
                self.context.getContext().intType(64).constInt(
                    slot,
                    .True,
                ),
            },
            1,
            "slot",
        );

        return self.state.?.builder.buildLoad(
            try self.lowerExternApi(.value),
            slot_ptr,
            "local",
        );
    }

    /// Build instructions to set local at given index
    fn buildSetLocal(self: *Self, slot: usize, value: *l.Value) !*l.Value {
        const base_field_ptr = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.nativectx),
            self.state.?.current.?.function.?.getParam(0),
            3,
            "base_field_ptr",
        );

        const base_ptr = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0),
            base_field_ptr,
            "base_ptr",
        );

        const slot_ptr = self.state.?.builder.buildInBoundsGEP(
            try self.lowerExternApi(.value),
            base_ptr,
            &[_]*l.Value{
                self.context.getContext().intType(64).constInt(
                    slot,
                    .True,
                ),
            },
            1,
            "slot",
        );

        return self.state.?.builder.buildStore(
            value,
            slot_ptr,
        );
    }

    /// Build instructions to get global at given index
    fn buildGetGlobal(self: *Self, slot: usize) !*l.Value {
        // Get ptr on NativeCtx `globals` field
        const globals_ptr = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.nativectx),
            self.state.?.current.?.function.?.getParam(0),
            1,
            "globals_ptr",
        );

        // Load globals ptr
        const globals = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.globals)).pointerType(0),
            globals_ptr,
            "globals",
        );

        // Get element ptr at `slot`
        const value_ptr = self.state.?.builder.buildInBoundsGEP(
            try self.lowerExternApi(.value),
            globals,
            &[_]*l.Value{
                self.context.getContext().intType(64).constInt(slot, .False),
            },
            1,
            "value_ptr",
        );

        // Load value
        return self.state.?.builder.buildLoad(
            try self.lowerExternApi(.value),
            value_ptr,
            "value",
        );
    }

    /// Build instructions to set global at given index
    fn buildSetGlobal(self: *Self, slot: usize, value: *l.Value) !*l.Value {
        // Get ptr on NativeCtx `globals` field
        const globals_ptr = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.nativectx),
            self.state.?.current.?.function.?.getParam(0),
            1,
            "globals_ptr",
        );

        // Load globals ptr
        const globals = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.globals)).pointerType(0),
            globals_ptr,
            "globals",
        );

        // Get element ptr at `slot`
        const value_ptr = self.state.?.builder.buildInBoundsGEP(
            try self.lowerExternApi(.value),
            globals,
            &[_]*l.Value{
                self.context.getContext().intType(64).constInt(slot, .False),
            },
            1,
            "value_ptr",
        );

        // Store value
        return self.state.?.builder.buildStore(
            value,
            value_ptr,
        );
    }

    fn buildValueToBoolean(self: *Self, value: *l.Value) *l.Value {
        return self.state.?.builder.buildICmp(
            .EQ,
            value,
            self.context.getContext().intType(64).constInt(_value.TrueMask, .False),
            "",
        );
    }

    fn buildValueFromBoolean(self: *Self, value: *l.Value) *l.Value {
        return self.state.?.builder.buildSelect(
            self.state.?.builder.buildICmp(
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
        return self.state.?.builder.buildTrunc(
            self.state.?.builder.buildAnd(
                value,
                self.context.getContext().intType(64).constInt(
                    0xffffffff,
                    .False,
                ),
                "",
            ),
            self.context.getContext().intType(32),
            "",
        );
    }

    fn buildValueFromInteger(self: *Self, integer: *l.Value) *l.Value {
        return self.state.?.builder.buildOr(
            self.context.getContext().intType(64).constInt(
                _value.IntegerMask,
                .False,
            ),
            // signed i32 -> i64
            self.state.?.builder.buildZExt(
                integer,
                self.context.getContext().intType(64),
                "",
            ),
            "",
        );
    }

    fn buildValueToObj(self: *Self, value: *l.Value) *l.Value {
        return self.state.?.builder.buildAnd(
            value,
            self.state.?.builder.buildNot(
                self.context.getContext().intType(64).constInt(_value.PointerMask, .False),
                "",
            ),
            "",
        );
    }

    fn buildValueFromObj(self: *Self, value: *l.Value) *l.Value {
        return self.state.?.builder.buildOr(
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
            .Float => self.state.?.builder.buildBitCast(
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
            .Float => self.state.?.builder.buildBitCast(
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

    fn generateNativeFn(self: *Self, function_node: *FunctionNode, raw_fn: *l.Value, ret_type: *l.Type) !*l.Value {
        const function_def = function_node.node.type_def.?.resolved_type.?.Function;
        const function_type = function_def.function_type;

        assert(function_type != .Extern);

        var nativefn_qualified_name = try self.getFunctionQualifiedName(function_node, false);
        defer nativefn_qualified_name.deinit();

        var native_fn = self.state.?.module.addFunction(
            @ptrCast([*:0]const u8, nativefn_qualified_name.items),
            try self.lowerExternApi(.nativefn),
        );

        // Error handling blocks
        const fun_block = self.context.getContext().createBasicBlock("fun");
        const err_propagate_block = self.context.getContext().createBasicBlock("err_propagate");

        // That version of the function takes argument from the stack and pushes the result of the raw version on the stack
        var block = self.context.getContext().appendBasicBlock(native_fn, @ptrCast([*:0]const u8, nativefn_qualified_name.items));
        self.state.?.builder.positionBuilderAtEnd(block);

        // Catch any error to forward them as a buzz error (push paylod + return -1)
        // Set it as current jump env
        const try_ctx = try self.buildExternApiCall(
            .bz_setTryCtx,
            &[_]*l.Value{
                self.vmConstant(),
            },
        );

        const env = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.tryctx),
            try_ctx,
            1,
            "env",
        );

        // setjmp
        const status = try self.buildExternApiCall(
            .setjmp,
            &[_]*l.Value{env},
        );

        // If status is 0, go to body, else go to catch clauses
        const has_error = self.state.?.builder.buildICmp(
            .EQ,
            status,
            self.context.getContext().intType(@sizeOf(c_int)).constInt(1, .False),
            "has_error",
        );

        _ = self.state.?.builder.buildCondBr(
            has_error,
            err_propagate_block,
            fun_block,
        );

        native_fn.appendExistingBasicBlock(err_propagate_block);
        self.state.?.builder.positionBuilderAtEnd(err_propagate_block);
        self.state.?.current.?.block = err_propagate_block;

        // Payload already on stack so juste return -1;
        _ = self.state.?.builder.buildRet(
            self.context.getContext().intType(8).constInt(
                @bitCast(c_ulonglong, @as(i64, -1)),
                .True,
            ),
        );

        native_fn.appendExistingBasicBlock(fun_block);
        self.state.?.builder.positionBuilderAtEnd(fun_block);
        self.state.?.current.?.block = fun_block;

        // Call the raw function
        const result = self.state.?.builder.buildCall(
            ret_type,
            raw_fn,
            &[_]*l.Value{native_fn.getParam(0)},
            1,
            "",
        );

        const should_return = function_def.return_type.def_type != .Void;

        // Push its result back into the VM
        if (should_return) {
            _ = try self.buildExternApiCall(
                .bz_push,
                &[_]*l.Value{
                    self.vmConstant(),
                    result,
                },
            );
            // _ = try self.buildPush(result);
        }

        // 1 = there's a return, 0 = no return, -1 = error
        _ = self.state.?.builder.buildRet(
            self.context.getContext().intType(8).constInt(
                if (should_return) 1 else 0,
                .True,
            ),
        );

        return native_fn;
    }

    fn buildCloseUpValues(self: *Self) !void {
        // Get stack_top
        const stack_top_field_ptr = self.state.?.builder.buildStructGEP(
            try self.lowerExternApi(.nativectx),
            self.state.?.current.?.function.?.getParam(0),
            4,
            "stack_top_field_ptr",
        );

        const stack_top_ptr = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0).pointerType(0),
            stack_top_field_ptr,
            "stack_top_ptr",
        );

        const stack_top = self.state.?.builder.buildLoad(
            (try self.lowerExternApi(.value)).pointerType(0),
            stack_top_ptr,
            "stack_top",
        );

        _ = try self.buildExternApiCall(
            .bz_closeUpValues,
            &[_]*l.Value{
                self.vmConstant(),
                self.state.?.builder.buildInBoundsGEP(
                    try self.lowerExternApi(.value),
                    stack_top,
                    &[_]*l.Value{
                        self.context.getContext().intType(64).constInt(
                            @bitCast(c_ulonglong, @as(i64, -1)),
                            .True,
                        ),
                    },
                    1,
                    "slot",
                ),
            },
        );
    }
};
