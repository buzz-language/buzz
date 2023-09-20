const std = @import("std");
const builtin = @import("builtin");
const BuildOptions = @import("build_options");
const jmp = @import("../jmp.zig").jmp;

// FIXME: all those should operate on Value
// FIXME: some should only be available to the JIT compiler
// FIXME: naming is not consistant

pub const ObjUpValue = opaque {};

pub const ObjNative = opaque {
    pub extern fn bz_toObjNative(value: Value) *ObjNative;
};

pub const ObjFunction = opaque {};

pub const NativeCtx = extern struct {
    vm: *VM,
    globals: [*]Value,
    upvalues: [*]*ObjUpValue,
    // Pointer to the stack_top field of the current fiber
    // !! Needs to change when current fiber changes !!
    stack_top: *[*]Value,
};

pub const TryCtx = extern struct {
    previous: ?*TryCtx,
    env: jmp.jmp_buf = undefined,
};

pub const ZigType = opaque {
    pub extern fn bz_zigTypeSize(self: *ZigType) usize;
    pub extern fn bz_zigTypeAlignment(self: *ZigType) u16;
};

pub const VM = opaque {
    pub const allocator = @import("../buzz_api.zig").allocator;

    pub extern fn bz_newVM(self: *VM) *VM;
    pub extern fn bz_deinitVM(self: *VM) void;
    pub extern fn bz_compile(
        self: *VM,
        source: ?[*]const u8,
        source_len: usize,
        file_name: ?[*]const u8,
        file_name_len: usize,
    ) ?*ObjFunction;
    pub extern fn bz_interpret(self: *VM, function: *ObjFunction) bool;
    pub extern fn bz_call(
        self: *VM,
        closure: *ObjClosure,
        arguments: ?[*]const *const Value,
        len: usize,
        catch_value: ?*Value,
    ) void;
    pub extern fn bz_push(self: *VM, value: Value) void;
    pub extern fn bz_pop(self: *VM) Value;
    pub extern fn bz_peek(self: *VM, distance: u32) Value;
    pub extern fn bz_pushBool(self: *VM, value: bool) void;
    pub extern fn bz_pushFloat(self: *VM, value: f64) void;
    pub extern fn bz_pushInteger(self: *VM, value: i32) void;
    pub extern fn bz_pushString(self: *VM, value: *ObjString) void;
    pub extern fn bz_pushList(self: *VM, value: *ObjList) void;
    pub extern fn bz_pushUserData(self: *VM, value: *ObjUserData) void;
    pub extern fn bz_pushNull(self: *VM) void;
    pub extern fn bz_pushVoid(self: *VM) void;
    pub extern fn bz_pushObjectInstance(vm: *VM, payload: *ObjObjectInstance) void;
    pub extern fn bz_pushEnumInstance(vm: *VM, payload: *ObjEnumInstance) void;
    pub extern fn bz_pushError(
        self: *VM,
        qualified_name: [*]const u8,
        len: usize,
        message: ?[*]const u8,
        mlen: usize,
    ) void;
    pub extern fn bz_pushErrorEnum(self: *VM, qualified_name: [*]const u8, name_len: usize, case: [*]const u8, case_len: usize) void;
    pub extern fn bz_zigValueSize(ztype: *ZigType) usize;
    pub extern fn bz_writeZigValueToBuffer(
        vm: *VM,
        value: Value,
        ztype: *ZigType,
        at: usize,
        buf: [*]u8,
        capacity: usize,
    ) void;
    pub extern fn bz_readZigValueFromBuffer(
        vm: *VM,
        ztype: *ZigType,
        at: usize,
        buf: [*]u8,
        len: usize,
    ) Value;
    pub extern fn bz_checkBuzzType(
        vm: *VM,
        value: Value,
        ztype: *ZigType,
        btype: Value,
    ) bool;

    pub inline fn pushError(self: *VM, qualified_name: []const u8, message: ?[]const u8) void {
        self.bz_pushError(
            qualified_name.ptr,
            qualified_name.len,
            if (message) |m| m.ptr else null,
            if (message) |m| m.len else 0,
        );
    }

    pub inline fn pushErrorEnum(self: *VM, qualified_name: []const u8, case: []const u8) void {
        self.bz_pushErrorEnum(
            qualified_name.ptr,
            qualified_name.len,
            case.ptr,
            case.len,
        );
    }

    pub extern fn bz_serialize(vm: *VM, value: Value, error_value: *Value) Value;

    pub extern fn bz_throw(vm: *VM, value: Value) void;
    pub extern fn bz_rethrow(vm: *VM) void;
    pub extern fn bz_getQualified(self: *VM, qualified_name: [*]const u8, len: usize) Value;

    pub extern fn bz_allocated(self: *VM) usize;
    pub extern fn bz_collect(self: *VM) bool;

    pub extern fn bz_setTryCtx(self: *VM) *TryCtx;
    pub extern fn bz_popTryCtx(self: *VM) void;

    pub extern fn bz_closeUpValues(vm: *VM, last: *Value) void;
    pub extern fn bz_getUpValue(ctx: *NativeCtx, slot: usize) Value;
    pub extern fn bz_setUpValue(ctx: *NativeCtx, slot: usize, value: Value) void;
    pub extern fn bz_closure(ctx: *NativeCtx, function_node: *FunctionNode, native: *anyopaque, native_raw: *anyopaque) Value;
    pub extern fn bz_bindMethod(vm: *VM, receiver: Value, method_value: Value, native_value: Value) Value;
    pub extern fn bz_context(ctx: *NativeCtx, closure_value: Value, new_ctx: *NativeCtx, arg_count: usize) *anyopaque;
    pub extern fn bz_clone(vm: *VM, value: Value) Value;
    pub extern fn bz_currentFiber(vm: *VM) Value;

    pub extern fn bz_dumpStack(vm: *VM) void;

    pub extern fn bz_zigType(
        vm: *VM,
        ztype: [*]const u8,
        len: usize,
        expected_type: *Value,
    ) ?*ZigType;
};

pub const FunctionNode = opaque {};

// FIXME: can we avoid duplicating this code from value.zig?
const Tag = u3;
pub const TagBoolean: Tag = 0;
pub const TagInteger: Tag = 1;
pub const TagNull: Tag = 2;
pub const TagVoid: Tag = 3;
pub const TagObj: Tag = 4;
pub const TagError: Tag = 5;

/// Most significant bit.
pub const SignMask: u64 = 1 << 63;

/// QNAN and one extra bit to the right.
pub const TaggedValueMask: u64 = 0x7ffc000000000000;

/// TaggedMask + Sign bit indicates a pointer value.
pub const PointerMask: u64 = TaggedValueMask | SignMask;

pub const BooleanMask: u64 = TaggedValueMask | (@as(u64, TagBoolean) << 32);
pub const FalseMask: u64 = BooleanMask;
pub const TrueBitMask: u64 = 1;
pub const TrueMask: u64 = BooleanMask | TrueBitMask;

pub const IntegerMask: u64 = TaggedValueMask | (@as(u64, TagInteger) << 32);
pub const NullMask: u64 = TaggedValueMask | (@as(u64, TagNull) << 32);
pub const VoidMask: u64 = TaggedValueMask | (@as(u64, TagVoid) << 32);
pub const ErrorMask: u64 = TaggedValueMask | (@as(u64, TagError) << 32);

pub const TagMask: u32 = (1 << 3) - 1;
pub const TaggedPrimitiveMask = TaggedValueMask | (@as(u64, TagMask) << 32);

pub const Value = packed struct {
    val: u64,

    pub const Null = Value{ .val = NullMask };
    pub const Void = Value{ .val = VoidMask };
    pub const True = Value{ .val = TrueMask };
    pub const False = Value{ .val = FalseMask };
    // We only need this so that an NativeFn can see the error returned by its raw function
    pub const Error = Value{ .val = ErrorMask };

    pub inline fn fromBoolean(val: bool) Value {
        return if (val) True else False;
    }

    pub inline fn fromInteger(val: i32) Value {
        return .{ .val = IntegerMask | @as(u32, @bitCast(val)) };
    }

    pub inline fn fromFloat(val: f64) Value {
        return .{ .val = @as(u64, @bitCast(val)) };
    }

    pub inline fn fromObj(val: *anyopaque) Value {
        return .{ .val = PointerMask | @intFromPtr(val) };
    }

    pub inline fn getTag(self: Value) u3 {
        return @intCast(@as(u32, @intCast(self.val >> 32)) & TagMask);
    }

    pub inline fn isBool(self: Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == BooleanMask;
    }

    pub inline fn isInteger(self: Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == IntegerMask;
    }

    pub inline fn isFloat(self: Value) bool {
        return self.val & TaggedValueMask != TaggedValueMask;
    }

    pub inline fn isNumber(self: Value) bool {
        return self.isFloat() or self.isInteger();
    }

    pub inline fn isObj(self: Value) bool {
        return self.val & PointerMask == PointerMask;
    }

    pub inline fn isNull(self: Value) bool {
        return self.val == NullMask;
    }

    pub inline fn isVoid(self: Value) bool {
        return self.val == VoidMask;
    }

    pub inline fn isError(self: Value) bool {
        return self.val == ErrorMask;
    }

    pub inline fn boolean(self: Value) bool {
        return self.val == TrueMask;
    }

    pub inline fn integer(self: Value) i32 {
        return @bitCast(@as(u32, @intCast(self.val & 0xffffffff)));
    }

    pub inline fn float(self: Value) f64 {
        return @bitCast(self.val);
    }

    pub inline fn obj(self: Value) *anyopaque {
        return @ptrFromInt(self.val & ~PointerMask);
    }

    pub extern fn bz_valueToObject(value: Value) *ObjObject;
    pub extern fn bz_valueToObjectInstance(value: Value) *ObjObjectInstance;
    pub extern fn bz_valueToObjEnumInstance(value: Value) *ObjEnumInstance;
    pub extern fn bz_valueToObjFiber(value: Value) *ObjFiber;
    pub extern fn bz_valueToObjList(value: Value) *ObjList;
    pub extern fn bz_valueToObjMap(value: Value) *ObjMap;
    pub extern fn bz_valueToObjPattern(value: Value) *ObjPattern;
    pub extern fn bz_valueToObjString(value: Value) *ObjString;
    pub extern fn bz_valueToString(value: Value, len: *usize) ?[*]const u8;
    pub extern fn bz_valueToCString(value: Value) ?[*:0]const u8;
    pub extern fn bz_valueToObjUserData(value: Value) *ObjUserData;
    pub extern fn bz_valueToUserData(value: Value) u64;
    pub extern fn bz_valueToObjTypeDef(value: Value) *ObjTypeDef;
    pub extern fn bz_valueToForeignStructPtr(value: Value) [*]u8;

    pub extern fn bz_valueDump(value: Value, vm: *VM) void;

    pub extern fn bz_valueToClosure(value: Value) *ObjClosure;
    pub extern fn bz_valueEqual(self: Value, other: Value) Value;
    pub extern fn bz_valueIs(self: Value, type_def: Value) Value;
    pub extern fn bz_valueTypeOf(self: Value, vm: *VM) Value;
};

pub const ObjClosure = opaque {};

pub const ObjTypeDef = opaque {
    pub extern fn bz_stringType(vm: *VM) Value;
    pub extern fn bz_mapType(vm: *VM, key_type: Value, value_type: Value) Value;
    pub extern fn bz_fstructTypeSize(self: *ObjTypeDef) usize;
    pub extern fn bz_fstructTypeAlign(type_def: *ObjTypeDef) usize;
};

pub const ObjString = opaque {
    pub extern fn bz_string(vm: *VM, string: ?[*]const u8, len: usize) ?*ObjString;
    pub extern fn bz_stringZ(vm: *VM, string: ?[*:0]const u8) ?*ObjString;
    pub extern fn bz_objStringToString(obj_string: *ObjString, len: *usize) ?[*]const u8;
    pub extern fn bz_objStringToValue(obj_string: *ObjString) Value;
    pub extern fn bz_objStringConcat(vm: *VM, obj_string: Value, other: Value) Value;
    pub extern fn bz_objStringSubscript(vm: *VM, obj_string: Value, index_value: Value) Value;
    pub extern fn bz_toString(vm: *VM, value: Value) Value;
    pub extern fn bz_getStringField(vm: *VM, field_name_value: Value) Value;
    pub extern fn bz_stringNext(vm: *VM, string_value: Value, index: *Value) Value;
};

pub const ObjList = opaque {
    pub extern fn bz_newList(vm: *VM, of_type: Value) Value;
    pub extern fn bz_listAppend(vm: *VM, list: Value, value: Value) void;
    pub extern fn bz_valueToList(value: Value) *ObjList;
    pub extern fn bz_listGet(self: Value, index: usize) Value;
    pub extern fn bz_listSet(vm: *VM, self: Value, index: usize, value: Value) void;
    pub extern fn bz_listLen(self: *ObjList) usize;
    pub extern fn bz_listConcat(vm: *VM, list: Value, other_list: Value) Value;
    pub extern fn bz_getListField(vm: *VM, list_value: Value, field_name_value: Value, bind: bool) Value;
    pub extern fn bz_listNext(vm: *VM, list_value: Value, index: *Value) Value;
};

pub const ObjMap = opaque {
    pub extern fn bz_newMap(vm: *VM, map_type: Value) Value;
    pub extern fn bz_mapSet(vm: *VM, map: Value, key: Value, value: Value) void;
    pub extern fn bz_mapGet(map: Value, key: Value) Value;
    pub extern fn bz_mapConcat(vm: *VM, map: Value, other_map: Value) Value;
    pub extern fn bz_getMapField(vm: *VM, map_value: Value, field_name_value: Value, bind: bool) Value;
    pub extern fn bz_mapNext(vm: *VM, map_value: Value, index: *Value) Value;
};

pub const ObjUserData = opaque {
    pub extern fn bz_newUserData(vm: *VM, userdata: u64) ?*ObjUserData;
    pub extern fn bz_userDataToValue(userdata: *ObjUserData) Value;
    pub extern fn bz_getUserDataPtr(userdata: *ObjUserData) u64;
};

pub const ObjObjectInstance = opaque {};

pub const ObjObject = opaque {
    pub extern fn bz_instanceQualified(self: *VM, qualified_name: [*]const u8, len: usize) Value;
    pub extern fn bz_instance(vm: *VM, object_value: Value, typedef_value: Value) Value;
    pub extern fn bz_setInstanceField(vm: *VM, instance_value: Value, field_name_value: Value, value: Value) void;
    pub extern fn bz_getInstanceField(vm: *VM, instance_value: Value, field_name_value: Value) Value;
    pub extern fn bz_getObjectField(object_value: Value, field_name_value: Value) Value;
    pub extern fn bz_setObjectField(vm: *VM, object_value: Value, field_name_value: Value, value: Value) void;
};

pub const ObjEnumInstance = opaque {
    pub extern fn bz_getEnumCaseValue(enum_instance_value: Value) Value;
};

pub const ObjEnum = opaque {
    pub extern fn bz_getEnumCase(vm: *VM, enum_value: Value, case_name_value: Value) Value;
    pub extern fn bz_getEnumCaseFromValue(vm: *VM, enum_value: Value, case_value: Value) Value;
    pub extern fn bz_enumNext(vm: *VM, enum_value: Value, case: Value) Value;
};

pub const ObjPattern = opaque {
    pub extern fn bz_getPatternField(vm: *VM, field_name_value: Value) Value;
};

pub const ObjFiber = opaque {
    pub extern fn bz_getFiberField(vm: *VM, field_name_value: Value) Value;
    pub extern fn bz_isMainFiber(self: *ObjFiber, vm: *VM) Value;
};

pub const ObjForeignStruct = opaque {
    pub extern fn bz_fstructGet(vm: *VM, value: Value, field: [*]const u8, len: usize) Value;
    pub extern fn bz_fstructSet(vm: *VM, value: Value, field: [*]const u8, len: usize, new_value: Value) void;
    pub extern fn bz_fstructInstance(vm: *VM, typedef_value: Value) Value;
    pub extern fn bz_fstructSlice(fstruct_value: Value, len: *usize) [*]u8;
    pub extern fn bz_fstructFromSlice(vm: *VM, type_def: *ObjTypeDef, ptr: [*]u8, len: usize) Value;
};

pub extern fn dumpInt(value: u64) void;
