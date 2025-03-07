const std = @import("std");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const BuildOptions = @import("build_options");
const jmp = if (!is_wasm) @import("jmp.zig").jmp else void;

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

pub const Native = fn (ctx: *NativeCtx) callconv(.C) c_int;
pub const NativeFn = *const Native;

pub const Double = f64;
pub const Integer = i48;

// FIXME: can we avoid duplicating this code from value.zig?
const Tag = u3;
const TagBoolean: Tag = 0;
const TagInteger: Tag = 1;
const TagNull: Tag = 2;
const TagVoid: Tag = 3;
const TagObj: Tag = 4;
const TagError: Tag = 5;

/// Most significant bit.
const SignMask: u64 = 1 << 63;

/// QNAN and one extra bit to the right.
const TaggedValueMask: u64 = 0x7ffc000000000000;
pub const TaggedUpperValueMask: u64 = 0xffff000000000000;

/// TaggedMask + Sign bit indicates a pointer value.
const PointerMask: u64 = TaggedValueMask | SignMask;

const BooleanMask: u64 = TaggedValueMask | (@as(u64, TagBoolean) << 32);
const FalseMask: u64 = BooleanMask;
const TrueBitMask: u64 = 1;
const TrueMask: u64 = BooleanMask | TrueBitMask;
const IntegerMask: u64 = TaggedValueMask | (@as(u64, TagInteger) << 49);
const NullMask: u64 = TaggedValueMask | (@as(u64, TagNull) << 32);
const VoidMask: u64 = TaggedValueMask | (@as(u64, TagVoid) << 32);
const ErrorMask: u64 = TaggedValueMask | (@as(u64, TagError) << 32);

const TagMask: u32 = (1 << 3) - 1;
const TaggedPrimitiveMask = TaggedValueMask | (@as(u64, TagMask) << 32) | IntegerMask;

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

    pub inline fn fromInteger(val: Integer) Value {
        return .{ .val = IntegerMask | @as(u48, @bitCast(val)) };
    }

    pub inline fn fromDouble(val: Double) Value {
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
        return self.val & (TaggedUpperValueMask | SignMask) == IntegerMask;
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

    pub inline fn integer(self: Value) Integer {
        return @bitCast(@as(u48, @intCast(self.val & 0xffffffffffff)));
    }

    pub inline fn double(self: Value) Double {
        return @bitCast(self.val);
    }

    pub inline fn obj(self: Value) *anyopaque {
        return @ptrFromInt(@as(usize, @truncate(self.val & ~PointerMask)));
    }

    pub extern fn bz_valueToString(value: Value, len: *usize) ?[*]const u8;
    pub extern fn bz_valueToCString(value: Value) callconv(.c) ?[*:0]const u8;
    pub extern fn bz_valueToForeignContainerPtr(value: Value) callconv(.c) [*]u8;
    pub extern fn bz_valueIsForeignContainer(value: Value) callconv(.c) bool;
    pub extern fn bz_valueDump(value: Value, vm: *VM) callconv(.c) void;
    pub extern fn bz_valueEqual(self: Value, other: Value) callconv(.c) Value;
    pub extern fn bz_valueIs(self: Value, type_def: Value) callconv(.c) Value;
    pub extern fn bz_valueTypeOf(self: Value, vm: *VM) callconv(.c) Value;
    pub extern fn bz_getUserDataPtr(userdata: Value) callconv(.c) u64;
    pub extern fn bz_containerTypeSize(container: Value) callconv(.c) usize;
    pub extern fn bz_containerTypeAlign(type_def: Value) callconv(.c) usize;
    pub extern fn bz_valueCastToString(value: Value, vm: *VM) callconv(.c) Value;
    pub extern fn bz_stringConcat(string: Value, other: Value, vm: *VM) callconv(.c) Value;
    pub extern fn bz_stringSubscript(obj_string: Value, index_value: Value, checked: bool, vm: *VM) callconv(.c) Value;
    pub extern fn bz_stringNext(string_value: Value, index: *Value, vm: *VM) callconv(.c) Value;
    pub extern fn bz_rangeNext(range_value: Value, index_slot: Value) callconv(.c) Value;
    pub extern fn bz_getRangeProperty(range_value: Value, property_idx: usize, bind: bool, vm: *VM) callconv(.c) Value;
    pub extern fn bz_listAppend(list: Value, value: Value, vm: *VM) callconv(.c) void;
    pub extern fn bz_listGet(list: Value, index: i64, checked: bool) callconv(.c) Value;
    pub extern fn bz_listSet(list: Value, index: usize, value: Value, vm: *VM) callconv(.c) void;
    pub extern fn bz_listLen(list: Value) callconv(.c) usize;
    pub extern fn bz_listConcat(list: Value, other_list: Value, vm: *VM) callconv(.c) Value;
    pub extern fn bz_listNext(list_value: Value, index: *Value, vm: *VM) callconv(.c) Value;
    pub extern fn bz_mapSet(map: Value, key: Value, value: Value, vm: *VM) callconv(.c) void;
    pub extern fn bz_mapGet(map: Value, key: Value) callconv(.c) Value;
    pub extern fn bz_mapConcat(map: Value, other_map: Value, vm: *VM) callconv(.c) Value;
    pub extern fn bz_mapNext(map_value: Value, index: *Value) callconv(.c) Value;
    pub extern fn bz_setObjectInstanceProperty(instance_value: Value, property_idx: usize, value: Value, vm: *VM) callconv(.c) void;
    pub extern fn bz_getObjectInstanceProperty(instance_value: Value, property_idx: usize) callconv(.c) Value;
    pub extern fn bz_getObjectInstanceMethod(instance_value: Value, method_idx: usize, bind: bool, vm: *VM) callconv(.c) Value;
    pub extern fn bz_getProtocolMethod(instance_value: Value, method_name: Value, vm: *VM) callconv(.c) Value;
    pub extern fn bz_getObjectField(object_value: Value, field_idx: usize) callconv(.c) Value;
    pub extern fn bz_setObjectField(object_value: Value, field_idx: usize, value: Value, vm: *VM) callconv(.c) void;
    pub extern fn bz_getEnumInstanceValue(enum_instance_value: Value) callconv(.c) Value;
    pub extern fn bz_getEnumCase(enum_value: Value, case_name_value: Value, vm: *VM) callconv(.c) Value;
    pub extern fn bz_getEnumCaseFromValue(enum_value: Value, case_value: Value, vm: *VM) callconv(.c) Value;
    pub extern fn bz_enumNext(enum_value: Value, case: Value, vm: *VM) callconv(.c) Value;
    pub extern fn bz_foreignContainerGet(value: Value, field_idx: usize, vm: *VM) callconv(.c) Value;
    pub extern fn bz_foreignContainerSet(value: Value, field_idx: usize, new_value: Value, vm: *VM) callconv(.c) void;
    pub extern fn bz_foreignContainerSlice(container_value: Value, len: *usize) callconv(.c) [*]u8;
};

pub const NativeCtx = extern struct {
    vm: *VM,
    globals: [*]Value,
    upvalues: [*]*anyopaque,
    base: [*]Value,
    // Pointer to the stack_top field of the current fiber
    // !! Needs to change when current fiber changes !!
    stack_top: *[*]Value,
};

pub const TryCtx = extern struct {
    previous: ?*TryCtx,
    env: jmp.jmp_buf = undefined,
};

pub const ZigType = opaque {
    pub extern fn bz_zigTypeSize(self: *ZigType) callconv(.c) usize;
    pub extern fn bz_zigTypeAlignment(self: *ZigType) callconv(.c) u16;
    pub extern fn bz_zigTypeToCString(self: *ZigType, vm: *VM) callconv(.c) [*:0]const u8;
};

pub const VM = opaque {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .safety = builtin.mode == .Debug,
    }){};

    pub const allocator = if (builtin.mode == .Debug or is_wasm)
        gpa.allocator()
    else if (BuildOptions.mimalloc)
        @import("mimalloc.zig").mim_allocator
    else
        std.heap.c_allocator;

    pub extern fn bz_newVM() *VM;
    pub extern fn bz_deinitVM(self: *VM) callconv(.c) void;
    pub extern fn bz_panic(vm: *VM, msg: [*]const u8, len: usize) callconv(.c) void;
    pub extern fn bz_run(self: *VM, source: ?[*]const u8, source_len: usize, file_name: ?[*]const u8, file_name_len: usize) callconv(.c) bool;
    pub extern fn bz_call(self: *VM, closure: Value, arguments: ?[*]const *const Value, len: usize, catch_value: ?*Value) callconv(.c) void;
    pub extern fn bz_push(self: *VM, value: Value) callconv(.c) void;
    pub extern fn bz_pop(self: *VM) callconv(.c) Value;
    pub extern fn bz_peek(self: *VM, distance: u32) callconv(.c) Value;
    pub extern fn bz_at(vm: *VM, at: u32) callconv(.c) Value;
    pub extern fn bz_pushError(self: *VM, qualified_name: [*]const u8, len: usize, message: ?[*]const u8, mlen: usize) callconv(.c) void;
    pub extern fn bz_pushErrorEnum(self: *VM, qualified_name: [*]const u8, name_len: usize, case: [*]const u8, case_len: usize) callconv(.c) void;
    pub extern fn bz_stringToValue(vm: *VM, string: ?[*]const u8, len: usize) callconv(.c) Value;
    pub extern fn bz_stringToValueZ(vm: *VM, string: ?[*:0]const u8) callconv(.c) Value;
    pub extern fn bz_newUserData(vm: *VM, userdata: u64) callconv(.c) Value;
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
    pub extern fn bz_serialize(vm: *VM, value: Value, error_value: *Value) callconv(.c) Value;
    pub extern fn bz_throw(vm: *VM, value: Value) callconv(.c) void;
    pub extern fn bz_rethrow(vm: *VM) callconv(.c) void;
    pub extern fn bz_getQualified(self: *VM, qualified_name: [*]const u8, len: usize) callconv(.c) Value;
    pub extern fn bz_allocated(self: *VM) callconv(.c) usize;
    pub extern fn bz_collect(self: *VM) callconv(.c) void;
    pub extern fn bz_setTryCtx(self: *VM) callconv(.c) *TryCtx;
    pub extern fn bz_popTryCtx(self: *VM) callconv(.c) void;
    pub extern fn bz_closeUpValues(vm: *VM, last: *Value) callconv(.c) void;
    pub extern fn bz_getUpValue(ctx: *NativeCtx, slot: usize) callconv(.c) Value;
    pub extern fn bz_setUpValue(ctx: *NativeCtx, slot: usize, value: Value) callconv(.c) void;
    pub extern fn bz_closure(ctx: *NativeCtx, function_node: u32, native: *anyopaque, native_raw: *anyopaque) callconv(.c) Value;
    pub extern fn bz_bindMethod(vm: *VM, receiver: Value, method_value: Value, native_value: Value) callconv(.c) Value;
    pub extern fn bz_context(ctx: *NativeCtx, closure_value: Value, new_ctx: *NativeCtx, arg_count: usize) callconv(.c) *anyopaque;
    pub extern fn bz_clone(vm: *VM, value: Value) callconv(.c) Value;
    pub extern fn bz_currentFiber(vm: *VM) callconv(.c) Value;
    pub extern fn bz_dumpStack(vm: *VM) callconv(.c) void;
    pub extern fn bz_zigType(vm: *VM, ztype: [*]const u8, len: usize, expected_type: *Value) callconv(.c) ?*ZigType;
    pub extern fn bz_stringType(vm: *VM) callconv(.c) Value;
    pub extern fn bz_intType(vm: *VM) callconv(.c) Value;
    pub extern fn bz_mapType(vm: *VM, key_type: Value, value_type: Value, mutable: bool) callconv(.c) Value;
    pub extern fn bz_listType(vm: *VM, item_type: Value, mutable: bool) callconv(.c) Value;
    pub extern fn bz_getStringProperty(vm: *VM, string: Value, method_idx: usize) callconv(.c) Value;
    pub extern fn bz_getListProperty(vm: *VM, list: Value, property_idx: usize, bind: bool) callconv(.c) Value;
    pub extern fn bz_getMapProperty(vm: *VM, map: Value, property_idx: usize, bind: bool) callconv(.c) Value;
    pub extern fn bz_getPatternProperty(vm: *VM, pattern: Value, property_idx: usize) callconv(.c) Value;
    pub extern fn bz_getFiberProperty(vm: *VM, fiber: Value, property_idx: usize) callconv(.c) Value;
    pub extern fn bz_newRange(vm: *VM, low: i64, high: i64) callconv(.c) Value;
    pub extern fn bz_newList(vm: *VM, list_type: Value) callconv(.c) Value;
    pub extern fn bz_newMap(vm: *VM, map_type: Value) callconv(.c) Value;
    pub extern fn bz_newQualifiedObjectInstance(self: *VM, qualified_name: [*]const u8, len: usize, mutable: bool) callconv(.c) Value;
    pub extern fn bz_newObjectInstance(vm: *VM, object_value: Value, typedef_value: Value) callconv(.c) Value;
    pub extern fn bz_newForeignContainerInstance(vm: *VM, typedef_value: Value) callconv(.c) Value;
    pub extern fn bz_newForeignContainerFromSlice(vm: *VM, type_def: Value, ptr: [*]u8, len: usize) callconv(.c) Value;
    pub extern fn bz_readZigValueFromBuffer(vm: *VM, ztype: *ZigType, at: usize, buf: [*]u8, len: usize) callconv(.c) Value;
    pub extern fn bz_writeZigValueToBuffer(vm: *VM, value: Value, ztype: *const ZigType, at: usize, buf: [*]u8, capacity: usize) callconv(.c) void;
};

pub extern fn bz_memcpy(dest: [*]u8, dest_len: usize, source: [*]u8, source_len: usize) callconv(.c) void;
