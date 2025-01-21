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

/// TaggedMask + Sign bit indicates a pointer value.
const PointerMask: u64 = TaggedValueMask | SignMask;

const BooleanMask: u64 = TaggedValueMask | (@as(u64, TagBoolean) << 32);
const FalseMask: u64 = BooleanMask;
const TrueBitMask: u64 = 1;
const TrueMask: u64 = BooleanMask | TrueBitMask;

const IntegerMask: u64 = TaggedValueMask | (@as(u64, TagInteger) << 32);
const NullMask: u64 = TaggedValueMask | (@as(u64, TagNull) << 32);
const VoidMask: u64 = TaggedValueMask | (@as(u64, TagVoid) << 32);
const ErrorMask: u64 = TaggedValueMask | (@as(u64, TagError) << 32);

const TagMask: u32 = (1 << 3) - 1;
const TaggedPrimitiveMask = TaggedValueMask | (@as(u64, TagMask) << 32);

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

    pub inline fn double(self: Value) f64 {
        return @bitCast(self.val);
    }

    pub inline fn obj(self: Value) *anyopaque {
        return @ptrFromInt(self.val & ~PointerMask);
    }

    pub extern fn bz_valueToString(value: Value, len: *usize) ?[*]const u8;
    pub extern fn bz_valueToCString(value: Value) ?[*:0]const u8;
    pub extern fn bz_valueToForeignContainerPtr(value: Value) [*]u8;
    pub extern fn bz_valueIsForeignContainer(value: Value) bool;
    pub extern fn bz_valueDump(value: Value, vm: *VM) void;
    pub extern fn bz_valueEqual(self: Value, other: Value) Value;
    pub extern fn bz_valueIs(self: Value, type_def: Value) Value;
    pub extern fn bz_valueTypeOf(self: Value, vm: *VM) Value;
    pub extern fn bz_getUserDataPtr(userdata: Value) u64;
    pub extern fn bz_containerTypeSize(container: Value) usize;
    pub extern fn bz_containerTypeAlign(type_def: Value) usize;
    pub extern fn bz_valueCastToString(value: Value, vm: *VM) Value;
    pub extern fn bz_stringConcat(string: Value, other: Value, vm: *VM) Value;
    pub extern fn bz_stringSubscript(obj_string: Value, index_value: Value, checked: bool, vm: *VM) Value;
    pub extern fn bz_stringNext(string_value: Value, index: *Value, vm: *VM) Value;
    pub extern fn bz_rangeNext(range_value: Value, index_slot: Value) Value;
    pub extern fn bz_getRangeProperty(range_value: Value, property_idx: usize, bind: bool, vm: *VM) Value;
    pub extern fn bz_listAppend(list: Value, value: Value, vm: *VM) void;
    pub extern fn bz_listGet(list: Value, index: i32, checked: bool) Value;
    pub extern fn bz_listSet(list: Value, index: usize, value: Value, vm: *VM) void;
    pub extern fn bz_listLen(list: Value) usize;
    pub extern fn bz_listConcat(list: Value, other_list: Value, vm: *VM) Value;
    pub extern fn bz_listNext(list_value: Value, index: *Value, vm: *VM) Value;
    pub extern fn bz_mapSet(map: Value, key: Value, value: Value, vm: *VM) void;
    pub extern fn bz_mapGet(map: Value, key: Value) Value;
    pub extern fn bz_mapConcat(map: Value, other_map: Value, vm: *VM) Value;
    pub extern fn bz_mapNext(map_value: Value, index: *Value) Value;
    pub extern fn bz_setObjectInstanceProperty(instance_value: Value, property_idx: usize, value: Value, vm: *VM) void;
    pub extern fn bz_getObjectInstanceProperty(instance_value: Value, property_idx: usize) Value;
    pub extern fn bz_getObjectInstanceMethod(instance_value: Value, method_idx: usize, bind: bool, vm: *VM) Value;
    pub extern fn bz_getProtocolMethod(instance_value: Value, method_name: Value, vm: *VM) Value;
    pub extern fn bz_getObjectField(object_value: Value, field_idx: usize) Value;
    pub extern fn bz_setObjectField(object_value: Value, field_idx: usize, value: Value, vm: *VM) void;
    pub extern fn bz_getEnumInstanceValue(enum_instance_value: Value) Value;
    pub extern fn bz_getEnumCase(enum_value: Value, case_name_value: Value, vm: *VM) Value;
    pub extern fn bz_getEnumCaseFromValue(enum_value: Value, case_value: Value, vm: *VM) Value;
    pub extern fn bz_enumNext(enum_value: Value, case: Value, vm: *VM) Value;
    pub extern fn bz_foreignContainerGet(value: Value, field_idx: usize, vm: *VM) Value;
    pub extern fn bz_foreignContainerSet(value: Value, field_idx: usize, new_value: Value, vm: *VM) void;
    pub extern fn bz_foreignContainerSlice(container_value: Value, len: *usize) [*]u8;
};

pub const NativeCtx = extern struct {
    vm: *VM,
    globals: [*]Value,
    upvalues: [*]*anyopaque,
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
    pub extern fn bz_zigTypeToCString(self: *ZigType, vm: *VM) [*:0]const u8;
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

    pub extern fn bz_newVM(self: *VM) *VM;
    pub extern fn bz_deinitVM(self: *VM) void;
    pub extern fn bz_panic(vm: *VM, msg: [*]const u8, len: usize) void;
    pub extern fn bz_run(self: *VM, source: ?[*]const u8, source_len: usize, file_name: ?[*]const u8, file_name_len: usize) bool;
    pub extern fn bz_call(self: *VM, closure: Value, arguments: ?[*]const *const Value, len: usize, catch_value: ?*Value) void;
    pub extern fn bz_push(self: *VM, value: Value) void;
    pub extern fn bz_pop(self: *VM) Value;
    pub extern fn bz_peek(self: *VM, distance: u32) Value;
    pub extern fn bz_at(vm: *VM, at: u32) Value;
    pub extern fn bz_pushError(self: *VM, qualified_name: [*]const u8, len: usize, message: ?[*]const u8, mlen: usize) void;
    pub extern fn bz_pushErrorEnum(self: *VM, qualified_name: [*]const u8, name_len: usize, case: [*]const u8, case_len: usize) void;
    pub extern fn bz_stringToValue(vm: *VM, string: ?[*]const u8, len: usize) Value;
    pub extern fn bz_stringToValueZ(vm: *VM, string: ?[*:0]const u8) Value;
    pub extern fn bz_newUserData(vm: *VM, userdata: u64) Value;
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
    pub extern fn bz_closure(ctx: *NativeCtx, function_node: u32, native: *anyopaque, native_raw: *anyopaque) Value;
    pub extern fn bz_bindMethod(vm: *VM, receiver: Value, method_value: Value, native_value: Value) Value;
    pub extern fn bz_context(ctx: *NativeCtx, closure_value: Value, new_ctx: *NativeCtx, arg_count: usize) *anyopaque;
    pub extern fn bz_clone(vm: *VM, value: Value) Value;
    pub extern fn bz_currentFiber(vm: *VM) Value;
    pub extern fn bz_dumpStack(vm: *VM) void;
    pub extern fn bz_zigType(vm: *VM, ztype: [*]const u8, len: usize, expected_type: *Value) ?*ZigType;
    pub extern fn bz_stringType(vm: *VM) Value;
    pub extern fn bz_mapType(vm: *VM, key_type: Value, value_type: Value, mutable: bool) Value;
    pub extern fn bz_listType(vm: *VM, item_type: Value, mutable: bool) Value;
    pub extern fn bz_getStringProperty(vm: *VM, string: Value, method_idx: usize) Value;
    pub extern fn bz_getListProperty(vm: *VM, list: Value, property_idx: usize, bind: bool) Value;
    pub extern fn bz_getMapProperty(vm: *VM, map: Value, property_idx: usize, bind: bool) Value;
    pub extern fn bz_getPatternProperty(vm: *VM, pattern: Value, property_idx: usize) Value;
    pub extern fn bz_getFiberProperty(vm: *VM, fiber: Value, property_idx: usize) Value;
    pub extern fn bz_newRange(vm: *VM, low: i32, high: i32) Value;
    pub extern fn bz_newList(vm: *VM, list_type: Value) Value;
    pub extern fn bz_newMap(vm: *VM, map_type: Value) Value;
    pub extern fn bz_newQualifiedObjectInstance(self: *VM, qualified_name: [*]const u8, len: usize, mutable: bool) Value;
    pub extern fn bz_newObjectInstance(vm: *VM, object_value: Value, typedef_value: Value) Value;
    pub extern fn bz_newForeignContainerInstance(vm: *VM, typedef_value: Value) Value;
    pub extern fn bz_newForeignContainerFromSlice(vm: *VM, type_def: Value, ptr: [*]u8, len: usize) Value;
    pub extern fn bz_readZigValueFromBuffer(vm: *VM, ztype: *ZigType, at: usize, buf: [*]u8, len: usize) Value;
    pub extern fn bz_writeZigValueToBuffer(vm: *VM, value: Value, ztype: *const ZigType, at: usize, buf: [*]u8, capacity: usize) void;
};

pub extern fn bz_memcpy(dest: [*]u8, dest_len: usize, source: [*]u8, source_len: usize) void;
