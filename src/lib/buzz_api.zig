const std = @import("std");
const builtin = @import("builtin");
const BuildOptions = @import("build_options");

var gpa = std.heap.GeneralPurposeAllocator(.{
    .safety = true,
}){};

pub const GarbageCollector = opaque {};

pub const ObjUpValue = opaque {};

pub const ObjNative = opaque {
    pub extern fn bz_toObjNative(value: Value) *ObjNative;
    pub extern fn bz_toObjNativeOpt(value: Value) ?*ObjNative;
};

pub const ObjFunction = opaque {};

pub const NativeCtx = extern struct {
    vm: *VM,
    globals: [*]Value,
    upvalues: [*]*ObjUpValue,
};

pub const VM = opaque {
    pub extern fn bz_newVM(self: *VM) *VM;
    pub extern fn bz_deinitVM(self: *VM) void;
    pub extern fn bz_compile(self: *VM, source: ?[*]const u8, source_len: usize, file_name: ?[*]const u8, file_name_len: usize) ?*ObjFunction;
    pub extern fn bz_interpret(self: *VM, function: *ObjFunction) bool;
    pub extern fn bz_call(self: *VM, closure: *ObjClosure, arguments: [*]const *const Value, len: usize, catch_value: ?*Value) void;
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
    pub extern fn bz_pushError(self: *VM, qualified_name: [*]const u8, len: usize) void;
    pub extern fn bz_pushErrorEnum(self: *VM, qualified_name: [*]const u8, name_len: usize, case: [*]const u8, case_len: usize) void;
    pub extern fn bz_throw(vm: *VM, value: Value) void;
    pub extern fn bz_throwString(vm: *VM, message: ?[*]const u8, len: usize) void;
    pub extern fn bz_getGC(vm: *VM) *GarbageCollector;
    pub extern fn bz_getQualified(self: *VM, qualified_name: [*]const u8, len: usize) Value;

    pub extern fn bz_allocated(self: *VM) usize;

    pub extern fn bz_collect(self: *VM) bool;

    pub extern fn bz_jitFunction(self: *VM, function: *ObjClosure) void;

    pub var allocator: std.mem.Allocator = if (builtin.mode == .Debug)
        gpa.allocator()
    else if (BuildOptions.use_mimalloc)
        @import("../mimalloc.zig").mim_allocator
    else
        std.heap.c_allocator;
};

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
        return .{ .val = IntegerMask | @bitCast(u32, val) };
    }

    pub inline fn fromFloat(val: f64) Value {
        return .{ .val = @bitCast(u64, val) };
    }

    pub inline fn fromObj(val: *anyopaque) Value {
        return .{ .val = PointerMask | @ptrToInt(val) };
    }

    pub inline fn getTag(self: Value) u3 {
        return @intCast(Tag, @intCast(u32, self.val >> 32) & TagMask);
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
        return @bitCast(i32, @intCast(u32, self.val & 0xffffffff));
    }

    pub inline fn float(self: Value) f64 {
        return @bitCast(f64, self.val);
    }

    pub inline fn obj(self: Value) *anyopaque {
        return @intToPtr(*anyopaque, self.val & ~PointerMask);
    }

    pub extern fn bz_valueToString(value: Value, len: *usize) ?[*]const u8;
    pub extern fn bz_valueToUserData(value: Value) *UserData;
    pub extern fn bz_valueDump(value: Value, vm: *VM) void;

    pub extern fn bz_valueIsBuzzFn(value: Value) bool;
    pub extern fn bz_valueToClosure(value: Value) *ObjClosure;
    pub extern fn bz_valueToRawNativeFn(value: Value) *anyopaque;
};

pub const ObjClosure = opaque {};

pub const ObjTypeDef = opaque {
    pub extern fn bz_boolType() ?*ObjTypeDef;
    pub extern fn bz_stringType() Value;
    pub extern fn bz_voidType() ?*ObjTypeDef;
};

pub const ObjString = opaque {
    pub extern fn bz_string(vm: *VM, string: ?[*]const u8, len: usize) ?*ObjString;
    pub extern fn bz_objStringToString(obj_string: *ObjString, len: *usize) ?[*]const u8;
    pub extern fn bz_objStringToValue(obj_string: *ObjString) Value;
    pub extern fn bz_objStringConcat(vm: *VM, obj_string: Value, other: Value) Value;
    pub extern fn bz_toString(vm: *VM, value: Value) Value;
};

pub const ObjList = opaque {
    pub extern fn bz_newList(vm: *VM, of_type: Value) Value;
    pub extern fn bz_listAppend(vm: *VM, list: Value, value: Value) void;
    pub extern fn bz_valueToList(value: Value) *ObjList;
    pub extern fn bz_listGet(self: *ObjList, index: usize) Value;
    pub extern fn bz_listLen(self: *ObjList) usize;
    pub extern fn bz_listMethod(vm: *VM, list: Value, member: [*]const u8, member_len: usize) Value;
};

pub const UserData = anyopaque;

pub const ObjUserData = opaque {
    pub extern fn bz_newUserData(vm: *VM, userdata: *UserData) ?*ObjUserData;
    pub extern fn bz_getUserData(userdata: *ObjUserData) *UserData;
    pub extern fn bz_userDataToValue(userdata: *ObjUserData) Value;
};

pub const ObjObjectInstance = opaque {};

pub const ObjObject = opaque {
    pub extern fn bz_valueToObject(value: Value) *ObjObject;
    pub extern fn bz_instance(self: *ObjObject, vm: *VM) ?*ObjObjectInstance;
};

pub const ObjEnumInstance = opaque {};

pub const ObjEnum = opaque {
    pub extern fn bz_getEnumCase(self: *ObjEnum, vm: *VM, case: [*]const u8, len: usize) ?*ObjEnumInstance;
};
