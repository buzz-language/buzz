const std = @import("std");
const builtin = @import("builtin");

var gpa = std.heap.GeneralPurposeAllocator(.{
    .safety = true,
}){};

pub const GarbageCollector = opaque {};

pub const VM = opaque {
    pub extern fn bz_newVM(self: *VM) *VM;
    pub extern fn bz_deinitVM(self: *VM) void;
    pub extern fn bz_compile(self: *VM, source: ?[*]const u8, source_len: usize, file_name: ?[*]const u8, file_name_len: usize) ?*ObjFunction;
    pub extern fn bz_interpret(self: *VM, function: *ObjFunction) bool;
    pub extern fn bz_call(self: *VM, closure: *ObjClosure, arguments: [*]const *const Value, len: usize, catch_value: ?*Value) void;
    pub extern fn bz_push(self: *VM, value: *Value) void;
    pub extern fn bz_pop(self: *VM) *Value;
    pub extern fn bz_peek(self: *VM, distance: u32) *Value;
    pub extern fn bz_pushBool(self: *VM, value: bool) void;
    pub extern fn bz_pushFloat(self: *VM, value: f64) void;
    pub extern fn bz_pushInteger(self: *VM, value: i64) void;
    pub extern fn bz_pushString(self: *VM, value: *ObjString) void;
    pub extern fn bz_pushList(self: *VM, value: *ObjList) void;
    pub extern fn bz_pushUserData(self: *VM, value: *ObjUserData) void;
    pub extern fn bz_pushNull(self: *VM) void;
    pub extern fn bz_pushVoid(self: *VM) void;
    pub extern fn bz_pushObjectInstance(vm: *VM, payload: *ObjObjectInstance) void;
    pub extern fn bz_pushEnumInstance(vm: *VM, payload: *ObjEnumInstance) void;
    pub extern fn bz_pushError(self: *VM, qualified_name: [*]const u8, len: usize) void;
    pub extern fn bz_pushErrorEnum(self: *VM, qualified_name: [*]const u8, name_len: usize, case: [*]const u8, case_len: usize) void;
    pub extern fn bz_throw(vm: *VM, value: *Value) void;
    pub extern fn bz_throwString(vm: *VM, message: ?[*]const u8, len: usize) void;
    pub extern fn bz_getGC(vm: *VM) *GarbageCollector;
    pub extern fn bz_getQualified(self: *VM, qualified_name: [*]const u8, len: usize) ?*Value;

    pub extern fn bz_allocated(self: *VM) usize;

    pub extern fn bz_collect(self: *VM) bool;

    pub var allocator: std.mem.Allocator = if (builtin.mode == .Debug)
        gpa.allocator()
    else
        std.heap.c_allocator;
};

pub const Value = opaque {
    pub extern fn bz_valueToBool(value: *Value) bool;
    pub extern fn bz_valueToString(value: *Value, len: *usize) ?[*]const u8;
    pub extern fn bz_valueToInteger(value: *Value) i64;
    pub extern fn bz_valueToFloat(value: *Value) f64;
    pub extern fn bz_valueToUserData(value: *Value) *UserData;
    pub extern fn bz_valueIsInteger(value: *Value) bool;
    pub extern fn bz_valueIsFloat(value: *Value) bool;
    pub extern fn bz_valueDump(value: *Value, vm: *VM) void;
};

pub const ObjClosure = opaque {};

pub const ObjTypeDef = opaque {
    pub extern fn bz_boolType() ?*ObjTypeDef;
    pub extern fn bz_stringType() ?*ObjTypeDef;
    pub extern fn bz_voidType() ?*ObjTypeDef;
};

pub const ObjString = opaque {
    pub extern fn bz_string(vm: *VM, string: ?[*]const u8, len: usize) ?*ObjString;
};

pub const ObjList = opaque {
    pub extern fn bz_newList(vm: *VM, of_type: *ObjTypeDef) ?*ObjList;
    pub extern fn bz_listAppend(self: *ObjList, gc: *GarbageCollector, value: *Value) bool;
    pub extern fn bz_valueToList(value: *Value) *ObjList;
    pub extern fn bz_listGet(self: *ObjList, index: usize) *Value;
    pub extern fn bz_listLen(self: *ObjList) usize;
};

pub const ObjFunction = opaque {};

pub const UserData = anyopaque;

pub const ObjUserData = opaque {
    pub extern fn bz_newUserData(vm: *VM, userdata: *UserData) ?*ObjUserData;
    pub extern fn bz_getUserData(userdata: *ObjUserData) *UserData;
};

pub const ObjObjectInstance = opaque {};

pub const ObjObject = opaque {
    pub extern fn bz_valueToObject(value: *Value) *ObjObject;
    pub extern fn bz_instance(self: *ObjObject, vm: *VM) ?*ObjObjectInstance;
};

pub const ObjEnumInstance = opaque {};

pub const ObjEnum = opaque {
    pub extern fn bz_getEnumCase(self: *ObjEnum, vm: *VM, case: [*]const u8, len: usize) ?*ObjEnumInstance;
};
