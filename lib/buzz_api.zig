const std = @import("std");
const builtin = @import("builtin");

var gpa = std.heap.GeneralPurposeAllocator(.{
    .safety = true,
}){};

pub const GarbageCollector = opaque {};

pub const VM = opaque {
    pub extern fn bz_newVM(self: *VM) *VM;
    pub extern fn bz_deinitVM(self: *VM) void;
    pub extern fn bz_compile(self: *VM, source: [*:0]const u8, file_name: [*:0]const u8) ?*ObjFunction;
    pub extern fn bz_interpret(self: *VM, function: *ObjFunction) bool;
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
    pub extern fn bz_throw(vm: *VM, value: *Value) void;
    pub extern fn bz_throwString(vm: *VM, message: [*:0]const u8) void;
    pub extern fn bz_getGC(vm: *VM) *GarbageCollector;

    pub extern fn bz_allocated(self: *VM) usize;

    pub extern fn bz_collect(self: *VM) bool;

    pub var allocator: std.mem.Allocator = if (builtin.mode == .Debug)
        gpa.allocator()
    else
        std.heap.c_allocator;
};

pub const Value = opaque {
    pub extern fn bz_valueToBool(value: *Value) bool;
    pub extern fn bz_valueToString(value: *Value) ?[*:0]const u8;
    pub extern fn bz_valueToInteger(value: *Value) i64;
    pub extern fn bz_valueToFloat(value: *Value) f64;
    pub extern fn bz_valueToUserData(value: *Value) *UserData;
    pub extern fn bz_valueIsInteger(value: *Value) bool;
    pub extern fn bz_valueIsFloat(value: *Value) bool;
};

pub const ObjTypeDef = opaque {
    pub extern fn bz_boolType() ?*ObjTypeDef;
    pub extern fn bz_stringType() ?*ObjTypeDef;
    pub extern fn bz_voidType() ?*ObjTypeDef;
};

pub const ObjString = opaque {
    pub extern fn bz_string(vm: *VM, string: [*:0]const u8) ?*ObjString;
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
