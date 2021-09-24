// zig fmt: off
const std = @import("std");

const VM = opaque {
    extern fn bz_push(self: *VM, value: *Value) void;
    extern fn bz_pop(self: *VM) *Value;
    extern fn bz_peek(self: *VM, distance: u32) *Value;
    extern fn bz_pushBool(self: *VM, value: bool) void;
    extern fn bz_throw(self: *VM, message: [*:0]const u8) void;
};

const Value = opaque {
    extern fn bz_valueToBool(value: *Value) bool;
    extern fn bz_valueToString(value: *Value) ?[*:0]const u8;
};

const ObjTypeDef = opaque {
    extern fn bz_boolType() ?*ObjTypeDef;
    extern fn bz_stringType() ?*ObjTypeDef;
    extern fn bz_voidType() ?*ObjTypeDef;
    extern fn bz_newFunctionType(name: [*:0]const u8, return_type: ?*ObjTypeDef) ?*ObjTypeDef;
    extern fn bz_addFunctionArgument(function_type: *ObjTypeDef, name: [*:0]const u8, arg_type: *ObjTypeDef) bool;
};

const ObjString = opaque {
    extern fn bz_string(string: [*:0]const u8) ?*ObjString;
};

export fn assert(vm: *VM) bool {
    var condition: bool = vm.bz_peek(1).bz_valueToBool();

    if (!condition) {
        vm.bz_throw(vm.bz_peek(0).bz_valueToString().?);
    }

    return false;
}

export fn assertTypeDef() *ObjTypeDef {
    var type_def: *ObjTypeDef = ObjTypeDef.bz_newFunctionType("assert", null).?;
    _ = type_def.bz_addFunctionArgument("condition", ObjTypeDef.bz_boolType().?);
    _ = type_def.bz_addFunctionArgument("message", ObjTypeDef.bz_stringType().?);

    return type_def;
}

export fn openLib() [*:0]const u8 {
    return "assert";
}

// zig build-lib -dynamic tests/utils/testing.zig -lbuzz -Lzig-out/lib -rpath zig-out/lib