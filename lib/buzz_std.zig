const std = @import("std");
const api = @import("buzz_api.zig");

export fn assert(vm: *api.VM) bool {
    var condition: bool = vm.bz_peek(1).bz_valueToBool();

    if (!condition) {
        vm.bz_throw(vm.bz_peek(0).bz_valueToString().?);
    }

    return false;
}

export fn assertTypeDef() *api.ObjTypeDef {
    var type_def: *api.ObjTypeDef = api.ObjTypeDef.bz_newFunctionType("assert", null).?;
    _ = type_def.bz_addFunctionArgument("condition", api.ObjTypeDef.bz_boolType().?);
    _ = type_def.bz_addFunctionArgument("message", api.ObjTypeDef.bz_stringType().?);

    return type_def;
}

export fn print(vm: *api.VM) bool {
    _ = std.io.getStdOut().write(std.mem.span(vm.bz_peek(0).bz_valueToString().?)) catch {
        vm.bz_throw("Could not print.");
        return false;
    };

    return false;
}

export fn printTypeDef() *api.ObjTypeDef {
    var type_def: *api.ObjTypeDef = api.ObjTypeDef.bz_newFunctionType("assert", null).?;
    _ = type_def.bz_addFunctionArgument("value", api.ObjTypeDef.bz_stringType().?);

    return type_def;
}

export fn openLib() [*:0]const u8 {
    return "assert,print";
}