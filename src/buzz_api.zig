const std = @import("std");
const VM = @import("./vm.zig").VM;
const _obj = @import("./obj.zig");
const _value = @import("./value.zig");
const utils = @import("./utils.zig");

const Value = _value.Value;
const valueToString = _value.valueToString;
const ObjString = _obj.ObjString;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjFunction = _obj.ObjFunction;

// Stack manipulation

/// Push a Value to the stack
export fn bz_push(self: *VM, value: *Value) void {
    self.push(value.*);
}

/// Pop a Value from the stack and returns it
export fn bz_pop(self: *VM) *Value {
    self.stack_top -= 1;
    return @ptrCast(*Value, self.stack_top);
}

/// Peeks at the stack at [distance] from the stack top
export fn bz_peek(self: *VM, distance: u32) *Value {
    return @ptrCast(*Value, self.stack_top - 1 - distance);
}

// Value manipulations

/// Push a boolean value on the stack
export fn bz_pushBool(self: *VM, value: bool) void {
    self.push(Value{ .Boolean = value });
}

/// Converts a value to a boolean
export fn bz_valueToBool(value: *Value) bool {
    // TODO: should i type check ?
    return value.Boolean;
}

/// Converts a value to a string
export fn bz_valueToString(value: *Value) ?[*:0]const u8 {
    return utils.toCString(std.heap.c_allocator, ObjString.cast(value.Obj).?.string);
}

// Obj manipulations

/// Converts a c string to a *ObjString
export fn bz_string(string: [*:0]const u8) ?*ObjString {
    var obj_string: ?*ObjString = std.heap.c_allocator.create(ObjString) catch null;

    if (obj_string == null) {
        return null;
    }

    obj_string.?.* = ObjString {
        .string = utils.toSlice(string)
    };

    return obj_string.?;
}

// Other stuff

/// Throw an error with the given [message]
export fn bz_throw(self: *VM, message: [*:0]const u8) void {
    self.runtimeError(utils.toSlice(message), null) catch {
        // We can't return zig erros so we have to handles them here
        std.debug.warn("Error occured\n", .{});
    };
}

// Type helpers

// TODO: should always return the same instance
/// Returns the [bool] type
export fn bz_boolType() ?*ObjTypeDef {
    var bool_type: ?*ObjTypeDef = std.heap.c_allocator.create(ObjTypeDef) catch null;

    if (bool_type == null) {
        return null;
    }

    bool_type.?.* = ObjTypeDef {
        .def_type = .Bool,
        .optional = false
    };

    return bool_type;
}

/// Returns the [str] type
export fn bz_stringType() ?*ObjTypeDef {
    var bool_type: ?*ObjTypeDef = std.heap.c_allocator.create(ObjTypeDef) catch null;

    if (bool_type == null) {
        return null;
    }

    bool_type.?.* = ObjTypeDef {
        .def_type = .String,
        .optional = false
    };

    return bool_type;
}

/// Returns the [void] type
export fn bz_voidType() ?*ObjTypeDef {
    var void_type: ?*ObjTypeDef = std.heap.c_allocator.create(ObjTypeDef) catch null;

    if (void_type == null) {
        return null;
    }

    void_type.?.* = ObjTypeDef {
        .def_type = .Void,
        .optional = false
    };

    return void_type;
}

/// Creates a function type with no argument. Argument should be added with [bz_addFunctionArgument]
export fn bz_newFunctionType(name: [*:0]const u8, return_type: ?*ObjTypeDef) ?*ObjTypeDef {
    var function_type: ?*ObjTypeDef = std.heap.c_allocator.create(ObjTypeDef) catch null;

    if (function_type == null) {
        return null;
    }

    var function_def = ObjFunction.FunctionDef {
        // If oom, empty string should not fail
        .name = (bz_string(name) orelse bz_string(@ptrCast([*:0]const u8, ""))).?,
        .return_type = return_type orelse bz_voidType().?,
        .parameters = std.StringArrayHashMap(*ObjTypeDef).init(std.heap.c_allocator)
    };

    var resolved_type: ObjTypeDef.TypeUnion = .{
        .Function = function_def
    };

    function_type.?.* = .{
        .optional = false,
        .def_type = .Function,
        .resolved_type = resolved_type
    };

    return function_type;
}

/// Adds a argument to a function definition. Returns false if could not add it.
export fn bz_addFunctionArgument(function_type: *ObjTypeDef, name: [*:0]const u8, arg_type: *ObjTypeDef) bool {
    function_type.resolved_type.?.Function.parameters.put(utils.toSlice(name), arg_type) catch {
        return false;
    };

    return true;
}