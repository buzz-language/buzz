const std = @import("std");
const VM = @import("./vm.zig").VM;
const _obj = @import("./obj.zig");
const _value = @import("./value.zig");
const utils = @import("./utils.zig");
const memory = @import("./memory.zig");
const _parser = @import("./parser.zig");
const _codegen = @import("./codegen.zig");

const Value = _value.Value;
const valueToString = _value.valueToString;
const copyString = _obj.copyString;
const ObjString = _obj.ObjString;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjFunction = _obj.ObjFunction;
const ObjList = _obj.ObjList;
const ObjUserData = _obj.ObjUserData;
const UserData = _obj.UserData;
const Parser = _parser.Parser;
const CodeGen = _codegen.CodeGen;

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

/// Push a float value on the stack
export fn bz_pushNum(self: *VM, value: f64) void {
    self.push(Value{ .Number = value });
}

/// Push null on the stack
export fn bz_pushNull(self: *VM) void {
    self.push(Value{ .Null = null });
}

/// Push void on the stack
export fn bz_pushVoid(self: *VM) void {
    self.push(Value{ .Void = null });
}

/// Push string on the stack
export fn bz_pushString(self: *VM, value: *ObjString) void {
    self.push(value.toValue());
}

/// Push list on the stack
export fn bz_pushList(self: *VM, value: *ObjList) void {
    self.push(value.toValue());
}

/// Converts a value to a boolean
export fn bz_valueToBool(value: *Value) bool {
    return value.Boolean;
}

/// Converts a value to a string
export fn bz_valueToString(value: *Value) ?[*:0]const u8 {
    if (value.* != .Obj or value.Obj.obj_type != .String) {
        return null;
    }

    return utils.toCString(std.heap.c_allocator, ObjString.cast(value.Obj).?.string);
}

/// Converts a value to a number
export fn bz_valueToNumber(value: *Value) f64 {
    return value.Number;
}

// Obj manipulations

/// Converts a c string to a *ObjString
export fn bz_string(vm: *VM, string: [*:0]const u8) ?*ObjString {
    return copyString(vm, utils.toSlice(string)) catch null;
}

// Other stuff

// Type helpers

// TODO: should always return the same instance
/// Returns the [bool] type
export fn bz_boolType() ?*ObjTypeDef {
    var bool_type: ?*ObjTypeDef = std.heap.c_allocator.create(ObjTypeDef) catch null;

    if (bool_type == null) {
        return null;
    }

    bool_type.?.* = ObjTypeDef{ .def_type = .Bool, .optional = false };

    return bool_type;
}

/// Returns the [str] type
export fn bz_stringType() ?*ObjTypeDef {
    var bool_type: ?*ObjTypeDef = std.heap.c_allocator.create(ObjTypeDef) catch null;

    if (bool_type == null) {
        return null;
    }

    bool_type.?.* = ObjTypeDef{ .def_type = .String, .optional = false };

    return bool_type;
}

/// Returns the [void] type
export fn bz_voidType() ?*ObjTypeDef {
    var void_type: ?*ObjTypeDef = std.heap.c_allocator.create(ObjTypeDef) catch null;

    if (void_type == null) {
        return null;
    }

    void_type.?.* = ObjTypeDef{ .def_type = .Void, .optional = false };

    return void_type;
}

/// Creates a function type with no argument. Argument should be added with [bz_addFunctionArgument]
export fn bz_newFunctionType(vm: *VM, name: [*:0]const u8, return_type: ?*ObjTypeDef) ?*ObjTypeDef {
    // TODO: this obj is not in the GC
    var function_type: ?*ObjTypeDef = std.heap.c_allocator.create(ObjTypeDef) catch null;

    if (function_type == null) {
        return null;
    }

    var function_def = ObjFunction.FunctionDef{
        // If oom, empty string should not fail
        .name = (bz_string(vm, name) orelse bz_string(vm, @ptrCast([*:0]const u8, ""))).?,
        .return_type = return_type orelse bz_voidType().?,
        .parameters = std.StringArrayHashMap(*ObjTypeDef).init(std.heap.c_allocator),
        .has_defaults = std.StringArrayHashMap(bool).init(std.heap.c_allocator),
    };

    var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = function_def };

    function_type.?.* = .{ .def_type = .Function, .resolved_type = resolved_type };

    return function_type;
}

/// Adds a argument to a function definition. Returns false if could not add it.
export fn bz_addFunctionArgument(function_type: *ObjTypeDef, name: [*:0]const u8, arg_type: *ObjTypeDef) bool {
    function_type.resolved_type.?.Function.parameters.put(utils.toSlice(name), arg_type) catch {
        return false;
    };

    return true;
}

export fn bz_allocated(self: *VM) usize {
    return self.bytes_allocated;
}

export fn bz_collect(self: *VM) bool {
    memory.collectGarbage(self) catch {
        return false;
    };

    return true;
}

export fn bz_newList(vm: *VM, of_type: *ObjTypeDef) ?*ObjList {
    var list_def: ObjList.ListDef = ObjList.ListDef.init(
        vm.allocator,
        of_type,
    );

    var list_def_union: ObjTypeDef.TypeUnion = .{
        .List = list_def,
    };

    var list_def_type: *ObjTypeDef = _obj.allocateObject(vm, ObjTypeDef, ObjTypeDef{
        .def_type = .List,
        .optional = false,
        .resolved_type = list_def_union,
    }) catch {
        return null;
    };

    return _obj.allocateObject(
        vm,
        ObjList,
        ObjList.init(vm.allocator, list_def_type),
    ) catch {
        return null;
    };
}

export fn bz_listAppend(self: *ObjList, value: *Value) bool {
    self.rawAppend(value.*) catch {
        return false;
    };

    return true;
}

export fn bz_valueToList(value: *Value) *ObjList {
    return ObjList.cast(value.Obj).?;
}

export fn bz_listGet(self: *ObjList, index: usize) *Value {
    return &self.items.items[index];
}

export fn bz_listLen(self: *ObjList) usize {
    return self.items.items.len;
}

export fn bz_newUserData(vm: *VM, userdata: *UserData) ?*ObjUserData {
    return _obj.allocateObject(
        vm,
        ObjUserData,
        ObjUserData{ .userdata = userdata },
    ) catch {
        return null;
    };
}

export fn bz_throw(vm: *VM, value: *Value) void {
    vm.push(value.*);
}

export fn bz_throwString(vm: *VM, message: [*:0]const u8) void {
    bz_pushString(vm, bz_string(vm, message) orelse {
        _ = std.io.getStdErr().write(utils.toSlice(message)) catch unreachable;
        std.os.exit(1);
    });
}

export fn bz_newVM(self: *VM) ?*VM {
    var vm = self.allocator.create(VM) catch {
        return null;
    };
    vm.* = VM.init(self.allocator, self.strings) catch {
        return null;
    };

    return vm;
}

export fn bz_deinitVM(self: *VM) void {
    self.deinit();
}

export fn bz_compile(self: *VM, source: [*:0]const u8, file_name: [*:0]const u8) ?*ObjFunction {
    var imports = std.StringHashMap(Parser.ScriptImport).init(self.allocator);
    var strings = std.StringHashMap(*ObjString).init(self.allocator);
    var parser = Parser.init(self.allocator, self.strings, &imports, false);
    var codegen = CodeGen.init(self.allocator, &parser, self.strings, &parser.type_defs, false);
    defer {
        codegen.deinit();
        imports.deinit();
        parser.deinit();
        strings.deinit();
    }

    if (parser.parse(utils.toSlice(source), utils.toSlice(file_name)) catch null) |function_node| {
        return function_node.toByteCode(function_node, &codegen, null) catch null;
    } else {
        return null;
    }
}

export fn bz_interpret(self: *VM, function: *ObjFunction) bool {
    self.interpret(function, null) catch {
        return false;
    };

    return true;
}
