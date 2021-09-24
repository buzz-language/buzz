pub const VM = opaque {
    pub extern fn bz_push(self: *VM, value: *Value) void;
    pub extern fn bz_pop(self: *VM) *Value;
    pub extern fn bz_peek(self: *VM, distance: u32) *Value;
    pub extern fn bz_pushBool(self: *VM, value: bool) void;
    pub extern fn bz_throw(self: *VM, message: [*:0]const u8) void;
};

pub const Value = opaque {
    pub extern fn bz_valueToBool(value: *Value) bool;
    pub extern fn bz_valueToString(value: *Value) ?[*:0]const u8;
};

pub const ObjTypeDef = opaque {
    pub extern fn bz_boolType() ?*ObjTypeDef;
    pub extern fn bz_stringType() ?*ObjTypeDef;
    pub extern fn bz_voidType() ?*ObjTypeDef;
    pub extern fn bz_newFunctionType(name: [*:0]const u8, return_type: ?*ObjTypeDef) ?*ObjTypeDef;
    pub extern fn bz_addFunctionArgument(function_type: *ObjTypeDef, name: [*:0]const u8, arg_type: *ObjTypeDef) bool;
};

pub const ObjString = opaque {
    pub extern fn bz_string(string: [*:0]const u8) ?*ObjString;
};