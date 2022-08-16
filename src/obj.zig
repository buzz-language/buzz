const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;
const StringHashMap = std.StringHashMap;
const Chunk = @import("./chunk.zig").Chunk;
const _vm = @import("./vm.zig");
const VM = _vm.VM;
const Fiber = _vm.Fiber;
const Parser = @import("./parser.zig").Parser;
const _memory = @import("./memory.zig");
const _value = @import("./value.zig");
const Token = @import("./token.zig").Token;
const Config = @import("./config.zig").Config;
const CodeGen = @import("./codegen.zig").CodeGen;
const utils = @import("./utils.zig");

pub const pcre = @import("./pcre.zig").pcre;

const Value = _value.Value;
const HashableValue = _value.HashableValue;
const ValueType = _value.ValueType;
const valueToHashable = _value.valueToHashable;
const hashableToValue = _value.hashableToValue;
const valueToString = _value.valueToString;
const valueEql = _value.valueEql;
const valueIs = _value.valueIs;
const valueTypeEql = _value.valueTypeEql;
const allocate = _memory.allocate;
const allocateMany = _memory.allocateMany;
const free = _memory.free;
const freeMany = _memory.freeMany;
const markObj = _memory.markObj;
const markValue = _memory.markValue;
const collectGarbage = _memory.collectGarbage;

pub const ObjType = enum {
    String,
    Type,
    UpValue,
    Closure,
    Function,
    ObjectInstance,
    Object,
    List,
    Map,
    Enum,
    EnumInstance,
    Bound,
    Native,
    UserData,
    Pattern,
    Fiber,
};

pub fn allocateObject(vm: *VM, comptime T: type, data: T) !*T {
    // var before: usize = vm.bytes_allocated;

    var obj: *T = try allocate(vm, T);
    obj.* = data;

    var object: *Obj = switch (T) {
        ObjString => ObjString.toObj(obj),
        ObjTypeDef => ObjTypeDef.toObj(obj),
        ObjUpValue => ObjUpValue.toObj(obj),
        ObjClosure => ObjClosure.toObj(obj),
        ObjFunction => ObjFunction.toObj(obj),
        ObjObjectInstance => ObjObjectInstance.toObj(obj),
        ObjObject => ObjObject.toObj(obj),
        ObjList => ObjList.toObj(obj),
        ObjMap => ObjMap.toObj(obj),
        ObjEnum => ObjEnum.toObj(obj),
        ObjEnumInstance => ObjEnumInstance.toObj(obj),
        ObjBoundMethod => ObjBoundMethod.toObj(obj),
        ObjNative => ObjNative.toObj(obj),
        ObjUserData => ObjUserData.toObj(obj),
        ObjPattern => ObjPattern.toObj(obj),
        ObjFiber => ObjFiber.toObj(obj),
        else => {},
    };

    // if (Config.debug_gc) {
    //     std.debug.print("allocated {*} {*}\n", .{ obj, object });
    //     std.debug.print("(from {}) {} allocated, total {}\n", .{ before, @sizeOf(T), vm.bytes_allocated });
    // }

    // Add new object at start of vm.objects linked list
    object.next = vm.objects;
    vm.objects = object;

    return obj;
}

pub fn allocateString(vm: *VM, chars: []const u8) !*ObjString {
    if (vm.strings.get(chars)) |interned| {
        return interned;
    } else {
        var string: *ObjString = try allocateObject(vm, ObjString, ObjString{ .string = chars });

        vm.push(Value{ .Obj = string.toObj() });
        try vm.strings.put(chars, string);
        _ = vm.pop();

        return string;
    }
}

pub fn copyString(vm: *VM, chars: []const u8) !*ObjString {
    if (vm.strings.get(chars)) |interned| {
        return interned;
    }

    var copy: []u8 = try allocateMany(vm, u8, chars.len);
    mem.copy(u8, copy, chars);

    return try allocateString(vm, copy);
}

pub fn copyStringRaw(strings: *std.StringHashMap(*ObjString), allocator: Allocator, chars: []const u8, owned: bool) !*ObjString {
    if (strings.get(chars)) |interned| {
        return interned;
    }

    var copy: []u8 = undefined;
    if (!owned) {
        copy = try allocator.alloc(u8, chars.len);
        mem.copy(u8, copy, chars);
    }

    var obj_string: *ObjString = try allocator.create(ObjString);
    obj_string.* = ObjString{
        .string = if (owned) chars else copy,
    };

    try strings.put(chars, obj_string);

    return obj_string;
}

pub const Obj = struct {
    const Self = @This();

    obj_type: ObjType,
    is_marked: bool = false,
    next: ?*Obj = null,

    pub fn is(self: *Self, type_def: *ObjTypeDef) bool {
        return switch (self.obj_type) {
            .String => type_def.def_type == .String,
            .Pattern => type_def.def_type == .Pattern,
            .Fiber => type_def.def_type == .Fiber,

            .Type, .Object, .Enum => type_def.def_type == .Type,

            .ObjectInstance => type_def.def_type == .Object and ObjObjectInstance.cast(self).?.is(null, type_def),
            .EnumInstance => type_def.def_type == .Enum and ObjEnumInstance.cast(self).?.enum_ref.type_def == type_def,
            .Function => function: {
                const function: *ObjFunction = ObjFunction.cast(self).?;
                break :function function.type_def.eql(type_def);
            },

            .UpValue => upvalue: {
                const upvalue: *ObjUpValue = ObjUpValue.cast(self).?;
                break :upvalue valueIs(
                    Value{ .Obj = type_def.toObj() },
                    upvalue.closed orelse upvalue.location.*,
                );
            },
            .Closure => ObjClosure.cast(self).?.function.toObj().is(type_def),
            .List => ObjList.cast(self).?.type_def.eql(type_def),
            .Map => ObjMap.cast(self).?.type_def.eql(type_def),
            .Bound => bound: {
                const bound: *ObjBoundMethod = ObjBoundMethod.cast(self).?;
                break :bound valueIs(
                    Value{ .Obj = type_def.toObj() },
                    Value{ .Obj = if (bound.closure) |cls| cls.function.toObj() else bound.native.?.toObj() },
                );
            },

            .UserData, .Native => unreachable, // TODO: we don't know how to embark NativeFn type at runtime yet
        };
    }

    pub fn typeEql(self: *Self, type_def: *ObjTypeDef) bool {
        return switch (self.obj_type) {
            .Pattern => type_def.def_type == .Pattern,
            .String => type_def.def_type == .String,
            .Type => type_def.def_type == .Type,
            .UpValue => uv: {
                var upvalue: *ObjUpValue = ObjUpValue.cast(self).?;
                break :uv valueTypeEql(upvalue.closed orelse upvalue.location.*, type_def);
            },
            .EnumInstance => ei: {
                var instance: *ObjEnumInstance = ObjEnumInstance.cast(self).?;
                break :ei type_def.def_type == .EnumInstance and instance.enum_ref.type_def.eql(type_def.resolved_type.?.EnumInstance);
            },
            .ObjectInstance => oi: {
                var instance: *ObjObjectInstance = ObjObjectInstance.cast(self).?;
                break :oi type_def.def_type == .ObjectInstance and instance.is(null, type_def.resolved_type.?.ObjectInstance);
            },
            .Enum => ObjEnum.cast(self).?.type_def.eql(type_def),
            .Object => ObjObject.cast(self).?.type_def.eql(type_def),
            .Function => ObjFunction.cast(self).?.type_def.eql(type_def),
            .Closure => ObjClosure.cast(self).?.function.type_def.eql(type_def),
            .Bound => bound: {
                var bound = ObjBoundMethod.cast(self).?;
                break :bound if (bound.closure) |cls| cls.function.type_def.eql(type_def) else unreachable; // TODO
            },
            .List => ObjList.cast(self).?.type_def.eql(type_def),
            .Map => ObjMap.cast(self).?.type_def.eql(type_def),
            .Fiber => ObjFiber.cast(self).?.type_def.eql(type_def),
            .UserData, .Native => unreachable, // TODO
        };
    }

    pub fn eql(self: *Self, other: *Self) bool {
        if (self.obj_type != other.obj_type) {
            return false;
        }

        switch (self.obj_type) {
            .Pattern => {
                return mem.eql(u8, ObjPattern.cast(self).?.source, ObjPattern.cast(other).?.source);
            },
            .String => {
                // return mem.eql(u8, ObjString.cast(self).?.string, ObjString.cast(other).?.string);

                // since string are interned this should be enough
                return self == other;
            },
            .Type => {
                const self_type: *ObjTypeDef = ObjTypeDef.cast(self).?;
                const other_type: *ObjTypeDef = ObjTypeDef.cast(other).?;

                return self_type.optional == other_type.optional and self_type.eql(other_type);
            },
            .UpValue => {
                const self_upvalue: *ObjUpValue = ObjUpValue.cast(self).?;
                const other_upvalue: *ObjUpValue = ObjUpValue.cast(other).?;

                return valueEql(self_upvalue.closed orelse self_upvalue.location.*, other_upvalue.closed orelse other_upvalue.location.*);
            },
            .EnumInstance => {
                const self_enum_instance: *ObjEnumInstance = ObjEnumInstance.cast(self).?;
                const other_enum_instance: *ObjEnumInstance = ObjEnumInstance.cast(other).?;

                return self_enum_instance.enum_ref == other_enum_instance.enum_ref and self_enum_instance.case == other_enum_instance.case;
            },
            .Bound,
            .Closure,
            .Function,
            .ObjectInstance,
            .Object,
            .List,
            .Map,
            .Enum,
            .Native,
            .UserData,
            .Fiber,
            => {
                return self == other;
            },
        }
    }
};

pub const ObjFiber = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Fiber },

    fiber: *Fiber,

    type_def: *ObjTypeDef,

    var members: ?std.StringArrayHashMap(*ObjNative) = null;
    var memberDefs: ?std.StringHashMap(*ObjTypeDef) = null;

    pub fn mark(self: *Self, vm: *VM) !void {
        try _memory.markFiber(vm, self.fiber);
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Fiber) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    pub fn over(vm: *VM) c_int {
        var self = Self.cast(vm.peek(0).Obj).?;

        vm.push(Value{ .Boolean = self.fiber.status == .Over });

        return 1;
    }

    pub fn rawMember(method: []const u8) ?NativeFn {
        if (mem.eql(u8, method, "over")) {
            return over;
        }

        return null;
    }

    pub fn member(vm: *VM, method: []const u8) !?*ObjNative {
        if (Self.members) |umembers| {
            if (umembers.get(method)) |umethod| {
                return umethod;
            }
        }

        Self.members = Self.members orelse std.StringArrayHashMap(*ObjNative).init(vm.allocator);

        var nativeFn: ?NativeFn = rawMember(method);

        if (nativeFn) |unativeFn| {
            var native: *ObjNative = try allocateObject(
                vm,
                ObjNative,
                .{
                    .native = unativeFn,
                },
            );

            try Self.members.?.put(method, native);

            return native;
        }

        return null;
    }

    pub fn memberDef(parser: *Parser, method: []const u8) !?*ObjTypeDef {
        if (Self.memberDefs) |umembers| {
            if (umembers.get(method)) |umethod| {
                return umethod;
            }
        }

        Self.memberDefs = Self.memberDefs orelse std.StringHashMap(*ObjTypeDef).init(parser.allocator);

        // over() > bool
        if (mem.eql(u8, method, "over")) {
            var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

            // We omit first arg: it'll be OP_SWAPed in and we already parsed it
            // It's always the fiber.

            var method_def = ObjFunction.FunctionDef{
                .name = try copyStringRaw(parser.strings, parser.allocator, "over", false),
                .parameters = parameters,
                .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                .return_type = try parser.type_registry.getTypeDef(ObjTypeDef{
                    .def_type = .Bool,
                }),
                .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
            };

            var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

            var native_type = try parser.type_registry.getTypeDef(
                ObjTypeDef{
                    .def_type = .Native,
                    .resolved_type = resolved_type,
                },
            );

            try Self.memberDefs.?.put("over", native_type);

            return native_type;
        }

        return null;
    }

    pub const FiberDef = struct {
        const SelfFiberDef = @This();

        return_type: *ObjTypeDef,
        yield_type: *ObjTypeDef,

        pub fn mark(self: *SelfFiberDef, vm: *VM) !void {
            try markObj(vm, self.return_type.toObj());
            try markObj(vm, self.yield_type.toObj());
        }
    };
};

// Patterns are pcre regex, @see https://www.pcre.org/original/doc/html/index.html
pub const ObjPattern = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Pattern },

    source: []const u8,
    pattern: *pcre.struct_real_pcre8_or_16,

    var members: ?std.StringArrayHashMap(*ObjNative) = null;
    var memberDefs: ?std.StringHashMap(*ObjTypeDef) = null;

    pub fn mark(_: *Self, vm: *VM) !void {
        if (Self.members) |umembers| {
            var it = umembers.iterator();
            while (it.next()) |kv| {
                try markObj(vm, kv.value_ptr.*.toObj());
            }
        }

        if (Self.memberDefs) |umemberDefs| {
            var it = umemberDefs.iterator();
            while (it.next()) |kv| {
                try markObj(vm, kv.value_ptr.*.toObj());
            }
        }
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Pattern) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    fn rawMatch(self: *Self, vm: *VM, subject: [:0]const u8, offset: *usize) !?*ObjList {
        var results: ?*ObjList = null;

        var output_vector: [3000]c_int = undefined;

        const rc = pcre.pcre_exec(
            self.pattern, // the compiled pattern
            null, // no extra data - we didn't study the pattern
            @ptrCast([*c]const u8, subject), // the subject string
            @intCast(c_int, subject.len), // the length of the subject
            @intCast(c_int, offset.*), // start offset
            0, // default options
            @ptrCast([*c]c_int, &output_vector), // output vector for substring information
            output_vector.len, // number of elements in the output vector
        );

        switch (rc) {
            pcre.PCRE_ERROR_UNSET...pcre.PCRE_ERROR_NOMATCH => return null,
            // TODO: handle ouptut_vector too small
            0 => unreachable,
            else => {
                offset.* = @intCast(usize, output_vector[1]);

                results = try allocateObject(
                    vm,
                    ObjList,
                    ObjList.init(vm.allocator, try allocateObject(
                        vm,
                        ObjTypeDef,
                        ObjTypeDef{
                            .def_type = .String,
                        },
                    )),
                );

                // Prevent gc collection
                vm.push(results.?.toValue());

                var i: usize = 0;
                while (i < rc) : (i += 1) {
                    try results.?.items.append(
                        (try copyString(
                            vm,
                            subject[@intCast(usize, output_vector[2 * i])..@intCast(usize, output_vector[2 * i + 1])],
                        )).toValue(),
                    );
                }

                _ = vm.pop();
            },
        }

        return results;
    }

    fn rawMatchAll(self: *Self, vm: *VM, subject: [:0]const u8) !?*ObjList {
        var results: ?*ObjList = null;
        var offset: usize = 0;
        while (true) {
            if (try self.rawMatch(vm, subject, &offset)) |matches| {
                var was_null = results == null;
                results = results orelse try allocateObject(
                    vm,
                    ObjList,
                    ObjList.init(vm.allocator, matches.type_def),
                );

                if (was_null) {
                    vm.push(results.?.toValue());
                }

                try results.?.items.append(matches.toValue());
            } else {
                if (results != null) {
                    _ = vm.pop();
                }

                return results;
            }
        }

        if (results != null) {
            _ = vm.pop();
        }

        return results;
    }

    pub fn match(vm: *VM) c_int {
        var self = Self.cast(vm.peek(1).Obj).?;
        var subject = utils.toNullTerminated(
            vm.allocator,
            ObjString.cast(vm.peek(0).Obj).?.string,
        );

        if (subject == null) {
            var err: ?*ObjString = copyString(vm, "Could not match") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        }

        var offset: usize = 0;
        if (self.rawMatch(vm, subject.?, &offset) catch {
            var err: ?*ObjString = copyString(vm, "Could not match") catch null;
            vm.throw(VM.Error.Custom, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        }) |results| {
            vm.push(results.toValue());
        } else {
            vm.push(Value{ .Null = null });
        }

        return 1;
    }

    pub fn matchAll(vm: *VM) c_int {
        var self = Self.cast(vm.peek(1).Obj).?;
        var subject = utils.toNullTerminated(
            vm.allocator,
            ObjString.cast(vm.peek(0).Obj).?.string,
        );

        if (subject == null) {
            var err: ?*ObjString = copyString(vm, "Could not match") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        }

        if (self.rawMatchAll(vm, subject.?) catch {
            var err: ?*ObjString = copyString(vm, "Could not match") catch null;
            vm.throw(VM.Error.Custom, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        }) |results| {
            vm.push(results.toValue());
        } else {
            vm.push(Value{ .Null = null });
        }

        return 1;
    }

    pub fn rawMember(method: []const u8) ?NativeFn {
        if (mem.eql(u8, method, "match")) {
            return match;
        } else if (mem.eql(u8, method, "matchAll")) {
            return matchAll;
        }

        return null;
    }

    pub fn member(vm: *VM, method: []const u8) !?*ObjNative {
        if (Self.members) |umembers| {
            if (umembers.get(method)) |umethod| {
                return umethod;
            }
        }

        Self.members = Self.members orelse std.StringArrayHashMap(*ObjNative).init(vm.allocator);

        var nativeFn: ?NativeFn = rawMember(method);

        if (nativeFn) |unativeFn| {
            var native: *ObjNative = try allocateObject(
                vm,
                ObjNative,
                .{
                    .native = unativeFn,
                },
            );

            try Self.members.?.put(method, native);

            return native;
        }

        return null;
    }

    pub fn memberDef(parser: *Parser, method: []const u8) !?*ObjTypeDef {
        if (Self.memberDefs) |umembers| {
            if (umembers.get(method)) |umethod| {
                return umethod;
            }
        }

        Self.memberDefs = Self.memberDefs orelse std.StringHashMap(*ObjTypeDef).init(parser.allocator);

        // match(str subject) > [str]?
        if (mem.eql(u8, method, "match")) {
            var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

            // We omit first arg: it'll be OP_SWAPed in and we already parsed it
            // It's always the string.

            try parameters.put(
                "subject",
                try parser.type_registry.getTypeDef(
                    .{
                        .def_type = .String,
                    },
                ),
            );

            var list_def: ObjList.ListDef = ObjList.ListDef.init(
                parser.allocator,
                try parser.type_registry.getTypeDef(ObjTypeDef{
                    .def_type = .String,
                }),
            );

            var list_def_union: ObjTypeDef.TypeUnion = .{
                .List = list_def,
            };

            var method_def = ObjFunction.FunctionDef{
                .name = try copyStringRaw(parser.strings, parser.allocator, "match", false),
                .parameters = parameters,
                .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                .return_type = try parser.type_registry.getTypeDef(ObjTypeDef{
                    .def_type = .List,
                    .optional = true,
                    .resolved_type = list_def_union,
                }),
                .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
            };

            var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

            var native_type = try parser.type_registry.getTypeDef(
                ObjTypeDef{
                    .def_type = .Native,
                    .resolved_type = resolved_type,
                },
            );

            try Self.memberDefs.?.put("match", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "matchAll")) { // match(str subject) > [[str]]?
            var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

            // We omit first arg: it'll be OP_SWAPed in and we already parsed it
            // It's always the string.

            try parameters.put(
                "subject",
                try parser.type_registry.getTypeDef(
                    .{
                        .def_type = .String,
                    },
                ),
            );

            var sub_list_def: ObjList.ListDef = ObjList.ListDef.init(
                parser.allocator,
                try parser.type_registry.getTypeDef(ObjTypeDef{
                    .def_type = .String,
                }),
            );

            var sub_list_def_union: ObjTypeDef.TypeUnion = .{
                .List = sub_list_def,
            };

            var sub_list_type = try parser.type_registry.getTypeDef(ObjTypeDef{
                .def_type = .List,
                .optional = false,
                .resolved_type = sub_list_def_union,
            });

            var list_def: ObjList.ListDef = ObjList.ListDef.init(
                parser.allocator,
                sub_list_type,
            );

            var list_def_union: ObjTypeDef.TypeUnion = .{
                .List = list_def,
            };

            var method_def = ObjFunction.FunctionDef{
                .name = try copyStringRaw(parser.strings, parser.allocator, "matchAll", false),
                .parameters = parameters,
                .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                .return_type = try parser.type_registry.getTypeDef(ObjTypeDef{
                    .def_type = .List,
                    .optional = true,
                    .resolved_type = list_def_union,
                }),
                .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
            };

            var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

            var native_type = try parser.type_registry.getTypeDef(
                ObjTypeDef{
                    .def_type = .Native,
                    .resolved_type = resolved_type,
                },
            );

            try Self.memberDefs.?.put("matchAll", native_type);

            return native_type;
        }

        return null;
    }
};

// 1 = return value on stack, 0 = no return value, -1 = error
pub const NativeFn = fn (*VM) c_int;

/// Native function
pub const ObjNative = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Native },

    // TODO: issue is list.member which separate its type definition from its runtime creation
    // type_def: *ObjTypeDef,

    native: NativeFn,

    pub fn mark(_: *Self, _: *VM) void {}

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Native) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

pub const UserData = opaque {};

/// User data, type around an opaque pointer
pub const ObjUserData = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .UserData },

    userdata: *UserData,

    pub fn mark(_: *Self, _: *VM) void {}

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Native) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

/// A String
pub const ObjString = struct {
    const Self = @This();

    var members: ?std.StringArrayHashMap(*ObjNative) = null;
    var memberDefs: ?std.StringHashMap(*ObjTypeDef) = null;

    obj: Obj = .{ .obj_type = .String },

    /// The actual string
    string: []const u8,

    pub fn mark(_: *Self, vm: *VM) !void {
        if (Self.members) |umembers| {
            var it = umembers.iterator();
            while (it.next()) |kv| {
                try markObj(vm, kv.value_ptr.*.toObj());
            }
        }

        if (Self.memberDefs) |umemberDefs| {
            var it = umemberDefs.iterator();
            while (it.next()) |kv| {
                try markObj(vm, kv.value_ptr.*.toObj());
            }
        }
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .String) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    pub fn concat(self: *Self, vm: *VM, other: *Self) !*Self {
        var new_string: std.ArrayList(u8) = std.ArrayList(u8).init(vm.allocator);
        try new_string.appendSlice(self.string);
        try new_string.appendSlice(other.string);

        return copyString(vm, new_string.items);
    }

    pub fn len(vm: *VM) c_int {
        var str: *Self = Self.cast(vm.peek(0).Obj).?;

        vm.push(Value{ .Number = @intToFloat(f64, str.string.len) });

        return 1;
    }

    pub fn byte(vm: *VM) c_int {
        var self: *Self = Self.cast(vm.peek(1).Obj).?;
        var index: f64 = vm.peek(0).Number;

        if (index < 0 or index >= @intToFloat(f64, self.string.len)) {
            var err: ?*ObjString = copyString(vm, "Out of bound access to str") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        }

        vm.push(Value{ .Number = @intToFloat(f64, self.string[@floatToInt(usize, index)]) });

        return 1;
    }

    pub fn indexOf(vm: *VM) c_int {
        var self: *Self = Self.cast(vm.peek(1).Obj).?;
        var needle: *Self = Self.cast(vm.peek(0).Obj).?;

        var index = std.mem.indexOf(u8, self.string, needle.string);

        vm.push(if (index) |uindex| Value{ .Number = @intToFloat(f64, uindex) } else Value{ .Null = false });

        return 1;
    }

    pub fn startsWith(vm: *VM) c_int {
        var self: *Self = Self.cast(vm.peek(1).Obj).?;
        var needle: *Self = Self.cast(vm.peek(0).Obj).?;

        vm.push(Value{ .Boolean = std.mem.startsWith(u8, self.string, needle.string) });

        return 1;
    }

    pub fn endsWith(vm: *VM) c_int {
        var self: *Self = Self.cast(vm.peek(1).Obj).?;
        var needle: *Self = Self.cast(vm.peek(0).Obj).?;

        vm.push(Value{ .Boolean = std.mem.endsWith(u8, self.string, needle.string) });

        return 1;
    }

    pub fn replace(vm: *VM) c_int {
        var self: *Self = Self.cast(vm.peek(2).Obj).?;
        var needle: *Self = Self.cast(vm.peek(1).Obj).?;
        var replacement: *Self = Self.cast(vm.peek(0).Obj).?;

        const new_string = std.mem.replaceOwned(u8, vm.allocator, self.string, needle.string, replacement.string) catch {
            var err: ?*ObjString = copyString(vm, "Could not replace string") catch null;
            vm.throw(VM.Error.Custom, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        };

        vm.push(
            (copyString(vm, new_string) catch {
                var err: ?*ObjString = copyString(vm, "Could not replace string") catch null;
                vm.throw(VM.Error.Custom, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

                return -1;
            }).toValue(),
        );

        return 1;
    }

    pub fn sub(vm: *VM) c_int {
        var self: *Self = Self.cast(vm.peek(2).Obj).?;
        var start: f64 = vm.peek(1).Number;
        var upto_value: Value = vm.peek(0);
        var upto: ?f64 = if (upto_value == .Number) upto_value.Number else null;

        if (start < 0 or start >= @intToFloat(f64, self.string.len)) {
            var err: ?*ObjString = copyString(vm, "`start` is out of bound") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        }

        if (upto != null and upto.? < 0) {
            var err: ?*ObjString = copyString(vm, "`len` must greater or equal to 0") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        }

        const limit: usize = if (upto != null and @floatToInt(usize, start + upto.?) < self.string.len) @floatToInt(usize, start + upto.?) else self.string.len;
        var substr: []const u8 = self.string[@floatToInt(usize, start)..limit];

        vm.push(
            (copyString(vm, substr) catch {
                var err: ?*ObjString = copyString(vm, "Could not get sub string") catch null;
                vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

                return -1;
            }).toValue(),
        );

        return 1;
    }

    pub fn split(vm: *VM) c_int {
        var self: *Self = Self.cast(vm.peek(1).Obj).?;
        var separator: *Self = Self.cast(vm.peek(0).Obj).?;

        // std.mem.split(u8, self.string, separator.string);
        var list_def: ObjList.ListDef = ObjList.ListDef.init(
            vm.allocator,
            allocateObject(vm, ObjTypeDef, ObjTypeDef{
                .def_type = .String,
            }) catch {
                var err: ?*ObjString = copyString(vm, "Could not split string") catch null;
                vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

                return -1;
            },
        );

        var list_def_union: ObjTypeDef.TypeUnion = .{
            .List = list_def,
        };

        // TODO: reuse already allocated similar typedef
        var list_def_type: *ObjTypeDef = allocateObject(vm, ObjTypeDef, ObjTypeDef{
            .def_type = .List,
            .optional = false,
            .resolved_type = list_def_union,
        }) catch {
            var err: ?*ObjString = copyString(vm, "Could not split string") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        };

        var list: *ObjList = allocateObject(
            vm,
            ObjList,
            ObjList.init(vm.allocator, list_def_type),
        ) catch {
            var err: ?*ObjString = copyString(vm, "Could not split string") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        };

        // Prevent gc & is result
        vm.push(list.toValue());

        var it = std.mem.split(u8, self.string, separator.string);
        while (it.next()) |fragment| {
            var fragment_str: ?*ObjString = copyString(vm, fragment) catch {
                var err: ?*ObjString = copyString(vm, "Could not split string") catch null;
                vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

                return -1;
            };

            list.rawAppend(fragment_str.?.toValue()) catch {
                var err: ?*ObjString = copyString(vm, "Could not split string") catch null;
                vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

                return -1;
            };
        }

        return 1;
    }

    pub fn next(self: *Self, vm: *VM, str_index: ?f64) !?f64 {
        if (str_index) |index| {
            if (index < 0 or index >= @intToFloat(f64, self.string.len)) {
                try vm.throw(VM.Error.OutOfBound, (try copyString(vm, "Out of bound access to str")).toValue());
            }

            return if (index + 1 >= @intToFloat(f64, self.string.len))
                null
            else
                index + 1;
        } else {
            return if (self.string.len > 0) @intToFloat(f64, 0) else null;
        }
    }

    pub fn rawMember(method: []const u8) ?NativeFn {
        if (mem.eql(u8, method, "len")) {
            return len;
        } else if (mem.eql(u8, method, "byte")) {
            return byte;
        } else if (mem.eql(u8, method, "indexOf")) {
            return indexOf;
        } else if (mem.eql(u8, method, "split")) {
            return split;
        } else if (mem.eql(u8, method, "sub")) {
            return sub;
        } else if (mem.eql(u8, method, "startsWith")) {
            return startsWith;
        } else if (mem.eql(u8, method, "endsWith")) {
            return endsWith;
        } else if (mem.eql(u8, method, "replace")) {
            return replace;
        }

        return null;
    }

    // TODO: find a way to return the same ObjNative pointer for the same type of Lists
    pub fn member(vm: *VM, method: []const u8) !?*ObjNative {
        if (Self.members) |umembers| {
            if (umembers.get(method)) |umethod| {
                return umethod;
            }
        }

        Self.members = Self.members orelse std.StringArrayHashMap(*ObjNative).init(vm.allocator);

        var nativeFn: ?NativeFn = rawMember(method);

        if (nativeFn) |unativeFn| {
            var native: *ObjNative = try allocateObject(
                vm,
                ObjNative,
                .{
                    .native = unativeFn,
                },
            );

            try Self.members.?.put(method, native);

            return native;
        }

        return null;
    }

    pub fn memberDef(parser: *Parser, method: []const u8) !?*ObjTypeDef {
        if (Self.memberDefs) |umembers| {
            if (umembers.get(method)) |umethod| {
                return umethod;
            }
        }

        Self.memberDefs = Self.memberDefs orelse std.StringHashMap(*ObjTypeDef).init(parser.allocator);

        if (mem.eql(u8, method, "len")) {
            // We omit first arg: it'll be OP_SWAPed in and we already parsed it
            // It's always the string.

            var method_def = ObjFunction.FunctionDef{
                .name = try copyStringRaw(parser.strings, parser.allocator, "len", false),
                .parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator),
                .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                .return_type = try parser.type_registry.getTypeDef(.{ .def_type = .Number }),
                .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
            };

            var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

            var native_type = try parser.type_registry.getTypeDef(
                ObjTypeDef{
                    .def_type = .Native,
                    .resolved_type = resolved_type,
                },
            );

            try Self.memberDefs.?.put("len", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "byte")) {
            var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

            // We omit first arg: it'll be OP_SWAPed in and we already parsed it
            // It's always the string.

            try parameters.put("at", try parser.type_registry.getTypeDef(.{ .def_type = .Number }));

            var method_def = ObjFunction.FunctionDef{
                .name = try copyStringRaw(parser.strings, parser.allocator, "byte", false),
                .parameters = parameters,
                .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                .return_type = try parser.type_registry.getTypeDef(.{ .def_type = .Number }),
                .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
            };

            var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

            var native_type = try parser.type_registry.getTypeDef(
                ObjTypeDef{
                    .def_type = .Native,
                    .resolved_type = resolved_type,
                },
            );

            try Self.memberDefs.?.put("byte", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "indexOf")) {
            var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

            // We omit first arg: it'll be OP_SWAPed in and we already parsed it
            // It's always the string.

            try parameters.put("needle", try parser.type_registry.getTypeDef(.{ .def_type = .String }));

            var method_def = ObjFunction.FunctionDef{
                .name = try copyStringRaw(parser.strings, parser.allocator, "indexOf", false),
                .parameters = parameters,
                .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                .return_type = try parser.type_registry.getTypeDef(
                    .{
                        .def_type = .Number,
                        .optional = true,
                    },
                ),
                .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
            };

            var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

            var native_type = try parser.type_registry.getTypeDef(
                ObjTypeDef{
                    .def_type = .Native,
                    .resolved_type = resolved_type,
                },
            );

            try Self.memberDefs.?.put("indexOf", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "startsWith")) {
            var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

            // We omit first arg: it'll be OP_SWAPed in and we already parsed it
            // It's always the string.

            try parameters.put("needle", try parser.type_registry.getTypeDef(.{ .def_type = .String }));

            var method_def = ObjFunction.FunctionDef{
                .name = try copyStringRaw(parser.strings, parser.allocator, "startsWith", false),
                .parameters = parameters,
                .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                .return_type = try parser.type_registry.getTypeDef(
                    .{
                        .def_type = .Bool,
                        .optional = false,
                    },
                ),
                .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
            };

            var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

            var native_type = try parser.type_registry.getTypeDef(
                ObjTypeDef{
                    .def_type = .Native,
                    .resolved_type = resolved_type,
                },
            );

            try Self.memberDefs.?.put("startsWith", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "endsWith")) {
            var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

            // We omit first arg: it'll be OP_SWAPed in and we already parsed it
            // It's always the string.

            try parameters.put("needle", try parser.type_registry.getTypeDef(.{ .def_type = .String }));

            var method_def = ObjFunction.FunctionDef{
                .name = try copyStringRaw(parser.strings, parser.allocator, "endsWith", false),
                .parameters = parameters,
                .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                .return_type = try parser.type_registry.getTypeDef(
                    .{
                        .def_type = .Bool,
                        .optional = false,
                    },
                ),
                .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
            };

            var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

            var native_type = try parser.type_registry.getTypeDef(
                ObjTypeDef{
                    .def_type = .Native,
                    .resolved_type = resolved_type,
                },
            );

            try Self.memberDefs.?.put("endsWith", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "replace")) {
            var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

            // We omit first arg: it'll be OP_SWAPed in and we already parsed it
            // It's always the string.

            try parameters.put("needle", try parser.type_registry.getTypeDef(.{ .def_type = .String }));
            try parameters.put("with", try parser.type_registry.getTypeDef(.{ .def_type = .String }));

            var method_def = ObjFunction.FunctionDef{
                .name = try copyStringRaw(parser.strings, parser.allocator, "replace", false),
                .parameters = parameters,
                .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                .return_type = try parser.type_registry.getTypeDef(ObjTypeDef{ .def_type = .String }),
                .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
            };

            var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

            var native_type = try parser.type_registry.getTypeDef(
                ObjTypeDef{
                    .def_type = .Native,
                    .resolved_type = resolved_type,
                },
            );

            try Self.memberDefs.?.put("replace", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "split")) {
            var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

            // We omit first arg: it'll be OP_SWAPed in and we already parsed it
            // It's always the string.

            try parameters.put("separator", try parser.type_registry.getTypeDef(.{ .def_type = .String }));

            var list_def: ObjList.ListDef = ObjList.ListDef.init(
                parser.allocator,
                try parser.type_registry.getTypeDef(ObjTypeDef{
                    .def_type = .String,
                }),
            );

            var list_def_union: ObjTypeDef.TypeUnion = .{
                .List = list_def,
            };

            var method_def = ObjFunction.FunctionDef{
                .name = try copyStringRaw(parser.strings, parser.allocator, "split", false),
                .parameters = parameters,
                .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                .return_type = try parser.type_registry.getTypeDef(ObjTypeDef{
                    .def_type = .List,
                    .optional = false,
                    .resolved_type = list_def_union,
                }),
                .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
            };

            var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

            var native_type = try parser.type_registry.getTypeDef(
                ObjTypeDef{
                    .def_type = .Native,
                    .resolved_type = resolved_type,
                },
            );

            try Self.memberDefs.?.put("split", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "sub")) {
            var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);
            var defaults = std.StringArrayHashMap(Value).init(parser.allocator);

            // We omit first arg: it'll be OP_SWAPed in and we already parsed it
            // It's always the string.

            try parameters.put(
                "start",
                try parser.type_registry.getTypeDef(
                    .{
                        .def_type = .Number,
                    },
                ),
            );

            try parameters.put(
                "len",
                try parser.type_registry.getTypeDef(
                    .{
                        .def_type = .Number,
                        .optional = true,
                    },
                ),
            );

            // `len` can be omitted
            try defaults.put("len", Value{ .Null = null });

            var method_def = ObjFunction.FunctionDef{
                .name = try copyStringRaw(parser.strings, parser.allocator, "sub", false),
                .parameters = parameters,
                .defaults = defaults,
                .return_type = try parser.type_registry.getTypeDef(ObjTypeDef{ .def_type = .String }),
                .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
            };

            var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

            var native_type = try parser.type_registry.getTypeDef(
                ObjTypeDef{
                    .def_type = .Native,
                    .resolved_type = resolved_type,
                },
            );

            try Self.memberDefs.?.put("sub", native_type);

            return native_type;
        }

        return null;
    }
};

/// Upvalue
pub const ObjUpValue = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .UpValue },

    /// Slot on the stack
    location: *Value,
    closed: ?Value,
    next: ?*ObjUpValue = null,

    pub fn init(slot: *Value) Self {
        return Self{ .closed = null, .location = slot, .next = null };
    }

    pub fn mark(self: *Self, vm: *VM) !void {
        if (self.closed) |uclosed| {
            try markValue(vm, uclosed);
        }
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .UpValue) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

/// Closure
pub const ObjClosure = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Closure },

    function: *ObjFunction,
    upvalues: std.ArrayList(*ObjUpValue),
    // Pointer to the global with which the function was declared
    globals: *std.ArrayList(Value),

    pub fn init(allocator: Allocator, vm: *VM, function: *ObjFunction) !Self {
        return Self{
            .globals = &vm.globals,
            .function = function,
            .upvalues = try std.ArrayList(*ObjUpValue).initCapacity(allocator, function.upvalue_count),
        };
    }

    pub fn mark(self: *Self, vm: *VM) !void {
        try markObj(vm, self.function.toObj());
        for (self.upvalues.items) |upvalue| {
            try markObj(vm, upvalue.toObj());
        }
        for (self.globals.items) |global| {
            try markValue(vm, global);
        }
    }

    pub fn deinit(self: *Self) void {
        self.upvalues.deinit();
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Closure) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

/// Function
pub const ObjFunction = struct {
    const Self = @This();

    pub const FunctionType = enum {
        Function,
        Method,
        Script, // Imported script
        ScriptEntryPoint, // main script
        EntryPoint, // main function
        Catch,
        Test,
        Anonymous,
        Extern,
    };

    obj: Obj = .{ .obj_type = .Function },

    type_def: *ObjTypeDef = undefined, // Undefined because function initialization is in several steps

    name: *ObjString,
    chunk: Chunk,
    upvalue_count: u8 = 0,

    pub fn init(allocator: Allocator, name: *ObjString) !Self {
        return Self{
            .name = name,
            .chunk = Chunk.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.chunk.deinit();
    }

    pub fn mark(self: *Self, vm: *VM) !void {
        try markObj(vm, self.name.toObj());
        try markObj(vm, self.type_def.toObj());
        for (self.chunk.constants.items) |constant| {
            try markValue(vm, constant);
        }
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Function) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    pub const FunctionDef = struct {
        const FunctionDefSelf = @This();

        name: *ObjString,
        return_type: *ObjTypeDef,
        yield_type: *ObjTypeDef,
        parameters: std.StringArrayHashMap(*ObjTypeDef),
        // Storing here the defaults means they can only be non-Obj values
        defaults: std.StringArrayHashMap(Value),
        function_type: FunctionType = .Function,
        lambda: bool = false,

        pub fn mark(self: *FunctionDefSelf, vm: *VM) !void {
            try markObj(vm, self.name.toObj());
            try markObj(vm, self.return_type.toObj());

            var it = self.parameters.iterator();
            while (it.next()) |parameter| {
                try markObj(vm, parameter.value_ptr.*.toObj());
            }

            var it2 = self.defaults.iterator();
            while (it2.next()) |default| {
                try markValue(vm, default.value_ptr.*);
            }
        }
    };
};

/// Object instance
pub const ObjObjectInstance = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .ObjectInstance },

    /// Object
    object: *ObjObject,
    /// Fields value
    fields: std.AutoHashMap(*ObjString, Value),

    pub fn init(allocator: Allocator, object: *ObjObject) Self {
        return Self{
            .object = object,
            .fields = std.AutoHashMap(*ObjString, Value).init(allocator),
        };
    }

    pub fn mark(self: *Self, vm: *VM) !void {
        try markObj(vm, self.object.toObj());
        var it = self.fields.iterator();
        while (it.next()) |kv| {
            try markObj(vm, kv.key_ptr.*.toObj());
            try markValue(vm, kv.value_ptr.*);
        }
    }

    pub fn deinit(self: *Self) void {
        self.fields.deinit();
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .ObjectInstance) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    fn is(self: *Self, instance_type: ?*ObjTypeDef, type_def: *ObjTypeDef) bool {
        const object_def: *ObjTypeDef = instance_type orelse self.object.type_def;

        if (type_def.def_type != .Object) {
            return false;
        }

        return object_def == type_def or (object_def.resolved_type.?.Object.super != null and self.is(object_def.resolved_type.?.Object.super.?, type_def));
    }
};

/// Object
pub const ObjObject = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Object },

    type_def: *ObjTypeDef,

    /// Object name
    name: *ObjString,
    /// Object methods
    methods: std.AutoHashMap(*ObjString, *ObjClosure),
    /// Object fields default values
    fields: std.AutoHashMap(*ObjString, Value),
    /// Object static fields
    static_fields: std.AutoHashMap(*ObjString, Value),
    /// Optional super class
    super: ?*ObjObject = null,

    pub fn init(allocator: Allocator, name: *ObjString, type_def: *ObjTypeDef) Self {
        return Self{
            .name = name,
            .methods = std.AutoHashMap(*ObjString, *ObjClosure).init(allocator),
            .fields = std.AutoHashMap(*ObjString, Value).init(allocator),
            .static_fields = std.AutoHashMap(*ObjString, Value).init(allocator),
            .type_def = type_def,
        };
    }

    pub fn mark(self: *Self, vm: *VM) !void {
        try markObj(vm, self.type_def.toObj());
        try markObj(vm, self.name.toObj());
        var it = self.methods.iterator();
        while (it.next()) |kv| {
            try markObj(vm, kv.key_ptr.*.toObj());
            try markObj(vm, kv.value_ptr.*.toObj());
        }
        var it2 = self.fields.iterator();
        while (it2.next()) |kv| {
            try markObj(vm, kv.key_ptr.*.toObj());
            try markValue(vm, kv.value_ptr.*);
        }
        var it3 = self.static_fields.iterator();
        while (it3.next()) |kv| {
            try markObj(vm, kv.key_ptr.*.toObj());
            try markValue(vm, kv.value_ptr.*);
        }
        if (self.super) |usuper| {
            try markObj(vm, usuper.toObj());
        }
    }

    pub fn deinit(self: *Self) void {
        self.methods.deinit();
        self.fields.deinit();
        self.static_fields.deinit();
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Object) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    pub const ObjectDef = struct {
        const ObjectDefSelf = @This();

        name: *ObjString,
        // TODO: Do i need to have two maps ?
        fields: std.StringHashMap(*ObjTypeDef),
        fields_defaults: std.StringHashMap(void),
        static_fields: std.StringHashMap(*ObjTypeDef),
        methods: std.StringHashMap(*ObjTypeDef),
        // When we have placeholders we don't know if they are properties or methods
        // That information is available only when the placeholder is resolved
        placeholders: std.StringHashMap(*ObjTypeDef),
        static_placeholders: std.StringHashMap(*ObjTypeDef),
        super: ?*ObjTypeDef = null,
        inheritable: bool = false,
        is_class: bool,

        pub fn init(allocator: Allocator, name: *ObjString, is_class: bool) ObjectDefSelf {
            return ObjectDefSelf{
                .name = name,
                .is_class = is_class,
                .fields = std.StringHashMap(*ObjTypeDef).init(allocator),
                .static_fields = std.StringHashMap(*ObjTypeDef).init(allocator),
                .fields_defaults = std.StringHashMap(void).init(allocator),
                .methods = std.StringHashMap(*ObjTypeDef).init(allocator),
                .placeholders = std.StringHashMap(*ObjTypeDef).init(allocator),
                .static_placeholders = std.StringHashMap(*ObjTypeDef).init(allocator),
            };
        }

        pub fn deinit(self: *ObjectDefSelf) void {
            self.fields.deinit();
            self.static_fields.deinit();
            self.fields_defaults.deinit();
            self.methods.deinit();
            self.placeholders.deinit();
            self.static_placeholders.deinit();
        }

        pub fn mark(self: *ObjectDefSelf, vm: *VM) !void {
            var it = self.fields.iterator();
            while (it.next()) |kv| {
                try markObj(vm, kv.value_ptr.*.toObj());
            }

            var it3 = self.static_fields.iterator();
            while (it3.next()) |kv| {
                try markObj(vm, kv.value_ptr.*.toObj());
            }

            var it4 = self.methods.iterator();
            while (it4.next()) |kv| {
                try markObj(vm, kv.value_ptr.*.toObj());
            }

            var it5 = self.placeholders.iterator();
            while (it5.next()) |kv| {
                try markObj(vm, kv.value_ptr.*.toObj());
            }

            var it6 = self.static_placeholders.iterator();
            while (it6.next()) |kv| {
                try markObj(vm, kv.value_ptr.*.toObj());
            }

            if (self.super) |super| {
                try markObj(vm, super.toObj());
            }
        }
    };
};

/// List
pub const ObjList = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .List },

    type_def: *ObjTypeDef,

    /// List items
    items: std.ArrayList(Value),

    methods: std.StringHashMap(*ObjNative),

    pub fn init(allocator: Allocator, type_def: *ObjTypeDef) Self {
        return Self{
            .items = std.ArrayList(Value).init(allocator),
            .type_def = type_def,
            .methods = std.StringHashMap(*ObjNative).init(allocator),
        };
    }

    pub fn mark(self: *Self, vm: *VM) !void {
        for (self.items.items) |value| {
            try markValue(vm, value);
        }
        try markObj(vm, self.type_def.toObj());
        var it = self.methods.iterator();
        while (it.next()) |kv| {
            try markObj(vm, kv.value_ptr.*.toObj());
        }
    }

    pub fn deinit(self: *Self) void {
        self.items.deinit();
        self.methods.deinit();
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .List) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    // TODO: find a way to return the same ObjNative pointer for the same type of Lists
    pub fn member(self: *Self, vm: *VM, method: []const u8) !?*ObjNative {
        if (self.methods.get(method)) |native| {
            return native;
        }

        var nativeFn: ?NativeFn = null;
        if (mem.eql(u8, method, "append")) {
            nativeFn = append;
        } else if (mem.eql(u8, method, "len")) {
            nativeFn = len;
        } else if (mem.eql(u8, method, "next")) {
            nativeFn = next;
        } else if (mem.eql(u8, method, "remove")) {
            nativeFn = remove;
        } else if (mem.eql(u8, method, "sub")) {
            nativeFn = sub;
        } else if (mem.eql(u8, method, "indexOf")) {
            nativeFn = indexOf;
        } else if (mem.eql(u8, method, "join")) {
            nativeFn = join;
        }

        if (nativeFn) |unativeFn| {
            var native: *ObjNative = try allocateObject(
                vm,
                ObjNative,
                .{
                    .native = unativeFn,
                },
            );

            try self.methods.put(method, native);

            return native;
        }

        return null;
    }

    pub fn rawAppend(self: *Self, value: Value) !void {
        try self.items.append(value);
    }

    fn append(vm: *VM) c_int {
        var list_value: Value = vm.peek(1);
        var list: *ObjList = ObjList.cast(list_value.Obj).?;
        var value: Value = vm.peek(0);

        list.rawAppend(value) catch |err| {
            const messageValue: Value = (copyString(vm, "Could not append to list") catch {
                std.debug.print("Could not append to list", .{});
                std.os.exit(1);
            }).toValue();

            vm.throw(err, messageValue) catch {
                std.debug.print("Could not append to list", .{});
                std.os.exit(1);
            };
            return -1;
        };

        vm.push(list_value);

        return 1;
    }

    fn len(vm: *VM) c_int {
        var list: *ObjList = ObjList.cast(vm.peek(0).Obj).?;

        vm.push(Value{ .Number = @intToFloat(f64, list.items.items.len) });

        return 1;
    }

    pub fn remove(vm: *VM) c_int {
        var list: *ObjList = ObjList.cast(vm.peek(1).Obj).?;
        var list_index: f64 = vm.peek(0).Number;

        if (list_index < 0 or list_index >= @intToFloat(f64, list.items.items.len)) {
            vm.push(Value{ .Null = false });

            return 1;
        }

        vm.push(list.items.orderedRemove(@floatToInt(usize, list_index)));

        return 1;
    }

    pub fn indexOf(vm: *VM) c_int {
        var self: *Self = Self.cast(vm.peek(1).Obj).?;
        var needle: Value = vm.peek(0);

        var index: ?usize = 0;
        var i: usize = 0;
        for (self.items.items) |item| {
            if (valueEql(needle, item)) {
                index = i;
                break;
            }

            i += 1;
        }

        vm.push(if (index) |uindex| Value{ .Number = @intToFloat(f64, uindex) } else Value{ .Null = false });

        return 1;
    }

    pub fn join(vm: *VM) c_int {
        var self: *Self = Self.cast(vm.peek(1).Obj).?;
        var separator: *ObjString = ObjString.cast(vm.peek(0).Obj).?;

        var result = std.ArrayList(u8).init(vm.allocator);
        var writer = result.writer();
        defer result.deinit();
        for (self.items.items) |item, i| {
            valueToString(writer, item) catch {
                var err: ?*ObjString = copyString(vm, "could not stringify item") catch null;
                vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

                return -1;
            };

            if (i + 1 < self.items.items.len) {
                writer.writeAll(separator.string) catch {
                    var err: ?*ObjString = copyString(vm, "could not join list") catch null;
                    vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

                    return -1;
                };
            }
        }

        vm.push(
            Value{
                .Obj = (copyString(vm, result.items) catch {
                    var err: ?*ObjString = copyString(vm, "could not join list") catch null;
                    vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

                    return -1;
                }).toObj(),
            },
        );

        return 1;
    }

    pub fn sub(vm: *VM) c_int {
        var self: *Self = Self.cast(vm.peek(2).Obj).?;
        var start: f64 = vm.peek(1).Number;
        var upto_value: Value = vm.peek(0);
        var upto: ?f64 = if (upto_value == .Number) upto_value.Number else null;

        if (start < 0 or start >= @intToFloat(f64, self.items.items.len)) {
            var err: ?*ObjString = copyString(vm, "`start` is out of bound") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        }

        if (upto != null and upto.? < 0) {
            var err: ?*ObjString = copyString(vm, "`len` must greater or equal to 0") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        }

        const limit: usize = if (upto != null and @floatToInt(usize, start + upto.?) < self.items.items.len) @floatToInt(usize, start + upto.?) else self.items.items.len;
        var substr: []Value = self.items.items[@floatToInt(usize, start)..limit];

        var list = allocateObject(vm, ObjList, ObjList{
            .type_def = self.type_def,
            .methods = self.methods.clone() catch {
                var err: ?*ObjString = copyString(vm, "Could not get sub list") catch null;
                vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

                return -1;
            },
            .items = std.ArrayList(Value).init(vm.allocator),
        }) catch {
            var err: ?*ObjString = copyString(vm, "Could not get sub list") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        };

        vm.push(list.toValue());

        list.items.appendSlice(substr) catch {
            var err: ?*ObjString = copyString(vm, "Could not get sub list") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        };

        return 1;
    }

    pub fn rawNext(self: *Self, vm: *VM, list_index: ?f64) !?f64 {
        if (list_index) |index| {
            if (index < 0 or index >= @intToFloat(f64, self.items.items.len)) {
                try vm.throw(VM.Error.OutOfBound, (try copyString(vm, "Out of bound access to list")).toValue());
            }

            return if (index + 1 >= @intToFloat(f64, self.items.items.len))
                null
            else
                index + 1;
        } else {
            return if (self.items.items.len > 0) @intToFloat(f64, 0) else null;
        }
    }

    fn next(vm: *VM) c_int {
        var list_value: Value = vm.peek(1);
        var list: *ObjList = ObjList.cast(list_value.Obj).?;
        var list_index: Value = vm.peek(0);

        var next_index: ?f64 = list.rawNext(vm, if (list_index == .Null) null else list_index.Number) catch |err| {
            // TODO: should we distinguish NativeFn and ExternFn ?
            std.debug.print("{}\n", .{err});
            std.os.exit(1);
        };

        vm.push(if (next_index) |unext_index| Value{ .Number = unext_index } else Value{ .Null = null });

        return 1;
    }

    pub const ListDef = struct {
        const SelfListDef = @This();

        item_type: *ObjTypeDef,
        methods: std.StringHashMap(*ObjTypeDef),

        pub fn init(allocator: Allocator, item_type: *ObjTypeDef) SelfListDef {
            return .{
                .item_type = item_type,
                .methods = std.StringHashMap(*ObjTypeDef).init(allocator),
            };
        }

        pub fn deinit(self: *SelfListDef) void {
            self.methods.deinit();
        }

        pub fn mark(self: *SelfListDef, vm: *VM) !void {
            try markObj(vm, self.item_type.toObj());
            var it = self.methods.iterator();
            while (it.next()) |method| {
                try markObj(vm, method.value_ptr.*.toObj());
            }
        }

        pub fn member(obj_list: *ObjTypeDef, parser: *Parser, method: []const u8) !?*ObjTypeDef {
            var self = obj_list.resolved_type.?.List;

            if (self.methods.get(method)) |native_def| {
                return native_def;
            }

            if (mem.eql(u8, method, "append")) {
                var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                // `value` arg is of item_type
                try parameters.put("value", self.item_type);

                var method_def = ObjFunction.FunctionDef{
                    .name = try copyStringRaw(parser.strings, parser.allocator, "append", false),
                    .parameters = parameters,
                    .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                    .return_type = obj_list,
                    .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

                var native_type = try parser.type_registry.getTypeDef(ObjTypeDef{ .def_type = .Native, .resolved_type = resolved_type });

                try self.methods.put("append", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "remove")) {
                var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var at_type = try parser.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Number,
                        .optional = false,
                    },
                );

                try parameters.put("at", at_type);

                var method_def = ObjFunction.FunctionDef{
                    .name = try copyStringRaw(parser.strings, parser.allocator, "remove", false),
                    .parameters = parameters,
                    .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                    .return_type = try parser.type_registry.getTypeDef(.{
                        .optional = true,
                        .def_type = self.item_type.def_type,
                        .resolved_type = self.item_type.resolved_type,
                    }),
                    .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

                var native_type = try parser.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Native,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("remove", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "len")) {
                var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

                var method_def = ObjFunction.FunctionDef{
                    .name = try copyStringRaw(parser.strings, parser.allocator, "len", false),
                    .parameters = parameters,
                    .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                    .return_type = try parser.type_registry.getTypeDef(
                        ObjTypeDef{
                            .def_type = .Number,
                        },
                    ),
                    .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

                var native_type = try parser.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Native,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("len", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "next")) {
                var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                // `key` arg is number
                try parameters.put(
                    "key",
                    try parser.type_registry.getTypeDef(
                        ObjTypeDef{
                            .def_type = .Number,
                            .optional = true,
                        },
                    ),
                );

                var method_def = ObjFunction.FunctionDef{
                    .name = try copyStringRaw(parser.strings, parser.allocator, "next", false),
                    .parameters = parameters,
                    .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                    // When reached end of list, returns null
                    .return_type = try parser.type_registry.getTypeDef(
                        ObjTypeDef{
                            .def_type = .Number,
                            .optional = true,
                        },
                    ),
                    .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

                var native_type = try parser.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Native,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("next", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "sub")) {
                var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put(
                    "start",
                    try parser.type_registry.getTypeDef(
                        .{
                            .def_type = .Number,
                        },
                    ),
                );
                try parameters.put(
                    "len",
                    try parser.type_registry.getTypeDef(
                        .{
                            .def_type = .Number,
                            .optional = true,
                        },
                    ),
                );

                var method_def = ObjFunction.FunctionDef{
                    .name = try copyStringRaw(parser.strings, parser.allocator, "sub", false),
                    .parameters = parameters,
                    .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                    .return_type = obj_list,
                    .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

                var native_type = try parser.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Native,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("sub", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "indexOf")) {
                var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put("needle", self.item_type);

                var method_def = ObjFunction.FunctionDef{
                    .name = try copyStringRaw(parser.strings, parser.allocator, "indexOf", false),
                    .parameters = parameters,
                    .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                    .return_type = try parser.type_registry.getTypeDef(
                        .{
                            .def_type = self.item_type.def_type,
                            .optional = true,
                            .resolved_type = self.item_type.resolved_type,
                        },
                    ),
                    .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

                var native_type = try parser.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Native,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("indexOf", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "join")) {
                var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put("separator", try parser.type_registry.getTypeDef(.{ .def_type = .String }));

                var method_def = ObjFunction.FunctionDef{
                    .name = try copyStringRaw(parser.strings, parser.allocator, "join", false),
                    .parameters = parameters,
                    .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                    .return_type = try parser.type_registry.getTypeDef(ObjTypeDef{
                        .def_type = .String,
                    }),
                    .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

                var native_type = try parser.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Native,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("join", native_type);

                return native_type;
            }

            return null;
        }
    };
};

/// Map
pub const ObjMap = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Map },

    type_def: *ObjTypeDef,

    // We need an ArrayHashMap for `next`
    // In order to use a regular HashMap, we would have to hack are away around it to implement next
    map: std.AutoArrayHashMap(HashableValue, Value),

    methods: std.StringHashMap(*ObjNative),

    pub fn init(allocator: Allocator, type_def: *ObjTypeDef) Self {
        return .{
            .type_def = type_def,
            .map = std.AutoArrayHashMap(HashableValue, Value).init(allocator),
            .methods = std.StringHashMap(*ObjNative).init(allocator),
        };
    }

    pub fn member(self: *Self, vm: *VM, method: []const u8) !?*ObjNative {
        if (self.methods.get(method)) |native| {
            return native;
        }

        var nativeFn: ?NativeFn = null;
        if (mem.eql(u8, method, "remove")) {
            nativeFn = remove;
        } else if (mem.eql(u8, method, "size")) {
            nativeFn = size;
        } else if (mem.eql(u8, method, "keys")) {
            nativeFn = keys;
        } else if (mem.eql(u8, method, "values")) {
            nativeFn = values;
        }

        if (nativeFn) |unativeFn| {
            var native: *ObjNative = try allocateObject(
                vm,
                ObjNative,
                .{
                    .native = unativeFn,
                },
            );

            try self.methods.put(method, native);

            return native;
        }

        return null;
    }

    pub fn mark(self: *Self, vm: *VM) !void {
        var it = self.map.iterator();
        while (it.next()) |kv| {
            try markValue(vm, hashableToValue(kv.key_ptr.*));
            try markValue(vm, kv.value_ptr.*);
        }
        try markObj(vm, self.type_def.toObj());
    }

    fn size(vm: *VM) c_int {
        var map: *ObjMap = ObjMap.cast(vm.peek(0).Obj).?;

        vm.push(Value{ .Number = @intToFloat(f64, map.map.count()) });

        return 1;
    }

    pub fn remove(vm: *VM) c_int {
        var map: *ObjMap = ObjMap.cast(vm.peek(1).Obj).?;
        var map_key: HashableValue = valueToHashable(vm.peek(0));

        if (map.map.fetchOrderedRemove(map_key)) |removed| {
            vm.push(removed.value);
        } else {
            vm.push(Value{ .Null = false });
        }

        return 1;
    }

    pub fn keys(vm: *VM) c_int {
        var self: *ObjMap = ObjMap.cast(vm.peek(0).Obj).?;

        var map_keys: []HashableValue = self.map.keys();
        var result = std.ArrayList(Value).init(vm.allocator);
        for (map_keys) |key| {
            result.append(hashableToValue(key)) catch {
                var err: ?*ObjString = copyString(vm, "could not get map keys") catch null;
                vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

                return -1;
            };
        }

        var list_def: ObjList.ListDef = ObjList.ListDef.init(
            vm.allocator,
            self.type_def.resolved_type.?.Map.key_type,
        );

        var list_def_union: ObjTypeDef.TypeUnion = .{
            .List = list_def,
        };

        var list_def_type: *ObjTypeDef = allocateObject(vm, ObjTypeDef, ObjTypeDef{
            .def_type = .List,
            .optional = false,
            .resolved_type = list_def_union,
        }) catch {
            var err: ?*ObjString = copyString(vm, "could not get map keys") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        };

        var list = allocateObject(
            vm,
            ObjList,
            ObjList.init(vm.allocator, list_def_type),
        ) catch {
            var err: ?*ObjString = copyString(vm, "could not get map keys") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        };

        list.items.deinit();
        list.items = result;

        vm.push(list.toValue());

        return 1;
    }

    pub fn values(vm: *VM) c_int {
        var self: *ObjMap = ObjMap.cast(vm.peek(0).Obj).?;

        var map_values: []Value = self.map.values();
        var result = std.ArrayList(Value).init(vm.allocator);
        result.appendSlice(map_values) catch {
            var err: ?*ObjString = copyString(vm, "could not get map values") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        };

        var list_def: ObjList.ListDef = ObjList.ListDef.init(
            vm.allocator,
            self.type_def.resolved_type.?.Map.value_type,
        );

        var list_def_union: ObjTypeDef.TypeUnion = .{
            .List = list_def,
        };

        var list_def_type: *ObjTypeDef = allocateObject(vm, ObjTypeDef, ObjTypeDef{
            .def_type = .List,
            .optional = false,
            .resolved_type = list_def_union,
        }) catch {
            var err: ?*ObjString = copyString(vm, "could not get map values") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        };

        var list = allocateObject(
            vm,
            ObjList,
            ObjList.init(vm.allocator, list_def_type),
        ) catch {
            var err: ?*ObjString = copyString(vm, "could not get map values") catch null;
            vm.throw(VM.Error.OutOfBound, if (err) |uerr| uerr.toValue() else Value{ .Boolean = false }) catch unreachable;

            return -1;
        };

        list.items.deinit();
        list.items = result;

        vm.push(list.toValue());

        return 1;
    }

    pub fn rawNext(self: *Self, key: ?HashableValue) ?HashableValue {
        const map_keys: []HashableValue = self.map.keys();

        if (key) |ukey| {
            const index: usize = self.map.getIndex(ukey).?;

            if (index < map_keys.len - 1) {
                return map_keys[index + 1];
            } else {
                return null;
            }
        } else {
            return if (map_keys.len > 0) map_keys[0] else null;
        }
    }

    pub fn deinit(self: *Self) void {
        self.map.deinit();
        self.methods.deinit();
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Map) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    pub const MapDef = struct {
        const SelfMapDef = @This();

        key_type: *ObjTypeDef,
        value_type: *ObjTypeDef,

        methods: std.StringHashMap(*ObjTypeDef),

        pub fn init(allocator: Allocator, key_type: *ObjTypeDef, value_type: *ObjTypeDef) SelfMapDef {
            return .{
                .key_type = key_type,
                .value_type = value_type,
                .methods = std.StringHashMap(*ObjTypeDef).init(allocator),
            };
        }

        pub fn deinit(self: *SelfMapDef) void {
            self.methods.deinit();
        }

        pub fn mark(self: *SelfMapDef, vm: *VM) !void {
            try markObj(vm, self.key_type.toObj());
            try markObj(vm, self.value_type.toObj());
            var it = self.methods.iterator();
            while (it.next()) |method| {
                try markObj(vm, method.value_ptr.*.toObj());
            }
        }

        pub fn member(obj_map: *ObjTypeDef, parser: *Parser, method: []const u8) !?*ObjTypeDef {
            var self = obj_map.resolved_type.?.Map;

            if (self.methods.get(method)) |native_def| {
                return native_def;
            }

            if (mem.eql(u8, method, "size")) {
                var method_def = ObjFunction.FunctionDef{
                    .name = try copyStringRaw(parser.strings, parser.allocator, "size", false),
                    .parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator),
                    .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                    .return_type = try parser.type_registry.getTypeDef(.{
                        .def_type = .Number,
                    }),
                    .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

                var native_type = try parser.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Native,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("size", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "remove")) {
                var parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                try parameters.put("at", self.key_type);

                var method_def = ObjFunction.FunctionDef{
                    .name = try copyStringRaw(parser.strings, parser.allocator, "remove", false),
                    .parameters = parameters,
                    .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                    .return_type = try parser.type_registry.getTypeDef(.{
                        .optional = true,
                        .def_type = self.value_type.def_type,
                        .resolved_type = self.value_type.resolved_type,
                    }),
                    .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

                var native_type = try parser.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Native,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("remove", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "keys")) {
                var list_def: ObjList.ListDef = ObjList.ListDef.init(
                    parser.allocator,
                    self.key_type,
                );

                var list_def_union: ObjTypeDef.TypeUnion = .{
                    .List = list_def,
                };

                var method_def = ObjFunction.FunctionDef{
                    .name = try copyStringRaw(parser.strings, parser.allocator, "keys", false),
                    .parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator),
                    .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                    .return_type = try parser.type_registry.getTypeDef(.{
                        .def_type = .List,
                        .optional = false,
                        .resolved_type = list_def_union,
                    }),
                    .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

                var native_type = try parser.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Native,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("keys", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "values")) {
                var list_def: ObjList.ListDef = ObjList.ListDef.init(
                    parser.allocator,
                    self.value_type,
                );

                var list_def_union: ObjTypeDef.TypeUnion = .{
                    .List = list_def,
                };

                var method_def = ObjFunction.FunctionDef{
                    .name = try copyStringRaw(parser.strings, parser.allocator, "values", false),
                    .parameters = std.StringArrayHashMap(*ObjTypeDef).init(parser.allocator),
                    .defaults = std.StringArrayHashMap(Value).init(parser.allocator),
                    .return_type = try parser.type_registry.getTypeDef(.{
                        .def_type = .List,
                        .optional = false,
                        .resolved_type = list_def_union,
                    }),
                    .yield_type = try parser.type_registry.getTypeDef(.{ .def_type = .Void }),
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Native = method_def };

                var native_type = try parser.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Native,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("values", native_type);

                return native_type;
            }

            return null;
        }
    };
};

/// Enum
pub const ObjEnum = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Enum },

    /// Used to allow type checking at runtime
    type_def: *ObjTypeDef,

    name: *ObjString,
    cases: std.ArrayList(Value),

    pub fn init(allocator: Allocator, def: *ObjTypeDef) Self {
        return Self{
            .type_def = def,
            .name = def.resolved_type.?.Enum.name,
            .cases = std.ArrayList(Value).init(allocator),
        };
    }

    pub fn mark(self: *Self, vm: *VM) !void {
        try markObj(vm, self.name.toObj());
        try markObj(vm, self.type_def.toObj());
        for (self.cases.items) |case| {
            try markValue(vm, case);
        }
    }

    pub fn rawNext(self: *Self, vm: *VM, enum_case: ?*ObjEnumInstance) !?*ObjEnumInstance {
        if (enum_case) |case| {
            assert(case.enum_ref == self);

            if (case.case == self.cases.items.len - 1) {
                return null;
            }

            return try allocateObject(vm, ObjEnumInstance, ObjEnumInstance{
                .enum_ref = self,
                .case = @intCast(u8, case.case + 1),
            });
        } else {
            return try allocateObject(vm, ObjEnumInstance, ObjEnumInstance{
                .enum_ref = self,
                .case = 0,
            });
        }
    }

    pub fn deinit(self: *Self) void {
        self.cases.deinit();
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Enum) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    pub const EnumDef = struct {
        const EnumDefSelf = @This();

        name: *ObjString,
        enum_type: *ObjTypeDef,
        cases: std.ArrayList([]const u8),

        pub fn init(allocator: Allocator, name: *ObjString, enum_type: *ObjTypeDef) EnumDefSelf {
            return EnumDefSelf{
                .name = name,
                .cases = std.ArrayList([]const u8).init(allocator),
                .enum_type = enum_type,
            };
        }

        pub fn deinit(self: *EnumDefSelf) void {
            self.cases.deinit();
        }

        pub fn mark(self: *EnumDefSelf, vm: *VM) !void {
            try markObj(vm, self.name.toObj());
            try markObj(vm, self.enum_type.toObj());
        }
    };
};

pub const ObjEnumInstance = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .EnumInstance },

    enum_ref: *ObjEnum,
    case: u8,

    pub fn mark(self: *Self, vm: *VM) !void {
        try markObj(vm, self.enum_ref.toObj());
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .EnumInstance) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    pub fn value(self: *Self) Value {
        return self.enum_ref.cases.items[self.case];
    }
};

/// Bound
pub const ObjBoundMethod = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Bound },

    receiver: Value,
    closure: ?*ObjClosure = null,
    native: ?*ObjNative = null,

    pub fn mark(self: *Self, vm: *VM) !void {
        try markValue(vm, self.receiver);
        if (self.closure) |closure| {
            try markObj(vm, closure.toObj());
        }
        if (self.native) |native| {
            try markObj(vm, native.toObj());
        }
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Bound) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

pub const TypeRegistry = struct {
    const Self = @This();

    allocator: Allocator,
    registry: std.StringHashMap(*ObjTypeDef),

    pub fn getTypeDef(self: *Self, type_def: ObjTypeDef) !*ObjTypeDef {
        var type_def_buf = std.ArrayList(u8).init(self.allocator);
        try type_def.toString(type_def_buf.writer());
        const type_def_str: []const u8 = type_def_buf.items;

        // We don't return a cached version of a placeholder since they all maintain a particular state (link)
        if (type_def.def_type != .Placeholder) {
            if (self.registry.get(type_def_str)) |type_def_ptr| {
                self.allocator.free(type_def_str); // If already in map, we don't need this string anymore
                return type_def_ptr;
            }
        }

        var type_def_ptr: *ObjTypeDef = try self.allocator.create(ObjTypeDef);
        type_def_ptr.* = type_def;

        _ = try self.registry.put(type_def_str, type_def_ptr);

        return type_def_ptr;
    }

    pub fn setTypeDef(self: *Self, type_def: *ObjTypeDef) !void {
        const type_def_str: []const u8 = try type_def.toString(self.allocator);

        assert(type_def.def_type != .Placeholder);

        _ = try self.registry.put(type_def_str, type_def);
    }

    pub inline fn getTypeDefByName(self: *Self, name: []const u8) ?*ObjTypeDef {
        return self.registry.get(name);
    }
};

/// Type
pub const ObjTypeDef = struct {
    const Self = @This();

    // TODO: merge this with ObjType
    pub const Type = enum {
        Bool,
        Number,
        String,
        Pattern,
        ObjectInstance,
        Object,
        Enum,
        EnumInstance,
        List,
        Map,
        Function,
        Type, // Something that holds a type, not an actual type
        Void,
        Native,
        Fiber,

        Placeholder, // Used in first-pass when we refer to a not yet parsed type
    };

    pub const TypeUnion = union(Type) {
        // For those type checking is obvious, the value is a placeholder
        Bool: bool,
        Number: bool,
        String: bool,
        Pattern: bool,
        Type: bool,
        Void: bool,
        Fiber: ObjFiber.FiberDef,

        // For those we check that the value is an instance of, because those are user defined types
        ObjectInstance: *ObjTypeDef,
        EnumInstance: *ObjTypeDef,

        // Those are never equal
        Object: ObjObject.ObjectDef,
        Enum: ObjEnum.EnumDef,

        // For those we compare definitions, so we own those structs, we don't use actual Obj because we don't want the data, only the types
        List: ObjList.ListDef,
        Map: ObjMap.MapDef,
        Function: ObjFunction.FunctionDef,
        Native: ObjFunction.FunctionDef,

        Placeholder: PlaceholderDef,
    };

    obj: Obj = .{ .obj_type = .Type },

    /// True means its an optional (e.g `str?`)
    optional: bool = false,
    def_type: Type,
    /// Used when the type is not a basic type
    resolved_type: ?TypeUnion = null,

    pub fn mark(self: *Self, vm: *VM) !void {
        if (self.resolved_type) |*resolved| {
            if (resolved.* == .ObjectInstance) {
                try markObj(vm, resolved.ObjectInstance.toObj());
            } else if (resolved.* == .EnumInstance) {
                try markObj(vm, resolved.EnumInstance.toObj());
            } else if (resolved.* == .Object) {
                try resolved.Object.mark(vm);
            } else if (resolved.* == .Enum) {
                try resolved.Enum.mark(vm);
            } else if (resolved.* == .Function) {
                try resolved.Function.mark(vm);
            } else if (resolved.* == .List) {
                try resolved.List.mark(vm);
            } else if (resolved.* == .Map) {
                try resolved.Map.mark(vm);
            } else if (resolved.* == .Native) {
                try resolved.Native.mark(vm);
            } else if (resolved.* == .Fiber) {
                try resolved.Fiber.mark(vm);
            } else if (resolved.* == .Placeholder) {
                unreachable;
            }
        }
    }

    pub fn rawCloneOptional(self: *Self) ObjTypeDef {
        return .{
            .obj = .{ .obj_type = self.obj.obj_type },
            .optional = true,
            .def_type = self.def_type,
            .resolved_type = self.resolved_type,
        };
    }

    pub fn rawCloneNonOptional(self: *Self) ObjTypeDef {
        return .{
            .obj = .{ .obj_type = self.obj.obj_type },
            .optional = false,
            .def_type = self.def_type,
            .resolved_type = self.resolved_type,
        };
    }

    pub fn cloneOptional(self: *Self, type_registry: *TypeRegistry) !*ObjTypeDef {
        // If already optional return itself
        if (self.optional and self.def_type != .Placeholder) {
            return self;
        }

        const optional = try type_registry.getTypeDef(self.rawCloneOptional());

        if (self.def_type == .Placeholder) {
            // Destroyed copied placeholder link
            optional.resolved_type.?.Placeholder.parent = null;
            optional.resolved_type.?.Placeholder.parent_relation = null;
            optional.resolved_type.?.Placeholder.children = std.ArrayList(*ObjTypeDef).init(type_registry.allocator);

            // Make actual link
            try PlaceholderDef.link(self, optional, .Optional);
        }

        return optional;
    }

    pub fn cloneNonOptional(self: *Self, type_registry: *TypeRegistry) !*ObjTypeDef {
        // If already non optional return itself
        if (!self.optional and self.def_type != .Placeholder) {
            return self;
        }

        const non_optional = try type_registry.getTypeDef(self.rawCloneNonOptional());

        if (self.def_type == .Placeholder) {
            // Destroyed copied placeholder link
            non_optional.resolved_type.?.Placeholder.parent = null;
            non_optional.resolved_type.?.Placeholder.parent_relation = null;
            non_optional.resolved_type.?.Placeholder.children = std.ArrayList(*ObjTypeDef).init(type_registry.allocator);

            // Make actual link
            try PlaceholderDef.link(self, non_optional, .Unwrap);
        }

        return non_optional;
    }

    pub fn deinit(_: *Self) void {}

    pub fn toStringAlloc(self: *const Self, allocator: Allocator) (Allocator.Error || std.fmt.BufPrintError)![]const u8 {
        var str = std.ArrayList(u8).init(allocator);

        try self.toString(str.writer());

        return str.items;
    }

    pub fn toString(self: *const Self, writer: std.ArrayList(u8).Writer) (Allocator.Error || std.fmt.BufPrintError)!void {
        switch (self.def_type) {
            .Bool => try writer.writeAll("bool"),
            .Number => try writer.writeAll("num"),
            .String => try writer.writeAll("str"),
            .Pattern => try writer.writeAll("pat"),
            .Fiber => {
                try writer.writeAll("fib<");
                try self.resolved_type.?.Fiber.return_type.toString(writer);
                try writer.writeAll(", ");
                try self.resolved_type.?.Fiber.yield_type.toString(writer);
                try writer.writeAll(">");
            },

            // TODO: Find a key for vm.getTypeDef which is unique for each class even with the same name
            .Object => {
                const object_def = self.resolved_type.?.Object;

                try writer.writeAll(if (object_def.is_class) "class " else "object ");
                try writer.writeAll(object_def.name.string);
            },
            .Enum => {
                try writer.writeAll("enum ");
                try writer.writeAll(self.resolved_type.?.Enum.name.string);
            },

            .ObjectInstance => try writer.writeAll(self.resolved_type.?.ObjectInstance.resolved_type.?.Object.name.string),
            .EnumInstance => try writer.writeAll(self.resolved_type.?.EnumInstance.resolved_type.?.Enum.name.string),

            .List => {
                try writer.writeAll("[");
                try self.resolved_type.?.List.item_type.toString(writer);
                try writer.writeAll("]");
            },
            .Map => {
                try writer.writeAll("{");
                try self.resolved_type.?.Map.key_type.toString(writer);
                try writer.writeAll(", ");
                try self.resolved_type.?.Map.value_type.toString(writer);
                try writer.writeAll("}");
            },
            .Native, .Function => {
                var function_def = if (self.def_type == .Function) self.resolved_type.?.Function else self.resolved_type.?.Native;

                try writer.writeAll("fun ");
                try writer.writeAll(function_def.name.string);
                try writer.writeAll("(");

                const size = function_def.parameters.count();
                var i: usize = 0;
                var it = function_def.parameters.iterator();
                while (it.next()) |kv| : (i = i + 1) {
                    try kv.value_ptr.*.toString(writer);
                    try writer.writeAll(" ");
                    try writer.writeAll(kv.key_ptr.*);

                    if (i < size - 1) {
                        try writer.writeAll(", ");
                    }
                }

                try writer.writeAll(")");

                if (function_def.return_type.def_type != Type.Void) {
                    try writer.writeAll(" > ");
                    try function_def.return_type.toString(writer);
                }
            },
            .Type => try writer.writeAll("type"),
            .Void => try writer.writeAll("void"),

            .Placeholder => {
                try writer.print("{{PlaceholderDef @{}}}", .{@ptrToInt(self)});
            },
        }

        if (self.optional) {
            try writer.writeAll("?");
        }
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn toValue(self: *Self) Value {
        return Value{ .Obj = self.toObj() };
    }

    pub fn toInstance(self: *Self, allocator: Allocator, type_registry: *TypeRegistry) !*Self {
        var instance_type = try type_registry.getTypeDef(
            switch (self.def_type) {
                .Object => object: {
                    var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{ .ObjectInstance = try self.cloneNonOptional(type_registry) };

                    break :object Self{
                        .optional = self.optional,
                        .def_type = .ObjectInstance,
                        .resolved_type = resolved_type,
                    };
                },
                .Enum => enum_instance: {
                    var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{ .EnumInstance = try self.cloneNonOptional(type_registry) };

                    break :enum_instance Self{
                        .optional = self.optional,
                        .def_type = .EnumInstance,
                        .resolved_type = resolved_type,
                    };
                },
                .Placeholder => placeholder: {
                    var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                        .Placeholder = PlaceholderDef.init(
                            allocator,
                            self.resolved_type.?.Placeholder.where.clone(),
                        ),
                    };
                    placeholder_resolved_type.Placeholder.name = self.resolved_type.?.Placeholder.name;

                    break :placeholder Self{
                        .def_type = .Placeholder,
                        .resolved_type = placeholder_resolved_type,
                    };
                },
                else => self.*,
            },
        );

        if (self.def_type == .Placeholder and instance_type.def_type == .Placeholder) {
            try PlaceholderDef.link(self, instance_type, .Instance);
        }

        return instance_type;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Type) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    pub fn instanceEqlTypeUnion(a: *ObjTypeDef, b: *ObjTypeDef) bool {
        assert(a.def_type == .Object);
        assert(b.def_type == .Object);

        return a == b or (b.resolved_type.?.Object.super != null and instanceEqlTypeUnion(a, b.resolved_type.?.Object.super.?));
    }

    // Compare two type definitions
    pub fn eqlTypeUnion(a: TypeUnion, b: TypeUnion) bool {
        if (@as(Type, a) != @as(Type, b)) {
            return false;
        }

        return switch (a) {
            .Bool,
            .Number,
            .String,
            .Type,
            .Void,
            .Pattern,
            => return true,

            .Fiber => {
                return a.Fiber.return_type.eql(b.Fiber.return_type) and a.Fiber.yield_type.eql(b.Fiber.yield_type);
            },

            .ObjectInstance => {
                return a.ObjectInstance.eql(b.ObjectInstance) or instanceEqlTypeUnion(a.ObjectInstance, b.ObjectInstance);
            },
            .EnumInstance => return a.EnumInstance.eql(b.EnumInstance),

            .Object, .Enum => false, // Those are never equal even if definition is the same

            .List => return a.List.item_type.eql(b.List.item_type),
            .Map => return a.Map.key_type.eql(b.Map.key_type) and a.Map.value_type.eql(b.Map.value_type),
            .Function => {
                // Compare return type
                if (!a.Function.return_type.eql(b.Function.return_type)) {
                    return false;
                }

                // Compare yield type
                if (!a.Function.yield_type.eql(b.Function.yield_type)) {
                    return false;
                }

                // Compare arity
                if (a.Function.parameters.count() != b.Function.parameters.count()) {
                    return false;
                }

                // Compare parameters (we ignore argument names and only compare types)
                const a_keys: [][]const u8 = a.Function.parameters.keys();
                const b_keys: [][]const u8 = b.Function.parameters.keys();

                if (a_keys.len != b_keys.len) {
                    return false;
                }

                for (a_keys) |_, index| {
                    if (!a.Function.parameters.get(a_keys[index]).?
                        .eql(b.Function.parameters.get(b_keys[index]).?))
                    {
                        return false;
                    }
                }

                return true;
            },

            .Placeholder => true, // TODO: should it be false?
            .Native => {
                // Compare return types
                if (a.Native.return_type.eql(b.Native.return_type)) {
                    return false;
                }

                // Compare arity
                if (a.Native.parameters.count() != b.Native.parameters.count()) {
                    return false;
                }

                // Compare parameters
                var it = a.Native.parameters.iterator();
                while (it.next()) |kv| {
                    if (b.Native.parameters.get(kv.key_ptr.*)) |value| {
                        if (!kv.value_ptr.*.eql(value)) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }

                return true;
            },
        };
    }

    // Compare two type definitions
    pub fn eql(self: *Self, other: *Self) bool {
        // zig fmt: off
        const type_eql: bool = self.def_type == other.def_type
            and (
                (self.resolved_type == null and other.resolved_type == null)
                    or eqlTypeUnion(self.resolved_type.?, other.resolved_type.?)
            );

        return self == other
            or (self.optional and other.def_type == .Void) // Void is equal to any optional type
            or (
                (type_eql or other.def_type == .Placeholder or self.def_type == .Placeholder)
                and (self.optional or !other.optional)
            );
        // zig fmt: on
    }
};

pub fn cloneObject(obj: *Obj, vm: *VM) !Value {
    switch (obj.obj_type) {
        .String,
        .Type,
        .UpValue,
        .Closure,
        .Function,
        .Object,
        .Enum,
        .EnumInstance,
        .Bound,
        .Native,
        .UserData,
        .Pattern,
        .Fiber,
        => return Value{ .Obj = obj },

        .List => {
            const list = ObjList.cast(obj).?;

            return (try allocateObject(
                vm,
                ObjList,
                .{
                    .type_def = list.type_def,
                    .items = try list.items.clone(),
                    .methods = list.methods,
                },
            )).toValue();
        },

        .Map => {
            const map = ObjMap.cast(obj).?;

            return (try allocateObject(
                vm,
                ObjMap,
                .{
                    .type_def = map.type_def,
                    .map = try map.map.clone(),
                    .methods = map.methods,
                },
            )).toValue();
        },

        // TODO
        .ObjectInstance => unreachable,
    }
}

pub fn objToString(writer: std.ArrayList(u8).Writer, obj: *Obj) (Allocator.Error || std.fmt.BufPrintError)!void {
    return switch (obj.obj_type) {
        .String => {
            const str = ObjString.cast(obj).?.string;

            try writer.print("{s}", .{str});
        },
        .Pattern => {
            const pattern = ObjPattern.cast(obj).?.source;

            try writer.print("{s}", .{pattern});
        },
        .Fiber => {
            const fiber = ObjFiber.cast(obj).?.fiber;

            try writer.print("fiber: 0x{x}", .{@ptrToInt(fiber)});
        },
        .Type => {
            const type_def: *ObjTypeDef = ObjTypeDef.cast(obj).?;

            try writer.print("type: 0x{x} `", .{
                @ptrToInt(type_def),
            });

            try type_def.toString(writer);

            try writer.writeAll("`");
        },
        .UpValue => {
            const upvalue: *ObjUpValue = ObjUpValue.cast(obj).?;

            try valueToString(writer, upvalue.closed orelse upvalue.location.*);
        },
        .Closure => try writer.print("closure: 0x{x} `{s}`", .{
            @ptrToInt(ObjClosure.cast(obj).?),
            ObjClosure.cast(obj).?.function.name.string,
        }),
        .Function => try writer.print("function: 0x{x} `{s}`", .{
            @ptrToInt(ObjFunction.cast(obj).?),
            ObjFunction.cast(obj).?.name.string,
        }),
        .ObjectInstance => try writer.print("object instance: 0x{x} `{s}`", .{
            @ptrToInt(ObjObjectInstance.cast(obj).?),
            ObjObjectInstance.cast(obj).?.object.name.string,
        }),
        .Object => try writer.print("object: 0x{x} `{s}`", .{
            @ptrToInt(ObjObject.cast(obj).?),
            ObjObject.cast(obj).?.name.string,
        }),
        .List => {
            const list: *ObjList = ObjList.cast(obj).?;

            try writer.print("list: 0x{x} [", .{@ptrToInt(list)});

            try list.type_def.resolved_type.?.List.item_type.toString(writer);

            try writer.writeAll("]");
        },
        .Map => {
            const map: *ObjMap = ObjMap.cast(obj).?;

            try writer.print("map: 0x{x} {{", .{
                @ptrToInt(map),
            });

            try map.type_def.resolved_type.?.Map.key_type.toString(writer);

            try writer.writeAll(", ");

            try map.type_def.resolved_type.?.Map.value_type.toString(writer);

            try writer.writeAll("}");
        },
        .Enum => try writer.print("enum: 0x{x} `{s}`", .{
            @ptrToInt(ObjEnum.cast(obj).?),
            ObjEnum.cast(obj).?.name.string,
        }),
        .EnumInstance => enum_instance: {
            var instance: *ObjEnumInstance = ObjEnumInstance.cast(obj).?;
            var enum_: *ObjEnum = instance.enum_ref;

            break :enum_instance try writer.print("{s}.{s}", .{
                enum_.name.string,
                enum_.type_def.resolved_type.?.Enum.cases.items[instance.case],
            });
        },
        .Bound => {
            const bound: *ObjBoundMethod = ObjBoundMethod.cast(obj).?;

            if (bound.closure) |closure| {
                var closure_name: []const u8 = closure.function.name.string;
                try writer.writeAll("bound method: ");

                try valueToString(writer, bound.receiver);

                try writer.print(" to {s}", .{closure_name});
            } else {
                assert(bound.native != null);
                try writer.writeAll("bound method: ");

                try valueToString(writer, bound.receiver);

                try writer.print(" to native 0x{}", .{@ptrToInt(bound.native.?)});
            }
        },
        .Native => {
            var native: *ObjNative = ObjNative.cast(obj).?;

            try writer.print("native: 0x{x}", .{@ptrToInt(native)});
        },
        .UserData => {
            var userdata: *ObjUserData = ObjUserData.cast(obj).?;

            try writer.print("userdata: 0x{x}", .{@ptrToInt(userdata)});
        },
    };
}

pub const PlaceholderDef = struct {
    const Self = @This();

    // TODO: are relations enough and booleans useless?
    const PlaceholderRelation = enum {
        Call,
        Yield,
        Subscript,
        Key,
        FieldAccess,
        Assignment,
        Instance,
        Optional,
        Unwrap,
    };

    name: ?*ObjString = null,
    where: Token, // Where the placeholder was created
    // When accessing/calling/subscrit/assign a placeholder we produce another. We keep them linked so we
    // can trace back the root of the unknown type.
    parent: ?*ObjTypeDef = null,
    // What's the relation with the parent?
    parent_relation: ?PlaceholderRelation = null,
    // Children adds themselves here
    children: std.ArrayList(*ObjTypeDef),

    pub fn init(allocator: Allocator, where: Token) Self {
        return Self{
            .where = where.clone(),
            .children = std.ArrayList(*ObjTypeDef).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.children.deinit();
    }

    pub fn link(parent: *ObjTypeDef, child: *ObjTypeDef, relation: PlaceholderRelation) !void {
        assert(parent.def_type == .Placeholder);
        assert(child.def_type == .Placeholder);

        if (parent == child) {
            return;
        }

        if (child.resolved_type.?.Placeholder.parent != null) {
            if (Config.debug_placeholders) {
                std.debug.print(
                    ">>> Placeholder @{} ({s}) has already a {} relation with @{} ({s})\n",
                    .{
                        @ptrToInt(child),
                        if (child.resolved_type.?.Placeholder.name) |name| name.string else "unknown",
                        child.resolved_type.?.Placeholder.parent_relation.?,
                        @ptrToInt(child.resolved_type.?.Placeholder.parent.?),
                        if (child.resolved_type.?.Placeholder.parent.?.resolved_type.?.Placeholder.name) |name| name.string else "unknown",
                    },
                );
            }
            return;
        }

        child.resolved_type.?.Placeholder.parent = parent;
        try parent.resolved_type.?.Placeholder.children.append(child);
        child.resolved_type.?.Placeholder.parent_relation = relation;

        if (Config.debug_placeholders) {
            std.debug.print(
                "Linking @{} (root: {}) with @{} as {}\n",
                .{
                    @ptrToInt(parent),
                    parent.resolved_type.?.Placeholder.parent == null,
                    @ptrToInt(child),
                    relation,
                },
            );
        }
    }
};
