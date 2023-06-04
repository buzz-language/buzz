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
const GarbageCollector = _memory.GarbageCollector;
const TypeRegistry = _memory.TypeRegistry;
const _value = @import("./value.zig");
const Token = @import("./token.zig").Token;
const BuildOptions = @import("build_options");
const CodeGen = @import("./codegen.zig").CodeGen;
const buzz_api = @import("./buzz_api.zig");
const _node = @import("./node.zig");
const FunctionNode = _node.FunctionNode;
const buzz_builtin = @import("./builtin.zig");

pub const pcre = @import("./pcre.zig").pcre;

const Value = _value.Value;
const ValueType = _value.ValueType;
const floatToInteger = _value.floatToInteger;
const valueToString = _value.valueToString;
const valueEql = _value.valueEql;
const valueIs = _value.valueIs;
const valueTypeEql = _value.valueTypeEql;

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

pub const Obj = struct {
    const Self = @This();

    obj_type: ObjType,
    is_marked: bool = false,
    // True when old obj and was modified
    is_dirty: bool = false,
    node: ?*std.TailQueue(*Obj).Node = null,

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
                    Value.fromObj(type_def.toObj()),
                    upvalue.closed orelse upvalue.location.*,
                );
            },
            .Closure => ObjClosure.cast(self).?.function.toObj().is(type_def),
            .List => ObjList.cast(self).?.type_def.eql(type_def),
            .Map => ObjMap.cast(self).?.type_def.eql(type_def),
            .Bound => bound: {
                const bound: *ObjBoundMethod = ObjBoundMethod.cast(self).?;
                break :bound valueIs(
                    Value.fromObj(type_def.toObj()),
                    Value.fromObj(if (bound.closure) |cls| cls.function.toObj() else bound.native.?.toObj()),
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
                if (BuildOptions.debug) {
                    assert(self != other or mem.eql(u8, ObjString.cast(self).?.string, ObjString.cast(other).?.string));
                    assert(self == other or !mem.eql(u8, ObjString.cast(self).?.string, ObjString.cast(other).?.string));
                }

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

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markFiber(self.fiber);
        try gc.markObj(self.type_def.toObj());
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Fiber) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    const members = std.ComptimeStringMap(
        NativeFn,
        .{
            .{ "over", buzz_builtin.fiber.over },
            .{ "cancel", buzz_builtin.fiber.cancel },
        },
    );

    pub fn member(vm: *VM, method: *ObjString) !?*ObjNative {
        if (vm.gc.objfiber_members.get(method)) |umethod| {
            return umethod;
        }

        if (members.get(method.string)) |unativeFn| {
            var native: *ObjNative = try vm.gc.allocateObject(
                ObjNative,
                .{
                    // Complains about const qualifier discard otherwise
                    .native = @intToPtr(*anyopaque, @ptrToInt(unativeFn)),
                },
            );

            try vm.gc.objfiber_members.put(method, native);

            return native;
        }

        return null;
    }

    pub fn memberDef(parser: *Parser, method: []const u8) !?*ObjTypeDef {
        if (parser.gc.objfiber_memberDefs.get(method)) |umethod| {
            return umethod;
        }

        if (mem.eql(u8, method, "over")) {
            const native_type = try parser.parseTypeDefFrom("extern Function over() > bool");

            try parser.gc.objfiber_memberDefs.put("over", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "cancel")) {
            const native_type = try parser.parseTypeDefFrom("extern Function cancel() > void");

            try parser.gc.objfiber_memberDefs.put("cancel", native_type);

            return native_type;
        }

        return null;
    }

    pub const FiberDef = struct {
        const SelfFiberDef = @This();

        return_type: *ObjTypeDef,
        yield_type: *ObjTypeDef,

        pub fn mark(self: *SelfFiberDef, gc: *GarbageCollector) !void {
            try gc.markObj(self.return_type.toObj());
            try gc.markObj(self.yield_type.toObj());
        }
    };
};

pub const pcre_struct = switch (builtin.os.tag) {
    .linux => pcre.pcre,
    .freebsd, .openbsd => pcre.struct_real_pcre,
    .macos, .tvos, .watchos, .ios => pcre.struct_real_pcre8_or_16,
    else => unreachable,
};

// Patterns are pcre regex, @see https://www.pcre.org/original/doc/html/index.html
pub const ObjPattern = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Pattern },

    source: []const u8,
    pattern: *pcre_struct,

    pub fn mark(_: *Self, _: *GarbageCollector) !void {}

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Pattern) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    pub fn rawMatch(self: *Self, vm: *VM, subject: ?[*]const u8, len: usize, offset: *usize) !?*ObjList {
        if (subject == null) {
            return null;
        }

        var results: ?*ObjList = null;

        var output_vector: [3000]c_int = undefined;

        const rc = pcre.pcre_exec(
            self.pattern, // the compiled pattern
            null, // no extra data - we didn't study the pattern
            @ptrCast([*c]const u8, subject.?), // the subject string
            @intCast(c_int, len), // the length of the subject
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

                results = try vm.gc.allocateObject(
                    ObjList,
                    ObjList.init(
                        vm.gc.allocator,
                        try vm.gc.type_registry.getTypeDef(
                            ObjTypeDef{
                                .def_type = .String,
                            },
                        ),
                    ),
                );

                // Prevent gc collection
                vm.push(results.?.toValue());

                var i: usize = 0;
                while (i < rc) : (i += 1) {
                    try results.?.items.append(
                        (try vm.gc.copyString(
                            subject.?[@intCast(usize, output_vector[2 * i])..@intCast(usize, output_vector[2 * i + 1])],
                        )).toValue(),
                    );
                }

                _ = vm.pop();
            },
        }

        return results;
    }

    pub fn rawMatchAll(self: *Self, vm: *VM, subject: ?[*]const u8, len: usize) !?*ObjList {
        if (subject == null) {
            return null;
        }

        var results: ?*ObjList = null;
        var offset: usize = 0;
        while (true) {
            if (try self.rawMatch(vm, subject.?, len, &offset)) |matches| {
                var was_null = results == null;
                results = results orelse try vm.gc.allocateObject(
                    ObjList,
                    ObjList.init(vm.gc.allocator, matches.type_def),
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

    const members = std.ComptimeStringMap(
        NativeFn,
        .{
            .{ "match", buzz_builtin.pattern.match },
            .{ "matchAll", buzz_builtin.pattern.matchAll },
        },
    );

    pub fn member(vm: *VM, method: *ObjString) !?*ObjNative {
        if (vm.gc.objpattern_members.get(method)) |umethod| {
            return umethod;
        }

        if (members.get(method.string)) |nativeFn| {
            var native: *ObjNative = try vm.gc.allocateObject(
                ObjNative,
                .{
                    // Complains about const qualifier discard otherwise
                    .native = @intToPtr(*anyopaque, @ptrToInt(nativeFn)),
                },
            );

            try vm.gc.objpattern_members.put(method, native);

            return native;
        }

        return null;
    }

    pub fn memberDef(parser: *Parser, method: []const u8) !?*ObjTypeDef {
        if (parser.gc.objpattern_memberDefs.get(method)) |umethod| {
            return umethod;
        }

        if (mem.eql(u8, method, "match")) {
            const native_type = try parser.parseTypeDefFrom("extern Function match(str subject) > [str]?");

            try parser.gc.objpattern_memberDefs.put("match", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "matchAll")) {
            const native_type = try parser.parseTypeDefFrom("extern Function matchAll(str subject) > [[str]]?");

            try parser.gc.objpattern_memberDefs.put("matchAll", native_type);

            return native_type;
        }

        return null;
    }
};

pub const UserData = anyopaque;

/// User data, type around an opaque pointer
pub const ObjUserData = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .UserData },

    userdata: *UserData,

    pub fn mark(_: *Self, _: *GarbageCollector) void {}

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .UserData) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

/// A String
pub const ObjString = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .String },

    /// The actual string
    string: []const u8,

    pub fn mark(_: *Self, _: *GarbageCollector) !void {}

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .String) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    pub fn concat(self: *Self, vm: *VM, other: *Self) !*Self {
        var new_string: std.ArrayList(u8) = std.ArrayList(u8).init(vm.gc.allocator);
        try new_string.appendSlice(self.string);
        try new_string.appendSlice(other.string);

        return vm.gc.copyString(new_string.items);
    }

    pub fn next(self: *Self, vm: *VM, str_index: ?i32) !?i32 {
        if (str_index) |index| {
            if (index < 0 or index >= @intCast(i32, self.string.len)) {
                try vm.throw(VM.Error.OutOfBound, (try vm.gc.copyString("Out of bound access to str")).toValue());
            }

            return if (index + 1 >= @intCast(i32, self.string.len))
                null
            else
                index + 1;
        } else {
            return if (self.string.len > 0) @intCast(i32, 0) else null;
        }
    }

    pub const members = std.ComptimeStringMap(
        NativeFn,
        .{
            .{ "len", buzz_builtin.str.len },
            .{ "trim", buzz_builtin.str.trim },
            .{ "byte", buzz_builtin.str.byte },
            .{ "indexOf", buzz_builtin.str.indexOf },
            .{ "split", buzz_builtin.str.split },
            .{ "sub", buzz_builtin.str.sub },
            .{ "startsWith", buzz_builtin.str.startsWith },
            .{ "endsWith", buzz_builtin.str.endsWith },
            .{ "replace", buzz_builtin.str.replace },
            .{ "repeat", buzz_builtin.str.repeat },
            .{ "encodeBase64", buzz_builtin.str.encodeBase64 },
            .{ "decodeBase64", buzz_builtin.str.decodeBase64 },
            .{ "upper", buzz_builtin.str.upper },
            .{ "lower", buzz_builtin.str.lower },
        },
    );

    // TODO: find a way to return the same ObjNative pointer for the same type of Lists
    pub fn member(vm: *VM, method: *ObjString) !?*ObjNative {
        if (vm.gc.objstring_members.get(method)) |umethod| {
            return umethod;
        }

        if (members.get(method.string)) |nativeFn| {
            var native: *ObjNative = try vm.gc.allocateObject(
                ObjNative,
                .{
                    // Complains about const qualifier discard otherwise
                    .native = @intToPtr(*anyopaque, @ptrToInt(nativeFn)),
                },
            );

            try vm.gc.objstring_members.put(method, native);

            return native;
        }

        return null;
    }

    pub fn memberDef(parser: *Parser, method: []const u8) !?*ObjTypeDef {
        if (parser.gc.objstring_memberDefs.get(method)) |umethod| {
            return umethod;
        }

        if (mem.eql(u8, method, "len")) {
            const native_type = try parser.parseTypeDefFrom("extern Function len() > int");

            try parser.gc.objstring_memberDefs.put("len", native_type);

            return native_type;
        }
        if (mem.eql(u8, method, "trim")) {
            const native_type = try parser.parseTypeDefFrom("extern Function trim() > str");

            try parser.gc.objstring_memberDefs.put("trim", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "byte")) {
            const native_type = try parser.parseTypeDefFrom("extern Function byte(int at) > int");

            try parser.gc.objstring_memberDefs.put("byte", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "indexOf")) {
            const native_type = try parser.parseTypeDefFrom("extern Function indexOf(str needle) > int?");

            try parser.gc.objstring_memberDefs.put("indexOf", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "startsWith")) {
            const native_type = try parser.parseTypeDefFrom("extern Function startsWith(str needle) > bool");

            try parser.gc.objstring_memberDefs.put("startsWith", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "endsWith")) {
            const native_type = try parser.parseTypeDefFrom("extern Function endsWith(str needle) > bool");

            try parser.gc.objstring_memberDefs.put("endsWith", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "replace")) {
            const native_type = try parser.parseTypeDefFrom("extern Function replace(str needle, str with) > str");

            try parser.gc.objstring_memberDefs.put("replace", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "split")) {
            const native_type = try parser.parseTypeDefFrom("extern Function split(str separator) > [str]");

            try parser.gc.objstring_memberDefs.put("split", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "sub")) {
            const native_type = try parser.parseTypeDefFrom("extern Function sub(int start, int? len) > str");

            try parser.gc.objstring_memberDefs.put("sub", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "repeat")) {
            const native_type = try parser.parseTypeDefFrom("extern Function repeat(int n) > str");

            try parser.gc.objstring_memberDefs.put("repeat", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "encodeBase64")) {
            const native_type = try parser.parseTypeDefFrom("extern Function encodeBase64() > str");

            try parser.gc.objstring_memberDefs.put("encodeBase64", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "decodeBase64")) {
            const native_type = try parser.parseTypeDefFrom("extern Function decodeBase64() > str");

            try parser.gc.objstring_memberDefs.put("decodeBase64", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "upper")) {
            const native_type = try parser.parseTypeDefFrom("extern Function upper() > str");

            try parser.gc.objstring_memberDefs.put("upper", native_type);

            return native_type;
        } else if (mem.eql(u8, method, "lower")) {
            const native_type = try parser.parseTypeDefFrom("extern Function lower() > str");

            try parser.gc.objstring_memberDefs.put("lower", native_type);

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

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markValue(self.location.*); // Useless
        if (self.closed) |uclosed| {
            try gc.markValue(uclosed);
        }
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
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

    // Buzz function
    function: *ObjFunction,

    upvalues: std.ArrayList(*ObjUpValue),
    // Pointer to the global with which the function was declared
    globals: *std.ArrayList(Value),

    pub fn init(allocator: Allocator, vm: *VM, function: *ObjFunction) !Self {
        return Self{
            // TODO: copy?
            .globals = &vm.globals,
            .function = function,
            .upvalues = try std.ArrayList(*ObjUpValue).initCapacity(allocator, function.upvalue_count),
        };
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markObj(self.function.toObj());
        for (self.upvalues.items) |upvalue| {
            try gc.markObj(upvalue.toObj());
        }
        for (self.globals.items) |global| {
            try gc.markValue(global);
        }
    }

    pub fn deinit(self: *Self) void {
        self.upvalues.deinit();
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Closure) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

pub const NativeCtx = extern struct {
    vm: *VM,
    globals: [*]Value,
    upvalues: [*]*ObjUpValue,
    // Where to reset the stack when we exit the function
    base: [*]Value,
    // Pointer to the stack_top field of the current fiber
    // !! Needs to change when current fiber changes !!
    stack_top: *[*]Value,
};

// 1 = return value on stack, 0 = no return value, -1 = error
pub const Native = fn (ctx: *NativeCtx) c_int;
pub const NativeFn = *const Native;

pub const Compiled = fn (ctx: *NativeCtx) Value;
pub const CompiledFn = *const Compiled;

/// Native function
pub const ObjNative = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .Native },

    // TODO: issue is list.member which separate its type definition from its runtime creation
    // type_def: *ObjTypeDef,
    native: *anyopaque,

    pub fn mark(_: *Self, _: *GarbageCollector) void {}

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Native) {
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
        Test,
        Anonymous,
        Extern,
        Abstract, // for protocol method, so we don't parse a body
    };

    obj: Obj = .{ .obj_type = .Function },

    type_def: *ObjTypeDef = undefined, // Undefined because function initialization is in several steps

    name: *ObjString,
    chunk: Chunk,
    upvalue_count: u8 = 0,

    // So we can JIT the function at runtime
    node: *anyopaque,
    // How many time the function was called
    call_count: u128 = 0,

    // JIT compiled function
    native_raw: ?*anyopaque = null,

    // JIT compiled function callable by buzz VM
    native: ?*anyopaque = null,

    pub fn init(allocator: Allocator, node: *FunctionNode, name: *ObjString) !Self {
        return Self{
            .name = name,
            .node = node,
            .chunk = Chunk.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.chunk.deinit();
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markObj(self.name.toObj());
        try gc.markObj(self.type_def.toObj());
        if (BuildOptions.gc_debug) {
            std.debug.print("MARKING CONSTANTS OF FUNCTION @{} {s}\n", .{ @ptrToInt(self), self.name.string });
        }
        for (self.chunk.constants.items) |constant| {
            try gc.markValue(constant);
        }
        if (BuildOptions.gc_debug) {
            std.debug.print("DONE MARKING CONSTANTS OF FUNCTION @{} {s}\n", .{ @ptrToInt(self), self.name.string });
        }
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Function) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    pub const FunctionDef = struct {
        const FunctionDefSelf = @This();

        var next_id: usize = 0;

        id: usize,
        name: *ObjString,
        script_name: *ObjString,
        return_type: *ObjTypeDef,
        yield_type: *ObjTypeDef,
        error_types: ?[]const *ObjTypeDef = null,
        parameters: std.AutoArrayHashMap(*ObjString, *ObjTypeDef),
        // Storing here the defaults means they can only be non-Obj values
        defaults: std.AutoArrayHashMap(*ObjString, Value),
        function_type: FunctionType = .Function,
        lambda: bool = false,

        generic_types: std.AutoArrayHashMap(*ObjString, *ObjTypeDef),

        pub fn nextId() usize {
            FunctionDefSelf.next_id += 1;

            return FunctionDefSelf.next_id;
        }

        pub fn mark(self: *FunctionDefSelf, gc: *GarbageCollector) !void {
            try gc.markObj(self.name.toObj());
            try gc.markObj(self.script_name.toObj());
            try gc.markObj(self.return_type.toObj());
            try gc.markObj(self.yield_type.toObj());

            var it = self.parameters.iterator();
            while (it.next()) |parameter| {
                try gc.markObj(parameter.key_ptr.*.toObj());
                try gc.markObj(parameter.value_ptr.*.toObj());
            }

            var it2 = self.defaults.iterator();
            while (it2.next()) |default| {
                try gc.markObj(default.key_ptr.*.toObj());
                try gc.markValue(default.value_ptr.*);
            }

            if (self.error_types) |error_types| {
                for (error_types) |error_item| {
                    try gc.markObj(error_item.toObj());
                }
            }

            var it3 = self.generic_types.iterator();
            while (it3.next()) |kv| {
                try gc.markObj(kv.key_ptr.*.toObj());
                try gc.markObj(kv.value_ptr.*.toObj());
            }
        }
    };
};

/// Object instance
pub const ObjObjectInstance = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .ObjectInstance },

    /// Object (null when anonymous)
    object: ?*ObjObject,
    /// Object type (null when not anonymous)
    type_def: ?*ObjTypeDef,
    /// Fields value
    fields: std.AutoHashMap(*ObjString, Value),

    pub fn setField(self: *Self, gc: *GarbageCollector, key: *ObjString, value: Value) !void {
        try self.fields.put(key, value);
        try gc.markObjDirty(&self.obj);
    }

    pub fn init(allocator: Allocator, object: ?*ObjObject, type_def: ?*ObjTypeDef) Self {
        return Self{
            .object = object,
            .type_def = type_def,
            .fields = std.AutoHashMap(*ObjString, Value).init(allocator),
        };
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        if (self.object) |object| {
            try gc.markObj(object.toObj());
        }
        if (self.type_def) |type_def| {
            try gc.markObj(type_def.toObj());
        }
        var it = self.fields.iterator();
        while (it.next()) |kv| {
            try gc.markObj(kv.key_ptr.*.toObj());
            try gc.markValue(kv.value_ptr.*);
        }
    }

    pub fn deinit(self: *Self) void {
        self.fields.deinit();
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .ObjectInstance) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    fn is(self: *Self, instance_type: ?*ObjTypeDef, type_def: *ObjTypeDef) bool {
        if (type_def.def_type == .Object) {
            const object_def: *ObjTypeDef = instance_type orelse (if (self.object) |object| object.type_def else self.type_def.?.resolved_type.?.ObjectInstance);

            return object_def == type_def;
        } else if (type_def.def_type == .Protocol) {
            const object_def: *ObjTypeDef = instance_type orelse (if (self.object) |object| object.type_def else self.type_def.?.resolved_type.?.ObjectInstance);

            return object_def.resolved_type.?.Object.conforms_to.get(type_def) != null;
        }

        return false;
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

    pub fn init(allocator: Allocator, name: *ObjString, type_def: *ObjTypeDef) Self {
        return Self{
            .name = name,
            .methods = std.AutoHashMap(*ObjString, *ObjClosure).init(allocator),
            .fields = std.AutoHashMap(*ObjString, Value).init(allocator),
            .static_fields = std.AutoHashMap(*ObjString, Value).init(allocator),
            .type_def = type_def,
        };
    }

    pub fn setField(self: *Self, gc: *GarbageCollector, key: *ObjString, value: Value) !void {
        try self.fields.put(key, value);
        try gc.markObjDirty(&self.obj);
    }

    pub fn setStaticField(self: *Self, gc: *GarbageCollector, key: *ObjString, value: Value) !void {
        try self.static_fields.put(key, value);
        try gc.markObjDirty(&self.obj);
    }

    pub fn setMethod(self: *Self, gc: *GarbageCollector, key: *ObjString, closure: *ObjClosure) !void {
        try self.methods.put(key, closure);
        try gc.markObjDirty(&self.obj);
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markObj(self.type_def.toObj());
        try gc.markObj(self.name.toObj());
        var it = self.methods.iterator();
        while (it.next()) |kv| {
            try gc.markObj(kv.key_ptr.*.toObj());
            try gc.markObj(kv.value_ptr.*.toObj());
        }
        var it2 = self.fields.iterator();
        while (it2.next()) |kv| {
            try gc.markObj(kv.key_ptr.*.toObj());
            try gc.markValue(kv.value_ptr.*);
        }
        var it3 = self.static_fields.iterator();
        while (it3.next()) |kv| {
            try gc.markObj(kv.key_ptr.*.toObj());
            try gc.markValue(kv.value_ptr.*);
        }
    }

    pub fn deinit(self: *Self) void {
        self.methods.deinit();
        self.fields.deinit();
        self.static_fields.deinit();
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Object) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    pub const ProtocolDef = struct {
        const ProtocolDefSelf = @This();

        name: *ObjString,
        qualified_name: *ObjString,
        methods: std.StringArrayHashMap(*ObjTypeDef),

        pub fn init(allocator: Allocator, name: *ObjString, qualified_name: *ObjString) ProtocolDefSelf {
            return ProtocolDefSelf{
                .name = name,
                .qualified_name = qualified_name,
                .methods = std.StringArrayHashMap(*ObjTypeDef).init(allocator),
            };
        }

        pub fn deinit(self: *ProtocolDefSelf) void {
            self.methods.deinit();
        }

        pub fn mark(self: *ProtocolDefSelf, gc: *GarbageCollector) !void {
            try gc.markObj(self.name.toObj());
            try gc.markObj(self.qualified_name.toObj());

            var it = self.methods.iterator();
            while (it.next()) |kv| {
                try gc.markObj(kv.value_ptr.*.toObj());
            }
        }
    };

    pub const ObjectDef = struct {
        const ObjectDefSelf = @This();

        name: *ObjString,
        qualified_name: *ObjString,
        // TODO: Do i need to have two maps ?
        fields: std.StringArrayHashMap(*ObjTypeDef),
        fields_defaults: std.StringArrayHashMap(void),
        static_fields: std.StringArrayHashMap(*ObjTypeDef),
        methods: std.StringArrayHashMap(*ObjTypeDef),
        // When we have placeholders we don't know if they are properties or methods
        // That information is available only when the placeholder is resolved
        placeholders: std.StringHashMap(*ObjTypeDef),
        static_placeholders: std.StringHashMap(*ObjTypeDef),
        anonymous: bool,
        conforms_to: std.AutoHashMap(*ObjTypeDef, void),

        pub fn init(allocator: Allocator, name: *ObjString, qualified_name: *ObjString, anonymous: bool) ObjectDefSelf {
            return ObjectDefSelf{
                .name = name,
                .qualified_name = qualified_name,
                .fields = std.StringArrayHashMap(*ObjTypeDef).init(allocator),
                .static_fields = std.StringArrayHashMap(*ObjTypeDef).init(allocator),
                .fields_defaults = std.StringArrayHashMap(void).init(allocator),
                .methods = std.StringArrayHashMap(*ObjTypeDef).init(allocator),
                .placeholders = std.StringHashMap(*ObjTypeDef).init(allocator),
                .static_placeholders = std.StringHashMap(*ObjTypeDef).init(allocator),
                .anonymous = anonymous,
                .conforms_to = std.AutoHashMap(*ObjTypeDef, void).init(allocator),
            };
        }

        pub fn deinit(self: *ObjectDefSelf) void {
            self.fields.deinit();
            self.static_fields.deinit();
            self.fields_defaults.deinit();
            self.methods.deinit();
            self.placeholders.deinit();
            self.static_placeholders.deinit();
            self.conforms_to.deinit();
        }

        // Do they both conform to a common protocol?
        pub fn both_conforms(self: ObjectDefSelf, other: ObjectDefSelf) ?*ObjTypeDef {
            var it = self.conforms_to.iterator();
            while (it.next()) |kv| {
                if (other.conforms_to.get(kv.key_ptr.*) != null) {
                    return kv.key_ptr.*;
                }
            }

            return null;
        }

        pub fn mark(self: *ObjectDefSelf, gc: *GarbageCollector) !void {
            try gc.markObj(self.name.toObj());
            try gc.markObj(self.qualified_name.toObj());

            var it = self.fields.iterator();
            while (it.next()) |kv| {
                try gc.markObj(kv.value_ptr.*.toObj());
            }

            var it3 = self.static_fields.iterator();
            while (it3.next()) |kv| {
                try gc.markObj(kv.value_ptr.*.toObj());
            }

            var it4 = self.methods.iterator();
            while (it4.next()) |kv| {
                try gc.markObj(kv.value_ptr.*.toObj());
            }

            var it5 = self.placeholders.iterator();
            while (it5.next()) |kv| {
                try gc.markObj(kv.value_ptr.*.toObj());
            }

            var it6 = self.static_placeholders.iterator();
            while (it6.next()) |kv| {
                try gc.markObj(kv.value_ptr.*.toObj());
            }

            var it7 = self.conforms_to.iterator();
            while (it7.next()) |kv| {
                try gc.markObj(kv.key_ptr.*.toObj());
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

    methods: std.AutoHashMap(*ObjString, *ObjNative),

    pub fn init(allocator: Allocator, type_def: *ObjTypeDef) Self {
        return Self{
            .items = std.ArrayList(Value).init(allocator),
            .type_def = type_def,
            .methods = std.AutoHashMap(*ObjString, *ObjNative).init(allocator),
        };
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        for (self.items.items) |value| {
            try gc.markValue(value);
        }
        try gc.markObj(self.type_def.toObj());
        var it = self.methods.iterator();
        while (it.next()) |kv| {
            try gc.markObj(kv.key_ptr.*.toObj());
            try gc.markObj(kv.value_ptr.*.toObj());
        }
    }

    pub fn deinit(self: *Self) void {
        self.items.deinit();
        self.methods.deinit();
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .List) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    const members = std.ComptimeStringMap(
        NativeFn,
        .{
            .{ "append", buzz_builtin.list.append },
            .{ "remove", buzz_builtin.list.remove },
            .{ "len", buzz_builtin.list.len },
            .{ "next", buzz_builtin.list.next },
            .{ "sub", buzz_builtin.list.sub },
            .{ "indexOf", buzz_builtin.list.indexOf },
            .{ "join", buzz_builtin.list.join },
            .{ "insert", buzz_builtin.list.insert },
            .{ "pop", buzz_builtin.list.pop },
            .{ "forEach", buzz_builtin.list.forEach },
            .{ "map", buzz_builtin.list.map },
            .{ "filter", buzz_builtin.list.filter },
            .{ "reduce", buzz_builtin.list.reduce },
            .{ "sort", buzz_builtin.list.sort },
        },
    );

    // TODO: find a way to return the same ObjNative pointer for the same type of Lists
    pub fn member(self: *Self, vm: *VM, method: *ObjString) !?*ObjNative {
        if (self.methods.get(method)) |native| {
            return native;
        }

        if (members.get(method.string)) |nativeFn| {
            var native: *ObjNative = try vm.gc.allocateObject(
                ObjNative,
                .{
                    // Complains about const qualifier discard otherwise
                    .native = @intToPtr(*anyopaque, @ptrToInt(nativeFn)),
                },
            );

            try self.methods.put(method, native);

            return native;
        }

        return null;
    }

    pub fn rawAppend(self: *Self, gc: *GarbageCollector, value: Value) !void {
        try self.items.append(value);
        try gc.markObjDirty(&self.obj);
    }

    pub fn rawInsert(self: *Self, gc: *GarbageCollector, index: usize, value: Value) !void {
        try self.items.insert(index, value);
        try gc.markObjDirty(&self.obj);
    }

    pub fn set(self: *Self, gc: *GarbageCollector, index: usize, value: Value) !void {
        self.items.items[index] = value;
        try gc.markObjDirty(&self.obj);
    }

    // Used also by the VM
    pub fn rawNext(self: *Self, vm: *VM, list_index: ?i32) !?i32 {
        if (list_index) |index| {
            if (index < 0 or index >= @intCast(i32, self.items.items.len)) {
                try vm.throw(VM.Error.OutOfBound, (try vm.gc.copyString("Out of bound access to list")).toValue());
            }

            return if (index + 1 >= @intCast(i32, self.items.items.len))
                null
            else
                index + 1;
        } else {
            return if (self.items.items.len > 0) @intCast(i32, 0) else null;
        }
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

        pub fn mark(self: *SelfListDef, gc: *GarbageCollector) !void {
            try gc.markObj(self.item_type.toObj());
            var it = self.methods.iterator();
            while (it.next()) |method| {
                try gc.markObj(method.value_ptr.*.toObj());
            }
        }

        pub fn member(obj_list: *ObjTypeDef, parser: *Parser, method: []const u8) !?*ObjTypeDef {
            var self = obj_list.resolved_type.?.List;

            if (self.methods.get(method)) |native_def| {
                return native_def;
            }

            if (mem.eql(u8, method, "append")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                // `value` arg is of item_type
                try parameters.put(try parser.gc.copyString("value"), self.item_type);

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("append"),
                    .parameters = parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = obj_list,
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(ObjTypeDef{ .def_type = .Function, .resolved_type = resolved_type });

                try self.methods.put("append", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "remove")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var at_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Integer,
                        .optional = false,
                    },
                );

                try parameters.put(try parser.gc.copyString("at"), at_type);

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("remove"),
                    .parameters = parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(.{
                        .optional = true,
                        .def_type = self.item_type.def_type,
                        .resolved_type = self.item_type.resolved_type,
                    }),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("remove", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "len")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("len"),
                    .parameters = parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(
                        ObjTypeDef{
                            .def_type = .Integer,
                        },
                    ),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("len", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "next")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                // `key` arg is number
                try parameters.put(
                    try parser.gc.copyString("key"),
                    try parser.gc.type_registry.getTypeDef(
                        ObjTypeDef{
                            .def_type = .Integer,
                            .optional = true,
                        },
                    ),
                );

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("next"),
                    .parameters = parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    // When reached end of list, returns null
                    .return_type = try parser.gc.type_registry.getTypeDef(
                        ObjTypeDef{
                            .def_type = .Integer,
                            .optional = true,
                        },
                    ),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("next", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "sub")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put(
                    try parser.gc.copyString("start"),
                    try parser.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Integer,
                        },
                    ),
                );

                const len_str = try parser.gc.copyString("len");
                try parameters.put(
                    len_str,
                    try parser.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Integer,
                            .optional = true,
                        },
                    ),
                );

                var defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator);
                try defaults.put(len_str, Value.Null);

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("sub"),
                    .parameters = parameters,
                    .defaults = defaults,
                    .return_type = obj_list,
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("sub", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "indexOf")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put(try parser.gc.copyString("needle"), self.item_type);

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("indexOf"),
                    .parameters = parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Integer,
                            .optional = true,
                        },
                    ),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("indexOf", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "join")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the string.

                try parameters.put(try parser.gc.copyString("separator"), try parser.gc.type_registry.getTypeDef(.{ .def_type = .String }));

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("join"),
                    .parameters = parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(ObjTypeDef{
                        .def_type = .String,
                    }),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("join", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "insert")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                try parameters.put(
                    try parser.gc.copyString("index"),
                    try parser.gc.type_registry.getTypeDef(.{
                        .def_type = .Integer,
                    }),
                );

                // `value` arg is of item_type
                try parameters.put(try parser.gc.copyString("value"), self.item_type);

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("insert"),
                    .parameters = parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try self.item_type.cloneOptional(&parser.gc.type_registry),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(ObjTypeDef{ .def_type = .Function, .resolved_type = resolved_type });

                try self.methods.put("insert", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "pop")) {
                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("pop"),
                    .parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try self.item_type.cloneOptional(&parser.gc.type_registry),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(ObjTypeDef{ .def_type = .Function, .resolved_type = resolved_type });

                try self.methods.put("pop", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "forEach")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(try parser.gc.copyString("index"), try parser.gc.type_registry.getTypeDef(.{ .def_type = .Integer }));
                try callback_parameters.put(try parser.gc.copyString("element"), self.item_type);

                var callback_method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    // TODO: is this ok?
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("anonymous"),
                    .parameters = callback_parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                };

                var callback_resolved_type: ObjTypeDef.TypeUnion = .{ .Function = callback_method_def };

                var callback_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = callback_resolved_type,
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("forEach"),
                    .parameters = parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(ObjTypeDef{ .def_type = .Function, .resolved_type = resolved_type });

                try self.methods.put("forEach", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "map")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(try parser.gc.copyString("index"), try parser.gc.type_registry.getTypeDef(.{ .def_type = .Integer }));
                try callback_parameters.put(try parser.gc.copyString("element"), self.item_type);

                var callback_method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    // TODO: is this ok?
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("anonymous"),
                    .parameters = callback_parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = undefined,
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    // FIXME: user could provide an .Extern function and JIT will be lost here
                };

                const map_origin = ObjFunction.FunctionDef.nextId();

                // Mapped type
                const generic = ObjTypeDef.GenericDef{
                    .origin = map_origin,
                    .index = 0,
                };
                const generic_resolved_type = ObjTypeDef.TypeUnion{ .Generic = generic };
                const generic_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Generic,
                        .resolved_type = generic_resolved_type,
                    },
                );

                callback_method_def.return_type = generic_type;

                var callback_resolved_type: ObjTypeDef.TypeUnion = .{ .Function = callback_method_def };

                var callback_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = callback_resolved_type,
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );

                var new_list_def = ObjList.ListDef.init(parser.gc.allocator, generic_type);

                var new_list_type = ObjTypeDef.TypeUnion{ .List = new_list_def };

                var method_def = ObjFunction.FunctionDef{
                    .id = map_origin,
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("map"),
                    .parameters = parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(.{
                        .def_type = .List,
                        .resolved_type = new_list_type,
                    }),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                try method_def.generic_types.put(
                    try parser.gc.copyString("T"),
                    generic_type,
                );

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(ObjTypeDef{ .def_type = .Function, .resolved_type = resolved_type });

                try self.methods.put("map", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "filter")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(
                    try parser.gc.copyString("index"),
                    try parser.gc.type_registry.getTypeDef(.{ .def_type = .Integer }),
                );
                try callback_parameters.put(
                    try parser.gc.copyString("element"),
                    self.item_type,
                );

                var callback_method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    // TODO: is this ok?
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("anonymous"),
                    .parameters = callback_parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Bool }),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                };

                var callback_resolved_type: ObjTypeDef.TypeUnion = .{ .Function = callback_method_def };

                var callback_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = callback_resolved_type,
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("filter"),
                    .parameters = parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = obj_list,
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(ObjTypeDef{ .def_type = .Function, .resolved_type = resolved_type });

                try self.methods.put("filter", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "reduce")) {
                const reduce_origin = ObjFunction.FunctionDef.nextId();

                // Mapped type
                const generic = ObjTypeDef.GenericDef{
                    .origin = reduce_origin,
                    .index = 0,
                };
                const generic_resolved_type = ObjTypeDef.TypeUnion{ .Generic = generic };
                const generic_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Generic,
                        .resolved_type = generic_resolved_type,
                    },
                );

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(
                    try parser.gc.copyString("index"),
                    try parser.gc.type_registry.getTypeDef(.{ .def_type = .Integer }),
                );
                try callback_parameters.put(
                    try parser.gc.copyString("element"),
                    self.item_type,
                );
                try callback_parameters.put(
                    try parser.gc.copyString("accumulator"),
                    generic_type,
                );

                var callback_method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    // TODO: is this ok?
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("anonymous"),
                    .parameters = callback_parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = generic_type,
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                };

                var callback_resolved_type: ObjTypeDef.TypeUnion = .{ .Function = callback_method_def };

                var callback_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = callback_resolved_type,
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );
                try parameters.put(
                    try parser.gc.copyString("initial"),
                    generic_type,
                );

                var generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);
                try generic_types.put(try parser.gc.copyString("T"), generic_type);

                var method_def = ObjFunction.FunctionDef{
                    .id = reduce_origin,
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("reduce"),
                    .parameters = parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = generic_type,
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = generic_types,
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("reduce", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "sort")) {
                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                var callback_parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try callback_parameters.put(try parser.gc.copyString("left"), self.item_type);
                try callback_parameters.put(try parser.gc.copyString("right"), self.item_type);

                var callback_method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    // TODO: is this ok?
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("anonymous"),
                    .parameters = callback_parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Bool }),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                };

                var callback_resolved_type: ObjTypeDef.TypeUnion = .{ .Function = callback_method_def };

                var callback_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = callback_resolved_type,
                    },
                );

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                try parameters.put(
                    try parser.gc.copyString("callback"),
                    callback_type,
                );

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("sort"),
                    .parameters = parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = obj_list,
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("sort", native_type);

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
    map: std.AutoArrayHashMap(Value, Value),

    methods: std.AutoHashMap(*ObjString, *ObjNative),

    pub fn init(allocator: Allocator, type_def: *ObjTypeDef) Self {
        return .{
            .type_def = type_def,
            .map = std.AutoArrayHashMap(Value, Value).init(allocator),
            .methods = std.AutoHashMap(*ObjString, *ObjNative).init(allocator),
        };
    }

    pub fn set(self: *Self, gc: *GarbageCollector, key: Value, value: Value) !void {
        try self.map.put(key, value);
        try gc.markObjDirty(&self.obj);
    }

    const members = std.ComptimeStringMap(
        NativeFn,
        .{
            .{ "remove", buzz_builtin.map.remove },
            .{ "size", buzz_builtin.map.size },
            .{ "keys", buzz_builtin.map.keys },
            .{ "values", buzz_builtin.map.values },
            // TODO: next
        },
    );

    pub fn member(self: *Self, vm: *VM, method: *ObjString) !?*ObjNative {
        if (self.methods.get(method)) |native| {
            return native;
        }

        if (members.get(method.string)) |nativeFn| {
            var native: *ObjNative = try vm.gc.allocateObject(
                ObjNative,
                .{
                    // Complains about const qualifier discard otherwise
                    .native = @intToPtr(*anyopaque, @ptrToInt(nativeFn)),
                },
            );

            try self.methods.put(method, native);

            return native;
        }

        return null;
    }

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        var it = self.map.iterator();
        while (it.next()) |kv| {
            try gc.markValue(kv.key_ptr.*);
            try gc.markValue(kv.value_ptr.*);
        }

        var it2 = self.methods.iterator();
        while (it2.next()) |kv| {
            try gc.markObj(kv.key_ptr.*.toObj());
            try gc.markObj(kv.value_ptr.*.toObj());
        }

        try gc.markObj(self.type_def.toObj());
    }

    pub fn rawNext(self: *Self, key: ?Value) ?Value {
        const map_keys: []Value = self.map.keys();

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

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
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

        pub fn mark(self: *SelfMapDef, gc: *GarbageCollector) !void {
            try gc.markObj(self.key_type.toObj());
            try gc.markObj(self.value_type.toObj());
            var it = self.methods.iterator();
            while (it.next()) |method| {
                try gc.markObj(method.value_ptr.*.toObj());
            }
        }

        pub fn member(obj_map: *ObjTypeDef, parser: *Parser, method: []const u8) !?*ObjTypeDef {
            var self = obj_map.resolved_type.?.Map;

            if (self.methods.get(method)) |native_def| {
                return native_def;
            }

            if (mem.eql(u8, method, "size")) {
                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("size"),
                    .parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(.{
                        .def_type = .Integer,
                    }),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("size", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "remove")) {
                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator);

                // We omit first arg: it'll be OP_SWAPed in and we already parsed it
                // It's always the list.

                try parameters.put(try parser.gc.copyString("at"), self.key_type);

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("remove"),
                    .parameters = parameters,
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(.{
                        .optional = true,
                        .def_type = self.value_type.def_type,
                        .resolved_type = self.value_type.resolved_type,
                    }),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("remove", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "keys")) {
                var list_def: ObjList.ListDef = ObjList.ListDef.init(
                    parser.gc.allocator,
                    self.key_type,
                );

                var list_def_union: ObjTypeDef.TypeUnion = .{
                    .List = list_def,
                };

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("keys"),
                    .parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(.{
                        .def_type = .List,
                        .optional = false,
                        .resolved_type = list_def_union,
                    }),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
                        .resolved_type = resolved_type,
                    },
                );

                try self.methods.put("keys", native_type);

                return native_type;
            } else if (mem.eql(u8, method, "values")) {
                var list_def: ObjList.ListDef = ObjList.ListDef.init(
                    parser.gc.allocator,
                    self.value_type,
                );

                var list_def_union: ObjTypeDef.TypeUnion = .{
                    .List = list_def,
                };

                var method_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .script_name = try parser.gc.copyString("builtin"),
                    .name = try parser.gc.copyString("values"),
                    .parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .defaults = std.AutoArrayHashMap(*ObjString, Value).init(parser.gc.allocator),
                    .return_type = try parser.gc.type_registry.getTypeDef(.{
                        .def_type = .List,
                        .optional = false,
                        .resolved_type = list_def_union,
                    }),
                    .yield_type = try parser.gc.type_registry.getTypeDef(.{ .def_type = .Void }),
                    .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(parser.gc.allocator),
                    .function_type = .Extern,
                };

                var resolved_type: ObjTypeDef.TypeUnion = .{ .Function = method_def };

                var native_type = try parser.gc.type_registry.getTypeDef(
                    ObjTypeDef{
                        .def_type = .Function,
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

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markObj(self.name.toObj());
        try gc.markObj(self.type_def.toObj());
        for (self.cases.items) |case| {
            try gc.markValue(case);
        }
    }

    pub fn rawNext(self: *Self, vm: *VM, enum_case: ?*ObjEnumInstance) !?*ObjEnumInstance {
        if (enum_case) |case| {
            assert(case.enum_ref == self);

            if (case.case == self.cases.items.len - 1) {
                return null;
            }

            return try vm.gc.allocateObject(ObjEnumInstance, ObjEnumInstance{
                .enum_ref = self,
                .case = @intCast(u8, case.case + 1),
            });
        } else {
            return try vm.gc.allocateObject(ObjEnumInstance, ObjEnumInstance{
                .enum_ref = self,
                .case = 0,
            });
        }
    }

    pub fn deinit(self: *Self) void {
        self.cases.deinit();
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(&self.obj);
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Enum) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    pub const EnumDef = struct {
        const EnumDefSelf = @This();

        name: *ObjString,
        qualified_name: *ObjString,
        enum_type: *ObjTypeDef,
        // TODO: should be a slice
        cases: std.ArrayList([]const u8),

        pub fn init(allocator: Allocator, name: *ObjString, qualified_name: *ObjString, enum_type: *ObjTypeDef) EnumDefSelf {
            return EnumDefSelf{
                .name = name,
                .qualified_name = qualified_name,
                .cases = std.ArrayList([]const u8).init(allocator),
                .enum_type = enum_type,
            };
        }

        pub fn deinit(self: *EnumDefSelf) void {
            self.cases.deinit();
        }

        pub fn mark(self: *EnumDefSelf, gc: *GarbageCollector) !void {
            try gc.markObj(self.name.toObj());
            try gc.markObj(self.qualified_name.toObj());
            try gc.markObj(self.enum_type.toObj());
        }
    };
};

pub const ObjEnumInstance = struct {
    const Self = @This();

    obj: Obj = .{ .obj_type = .EnumInstance },

    enum_ref: *ObjEnum,
    case: u8,

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markObj(self.enum_ref.toObj());
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
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

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        try gc.markValue(self.receiver);
        if (self.closure) |closure| {
            try gc.markObj(closure.toObj());
        }
        if (self.native) |native| {
            try gc.markObj(native.toObj());
        }
    }

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub inline fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Bound) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

/// Type
pub const ObjTypeDef = struct {
    const Self = @This();

    // TODO: merge this with ObjType
    pub const Type = enum {
        Bool,
        Integer,
        Float,
        String,
        Pattern,
        ObjectInstance,
        Object,
        Protocol,
        ProtocolInstance,
        Enum,
        EnumInstance,
        List,
        Map,
        Function,
        Generic,
        Type, // Something that holds a type, not an actual type
        Void,
        Fiber,
        UserData,

        Placeholder, // Used in first-pass when we refer to a not yet parsed type
    };

    pub const GenericDef = struct {
        // Function def id rather than pointer since we don't get a definitive pointer until the function signature is fully parsed
        origin: usize,
        index: usize, // Index in generic list
    };

    pub const TypeUnion = union(Type) {
        // For those type checking is obvious, the value is a placeholder
        Bool: void,
        Integer: void,
        Float: void,
        String: void,
        Pattern: void,
        Generic: GenericDef,
        Type: void,
        Void: void,
        UserData: void,
        Fiber: ObjFiber.FiberDef,

        // For those we check that the value is an instance of, because those are user defined types
        ObjectInstance: *ObjTypeDef,
        EnumInstance: *ObjTypeDef,
        ProtocolInstance: *ObjTypeDef,

        // Those are never equal
        Object: ObjObject.ObjectDef,
        Enum: ObjEnum.EnumDef,
        Protocol: ObjObject.ProtocolDef,

        // For those we compare definitions, so we own those structs, we don't use actual Obj because we don't want the data, only the types
        List: ObjList.ListDef,
        Map: ObjMap.MapDef,
        Function: ObjFunction.FunctionDef,

        Placeholder: PlaceholderDef,
    };

    obj: Obj = .{ .obj_type = .Type },

    /// True means its an optional (e.g `str?`)
    optional: bool = false,
    def_type: Type,
    /// Used when the type is not a basic type
    resolved_type: ?TypeUnion = null,

    pub fn mark(self: *Self, gc: *GarbageCollector) !void {
        if (self.resolved_type) |*resolved| {
            if (resolved.* == .ObjectInstance) {
                try gc.markObj(resolved.ObjectInstance.toObj());
            } else if (resolved.* == .EnumInstance) {
                try gc.markObj(resolved.EnumInstance.toObj());
            } else if (resolved.* == .Object) {
                try resolved.Object.mark(gc);
            } else if (resolved.* == .Protocol) {
                try resolved.Protocol.mark(gc);
            } else if (resolved.* == .Enum) {
                try resolved.Enum.mark(gc);
            } else if (resolved.* == .Function) {
                try resolved.Function.mark(gc);
            } else if (resolved.* == .List) {
                try resolved.List.mark(gc);
            } else if (resolved.* == .Map) {
                try resolved.Map.mark(gc);
            } else if (resolved.* == .Fiber) {
                try resolved.Fiber.mark(gc);
            } else if (resolved.* == .Placeholder) {
                // unreachable;
            }
        }
    }

    pub fn populateGenerics(
        self: *Self,
        origin: usize,
        generics: []*Self,
        type_registry: *TypeRegistry,
        visited: ?*std.AutoHashMap(*Self, void),
    ) VM.Error!*Self {
        var visited_nodes = if (visited == null) std.AutoHashMap(*Self, void).init(type_registry.gc.allocator) else null;
        defer {
            if (visited == null) {
                visited_nodes.?.deinit();
            }
        }

        var visited_ptr = visited orelse &visited_nodes.?;

        if (visited_ptr.get(self) != null) {
            return self;
        }

        try visited_ptr.put(self, {});

        if (generics.len == 0) {
            return self;
        }

        const result = switch (self.def_type) {
            .Bool,
            .Integer,
            .Float,
            .String,
            .Pattern,
            .Void,
            .UserData,
            .Type,
            .Placeholder,
            .Enum, // Enum are defined in global scope without any generic possible
            .EnumInstance,
            .Protocol,
            .ProtocolInstance,
            => self,

            .Generic => generic: {
                if (self.resolved_type.?.Generic.origin == origin) {
                    break :generic generics[self.resolved_type.?.Generic.index];
                }

                break :generic self;
            },

            .Fiber => fiber: {
                const new_fiber_def = ObjFiber.FiberDef{
                    .return_type = try self.resolved_type.?.Fiber.return_type.populateGenerics(
                        origin,
                        generics,
                        type_registry,
                        visited_ptr,
                    ),
                    .yield_type = try self.resolved_type.?.Fiber.yield_type.populateGenerics(
                        origin,
                        generics,
                        type_registry,
                        visited_ptr,
                    ),
                };

                const resolved = ObjTypeDef.TypeUnion{ .Fiber = new_fiber_def };

                const new_fiber = ObjTypeDef{
                    .def_type = .Fiber,
                    .optional = self.optional,
                    .resolved_type = resolved,
                };

                break :fiber try type_registry.getTypeDef(new_fiber);
            },
            .ObjectInstance => try (try self.resolved_type.?.ObjectInstance.populateGenerics(origin, generics, type_registry, visited_ptr)).toInstance(type_registry.gc.allocator, type_registry),
            .Object => object: {
                // Only anonymous objects can be with generics so no need to check anything other than fields
                const old_object_def = self.resolved_type.?.Object;

                var resolved = ObjObject.ObjectDef.init(
                    type_registry.gc.allocator,
                    old_object_def.name,
                    old_object_def.qualified_name,
                    old_object_def.anonymous,
                );

                {
                    var fields = std.StringArrayHashMap(*ObjTypeDef).init(type_registry.gc.allocator);
                    var it = old_object_def.fields.iterator();
                    while (it.next()) |kv| {
                        try fields.put(
                            kv.key_ptr.*,
                            try kv.value_ptr.*.populateGenerics(
                                origin,
                                generics,
                                type_registry,
                                visited_ptr,
                            ),
                        );
                    }
                    resolved.fields = fields;
                }

                const resolved_type_union = ObjTypeDef.TypeUnion{ .Object = resolved };

                var new_object = ObjTypeDef{
                    .def_type = .Object,
                    .optional = self.optional,
                    .resolved_type = resolved_type_union,
                };

                break :object try type_registry.getTypeDef(new_object);
            },
            .List => list: {
                const old_list_def = self.resolved_type.?.List;

                var methods = std.StringHashMap(*ObjTypeDef).init(type_registry.gc.allocator);
                var it = old_list_def.methods.iterator();
                while (it.next()) |kv| {
                    try methods.put(
                        kv.key_ptr.*,
                        try kv.value_ptr.*.populateGenerics(
                            origin,
                            generics,
                            type_registry,
                            visited_ptr,
                        ),
                    );
                }

                var new_list_def = ObjList.ListDef{
                    .item_type = try (try old_list_def.item_type.populateGenerics(
                        origin,
                        generics,
                        type_registry,
                        visited_ptr,
                    )).toInstance(type_registry.gc.allocator, type_registry),
                    .methods = methods,
                };

                var new_resolved = ObjTypeDef.TypeUnion{ .List = new_list_def };

                const new_list = ObjTypeDef{
                    .def_type = .List,
                    .optional = self.optional,
                    .resolved_type = new_resolved,
                };

                break :list try type_registry.getTypeDef(new_list);
            },
            .Map => map: {
                const old_map_def = self.resolved_type.?.Map;

                var methods = std.StringHashMap(*ObjTypeDef).init(type_registry.gc.allocator);
                var it = old_map_def.methods.iterator();
                while (it.next()) |kv| {
                    try methods.put(
                        kv.key_ptr.*,
                        try kv.value_ptr.*.populateGenerics(
                            origin,
                            generics,
                            type_registry,
                            visited_ptr,
                        ),
                    );
                }

                var new_map_def = ObjMap.MapDef{
                    .key_type = try old_map_def.key_type.populateGenerics(
                        origin,
                        generics,
                        type_registry,
                        visited_ptr,
                    ),
                    .value_type = try old_map_def.value_type.populateGenerics(
                        origin,
                        generics,
                        type_registry,
                        visited_ptr,
                    ),
                    .methods = methods,
                };

                const resolved = ObjTypeDef.TypeUnion{ .Map = new_map_def };

                const new_map = ObjTypeDef{
                    .def_type = .Map,
                    .optional = self.optional,
                    .resolved_type = resolved,
                };

                break :map try type_registry.getTypeDef(new_map);
            },
            .Function => function: {
                const old_fun_def = self.resolved_type.?.Function;

                var error_types: ?std.ArrayList(*ObjTypeDef) = null;
                if (old_fun_def.error_types) |old_error_types| {
                    error_types = std.ArrayList(*ObjTypeDef).init(type_registry.gc.allocator);
                    for (old_error_types) |error_type| {
                        try error_types.?.append(try error_type.populateGenerics(
                            origin,
                            generics,
                            type_registry,
                            visited_ptr,
                        ));
                    }
                }

                var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(type_registry.gc.allocator);
                {
                    var it = old_fun_def.parameters.iterator();
                    while (it.next()) |kv| {
                        try parameters.put(
                            kv.key_ptr.*,
                            try (try kv.value_ptr.*.populateGenerics(
                                origin,
                                generics,
                                type_registry,
                                visited_ptr,
                            )).toInstance(type_registry.gc.allocator, type_registry),
                        );
                    }
                }

                var new_fun_def = ObjFunction.FunctionDef{
                    .id = ObjFunction.FunctionDef.nextId(),
                    .name = old_fun_def.name,
                    .script_name = old_fun_def.script_name,
                    .return_type = try (try old_fun_def.return_type.populateGenerics(
                        origin,
                        generics,
                        type_registry,
                        visited_ptr,
                    )).toInstance(type_registry.gc.allocator, type_registry),
                    .yield_type = try (try old_fun_def.yield_type.populateGenerics(
                        origin,
                        generics,
                        type_registry,
                        visited_ptr,
                    )).toInstance(type_registry.gc.allocator, type_registry),
                    .error_types = if (error_types) |types| types.items else null,
                    .parameters = parameters,
                    .defaults = old_fun_def.defaults,
                    .function_type = old_fun_def.function_type,
                    .lambda = old_fun_def.lambda,
                    .generic_types = old_fun_def.generic_types,
                };

                const resolved = ObjTypeDef.TypeUnion{ .Function = new_fun_def };

                const new_fun_type = ObjTypeDef{
                    .def_type = .Function,
                    .optional = self.optional,
                    .resolved_type = resolved,
                };

                break :function try type_registry.getTypeDef(new_fun_type);
            },
        };

        _ = visited_ptr.remove(self);

        return result;
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
            optional.resolved_type.?.Placeholder.children = std.ArrayList(*ObjTypeDef).init(type_registry.gc.allocator);

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
            non_optional.resolved_type.?.Placeholder.children = std.ArrayList(*ObjTypeDef).init(type_registry.gc.allocator);

            // Make actual link
            try PlaceholderDef.link(self, non_optional, .Unwrap);
        }

        return non_optional;
    }

    pub fn deinit(_: *Self) void {
        // FIXME
    }

    pub fn toStringAlloc(self: *const Self, allocator: Allocator) (Allocator.Error || std.fmt.BufPrintError)!std.ArrayList(u8) {
        var str = std.ArrayList(u8).init(allocator);

        try self.toString(&str.writer());

        return str;
    }

    pub fn toString(self: *const Self, writer: *const std.ArrayList(u8).Writer) (Allocator.Error || std.fmt.BufPrintError)!void {
        try self.toStringRaw(writer, true);
    }

    pub fn toStringUnqualified(self: *const Self, writer: *const std.ArrayList(u8).Writer) (Allocator.Error || std.fmt.BufPrintError)!void {
        try self.toStringRaw(writer, false);
    }

    fn toStringRaw(self: *const Self, writer: *const std.ArrayList(u8).Writer, qualified: bool) (Allocator.Error || std.fmt.BufPrintError)!void {
        switch (self.def_type) {
            .Generic => try writer.print("generic type #{}-{}", .{ self.resolved_type.?.Generic.origin, self.resolved_type.?.Generic.index }),
            .UserData => try writer.writeAll("ud"),
            .Bool => try writer.writeAll("bool"),
            .Integer => try writer.writeAll("int"),
            .Float => try writer.writeAll("float"),
            .String => try writer.writeAll("str"),
            .Pattern => try writer.writeAll("pat"),
            .Fiber => {
                try writer.writeAll("fib<");
                try self.resolved_type.?.Fiber.return_type.toStringRaw(writer, qualified);
                try writer.writeAll(", ");
                try self.resolved_type.?.Fiber.yield_type.toStringRaw(writer, qualified);
                try writer.writeAll(">");
            },

            // TODO: Find a key for vm.getTypeDef which is unique for each class even with the same name
            .Object => {
                const object_def = self.resolved_type.?.Object;

                if (object_def.anonymous) {
                    try writer.writeAll("obj{ ");
                    var it = object_def.fields.iterator();
                    var count = object_def.fields.count();
                    var i: usize = 0;
                    while (it.next()) |kv| {
                        try kv.value_ptr.*.toStringRaw(writer, qualified);
                        try writer.print(" {s}", .{kv.key_ptr.*});

                        if (i < count - 1) {
                            try writer.writeAll(", ");
                        }

                        i += 1;
                    }
                    try writer.writeAll(" }");
                } else {
                    try writer.writeAll("object ");
                    if (qualified) {
                        try writer.writeAll(object_def.qualified_name.string);
                    } else {
                        try writer.writeAll(object_def.name.string);
                    }
                }
            },
            .Protocol => {
                const protocol_def = self.resolved_type.?.Protocol;

                if (qualified) {
                    try writer.print("protocol {s}", .{protocol_def.qualified_name.string});
                } else {
                    try writer.print("protocol {s}", .{protocol_def.name.string});
                }
            },
            .Enum => {
                try writer.writeAll("enum ");
                if (qualified) {
                    try writer.writeAll(self.resolved_type.?.Enum.qualified_name.string);
                } else {
                    try writer.writeAll(self.resolved_type.?.Enum.name.string);
                }
            },

            .ObjectInstance => {
                const object_def = self.resolved_type.?.ObjectInstance.resolved_type.?.Object;

                if (object_def.anonymous) {
                    try writer.writeAll(".{ ");
                    var it = object_def.fields.iterator();
                    var count = object_def.fields.count();
                    var i: usize = 0;
                    while (it.next()) |kv| {
                        try kv.value_ptr.*.toStringRaw(writer, qualified);
                        try writer.print(" {s}", .{kv.key_ptr.*});

                        if (i < count - 1) {
                            try writer.writeAll(", ");
                        }

                        i += 1;
                    }
                    try writer.writeAll(" }");
                } else {
                    if (qualified) {
                        try writer.writeAll(object_def.qualified_name.string);
                    } else {
                        try writer.writeAll(object_def.name.string);
                    }
                }
            },
            .ProtocolInstance => {
                const protocol_def = self.resolved_type.?.ProtocolInstance.resolved_type.?.Protocol;

                if (qualified) {
                    try writer.writeAll(protocol_def.qualified_name.string);
                } else {
                    try writer.writeAll(protocol_def.name.string);
                }
            },
            .EnumInstance => {
                if (qualified) {
                    try writer.writeAll(self.resolved_type.?.EnumInstance.resolved_type.?.Enum.qualified_name.string);
                } else {
                    try writer.writeAll(self.resolved_type.?.EnumInstance.resolved_type.?.Enum.name.string);
                }
            },

            .List => {
                try writer.writeAll("[");
                try self.resolved_type.?.List.item_type.toStringRaw(writer, qualified);
                try writer.writeAll("]");
            },
            .Map => {
                try writer.writeAll("{");
                try self.resolved_type.?.Map.key_type.toStringRaw(writer, qualified);
                try writer.writeAll(", ");
                try self.resolved_type.?.Map.value_type.toStringRaw(writer, qualified);
                try writer.writeAll("}");
            },
            .Function => {
                var function_def = self.resolved_type.?.Function;

                if (function_def.function_type == .Extern) {
                    try writer.writeAll("extern ");
                }
                try writer.writeAll("fun ");
                try writer.writeAll(function_def.name.string);
                try writer.writeAll("(");

                {
                    const size = function_def.generic_types.count();
                    if (size > 0) {
                        try writer.writeAll("<");
                        var i: usize = 0;
                        var it = function_def.generic_types.iterator();
                        while (it.next()) |kv| : (i = i + 1) {
                            try writer.print("{s}", .{kv.key_ptr.*.string});

                            if (i < size - 1) {
                                try writer.writeAll(", ");
                            }
                        }

                        if (function_def.parameters.count() > 0) {
                            try writer.writeAll(">, ");
                        } else {
                            try writer.writeAll(">");
                        }
                    }
                }

                {
                    const size = function_def.parameters.count();
                    var i: usize = 0;
                    var it = function_def.parameters.iterator();
                    while (it.next()) |kv| : (i = i + 1) {
                        try kv.value_ptr.*.toStringRaw(writer, qualified);
                        try writer.writeAll(" ");
                        try writer.writeAll(kv.key_ptr.*.string);

                        if (i < size - 1) {
                            try writer.writeAll(", ");
                        }
                    }
                }

                try writer.writeAll(")");

                if (function_def.yield_type.def_type != .Void) {
                    try writer.writeAll(" > ");
                    try function_def.yield_type.toStringRaw(writer, qualified);
                }

                try writer.writeAll(" > ");
                try function_def.return_type.toStringRaw(writer, qualified);

                if (function_def.error_types != null and function_def.error_types.?.len > 0) {
                    try writer.writeAll(" !> ");
                    for (function_def.error_types.?, 0..) |error_type, index| {
                        try error_type.toStringRaw(writer, qualified);

                        if (index < function_def.error_types.?.len - 1) {
                            try writer.writeAll(", ");
                        }
                    }
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

    pub inline fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub inline fn toValue(self: *Self) Value {
        return Value.fromObj(self.toObj());
    }

    pub fn toParentType(self: *Self, allocator: Allocator, type_registry: *TypeRegistry) !*Self {
        return switch (self.def_type) {
            .ObjectInstance => self.resolved_type.?.ObjectInstance,
            .ProtocolInstance => self.resolved_type.?.ProtocolInstance,
            .EnumInstance => self.resolved_type.?.EnumInstance,
            .Placeholder => placeholder: {
                if (self.resolved_type.?.Placeholder.parent_relation != null and self.resolved_type.?.Placeholder.parent_relation.? == .Instance) {
                    return self.resolved_type.?.Placeholder.parent.?;
                }

                var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                    .Placeholder = PlaceholderDef.init(
                        allocator,
                        self.resolved_type.?.Placeholder.where.clone(),
                    ),
                };
                placeholder_resolved_type.Placeholder.name = self.resolved_type.?.Placeholder.name;

                const placeholder = try type_registry.getTypeDef(Self{
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type,
                });

                try PlaceholderDef.link(self, placeholder, .Parent);

                break :placeholder placeholder;
            },
            else => self,
        };
    }

    pub fn toInstance(self: *Self, allocator: Allocator, type_registry: *TypeRegistry) !*Self {
        // Avoid placeholder double links like: Object Placeholder -> Instance -> Instance
        if (self.def_type == .Placeholder and self.resolved_type.?.Placeholder.parent_relation != null and self.resolved_type.?.Placeholder.parent_relation.? == .Instance) {
            return self;
        }

        var instance_type = try type_registry.getTypeDef(
            switch (self.def_type) {
                .Object => object: {
                    var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
                        .ObjectInstance = try self.cloneNonOptional(type_registry),
                    };

                    break :object Self{
                        .optional = self.optional,
                        .def_type = .ObjectInstance,
                        .resolved_type = resolved_type,
                    };
                },
                .Protocol => protocol: {
                    var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
                        .ProtocolInstance = try self.cloneNonOptional(type_registry),
                    };

                    break :protocol Self{
                        .optional = self.optional,
                        .def_type = .ProtocolInstance,
                        .resolved_type = resolved_type,
                    };
                },
                .Enum => enum_instance: {
                    var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
                        .EnumInstance = try self.cloneNonOptional(type_registry),
                    };

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

    pub inline fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Type) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    // Compare two type definitions
    pub fn eqlTypeUnion(expected: TypeUnion, actual: TypeUnion) bool {
        if (@as(Type, expected) != @as(Type, actual) and (expected != .ProtocolInstance or actual != .ObjectInstance)) {
            return false;
        }

        return switch (expected) {
            .Bool,
            .Integer,
            .Float,
            .String,
            .Void,
            .Pattern,
            .UserData,
            .Type,
            => return true,

            .Generic => expected.Generic.origin == actual.Generic.origin and expected.Generic.index == actual.Generic.index,

            .Fiber => {
                return expected.Fiber.return_type.eql(actual.Fiber.return_type) and expected.Fiber.yield_type.eql(actual.Fiber.yield_type);
            },

            .ObjectInstance => {
                return expected.ObjectInstance.eql(actual.ObjectInstance) or expected.ObjectInstance == actual.ObjectInstance;
            },
            .ProtocolInstance => {
                if (actual == .ProtocolInstance) {
                    return expected.ProtocolInstance.eql(actual.ProtocolInstance) or expected.ProtocolInstance == actual.ProtocolInstance;
                } else {
                    assert(actual == .ObjectInstance);
                    return actual.ObjectInstance.resolved_type.?.Object.conforms_to.get(expected.ProtocolInstance) != null;
                }
            },
            .EnumInstance => return expected.EnumInstance.eql(actual.EnumInstance),

            .Object, .Protocol, .Enum => false, // Those are never equal even if definition is the same

            .List => return expected.List.item_type.eql(actual.List.item_type),
            .Map => return expected.Map.key_type.eql(actual.Map.key_type) and expected.Map.value_type.eql(actual.Map.value_type),
            .Function => {
                // Compare return type
                if (!expected.Function.return_type.eql(actual.Function.return_type)) {
                    return false;
                }

                // Compare yield type
                if (!expected.Function.yield_type.eql(actual.Function.yield_type)) {
                    return false;
                }

                // Compare arity
                if (expected.Function.parameters.count() != actual.Function.parameters.count()) {
                    return false;
                }

                // Compare parameters (we ignore argument names and only compare types)
                const a_keys: []*ObjString = expected.Function.parameters.keys();
                const b_keys: []*ObjString = actual.Function.parameters.keys();

                if (a_keys.len != b_keys.len) {
                    return false;
                }

                for (a_keys, 0..) |_, index| {
                    if (!expected.Function.parameters.get(a_keys[index]).?
                        .eql(actual.Function.parameters.get(b_keys[index]).?))
                    {
                        return false;
                    }
                }

                return true;
            },

            .Placeholder => true, // TODO: should it be false?
        };
    }

    // Compare two type definitions
    pub fn eql(expected: *Self, actual: *Self) bool {
        // zig fmt: off
        const type_eql: bool = (expected.resolved_type == null and actual.resolved_type == null and expected.def_type == actual.def_type)
            or (expected.resolved_type != null and actual.resolved_type != null and eqlTypeUnion(expected.resolved_type.?, actual.resolved_type.?));

        // TODO: in an ideal world comparing pointers should be enough, but typedef can come from different type_registries and we can't reconcile them like we can with strings
        // FIXME: previous comment should be wrong now? we do share type_registries between fibers and this should be enough ?
        return expected == actual
            or (expected.optional and actual.def_type == .Void) // Void is equal to any optional type
            or (
                (type_eql or actual.def_type == .Placeholder or expected.def_type == .Placeholder)
                and (expected.optional or !actual.optional)
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
        => return Value.fromObj(obj),

        .List => {
            const list = ObjList.cast(obj).?;

            return (try vm.gc.allocateObject(
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

            return (try vm.gc.allocateObject(
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

pub fn objToString(writer: *const std.ArrayList(u8).Writer, obj: *Obj) (Allocator.Error || std.fmt.BufPrintError)!void {
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
        .ObjectInstance => {
            const instance = ObjObjectInstance.cast(obj).?;

            if (instance.object) |object| {
                try writer.print("object instance: 0x{x} `{s}`", .{
                    @ptrToInt(instance),
                    object.name.string,
                });
            } else {
                try writer.print("object instance: 0x{x} obj{{ ", .{
                    @ptrToInt(instance),
                });
                var it = instance.fields.iterator();
                while (it.next()) |kv| {
                    // This line is awesome
                    try instance.type_def.?.resolved_type.?.ObjectInstance.resolved_type.?.Object.fields.get(kv.key_ptr.*.string).?.toString(writer);
                    try writer.print(" {s}, ", .{kv.key_ptr.*.string});
                }
                try writer.writeAll("}");
            }
        },
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
    pub const PlaceholderRelation = enum {
        Call,
        Yield,
        Subscript,
        Key,
        FieldAccess,
        Assignment,
        Instance,
        Parent,
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

    // If the placeholder is a function return, we need to remember eventual generic types defined in that call
    call_generics: ?[]*ObjTypeDef = null,

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
            if (BuildOptions.debug_placeholders) {
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

        if (BuildOptions.debug_placeholders) {
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
