// zig fmt: off
const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;
const StringHashMap = std.StringHashMap;
const Chunk = @import("./chunk.zig").Chunk;
const VM = @import("./vm.zig").VM;
const memory = @import("./memory.zig");
const _value = @import("./value.zig");
const Value = _value.Value;
const HashableValue = _value.HashableValue;
const Token = @import("./token.zig").Token;

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
    // Native,
};

pub fn allocateObject(vm: *VM, obj_type: ObjType) !*Obj {
    var size: usize = 0;

    var object: *Obj = switch (obj_type) {
        .String => string: {
            size = @sizeOf(*ObjString);
            var obj: *ObjString = try memory.allocate(vm, ObjString);
            obj.obj = .{
                .obj_type = .String,
            };

            break :string &obj.obj;
        },
        .Type => typeObj: {
            size = @sizeOf(*ObjTypeDef);
            var obj: *ObjTypeDef = try memory.allocate(vm, ObjTypeDef);
            obj.obj = .{
                .obj_type = .Type,
            };

            break :typeObj &obj.obj;
        },
        .UpValue => upValue: {
            size = @sizeOf(*ObjUpValue);
            var obj: *ObjUpValue = try memory.allocate(vm, ObjUpValue);
            obj.obj = .{
                .obj_type = .UpValue,
            };

            break :upValue &obj.obj;
        },
        .Closure => closure: {
            size = @sizeOf(*ObjClosure);
            var obj: *ObjClosure = try memory.allocate(vm, ObjClosure);
            obj.obj = .{
                .obj_type = .Closure,
            };

            break :closure &obj.obj;
        },
        .Function => function: {
            size = @sizeOf(*ObjFunction);
            var obj: *ObjFunction = try memory.allocate(vm, ObjFunction);
            obj.obj = .{
                .obj_type = .Function,
            };

            break :function &obj.obj;
        },
        .ObjectInstance => objectInstance: {
            size = @sizeOf(*ObjObjectInstance);
            var obj: *ObjObjectInstance = try memory.allocate(vm, ObjObjectInstance);
            obj.obj = .{
                .obj_type = .ObjectInstance,
            };

            break :objectInstance &obj.obj;
        },
        .Object => object: {
            size = @sizeOf(*ObjObject);
            var obj: *ObjObject = try memory.allocate(vm, ObjObject);
            obj.obj = .{
                .obj_type = .Object,
            };

            break :object &obj.obj;
        },
        .List => list: {
            size = @sizeOf(*ObjList);
            var obj: *ObjList = try memory.allocate(vm, ObjList);
            obj.obj = .{
                .obj_type = .List,
            };

            break :list &obj.obj;
        },
        .Map => map: {
            size = @sizeOf(*ObjMap);
            var obj: *ObjMap = try memory.allocate(vm, ObjMap);
            obj.obj = .{
                .obj_type = .Map,
            };

            break :map &obj.obj;
        },
        .Enum => enumObj: {
            size = @sizeOf(*ObjEnum);
            var obj: *ObjEnum = try memory.allocate(vm, ObjEnum);
            obj.obj = .{
                .obj_type = .Enum,
            };

            break :enumObj &obj.obj;
        },
        .EnumInstance => enumInstance: {
            size = @sizeOf(*ObjEnumInstance);
            var obj: *ObjEnumInstance = try memory.allocate(vm, ObjEnumInstance);
            obj.obj = .{
                .obj_type = .EnumInstance,
            };

            break :enumInstance &obj.obj;
        },
        .Bound => bound: {
            size = @sizeOf(*ObjBoundMethod);
            var obj: *ObjBoundMethod = try memory.allocate(vm, ObjBoundMethod);
            obj.obj.obj_type = .Bound;

            break :bound &obj.obj;
        },
        // .Native => native: {
        //     size = @sizeOf(*ObjNative);
        //     var obj: *ObjNative = try memory.allocate(vm, ObjNative);
        //     obj.obj.obj_type = .Native;

        //     break :native &obj.obj;
        // },
    };

    // Add new object at start of vm.objects linked list
    object.next = vm.objects;
    vm.objects = object;

    vm.bytes_allocated += size;

    if (vm.bytes_allocated > vm.next_gc) {
        memory.collectGarbage(vm);
    }

    return object;
}

pub fn allocateString(vm: *VM, chars: []const u8) !*ObjString {
    if (vm.strings.get(chars)) |interned| {
        return interned;
    } else {
        var string: *ObjString = ObjString.cast(try allocateObject(vm, .String)).?;
        string.string = chars;

        vm.push(Value { .Obj = string.toObj() });
        try vm.strings.put(chars, string);
        _ = vm.pop();

        return string;
    }
}

pub fn copyString(vm: *VM, chars: []const u8) !*ObjString {
    if (vm.strings.get(chars)) |interned| {
        return interned;
    }

    var copy: []u8 = try memory.allocateMany(vm, u8, chars.len);
    mem.copy(u8, copy, chars);

    return try allocateString(vm, copy);
}

pub const Obj = struct {
    const Self = @This();

    obj_type: ObjType,
    is_marked: bool = false,
    next: ?*Obj = null,
};

/// A String
pub const ObjString = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .String
    },

    /// The actual string
    string: []const u8,

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .String) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

/// Upvalue
pub const ObjUpValue = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .UpValue
    },

    /// Slot on the stack
    location: *Value,
    closed: ?Value,
    next: ?*ObjUpValue = null,

    pub fn init(slot: *Value) Self {
        return Self {
            .closed = null,
            .location = slot,
            .next = null
        };
    } 

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
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

    obj: Obj = .{
        .obj_type = .Closure
    },

    function: *ObjFunction,
    upvalues: std.ArrayList(*ObjUpValue),

    pub fn init(allocator: *Allocator, function: *ObjFunction) !Self {
        return Self {
            .function = function,
            .upvalues = try std.ArrayList(*ObjUpValue).initCapacity(allocator, function.upvalue_count),
        };
    }

    pub fn deinit(self: *Self) void {
        self.upvalues.deinit();
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
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

    obj: Obj = .{
        .obj_type = .Function
    },

    name: *ObjString,
    parameters: std.StringArrayHashMap(*ObjTypeDef),
    return_type: *ObjTypeDef,
    chunk: Chunk,
    upvalue_count: u8 = 0,

    pub fn init(allocator: *Allocator, name: *ObjString, return_type: *ObjTypeDef) !Self {
        return Self {
            .name = name,
            .return_type = return_type,
            .parameters = std.StringArrayHashMap(*ObjTypeDef).init(allocator),
            .chunk = try Chunk.init(allocator),
        };
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Function) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

/// Object instance
pub const ObjObjectInstance = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .ObjectInstance
    },

    /// Object
    object: *ObjObject,
    /// Fields value
    fields: StringHashMap(Value),

    pub fn init(allocator: *Allocator, object: *ObjObject) Self {
        return Self {
            .object = object,
            .fields = StringHashMap(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.fields.deinit();
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .ObjectInstance) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

/// Object
pub const ObjObject = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .Object
    },

    /// Used to allow type checking at runtime ObjTypeDef(Object)
    object_def: *ObjTypeDef,

    /// Object name
    name: *ObjString,
    /// Object methods
    methods: StringHashMap(*ObjClosure),
    /// Object fields definition
    fields: StringHashMap(*ObjTypeDef),
    /// Optional super class
    super: ?*ObjObject = null,
    /// If false, can't be inherited from
    inheritable: bool = false,

    pub fn init(allocator: *Allocator, def: *ObjTypeDef) Self {
        return Self {
            .object_def = def,
            .name = def.resolved_type.?.Object.name,
            .methods = StringHashMap(*ObjClosure).init(allocator),
            .fields = StringHashMap(*ObjTypeDef).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.methods.deinit();
        self.fields.deinit();
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Object) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

/// List
pub const ObjList = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .List
    },

    /// List items
    items: std.ArrayList(Value),
    /// Allowed type in this list
    item_type: *ObjTypeDef,

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .List) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

/// Map
pub const ObjMap = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .Map
    },

    key_type: *ObjTypeDef,
    value_type: *ObjTypeDef,
    // TODO: key can't be a Value: a *ObjString should not be a key, the []u8 inside should be the key
    map: std.AutoArrayHashMap(HashableValue, Value),

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Map) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

/// Enum
pub const ObjEnum = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .Enum
    },

    /// Used to allow type checking at runtime
    object_def: *ObjTypeDef,

    name: *ObjString,
    enum_type: *ObjTypeDef,
    // Maybe a waste to have 255, but we don't define many enum and they are long lived
    cases: [255]Value,
    names: [255][]const u8,

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Enum) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

pub const ObjEnumInstance = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .EnumInstance
    },

    enum_ref: *ObjEnum,
    case: u8,

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .EnumInstance) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

/// Bound
pub const ObjBoundMethod = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .Bound
    },

    receiver: Value,
    closure: *ObjClosure,

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Bound) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

/// Type
pub const ObjTypeDef = struct {
    const Self = @This();

    pub const Type = enum {
        Bool,
        Number,
        Byte,
        String,
        ObjectInstance,
        Object,
        Enum,
        EnumInstance,
        List,
        Map,
        Function,
        Type, // Something that holds a type, not an actual type
        Void,

        Placeholder, // Used in first-pass when we refer to a not yet parsed type
    };

    pub const ListDef = struct {
        item_type: *ObjTypeDef,
    };

    pub const MapDef = struct {
        key_type: *ObjTypeDef,
        value_type: *ObjTypeDef,
    };

    pub const FunctionDef = struct {
        name: *ObjString,
        return_type: *ObjTypeDef,
        parameters: std.StringArrayHashMap(*ObjTypeDef),
    };

    pub const ObjectDef = struct {
        const ObjectDefSelf = @This();

        name: *ObjString,
        // TODO: Do i need to have two maps ?
        fields: StringHashMap(*ObjTypeDef),
        methods: StringHashMap(*ObjTypeDef),
        super: ?*ObjTypeDef = null,
        inheritable: bool = false,
        

        pub fn init(allocator: *Allocator, name: *ObjString) ObjectDefSelf {
            return ObjectDefSelf {
                .name = name,
                .fields = StringHashMap(*ObjTypeDef).init(allocator),
                .methods = StringHashMap(*ObjTypeDef).init(allocator),
            };
        }

        pub fn deinit(self: *ObjectDefSelf) void {
            self.fields.deinit();
            self.methods.deinit();
        }
    };


    pub const EnumDef = struct {
        name: *ObjString,
        enum_type: *ObjTypeDef,
        cases: [255][]const u8,
    };

    pub const PlaceholderDef = struct {
        const PlaceholderSelf = @This();

        // TODO: are relations enough and booleans useless?
        const PlaceholderRelation = enum {
            Call,
            Subscript,
            FieldAccess,
            Assignment,
        };

        name: ?*ObjString = null,

        // Assumption made by the code referencing the value
        callable: ?bool = null,             // Function, Object or Class
        subscriptable: ?bool = null,        // Array or Map
        field_accessible: ?bool = null,     // Object, Class or Enum
        assignable: ?bool = null,           // Not a Function, Object, Class or Enum
        resolved_parameters: ?std.StringArrayHashMap(*ObjTypeDef) = null, // Maybe we resolved argument list but we don't know yet if Object/Class or Function
        resolved_def_type: ?Type = null,    // Meta type
        resolved_type: ?*ObjTypeDef = null, // Actual type
        where: Token,                       // Where the placeholder was created

        // When accessing/calling/subscrit/assign a placeholder we produce another. We keep them linked so we
        // can trace back the root of the unknown type.
        parent: ?*ObjTypeDef = null,
        // What's the relation with the parent?
        parent_relation: ?PlaceholderRelation = null,
        // Children adds themselves here
        children: std.ArrayList(*ObjTypeDef),

        pub fn init(allocator: *Allocator, where: Token) PlaceholderSelf {
            return PlaceholderSelf {
                .where = where.clone(),
                .children = std.ArrayList(*ObjTypeDef).init(allocator)
            };
        }

        pub fn deinit(self: *PlaceholderSelf) void {
            self.children.deinit();
        }

        pub fn link(parent: *ObjTypeDef, child: *ObjTypeDef, relation: PlaceholderRelation) !void {
            assert(parent.def_type == .Placeholder);
            assert(child.def_type == .Placeholder);

            child.resolved_type.?.Placeholder.parent = parent;
            try parent.resolved_type.?.Placeholder.children.append(child);
            child.resolved_type.?.Placeholder.parent_relation = relation;
        }

        pub fn eql(a: PlaceholderSelf, b: PlaceholderSelf) bool {
            if (a.resolved_parameters != null and b.resolved_parameters != null) {
                var it = a.resolved_parameters.?.iterator();
                while (it.next()) |kv| {
                    if (b.resolved_parameters.?.get(kv.key_ptr.*)) |b_arg_type| {
                        return b_arg_type.eql(kv.value_ptr.*);
                    } else {
                        return false;
                    }
                }
            }

            return ((a.callable != null and b.callable != null and a.callable.? == b.callable.?) or a.callable == null or b.callable == null)
                and ((a.subscriptable != null and b.subscriptable != null and a.subscriptable.? == b.subscriptable.?) or a.subscriptable == null or b.subscriptable == null)
                and ((a.field_accessible != null and b.field_accessible != null and a.field_accessible.? == b.field_accessible.?) or a.field_accessible == null or b.subscriptable == null)
                and ((a.assignable != null and b.assignable != null and a.assignable.? == b.assignable.?) or a.assignable == null or b.subscriptable == null)
                and ((a.resolved_def_type != null and b.resolved_def_type != null and a.resolved_def_type.? == b.resolved_def_type.?) or a.resolved_def_type == null or b.resolved_def_type == null)
                and ((a.resolved_type != null and b.resolved_type != null and a.resolved_type.?.eql(b.resolved_type.?)) or a.resolved_type == null or b.subscriptable == null);
        }

        pub fn enrich(one: *PlaceholderSelf, other: *PlaceholderSelf) !void {
            one.callable = one.callable orelse other.callable;
            other.callable = one.callable orelse other.callable;

            one.subscriptable = one.subscriptable orelse other.subscriptable;
            other.subscriptable = one.subscriptable orelse other.subscriptable;

            one.field_accessible = one.field_accessible orelse other.field_accessible;
            other.field_accessible = one.field_accessible orelse other.field_accessible;

            one.assignable = one.assignable orelse other.assignable;
            other.assignable = one.assignable orelse other.assignable;

            one.resolved_def_type = one.resolved_def_type orelse other.resolved_def_type;
            other.resolved_def_type = one.resolved_def_type orelse other.resolved_def_type;

            one.resolved_type = one.resolved_type orelse other.resolved_type;
            other.resolved_type = one.resolved_type orelse other.resolved_type;

            if (other.resolved_parameters) |parameters| {
                one.resolved_parameters = try parameters.clone();
            } else if (one.resolved_parameters) |parameters| {
                other.resolved_parameters = try parameters.clone();
            }
        }

        pub fn isAssignable(self: *PlaceholderSelf) bool {
            return (self.assignable == null or self.assignable.?)
                and (self.resolved_def_type == null
                    // TODO: method actually but right now we have no way to distinguish them
                    or self.resolved_def_type.? != .Function
                    or self.resolved_def_type.? != .Object)
                and (self.resolved_type == null
                    // TODO: method actually but right now we have no way to distinguish them
                    or self.resolved_type.?.def_type != .Function
                    or self.resolved_type.?.def_type != .Object);
        }

        pub fn isCallable(self: *PlaceholderSelf) bool {
            return (self.callable == null or self.callable.?)
                and (self.resolved_def_type == null
                    or self.resolved_def_type.? == .Function
                    or self.resolved_def_type.? == .Object)
                and (self.resolved_type == null
                    or self.resolved_type.?.def_type == .Function
                    or self.resolved_type.?.def_type == .Object);
        }

        pub fn isFieldAccessible(self: *PlaceholderSelf) bool {
            return (self.field_accessible == null or self.field_accessible.?)
                and (self.resolved_def_type == null
                    or self.resolved_def_type.? == .Enum
                    or self.resolved_def_type.? == .ObjectInstance)
                and (self.resolved_type == null
                    or self.resolved_type.?.def_type == .Enum
                    or self.resolved_type.?.def_type == .ObjectInstance);
        }

        pub fn isSubscriptable(self: *PlaceholderSelf) bool {
            return (self.subscriptable == null or self.subscriptable.?)
                and (self.resolved_def_type == null
                    or self.resolved_def_type.? == .List
                    or self.resolved_def_type.? == .Map)
                and (self.resolved_type == null
                    or self.resolved_type.?.def_type == .List
                    or self.resolved_type.?.def_type == .Map);
        }

        pub fn isCoherent(self: *PlaceholderSelf) bool {
            if (self.resolved_def_type != null
                and self.resolved_type != null
                and @as(Type, self.resolved_type.?.def_type) != self.resolved_def_type.?) {
                return false;
            }

            // Nothing can be called and subscrited
            if ((self.callable orelse false) and (self.subscriptable orelse false)) {
                return false;
            }

            // Nothing with fields can be subscrited
            if ((self.field_accessible orelse false) and (self.subscriptable orelse false)) {
                return false;
            }

            // `and` because we checked for compatibility earlier and those function will return true if the flag is null
            return self.isCallable() and self.isSubscriptable() and self.isFieldAccessible() and self.isAssignable();
        }
    };

    pub const TypeUnion = union(Type) {
        // For those type checking is obvious, the value is a placeholder
        Bool: bool,
        Number: bool,
        Byte: bool,
        String: bool,
        Type: bool,
        Void: bool,

        // For those we check that the value is an instance of, because those are user defined types
        ObjectInstance: *ObjTypeDef,
        EnumInstance: *ObjTypeDef,

        // Those are never equal
        Object: ObjectDef,
        Enum: EnumDef,

        // For those we compare definitions, so we own those structs, we don't use actual Obj because we don't want the data, only the types
        List: ListDef,
        Map: MapDef,
        Function: FunctionDef,

        Placeholder: PlaceholderDef
    };

    obj: Obj = .{
        .obj_type = .Type
    },

    /// True means its an optional (e.g `str?`)
    optional: bool,
    def_type: Type,
    /// Used when the type is not a basic type
    resolved_type: ?TypeUnion = null,

    /// Beware: allocates a string, caller owns it
    pub fn toString(self: Self, allocator: *Allocator) anyerror![]const u8 {
        var type_str: std.ArrayList(u8) = std.ArrayList(u8).init(allocator);

        if (self.optional) {
            try type_str.append('?');
        }

        switch (self.def_type) {
            .Bool => try type_str.appendSlice("bool"),
            .Number => try type_str.appendSlice("num"),
            .Byte => try type_str.appendSlice("byte"),
            .String => try type_str.appendSlice("str"),

            // TODO: Find a key for vm.getTypeDef which is unique for each class even with the same name
            .Object => {
                try type_str.appendSlice("{ObjectDef}");
                try type_str.appendSlice(self.resolved_type.?.Object.name.string);
            },
            .Enum => {
                try type_str.appendSlice("{EnumDef}");
                try type_str.appendSlice(self.resolved_type.?.Enum.name.string);
            },
        
            .ObjectInstance => try type_str.appendSlice(self.resolved_type.?.ObjectInstance.resolved_type.?.Object.name.string),
            .EnumInstance => try type_str.appendSlice(self.resolved_type.?.EnumInstance.resolved_type.?.Enum.name.string),

            .List => {
                var list_type = try self.resolved_type.?.List.item_type.toString(allocator);
                defer allocator.free(list_type);

                try type_str.append('[');
                try type_str.appendSlice(list_type);
                try type_str.append(']');
            },
            .Map => {
                var key_type = try self.resolved_type.?.Map.key_type.toString(allocator);
                defer allocator.free(key_type);
                var value_type = try self.resolved_type.?.Map.value_type.toString(allocator);
                defer allocator.free(value_type);
                
                try type_str.append('{');
                try type_str.appendSlice(key_type);
                try type_str.append(',');
                try type_str.appendSlice(value_type);
                try type_str.append('}');
            },
            .Function => {
                var function_def = self.resolved_type.?.Function;

                try type_str.appendSlice("Function");
                try type_str.appendSlice(function_def.name.string);
                try type_str.appendSlice("(");

                var it = function_def.parameters.iterator();
                while (it.next()) |kv| {
                    var param_type = try kv.value_ptr.*.toString(allocator);
                    defer allocator.free(param_type);

                    try type_str.appendSlice(param_type);
                    try type_str.append(',');
                }

                try type_str.append(')');

                if (function_def.return_type.def_type != Type.Void) {
                    var return_type = try self.resolved_type.?.Function.return_type.toString(allocator);
                    defer allocator.free(return_type);

                    try type_str.appendSlice(") > ");
                    try type_str.appendSlice(return_type);
                }
            },
            .Type => try type_str.appendSlice("type"),
            .Void => try type_str.appendSlice("void"),

            .Placeholder => {
                try type_str.appendSlice("{PlaceholderDef}");
            }
        }

        return type_str.items;
    }

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Type) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }

    // Compare two type definitions
    pub fn eqlTypeUnion(a: TypeUnion, b: TypeUnion) bool {
        if (@as(Type, a) != @as(Type, b)) {
            return false;
        }

        return switch (a) {
            .Bool, .Number, .Byte, .String, .Type, .Void => return true,

            .ObjectInstance => return a.ObjectInstance == b.ObjectInstance,
            .EnumInstance => return a.EnumInstance == b.EnumInstance,

            .Object,
            .Enum => false, // Thore are never equal even if definition is the same

            .List => return a.List.item_type.eql(b.List.item_type),
            .Map => return a.Map.key_type.eql(b.Map.key_type)
                and a.Map.value_type.eql(b.Map.value_type),
            .Function => {
                // Compare return types
                if (a.Function.return_type.eql(b.Function.return_type)) {
                    return false;
                }

                // Compare arity
                if (a.Function.parameters.count() != b.Function.parameters.count()) {
                    return false;
                }

                // Compare parameters
                var it = a.Function.parameters.iterator();
                while (it.next()) |kv| {
                    if (b.Function.parameters.get(kv.key_ptr.*)) |value| {
                        if (!kv.value_ptr.*.eql(value)) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }

                return true;
            },

            .Placeholder => a.Placeholder.eql(b.Placeholder),
        };
    }

    // Compare two type definitions
    pub fn eql(self: *Self, other: *Self) bool {
        const type_eql: bool = self.def_type == other.def_type
            and ((self.resolved_type == null and other.resolved_type == null)
                or eqlTypeUnion(self.resolved_type.?, other.resolved_type.?));

        return self == other
            or (self.optional and other.def_type == .Void) // Void is equal to any optional type
            or (self.optional == other.optional
                and (type_eql or other.def_type == .Placeholder or self.def_type == .Placeholder));
    }
};

pub fn objToString(allocator: *Allocator, buf: []u8, obj: *Obj) anyerror![]u8 {
    return switch (obj.obj_type) {
        .String => try std.fmt.bufPrint(buf, "{s}", .{ ObjString.cast(obj).?.string }),
        .Type => {
            // TODO: no use for typedef.toString to allocate a buffer
            var type_def: *ObjTypeDef = ObjTypeDef.cast(obj).?; 
            var type_str: []const u8 = try type_def.toString(allocator);
            defer allocator.free(type_str);

            return try std.fmt.bufPrint(buf, "type: 0x{x} `{s}`", .{ @ptrToInt(type_def), type_str });
        },
        .UpValue => {
            var upvalue: *ObjUpValue = ObjUpValue.cast(obj).?;
            var upvalue_str: []const u8 = try _value.valueToString(allocator, upvalue.closed orelse upvalue.location.*);
            defer allocator.free(upvalue_str);

            return try std.fmt.bufPrint(buf, "upvalue: 0x{x} `{s}`", .{ @ptrToInt(upvalue), upvalue_str });
        },
        .Closure => try std.fmt.bufPrint(buf, "closure: 0x{x} `{s}`", .{ @ptrToInt(ObjClosure.cast(obj).?), ObjClosure.cast(obj).?.function.name.string }),
        .Function => try std.fmt.bufPrint(buf, "function: 0x{x} `{s}`", .{ @ptrToInt(ObjFunction.cast(obj).?), ObjFunction.cast(obj).?.name.string }),
        .ObjectInstance => try std.fmt.bufPrint(buf, "object instance: 0x{x} `{s}`", .{ @ptrToInt(ObjObjectInstance.cast(obj).?), ObjObjectInstance.cast(obj).?.object.name.string }),
        .Object => try std.fmt.bufPrint(buf, "object: 0x{x} `{s}`", .{ @ptrToInt(ObjObject.cast(obj).?), ObjObject.cast(obj).?.name.string }),
        .List => {
            var list: *ObjList = ObjList.cast(obj).?;
            var type_str: []const u8 = try list.item_type.toString(allocator);
            defer allocator.free(type_str);

            return try std.fmt.bufPrint(buf, "array: 0x{x} [{s}]", .{ @ptrToInt(list), type_str });
        },
        .Map => {
            var map: *ObjMap = ObjMap.cast(obj).?;
            var key_type_str: []const u8 = try map.key_type.toString(allocator);
            defer allocator.free(key_type_str);
            var value_type_str: []const u8 = try map.value_type.toString(allocator);
            defer allocator.free(value_type_str);

            return try std.fmt.bufPrint(buf, "map: 0x{x} <{s}, {s}>", .{ @ptrToInt(map), key_type_str, value_type_str });
        },
        .Enum => try std.fmt.bufPrint(buf, "enum: 0x{x} `{s}`", .{ @ptrToInt(ObjEnum.cast(obj).?), ObjEnum.cast(obj).?.name.string }),
        .EnumInstance => enum_instance: {
            var instance: *ObjEnumInstance = ObjEnumInstance.cast(obj).?;

            break :enum_instance try std.fmt.bufPrint(buf, "{s}.{s}", .{ instance.enum_ref.name.string, instance.enum_ref.names[instance.case] });
        },
        .Bound => {
            var bound: *ObjBoundMethod = ObjBoundMethod.cast(obj).?;
            var receiver_str: []const u8 = try _value.valueToString(allocator, bound.receiver);
            defer allocator.free(receiver_str);

            var closure_name: []const u8 =  bound.closure.function.name.string;

            return try std.fmt.bufPrint(buf, "bound method: {s} to {s}", .{ receiver_str, closure_name });
        },
    };
}
