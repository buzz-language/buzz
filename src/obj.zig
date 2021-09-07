// zig fmt: off
const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const StringHashMap = std.StringHashMap;
const Chunk = @import("./chunk.zig").Chunk;
const VM = @import("./vm.zig").VM;
const memory = @import("./memory.zig");
const _value = @import("./value.zig");
const Value = _value.Value;
const HashableValue = _value.HashableValue;

pub const ObjType = enum {
    String,
    Type,
    UpValue,
    Closure,
    Function,
    ClassInstance,
    ObjectInstance,
    Class,
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
        .ClassInstance => classInstance: {
            size = @sizeOf(*ObjClassInstance);
            var obj: *ObjClassInstance = try memory.allocate(vm, ObjClassInstance);
            obj.obj = .{
                .obj_type = .ClassInstance,
            };

            break :classInstance &obj.obj;
        },
        .ObjectInstance => objectInstance: {
            size = @sizeOf(*ObjObjectInstance);
            var obj: *ObjObjectInstance = try memory.allocate(vm, ObjObjectInstance);
            obj.obj = .{
                .obj_type = .ObjectInstance,
            };

            break :objectInstance &obj.obj;
        },
        .Class => class: {
            size = @sizeOf(*ObjClass);
            var obj: *ObjClass = try memory.allocate(vm, ObjClass);
            obj.obj = .{
                .obj_type = .Class,
            };

            break :class &obj.obj;
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
    parameters: StringHashMap(*ObjTypeDef),
    return_type: *ObjTypeDef,
    chunk: Chunk,
    upvalue_count: u8 = 0,

    pub fn init(allocator: *Allocator, name: *ObjString, return_type: *ObjTypeDef) !Self {
        return Self {
            .name = name,
            .return_type = return_type,
            .parameters = StringHashMap(*ObjTypeDef).init(allocator),
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

/// Class instance
pub const ObjClassInstance = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .ClassInstance
    },

    /// Class
    class: *ObjClass,
    /// Fields value
    fields: StringHashMap(Value),

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .ClassInstance) {
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

/// Class
pub const ObjClass = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .Class
    },

    /// Used to allow type checking at runtime
    class_def: *ObjTypeDef,
    
    /// Class name
    name: *ObjString,
    /// Class methods
    methods: StringHashMap(*ObjClosure),
    /// Class fields definition
    fields: StringHashMap(*ObjTypeDef),
    /// Optional super class
    super: ?*ObjClass = null,

    pub fn init(allocator: *Allocator, name: *ObjString) Self {
        return Self {
            .name = name,
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
        if (obj.obj_type != .Class) {
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
        ClassInstance,
        Class,
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

    pub const ClassDef = struct {
        name: *ObjString,
        fields: StringHashMap(*ObjTypeDef),
        methods: StringHashMap(*ObjTypeDef),
        super: *ObjTypeDef,
    };

    pub const EnumDef = struct {
        name: *ObjString,
        enum_type: *ObjTypeDef,
        cases: [255][]const u8,
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
        ClassInstance: *ObjTypeDef,
        ObjectInstance: *ObjTypeDef,
        EnumInstance: *ObjTypeDef,

        // Those are never equal
        Class: ClassDef,
        Object: ObjectDef,
        Enum: EnumDef,

        // For those we compare definitions, so we own those structs, we don't use actual Obj because we don't want the data, only the types
        List: ListDef,
        Map: MapDef,
        Function: FunctionDef,

        Placeholder: *ObjString // We know the type name but not its definition
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
            .Class => {
                try type_str.appendSlice("{ClassDef}");
                try type_str.appendSlice(self.resolved_type.?.Class.name.string);
            },
            .Object => {
                try type_str.appendSlice("{ObjectDef}");
                try type_str.appendSlice(self.resolved_type.?.Object.name.string);
            },
            .Enum => {
                try type_str.appendSlice("{EnumDef}");
                try type_str.appendSlice(self.resolved_type.?.Enum.name.string);
            },
        
            .ClassInstance => try type_str.appendSlice(self.resolved_type.?.ClassInstance.resolved_type.?.Class.name.string),
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
                try type_str.appendSlice(self.resolved_type.?.Placeholder.string);
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

            .ClassInstance => return a.ClassInstance == b.ClassInstance,
            .ObjectInstance => return a.ObjectInstance == b.ObjectInstance,
            .EnumInstance => return a.EnumInstance == b.EnumInstance,

            .Class,
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

            .Placeholder => mem.eql(u8, a.Placeholder.string, b.Placeholder.string),
        };
    }

    // Compare two type definitions
    pub fn eql(self: *Self, other: *Self) bool {
        const type_eql: bool = self.def_type == other.def_type
            and ((self.resolved_type == null and other.resolved_type == null)
                or eqlTypeUnion(self.resolved_type.?, other.resolved_type.?));
        
        // Placeholder bypasses type checking only for Class/Object/Enum[Instance]
        const self_is_placeholder: bool = self.def_type == .Placeholder
            and (other.def_type == .Object
                or other.def_type == .Class
                or other.def_type == .Enum
                or other.def_type == .ObjectInstance
                or other.def_type == .ClassInstance
                or other.def_type == .EnumInstance);
        const other_is_placeholder: bool = other.def_type == .Placeholder
            and (self.def_type == .Object
                or self.def_type == .Class
                or self.def_type == .Enum
                or self.def_type == .ObjectInstance
                or self.def_type == .ClassInstance
                or self.def_type == .EnumInstance);
        const placeholder_eql: bool = self_is_placeholder or other_is_placeholder;

        return self == other
            or (self.optional and other.def_type == .Void) // Void is equal to any optional type
            or (self.optional == other.optional
                and (type_eql or placeholder_eql));
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
        .ClassInstance => try std.fmt.bufPrint(buf, "class instance: 0x{x} `{s}`", .{ @ptrToInt(ObjClassInstance.cast(obj).?), ObjClassInstance.cast(obj).?.class.name.string }),
        .ObjectInstance => try std.fmt.bufPrint(buf, "object instance: 0x{x} `{s}`", .{ @ptrToInt(ObjObjectInstance.cast(obj).?), ObjObjectInstance.cast(obj).?.object.name.string }),
        .Class => try std.fmt.bufPrint(buf, "class: 0x{x}`{s}`", .{ @ptrToInt(ObjClass.cast(obj).?), ObjClass.cast(obj).?.name.string }),
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
