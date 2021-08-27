const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const StringArrayHashMap = std.StringArrayHashMap;
const Chunk = @import("./chunk.zig").Chunk;
const VM = @import("./vm.zig").VM;
const memory = @import("./memory.zig");
const Value = @import("./value.zig").Value;

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
    Native,
};

pub fn allocateObject(vm: *VM, obj_type: ObjType) !*Obj {
    var size: usize = 0;

    var object: *Obj = switch (obj_type) {
        .String => string: {
            size = @sizeOf(*ObjString);
            var obj: *ObjString = try memory.allocate(vm, *ObjString);
            obj.obj = .{
                .obj_type = .String,
            };

            break :string &obj.obj;
        },
        .Type => typeObj: {
            size = @sizeOf(*ObjType);
            var obj: *ObjType = try memory.allocate(vm, *ObjType);
            obj.obj = .{
                .obj_type = .Type,
            };

            break :typeObj &obj.obj;
        },
        .UpValue => upValue: {
            size = @sizeOf(*ObjUpValue);
            var obj: *ObjUpValue = try memory.allocate(vm, *ObjUpValue);
            obj.obj = .{
                .obj_type = .UpValue,
            };

            break :upValue &obj.obj;
        },
        .Closure => closure: {
            size = @sizeOf(*ObjClosure);
            var obj: *ObjClosure = try memory.allocate(vm, *ObjClosure);
            obj.obj = .{
                .obj_type = .Closure,
            };

            break :closure &obj.obj;
        },
        .Function => function: {
            size = @sizeOf(*ObjFunction);
            var obj: *ObjFunction = try memory.allocate(vm, *ObjFunction);
            obj.obj = .{
                .obj_type = .Function,
            };

            break :function &obj.obj;
        },
        .ClassInstance => classInstance: {
            size = @sizeOf(*ObjClassInstance);
            var obj: *ObjClassInstance = try memory.allocate(vm, *ObjClassInstance);
            obj.obj = .{
                .obj_type = .ClassInstance,
            };

            break :classInstance &obj.obj;
        },
        .ObjectInstance => objectInstance: {
            size = @sizeOf(*ObjObjectInstance);
            var obj: *ObjObjectInstance = try memory.allocate(vm, *ObjObjectInstance);
            obj.obj = .{
                .obj_type = .ObjectInstance,
            };

            break :objectInstance &obj.obj;
        },
        .Class => class: {
            size = @sizeOf(*ObjClass);
            var obj: *ObjClass = try memory.allocate(vm, *ObjClass);
            obj.obj = .{
                .obj_type = .Class,
            };

            break :class &obj.obj;
        },
        .Object => object: {
            size = @sizeOf(*ObjObject);
            var obj: *ObjObject = try memory.allocate(vm, *ObjObject);
            obj.obj = .{
                .obj_type = .Object,
            };

            break :object &obj.obj;
        },
        .List => list: {
            size = @sizeOf(*ObjList);
            var obj: *ObjList = try memory.allocate(vm, *ObjList);
            obj.obj = .{
                .obj_type = .List,
            };

            break :list &obj.obj;
        },
        .Map => map: {
            size = @sizeOf(*ObjMap);
            var obj: *ObjMap = try memory.allocate(vm, *ObjMap);
            obj.obj = .{
                .obj_type = .Map,
            };

            break :map &obj.obj;
        },
        .Enum => enumObj: {
            size = @sizeOf(*ObjEnum);
            var obj: *ObjEnum = try memory.allocate(vm, *ObjEnum);
            obj.obj = .{
                .obj_type = .Enum,
            };

            break :enumObj &obj.obj;
        },
        .EnumInstance => enumInstance: {
            size = @sizeOf(*ObjEnumInstance);
            var obj: *ObjEnumInstance = try memory.allocate(vm, *ObjEnumInstance);
            obj.obj = .{
                .obj_type = .EnumInstance,
            };

            break :enumInstance &obj.obj;
        },
        .Bound => bound: {
            size = @sizeOf(*ObjBound);
            var obj: *ObjBound = try memory.allocate(vm, *ObjBound);
            obj.obj.obj_type = .Bound;

            break :bound &obj.obj;
        },
        .Native => native: {
            size = @sizeOf(*ObjNative);
            var obj: *ObjNative = try memory.allocate(vm, *ObjNative);
            obj.obj.obj_type = .Native;

            break :native &obj.obj;
        },
    };

    // Add new object at start of vm.objects linked list
    object.next = vm.objects;
    vm.objects = object;

    vm.bytes_allocated += size;

    if (vm.bytes_allocated > vm.next_gc) {
        memory.collect_garbage();
    }

    return object;
}

pub fn allocateString(vm: *VM, chars: []u8) !*ObjString {
    if (vm.strings.get(chars)) |interned| {
        return interned;
    } else {
        var string: *ObjString = try allocateObject(vm, .String);

        try vm.string.put(chars, string);

        return string;
    }
}

pub fn copyString(vm: *VM, chars: []u8) !*ObjString {
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
    string: []u8,

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
    closed: ?Value = null,
    next: ?*ObjUpValue = null,

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
        return .{
            .function = function,
            .upvalues = try std.ArrayList(*ObjUpValue).initCapacity(allocator, function.upValueCount),
        };
    }

    pub fn deinit(self: *Self) void {
        
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
    parameters: StringArrayHashMap(*ObjTypeDef),
    return_type: *ObjTypeDef,
    chunk: Chunk,
    upValueCount: u8 = 0,

    pub fn init(allocator: *Allocator) Self {
        return .{
            .parameters = StringArrayHashMap(*ObjTypeDef).init(allocator),
            .chunk = Chunk.init(allocator),
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
    fields: StringArrayHashMap(Value),

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
    fields: StringArrayHashMap(Value),

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
    
    /// Class name
    name: *ObjString,
    /// Class methods
    methods: StringArrayHashMap(*ObjFunction),
    /// Class fields definition
    fields: StringArrayHashMap(*ObjTypeDef),
    /// Optional super class
    super: ?*ObjClass = null,

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

    /// Object name
    name: *ObjString,
    /// Object methods
    methods: StringArrayHashMap(*ObjFunction),
    /// Object fields definition
    fields: StringArrayHashMap(*ObjTypeDef),

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
    map: std.AutoArrayHashMap(Value, Value),

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

    enum_type: *ObjTypeDef,
    map: std.StringArrayHashMap(Value),

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
    // TODO: will const be an issue here?
    case: []const u8,
    value: Value,

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

/// Bound
pub const ObjBound = struct {
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
        if (obj.obj_type != .Enum) {
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
        Class,
        Object,
        Enum,
        List,
        Map,
        Function,
        Type, // Something that holds a type, not an actual type
        Void,
    };

    pub const TypeUnion = union(Type) {
        // For those type checking is obvious, the value is a placeholder
        Bool = bool,
        Number = f64,
        Byte = u8,
        String = []u8,
        Type = bool,
        Void = bool,

        // For those we check that the value is an instance of, because those are user defined types
        Class = *ObjClass,
        Object = *ObjObject,
        Enum = *ObjEnum,

        // For those we compare definitions, so we own those structs
        List = ObjList,
        Map = ObjMap,
        Function = ObjFunction,
    };

    obj: Obj = .{
        .obj_type = .Type
    },

    /// True means its an optional (e.g `str?`)
    optional: bool,
    def_type: Type,
    /// Used when the type is not a basic type
    resolved_type: ?TypeUnion = null,

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
        if (std.meta.Tag(a) != bstd.meta.Tag(b)) {
            return false;
        }

        switch (a) {
            Bool, Number, Byte, String, Type, Void => return true,

            Class => a.Class == b.Class,
            Object => a.Object == b.Object,
            Enum => a.Enum == b.Enum,

            List => a.List.item_type.eql(b.List.item_type),
            Map => a.Map.key_type.eql(b.Map.key_type)
                and a.Map.value_type.eql(b.Map.value_type),
            Function => {
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
                    if (b.Function.parameters.get(kv.key)) |value| {
                        if (!kv.value.eql(value)) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }

                return true;
            }
        }
    }

    // Compare two type definitions
    pub fn eql(self: *Self, other: *Self) bool {
        // TODO: if we ever put typedef in a set somewhere we could replace all this witha pointer comparison
        return self.optional == other.optional
            and self.def_type == other.def_type
            and eqlTypeUnion(self.resolved_type, other.resolved_type);
    }

    // Compare value type to this type definition
    pub fn is(self: *Self, value: Value) bool {
        return switch (self.def_type) {
            Bool => value == .Bool,
            Number => value == .Number,
            Byte => value == .Byte,
            String => value == .Obj and value.Obj.obj_type == .String,
            Type => value == .Obj and value.Obj.obj_type == .Type,
            Void => value == .Null,
            Class => {
                if (value != .Obj) {
                    return false;
                }

                if (value.Obj.obj_type != .ClassInstance) {
                    return false;
                }

                const instance: ?*ObjClassInstance = ObjClassInstance.cast(value.Obj);

                if (instance == null) {
                    return false;
                }

                return instance.?.class == self.resolved_type.Class;
            },
            Object => {
                if (value != .Obj) {
                    return false;
                }

                if (value.Obj.obj_type != .ObjectInstance) {
                    return false;
                }

                const instance: ?*ObjObjectInstance = ObjObjectInstance.cast(value.Obj);

                if (instance == null) {
                    return false;
                }

                return instance.?.object == self.resolved_type.Object;
            },
            Enum => {
                if (value != .Obj) {
                    return false;
                }

                if (value.Obj.obj_type != .Enum) {
                    return false;
                }

                const instance: ?*ObjEnumInstance = ObjEnumInstance.cast(value.Obj);

                if (instance == null) {
                    return false;
                }

                return instance.?.enum_ref == self.resolved_type.Enum;
            },
            List => {
                if (value != .Obj) {
                    return false;
                }

                if (value.Obj.obj_type != .List) {
                    return false;
                }

                const instance: ?*ObjList = ObjList.cast(value.Obj);

                if (instance == null) {
                    return false;
                }

                // TODO: maybe 2 type definition in 2 separate places should refer to the same typedef in memory?
                //       until then do a deep comparison
                return instance.?.item_type.eql(self.resolved_type.List.item_type);
            },
            Map => {
                if (value != .Obj) {
                    return false;
                }

                if (value.Obj.obj_type != .Map) {
                    return false;
                }

                const instance: ?*ObjMap = ObjMap.cast(value.Obj);

                if (instance == null) {
                    return false;
                }

                // TODO: maybe 2 type definition in 2 separate places should refer to the same typedef in memory?
                //       until then do a deep comparison
                return instance.?.key_type.eql(self.resolved_type.Map.key_type)
                    and instance.?.value_type.eql(self.resolved_type.Map.value_type);
            },
            Function => {
                if (value != .Obj) {
                    return false;
                }

                if (value.Obj.obj_type != .Function) {
                    return false;
                }

                const instance: ?*ObjFunction = ObjFunction.cast(value.Obj);

                if (instance == null) {
                    return false;
                }

                // TODO: maybe 2 type definition in 2 separate places should refer to the same typedef in memory?
                //       until then do a deep comparison

                // Compare return types
                if (instance.?.return_type.eql(self.resolved_type.Function.return_type)) {
                    return false;
                }

                // Compare arity
                if (instance.?.parameters.count() != self.resolved_type.Function.parameters.count()) {
                    return false;
                }

                // Compare parameters
                var it = instance.?.parameters.iterator();
                while (it.next()) |kv| {
                    if (self.resolved_type.Function.parameters.get(kv.key)) |value| {
                        if (!kv.value.eql(value)) {
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
};