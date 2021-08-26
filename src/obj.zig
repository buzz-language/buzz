const std = @import("std");
const StringArrayHashMap = std.StringArrayHashMap;
const Chunck = @import("./chunck.zig").Chunck;

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

pub const Obj = struct {
    const Self = @This();

    obj_type: ObjType,
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
    upvalues: []*ObjUpValue,

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
    chunk: Chunck,
    upValueCount: u8,

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
    };

    pub const TypeUnion = union(Type) {
        // For those type checking is obvious, the value is a placeholder
        Bool = bool,
        Number = f64,
        Byte = u8,
        String = []u8,
        Type = bool,

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
    resolved_type: ?TypeUnion,

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
            Bool, Number, Byte, String, Type => return true,

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