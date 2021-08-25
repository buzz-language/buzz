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
    Bound,
    // TODO: Native -> for FFI
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

/// Type
pub const ObjTypeDef = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .TypeDef
    },

    /// True means its an optional (e.g `str?`)
    optional: bool,
    /// Actual type is an identifier, when type checking it's resolved to either basic types or classes/objects/enums
    def: []u8,

    // TODO: how to express function types?

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .TypeDef) {
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

    pub const Parameter = struct {
        name: *ObjString,
        paramType: *ObjTypeDef,
    };

    obj: Obj = .{
        .obj_type = .Function
    },

    name: *ObjString,
    parameters: []Parameter,
    return_type: *ObjTypeDef,
    chunk: Chunck,

    // TODO
    upValueCount: u32,

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
    super: ?*ObjClass,

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
    // TODO: if key is *Obj, that means the actual key is a pointer: can this be an issue?
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