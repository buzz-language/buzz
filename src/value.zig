const std = @import("std");
const StringHashMap = std.StringHashMap;

const ValueType = enum {
    Boolean,
    Number,
    Byte,
    Obj
};

const Value = union(ValueType) {
    Boolean: bool,
    Number: f64,
    Byte: u8,
    Obj: *Obj
};

const ObjType = enum {
    String,
    Type,
    UpValue,
    Closure,
    Function,
    Instance,
    Class,
    Object,
    List,
    Map,
    Enum,
};

const Obj = struct {
    const Self = @This();

    obj_type: ObjType,
};

const ObjString = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .String
    },
    chars: []u8,

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

const ObjTypeDef = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .TypeDef
    },

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

const ObjUpValue = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .UpValue
    },

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

const ObjClosure = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .Closure
    },

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

const ObjFunction = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .Function
    },

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

const ObjInstance = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .Instance
    },

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn cast(obj: *Obj) ?*Self {
        if (obj.obj_type != .Instance) {
            return null;
        }

        return @fieldParentPtr(Self, "obj", obj);
    }
};

const ObjClass = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .Class
    },
    name: *ObjString,
    methods: StringHashMap(ObjFunction),
    fields: StringHashMap(ObjTypeDef),
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

const ObjObject = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .Object
    },

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

const ObjList = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .List
    },

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

const ObjMap = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .Map
    },

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

const ObjEnum = struct {
    const Self = @This();

    obj: Obj = .{
        .obj_type = .Enum
    },

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

test {
    const hello: []u8 = try std.heap.c_allocator.alloc(u8, 5);
    defer std.heap.c_allocator.destroy(hello.ptr);

    _ = try std.fmt.bufPrint(hello, "hello", .{});

    var boolean: Value = .{
        .Boolean = true
    };
    
    var objString: *ObjString = try std.heap.c_allocator.create(ObjString);
    defer std.heap.c_allocator.destroy(objString);
    objString.* = .{
        .chars = hello
    };

    var obj: Value = .{
        .Obj = objString.toObj()
    };

    std.debug.print("\nboolean {}, string {s}\n", .{
        boolean.Boolean,
        ObjString.cast(obj.Obj).?.chars,
    });
}