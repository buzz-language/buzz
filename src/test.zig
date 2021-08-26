const std = @import("std");
const assert = std.debug.assert;

test "wtf zig" {

    const RandomStruct = struct {
        age: f32,
        dies: f32
    };

    // const Type = enum {
    //     Bool,
    //     Struct,
    // };

    // const TypeUnion = union(Type) {
    //     Bool = bool,
    //     Struct = *RandomStruct
    // };

    const rand: RandomStruct = .{
        .age = 12,
        .dies = 90
    };

    const rand2: RandomStruct = .{
        .age = 12,
        .dies = 90
    };

    // const myBool: TypeUnion = .{
    //     .Bool = false
    // };

    // const myStruct: TypeUnion = .{
    //     .Struct = &rand
    // };

    // const myOtherStruct: TypeUnion = .{
    //     .Struct = &rand2
    // };

    assert(rand == rand2);
}