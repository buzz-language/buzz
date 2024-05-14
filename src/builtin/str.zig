const std = @import("std");
const _obj = @import("../obj.zig");
const ObjString = _obj.ObjString;
const ObjList = _obj.ObjList;
const ObjTypeDef = _obj.ObjTypeDef;
const NativeCtx = _obj.NativeCtx;
const VM = @import("../vm.zig").VM;
const _value = @import("../value.zig");
const Value = _value.Value;

pub fn trim(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(0).obj()).?;

    const trimmed = std.mem.trim(u8, str.string, " \t\r\n");

    ctx.vm.push((ctx.vm.gc.copyString(trimmed) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    }).toValue());

    return 1;
}

pub fn len(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(
        Value.fromInteger(
            @intCast(str.string.len),
        ),
    );

    return 1;
}

pub fn utf8Len(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(
        Value.fromInteger(
            @intCast(std.unicode.utf8CountCodepoints(str.string) catch 0),
        ),
    );

    return 1;
}

pub fn utf8Valid(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(
        Value.fromBoolean(
            std.unicode.utf8ValidateSlice(str.string),
        ),
    );

    return 1;
}

pub fn utf8Codepoints(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(0).obj()).?;

    const list_def_type = ctx.vm.gc.type_registry.getTypeDef(
        ObjTypeDef{
            .def_type = .List,
            .optional = false,
            .resolved_type = .{
                .List = ObjList.ListDef.init(
                    ctx.vm.gc.allocator,
                    ctx.vm.gc.type_registry.str_type,
                ),
            },
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    var list = (ctx.vm.gc.allocateObject(
        ObjList,
        ObjList.init(ctx.vm.gc.allocator, list_def_type),
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    });

    if (std.unicode.utf8ValidateSlice(str.string)) {
        const view = std.unicode.Utf8View.init(str.string) catch unreachable;
        var it = view.iterator();
        while (it.nextCodepointSlice()) |codepoint| {
            const codepoint_str = ctx.vm.gc.copyString(codepoint) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };

            list.rawAppend(ctx.vm.gc, codepoint_str.toValue()) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    ctx.vm.push(list.toValue());

    return 1;
}

pub fn repeat(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const n = ctx.vm.peek(0).integer();

    var new_string: std.ArrayList(u8) = std.ArrayList(u8).init(ctx.vm.gc.allocator);
    var i: usize = 0;
    while (i < n) : (i += 1) {
        new_string.appendSlice(str.string) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };
    }

    const new_objstring = ctx.vm.gc.copyString(new_string.items) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(new_objstring.toValue());

    return 1;
}

pub fn byte(ctx: *NativeCtx) c_int {
    const self = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const index = @min(
        @max(
            0,
            ctx.vm.peek(0).integer(),
        ),
        self.string.len - 1,
    );

    ctx.vm.push(
        Value.fromInteger(
            @intCast(self.string[@intCast(index)]),
        ),
    );

    return 1;
}

pub fn indexOf(ctx: *NativeCtx) c_int {
    const self = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const needle = ObjString.cast(ctx.vm.peek(0).obj()).?;

    const index = std.mem.indexOf(u8, self.string, needle.string);

    ctx.vm.push(
        if (index) |uindex|
            Value.fromInteger(@intCast(uindex))
        else
            Value.Null,
    );

    return 1;
}

pub fn startsWith(ctx: *NativeCtx) c_int {
    const self = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const needle = ObjString.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(
        Value.fromBoolean(
            std.mem.startsWith(u8, self.string, needle.string),
        ),
    );

    return 1;
}

pub fn endsWith(ctx: *NativeCtx) c_int {
    const self = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const needle = ObjString.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(
        Value.fromBoolean(
            std.mem.endsWith(u8, self.string, needle.string),
        ),
    );

    return 1;
}

pub fn replace(ctx: *NativeCtx) c_int {
    const self = ObjString.cast(ctx.vm.peek(2).obj()).?;
    const needle = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const replacement = ObjString.cast(ctx.vm.peek(0).obj()).?;

    const new_string = std.mem.replaceOwned(
        u8,
        ctx.vm.gc.allocator,
        self.string,
        needle.string,
        replacement.string,
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(
        (ctx.vm.gc.copyString(new_string) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        }).toValue(),
    );

    return 1;
}

pub fn sub(ctx: *NativeCtx) c_int {
    const self = ObjString.cast(ctx.vm.peek(2).obj()).?;
    const start = @min(
        @max(
            0,
            ctx.vm.peek(1).integer(),
        ),
        self.string.len - 1,
    );
    const upto = if (ctx.vm.peek(0).integerOrNull()) |u|
        @max(0, u)
    else
        null;

    const limit: usize = if (upto != null and @as(usize, @intCast(start + upto.?)) < self.string.len)
        @intCast(start + upto.?)
    else
        self.string.len;
    const substr = self.string[@intCast(start)..limit];

    ctx.vm.push(
        (ctx.vm.gc.copyString(substr) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        }).toValue(),
    );

    return 1;
}

pub fn split(ctx: *NativeCtx) c_int {
    const self = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const separator = ObjString.cast(ctx.vm.peek(0).obj()).?;

    const list_def_type = ctx.vm.gc.type_registry.getTypeDef(
        .{
            .def_type = .List,
            .optional = false,
            .resolved_type = .{
                .List = ObjList.ListDef.init(
                    ctx.vm.gc.allocator,
                    ctx.vm.gc.type_registry.str_type,
                ),
            },
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    var list = ctx.vm.gc.allocateObject(
        ObjList,
        ObjList.init(ctx.vm.gc.allocator, list_def_type),
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    // Prevent gc & is result
    ctx.vm.push(list.toValue());

    if (separator.string.len > 0) {
        var it = std.mem.splitSequence(u8, self.string, separator.string);
        while (it.next()) |fragment| {
            const fragment_str = ctx.vm.gc.copyString(fragment) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };

            list.rawAppend(ctx.vm.gc, fragment_str.toValue()) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    } else {
        for (self.string) |char| {
            const fragment_str = ctx.vm.gc.copyString(&([_]u8{char})) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };

            list.rawAppend(ctx.vm.gc, fragment_str.toValue()) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    return 1;
}

pub fn encodeBase64(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(0).obj()).?;

    const encoded = ctx.vm.gc.allocator.alloc(
        u8,
        std.base64.standard.Encoder.calcSize(str.string.len),
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };
    defer ctx.vm.gc.allocator.free(encoded);

    var new_string = ctx.vm.gc.copyString(
        std.base64.standard.Encoder.encode(encoded, str.string),
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(new_string.toValue());

    return 1;
}

// FIXME: signature should be fun decodeBase64(str self) > str !> DecodeError
pub fn decodeBase64(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(0).obj()).?;

    const size = std.base64.standard.Decoder.calcSizeForSlice(str.string) catch {
        ctx.vm.push((ctx.vm.gc.copyString("Could not decode string") catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        }).toValue());

        return -1;
    };
    const decoded = ctx.vm.gc.allocator.alloc(u8, size) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };
    defer ctx.vm.gc.allocator.free(decoded);

    std.base64.standard.Decoder.decode(decoded, str.string) catch {
        ctx.vm.push((ctx.vm.gc.copyString("Could not decode string") catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        }).toValue());

        return -1;
    };

    var new_string = ctx.vm.gc.copyString(decoded) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(new_string.toValue());

    return 1;
}

pub fn upper(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(0).obj()).?;

    if (str.string.len == 0) {
        ctx.vm.push(str.toValue());

        return 1;
    }

    var new_str = ctx.vm.gc.allocator.alloc(u8, str.string.len) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };
    defer ctx.vm.gc.allocator.free(new_str);

    for (str.string, 0..) |char, index| {
        switch (char) {
            'a'...'z' => new_str[index] = std.ascii.toUpper(char),
            else => new_str[index] = char,
        }
    }

    var obj_string = ctx.vm.gc.copyString(new_str) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(obj_string.toValue());

    return 1;
}

pub fn lower(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(0).obj()).?;

    if (str.string.len == 0) {
        ctx.vm.push(str.toValue());

        return 1;
    }

    var new_str = ctx.vm.gc.allocator.alloc(u8, str.string.len) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };
    defer ctx.vm.gc.allocator.free(new_str);

    for (str.string, 0..) |char, index| {
        switch (char) {
            'A'...'Z' => new_str[index] = std.ascii.toLower(char),
            else => new_str[index] = char,
        }
    }

    var obj_string = ctx.vm.gc.copyString(new_str) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(obj_string.toValue());

    return 1;
}

pub fn hex(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(0).obj()).?;

    if (str.string.len == 0) {
        ctx.vm.push(str.toValue());

        return 1;
    }

    var result = std.ArrayList(u8).init(ctx.vm.gc.allocator);
    defer result.deinit();
    var writer = result.writer();

    for (str.string) |char| {
        writer.print("{x:0>2}", .{char}) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };
    }

    var obj_string = ctx.vm.gc.copyString(result.items) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(obj_string.toValue());

    return 1;
}

pub fn bin(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(0).obj()).?;

    if (str.string.len == 0) {
        ctx.vm.push(str.toValue());

        return 1;
    }

    var result = ctx.vm.gc.allocator.alloc(u8, str.string.len / 2) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };
    defer ctx.vm.gc.allocator.free(result);

    for (0..result.len) |i| {
        result[i] = std.fmt.parseInt(u8, str.string[(i * 2)..(i * 2 + 2)], 16) catch {
            ctx.vm.push(
                (ctx.vm.gc.copyString("String does not contain valid hex values") catch {
                    ctx.vm.panic("Out of memory");
                    unreachable;
                }).toValue(),
            );

            return -1;
        };
    }

    var obj_string = ctx.vm.gc.copyString(result) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(obj_string.toValue());

    return 1;
}
