const std = @import("std");
const o = @import("../obj.zig");
const VM = @import("../vm.zig").VM;
const v = @import("../value.zig");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const Token = @import("../Token.zig");

// In a wasm build, the host is providing the pattern matching (JS regexes)
extern fn patternReplaceLength(
    string_ptr: [*]const u8,
    string_len: isize,
    replacement_ptr: [*]const u8,
    replacement_len: isize,
    pattern_ptr: [*]const u8,
    pattern_len: isize,
) callconv(.c) isize;

extern fn patternReplace(
    string_ptr: [*]const u8,
    string_len: isize,
    replacement_ptr: [*]const u8,
    replacement_len: isize,
    pattern_ptr: [*]const u8,
    pattern_len: isize,
    output_ptr: [*]const u8,
    output_len: isize,
) callconv(.c) void;

extern fn patternReplaceAllLength(
    string_ptr: [*]const u8,
    string_len: isize,
    replacement_ptr: [*]const u8,
    replacement_len: isize,
    pattern_ptr: [*]const u8,
    pattern_len: isize,
) callconv(.c) isize;

extern fn patternReplaceAll(
    string_ptr: [*]const u8,
    string_len: isize,
    replacement_ptr: [*]const u8,
    replacement_len: isize,
    pattern_ptr: [*]const u8,
    pattern_len: isize,
    output_ptr: [*]const u8,
    output_len: isize,
) callconv(.c) void;

pub const pcre = @import("../pcre.zig");

const fake_token: Token = .{
    .lexeme = "",
    .source = "builtin",
    .script_name = "builtin",
    .tag = .Obj,
    .line = 0,
    .column = 0,
    .offset = 0,
};

// Return match anonymous object type: obj{ start: int, end: int, capture: str }
fn matchType(vm: *VM) !*o.ObjTypeDef {
    var object_def = o.ObjObject.ObjectDef.init(
        0,
        try vm.gc.copyString("match"),
        try vm.gc.copyString("builtin.match"),
        true,
    );

    try object_def.fields.put(
        vm.gc.allocator,
        "capture",
        .{
            .name = "capture",
            .index = 0,
            .location = 0,
            .type_def = vm.gc.type_registry.str_type,
            .final = true,
            .method = false,
            .static = false,
            .has_default = false,
            .mutable = false,
        },
    );

    try object_def.fields.put(
        vm.gc.allocator,
        "start",
        .{
            .name = "start",
            .index = 2, // because fields will be sorted
            .location = 0,
            .type_def = vm.gc.type_registry.int_type,
            .final = true,
            .method = false,
            .static = false,
            .has_default = false,
            .mutable = false,
        },
    );

    try object_def.fields.put(
        vm.gc.allocator,
        "end",
        .{
            .name = "end",
            .index = 1,
            .location = 0,
            .type_def = vm.gc.type_registry.int_type,
            .final = true,
            .method = false,
            .static = false,
            .has_default = false,
            .mutable = false,
        },
    );

    return (try vm.gc.type_registry.getTypeDef(
        .{
            .def_type = .Object,
            .resolved_type = .{
                .Object = object_def,
            },
        },
    )).toInstance(
        &vm.gc.type_registry,
        false,
    );
}

fn rawMatch(self: *o.ObjPattern, vm: *VM, subject: *o.ObjString, offset: *usize) !?*o.ObjList {
    if (subject.string.len == 0) {
        return null;
    }

    var results: ?*o.ObjList = null;
    var match_data = self.pattern.createMatchData(null) orelse {
        vm.panic("Out of memory");
        unreachable;
    };
    defer match_data.free();

    const rc = self.pattern.match(
        subject.string.ptr,
        subject.string.len,
        offset.*,
        0,
        match_data,
        null,
    );

    switch (rc) {
        @intFromEnum(pcre.MatchingError.DFA_UINVALID_UTF)...@intFromEnum(pcre.MatchingError.NOMATCH) => {},
        // TODO: handle ouptut_vector too small
        0 => {
            vm.panic("Could not match pattern");
            unreachable;
        },
        else => {
            const output_vector = match_data.getOVectorPointer();

            offset.* = @intCast(output_vector[1]);

            results = try vm.gc.allocateObject(
                o.ObjList,
                try o.ObjList.init(
                    vm.gc.allocator,
                    try vm.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .List,
                            .optional = false,
                            .resolved_type = .{
                                .List = o.ObjList.ListDef.init(
                                    vm.gc.type_registry.str_type,
                                    false,
                                ),
                            },
                        },
                    ),
                ),
            );

            // Prevent gc collection
            vm.push(results.?.toValue());

            const match_type = try matchType(vm);

            var i: usize = 0;
            while (i < rc) : (i += 1) {
                const match_instance = try vm.gc.allocateObject(
                    o.ObjObjectInstance,
                    try o.ObjObjectInstance.init(
                        vm,
                        null,
                        match_type,
                        vm.gc,
                    ),
                );

                // start
                match_instance.fields[2] = v.Value.fromInteger(@intCast(output_vector[2 * i]));
                // end
                match_instance.fields[1] = v.Value.fromInteger(@intCast(output_vector[2 * i + 1]));
                // capture
                match_instance.fields[0] = (try vm.gc.copyString(
                    subject.string[@intCast(output_vector[2 * i])..@intCast(output_vector[2 * i + 1])],
                )).toValue();

                try results.?.items.append(
                    vm.gc.allocator,
                    match_instance.toValue(),
                );
            }

            _ = vm.pop();
        },
    }

    return results;
}

fn rawMatchAll(self: *o.ObjPattern, vm: *VM, subject: *o.ObjString) !?*o.ObjList {
    if (subject.string.len == 0) {
        return null;
    }

    var results: ?*o.ObjList = null;
    var offset: usize = 0;
    while (true) {
        if (try rawMatch(self, vm, subject, &offset)) |matches| {
            const was_null = results == null;
            results = results orelse try vm.gc.allocateObject(
                o.ObjList,
                try o.ObjList.init(
                    vm.gc.allocator,
                    try vm.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .List,
                            .optional = false,
                            .resolved_type = .{
                                .List = o.ObjList.ListDef.init(
                                    matches.type_def,
                                    false,
                                ),
                            },
                        },
                    ),
                ),
            );

            if (was_null) {
                vm.push(results.?.toValue());
            }

            try results.?.items.append(vm.gc.allocator, matches.toValue());
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

fn rawReplace(self: *o.ObjPattern, vm: *VM, subject: *o.ObjString, replacement: *o.ObjString, offset: *usize) !*o.ObjString {
    if (subject.string.len == 0) {
        return subject;
    }

    var result = std.ArrayList(u8){};
    defer result.deinit(vm.gc.allocator);

    var match_data = self.pattern.createMatchData(null) orelse {
        vm.panic("Out of memory");
        unreachable;
    };
    defer match_data.free();

    const rc = self.pattern.match(
        subject.string.ptr,
        subject.string.len,
        offset.*,
        0,
        match_data,
        null,
    );

    switch (rc) {
        @intFromEnum(pcre.MatchingError.DFA_UINVALID_UTF)...@intFromEnum(pcre.MatchingError.NOMATCH) => return subject,
        // TODO: handle ouptut_vector too small
        0 => {
            vm.panic("Could not match pattern");
            unreachable;
        },
        else => {
            const output_vector = match_data.getOVectorPointer();

            offset.* = @as(usize, @intCast(output_vector[1]));

            try result.appendSlice(vm.gc.allocator, subject.string[0..@as(usize, @intCast(output_vector[0]))]);
            try result.appendSlice(vm.gc.allocator, replacement.string);
            try result.appendSlice(vm.gc.allocator, subject.string[offset.*..]);
        },
    }

    return try vm.gc.copyString(result.items);
}

fn rawReplaceAll(self: *o.ObjPattern, vm: *VM, subject: *o.ObjString, replacement: *o.ObjString) !*o.ObjString {
    if (subject.string.len == 0) {
        return subject;
    }

    var offset: usize = 0;
    var current = subject;
    while (true) {
        const replaced = try rawReplace(
            self,
            vm,
            current,
            replacement,
            &offset,
        );

        if (replaced == current) {
            break;
        } else {
            current = replaced;
        }
    }

    return current;
}

pub fn match(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = o.ObjPattern.cast(ctx.vm.peek(1).obj()).?;
    const subject = o.ObjString.cast(ctx.vm.peek(0).obj()).?;

    var offset: usize = 0;
    if (rawMatch(
        self,
        ctx.vm,
        subject,
        &offset,
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    }) |results| {
        ctx.vm.push(results.toValue());
    } else {
        ctx.vm.push(v.Value.Null);
    }

    return 1;
}

pub fn replace(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = o.ObjPattern.cast(ctx.vm.peek(2).obj()).?;
    const subject = o.ObjString.cast(ctx.vm.peek(1).obj()).?;
    const replacement = o.ObjString.cast(ctx.vm.peek(0).obj()).?;

    if (!is_wasm) {
        var offset: usize = 0;
        const result = rawReplace(
            self,
            ctx.vm,
            subject,
            replacement,
            &offset,
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };

        ctx.vm.push(result.toValue());
    } else {
        const buffer = ctx.vm.gc.allocator.alloc(
            u8,
            @intCast(
                patternReplaceLength(
                    subject.string.ptr,
                    @intCast(subject.string.len),
                    replacement.string.ptr,
                    @intCast(replacement.string.len),
                    self.source.ptr,
                    @intCast(self.source.len),
                ),
            ),
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };
        defer ctx.vm.gc.allocator.free(buffer);

        patternReplace(
            subject.string.ptr,
            @intCast(subject.string.len),
            replacement.string.ptr,
            @intCast(replacement.string.len),
            self.source.ptr,
            @intCast(self.source.len),
            buffer.ptr,
            @intCast(buffer.len),
        );

        ctx.vm.push(
            (ctx.vm.gc.copyString(buffer) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            }).toValue(),
        );
    }

    return 1;
}

pub fn matchAll(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = o.ObjPattern.cast(ctx.vm.peek(1).obj()).?;
    const subject = o.ObjString.cast(ctx.vm.peek(0).obj()).?;

    if (rawMatchAll(
        self,
        ctx.vm,
        subject,
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    }) |results| {
        ctx.vm.push(results.toValue());
    } else {
        ctx.vm.push(v.Value.Null);
    }

    return 1;
}

pub fn replaceAll(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = o.ObjPattern.cast(ctx.vm.peek(2).obj()).?;
    const subject = o.ObjString.cast(ctx.vm.peek(1).obj()).?;
    const replacement = o.ObjString.cast(ctx.vm.peek(0).obj()).?;

    if (!is_wasm) {
        const result = rawReplaceAll(
            self,
            ctx.vm,
            subject,
            replacement,
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };

        ctx.vm.push(result.toValue());
    } else {
        const buffer = ctx.vm.gc.allocator.alloc(
            u8,
            @intCast(
                patternReplaceAllLength(
                    subject.string.ptr,
                    @intCast(subject.string.len),
                    replacement.string.ptr,
                    @intCast(replacement.string.len),
                    self.source.ptr,
                    @intCast(self.source.len),
                ),
            ),
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };
        defer ctx.vm.gc.allocator.free(buffer);

        patternReplaceAll(
            subject.string.ptr,
            @intCast(subject.string.len),
            replacement.string.ptr,
            @intCast(replacement.string.len),
            self.source.ptr,
            @intCast(self.source.len),
            buffer.ptr,
            @intCast(buffer.len),
        );

        ctx.vm.push(
            (ctx.vm.gc.copyString(buffer) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            }).toValue(),
        );
    }

    return 1;
}
