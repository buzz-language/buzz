const std = @import("std");
const api = @import("./buzz_api.zig");
const _obj = @import("../obj.zig");
const _parser = @import("../parser.zig");
const Parser = _parser.Parser;
const ObjString = _obj.ObjString;
const ObjTypeDef = _obj.ObjTypeDef;
const _memory = @import("../memory.zig");
const GarbageCollector = _memory.GarbageCollector;
const TypeRegistry = _memory.TypeRegistry;

export fn dump_raw(ctx: *api.NativeCtx, value: api.Value) void {
    value.bz_valueDump(ctx.vm);

    std.debug.print("\n", .{});
}

export fn dump(ctx: *api.NativeCtx) c_int {
    dump_raw(ctx, ctx.vm.bz_peek(0));

    return 0;
}

export fn ast_raw(ctx: *api.NativeCtx, source_value: api.Value, script_name_value: api.Value) api.Value {
    var source_len: usize = 0;
    const source = api.Value.bz_valueToString(source_value, &source_len);

    var script_len: usize = 0;
    const script_name = api.Value.bz_valueToString(script_name_value, &script_len);

    var gc = GarbageCollector.init(api.VM.allocator);
    gc.type_registry = TypeRegistry{
        .gc = &gc,
        .registry = std.StringHashMap(*ObjTypeDef).init(api.VM.allocator),
    };
    var strings = std.StringHashMap(*ObjString).init(api.VM.allocator);
    var imports = std.StringHashMap(Parser.ScriptImport).init(api.VM.allocator);

    var parser = Parser.init(&gc, &imports, false);

    defer {
        parser.deinit();
        strings.deinit();
        var it = imports.iterator();
        while (it.next()) |kv| {
            kv.value_ptr.*.globals.deinit();
        }
        imports.deinit();
        // TODO: free type_registry and its keys which are on the heap
    }

    const root = parser.parse(source.?[0..source_len], script_name.?[0..script_len]) catch |err| {
        switch (err) {
            error.OutOfMemory,
            error.Overflow,
            error.InvalidCharacter,
            error.NoSpaceLeft,
            => ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return api.Value.Error;
    };

    if (root != null) {
        var out = std.ArrayList(u8).init(api.VM.allocator);

        root.?.toJson(root.?, &out.writer()) catch |err| {
            switch (err) {
                error.OutOfMemory,
                error.NoSpaceLeft,
                => ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
            }

            return api.Value.Error;
        };

        return (api.ObjString.bz_string(ctx.vm, if (out.items.len > 0) @ptrCast([*]const u8, out.items) else null, out.items.len) orelse {
            ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

            return api.Value.Error;
        }).bz_objStringToValue();
    }

    ctx.vm.bz_pushError("lib.errors.CompileError", "lib.errors.CompileError".len);

    return api.Value.Error;
}

export fn ast(ctx: *api.NativeCtx) c_int {
    const result = ast_raw(
        ctx,
        ctx.vm.bz_peek(1),
        ctx.vm.bz_peek(0),
    );

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}
