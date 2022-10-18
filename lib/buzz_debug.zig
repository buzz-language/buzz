const std = @import("std");
const api = @import("./buzz_api.zig");
const _obj = @import("../src/obj.zig");
const _parser = @import("../src/parser.zig");
const Parser = _parser.Parser;
const ObjString = _obj.ObjString;
const ObjTypeDef = _obj.ObjTypeDef;
const _memory = @import("../src/memory.zig");
const GarbageCollector = _memory.GarbageCollector;
const TypeRegistry = _memory.TypeRegistry;

export fn dump(vm: *api.VM) c_int {
    vm.bz_peek(0).bz_valueDump(vm);

    std.debug.print("\n", .{});

    return 0;
}

export fn ast(vm: *api.VM) c_int {
    var source_len: usize = 0;
    const source = api.Value.bz_valueToString(vm.bz_peek(1), &source_len);

    var script_len: usize = 0;
    const script_name = api.Value.bz_valueToString(vm.bz_peek(0), &script_len);

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
            => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return -1;
    };

    if (root != null) {
        var out = std.ArrayList(u8).init(api.VM.allocator);

        root.?.toJson(root.?, out.writer()) catch |err| {
            switch (err) {
                error.OutOfMemory,
                error.NoSpaceLeft,
                => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
            }

            return -1;
        };

        vm.bz_pushString(
            api.ObjString.bz_string(vm, if (out.items.len > 0) @ptrCast([*]const u8, out.items) else null, out.items.len) orelse {
                vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

                return -1;
            },
        );

        return 1;
    }

    vm.bz_pushError("lib.errors.CompileError", "lib.errors.CompileError".len);

    return -1;
}
