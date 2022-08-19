const std = @import("std");
const api = @import("./buzz_api.zig");
const _obj = @import("../src/obj.zig");
const _parser = @import("../src/parser.zig");
const Parser = _parser.Parser;
const TypeRegistry = _obj.TypeRegistry;
const ObjString = _obj.ObjString;
const ObjTypeDef = _obj.ObjTypeDef;
const utils = @import("../src/utils.zig");
const GarbageCollector = @import("../src/memory.zig").GarbageCollector;

export fn ast(vm: *api.VM) c_int {
    const source: [*:0]const u8 = api.Value.bz_valueToString(vm.bz_peek(1)) orelse {
        vm.bz_throwString("Could not build AST");

        return -1;
    };

    const script_name: [*:0]const u8 = api.Value.bz_valueToString(vm.bz_peek(0)) orelse {
        vm.bz_throwString("Could not build AST");

        return -1;
    };

    var gc = GarbageCollector.init(api.VM.allocator);
    var strings = std.StringHashMap(*ObjString).init(api.VM.allocator);
    var imports = std.StringHashMap(Parser.ScriptImport).init(api.VM.allocator);
    var type_registry = TypeRegistry{
        .gc = &gc,
        .registry = std.StringHashMap(*ObjTypeDef).init(api.VM.allocator),
    };

    var parser = Parser.init(&gc, &imports, &type_registry, false);

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

    const root = parser.parse(utils.toSlice(source), utils.toSlice(script_name)) catch {
        vm.bz_throwString("Could not build AST");

        return -1;
    };

    if (root != null) {
        var out = std.ArrayList(u8).init(api.VM.allocator);

        root.?.toJson(root.?, out.writer()) catch {
            vm.bz_throwString("Could not build AST");

            return -1;
        };

        vm.bz_pushString(
            api.ObjString.bz_string(vm, utils.toCString(api.VM.allocator, out.items) orelse {
                vm.bz_throwString("Could not build AST");

                return -1;
            }) orelse {
                vm.bz_throwString("Could not build AST");

                return -1;
            },
        );

        return 1;
    }

    vm.bz_throwString("Could not parse buzz program");

    return -1;
}
