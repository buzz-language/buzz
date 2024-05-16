const std = @import("std");
const api = @import("buzz_api.zig");
const io = @import("io.zig");

const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

pub export fn dump(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_peek(0).bz_valueDump(ctx.vm);

    io.print("\n", .{});

    return 0;
}

// TODO: reactivate
// fn ast(ctx: *api.NativeCtx) c_int {
//     var source_len: usize = 0;
//     const source = api.Value.bz_valueToString(ctx.vm.bz_peek(1), &source_len);

//     var script_len: usize = 0;
//     const script_name = api.Value.bz_valueToString(ctx.vm.bz_peek(0), &script_len);

//     var gc = GarbageCollector.init(api.VM.allocator);
//     gc.type_registry = TypeRegistry{
//         .gc = &gc,
//         .registry = std.StringHashMap(*ObjTypeDef).init(api.VM.allocator),
//     };
//     var strings = std.StringHashMap(*ObjString).init(api.VM.allocator);
//     var imports = std.StringHashMap(Parser.ScriptImport).init(api.VM.allocator);

//     var parser = Parser.init(
//         &gc,
//         &imports,
//         false,
//         .Ast,
//     );

//     defer {
//         parser.deinit();
//         strings.deinit();
//         var it = imports.iterator();
//         while (it.next()) |kv| {
//             kv.value_ptr.*.globals.deinit();
//         }
//         imports.deinit();
//         // TODO: free type_registry and its keys which are on the heap
//     }

//     const root = parser.parse(source.?[0..source_len], script_name.?[0..script_len]) catch |err| {
//         switch (err) {
//             error.OutOfMemory,
//             error.NoSpaceLeft,
//             => @panic("Out of memory"),
//         }

//         return -1;
//     };

//     if (root != null) {
//         var out = std.ArrayList(u8).init(api.VM.allocator);

//         root.?.toJson(root.?, &out.writer()) catch |err| {
//             switch (err) {
//                 error.OutOfMemory,
//                 error.NoSpaceLeft,
//                 => @panic("Out of memory"),
//             }

//             return -1;
//         };

//         ctx.vm.bz_pushString(
//             api.ObjString.bz_string(ctx.vm, if (out.items.len > 0) @as([*]const u8, @ptrCast(out.items)) else null, out.items.len) orelse {
//                 @panic("Out of memory");
//             },
//         );

//         return 1;
//     }

//     ctx.vm.pushError("errors.CompileError", null);

//     return -1;
// }
