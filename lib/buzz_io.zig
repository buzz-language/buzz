const std = @import("std");
const api = @import("buzz_api.zig");

export fn getStdIn(vm: *api.VM) bool {
    vm.bz_pushNum(@intToFloat(f64, std.io.getStdIn().handle));

    return true;
}

export fn getStdOut(vm: *api.VM) bool {
    vm.bz_pushNum(@intToFloat(f64, std.io.getStdOut().handle));

    return true;
}

export fn getStdErr(vm: *api.VM) bool {
    vm.bz_pushNum(@intToFloat(f64, std.io.getStdErr().handle));

    return true;
}

export fn FileOpen(vm: *api.VM) bool {
    const filename: [*:0]const u8 = api.Value.bz_valueToString(vm.bz_pop()) orelse "";
    const mode: u8 = @floatToInt(u8, api.Value.bz_valueToNumber(vm.bz_pop()));

    const file: std.fs.File = std.fs.openFileAbsolute(
        std.mem.sliceTo(filename, 0),
        .{
            .mode = switch (mode) {
                0 => .read_only,
                1 => .write_only,
                2 => .read_write,
                else => .read_only,
            }
        }
    ) catch {
        vm.bz_pushNull();

        return true;
    };

    vm.bz_pushNum(@intToFloat(f64, file.handle));

    return true;
}

export fn FileClose(vm: *api.VM) bool {
    const handle: std.fs.File.Handle = @floatToInt(
        std.fs.File.Handle,
        api.Value.bz_valueToNumber(vm.bz_pop())
    );

    const file: std.fs.File = std.fs.File { .handle = handle };
    file.close();

    return false;
}

export fn FileReadAll(vm: *api.VM) bool {
    const handle: std.fs.File.Handle = @floatToInt(
        std.fs.File.Handle,
        api.Value.bz_valueToNumber(vm.bz_pop())
    );

    const file: std.fs.File = std.fs.File { .handle = handle };

    const content: []u8 = file.readToEndAlloc(api.VM.allocator, 10240) catch {
        vm.bz_pushNull();

        return true;
    };

    vm.bz_pushString(api.ObjString.bz_string(api.VM.allocator.dupeZ(u8, content) catch {
        vm.bz_pushNull();

        return true;
    }) orelse {
        vm.bz_pushNull();

        return true;
    });

    return true;
}

// extern fun File_read(num fd) > num?;
export fn FileRead(_: *api.VM) bool {
    unreachable;
}
// extern fun File_readN(num fd, num n) > [num]?;
export fn FileReadN(_: *api.VM) bool {
    unreachable;
}
// extern fun File_readLine(num fd) > [num]?;
export fn FileReadLine(_: *api.VM) bool {
    unreachable;
}

// extern fun File_readUntil(num fd, Function([num]) > bool predicate) > [num]?;
export fn FileReadUntil(_: *api.VM) bool {
    unreachable;
}
// extern fun File_write(num fd, [num] bytes) > void;
export fn FileWrite(_: *api.VM) bool {
    unreachable;
}