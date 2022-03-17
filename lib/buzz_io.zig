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
    const mode: u8 = @floatToInt(u8, api.Value.bz_valueToNumber(vm.bz_peek(0)));
    const filename: [*:0]const u8 = api.Value.bz_valueToString(vm.bz_peek(1)) orelse "";

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
        vm.bz_throwString("Could not open file");

        return false;
    };

    vm.bz_pushNum(@intToFloat(f64, file.handle));

    return true;
}

export fn FileClose(vm: *api.VM) bool {
    const handle: std.fs.File.Handle = @floatToInt(
        std.fs.File.Handle,
        api.Value.bz_valueToNumber(vm.bz_peek(0))
    );

    const file: std.fs.File = std.fs.File { .handle = handle };
    file.close();

    return false;
}

export fn FileReadAll(vm: *api.VM) bool {
    const handle: std.fs.File.Handle = @floatToInt(
        std.fs.File.Handle,
        api.Value.bz_valueToNumber(vm.bz_peek(0))
    );

    const file: std.fs.File = std.fs.File { .handle = handle };

    const content: [*:0]u8 = file.readToEndAllocOptions(api.VM.allocator, 10240, null, @alignOf(u8), 0) catch {
        vm.bz_throwString("Could not read file");

        return false;
    };

    vm.bz_pushString(api.ObjString.bz_string(content) orelse {
        vm.bz_throwString("Could get file content");

        return false;
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