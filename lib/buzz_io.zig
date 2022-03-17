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

    const file: std.fs.File = switch (mode) {
        0 => std.fs.openFileAbsolute(std.mem.sliceTo(filename, 0), .{ .mode = .read_only }) catch {
            vm.bz_throwString("Could not open file");

            return false;
        },
        else => std.fs.createFileAbsolute(std.mem.sliceTo(filename, 0), .{ .read = mode != 1 }) catch {
            vm.bz_throwString("Could not open file");

            return false;
        }
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

    vm.bz_pushString(api.ObjString.bz_string(vm, content) orelse {
        vm.bz_throwString("Could get file content");

        return false;
    });

    return true;
}

// extern fun File_write(num fd, [num] bytes) > void;
export fn FileWrite(vm: *api.VM) bool {
    const handle: std.fs.File.Handle = @floatToInt(
        std.fs.File.Handle,
        api.Value.bz_valueToNumber(vm.bz_peek(1))
    );

    const file: std.fs.File = std.fs.File { .handle = handle };

    _ = file.write(std.mem.sliceTo(vm.bz_peek(0).bz_valueToString().?, 0)) catch {
        vm.bz_throwString("Could not write to file");

        return false;
    };

    return false;
}