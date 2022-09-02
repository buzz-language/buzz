const std = @import("std");
const api = @import("./buzz_api.zig");

export fn getStdIn(vm: *api.VM) c_int {
    vm.bz_pushInteger(@intCast(i64, std.io.getStdIn().handle));

    return 1;
}

export fn getStdOut(vm: *api.VM) c_int {
    vm.bz_pushInteger(@intCast(i64, std.io.getStdOut().handle));

    return 1;
}

export fn getStdErr(vm: *api.VM) c_int {
    vm.bz_pushInteger(@intCast(i64, std.io.getStdErr().handle));

    return 1;
}

export fn FileOpen(vm: *api.VM) c_int {
    const mode: u8 = @intCast(u8, api.Value.bz_valueToInteger(vm.bz_peek(0)));
    var len: usize = 0;
    const filename = vm.bz_peek(1).bz_valueToString(&len);

    if (len == 0) {
        vm.bz_throwString("File name is empty", "File name is empty".len);

        return -1;
    }

    const filename_slice = filename.?[0..len];

    var file: std.fs.File = if (std.fs.path.isAbsolute(filename_slice))
        switch (mode) {
            0 => std.fs.openFileAbsolute(filename_slice, .{ .mode = .read_only }) catch |err| {
                if (err == std.fs.File.OpenError.FileNotFound) {
                    vm.bz_pushError("lib.io.FileNotFoundError", "lib.io.FileNotFoundError".len);
                } else {
                    vm.bz_throwString("Could not open file", "Could not open file".len);
                }

                return -1;
            },
            else => std.fs.createFileAbsolute(filename_slice, .{ .read = mode != 1 }) catch |err| {
                if (err == std.fs.File.OpenError.FileNotFound) {
                    vm.bz_pushError("lib.io.FileNotFoundError", "lib.io.FileNotFoundError".len);
                } else {
                    vm.bz_throwString("Could not open file", "Could not open file".len);
                }

                return -1;
            },
        }
    else switch (mode) {
        0 => std.fs.cwd().openFile(filename_slice, .{ .mode = .read_only }) catch |err| {
            if (err == std.fs.File.OpenError.FileNotFound) {
                vm.bz_pushError("lib.io.FileNotFoundError", "lib.io.FileNotFoundError".len);
            } else {
                vm.bz_throwString("Could not open file", "Could not open file".len);
            }

            return -1;
        },
        else => std.fs.cwd().createFile(filename_slice, .{ .read = mode != 1 }) catch |err| {
            if (err == std.fs.File.OpenError.FileNotFound) {
                vm.bz_pushError("lib.io.FileNotFoundError", "lib.io.FileNotFoundError".len);
            } else {
                vm.bz_throwString("Could not open file", "Could not open file".len);
            }

            return -1;
        },
    };

    vm.bz_pushInteger(@intCast(i64, file.handle));

    return 1;
}

export fn FileClose(vm: *api.VM) c_int {
    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        api.Value.bz_valueToInteger(vm.bz_peek(0)),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };
    file.close();

    return 0;
}

export fn FileReadAll(vm: *api.VM) c_int {
    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        api.Value.bz_valueToInteger(vm.bz_peek(0)),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };

    const content: []u8 = file.readToEndAllocOptions(
        api.VM.allocator,
        std.math.maxInt(u64),
        null,
        @alignOf(u8),
        null,
    ) catch {
        vm.bz_throwString("Could not read file", "Could not read file".len);

        return -1;
    };

    vm.bz_pushString(api.ObjString.bz_string(vm, if (content.len > 0) @ptrCast([*]const u8, content) else null, content.len) orelse {
        vm.bz_throwString("Could not read file", "Could not read file".len);

        return -1;
    });

    return 1;
}

export fn FileReadLine(vm: *api.VM) c_int {
    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        api.Value.bz_valueToInteger(vm.bz_peek(0)),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };
    const reader = file.reader();

    var buffer = reader.readUntilDelimiterAlloc(api.VM.allocator, '\n', 16 * 8 * 64) catch |err| {
        if (err == error.EndOfStream) {
            vm.bz_pushNull();

            return 1;
        }

        vm.bz_throwString("Could not read file", "Could not read file".len);

        return -1;
    };

    vm.bz_pushString(api.ObjString.bz_string(vm, if (buffer.len > 0) @ptrCast([*]const u8, buffer) else null, buffer.len) orelse {
        vm.bz_throwString("Could not read file", "Could not read file".len);

        return -1;
    });

    return 1;
}

export fn FileRead(vm: *api.VM) c_int {
    const n = api.Value.bz_valueToInteger(vm.bz_peek(0));
    if (n < 0) {
        vm.bz_throwString("Could not read file: `n` is not positive integer", "Could not read file: `n` is not positive integer".len);

        return -1;
    }

    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        api.Value.bz_valueToInteger(vm.bz_peek(1)),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };
    const reader = file.reader();

    var buffer = api.VM.allocator.alloc(u8, @intCast(usize, n)) catch {
        vm.bz_throwString("Could not read file", "Could not read file".len);

        return -1;
    };

    // bz_string will copy it
    defer api.VM.allocator.free(buffer);

    const read = reader.readAll(buffer) catch {
        vm.bz_throwString("Could not read file", "Could not read file".len);

        return -1;
    };

    if (read == 0) {
        vm.bz_pushNull();
    } else {
        vm.bz_pushString(api.ObjString.bz_string(vm, if (buffer[0..read].len > 0) @ptrCast([*]const u8, buffer[0..read]) else null, read) orelse {
            vm.bz_throwString("Could not read file", "Could not read file".len);

            return -1;
        });
    }

    return 1;
}

// extern fun File_write(num fd, [num] bytes) > void;
export fn FileWrite(vm: *api.VM) c_int {
    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        api.Value.bz_valueToInteger(vm.bz_peek(1)),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };

    var len: usize = 0;
    var value = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        return 0;
    }

    _ = file.write(value.?[0..len]) catch {
        vm.bz_throwString("Could not write file", "Could not write file".len);

        return -1;
    };

    return 0;
}
