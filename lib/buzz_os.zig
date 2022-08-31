const std = @import("std");
const api = @import("./buzz_api.zig");
const builtin = @import("builtin");

export fn time(vm: *api.VM) c_int {
    vm.bz_pushInteger(std.time.milliTimestamp());

    return 1;
}

export fn env(vm: *api.VM) c_int {
    var len: usize = 0;
    const key = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        vm.bz_throwString("Env key is empty", "Env key is empty".len);

        return -1;
    }

    const key_slice = api.VM.allocator.dupeZ(u8, key.?[0..len]) catch null;
    defer {
        if (key_slice != null) {
            api.VM.allocator.free(key_slice.?);
        }
    }

    if (key_slice == null) {
        vm.bz_throwString("Could not get environment variable", "Could not get environment variable".len);

        return -1;
    }

    if (std.os.getenvZ(key_slice.?)) |value| {
        vm.bz_pushString(api.ObjString.bz_string(vm, if (value.len > 0) @ptrCast([*]const u8, value) else null, value.len) orelse {
            vm.bz_throwString("Could not get environment variable", "Could not get environment variable".len);

            return -1;
        });

        return 1;
    }

    vm.bz_pushNull();

    return 1;
}

fn sysTempDir() []const u8 {
    return switch (builtin.os.tag) {
        .windows => unreachable, // TODO: GetTempPath
        else => std.os.getenv("TMPDIR") orelse std.os.getenv("TMP") orelse std.os.getenv("TEMP") orelse std.os.getenv("TEMPDIR") orelse "/tmp",
    };
}

export fn tmpDir(vm: *api.VM) c_int {
    const tmp_dir: []const u8 = sysTempDir();

    vm.bz_pushString(api.ObjString.bz_string(vm, if (tmp_dir.len > 0) @ptrCast([*]const u8, tmp_dir) else null, tmp_dir.len) orelse {
        vm.bz_throwString("Could not get tmp dir", "Could not get tmp dir".len);

        return -1;
    });

    return 1;
}

// TODO: what if file with same random name exists already?
export fn tmpFilename(vm: *api.VM) c_int {
    var prefix_len: usize = 0;
    const prefix = vm.bz_peek(0).bz_valueToString(&prefix_len);

    const prefix_slice = if (prefix_len == 0) "" else prefix.?[0..prefix_len];

    var random_part = std.ArrayList(u8).init(api.VM.allocator);
    defer random_part.deinit();
    random_part.writer().print("{x}", .{std.crypto.random.int(i64)}) catch {
        vm.bz_throwString("Could not get tmp file", "Could not get tmp file".len);

        return -1;
    };

    var random_part_b64 = std.ArrayList(u8).initCapacity(api.VM.allocator, std.base64.standard.Encoder.calcSize(random_part.items.len)) catch {
        vm.bz_throwString("Could not get tmp file", "Could not get tmp file".len);

        return -1;
    };
    random_part_b64.expandToCapacity();
    defer random_part_b64.deinit();

    _ = std.base64.standard.Encoder.encode(random_part_b64.items, random_part.items);

    var final = std.ArrayList(u8).init(api.VM.allocator);
    defer final.deinit();

    // TODO: take into account system file separator (windows is \)
    final.writer().print("{s}{s}-{s}", .{ sysTempDir(), prefix_slice, random_part_b64.items }) catch {
        vm.bz_throwString("Could not get tmp file", "Could not get tmp file".len);

        return -1;
    };

    vm.bz_pushString(api.ObjString.bz_string(vm, if (final.items.len > 0) @ptrCast([*]const u8, final.items) else null, final.items.len) orelse {
        vm.bz_throwString("Could not get tmp file", "Could not get tmp file".len);

        return -1;
    });

    return 1;
}

// If it was named `exit` it would be considered by zig as a callback when std.os.exit is called
export fn buzzExit(vm: *api.VM) c_int {
    const exitCode: i64 = api.Value.bz_valueToInteger(vm.bz_peek(0));

    std.os.exit(@intCast(u8, exitCode));

    return 0;
}

export fn execute(vm: *api.VM) c_int {
    var command = std.ArrayList([]const u8).init(api.VM.allocator);
    defer command.deinit();

    const argv = api.ObjList.bz_valueToList(vm.bz_peek(0));
    const len = argv.bz_listLen();
    var i: usize = 0;
    while (i < len) : (i += 1) {
        const arg = argv.bz_listGet(i);
        var arg_len: usize = 0;
        var arg_str = arg.bz_valueToString(&arg_len);

        std.debug.assert(arg_len > 0);

        command.append(arg_str.?[0..arg_len]) catch {
            vm.bz_throwString("Could not execute", "Could not execute".len);

            return -1;
        };
    }

    var child_process = std.ChildProcess.init(command.items, api.VM.allocator);
    child_process.disable_aslr = builtin.target.isDarwin();

    child_process.spawn() catch {
        vm.bz_throwString("Could not execute", "Could not execute".len);

        return -1;
    };

    vm.bz_pushInteger(@intCast(i64, (child_process.wait() catch {
        vm.bz_throwString("Could not execute", "Could not execute".len);

        return -1;
    }).Exited));

    return 1;
}

export fn SocketConnect(vm: *api.VM) c_int {
    var len: usize = 0;
    const address_value = api.Value.bz_valueToString(vm.bz_peek(2), &len);
    const address = if (len > 0) address_value.?[0..len] else "";
    const port: ?i64 = api.Value.bz_valueToInteger(vm.bz_peek(1));
    if (port == null or port.? < 0) {
        vm.bz_throwString("Port should be a positive integer", "Port should be a positive integer".len);

        return -1;
    }

    const protocol = api.Value.bz_valueToInteger(vm.bz_peek(0));

    switch (protocol) {
        0 => {
            const stream = std.net.tcpConnectToHost(api.VM.allocator, address, @intCast(u16, port.?)) catch {
                vm.bz_throwString("Could not connect", "Could not connect".len);

                return -1;
            };

            vm.bz_pushInteger(@intCast(i64, stream.handle));

            return 1;
        },
        1, // TODO: UDP
        2, // TODO: IPC
        => {
            vm.bz_throwString("Not yet implemented", "Not yet implemented".len);

            return -1;
        },
        else => {
            vm.bz_throwString("Unsupported protocol", "Unsupported protocol".len);

            return -1;
        },
    }
}

export fn SocketClose(vm: *api.VM) c_int {
    const socket: std.os.socket_t = @intCast(
        std.os.socket_t,
        api.Value.bz_valueToInteger(vm.bz_peek(0)),
    );

    std.os.closeSocket(socket);

    return 0;
}

export fn SocketRead(vm: *api.VM) c_int {
    const n: i64 = api.Value.bz_valueToInteger(vm.bz_peek(0));
    if (n < 0) {
        vm.bz_throwString("Could not read from socket: `n` is not a positive integer", "Could not read from socket: `n` is not a positive integer".len);

        return -1;
    }

    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
        api.Value.bz_valueToInteger(vm.bz_peek(1)),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = api.VM.allocator.alloc(u8, @intCast(usize, n)) catch {
        vm.bz_throwString("Could not read from socket", "Could not read from socket".len);

        return -1;
    };

    // bz_string will copy it
    defer api.VM.allocator.free(buffer);

    const read = reader.readAll(buffer) catch {
        vm.bz_throwString("Could not read from socket", "Could not read from socket".len);

        return -1;
    };

    if (read == 0) {
        vm.bz_pushNull();
    } else {
        vm.bz_pushString(api.ObjString.bz_string(vm, if (buffer[0..read].len > 0) @ptrCast([*]const u8, buffer[0..read]) else null, read) orelse {
            vm.bz_throwString("Could not read from socket", "Could not read from socket".len);

            return -1;
        });
    }

    return 1;
}

export fn SocketReadLine(vm: *api.VM) c_int {
    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
        api.Value.bz_valueToInteger(vm.bz_peek(0)),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = reader.readUntilDelimiterAlloc(api.VM.allocator, '\n', 16 * 8 * 64) catch {
        vm.bz_throwString("Could not read from socket", "Could not read from socket".len);

        return -1;
    };

    // EOF?
    if (buffer.len == 0) {
        vm.bz_pushNull();
    } else {
        vm.bz_pushString(api.ObjString.bz_string(vm, if (buffer.len > 0) @ptrCast([*]const u8, buffer) else null, buffer.len) orelse {
            vm.bz_throwString("Could not read from socket", "Could not read from socket".len);

            return -1;
        });
    }

    return 1;
}

export fn SocketWrite(vm: *api.VM) c_int {
    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
        api.Value.bz_valueToInteger(vm.bz_peek(1)),
    );

    const stream: std.net.Stream = .{ .handle = handle };

    var len: usize = 0;
    var value = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        return 0;
    }

    _ = stream.write(value.?[0..len]) catch {
        vm.bz_throwString("Could not write on socket", "Could not write on socket".len);

        return -1;
    };

    return 0;
}

export fn SocketServerStart(vm: *api.VM) c_int {
    var len: usize = 0;
    const address_value = api.Value.bz_valueToString(vm.bz_peek(2), &len);
    const address = if (len > 0) address_value.?[0..len] else "";
    const port: ?i64 = api.Value.bz_valueToInteger(vm.bz_peek(1));
    if (port == null or port.? < 0) {
        vm.bz_throwString("Port should be a positive integer", "Port should be a positive integer".len);

        return -1;
    }

    const reuse_address: bool = api.Value.bz_valueToBool(vm.bz_peek(0));

    var server = std.net.StreamServer.init(.{ .reuse_address = reuse_address });

    const list = std.net.getAddressList(api.VM.allocator, address, @intCast(u16, port.?)) catch {
        vm.bz_throwString("Could not start socket server", "Could not start socket server".len);

        return -1;
    };
    defer list.deinit();

    if (list.addrs.len == 0) {
        vm.bz_throwString("Could not start socket server", "Could not start socket server".len);

        return -1;
    }

    server.listen(list.addrs[0]) catch {
        vm.bz_throwString("Could not start socket server", "Could not start socket server".len);

        return -1;
    };

    vm.bz_pushInteger(@intCast(i64, server.sockfd.?));

    return 1;
}

export fn SocketServerAccept(vm: *api.VM) c_int {
    const server_socket: std.os.socket_t = @intCast(
        std.os.socket_t,
        api.Value.bz_valueToInteger(vm.bz_peek(1)),
    );
    const reuse_address: bool = api.Value.bz_valueToBool(vm.bz_peek(0));

    const default_options = std.net.StreamServer.Options{};
    var server = std.net.StreamServer{
        .sockfd = server_socket,
        .kernel_backlog = default_options.kernel_backlog,
        .reuse_address = reuse_address,
        .listen_address = undefined,
    };

    const connection = server.accept() catch {
        vm.bz_throwString("Could not accept a connection", "Could not accept a connection".len);

        return -1;
    };

    vm.bz_pushInteger(@intCast(i64, connection.stream.handle));

    return 1;
}
