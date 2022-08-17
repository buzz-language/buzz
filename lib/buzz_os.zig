const std = @import("std");
const api = @import("./buzz_api.zig");
const utils = @import("../src/utils.zig");
const builtin = @import("builtin");

export fn time(vm: *api.VM) c_int {
    vm.bz_pushNum(@intToFloat(f64, std.time.milliTimestamp()));

    return 1;
}

export fn env(vm: *api.VM) c_int {
    const key = api.Value.bz_valueToString(vm.bz_peek(0)) orelse "";

    if (std.os.getenvZ(key)) |value| {
        vm.bz_pushString(api.ObjString.bz_string(vm, utils.toCString(api.VM.allocator, value) orelse {
            vm.bz_throwString("Could not get environment variable");

            return -1;
        }) orelse {
            vm.bz_throwString("Could not get environment variable");

            return -1;
        });

        return 1;
    }

    return 0;
}

fn sysTempDir() []const u8 {
    return switch (builtin.os.tag) {
        .windows => unreachable, // TODO: GetTempPath
        else => std.os.getenv("TMPDIR") orelse std.os.getenv("TMP") orelse std.os.getenv("TEMP") orelse std.os.getenv("TEMPDIR") orelse "/tmp",
    };
}

export fn tmpDir(vm: *api.VM) c_int {
    const tmp_dir: []const u8 = sysTempDir();

    vm.bz_pushString(api.ObjString.bz_string(vm, utils.toCString(api.VM.allocator, tmp_dir) orelse {
        vm.bz_throwString("Could not get tmp dir");

        return -1;
    }) orelse {
        vm.bz_throwString("Could not get tmp dir");

        return -1;
    });

    return 1;
}

// TODO: what if file with same random name exists already?
export fn tmpFilename(vm: *api.VM) c_int {
    const prefix: ?[*:0]const u8 = api.Value.bz_valueToString(vm.bz_peek(0));

    var random_part = std.ArrayList(u8).init(api.VM.allocator);
    defer random_part.deinit();
    random_part.writer().print("{x}", .{std.crypto.random.int(i64)}) catch {
        vm.bz_throwString("Could not get tmp file");

        return -1;
    };

    var random_part_b64 = std.ArrayList(u8).initCapacity(api.VM.allocator, std.base64.standard.Encoder.calcSize(random_part.items.len)) catch {
        vm.bz_throwString("Could not get tmp file");

        return -1;
    };
    random_part_b64.expandToCapacity();
    defer random_part_b64.deinit();

    _ = std.base64.standard.Encoder.encode(random_part_b64.items, random_part.items);

    var final = std.ArrayList(u8).init(api.VM.allocator);
    defer final.deinit();

    // TODO: take into account system file separator (windows is \)
    if (prefix) |uprefix| {
        final.writer().print("{s}{s}-{s}", .{ sysTempDir(), uprefix, random_part_b64.items }) catch {
            vm.bz_throwString("Could not get tmp file");

            return -1;
        };
    } else {
        final.writer().print("{s}{s}", .{ sysTempDir(), random_part_b64.items }) catch {
            vm.bz_throwString("Could not get tmp file");

            return -1;
        };
    }

    vm.bz_pushString(api.ObjString.bz_string(vm, utils.toCString(api.VM.allocator, final.items) orelse {
        vm.bz_throwString("Could not get tmp file");

        return -1;
    }) orelse {
        vm.bz_throwString("Could not get tmp file");

        return -1;
    });

    return 1;
}

// If it was named `exit` it would be considered by zig as a callback when std.os.exit is called
export fn buzzExit(vm: *api.VM) c_int {
    const exitCode: u8 = @floatToInt(u8, api.Value.bz_valueToNumber(vm.bz_peek(0)));

    std.os.exit(exitCode);

    return 0;
}

export fn execute(vm: *api.VM) c_int {
    // const command: []const u8 = std.mem.sliceTo(api.Value.bz_valueToString(vm.bz_peek(0)) orelse "", 0);
    var command = std.ArrayList([]const u8).init(api.VM.allocator);
    defer command.deinit();

    const argv = api.ObjList.bz_valueToList(vm.bz_peek(0));
    const len = argv.bz_listLen();
    var i: usize = 0;
    while (i < len) : (i += 1) {
        command.append(utils.toSlice(api.Value.bz_valueToString(argv.bz_listGet(i)) orelse "")) catch {
            vm.bz_throwString("Could not execute");

            return -1;
        };
    }

    var child_process = std.ChildProcess.init(command.items, api.VM.allocator);
    child_process.disable_aslr = true;

    child_process.spawn() catch {
        vm.bz_throwString("Could not execute");

        return -1;
    };

    vm.bz_pushNum(@intToFloat(f64, (child_process.wait() catch {
        vm.bz_throwString("Could not execute");

        return -1;
    }).Exited));

    return 1;
}

export fn SocketConnect(vm: *api.VM) c_int {
    const address: [*:0]const u8 = api.Value.bz_valueToString(vm.bz_peek(2)) orelse "";
    const port: u16 = @floatToInt(u16, api.Value.bz_valueToNumber(vm.bz_peek(1)));
    const protocol: u8 = @floatToInt(u8, api.Value.bz_valueToNumber(vm.bz_peek(0)));

    switch (protocol) {
        0 => {
            const stream = std.net.tcpConnectToHost(api.VM.allocator, utils.toSlice(address), port) catch {
                vm.bz_throwString("Could not connect");

                return -1;
            };

            vm.bz_pushNum(@intToFloat(f64, stream.handle));

            return 1;
        },
        1, // TODO: UDP
        2, // TODO: IPC
        => {
            vm.bz_throwString("Not yet implemented");

            return -1;
        },
        else => {
            vm.bz_throwString("Unsupported protocol");

            return -1;
        },
    }
}

export fn SocketClose(vm: *api.VM) c_int {
    const socket: std.os.socket_t = @floatToInt(
        std.os.socket_t,
        api.Value.bz_valueToNumber(vm.bz_peek(0)),
    );

    std.os.closeSocket(socket);

    return 0;
}

export fn SocketRead(vm: *api.VM) c_int {
    const n: u64 = @floatToInt(u64, api.Value.bz_valueToNumber(vm.bz_peek(0)));
    const handle: std.os.socket_t = @floatToInt(
        std.os.socket_t,
        api.Value.bz_valueToNumber(vm.bz_peek(1)),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = api.VM.allocator.alloc(u8, n) catch {
        vm.bz_throwString("Could not read from socket");

        return -1;
    };

    // bz_string will copy it
    defer api.VM.allocator.free(buffer);

    const read = reader.readAll(buffer) catch {
        vm.bz_throwString("Could not read from socket");

        return -1;
    };

    if (read == 0) {
        vm.bz_pushNull();
    } else {
        vm.bz_pushString(api.ObjString.bz_string(vm, utils.toCString(api.VM.allocator, buffer[0..read]) orelse {
            vm.bz_throwString("Could not read from socket");

            return -1;
        }) orelse {
            vm.bz_throwString("Could not read from socket");

            return -1;
        });
    }

    return 1;
}

export fn SocketReadLine(vm: *api.VM) c_int {
    const handle: std.os.socket_t = @floatToInt(
        std.os.socket_t,
        api.Value.bz_valueToNumber(vm.bz_peek(0)),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = reader.readUntilDelimiterAlloc(api.VM.allocator, '\n', 16 * 8 * 64) catch {
        vm.bz_throwString("Could not read from socket");

        return -1;
    };

    // EOF?
    if (buffer.len == 0) {
        vm.bz_pushNull();
    } else {
        vm.bz_pushString(api.ObjString.bz_string(vm, utils.toCString(api.VM.allocator, buffer) orelse {
            vm.bz_throwString("Could not read from socket");

            return -1;
        }) orelse {
            vm.bz_throwString("Could not read from socket");

            return -1;
        });
    }

    return 1;
}

export fn SocketWrite(vm: *api.VM) c_int {
    const handle: std.os.socket_t = @floatToInt(
        std.os.socket_t,
        api.Value.bz_valueToNumber(vm.bz_peek(1)),
    );

    const stream: std.net.Stream = .{ .handle = handle };

    _ = stream.write(std.mem.sliceTo(vm.bz_peek(0).bz_valueToString().?, 0)) catch {
        vm.bz_throwString("Could not write on socket");

        return -1;
    };

    return 0;
}

export fn SocketServerStart(vm: *api.VM) c_int {
    const address: [*:0]const u8 = api.Value.bz_valueToString(vm.bz_peek(2)) orelse "";
    const port: u16 = @floatToInt(u16, api.Value.bz_valueToNumber(vm.bz_peek(1)));
    const reuse_address: bool = api.Value.bz_valueToBool(vm.bz_peek(0));

    var server = std.net.StreamServer.init(.{ .reuse_address = reuse_address });

    const list = std.net.getAddressList(api.VM.allocator, utils.toSlice(address), port) catch {
        vm.bz_throwString("Could not start socket server");

        return -1;
    };
    defer list.deinit();

    if (list.addrs.len == 0) {
        vm.bz_throwString("Could not start socket server");

        return -1;
    }

    server.listen(list.addrs[0]) catch {
        vm.bz_throwString("Could not start socket server");

        return -1;
    };

    vm.bz_pushNum(@intToFloat(f64, server.sockfd.?));

    return 1;
}

export fn SocketServerAccept(vm: *api.VM) c_int {
    const server_socket: std.os.socket_t = @floatToInt(
        std.os.socket_t,
        api.Value.bz_valueToNumber(vm.bz_peek(1)),
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
        vm.bz_throwString("Could not accept a connection");

        return -1;
    };

    vm.bz_pushNum(@intToFloat(f64, connection.stream.handle));

    return 1;
}
