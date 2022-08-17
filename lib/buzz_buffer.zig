const std = @import("std");
const api = @import("./buzz_api.zig");
const utils = @import("../src/utils.zig");
const native_endian = @import("builtin").target.cpu.arch.endian();

export fn BufferWriteNumber(vm: *api.VM) c_int {
    const number = vm.bz_peek(0);
    const number_integer = if (api.Value.bz_valueIsInteger(number)) api.Value.bz_valueToInteger(number) else null;
    const number_float = if (api.Value.bz_valueIsFloat(number)) api.Value.bz_valueToFloat(number) else null;

    var buffer = std.ArrayList(u8).init(api.VM.allocator);
    defer buffer.deinit();

    var writer = buffer.writer();

    if (number_integer) |integer| {
        // Flag so we know it an integer
        writer.writeByte(1) catch {
            vm.bz_throwString("Could not write number");

            return -1;
        };
        writer.writeIntNative(i64, integer) catch {
            vm.bz_throwString("Could not write number");

            return -1;
        };
    } else {
        // Flag so we know it an integer
        writer.writeByte(2) catch {
            vm.bz_throwString("Could not write number");

            return -1;
        };
        writer.writeIntNative(u64, @bitCast(u64, number_float.?)) catch {
            vm.bz_throwString("Could not write number");

            return -1;
        };
    }

    vm.bz_pushString(
        api.ObjString.bz_string(vm, utils.toCString(api.VM.allocator, buffer.items) orelse {
            vm.bz_throwString("Could not write number");

            return -1;
        }) orelse {
            vm.bz_throwString("Could not write number");

            return -1;
        },
    );

    return 1;
}

export fn BufferReadNumber(vm: *api.VM) c_int {
    const string = std.mem.sliceTo(vm.bz_peek(0).bz_valueToString().?, 0);

    var buffer = std.ArrayList(u8).init(api.VM.allocator);
    buffer.appendSlice(string) catch {
        vm.bz_throwString("Could not read number");

        return -1;
    };
    buffer.appendNTimes(0, 9 - std.math.min(8, string.len)) catch {
        vm.bz_throwString("Could not read number");

        return -1;
    };
    defer buffer.deinit();

    var reader = std.io.fixedBufferStream(buffer.items).reader();

    const flag = reader.readByte() catch |err| {
        if (err == error.EndOfStream) {
            vm.bz_pushNull();

            return 1;
        }

        vm.bz_throwString("Could not read number");

        return -1;
    };

    if (flag == 1) {
        const number = reader.readIntNative(i64) catch |err| {
            if (err == error.EndOfStream) {
                vm.bz_pushNull();

                return 1;
            }

            vm.bz_throwString("Could not read number");

            return -1;
        };

        vm.bz_pushInteger(number);
    } else {
        const number = reader.readIntNative(u64) catch |err| {
            if (err == error.EndOfStream) {
                vm.bz_pushNull();

                return 1;
            }

            vm.bz_throwString("Could not read number");

            return -1;
        };

        vm.bz_pushFloat(@bitCast(f64, number));
    }

    return 1;
}
