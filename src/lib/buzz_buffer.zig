const std = @import("std");
const api = @import("./buzz_api.zig");
const native_endian = @import("builtin").target.cpu.arch.endian();

export fn BufferNew(vm: *api.VM) c_int {
    const capacity = vm.bz_peek(0).bz_valueToInteger();

    var buffer = api.VM.allocator.create(Buffer) catch {
        vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    };
    buffer.* = Buffer.init(api.VM.allocator, @intCast(usize, capacity)) catch {
        vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    };

    if (api.ObjUserData.bz_newUserData(vm, @ptrCast(*api.UserData, buffer))) |userdata| {
        vm.bz_pushUserData(userdata);

        return 1;
    } else {
        vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    }
}

export fn BufferDeinit(vm: *api.VM) c_int {
    var userdata = vm.bz_peek(0).bz_valueToUserData();

    var buffer = Buffer.fromUserData(userdata);

    buffer.deinit();
    api.VM.allocator.destroy(buffer);

    return 0;
}

const Buffer = struct {
    const Self = @This();

    pub const Error = error{WriteWhileReading};

    buffer: std.ArrayList(u8),
    cursor: usize = 0,

    pub fn fromUserData(userdata: *api.UserData) *Self {
        return @ptrCast(*Self, @alignCast(@alignOf(Self), userdata));
    }

    pub fn init(allocator: std.mem.Allocator, capacity: usize) !Self {
        var self = Self{
            .buffer = try std.ArrayList(u8).initCapacity(allocator, capacity),
        };

        if (capacity > 0) {
            try self.buffer.appendNTimes(0, capacity);
        }

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.buffer.deinit();
    }

    pub fn at(self: *Self, index: usize) u8 {
        return self.buffer.items[index];
    }

    pub fn read(self: *Self, n: usize) ?[]const u8 {
        if (self.cursor > self.buffer.items.len) {
            return null;
        }

        const read_slice = self.buffer.items[self.cursor..std.math.min(self.cursor + n, self.buffer.items.len)];

        self.cursor += read_slice.len;

        return read_slice;
    }

    pub fn write(self: *Self, bytes: []const u8) !void {
        if (self.cursor > 0) {
            return Error.WriteWhileReading;
        }

        try self.buffer.appendSlice(bytes);
    }

    pub fn setAt(self: *Self, index: usize, byte: u8) !void {
        if (self.cursor > 0) {
            return Error.WriteWhileReading;
        }

        self.buffer.items[index] = byte;
    }

    pub fn readBool(self: *Self) ?bool {
        if (self.cursor > self.buffer.items.len) {
            return null;
        }

        const value = self.buffer.items[self.cursor] == 1;

        self.cursor += 1;

        return value;
    }

    pub fn writeBool(self: *Self, value: bool) !void {
        if (self.cursor > 0) {
            return Error.WriteWhileReading;
        }

        try self.buffer.append(if (value) 1 else 0);
    }

    // Assumes we read the int/float flag
    pub fn readInteger(self: *Self) !?i64 {
        if (self.cursor > self.buffer.items.len) {
            return null;
        }

        var buffer_stream = std.io.fixedBufferStream(self.buffer.items[self.cursor..self.buffer.items.len]);
        var reader = buffer_stream.reader();

        const number = try reader.readIntNative(i64);

        self.cursor += 8;

        return number;
    }

    pub fn writeInteger(self: *Self, integer: i64) !void {
        if (self.cursor > 0) {
            return Error.WriteWhileReading;
        }

        var writer = self.buffer.writer();

        // Flag so we know it an integer
        try writer.writeIntNative(i64, integer);
    }

    // Assumes we read the int/float flag
    pub fn readFloat(self: *Self) !?f64 {
        if (self.cursor > self.buffer.items.len) {
            return null;
        }

        var buffer_stream = std.io.fixedBufferStream(self.buffer.items[self.cursor..self.buffer.items.len]);
        var reader = buffer_stream.reader();

        const number = try reader.readIntNative(u64);

        self.cursor += 8;

        return @bitCast(f64, number);
    }

    pub fn writeFloat(self: *Self, float: f64) !void {
        if (self.cursor > 0) {
            return Error.WriteWhileReading;
        }

        var writer = self.buffer.writer();

        // Flag so we know it an float
        try writer.writeIntNative(u64, @bitCast(u64, float));
    }

    pub fn empty(self: *Self) void {
        self.buffer.shrinkRetainingCapacity(0);
        self.cursor = 0;
    }
};

export fn BufferRead(vm: *api.VM) c_int {
    var buffer = Buffer.fromUserData(vm.bz_peek(1).bz_valueToUserData());
    const n = vm.bz_peek(0).bz_valueToInteger();

    const read_slice = buffer.read(@intCast(usize, n));
    if (read_slice) |uread_slice| {
        if (api.ObjString.bz_string(vm, if (uread_slice.len > 0) @ptrCast([*]const u8, uread_slice) else null, uread_slice.len)) |obj_string| {
            vm.bz_pushString(obj_string);

            return 1;
        }
    }

    vm.bz_pushNull();

    return 1;
}

export fn BufferWrite(vm: *api.VM) c_int {
    var buffer = Buffer.fromUserData(vm.bz_peek(1).bz_valueToUserData());
    var len: usize = 0;
    var bytes = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        return 0;
    }

    buffer.write(bytes.?[0..len]) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
            error.OutOfMemory => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return -1;
    };

    return 0;
}

export fn BufferSetAt(vm: *api.VM) c_int {
    var buffer = Buffer.fromUserData(vm.bz_peek(2).bz_valueToUserData());
    const index = vm.bz_peek(1).bz_valueToInteger();
    const value = vm.bz_peek(0).bz_valueToInteger();

    buffer.setAt(@intCast(usize, index), @intCast(u8, value)) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
        }

        return -1;
    };

    return 0;
}

export fn BufferReadBoolean(vm: *api.VM) c_int {
    var buffer = Buffer.fromUserData(vm.bz_peek(0).bz_valueToUserData());

    if (buffer.readBool()) |value| {
        vm.bz_pushBool(value);
    } else {
        vm.bz_pushNull();
    }

    return 1;
}

export fn BufferWriteBoolean(vm: *api.VM) c_int {
    var buffer = Buffer.fromUserData(vm.bz_peek(1).bz_valueToUserData());
    const value = vm.bz_peek(0).bz_valueToBool();

    buffer.writeBool(value) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
            error.OutOfMemory => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return -1;
    };

    return 0;
}

export fn BufferReadInt(vm: *api.VM) c_int {
    const buffer = Buffer.fromUserData(vm.bz_peek(0).bz_valueToUserData());

    if (buffer.readInteger() catch |err| {
        switch (err) {
            error.EndOfStream => {
                vm.bz_pushNull();

                return 1;
            },
        }
    }) |value| {
        vm.bz_pushInteger(value);

        return 1;
    }

    vm.bz_pushNull();
    return 1;
}

export fn BufferReadFloat(vm: *api.VM) c_int {
    const buffer = Buffer.fromUserData(vm.bz_peek(0).bz_valueToUserData());

    if (buffer.readFloat() catch |err| {
        switch (err) {
            error.EndOfStream => {
                vm.bz_pushNull();

                return 1;
            },
        }
    }) |value| {
        vm.bz_pushFloat(value);

        return 1;
    }

    vm.bz_pushNull();
    return 1;
}

export fn BufferWriteInt(vm: *api.VM) c_int {
    var buffer = Buffer.fromUserData(vm.bz_peek(1).bz_valueToUserData());
    const number = vm.bz_peek(0);

    buffer.writeInteger(number.bz_valueToInteger()) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
            error.OutOfMemory => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return -1;
    };

    return 0;
}

export fn BufferWriteFloat(vm: *api.VM) c_int {
    var buffer = Buffer.fromUserData(vm.bz_peek(1).bz_valueToUserData());
    const number = vm.bz_peek(0);

    buffer.writeFloat(number.bz_valueToFloat()) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
            error.OutOfMemory => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return -1;
    };

    return 0;
}

export fn BufferEmpty(vm: *api.VM) c_int {
    var buffer = Buffer.fromUserData(vm.bz_peek(0).bz_valueToUserData());

    buffer.empty();

    return 0;
}

export fn BufferLen(vm: *api.VM) c_int {
    const buffer = Buffer.fromUserData(vm.bz_peek(0).bz_valueToUserData());

    vm.bz_pushInteger(@intCast(i64, buffer.buffer.items.len));

    return 1;
}

export fn BufferCursor(vm: *api.VM) c_int {
    const buffer = Buffer.fromUserData(vm.bz_peek(0).bz_valueToUserData());

    vm.bz_pushInteger(@intCast(i64, buffer.cursor));

    return 1;
}

export fn BufferBuffer(vm: *api.VM) c_int {
    const buffer = Buffer.fromUserData(vm.bz_peek(0).bz_valueToUserData());

    if (api.ObjString.bz_string(vm, if (buffer.buffer.items.len > 0) @ptrCast([*]const u8, buffer.buffer.items) else null, buffer.buffer.items.len)) |objstring| {
        vm.bz_pushString(objstring);
    } else {
        vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    }

    return 1;
}

export fn BufferAt(vm: *api.VM) c_int {
    const buffer = Buffer.fromUserData(vm.bz_peek(1).bz_valueToUserData());
    const number = vm.bz_peek(0).bz_valueToInteger();

    vm.bz_pushInteger(buffer.at(@intCast(usize, number)));

    return 1;
}
