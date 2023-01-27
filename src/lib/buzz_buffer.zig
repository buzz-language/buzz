const std = @import("std");
const api = @import("./buzz_api.zig");
const native_endian = @import("builtin").target.cpu.arch.endian();

export fn BufferNew(ctx: *api.NativeCtx) c_int {
    const capacity = ctx.vm.bz_peek(0).integer();

    var buffer = api.VM.allocator.create(Buffer) catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    };
    buffer.* = Buffer.init(api.VM.allocator, @intCast(usize, capacity)) catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    };

    if (api.ObjUserData.bz_newUserData(ctx.vm, @ptrCast(*api.UserData, buffer))) |userdata| {
        ctx.vm.bz_pushUserData(userdata);

        return 1;
    } else {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    }
}

export fn BufferDeinit(ctx: *api.NativeCtx) c_int {
    var userdata = ctx.vm.bz_peek(0).bz_valueToUserData();

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
    pub fn readInteger(self: *Self) !?i32 {
        if (self.cursor > self.buffer.items.len) {
            return null;
        }

        var buffer_stream = std.io.fixedBufferStream(self.buffer.items[self.cursor..self.buffer.items.len]);
        var reader = buffer_stream.reader();

        const number = try reader.readIntNative(i32);

        self.cursor += @sizeOf(i32);

        return number;
    }

    pub fn writeInteger(self: *Self, integer: i32) !void {
        if (self.cursor > 0) {
            return Error.WriteWhileReading;
        }

        var writer = self.buffer.writer();

        // Flag so we know it an integer
        try writer.writeIntNative(i32, integer);
    }

    // Assumes we read the int/float flag
    pub fn readFloat(self: *Self) !?f64 {
        if (self.cursor > self.buffer.items.len) {
            return null;
        }

        var buffer_stream = std.io.fixedBufferStream(self.buffer.items[self.cursor..self.buffer.items.len]);
        var reader = buffer_stream.reader();

        const number = try reader.readIntNative(u64);

        self.cursor += @sizeOf(f64);

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

export fn BufferRead(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_valueToUserData());
    const n = ctx.vm.bz_peek(0).integer();

    const read_slice = buffer.read(@intCast(usize, n));
    if (read_slice) |uread_slice| {
        if (api.ObjString.bz_string(ctx.vm, if (uread_slice.len > 0) @ptrCast([*]const u8, uread_slice) else null, uread_slice.len)) |obj_string| {
            ctx.vm.bz_pushString(obj_string);

            return 1;
        }
    }

    ctx.vm.bz_pushNull();

    return 1;
}

export fn BufferWrite(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_valueToUserData());
    var len: usize = 0;
    var bytes = ctx.vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        return 0;
    }

    buffer.write(bytes.?[0..len]) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
            error.OutOfMemory => ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return -1;
    };

    return 0;
}

export fn BufferSetAt(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(2).bz_valueToUserData());
    const index = ctx.vm.bz_peek(1).integer();
    const value = ctx.vm.bz_peek(0).integer();

    buffer.setAt(@intCast(usize, index), @intCast(u8, value)) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
        }

        return -1;
    };

    return 0;
}

export fn BufferReadBoolean(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_valueToUserData());

    if (buffer.readBool()) |value| {
        ctx.vm.bz_pushBool(value);
    } else {
        ctx.vm.bz_pushNull();
    }

    return 1;
}

export fn BufferWriteBoolean(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_valueToUserData());
    const value = ctx.vm.bz_peek(0).boolean();

    buffer.writeBool(value) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
            error.OutOfMemory => ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return -1;
    };

    return 0;
}

export fn BufferReadInt(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_valueToUserData());

    if (buffer.readInteger() catch |err| {
        switch (err) {
            error.EndOfStream => {
                ctx.vm.bz_pushNull();

                return 1;
            },
        }
    }) |value| {
        ctx.vm.bz_pushInteger(value);

        return 1;
    }

    ctx.vm.bz_pushNull();
    return 1;
}

export fn BufferReadFloat(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_valueToUserData());

    if (buffer.readFloat() catch |err| {
        switch (err) {
            error.EndOfStream => {
                ctx.vm.bz_pushNull();

                return 1;
            },
        }
    }) |value| {
        ctx.vm.bz_pushFloat(value);

        return 1;
    }

    ctx.vm.bz_pushNull();
    return 1;
}

export fn BufferWriteInt(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_valueToUserData());
    const number = ctx.vm.bz_peek(0);

    buffer.writeInteger(number.integer()) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
            error.OutOfMemory => ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return -1;
    };

    return 0;
}

export fn BufferWriteFloat(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_valueToUserData());
    const number = ctx.vm.bz_peek(0);

    buffer.writeFloat(number.float()) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
            error.OutOfMemory => ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return -1;
    };

    return 0;
}

export fn BufferEmpty(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_valueToUserData());

    buffer.empty();

    return 0;
}

export fn BufferLen(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_valueToUserData());

    ctx.vm.bz_pushInteger(@intCast(i32, buffer.buffer.items.len));

    return 1;
}

export fn BufferCursor(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_valueToUserData());

    ctx.vm.bz_pushInteger(@intCast(i32, buffer.cursor));

    return 1;
}

export fn BufferBuffer(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_valueToUserData());

    if (api.ObjString.bz_string(ctx.vm, if (buffer.buffer.items.len > 0) @ptrCast([*]const u8, buffer.buffer.items) else null, buffer.buffer.items.len)) |objstring| {
        ctx.vm.bz_pushString(objstring);
    } else {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    }

    return 1;
}

export fn BufferAt(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_valueToUserData());
    const number = ctx.vm.bz_peek(0).integer();

    ctx.vm.bz_pushInteger(buffer.at(@intCast(usize, number)));

    return 1;
}
