const std = @import("std");
const api = @import("./buzz_api.zig");
const native_endian = @import("builtin").target.cpu.arch.endian();

export fn BufferNew_raw(ctx: *api.NativeCtx, capacity_value: api.Value) api.Value {
    const capacity = capacity_value.integer();

    var buffer = api.VM.allocator.create(Buffer) catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    };
    buffer.* = Buffer.init(api.VM.allocator, @intCast(usize, capacity)) catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    };

    if (api.ObjUserData.bz_newUserData(ctx.vm, @ptrCast(*api.UserData, buffer))) |userdata| {
        return userdata.bz_userDataToValue();
    } else {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    }
}

export fn BufferNew(ctx: *api.NativeCtx) c_int {
    const result = BufferNew_raw(ctx, ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}

export fn BufferDeinit_raw(_: *api.NativeCtx, userdata_value: api.Value) void {
    var userdata = userdata_value.bz_valueToUserData();

    var buffer = Buffer.fromUserData(userdata);

    buffer.deinit();
    api.VM.allocator.destroy(buffer);
}

export fn BufferDeinit(ctx: *api.NativeCtx) c_int {
    BufferDeinit_raw(ctx, ctx.vm.bz_peek(0));

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

        self.cursor += 8;

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

export fn BufferRead_raw(ctx: *api.NativeCtx, buffer_value: api.Value, n_value: api.Value) api.Value {
    var buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());
    const n = n_value.integer();

    const read_slice = buffer.read(@intCast(usize, n));
    if (read_slice) |uread_slice| {
        if (api.ObjString.bz_string(ctx.vm, if (uread_slice.len > 0) @ptrCast([*]const u8, uread_slice) else null, uread_slice.len)) |obj_string| {
            return obj_string.bz_objStringToValue();
        }
    }

    return api.Value.Null;
}

export fn BufferRead(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(
        BufferRead_raw(
            ctx,
            ctx.vm.bz_peek(0),
            ctx.vm.bz_peek(1),
        ),
    );

    return 1;
}

export fn BufferWrite_raw(ctx: *api.NativeCtx, buffer_value: api.Value, bytes_value: api.Value) api.Value {
    const buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());
    var len: usize = 0;
    const bytes = bytes_value.bz_valueToString(&len);

    if (len == 0) {
        return api.Value.Void;
    }

    buffer.write(bytes.?[0..len]) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
            error.OutOfMemory => ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return api.Value.Error;
    };

    return api.Value.Void;
}

export fn BufferWrite(ctx: *api.NativeCtx) c_int {
    const result = BufferWrite_raw(
        ctx,
        ctx.vm.bz_peek(1),
        ctx.vm.bz_peek(0),
    );

    if (result.isError()) {
        return -1;
    }

    return 0;
}

export fn BufferSetAt_raw(ctx: *api.NativeCtx, buffer_value: api.Value, index_value: api.Value, value_value: api.Value) api.Value {
    var buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());
    const index = index_value.integer();
    const value = value_value.integer();

    buffer.setAt(
        @intCast(usize, index),
        @intCast(u8, value),
    ) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
        }

        return api.Value.Error;
    };

    return api.Value.Void;
}

export fn BufferSetAt(ctx: *api.NativeCtx) c_int {
    const result = BufferSetAt_raw(
        ctx,
        ctx.vm.bz_peek(2),
        ctx.vm.bz_peek(1),
        ctx.vm.bz_peek(0),
    );

    if (result.isError()) {
        return -1;
    }

    return 0;
}

export fn BufferReadBoolean_raw(_: *api.NativeCtx, buffer_value: api.Value) api.Value {
    const buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());

    if (buffer.readBool()) |value| {
        return api.Value.fromBoolean(value);
    }

    return api.Value.Null;
}

export fn BufferReadBoolean(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(BufferReadBoolean_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn BufferWriteBoolean_raw(ctx: *api.NativeCtx, buffer_value: api.Value, value_value: api.Value) api.Value {
    var buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());
    const value = value_value.boolean();

    buffer.writeBool(value) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
            error.OutOfMemory => ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return api.Value.Error;
    };

    return api.Value.Void;
}

export fn BufferWriteBoolean(ctx: *api.NativeCtx) c_int {
    const result = BufferWriteBoolean_raw(ctx, ctx.vm.bz_peek(1), ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    return 0;
}

export fn BufferReadInt_raw(_: *api.NativeCtx, buffer_value: api.Value) api.Value {
    const buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());

    if (buffer.readInteger() catch |err| {
        switch (err) {
            error.EndOfStream => {
                return api.Value.Null;
            },
        }
    }) |value| {
        return api.Value.fromInteger(value);
    }

    return api.Value.Null;
}

export fn BufferReadInt(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(BufferReadInt_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn BufferReadFloat_raw(_: *api.NativeCtx, buffer_value: api.Value) api.Value {
    const buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());

    if (buffer.readFloat() catch |err| {
        switch (err) {
            error.EndOfStream => {
                return api.Value.Null;
            },
        }
    }) |value| {
        return api.Value.fromFloat(value);
    }

    return api.Value.Null;
}

export fn BufferReadFloat(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(BufferReadFloat_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn BufferWriteInt_raw(ctx: *api.NativeCtx, buffer_value: api.Value, number: api.Value) api.Value {
    const buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());

    buffer.writeInteger(number.integer()) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
            error.OutOfMemory => ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return api.Value.Error;
    };

    return api.Value.Void;
}

export fn BufferWriteInt(ctx: *api.NativeCtx) c_int {
    const result = BufferWriteInt_raw(
        ctx,
        ctx.vm.bz_peek(1),
        ctx.vm.bz_peek(0),
    );

    if (result.isError()) {
        return -1;
    }

    return 0;
}

export fn BufferWriteFloat_raw(ctx: *api.NativeCtx, buffer_value: api.Value, number: api.Value) api.Value {
    var buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());

    buffer.writeFloat(number.float()) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.bz_pushError("lib.buffer.WriteWhileReadingError", "lib.buffer.WriteWhileReadingError".len),
            error.OutOfMemory => ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return api.Value.Error;
    };

    return api.Value.Void;
}

export fn BufferWriteFloat(ctx: *api.NativeCtx) c_int {
    const result = BufferWriteFloat_raw(
        ctx,
        ctx.vm.bz_peek(1),
        ctx.vm.bz_peek(0),
    );

    if (result.isError()) {
        return -1;
    }

    return 0;
}

export fn BufferEmpty_raw(_: *api.NativeCtx, buffer_value: api.Value) void {
    var buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());

    buffer.empty();
}

export fn BufferEmpty(ctx: *api.NativeCtx) c_int {
    BufferEmpty_raw(ctx, ctx.vm.bz_peek(0));

    return 0;
}

export fn BufferLen_raw(_: *api.NativeCtx, buffer_value: api.Value) api.Value {
    const buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());

    return api.Value.fromInteger(@intCast(i32, buffer.buffer.items.len));
}

export fn BufferLen(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(BufferLen_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn BufferCursor_raw(_: *api.NativeCtx, buffer_value: api.Value) api.Value {
    const buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());

    return api.Value.fromInteger(@intCast(i32, buffer.cursor));
}

export fn BufferCursor(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(BufferCursor_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn BufferBuffer_raw(ctx: *api.NativeCtx, buffer_value: api.Value) api.Value {
    const buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());

    if (api.ObjString.bz_string(ctx.vm, if (buffer.buffer.items.len > 0) @ptrCast([*]const u8, buffer.buffer.items) else null, buffer.buffer.items.len)) |objstring| {
        return objstring.bz_objStringToValue();
    } else {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    }
}

export fn BufferBuffer(ctx: *api.NativeCtx) c_int {
    const result = BufferBuffer_raw(ctx, ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}

export fn BufferAt_raw(_: *api.NativeCtx, buffer_value: api.Value, number_value: api.Value) api.Value {
    const buffer = Buffer.fromUserData(buffer_value.bz_valueToUserData());
    const number = number_value.integer();

    return api.Value.fromInteger(buffer.at(@intCast(usize, number)));
}

export fn BufferAt(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(
        BufferAt_raw(
            ctx,
            ctx.vm.bz_peek(1),
            ctx.vm.bz_peek(0),
        ),
    );

    return 1;
}
