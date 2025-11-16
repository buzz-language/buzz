const std = @import("std");
const api = @import("buzz_api.zig");
const builtin = @import("builtin");
const native_endian = @import("builtin").target.cpu.arch.endian();

const is_wasm = builtin.cpu.arch.isWasm();

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

pub export fn BufferNew(ctx: *api.NativeCtx) callconv(.c) c_int {
    const capacity = api.bz_peek(ctx.vm, 0).integer();

    const buffer = api.VM.allocator.create(Buffer) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };
    buffer.* = Buffer.init(api.VM.allocator, @intCast(capacity)) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    api.bz_push(
        ctx.vm,
        api.bz_newUserData(ctx.vm, @intFromPtr(buffer)),
    );

    return 1;
}

pub export fn BufferDeinit(ctx: *api.NativeCtx) callconv(.c) c_int {
    const userdata = api.bz_getUserDataPtr(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
    );

    var buffer = Buffer.fromUserData(userdata);

    buffer.deinit();
    api.VM.allocator.destroy(buffer);

    return 0;
}

const Buffer = struct {
    const Self = @This();

    pub const Error = error{WriteWhileReading};

    buffer: std.ArrayList(u8) = .{},
    cursor: usize = 0,

    pub fn fromUserData(userdata: u64) *Self {
        return @ptrCast(
            @alignCast(
                @as(
                    *anyopaque,
                    @ptrFromInt(
                        @as(
                            usize,
                            @truncate(userdata),
                        ),
                    ),
                ),
            ),
        );
    }

    pub fn init(allocator: std.mem.Allocator, capacity: usize) !Self {
        var self = Self{};

        if (capacity > 0) {
            try self.buffer.appendNTimes(allocator, 0, capacity);
        }

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.buffer.deinit(api.VM.allocator);
    }

    pub fn at(self: *Self, index: usize) u8 {
        return self.buffer.items[index];
    }

    pub fn read(self: *Self, n: usize) ?[]const u8 {
        if (self.cursor > self.buffer.items.len) {
            return null;
        }

        const read_slice = self.buffer.items[self.cursor..@min(self.cursor + n, self.buffer.items.len)];

        self.cursor += read_slice.len;

        return read_slice;
    }

    pub fn write(self: *Self, bytes: []const u8) !void {
        if (self.cursor > 0) {
            return Error.WriteWhileReading;
        }

        try self.buffer.appendSlice(api.VM.allocator, bytes);
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

        try self.buffer.append(api.VM.allocator, if (value) 1 else 0);
    }

    pub fn readInteger(self: *Self) !?api.Integer {
        if (self.cursor > self.buffer.items.len) {
            return null;
        }

        var buffer_stream = std.Io.Reader.fixed(self.buffer.items[self.cursor..self.buffer.items.len]);

        const number = try buffer_stream.takeInt(api.Integer, builtin.cpu.arch.endian());

        self.cursor += @divExact(@typeInfo(api.Integer).int.bits, 8);

        return number;
    }

    pub fn writeInteger(self: *Self, integer: api.Integer) !void {
        if (self.cursor > 0) {
            return Error.WriteWhileReading;
        }

        var buffer = std.Io.Writer.Allocating.fromArrayList(api.VM.allocator, &self.buffer);

        // Flag so we know it an integer
        try buffer.writer.writeInt(api.Integer, integer, native_endian);

        self.buffer = buffer.toArrayList();
    }

    pub fn readUserData(self: *Self, vm: *api.VM) !?api.Value {
        if (self.cursor > self.buffer.items.len) {
            return null;
        }

        var buffer_stream = std.Io.Reader.fixed(self.buffer.items[self.cursor..self.buffer.items.len]);

        const number = try buffer_stream.takeInt(u64, builtin.cpu.arch.endian());

        self.cursor += @sizeOf(u64);

        return api.bz_newUserData(vm, number);
    }

    pub fn writeUserData(self: *Self, vm: *api.VM, userdata: api.Value) !void {
        if (self.cursor > 0) {
            return Error.WriteWhileReading;
        }

        var buffer = std.Io.Writer.Allocating.fromArrayList(api.VM.allocator, &self.buffer);

        // Flag so we know it an integer
        try buffer.writer.writeInt(
            u64,
            api.bz_getUserDataPtr(vm, userdata),
            native_endian,
        );

        self.buffer = buffer.toArrayList();
    }

    pub fn readDouble(self: *Self) !?api.Double {
        if (self.cursor > self.buffer.items.len) {
            return null;
        }

        var buffer_stream = std.Io.Reader.fixed(self.buffer.items[self.cursor..self.buffer.items.len]);

        const number = try buffer_stream.takeInt(u64, builtin.cpu.arch.endian());

        self.cursor += @divExact(@typeInfo(u64).int.bits, 8);

        return @bitCast(number);
    }

    pub fn writeFloat(self: *Self, double: api.Double) !void {
        if (self.cursor > 0) {
            return Error.WriteWhileReading;
        }

        var buffer = std.Io.Writer.Allocating.fromArrayList(api.VM.allocator, &self.buffer);

        try buffer.writer.writeInt(
            u64,
            @as(u64, @bitCast(double)),
            native_endian,
        );

        self.buffer = buffer.toArrayList();
    }

    pub fn empty(self: *Self) void {
        self.buffer.shrinkRetainingCapacity(0);
        self.cursor = 0;
    }
};

pub export fn BufferRead(ctx: *api.NativeCtx) callconv(.c) c_int {
    var buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );
    const n = api.bz_peek(ctx.vm, 0).integer();

    const read_slice = buffer.read(@intCast(n));
    if (read_slice) |uread_slice| {
        api.bz_push(
            ctx.vm,
            api.bz_stringToValue(
                ctx.vm,
                if (uread_slice.len > 0) @as([*]const u8, @ptrCast(uread_slice)) else null,
                uread_slice.len,
            ),
        );

        return 1;
    }

    api.bz_push(ctx.vm, .Null);

    return 1;
}

pub export fn BufferWrite(ctx: *api.NativeCtx) callconv(.c) c_int {
    var buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );
    var len: usize = 0;
    var bytes = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    );

    if (len == 0) {
        return 0;
    }

    buffer.write(bytes.?[0..len]) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => api.pushError(ctx.vm, "buffer.WriteWhileReadingError", null),
            error.OutOfMemory => {
                api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                unreachable;
            },
        }

        return -1;
    };

    return 0;
}

pub export fn BufferSetAt(ctx: *api.NativeCtx) callconv(.c) c_int {
    var buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 2),
        ),
    );
    const index = api.bz_peek(ctx.vm, 1).integer();
    const value = api.bz_peek(ctx.vm, 0).integer();

    buffer.setAt(@intCast(index), @intCast(value)) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => api.pushError(ctx.vm, "buffer.WriteWhileReadingError", null),
        }

        return -1;
    };

    return 0;
}

pub export fn BufferReadBoolean(ctx: *api.NativeCtx) callconv(.c) c_int {
    var buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 0),
        ),
    );

    if (buffer.readBool()) |value| {
        api.bz_push(ctx.vm, .fromBoolean(value));
    } else {
        api.bz_push(ctx.vm, .Null);
    }

    return 1;
}

pub export fn BufferWriteBoolean(ctx: *api.NativeCtx) callconv(.c) c_int {
    var buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );
    const value = api.bz_peek(ctx.vm, 0).boolean();

    buffer.writeBool(value) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => api.pushError(ctx.vm, "buffer.WriteWhileReadingError", null),
            error.OutOfMemory => {
                api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                unreachable;
            },
        }

        return -1;
    };

    return 0;
}

pub export fn BufferReadInt(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 0),
        ),
    );

    if (buffer.readInteger() catch |err| {
        switch (err) {
            error.EndOfStream => {
                api.bz_push(ctx.vm, .Null);

                return 1;
            },
            error.ReadFailed => {
                api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err));
                return -1;
            },
        }
    }) |value| {
        api.bz_push(ctx.vm, .fromInteger(value));

        return 1;
    }

    api.bz_push(ctx.vm, .Null);
    return 1;
}

pub export fn BufferReadUserData(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 0),
        ),
    );

    if (buffer.readUserData(ctx.vm) catch |err| {
        switch (err) {
            error.EndOfStream => {
                api.bz_push(ctx.vm, .Null);

                return 1;
            },
            error.ReadFailed => {
                api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err));
                return -1;
            },
        }
    }) |value| {
        api.bz_push(ctx.vm, value);

        return 1;
    }

    api.bz_push(ctx.vm, .Null);
    return 1;
}

pub export fn BufferReadDouble(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 0),
        ),
    );

    if (buffer.readDouble() catch |err| {
        switch (err) {
            error.EndOfStream => {
                api.bz_push(ctx.vm, .Null);

                return 1;
            },
            error.ReadFailed => {
                api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err));
                return -1;
            },
        }
    }) |value| {
        api.bz_push(ctx.vm, .fromDouble(value));

        return 1;
    }

    api.bz_push(ctx.vm, .Null);
    return 1;
}

pub export fn BufferWriteInt(ctx: *api.NativeCtx) callconv(.c) c_int {
    var buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );
    const number = api.bz_peek(ctx.vm, 0);

    buffer.writeInteger(number.integer()) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => api.pushError(ctx.vm, "buffer.WriteWhileReadingError", null),
            error.WriteFailed => {
                api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                unreachable;
            },
        }

        return -1;
    };

    return 0;
}

pub export fn BufferWriteUserData(ctx: *api.NativeCtx) callconv(.c) c_int {
    var buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );
    const userdata = api.bz_peek(ctx.vm, 0);

    buffer.writeUserData(ctx.vm, userdata) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => api.pushError(ctx.vm, "buffer.WriteWhileReadingError", null),
            error.WriteFailed => {
                api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                unreachable;
            },
        }

        return -1;
    };

    return 0;
}

pub export fn BufferWriteDouble(ctx: *api.NativeCtx) callconv(.c) c_int {
    var buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );
    const number = api.bz_peek(ctx.vm, 0);

    buffer.writeFloat(number.double()) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => api.pushError(ctx.vm, "buffer.WriteWhileReadingError", null),
            error.WriteFailed => {
                api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                unreachable;
            },
        }

        return -1;
    };

    return 0;
}

pub export fn BufferEmpty(ctx: *api.NativeCtx) callconv(.c) c_int {
    var buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 0),
        ),
    );

    buffer.empty();

    return 0;
}

pub export fn BufferLen(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );
    const buf_align = api.bz_peek(ctx.vm, 0).integer();

    api.bz_push(ctx.vm, .fromInteger(@intCast(buffer.buffer.items.len / @as(usize, @intCast(buf_align)))));

    return 1;
}

pub export fn BufferCursor(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 0),
        ),
    );

    api.bz_push(ctx.vm, .fromInteger(@intCast(buffer.cursor)));

    return 1;
}

pub export fn BufferBuffer(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 0),
        ),
    );

    api.bz_push(
        ctx.vm,
        api.bz_stringToValue(
            ctx.vm,
            if (buffer.buffer.items.len > 0)
                @as([*]const u8, @ptrCast(buffer.buffer.items))
            else
                null,
            buffer.buffer.items.len,
        ),
    );

    return 1;
}

pub export fn BufferPtr(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 2),
        ),
    );
    const at = api.bz_peek(ctx.vm, 1).integer();
    const alignment = api.bz_peek(ctx.vm, 0).integer();

    api.bz_push(
        ctx.vm,
        api.bz_newUserData(
            ctx.vm,
            @intFromPtr(&buffer.buffer.items.ptr[@intCast(at * alignment)]),
        ),
    );

    return 1;
}

pub export fn BufferAt(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );
    const number = api.bz_peek(ctx.vm, 0).integer();

    api.bz_push(ctx.vm, .fromInteger(buffer.at(@intCast(number))));

    return 1;
}

fn checkBuzzType(
    vm: *api.VM,
    value: api.Value,
    ztype: *api.ZigType,
    btype: api.Value,
) bool {
    if (!api.bz_valueIs(vm, value, btype).boolean()) {
        var err = std.Io.Writer.Allocating.init(api.VM.allocator);
        defer err.deinit();

        err.writer.print(
            "Expected buzz value of type `{s}` to match FFI type `{s}`",
            .{
                api.bz_valueToCString(vm, api.bz_valueCastToString(vm, btype)).?,
                api.bz_zigTypeToCString(vm, ztype),
            },
        ) catch {
            const msg = "Out of memory";
            api.bz_panic(vm, msg.ptr, msg.len);
            unreachable;
        };

        const err_owned = err.toOwnedSlice() catch {
            const msg = "Out of memory";
            api.bz_panic(vm, msg.ptr, msg.len);
            unreachable;
        };

        api.bz_pushError(
            vm,
            "ffi.FFITypeMismatchError",
            "ffi.FFITypeMismatchError".len,
            err_owned.ptr,
            err_owned.len,
        );

        return false;
    }

    return true;
}

fn rawWriteZ(
    ctx: *api.NativeCtx,
    buffer: *Buffer,
    ztype: []const u8,
    at: usize,
    values: api.Value,
) bool {
    var obj_typedef: api.Value = undefined;
    const zig_type = api.bz_zigType(
        ctx.vm,
        @ptrCast(ztype),
        ztype.len,
        &obj_typedef,
    );

    var index = at;
    for (0..api.bz_listLen(ctx.vm, values)) |i| {
        const value = api.bz_listGet(
            ctx.vm,
            values,
            @intCast(i),
            false,
        );

        if (!checkBuzzType(ctx.vm, value, zig_type.?, obj_typedef)) {
            return false;
        }

        const len = api.bz_zigTypeSize(ctx.vm, zig_type.?);

        buffer.buffer.ensureTotalCapacityPrecise(api.VM.allocator, buffer.buffer.items.len + len) catch {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        };
        buffer.buffer.expandToCapacity();

        std.debug.assert(buffer.buffer.capacity == buffer.buffer.items.len);

        api.bz_writeZigValueToBuffer(
            ctx.vm,
            value,
            zig_type.?,
            index,
            buffer.buffer.items.ptr,
            buffer.buffer.capacity,
        );

        index += len;
    }

    return true;
}

pub export fn BufferWriteZ(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 2),
        ),
    );
    var len: usize = 0;
    const ztype = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 1),
        &len,
    ).?;
    const values = api.bz_peek(ctx.vm, 0);

    return if (!rawWriteZ(
        ctx,
        buffer,
        ztype[0..len],
        buffer.buffer.items.len,
        values,
    )) -1 else 0;
}

pub export fn BufferWriteZAt(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 3),
        ),
    );
    const index = api.bz_peek(ctx.vm, 2).integer();
    var len: usize = 0;
    const ztype = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 1),
        &len,
    ).?;
    const values = api.bz_peek(ctx.vm, 0);

    return if (!rawWriteZ(
        ctx,
        buffer,
        ztype[0..len],
        @intCast(index),
        values,
    )) -1 else 0;
}

fn rawWriteStruct(
    vm: *api.VM,
    buffer: *Buffer,
    at: usize,
    type_def_value: api.Value,
    values: api.Value,
) bool {
    var index = at;
    for (0..api.bz_listLen(vm, values)) |i| {
        const value = api.bz_listGet(
            vm,
            values,
            @intCast(i),
            false,
        );

        if (!api.bz_valueIs(vm, value, type_def_value).boolean()) {
            api.bz_pushError(
                vm,
                "ffi.FFITypeMismatchError",
                "ffi.FFITypeMismatchError".len,
                null,
                0,
            );

            return false;
        }

        var len: usize = 0;
        const ptr = api.bz_foreignContainerSlice(vm, value, &len);

        buffer.buffer.ensureTotalCapacityPrecise(api.VM.allocator, buffer.buffer.items.len + len) catch {
            api.bz_panic(vm, "vm,Out of memory", "Out of memory".len);
            unreachable;
        };
        buffer.buffer.expandToCapacity();

        std.debug.assert(buffer.buffer.capacity == buffer.buffer.items.len);

        buffer.buffer.replaceRange(api.VM.allocator, index, len, ptr[0..len]) catch {
            api.bz_panic(vm, "vm,Out of memory", "Out of memory".len);
            unreachable;
        };

        index += len;
    }

    return true;
}

pub export fn BufferWriteStruct(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 2),
        ),
    );
    const type_def = api.bz_peek(ctx.vm, 1);
    const values = api.bz_peek(ctx.vm, 0);

    return if (!rawWriteStruct(
        ctx.vm,
        buffer,
        buffer.buffer.items.len,
        type_def,
        values,
    )) -1 else 0;
}

pub export fn BufferWriteStructAt(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 3),
        ),
    );
    const type_def = api.bz_peek(ctx.vm, 2);
    const index = api.bz_peek(ctx.vm, 1).integer();
    const values = api.bz_peek(ctx.vm, 0);

    return if (!rawWriteStruct(
        ctx.vm,
        buffer,
        @intCast(index),
        type_def,
        values,
    )) -1 else 0;
}

fn rawReadStruct(
    vm: *api.VM,
    buffer: *Buffer,
    at: ?usize,
    type_def: api.Value,
) api.Value {
    const size = api.bz_containerTypeSize(vm, type_def);

    const from = (at orelse buffer.cursor);
    const slice = buffer.buffer.items[from .. from + size];

    return api.bz_newForeignContainerFromSlice(vm, type_def, slice.ptr, slice.len);
}

pub export fn BufferReadStruct(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );
    const type_def = api.bz_peek(ctx.vm, 0);

    api.bz_push(
        ctx.vm,
        rawReadStruct(
            ctx.vm,
            buffer,
            null,
            type_def,
        ),
    );

    return 1;
}

pub export fn BufferReadStructAt(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 2),
        ),
    );
    const index: usize = @intCast(api.bz_peek(ctx.vm, 1).integer());
    const type_def = api.bz_peek(ctx.vm, 0);

    api.bz_push(
        ctx.vm,
        rawReadStruct(
            ctx.vm,
            buffer,
            index,
            type_def,
        ),
    );

    return 1;
}

fn rawReadZ(vm: *api.VM, buffer: *Buffer, at: ?usize, ztype: []const u8) c_int {
    var obj_typedef: api.Value = undefined;
    const zig_type = api.bz_zigType(
        vm,
        @ptrCast(ztype),
        ztype.len,
        &obj_typedef,
    );

    const len = api.bz_zigTypeSize(vm, zig_type.?);

    const value = api.bz_readZigValueFromBuffer(
        vm,
        zig_type.?,
        at orelse buffer.cursor,
        buffer.buffer.items.ptr,
        buffer.buffer.items.len,
    );

    if (!checkBuzzType(vm, value, zig_type.?, obj_typedef)) {
        return -1;
    }

    buffer.cursor += len;

    api.bz_push(vm, value);

    return 1;
}

pub export fn BufferReadZ(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );
    var len: usize = 0;
    const ztype = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    ).?;

    return rawReadZ(
        ctx.vm,
        buffer,
        null,
        ztype[0..len],
    );
}

pub export fn BufferReadZAt(ctx: *api.NativeCtx) callconv(.c) c_int {
    const buffer = Buffer.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 2),
        ),
    );
    const index: usize = @intCast(api.bz_peek(ctx.vm, 1).integer());
    var len: usize = 0;
    const ztype = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    ).?;

    return rawReadZ(
        ctx.vm,
        buffer,
        index,
        ztype[0..len],
    );
}
