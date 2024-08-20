const std = @import("std");
const api = @import("buzz_api.zig");
const builtin = @import("builtin");
const native_endian = @import("builtin").target.cpu.arch.endian();

const is_wasm = builtin.cpu.arch.isWasm();

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

pub export fn BufferNew(ctx: *api.NativeCtx) c_int {
    const capacity = ctx.vm.bz_peek(0).integer();

    const buffer = api.VM.allocator.create(Buffer) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };
    buffer.* = Buffer.init(api.VM.allocator, @intCast(capacity)) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    ctx.vm.bz_push(
        api.VM.bz_newUserData(
            ctx.vm,
            @intFromPtr(buffer),
        ),
    );

    return 1;
}

pub export fn BufferDeinit(ctx: *api.NativeCtx) c_int {
    const userdata = ctx.vm.bz_peek(0).bz_getUserDataPtr();

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

        const read_slice = self.buffer.items[self.cursor..@min(self.cursor + n, self.buffer.items.len)];

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

    pub fn readInteger(self: *Self) !?i32 {
        if (self.cursor > self.buffer.items.len) {
            return null;
        }

        var buffer_stream = std.io.fixedBufferStream(self.buffer.items[self.cursor..self.buffer.items.len]);
        var reader = buffer_stream.reader();

        const number = try reader.readInt(i32, builtin.cpu.arch.endian());

        self.cursor += @sizeOf(i32);

        return number;
    }

    pub fn writeInteger(self: *Self, integer: i32) !void {
        if (self.cursor > 0) {
            return Error.WriteWhileReading;
        }

        var writer = self.buffer.writer();

        // Flag so we know it an integer
        try writer.writeInt(i32, integer, native_endian);
    }

    pub fn readUserData(self: *Self, vm: *api.VM) !?api.Value {
        if (self.cursor > self.buffer.items.len) {
            return null;
        }

        var buffer_stream = std.io.fixedBufferStream(self.buffer.items[self.cursor..self.buffer.items.len]);
        var reader = buffer_stream.reader();

        const number = try reader.readInt(u64, builtin.cpu.arch.endian());

        self.cursor += @sizeOf(u64);

        return vm.bz_newUserData(number);
    }

    pub fn writeUserData(self: *Self, userdata: api.Value) !void {
        if (self.cursor > 0) {
            return Error.WriteWhileReading;
        }

        var writer = self.buffer.writer();

        // Flag so we know it an integer
        try writer.writeInt(
            u64,
            userdata.bz_getUserDataPtr(),
            native_endian,
        );
    }

    pub fn readFloat(self: *Self) !?f64 {
        if (self.cursor > self.buffer.items.len) {
            return null;
        }

        var buffer_stream = std.io.fixedBufferStream(self.buffer.items[self.cursor..self.buffer.items.len]);
        var reader = buffer_stream.reader();

        const number = try reader.readInt(u64, builtin.cpu.arch.endian());

        self.cursor += @sizeOf(f64);

        return @bitCast(number);
    }

    pub fn writeFloat(self: *Self, float: f64) !void {
        if (self.cursor > 0) {
            return Error.WriteWhileReading;
        }

        var writer = self.buffer.writer();

        // Flag so we know it an float
        try writer.writeInt(
            u64,
            @as(u64, @bitCast(float)),
            native_endian,
        );
    }

    pub fn empty(self: *Self) void {
        self.buffer.shrinkRetainingCapacity(0);
        self.cursor = 0;
    }
};

pub export fn BufferRead(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());
    const n = ctx.vm.bz_peek(0).integer();

    const read_slice = buffer.read(@intCast(n));
    if (read_slice) |uread_slice| {
        ctx.vm.bz_push(
            api.VM.bz_stringToValue(
                ctx.vm,
                if (uread_slice.len > 0) @as([*]const u8, @ptrCast(uread_slice)) else null,
                uread_slice.len,
            ),
        );

        return 1;
    }

    ctx.vm.bz_push(api.Value.Null);

    return 1;
}

pub export fn BufferWrite(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());
    var len: usize = 0;
    var bytes = ctx.vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        return 0;
    }

    buffer.write(bytes.?[0..len]) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.pushError("buffer.WriteWhileReadingError", null),
            error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        }

        return -1;
    };

    return 0;
}

pub export fn BufferSetAt(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(2).bz_getUserDataPtr());
    const index = ctx.vm.bz_peek(1).integer();
    const value = ctx.vm.bz_peek(0).integer();

    buffer.setAt(@intCast(index), @intCast(value)) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.pushError("buffer.WriteWhileReadingError", null),
        }

        return -1;
    };

    return 0;
}

pub export fn BufferReadBoolean(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_getUserDataPtr());

    if (buffer.readBool()) |value| {
        ctx.vm.bz_push(api.Value.fromBoolean(value));
    } else {
        ctx.vm.bz_push(api.Value.Null);
    }

    return 1;
}

pub export fn BufferWriteBoolean(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());
    const value = ctx.vm.bz_peek(0).boolean();

    buffer.writeBool(value) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.pushError("buffer.WriteWhileReadingError", null),
            error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        }

        return -1;
    };

    return 0;
}

pub export fn BufferReadInt(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_getUserDataPtr());

    if (buffer.readInteger() catch |err| {
        switch (err) {
            error.EndOfStream => {
                ctx.vm.bz_push(api.Value.Null);

                return 1;
            },
        }
    }) |value| {
        ctx.vm.bz_push(api.Value.fromInteger(value));

        return 1;
    }

    ctx.vm.bz_push(api.Value.Null);
    return 1;
}

pub export fn BufferReadUserData(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_getUserDataPtr());

    if (buffer.readUserData(ctx.vm) catch |err| {
        switch (err) {
            error.EndOfStream => {
                ctx.vm.bz_push(api.Value.Null);

                return 1;
            },
        }
    }) |value| {
        ctx.vm.bz_push(value);

        return 1;
    }

    ctx.vm.bz_push(api.Value.Null);
    return 1;
}

pub export fn BufferReadFloat(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_getUserDataPtr());

    if (buffer.readFloat() catch |err| {
        switch (err) {
            error.EndOfStream => {
                ctx.vm.bz_push(api.Value.Null);

                return 1;
            },
        }
    }) |value| {
        ctx.vm.bz_push(api.Value.fromFloat(value));

        return 1;
    }

    ctx.vm.bz_push(api.Value.Null);
    return 1;
}

pub export fn BufferWriteInt(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());
    const number = ctx.vm.bz_peek(0);

    buffer.writeInteger(number.integer()) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.pushError("buffer.WriteWhileReadingError", null),
            error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        }

        return -1;
    };

    return 0;
}

pub export fn BufferWriteUserData(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());
    const userdata = ctx.vm.bz_peek(0);

    buffer.writeUserData(userdata) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.pushError("buffer.WriteWhileReadingError", null),
            error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        }

        return -1;
    };

    return 0;
}

pub export fn BufferWriteFloat(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());
    const number = ctx.vm.bz_peek(0);

    buffer.writeFloat(number.float()) catch |err| {
        switch (err) {
            Buffer.Error.WriteWhileReading => ctx.vm.pushError("buffer.WriteWhileReadingError", null),
            error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        }

        return -1;
    };

    return 0;
}

pub export fn BufferEmpty(ctx: *api.NativeCtx) c_int {
    var buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_getUserDataPtr());

    buffer.empty();

    return 0;
}

pub export fn BufferLen(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());
    const buf_align = ctx.vm.bz_peek(0).integer();

    ctx.vm.bz_push(api.Value.fromInteger(@intCast(buffer.buffer.items.len / @as(usize, @intCast(buf_align)))));

    return 1;
}

pub export fn BufferCursor(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_getUserDataPtr());

    ctx.vm.bz_push(api.Value.fromInteger(@intCast(buffer.cursor)));

    return 1;
}

pub export fn BufferBuffer(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(0).bz_getUserDataPtr());

    ctx.vm.bz_push(
        ctx.vm.bz_stringToValue(
            if (buffer.buffer.items.len > 0)
                @as([*]const u8, @ptrCast(buffer.buffer.items))
            else
                null,
            buffer.buffer.items.len,
        ),
    );

    return 1;
}

pub export fn BufferPtr(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(2).bz_getUserDataPtr());
    const at = ctx.vm.bz_peek(1).integer();
    const alignment = ctx.vm.bz_peek(0).integer();

    ctx.vm.bz_push(
        ctx.vm.bz_newUserData(
            @intFromPtr(&buffer.buffer.items.ptr[@intCast(at * alignment)]),
        ),
    );

    return 1;
}

pub export fn BufferAt(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());
    const number = ctx.vm.bz_peek(0).integer();

    ctx.vm.bz_push(api.Value.fromInteger(buffer.at(@intCast(number))));

    return 1;
}

fn checkBuzzType(
    vm: *api.VM,
    value: api.Value,
    ztype: *api.ZigType,
    btype: api.Value,
) bool {
    if (!value.bz_valueIs(btype).boolean()) {
        var err = std.ArrayList(u8).init(api.VM.allocator);
        defer err.deinit();

        err.writer().print(
            "Expected buzz value of type `{s}` to match FFI type `{s}`",
            .{
                btype.bz_valueCastToString(vm).bz_valueToCString().?,
                ztype.bz_zigTypeToCString(vm),
            },
        ) catch {
            const msg = "Out of memory";
            vm.bz_panic(msg.ptr, msg.len);
            unreachable;
        };

        vm.bz_pushError(
            "ffi.FFITypeMismatchError",
            "ffi.FFITypeMismatchError".len,
            err.items.ptr,
            err.items.len,
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
    const zig_type = ctx.vm.bz_zigType(
        @ptrCast(ztype),
        ztype.len,
        &obj_typedef,
    );

    var index = at;
    for (0..values.bz_listLen()) |i| {
        const value = values.bz_listGet(
            @intCast(i),
            false,
        );

        if (!checkBuzzType(ctx.vm, value, zig_type.?, obj_typedef)) {
            return false;
        }

        const len = zig_type.?.bz_zigTypeSize();

        buffer.buffer.ensureTotalCapacityPrecise(buffer.buffer.items.len + len) catch {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        };
        buffer.buffer.expandToCapacity();

        std.debug.assert(buffer.buffer.capacity == buffer.buffer.items.len);

        ctx.vm.bz_writeZigValueToBuffer(
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

pub export fn BufferWriteZ(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(2).bz_getUserDataPtr());
    var len: usize = 0;
    const ztype = ctx.vm.bz_peek(1).bz_valueToString(&len).?;
    const values = ctx.vm.bz_peek(0);

    return if (!rawWriteZ(
        ctx,
        buffer,
        ztype[0..len],
        buffer.buffer.items.len,
        values,
    )) -1 else 0;
}

pub export fn BufferWriteZAt(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(3).bz_getUserDataPtr());
    const index = ctx.vm.bz_peek(2).integer();
    var len: usize = 0;
    const ztype = ctx.vm.bz_peek(1).bz_valueToString(&len).?;
    const values = ctx.vm.bz_peek(0);

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
    for (0..values.bz_listLen()) |i| {
        const value = values.bz_listGet(
            @intCast(i),
            false,
        );

        if (!value.bz_valueIs(type_def_value).boolean()) {
            var msg = std.ArrayList(u8).init(api.VM.allocator);
            defer msg.deinit();

            vm.bz_pushError(
                "ffi.FFITypeMismatchError",
                "ffi.FFITypeMismatchError".len,
                null,
                0,
            );

            return false;
        }

        var len: usize = 0;
        const ptr = value.bz_foreignContainerSlice(&len);

        buffer.buffer.ensureTotalCapacityPrecise(buffer.buffer.items.len + len) catch {
            vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        };
        buffer.buffer.expandToCapacity();

        std.debug.assert(buffer.buffer.capacity == buffer.buffer.items.len);

        buffer.buffer.replaceRange(index, len, ptr[0..len]) catch {
            vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        };

        index += len;
    }

    return true;
}

pub export fn BufferWriteStruct(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(2).bz_getUserDataPtr());
    const type_def = ctx.vm.bz_peek(1);
    const values = ctx.vm.bz_peek(0);

    return if (!rawWriteStruct(
        ctx.vm,
        buffer,
        buffer.buffer.items.len,
        type_def,
        values,
    )) -1 else 0;
}

pub export fn BufferWriteStructAt(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(3).bz_getUserDataPtr());
    const type_def = ctx.vm.bz_peek(2);
    const index = ctx.vm.bz_peek(1).integer();
    const values = ctx.vm.bz_peek(0);

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
    const size = type_def.bz_containerTypeSize();

    const from = (at orelse buffer.cursor);
    const slice = buffer.buffer.items[from .. from + size];

    return vm.bz_newForeignContainerFromSlice(type_def, slice.ptr, slice.len);
}

pub export fn BufferReadStruct(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());
    const type_def = ctx.vm.bz_peek(0);

    ctx.vm.bz_push(
        rawReadStruct(
            ctx.vm,
            buffer,
            null,
            type_def,
        ),
    );

    return 1;
}

pub export fn BufferReadStructAt(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(2).bz_getUserDataPtr());
    const index: usize = @intCast(ctx.vm.bz_peek(1).integer());
    const type_def = ctx.vm.bz_peek(0);

    ctx.vm.bz_push(
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
    const zig_type = vm.bz_zigType(
        @ptrCast(ztype),
        ztype.len,
        &obj_typedef,
    );

    const len = api.ZigType.bz_zigTypeSize(zig_type.?);

    const value = vm.bz_readZigValueFromBuffer(
        zig_type.?,
        at orelse buffer.cursor,
        buffer.buffer.items.ptr,
        buffer.buffer.items.len,
    );

    if (!checkBuzzType(vm, value, zig_type.?, obj_typedef)) {
        return -1;
    }

    buffer.cursor += len;

    vm.bz_push(value);

    return 1;
}

pub export fn BufferReadZ(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());
    var len: usize = 0;
    const ztype = ctx.vm.bz_peek(0).bz_valueToString(&len).?;

    return rawReadZ(
        ctx.vm,
        buffer,
        null,
        ztype[0..len],
    );
}

pub export fn BufferReadZAt(ctx: *api.NativeCtx) c_int {
    const buffer = Buffer.fromUserData(ctx.vm.bz_peek(2).bz_getUserDataPtr());
    const index: usize = @intCast(ctx.vm.bz_peek(1).integer());
    var len: usize = 0;
    const ztype = ctx.vm.bz_peek(0).bz_valueToString(&len).?;

    return rawReadZ(
        ctx.vm,
        buffer,
        index,
        ztype[0..len],
    );
}
