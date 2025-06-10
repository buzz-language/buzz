const std = @import("std");

/// Wraps std.ArrayList to provide a std.Io.Writer until its provided by std lib
pub fn WriteableArrayList(comptime T: type) type {
    return struct {
        const Self = @This();

        list: std.array_list.Managed(T),
        writer: std.Io.Writer,

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .list = .init(allocator),
                .writer = .{
                    .buffer = &.{},
                    .vtable = &.{
                        .drain = drain,
                    },
                },
            };
        }

        pub fn deinit(self: *Self) void {
            self.list.deinit();
        }

        fn drain(w: *std.Io.Writer, data: []const []const u8, splat: usize) std.Io.Writer.Error!usize {
            var self: *Self = @alignCast(@fieldParentPtr("writer", w));

            if (w.buffer.len > 0) {
                self.list.appendSlice(w.buffer) catch return error.WriteFailed;
            }

            var written: usize = 0;
            for (data[0 .. data.len - 1]) |element| {
                self.list.appendSlice(element) catch return error.WriteFailed;
                written += element.len;
            }

            const last = data[data.len - 1];
            for (0..splat) |_| {
                self.list.appendSlice(last) catch return error.WriteFailed;
            }

            return written + last.len * splat;
        }
    };
}
