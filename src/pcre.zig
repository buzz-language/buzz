const std = @import("std");

pub const pcre = @cImport({
    @cInclude("pcre.h");
});

test "Testing pcre" {
    var err = try std.heap.c_allocator.allocSentinel(u8, 1000, 0);
    var err_offset: c_int = undefined;
    // PCRE_EXP_DECL pcre *pcre_compile(const char *, int, const char **, int *,
    //               const unsigned char *);
    const reg: ?*pcre.struct_real_pcre8_or_16 = pcre.pcre_compile("hello", 0, @ptrCast([*c][*c]const u8, &err), &err_offset, null);

    std.debug.print("\npattern {?}\n", .{reg});
}
