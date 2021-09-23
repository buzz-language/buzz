const std = @import("std");

const A = opaque {};

extern fn printName(a: *A) void;

export fn printPrintName(a: *A) void {
    printName(a);
}

// Build with: zig build-lib export.zig -dynamic -lcommon -L. -rpath .