const std = @import("std");

pub const pcre = @cImport({
    @cInclude("pcre.h");
});
