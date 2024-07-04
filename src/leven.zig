const std = @import("std");

pub fn leven(s: []const u8, t: []const u8, current: usize, limit: usize) usize {
    if (s.len == 0) return t.len;
    if (t.len == 0) return s.len;

    const s1 = s[1..];
    const t1 = t[1..];

    if (s[0] == t[0]) {
        return leven(s1, t1, current + 1, limit);
    }

    return @min(
        leven(s1, t1, current + 1, limit),
        leven(s, t1, current + 1, limit),
        leven(s1, t, current + 1, limit),
    ) + 1;
}
