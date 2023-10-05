const std = @import("std");
const api = @import("buzz_api.zig");

const tm = extern struct {
    /// seconds after the minute [0-60]
    sec: c_int,
    /// minutes after the hour [0-59]
    min: c_int,
    /// hours since midnight [0-23]
    hour: c_int,
    /// day of the month [1-31]
    mday: c_int,
    /// months since January [0-11]
    mon: c_int,
    /// years since 1900
    year: c_int,
    /// days since Sunday [0-6]
    wday: c_int,
    /// days since January 1 [0-365]
    yday: c_int,
    /// Daylight Savings Time flag
    isdst: c_int,
    /// offset from UTC in seconds
    gmtoff: c_int,
    /// timezone abbreviation
    tm_zone: ?[*:0]const u8,
};

// time.h stuff
const time_t = i32;
extern fn strftime(
    str: [*]u8,
    max_size: usize,
    format: [*:0]const u8,
    time_ptr: *tm,
) usize;
extern fn time(t: ?*time_t) time_t;
extern fn localtime(timer: *time_t) *tm;
extern fn mktime(t: *tm) time_t;
extern fn strptime(
    s: [*:0]const u8,
    format: [*:0]const u8,
    time: *tm,
) ?[*:0]const u8;

const fields = [_][]const u8{
    "second",
    "minute",
    "hour",
    "monthDay",
    "month",
    "year",
    "weekDay",
    "yearDay",
    "isdst",
    "gmtOffset",
};

fn dateTimeToTM(vm: *api.VM, datetime: api.Value) tm {
    var len: usize = 0;
    const zone_str = api.ObjObject.bz_getInstanceField(
        vm,
        datetime,
        api.ObjString.bz_objStringToValue(
            api.ObjString.bz_string(
                vm,
                "timeZone".ptr,
                "timeZone".len,
            ) orelse @panic("Out of memory"),
        ),
    ).bz_valueToString(&len);

    return .{
        .tm_zone = if (zone_str != null and len > 0)
            api.VM.allocator.dupeZ(u8, zone_str.?[0..len]) catch @panic("Out of memory")
        else
            null,
        .sec = api.ObjObject.bz_getInstanceField(
            vm,
            datetime,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    vm,
                    "second".ptr,
                    "second".len,
                ) orelse @panic("Out of memory"),
            ),
        ).integer(),
        .min = api.ObjObject.bz_getInstanceField(
            vm,
            datetime,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    vm,
                    "minute".ptr,
                    "minute".len,
                ) orelse @panic("Out of memory"),
            ),
        ).integer(),
        .hour = api.ObjObject.bz_getInstanceField(
            vm,
            datetime,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    vm,
                    "hour".ptr,
                    "hour".len,
                ) orelse @panic("Out of memory"),
            ),
        ).integer(),
        .mday = api.ObjObject.bz_getInstanceField(
            vm,
            datetime,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    vm,
                    "monthDay".ptr,
                    "monthDay".len,
                ) orelse @panic("Out of memory"),
            ),
        ).integer(),
        .mon = api.ObjObject.bz_getInstanceField(
            vm,
            datetime,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    vm,
                    "month".ptr,
                    "month".len,
                ) orelse @panic("Out of memory"),
            ),
        ).integer(),
        .year = api.ObjObject.bz_getInstanceField(
            vm,
            datetime,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    vm,
                    "year".ptr,
                    "year".len,
                ) orelse @panic("Out of memory"),
            ),
        ).integer(),
        .wday = api.ObjObject.bz_getInstanceField(
            vm,
            datetime,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    vm,
                    "weekDay".ptr,
                    "weekDay".len,
                ) orelse @panic("Out of memory"),
            ),
        ).integer(),
        .yday = api.ObjObject.bz_getInstanceField(
            vm,
            datetime,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    vm,
                    "yearDay".ptr,
                    "yearDay".len,
                ) orelse @panic("Out of memory"),
            ),
        ).integer(),
        .isdst = api.ObjObject.bz_getInstanceField(
            vm,
            datetime,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    vm,
                    "isdst".ptr,
                    "isdst".len,
                ) orelse @panic("Out of memory"),
            ),
        ).integer(),
        .gmtoff = api.ObjObject.bz_getInstanceField(
            vm,
            datetime,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    vm,
                    "gmtOffset".ptr,
                    "gmtOffset".len,
                ) orelse @panic("Out of memory"),
            ),
        ).integer(),
    };
}

export fn parse(ctx: *api.NativeCtx) c_int {
    var source_len: usize = 0;
    const source = ctx.vm.bz_peek(1).bz_valueToObjString().bz_objStringToString(&source_len);
    const source_c = api.VM.allocator.dupeZ(u8, source.?[0..source_len]) catch @panic("Out of memory");
    defer api.VM.allocator.free(source_c);

    var fmt_len: usize = 0;
    const fmt = ctx.vm.bz_peek(0).bz_valueToObjString().bz_objStringToString(&fmt_len);
    const fmt_c = api.VM.allocator.dupeZ(u8, fmt.?[0..fmt_len]) catch @panic("Out of memory");
    defer api.VM.allocator.free(fmt_c);

    var user_time: tm = undefined;
    if (strptime(
        source_c,
        fmt_c,
        &user_time,
    ) == null) {
        ctx.vm.pushError("time.DateFormatError", null);

        return -1;
    }

    const datetime = api.ObjObject.bz_instanceQualified(
        ctx.vm,
        "time.DateTime",
        "time.DateTime".len,
    );

    const values = [_]c_int{
        user_time.sec,
        user_time.min,
        user_time.hour,
        user_time.mday,
        user_time.mon,
        user_time.year,
        user_time.wday,
        user_time.yday,
        user_time.isdst,
        user_time.gmtoff,
    };

    for (fields, 0..) |field, i| {
        api.ObjObject.bz_setInstanceField(
            ctx.vm,
            datetime,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    ctx.vm,
                    field.ptr,
                    field.len,
                ) orelse @panic("Out of memory"),
            ),
            api.Value.fromInteger(values[i]),
        );
    }

    // FIXME: seems to be invalid pointer?
    // const timezone = if (info.tm_zone) |tz| std.mem.span(tz) else "";
    const timezone = "";
    api.ObjObject.bz_setInstanceField(
        ctx.vm,
        datetime,
        api.ObjString.bz_objStringToValue(
            api.ObjString.bz_string(
                ctx.vm,
                "timeZone".ptr,
                "timeZone".len,
            ) orelse @panic("Out of memory"),
        ),
        api.ObjString.bz_objStringToValue(
            api.ObjString.bz_string(
                ctx.vm,
                timezone.ptr,
                timezone.len,
            ) orelse @panic("Out of memory"),
        ),
    );

    ctx.vm.bz_push(
        datetime,
    );

    return 1;
}

export fn timestamp(ctx: *api.NativeCtx) c_int {
    const datetime = ctx.vm.bz_peek(0);
    var user_time = dateTimeToTM(ctx.vm, datetime);

    const result = mktime(&user_time);

    ctx.vm.bz_pushFloat(@floatFromInt(result));

    return 1;
}

export fn timeFormat(ctx: *api.NativeCtx) c_int {
    const datetime = ctx.vm.bz_peek(1);
    var fmt_len: usize = 0;
    const fmt = ctx.vm.bz_peek(0).bz_valueToObjString().bz_objStringToString(&fmt_len);

    if (fmt_len == 0) {
        ctx.vm.pushError("time.DateFormatError", null);

        return -1;
    }

    const fmt_c = api.VM.allocator.dupeZ(u8, fmt.?[0..fmt_len]) catch @panic("Out of memory");
    defer api.VM.allocator.free(fmt_c);

    var user_time = dateTimeToTM(ctx.vm, datetime);

    // Could we find out the exact needed space?
    var buffer: [1024]u8 = undefined;
    const len = strftime(
        buffer[0..].ptr,
        buffer.len,
        fmt_c,
        &user_time,
    );

    ctx.vm.bz_pushString(
        api.ObjString.bz_string(
            ctx.vm,
            @ptrCast(buffer[0..len]),
            len,
        ) orelse {
            @panic("Out of memory");
        },
    );

    return 1;
}

export fn now(ctx: *api.NativeCtx) c_int {
    var raw_time: time_t = undefined;
    _ = time(&raw_time);
    const vm = ctx.vm;

    const info = localtime(&raw_time);

    const datetime = api.ObjObject.bz_instanceQualified(
        vm,
        "time.DateTime",
        "time.DateTime".len,
    );

    const values = [_]c_int{
        info.sec,
        info.min,
        info.hour,
        info.mday,
        info.mon,
        info.year,
        info.wday,
        info.yday,
        info.isdst,
        info.gmtoff,
    };

    for (fields, 0..) |field, i| {
        api.ObjObject.bz_setInstanceField(
            vm,
            datetime,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    vm,
                    field.ptr,
                    field.len,
                ) orelse @panic("Out of memory"),
            ),
            api.Value.fromInteger(values[i]),
        );
    }

    // FIXME: seems to be invalid pointer?
    // const timezone = if (info.tm_zone) |tz| std.mem.span(tz) else "";
    const timezone = "";
    api.ObjObject.bz_setInstanceField(
        vm,
        datetime,
        api.ObjString.bz_objStringToValue(
            api.ObjString.bz_string(
                vm,
                "timeZone".ptr,
                "timeZone".len,
            ) orelse @panic("Out of memory"),
        ),
        api.ObjString.bz_objStringToValue(
            api.ObjString.bz_string(
                vm,
                timezone.ptr,
                timezone.len,
            ) orelse @panic("Out of memory"),
        ),
    );

    ctx.vm.bz_push(
        datetime,
    );

    return 1;
}

export fn fromTimestamp(ctx: *api.NativeCtx) c_int {
    var raw_time: time_t = @intFromFloat(ctx.vm.bz_peek(0).float());

    const vm = ctx.vm;

    // FIXME: localtime doesn't accept any value other than a ptr set by `time` ?
    const info = localtime(&raw_time);

    const datetime = api.ObjObject.bz_instanceQualified(
        vm,
        "time.DateTime",
        "time.DateTime".len,
    );

    const values = [_]c_int{
        info.sec,
        info.min,
        info.hour,
        info.mday,
        info.mon,
        info.year,
        info.wday,
        info.yday,
        info.isdst,
        info.gmtoff,
    };

    for (fields, 0..) |field, i| {
        api.ObjObject.bz_setInstanceField(
            vm,
            datetime,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    vm,
                    field.ptr,
                    field.len,
                ) orelse @panic("Out of memory"),
            ),
            api.Value.fromInteger(values[i]),
        );
    }

    // FIXME: seems to be invalid pointer?
    // const timezone = if (info.tm_zone) |tz| std.mem.span(tz) else "";
    const timezone = "";
    api.ObjObject.bz_setInstanceField(
        vm,
        datetime,
        api.ObjString.bz_objStringToValue(
            api.ObjString.bz_string(
                vm,
                "timeZone".ptr,
                "timeZone".len,
            ) orelse @panic("Out of memory"),
        ),
        api.ObjString.bz_objStringToValue(
            api.ObjString.bz_string(
                vm,
                timezone.ptr,
                timezone.len,
            ) orelse @panic("Out of memory"),
        ),
    );

    ctx.vm.bz_push(
        datetime,
    );

    return 1;
}
